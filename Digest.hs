-- | Digest algorithm implementations for strict bytestrings.

module Digest ( sha1
              , sha1Tests
              , sha1PrefixMac
              , checkSha1PrefixMac
              , checkDigestProps
              , sha1Extend
              , sha1ExtendN
              , md4Tests
              ) where

import Control.Monad.State
import Data.Array
import Data.Bits
import Data.List
import Data.Word
import Test.HUnit hiding (State)
import Test.QuickCheck hiding ((.&.))

import qualified Data.ByteString as B

import Base64
import Misc

-- | Compute the SHA1 digest of a message (not hex-encoded!)
sha1 :: B.ByteString -> B.ByteString
sha1 bs =
    digest $ foldl' update initState $ chunksOfSize 64 $ prepare bs

-- | Compute the prefix-MAC of a message under a given key.
sha1PrefixMac :: B.ByteString -- ^ Key
              -> B.ByteString -- ^ Message
              -> B.ByteString
sha1PrefixMac key message =
    sha1 $ B.append key message

-- | Check the validity of a prefix-MAC.
checkSha1PrefixMac :: B.ByteString -- ^ Key
                   -> B.ByteString -- ^ Message
                   -> B.ByteString -- ^ MAC
                   -> Bool
checkSha1PrefixMac key message mac =
    sha1PrefixMac key message == mac

prepare :: B.ByteString -> B.ByteString
prepare bs =
    B.append bs $ padding bs

padding :: B.ByteString -> B.ByteString
padding = paddingLen . B.length

paddingLen :: Int -> B.ByteString
paddingLen n =
    B.concat [B.singleton 0x80, pad, size]
        where
            modsize = (n+1) `mod` 64
            npad = (56 - modsize) `mod` 64
            pad = B.replicate npad 0x00
            ml = fromIntegral $ 8 * n
            size = w64BEtoBS ml

data SHA1State = SHA1S !Word32 !Word32 !Word32 !Word32 !Word32
    deriving (Eq, Show)

instance Arbitrary SHA1State where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        e <- arbitrary
        return $ SHA1S a b c d e

initState :: SHA1State
initState = SHA1S 0x67452301 0xEFCDAB89 0x98BADCFE 0x10325476 0xC3D2E1F0

stateAdd :: SHA1State -> SHA1State -> SHA1State
stateAdd (SHA1S xa xb xc xd xe) (SHA1S ya yb yc yd ye) =
    SHA1S (xa + ya) (xb + yb) (xc + yc) (xd + yd) (xe + ye)

update :: SHA1State -> B.ByteString -> SHA1State
update s0 bs =
    stateAdd s0 s'
        where
            w = expand bs
            s' = foldl' step s0 $ zip w phases
            phases = concat $ map (\ p -> replicate 20 p)
                [ (0x5A827999, step1_ch)
                , (0x6ED9EBA1, step1_par)
                , (0x8F1BBCDC, step1_maj)
                , (0xCA62C1D6, step1_par)
                ]
            step1_ch (SHA1S _ b c d _) = (b .&. c) .|. (complement b .&. d)
            step1_par (SHA1S _ b c d _) = b `xor` c `xor` d
            step1_maj (SHA1S _ b c d _) = (b .&. c) .|. (b .&. d) .|. (c .&. d)
            step (s@(SHA1S a b c d e)) (wi, (k, funF)) =
                SHA1S ((a `rotateL` 5) + funF s + e + k + wi) a (b `rotateL` 30) c d

expand :: B.ByteString -> [Word32]
expand bs = elems w
    where
        w = array (0, 79) $ [(i, bsToNthW32BE bs i) | i <- [0..15]]
                         ++ [(i, ((w ! (i-3))
                            `xor` (w ! (i-8))
                            `xor` (w ! (i-14))
                            `xor` (w ! (i-16))) `rotateL` 1) | i <- [16..79]]

digest :: SHA1State -> B.ByteString
digest (SHA1S a b c d e) =
    B.concat $ map w32BEtoBS [a, b, c, d, e]

-- | HUnit tests for the SHA1 implementation.
sha1Tests :: Test
sha1Tests =
    "SHA1" ~: map (uncurry tc)
        [ ("", "da39a3ee 5e6b4b0d 3255bfef 95601890 afd80709")
        , ("abc", "a9993e36 4706816a ba3e2571 7850c26c 9cd0d89d")
        , ( "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
          , "84983e44 1c3bd26e baae4aa1 f95129e5 e54670f1"
          )
        , ( "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
          , "a49b2446 a02c645b f419f995 b6709125 3a04a259"
          )
        , (replicate 1000000 'a', "34aa973c d4c4daa4 f61eeb2b dbad2731 6534016f")
        ]
    where
        tc input spec =
            unHex spec ~=? sha1 (string2bs input)

injectState :: B.ByteString -> SHA1State
injectState bs =
    SHA1S a b c d e
        where
            a = bsToNthW32BE bs 0
            b = bsToNthW32BE bs 1
            c = bsToNthW32BE bs 2
            d = bsToNthW32BE bs 3
            e = bsToNthW32BE bs 4

prop_inject_inv :: SHA1State -> Bool
prop_inject_inv s =
    injectState (digest s) == s

prop_sha1_extension :: GeneratedBSA -> GeneratedBSA -> Property
prop_sha1_extension (GBSA m) (GBSA e) =
    (B.length e < 56) ==> sha1 extended == digest (update originalsha paddedExt)
        where
            extended = B.concat [m, glue, e]
            glue = padding m
            paddedExt = B.append e $ padding extended
            originalsha = injectState $ sha1 m

-- | Prepare a valid extension for a Hash Extension Attack (knowing key length).
sha1ExtendN :: Int           -- ^ Len of key
            -> Int           -- ^ Len of original MAC'd message
            -> B.ByteString  -- ^ MAC
            -> B.ByteString  -- ^ Expected extension
            -> (B.ByteString, B.ByteString) -- ^ (suffix, new MAC)
sha1ExtendN keyLen msgLen mac ext =
    (suffix, newMac)
        where
            totalLen = keyLen + msgLen
            suffix = B.append glue ext
            glue = paddingLen totalLen
            paddedExt = B.append ext $ paddingLen $ totalLen + B.length suffix
            newMac = digest (update (injectState mac) paddedExt)

-- | Prepare a valid extension for a Hash Extension Attack.
-- It needs an oracle to check if a MAC is valid (to detect key length).
sha1Extend :: (B.ByteString -> B.ByteString -> Bool) -- ^ Oracle (takes msg & mac)
           -> B.ByteString  -- ^ Original MAC'd message
           -> B.ByteString  -- ^ MAC
           -> B.ByteString  -- ^ Expected extension
           -> (B.ByteString, B.ByteString) -- ^ (suffix, new MAC)
sha1Extend oracle message mac ext =
    head $ filter ok $ map (\ n -> sha1ExtendN n msgLen mac ext) [1..30]
        where
            ok (suffix, newMac) = oracle (B.append message suffix) newMac
            msgLen = B.length message

prop_sha1_extension_attack :: GeneratedBS16 -> GeneratedBSA -> GeneratedBSA -> Property
prop_sha1_extension_attack (GBS16 key) (GBSA message) (GBSA extension) =
    (B.length extension < 56) ==>
    checkSha1PrefixMac key extMsg extMac && extension `B.isSuffixOf` extMsg
        where
            mac = sha1PrefixMac key message
            oracle = checkSha1PrefixMac key
            (suffix, extMac) = sha1Extend oracle message mac extension
            extMsg = B.append message suffix

-- | QuickCheck tests for this module.
checkDigestProps :: IO ()
checkDigestProps = do
    quickCheck prop_inject_inv
    quickCheck prop_sha1_extension
    quickCheck prop_sha1_extension_attack

-- | HUnit tests for the MD4 implementation.
md4Tests :: Test
md4Tests =
    "MD4" ~: map (uncurry tc)
        [ (""
          , "31d6cfe0d16ae931b73c59d7e0c089c0"
          )
        , ( "a"
          , "bde52cb31de33e46245e05fbdbd6fb24"
          )
        , ( "abc"
          , "a448017aaf21d8525fc10ae87aa6729d"
          )
        , ( "message digest"
          , "d9130a8164549fe818874806e1c7014b"
          )
        , ( "abcdefghijklmnopqrstuvwxyz"
          , "d79e1c308aa5bbcdeea8ed63df412da9"
          )
        , ( "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
          , "043f8582f241db351ce627e153e7f0e4"
          )
        , ( "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
          , "e33b4ddc9c38f2199c3e7b164fcc0536"
          )
        ]
    where
        tc input spec =
            unHex spec ~=? md4 (string2bs input)

-- | Compute the MD4 digest of a message (not hex-encoded!)
md4 :: B.ByteString -> B.ByteString
md4 bs =
    md4digest $ foldl' md4update md4initState $ chunksOfSize 64 $ prepare bs

data MD4State = MD4S { md4sA :: !Word32
                     , md4sB :: !Word32
                     , md4sC :: !Word32
                     , md4sD :: !Word32
                     }
    deriving (Eq, Show)

md4initState :: MD4State
md4initState = MD4S 0x67452301 0xefcdab89 0x98badcfe 0x10325476

md4digest :: MD4State -> B.ByteString
md4digest (MD4S a b c d) =
    B.concat $ map w32BEtoBS [a, b, c, d]

md4_f, md4_g, md4_h :: Word32 -> Word32 -> Word32 -> Word32
md4_f x y z = (x .&. y) .|. (complement x .&. z)
md4_g x y z = (x .&. y) .|. (x .&. z) .|. (y .|. z)
md4_h x y z = x `xor` y `xor` z

md4StateAdd :: MD4State -> MD4State -> MD4State
md4StateAdd (MD4S xa xb xc xd) (MD4S ya yb yc yd) =
    MD4S (xa + ya) (xb + yb) (xc + yc) (xd + yd)

md4update :: MD4State -> B.ByteString -> MD4State
md4update s0 bs =
    md4StateAdd s0 s'
        where
            s' = execState m s0
            m = do
                round1 bs
                round2 bs
                round3 bs

type Lens r a = (r -> a, a -> r -> r)

lensA, lensB, lensC, lensD :: Lens MD4State Word32
lensA = (md4sA, \ x r -> r { md4sA = x })
lensB = (md4sB, \ x r -> r { md4sB = x })
lensC = (md4sC, \ x r -> r { md4sC = x })
lensD = (md4sD, \ x r -> r { md4sD = x })

round1, round2, round3 :: B.ByteString -> State MD4State ()
-- Round 1.
-- Let [abcd k s] denote the operation
--      a = (a + F(b,c,d) + X[k]) <<< s.
-- Do the following 16 operations.
-- [ABCD  0  3]  [DABC  1  7]  [CDAB  2 11]  [BCDA  3 19]
-- [ABCD  4  3]  [DABC  5  7]  [CDAB  6 11]  [BCDA  7 19]
-- [ABCD  8  3]  [DABC  9  7]  [CDAB 10 11]  [BCDA 11 19]
-- [ABCD 12  3]  [DABC 13  7]  [CDAB 14 11]  [BCDA 15 19]
round1 bs = do
    r1step bs lensA lensB lensC lensD   0  3
    r1step bs lensD lensA lensB lensC   1  7
    r1step bs lensC lensD lensA lensB   2 11
    r1step bs lensB lensC lensD lensA   3 19
    r1step bs lensA lensB lensC lensD   4  3
    r1step bs lensD lensA lensB lensC   5  7
    r1step bs lensC lensD lensA lensB   6 11
    r1step bs lensB lensC lensD lensA   7 19
    r1step bs lensA lensB lensC lensD   8  3
    r1step bs lensD lensA lensB lensC   9  7
    r1step bs lensC lensD lensA lensB  10 11
    r1step bs lensB lensC lensD lensA  11 19
    r1step bs lensA lensB lensC lensD  12  3
    r1step bs lensD lensA lensB lensC  13  7
    r1step bs lensC lensD lensA lensB  14 11
    r1step bs lensB lensC lensD lensA  15 19

-- Round 2.
-- Let [abcd k s] denote the operation
--   a = (a + G(b,c,d) + X[k] + 5A827999) <<< s.
--
-- Do the following 16 operations.
-- [ABCD  0  3]  [DABC  4  5]  [CDAB  8  9]  [BCDA 12 13]
-- [ABCD  1  3]  [DABC  5  5]  [CDAB  9  9]  [BCDA 13 13]
-- [ABCD  2  3]  [DABC  6  5]  [CDAB 10  9]  [BCDA 14 13]
-- [ABCD  3  3]  [DABC  7  5]  [CDAB 11  9]  [BCDA 15 13]
round2 bs = do
    r2step bs lensA lensB lensC lensD   0  3
    r2step bs lensD lensA lensB lensC   4  5
    r2step bs lensC lensD lensA lensB   8  9
    r2step bs lensB lensC lensD lensA  12 13
    r2step bs lensA lensB lensC lensD   1  3
    r2step bs lensD lensA lensB lensC   5  5
    r2step bs lensC lensD lensA lensB   9  9
    r2step bs lensB lensC lensD lensA  13 13
    r2step bs lensA lensB lensC lensD   2  3
    r2step bs lensD lensA lensB lensC   6  5
    r2step bs lensC lensD lensA lensB  10  9
    r2step bs lensB lensC lensD lensA  14 13
    r2step bs lensA lensB lensC lensD   3  3
    r2step bs lensD lensA lensB lensC   7  5
    r2step bs lensC lensD lensA lensB  11  9
    r2step bs lensB lensC lensD lensA  15 13

-- Round 3.
-- Let [abcd k s] denote the operation
--   a = (a + H(b,c,d) + X[k] + 6ED9EBA1) <<< s.
-- Do the following 16 operations.
-- [ABCD  0  3]  [DABC  8  9]  [CDAB  4 11]  [BCDA 12 15]
-- [ABCD  2  3]  [DABC 10  9]  [CDAB  6 11]  [BCDA 14 15]
-- [ABCD  1  3]  [DABC  9  9]  [CDAB  5 11]  [BCDA 13 15]
-- [ABCD  3  3]  [DABC 11  9]  [CDAB  7 11]  [BCDA 15 15]
round3 bs = do
    r3step bs lensA lensB lensC lensD   0  3
    r3step bs lensD lensA lensB lensC   8  9
    r3step bs lensC lensD lensA lensB   4 11
    r3step bs lensB lensC lensD lensA  12 15
    r3step bs lensA lensB lensC lensD   2  3
    r3step bs lensD lensA lensB lensC  10  9
    r3step bs lensC lensD lensA lensB   6 11
    r3step bs lensB lensC lensD lensA  14 15
    r3step bs lensA lensB lensC lensD   1  3
    r3step bs lensD lensA lensB lensC   9  9
    r3step bs lensC lensD lensA lensB   5 11
    r3step bs lensB lensC lensD lensA  13 15
    r3step bs lensA lensB lensC lensD   3  3
    r3step bs lensD lensA lensB lensC  11  9
    r3step bs lensC lensD lensA lensB   7 11
    r3step bs lensB lensC lensD lensA  15 15

r1step, r2step, r3step :: B.ByteString -> Lens MD4State Word32 -> Lens MD4State Word32
                                       -> Lens MD4State Word32 -> Lens MD4State Word32
                       -> Int -> Int -> State MD4State ()
r1step bs (ga, sa) (gb, _) (gc, _) (gd, _) k s =
    modify $ \ st -> sa ((ga st + md4_f (gb st) (gc st) (gd st) + bsToNthW32BE bs k) `rotateL` s) st

r2step bs (ga, sa) (gb, _) (gc, _) (gd, _) k s =
    modify $ \ st -> sa ((ga st + md4_g (gb st) (gc st) (gd st) + bsToNthW32BE bs k + 0x5A827999) `rotateL` s) st

r3step bs (ga, sa) (gb, _) (gc, _) (gd, _) k s =
    modify $ \ st -> sa ((ga st + md4_h (gb st) (gc st) (gd st) + bsToNthW32BE bs k + 0x6ED9EBA1) `rotateL` s) st
