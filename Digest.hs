-- | Digest algorithm implementations for strict bytestrings.

module Digest ( sha1
              , sha1Tests
              , sha1PrefixMac
              , checkSha1PrefixMac
              , checkDigestProps
              , sha1Extend
              , md4
              , md4Tests
              , md4PrefixMac
              , checkMd4PrefixMac
              , md4Extend
              ) where

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
    digest $ foldl' update initState $ chunksOfSize 64 $ sha1Prepare bs

-- | Compute the SHA1 prefix-MAC of a message under a given key.
sha1PrefixMac :: B.ByteString -- ^ Key
              -> B.ByteString -- ^ Message
              -> B.ByteString
sha1PrefixMac key message =
    sha1 $ B.append key message

-- | Check the validity of a SHA1 prefix-MAC.
checkSha1PrefixMac :: B.ByteString -- ^ Key
                   -> B.ByteString -- ^ Message
                   -> B.ByteString -- ^ MAC
                   -> Bool
checkSha1PrefixMac key message mac =
    sha1PrefixMac key message == mac

sha1Prepare :: B.ByteString -> B.ByteString
sha1Prepare bs =
    B.append bs $ sha1Padding bs

sha1Padding :: B.ByteString -> B.ByteString
sha1Padding = sha1PaddingLen . B.length

sha1PaddingLen :: Int -> B.ByteString
sha1PaddingLen n =
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

prop_sha1_inject_inv :: SHA1State -> Bool
prop_sha1_inject_inv s =
    injectState (digest s) == s

prop_sha1_extension :: GeneratedBSA -> GeneratedBSA -> Property
prop_sha1_extension (GBSA m) (GBSA e) =
    (B.length e < 56) ==> sha1 extended == digest (update originalsha paddedExt)
        where
            extended = B.concat [m, glue, e]
            glue = sha1Padding m
            paddedExt = B.append e $ sha1Padding extended
            originalsha = injectState $ sha1 m

-- | Prepare a valid extension for a Hash Length Extension Attack.
-- It needs an oracle to check if a MAC is valid (to detect key length).
sha1Extend :: (B.ByteString -> B.ByteString -> Bool) -- ^ Oracle (takes msg & mac)
           -> B.ByteString  -- ^ Original MAC'd message
           -> B.ByteString  -- ^ MAC
           -> B.ByteString  -- ^ Expected extension
           -> (B.ByteString, B.ByteString) -- ^ (suffix, new MAC)
sha1Extend = hleExtend sha1PaddingLen injectUpdate
    where
        injectUpdate st bs = digest $ update (injectState st) bs

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
    quickCheck prop_sha1_inject_inv
    quickCheck prop_sha1_extension
    quickCheck prop_sha1_extension_attack
    quickCheck prop_md4_inject_inv
    quickCheck prop_md4_extension
    quickCheck prop_md4_extension_attack

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
    md4digest $ foldl' md4update md4initState $ chunksOfSize (16*4) $ md4Prepare bs

md4Prepare :: B.ByteString -> B.ByteString
md4Prepare bs =
    B.append bs $ md4Padding bs

md4Padding :: B.ByteString -> B.ByteString
md4Padding = md4PaddingLen . B.length

md4PaddingLen :: Int -> B.ByteString
md4PaddingLen n =
    B.concat [B.singleton 0x80, pad, sizelo, sizehi]
        where
            modsize = (n+1) `mod` 64
            npad = (56 - modsize) `mod` 64
            pad = B.replicate npad 0x00
            sizelo = w32LEtoBS lo
            sizehi = w32LEtoBS hi
            (lo, hi) = splitW64 $ fromIntegral $ 8 * n

data MD4State = MD4S { md4sA :: !Word32
                     , md4sB :: !Word32
                     , md4sC :: !Word32
                     , md4sD :: !Word32
                     }
    deriving (Eq, Show)

instance Arbitrary MD4State where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ MD4S a b c d

md4initState :: MD4State
md4initState = MD4S 0x67452301 0xefcdab89 0x98badcfe 0x10325476

md4digest :: MD4State -> B.ByteString
md4digest (MD4S a b c d) =
    B.concat $ map w32LEtoBS [a, b, c, d]

md4F, md4G, md4H :: Word32 -> Word32 -> Word32 -> Word32
md4F x y z = (x .&. y) .|. (complement x .&. z)
md4G x y z = (x .&. y) .|. (x .&. z) .|. (y .&. z)
md4H x y z = x `xor` y `xor` z

md4StateAdd :: MD4State -> MD4State -> MD4State
md4StateAdd (MD4S xa xb xc xd) (MD4S ya yb yc yd) =
    MD4S (xa + ya) (xb + yb) (xc + yc) (xd + yd)

md4update :: MD4State -> B.ByteString -> MD4State
md4update s0 bs =
    md4StateAdd s0 s'
        where
            s' = md4rounds x s0
            x = listArray (0, 15) $ map (bsToNthW32LE bs) [0..15]

type Lens r a = (r -> a, a -> r -> r)

lensA, lensB, lensC, lensD :: Lens MD4State Word32
lensA = (md4sA, \ x r -> r { md4sA = x })
lensB = (md4sB, \ x r -> r { md4sB = x })
lensC = (md4sC, \ x r -> r { md4sC = x })
lensD = (md4sD, \ x r -> r { md4sD = x })

type Lens4 = (Lens MD4State Word32, Lens MD4State Word32, Lens MD4State Word32, Lens MD4State Word32)

abcd, dabc, cdab, bcda :: Lens4
abcd = (lensA, lensB, lensC, lensD)
dabc = (lensD, lensA, lensB, lensC)
cdab = (lensC, lensD, lensA, lensB)
bcda = (lensB, lensC, lensD, lensA)

execRounds :: [(Int -> Int -> Word32 -> Word32 -> Word32 -> Word32 -> Word32,
                Lens4, Int, Int)]
              -> MD4State -> MD4State
execRounds rs s0 = foldl' go s0 rs
    where
        go st (f, ((ga, sa), (gb, _), (gc, _), (gd, _)), xk, s) =
            sa (f xk s (ga st) (gb st) (gc st) (gd st)) st

md4rounds :: Array Int Word32 -> MD4State -> MD4State
md4rounds x = execRounds
    [ (f1, abcd,  0,  3), (f1, dabc,  1,  7), (f1, cdab,  2, 11), (f1, bcda,  3, 19)
    , (f1, abcd,  4,  3), (f1, dabc,  5,  7), (f1, cdab,  6, 11), (f1, bcda,  7, 19)
    , (f1, abcd,  8,  3), (f1, dabc,  9,  7), (f1, cdab, 10, 11), (f1, bcda, 11, 19)
    , (f1, abcd, 12,  3), (f1, dabc, 13,  7), (f1, cdab, 14, 11), (f1, bcda, 15, 19)
    , (f2, abcd,  0,  3), (f2, dabc,  4,  5), (f2, cdab,  8,  9), (f2, bcda, 12, 13)
    , (f2, abcd,  1,  3), (f2, dabc,  5,  5), (f2, cdab,  9,  9), (f2, bcda, 13, 13)
    , (f2, abcd,  2,  3), (f2, dabc,  6,  5), (f2, cdab, 10,  9), (f2, bcda, 14, 13)
    , (f2, abcd,  3,  3), (f2, dabc,  7,  5), (f2, cdab, 11,  9), (f2, bcda, 15, 13)
    , (f3, abcd,  0,  3), (f3, dabc,  8,  9), (f3, cdab,  4, 11), (f3, bcda, 12, 15)
    , (f3, abcd,  2,  3), (f3, dabc, 10,  9), (f3, cdab,  6, 11), (f3, bcda, 14, 15)
    , (f3, abcd,  1,  3), (f3, dabc,  9,  9), (f3, cdab,  5, 11), (f3, bcda, 13, 15)
    , (f3, abcd,  3,  3), (f3, dabc, 11,  9), (f3, cdab,  7, 11), (f3, bcda, 15, 15)
    ]
        where
            f1 k s a b c d = (a + md4F b c d + (x!k)) `rotateL` s
            f2 k s a b c d = (a + md4G b c d + (x!k) + 0x5a827999) `rotateL` s
            f3 k s a b c d = (a + md4H b c d + (x!k) + 0x6ed9eba1) `rotateL` s

-- | Compute the MD4 prefix-MAC of a message under a given key.
md4PrefixMac :: B.ByteString -- ^ Key
             -> B.ByteString -- ^ Message
             -> B.ByteString
md4PrefixMac key message =
    md4 $ B.append key message

-- | Check the validity of a MD4 prefix-MAC.
checkMd4PrefixMac :: B.ByteString -- ^ Key
                  -> B.ByteString -- ^ Message
                  -> B.ByteString -- ^ MAC
                  -> Bool
checkMd4PrefixMac key message mac =
    md4PrefixMac key message == mac

md4InjectState :: B.ByteString -> MD4State
md4InjectState bs =
    MD4S a b c d
        where
            a = bsToNthW32LE bs 0
            b = bsToNthW32LE bs 1
            c = bsToNthW32LE bs 2
            d = bsToNthW32LE bs 3

prop_md4_inject_inv :: MD4State -> Bool
prop_md4_inject_inv s =
    md4InjectState (md4digest s) == s

prop_md4_extension :: GeneratedBSA -> GeneratedBSA -> Property
prop_md4_extension (GBSA m) (GBSA e) =
    (B.length e < 56) ==> md4 extended == md4digest (md4update originalmd4 paddedExt)
        where
            extended = B.concat [m, glue, e]
            glue = md4Padding m
            paddedExt = B.append e $ md4Padding extended
            originalmd4 = md4InjectState $ md4 m

prop_md4_extension_attack :: GeneratedBS16 -> GeneratedBSA -> GeneratedBSA -> Property
prop_md4_extension_attack (GBS16 key) (GBSA message) (GBSA extension) =
    (B.length extension < 56) ==>
    checkMd4PrefixMac key extMsg extMac && extension `B.isSuffixOf` extMsg
        where
            mac = md4PrefixMac key message
            oracle = checkMd4PrefixMac key
            (suffix, extMac) = md4Extend oracle message mac extension
            extMsg = B.append message suffix

hleExtendN :: (Int -> B.ByteString) -- ^ Padding function
           -> (B.ByteString -> B.ByteString -> B.ByteString)
           -- ^ Inject+update function. Takes state and bs and return new state.
           -> Int                   -- ^ Len of key
           -> Int                   -- ^ Len of original MAC'd message
           -> B.ByteString          -- ^ MAC
           -> B.ByteString          -- ^ Expected extension
           -> (B.ByteString, B.ByteString) -- ^ (suffix, new MAC)
hleExtendN paddingLen injectUpdate keyLen msgLen mac ext =
    (suffix, newMac)
        where
            totalLen = keyLen + msgLen
            suffix = B.append glue ext
            glue = paddingLen totalLen
            paddedExt = B.append ext $ paddingLen $ totalLen + B.length suffix
            newMac = injectUpdate mac paddedExt

-- | Prepare a valid extension for a Hash Length Extension Attack (MD4).
-- It needs an oracle to check if a MAC is valid (to detect key length).
md4Extend :: (B.ByteString -> B.ByteString -> Bool) -- ^ Oracle (takes msg & mac)
          -> B.ByteString  -- ^ Original MAC'd message
          -> B.ByteString  -- ^ MAC
          -> B.ByteString  -- ^ Expected extension
          -> (B.ByteString, B.ByteString) -- ^ (suffix, new MAC)
md4Extend =
    hleExtend md4PaddingLen injectUpdate
        where
            injectUpdate st bs = md4digest $ md4update (md4InjectState st) bs

hleExtend :: (Int -> B.ByteString) -- ^ Padding function
          -> (B.ByteString -> B.ByteString -> B.ByteString)
          -- ^ Inject+update function. Takes state and bs and return new state.
          -> (B.ByteString -> B.ByteString -> Bool) -- ^ Oracle (takes msg & mac)
          -> B.ByteString -- ^ Original MAC'd message
          -> B.ByteString -- ^ MAC
          -> B.ByteString -- ^ Expected extension
          -> (B.ByteString, B.ByteString) -- ^ (suffix, new MAC)
hleExtend paddingLen injectUpdate oracle message mac ext =
    head $ filter ok $ map (\ n -> hleExtendN paddingLen injectUpdate n msgLen mac ext) [1..30]
        where
            ok (suffix, newMac) = oracle (B.append message suffix) newMac
            msgLen = B.length message
