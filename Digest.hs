-- | Digest algorithm implementations for strict bytestrings.

module Digest ( sha1
              , sha1Tests
              , sha1PrefixMac
              , checkSha1PrefixMac
              , checkDigestProps
              , sha1Extend
              , sha1ExtendN
              ) where

import Data.Array
import Data.Bits
import Data.List
import Data.Word
import Test.HUnit
import Test.QuickCheck hiding ((.&.))

import qualified Data.ByteString as B

import Base64
import Misc

-- | Compute the digest of a message (not hex-encoded!)
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
