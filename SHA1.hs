-- | SHA1 implementation for strict bytestrings.

module SHA1 (sha1Tests) where

import Data.Array
import Data.Bits
import Data.List
import Data.Word
import Test.HUnit

import qualified Data.ByteString as B

import Base64
import Misc

sha1 :: B.ByteString -> B.ByteString
sha1 bs =
    digest $ foldl' update initState $ chunksOfSize 64 $ prepare bs

prepare :: B.ByteString -> B.ByteString
prepare bs =
    B.concat [bs, B.singleton 0x80, pad, size]
        where
            modsize = (n+1) `mod` 64
            npad = (56 - modsize) `mod` 64
            pad = B.replicate npad 0x00
            n = B.length bs
            ml = fromIntegral $ 8 * n
            size = w64BEtoBS ml

data SHA1State = SHA1S !Word32 !Word32 !Word32 !Word32 !Word32

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

-- | HUnit tests for this module.
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
