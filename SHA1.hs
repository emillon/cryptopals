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

type SHA1State = (Word32, Word32, Word32, Word32, Word32)

initState :: SHA1State
initState =
    ( 0x67452301
    , 0xEFCDAB89
    , 0x98BADCFE
    , 0x10325476
    , 0xC3D2E1F0
    )

stateAdd :: SHA1State -> SHA1State -> SHA1State
stateAdd (xa, xb, xc, xd, xe) (ya, yb, yc, yd, ye) =
    (xa + ya, xb + yb, xc + yc, xd + yd, xe + ye)

update :: SHA1State -> B.ByteString -> SHA1State
update s0 bs =
    stateAdd s0 s'
        where
            w = expand bs
            s' = foldl' (step w) s0 [0..79]

step :: Array Int Word32 -> SHA1State -> Int -> SHA1State
step w (s@(a, b, c, d, e)) i =
    let f = compF i s
        k = compK i
        temp = (a `rotateL` 5) + f + e + k + (w ! i)
        e' = d
        d' = c
        c' = b `rotateL` 30
        b' = a
        a' = temp
    in
    (a', b', c', d', e')

compF :: Int -> SHA1State -> Word32
compF i (_, b, c, d, _) | inRange ( 0, 19) i = (b .&. c) .|. (complement b .&. d)
compF i (_, b, c, d, _) | inRange (20, 39) i = b `xor` c `xor` d
compF i (_, b, c, d, _) | inRange (40, 59) i = (b .&. c) .|. (b .&. d) .|. (c .&. d)
compF i (_, b, c, d, _) | inRange (60, 79) i = b `xor` c `xor` d

compK :: Int -> Word32
compK i | inRange ( 0, 19) i = 0x5A827999
compK i | inRange (20, 39) i = 0x6ED9EBA1
compK i | inRange (40, 59) i = 0x8F1BBCDC
compK i | inRange (60, 79) i = 0xCA62C1D6

expand :: B.ByteString -> Array Int Word32
expand bs = w
    where
        w = array (0, 79) $ [(i, bsToNthW32BE bs i) | i <- [0..15]]
                         ++ [(i, ((w ! (i-3))
                            `xor` (w ! (i-8))
                            `xor` (w ! (i-14))
                            `xor` (w ! (i-16))) `rotateL` 1) | i <- [16..79]]

digest :: SHA1State -> B.ByteString
digest (a, b, c, d, e) =
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
