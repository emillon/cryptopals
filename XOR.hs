module XOR (xorBuffer) where

import Data.Bits

import qualified Data.ByteString as B

xorBuffer :: B.ByteString -> B.ByteString -> B.ByteString
xorBuffer a b =
    snd $ B.mapAccumL (\ i w -> (i+1, w `xor` (B.index b i))) 0 a
