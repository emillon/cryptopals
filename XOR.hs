module XOR (xorBuffer) where

import Data.Bits

import qualified Data.ByteString as B

xorBuffer :: B.ByteString -> B.ByteString -> B.ByteString
xorBuffer a b =
    B.pack $ B.zipWith xor a b
