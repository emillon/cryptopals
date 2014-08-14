module Base64 ( decodeHex
              , encodeHex
              , encodeHexPair
              ) where

import Data.Char
import Data.Word

import qualified Data.ByteString as B

decodeHex :: String -> B.ByteString
decodeHex = B.pack . map (uncurry decodeHexPair) . twoByTwo

encodeHex :: B.ByteString -> String
encodeHex b = B.foldr go "" b
    where
        go w s =
            encodeHexPair w ++ s

twoByTwo :: [a] -> [(a, a)]
twoByTwo [] = []
twoByTwo (a:b:l) = (a, b):twoByTwo l
twoByTwo [_] = error "twoByTwo"

decodeHexPair :: Char -> Char -> Word8
decodeHexPair h l =
    0x10 * decodeHexChar h + decodeHexChar l

decodeHexChar :: Char -> Word8
decodeHexChar = fromIntegral . digitToInt

encodeHexPair :: Word8 -> String
encodeHexPair w = [h, l] where
    h = intToDigit $ fromIntegral q
    l = intToDigit $ fromIntegral r
    (q, r) = w `quotRem` 0x10

