-- | Functions to manipulate and convert bytestrings to/from base64 or hex.
module Base64 ( decodeHex
              , encodeHex
              , toHex
              , unHex
              , bs2string
              , string2bs
              , decodeBase64
              , hexToB64
              , readFileBase64
              , readFileLinesBase64
              ) where

import Control.Applicative
import Data.Array
import Data.Char
import Data.Word
import Data.Tuple

import qualified Data.ByteString as B

-- | Convert a hex representation such as @\"abcdef\"@ to a bytestring such as
-- @B.pack [0xab, 0xcd, 0xef]@.
decodeHex :: String -> B.ByteString
decodeHex = B.pack . map (uncurry decodeHexPair) . twoByTwo

-- | A generalization of 'decodeHex' that allows spaces between hex digits.
unHex :: String -> B.ByteString
unHex = decodeHex . concat . words

-- | Convert a bytestring to a hex string (converse of 'decodeHex').
encodeHex :: B.ByteString -> String
encodeHex b = B.foldr go "" b
    where
        go w s =
            encodeHexPair w ++ s

twoByTwo :: [a] -> [(a, a)]
twoByTwo [] = []
twoByTwo (a:b:l) = (a, b):twoByTwo l
twoByTwo [_] = error "twoByTwo"

-- | Turn an ASCII string to a hex representation.
--
-- >>> toHex "ABC"
-- "414243"
toHex :: String -> String
toHex =
    concatMap encodeHexPairChar

encodeHexPairChar :: Char -> String
encodeHexPairChar = encodeHexPair . fromIntegral . ord

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

-- | Convert a bytestring to a string. Somehow unsafe.
bs2string :: B.ByteString -> String
bs2string = map (toEnum . fromEnum) . B.unpack

-- | Convert a string to a bytestring, similar to the 'IsString' instance. Somehow unsafe.
string2bs :: String -> B.ByteString
string2bs = B.pack . map (toEnum . fromEnum)

base64Table :: Array Word8 Char
base64Table =
    listArray (0, 63) $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+' , '/']

-- | Convert Hex to Base64.
hexToB64 :: B.ByteString -> B.ByteString
hexToB64 =
    B.concat . map threeToFour . threeByThree . B.unpack

data ChunkOfThree a = C3 a a a
                    | C2 a a
                    | C1 a

threeByThree :: [a] -> [ChunkOfThree a]
threeByThree [] = []
threeByThree (x:y:z:r) = (C3 x y z):threeByThree r
threeByThree [x,y] = [C2 x y]
threeByThree [x] = [C1 x]

threeToFour :: ChunkOfThree Word8 -> B.ByteString
threeToFour (C3 x y z) = B.pack $ threeToFourW x y z
threeToFour (C2 x y) = B.pack $ take 3 (threeToFourW x y 0) ++ [b64pad]
threeToFour (C1 x) = B.pack $ take 2 (threeToFourW x 0 0) ++ [b64pad, b64pad]

b64pad :: Word8
b64pad = fromIntegral $ ord '='

threeToFourW :: Word8 -> Word8 -> Word8 -> [Word8]
threeToFourW x y z =
    map enc [a, b, c, d]
        where
            -- |    xh       xl    yh      yl     zh   zl
            -- | x x x x x x x x|y y y y y y y y|z z z z z z z z
            -- | a a a a a a b b|b b b b c c c c|c c d d d d d d
            a = xh
            b = xl * 16 + yh
            c = yl * 4 + zh
            d = zl
            (xh, xl) = x `quotRem` 4
            (yh, yl) = y `quotRem` 16
            (zh, zl) = z `quotRem` 64
            enc = fromIntegral . ord . encodeB64Word

encodeB64Word :: Word8 -> Char
encodeB64Word n =
    if n < 64
        then base64Table ! n
        else error $ "encodeB64Word : " ++ show n

findInArray :: (Ix i, Eq e) => Array i e -> e -> Maybe i
findInArray a x =
    lookup x $ map swap $ assocs a

findb64 :: Char -> Word8
findb64 c =
    case findInArray base64Table c of
        Nothing -> error $ "findb64 : " ++ show c
        Just x -> x

-- | Given a base64-encoded string, return a bytestring with the same contents.
decodeBase64 :: String -> B.ByteString
decodeBase64 =
    B.concat . map (B.pack . fourToThree) . fourByFour

fourToThree :: (Char, Char, Char, Char) -> [Word8]
fourToThree (a, b, '=', '=') =
    take 1 $ fourToThreeW (findb64 a, findb64 b, 0, 0)
fourToThree (a, b, c, '=') =
    take 2 $ fourToThreeW (findb64 a, findb64 b, findb64 c, 0)
fourToThree (a, b, c, d) =
    fourToThreeW (findb64 a, findb64 b, findb64 c, findb64 d)

fourToThreeW :: (Word8, Word8, Word8, Word8) -> [Word8]
fourToThreeW (a, b, c, d) = [x, y, z]
    where
        -- This is not a guitar tab.
        -- | x x x x x x x x|y y y y y y y y|z z z z z z z z
        -- | a a a a a a b b|b b b b c c c c|c c d d d d d d
        -- |      a      bh | bl       ch   | cl     d
        x = a * 4 + bh
        y = bl * 16 + ch
        z = cl * 64 + d
        (bh, bl) = b `quotRem` 16
        (ch, cl) = c `quotRem` 4

fourByFour :: [a] -> [(a, a, a, a)]
fourByFour [] = []
fourByFour (a:b:c:d:r) = (a, b, c, d):fourByFour r
fourByFour _ = error "fourByFour"

-- | Wrapper around 'readFile' and 'decodeBase64' to read
-- a file encoded in Base64.
readFileBase64 :: FilePath -> IO B.ByteString
readFileBase64 path =
    decodeBase64 <$> concat <$> lines <$> readFile path

-- | Variant of 'readFileBase64' that decodes one bytestring per line of input.
readFileLinesBase64 :: FilePath -> IO [B.ByteString]
readFileLinesBase64 path = do
    map decodeBase64 <$> lines <$> readFile path
