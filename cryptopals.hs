import Control.Monad
import Data.Binary.Strict.BitGet
import Data.Char
import Data.Word
import Test.HUnit

import qualified Data.ByteString as B

bs2string :: B.ByteString -> String
bs2string = map toChar . B.unpack
    where
        toChar :: Word8 -> Char
        toChar = toEnum . fromEnum

encodeB64Word :: Word8 -> Char
encodeB64Word 0  = 'A'
encodeB64Word 1  = 'B'
encodeB64Word 2  = 'C'
encodeB64Word 3  = 'D'
encodeB64Word 4  = 'E'
encodeB64Word 5  = 'F'
encodeB64Word 6  = 'G'
encodeB64Word 7  = 'H'
encodeB64Word 8  = 'I'
encodeB64Word 9  = 'J'
encodeB64Word 10 = 'K'
encodeB64Word 11 = 'L'
encodeB64Word 12 = 'M'
encodeB64Word 13 = 'N'
encodeB64Word 14 = 'O'
encodeB64Word 15 = 'P'
encodeB64Word 16 = 'Q'
encodeB64Word 17 = 'R'
encodeB64Word 18 = 'S'
encodeB64Word 19 = 'T'
encodeB64Word 20 = 'U'
encodeB64Word 21 = 'V'
encodeB64Word 22 = 'W'
encodeB64Word 23 = 'X'
encodeB64Word 24 = 'Y'
encodeB64Word 25 = 'Z'
encodeB64Word 26 = 'a'
encodeB64Word 27 = 'b'
encodeB64Word 28 = 'c'
encodeB64Word 29 = 'd'
encodeB64Word 30 = 'e'
encodeB64Word 31 = 'f'
encodeB64Word 32 = 'g'
encodeB64Word 33 = 'h'
encodeB64Word 34 = 'i'
encodeB64Word 35 = 'j'
encodeB64Word 36 = 'k'
encodeB64Word 37 = 'l'
encodeB64Word 38 = 'm'
encodeB64Word 39 = 'n'
encodeB64Word 40 = 'o'
encodeB64Word 41 = 'p'
encodeB64Word 42 = 'q'
encodeB64Word 43 = 'r'
encodeB64Word 44 = 's'
encodeB64Word 45 = 't'
encodeB64Word 46 = 'u'
encodeB64Word 47 = 'v'
encodeB64Word 48 = 'w'
encodeB64Word 49 = 'x'
encodeB64Word 50 = 'y'
encodeB64Word 51 = 'z'
encodeB64Word 52 = '0'
encodeB64Word 53 = '1'
encodeB64Word 54 = '2'
encodeB64Word 55 = '3'
encodeB64Word 56 = '4'
encodeB64Word 57 = '5'
encodeB64Word 58 = '6'
encodeB64Word 59 = '7'
encodeB64Word 60 = '8'
encodeB64Word 61 = '9'
encodeB64Word 62 = '+'
encodeB64Word 63 = '/'
encodeB64Word n = error $ "encodeB64Word : " ++ show n

hexToB64 :: B.ByteString -> B.ByteString
hexToB64 bs =
    either error B.pack $ runBitGet bs getAsB64

getAsB64 :: BitGet [Word8]
getAsB64 = do
    n <- remaining
    case n of
        0 -> return []
        _ | n >= 24 -> do
            a <- get6
            b <- get6
            c <- get6
            d <- get6
            r <- getAsB64
            return $ enc a : enc b : enc c : enc d : r
        _ | n >= 16 -> do
            a <- get6
            b <- get6
            c <- get4
            r <- getAsB64
            return $ enc a : enc b : enc c : pad : r
        _ | n >= 8 -> do
            a <- get6
            b <- get2
            r <- getAsB64
            return $ enc a : enc b : pad : pad : r
        _ -> error $ "getAsB64 : " ++ show n
    where
        get2 = do
            x <- getAsWord8 2
            return $ x * 16
        get4 = do
            x <- getAsWord8 4
            return $ x * 4
        get6 = getAsWord8 6
        enc :: Word8 -> Word8
        enc = fromIntegral . ord . encodeB64Word
        pad :: Word8
        pad = fromIntegral $ ord '='

decodeHex :: String -> B.ByteString
decodeHex s =
    foldr go B.empty $ twoByTwo s
        where
            go (h, l) bs =
                B.cons (decodeHexPair h l) bs -- FIXME quadratic complexity

twoByTwo :: [a] -> [(a, a)]
twoByTwo [] = []
twoByTwo (a:b:l) = (a, b):twoByTwo l
twoByTwo [_] = error "twoByTwo"

decodeHexPair :: Char -> Char -> Word8
decodeHexPair h l =
    0x10 * decodeHexChar h + decodeHexChar l

decodeHexChar :: Char -> Word8
decodeHexChar '0' = 0x0
decodeHexChar '1' = 0x1
decodeHexChar '2' = 0x2
decodeHexChar '3' = 0x3
decodeHexChar '4' = 0x4
decodeHexChar '5' = 0x5
decodeHexChar '6' = 0x6
decodeHexChar '7' = 0x7
decodeHexChar '8' = 0x8
decodeHexChar '9' = 0x9
decodeHexChar 'a' = 0xa
decodeHexChar 'b' = 0xb
decodeHexChar 'c' = 0xc
decodeHexChar 'd' = 0xd
decodeHexChar 'e' = 0xe
decodeHexChar 'f' = 0xf
decodeHexChar c = error $ "decodeHexChar: " ++ show c

tc :: String -> String -> Test
tc input spec =
    spec ~=? bs2string (hexToB64 (decodeHex input))

toHex :: String -> String
toHex =
    concatMap encodeHexPair

encodeHexPair :: Char -> String
encodeHexPair c = [h, l] where
    h = encodeHexChar q
    l = encodeHexChar r
    (q, r) = ord c `quotRem` 0x10

encodeHexChar :: Int -> Char
encodeHexChar 0x0 = '0'
encodeHexChar 0x1 = '1'
encodeHexChar 0x2 = '2'
encodeHexChar 0x3 = '3'
encodeHexChar 0x4 = '4'
encodeHexChar 0x5 = '5'
encodeHexChar 0x6 = '6'
encodeHexChar 0x7 = '7'
encodeHexChar 0x8 = '8'
encodeHexChar 0x9 = '9'
encodeHexChar 0xa = 'a'
encodeHexChar 0xb = 'b'
encodeHexChar 0xc = 'c'
encodeHexChar 0xd = 'd'
encodeHexChar 0xe = 'e'
encodeHexChar 0xf = 'f'
encodeHexChar _ = error "encodeHexChar"

main :: IO ()
main =
    void $ runTestTT $ TestList $ map (uncurry tc)
        [ ("" , "")
        , (toHex "Man" , "TWFu")
        , (toHex "pleasure.", "cGxlYXN1cmUu")
        , (toHex "leasure.", "bGVhc3VyZS4=")
        , (toHex "easure.", "ZWFzdXJlLg==")
        , ( "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
          , "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
          )
        ]
