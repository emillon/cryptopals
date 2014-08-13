import Control.Monad
import Data.Binary.Strict.BitGet
import Data.Word
import Test.HUnit

import qualified Data.ByteString as B

encodeB64 :: B.ByteString -> String
encodeB64 bs =
    B.foldr go [] bs
        where
            go w l =
                encodeB64Word w : l

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
encodeB64Word _ = error "encodeB64Word"

hexToB64 :: B.ByteString -> B.ByteString
hexToB64 bs =
    either error B.pack $ runBitGet bs getAsB64

getAsB64 :: BitGet [Word8]
getAsB64 = do
    n <- remaining
    case n of
        0 -> return []
        _ | n >= 6 -> do
            d <- getAsWord8 6
            r <- getAsB64
            return $ d : r
        _ -> error $ "getAsB64 : " ++ show n


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
    spec ~=? encodeB64 (hexToB64 (decodeHex input))

main :: IO ()
main =
    void $ runTestTT $ TestList $ map (uncurry tc)
        [ ( ""
          , ""
          )
        , ( "4d616e"
          , "TWFu"
          )
        ]
