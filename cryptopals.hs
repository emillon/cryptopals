import Control.Monad
import Control.Monad.Writer
import Data.Array
import Data.Binary.Strict.BitGet
import Data.Char
import Data.Word
import Test.HUnit

import qualified Data.ByteString as B

bs2string :: B.ByteString -> String
bs2string = map (toEnum . fromEnum) . B.unpack

encodeB64Word :: Word8 -> Char
encodeB64Word n =
    if n < 64
        then a ! n
        else error $ "encodeB64Word : " ++ show n
    where
        a = listArray (0, 63) $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+' , '/']

hexToB64 :: B.ByteString -> B.ByteString
hexToB64 bs =
    either error B.pack $ runBitGet bs $ execWriterT getAsB64

getAsB64Chunk :: WriterT [Word8] BitGet ()
getAsB64Chunk = do
    n <- lift remaining
    case () of
        _ | n >= 24 -> get6 >> get6 >> get6 >> get6
        _ | n >= 16 -> get6 >> get6 >> get4 >> tpad
        _ | n >=  8 -> get6 >> get2 >> tpad >> tpad
        _ -> error $ "getAsB64 : " ++ show n
    where
        get2 = do
            x <- lift $ getAsWord8 2
            tell [enc (x * 16)]
        get4 = do
            x <- lift $ getAsWord8 4
            tell [enc (x * 4)]
        get6 = do
            x <- lift $ getAsWord8 6
            tell [enc x]
        enc :: Word8 -> Word8
        enc = fromIntegral . ord . encodeB64Word
        tpad = tell [fromIntegral $ ord '=']

getAsB64 :: WriterT [Word8] BitGet ()
getAsB64 = do
    e <- lift isEmpty
    unless e $ do
        getAsB64Chunk
        getAsB64

decodeHex :: String -> B.ByteString
decodeHex = B.pack . map (uncurry decodeHexPair) . twoByTwo

twoByTwo :: [a] -> [(a, a)]
twoByTwo [] = []
twoByTwo (a:b:l) = (a, b):twoByTwo l
twoByTwo [_] = error "twoByTwo"

decodeHexPair :: Char -> Char -> Word8
decodeHexPair h l =
    0x10 * decodeHexChar h + decodeHexChar l

decodeHexChar :: Char -> Word8
decodeHexChar = fromIntegral . digitToInt

tc :: String -> String -> Test
tc input spec =
    spec ~=? bs2string (hexToB64 (decodeHex input))

toHex :: String -> String
toHex =
    concatMap encodeHexPair

encodeHexPair :: Char -> String
encodeHexPair c = [h, l] where
    h = intToDigit q
    l = intToDigit r
    (q, r) = ord c `quotRem` 0x10

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
