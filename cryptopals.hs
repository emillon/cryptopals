import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Array
import Data.Binary.Strict.BitGet
import Data.Bits
import Data.Char
import Data.List
import Data.Ord
import Data.Word
import Test.HUnit

import qualified Data.ByteString as B
import qualified Data.Map as M

bs2string :: B.ByteString -> String
bs2string = map (toEnum . fromEnum) . B.unpack

string2bs :: String -> B.ByteString
string2bs = B.pack . map (toEnum . fromEnum)

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

toHex :: String -> String
toHex =
    concatMap encodeHexPairChar

encodeHexPairChar :: Char -> String
encodeHexPairChar = encodeHexPair . fromIntegral . ord

encodeHexPair :: Word8 -> String
encodeHexPair w = [h, l] where
    h = intToDigit $ fromIntegral q
    l = intToDigit $ fromIntegral r
    (q, r) = w `quotRem` 0x10

xorBuffer :: B.ByteString -> B.ByteString -> B.ByteString
xorBuffer a b =
    B.pack $ B.zipWith xor a b

chall01 :: Test
chall01 =
    "Challenge 01" ~: map (uncurry tc)
        [ ("" , "")
        , (toHex "Man" , "TWFu")
        , (toHex "pleasure.", "cGxlYXN1cmUu")
        , (toHex "leasure.", "bGVhc3VyZS4=")
        , (toHex "easure.", "ZWFzdXJlLg==")
        , ( "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
          , "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
          )
        ]
    where
        tc input spec =
            spec ~=? bs2string (hexToB64 (decodeHex input))

chall02 :: Test
chall02 =
    "Challenge 02" ~: map (uncurry3 tc)
        [ ("", "", "")
        , ( "746865206b696420646f6e277420706c6179"
          , "1c0111001f010100061a024b53535009181c"
          , "686974207468652062756c6c277320657965"
          )
        ]
    where
        tc plain key spec =
            spec ~=? encodeHex (xorBuffer (decodeHex plain) (decodeHex key))

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

englishness :: B.ByteString -> Float
englishness s =
    similarity (freq s) englishFreq

type Freq = M.Map Char Float

toCommonLetter :: Word8 -> Char
toCommonLetter = toLower . chr . fromIntegral

englishFreq :: Freq
englishFreq = M.fromList $
    [ (' ', 0.19182)
    , ('e', 0.13000)
    , ('t', 0.09056)
    , ('a', 0.08167)
    , ('o', 0.07507)
    , ('i', 0.06966)
    , ('n', 0.06749)
    , ('s', 0.06327)
    , ('h', 0.06094)
    , ('r', 0.05987)
    , ('d', 0.04253)
    , ('l', 0.04025)
    , ('c', 0.02782)
    , ('u', 0.02758)
    , ('m', 0.02406)
    , ('w', 0.02360)
    , ('f', 0.02228)
    , ('g', 0.02015)
    , ('y', 0.01974)
    , ('p', 0.01929)
    , ('b', 0.01492)
    , ('v', 0.00978)
    , ('k', 0.00772)
    , ('j', 0.00153)
    , ('x', 0.00150)
    , ('q', 0.00095)
    , ('z', 0.00074)
    ]
    ++ map (\ w -> (w, -0.1)) (['\000'..'\031'] ++ ['\127'..'\255'])

freq :: B.ByteString -> Freq
freq b =
    M.map (\ c -> c / n) letterCount
        where
            letterCount = B.foldr go M.empty b
            go w m = M.insertWith (+) (toCommonLetter w) 1.0 m
            n = fromIntegral $ B.length b

norm :: Freq -> Float
norm f = sqrt $ sum $ map (\ l -> (M.findWithDefault 0.0 l f)^(2::Int)) ['a'..'z']

similarity  :: Freq -> Freq -> Float
similarity a b =
    let na = norm a in
    let nb = norm b in
    if na == 0.0 || nb == 0.0
        then -1.0
        else dot a b / (na * nb)

dot :: Freq -> Freq -> Float
dot a b =
    sum $ map (\ l -> find0 a l * find0 b l) [toEnum 0..toEnum 255]
        where
            find0 m k = M.findWithDefault 0.0 k m

findXorKey :: B.ByteString -> (Word8, Float, B.ByteString)
findXorKey b =
    maximumBy (comparing snd3) $ map f [0..]
        where
            n = B.length b
            makeKey k = B.replicate n k
            f k =
                let plain = xorBuffer b (makeKey k) in
                (k, englishness plain, plain)

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

chall03 :: Test
chall03 =
    "Challenge 03" ~:
        [ LT ~=? comparing (englishness . string2bs) "aa*ù$°+)kl!" "hello everybody"
        , 88 ~=? fst3 (findXorKey chall03text)
        ]

chall03text :: B.ByteString
chall03text = decodeHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

chall04 :: Test
chall04 = TestCase $ do
    ciphers <- lines <$> readFile "challenge-data/4.txt"
    let plains = map (\ (n, cipher) -> (n, (findXorKey $ decodeHex cipher))) $ zip [1::Int ..] ciphers
    let (n, (key, _, _)) = maximumBy (comparing (snd3 . snd)) plains
    assertEqual "Challenge 04" (171, 53) (n, key)

chall05 :: Test
chall05 =
    "Challenge 04" ~: map (uncurry3 tc)
        [ (B.empty, B.empty, "")
        ]
        where
            tc input key spec =
                spec ~=? encodeHex (xorBufferRepeat input key)

xorBufferRepeat :: B.ByteString -> B.ByteString -> B.ByteString
xorBufferRepeat input key =
    input

main :: IO ()
main =
    void $ runTestTT $ TestList
        [ chall01
        , chall02
        , chall03
        , chall05
        ]
