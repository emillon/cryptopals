import Control.Applicative
import Control.Monad
import Data.Bits
import Data.List
import Data.Ord
import Data.Word
import Test.HUnit

import qualified Data.ByteString as B

import AES
import Base64
import LetterFreq
import Misc
import XOR

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

findXorKey :: B.ByteString -> (Word8, Float, B.ByteString)
findXorKey b =
    maximumBy (comparing snd3) $ map f [0..]
        where
            n = B.length b
            makeKey k = B.replicate n k
            f k = let plain = xorBuffer b (makeKey k) in
                (k, englishness plain, plain)

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
    "Challenge 05" ~: map (uncurry3 tc)
        [ (B.empty, B.empty, "")
        , ( string2bs "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
          , string2bs "ICE"
          , "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272"
           ++ "a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
          )
        ]
        where
            tc input key spec =
                spec ~=? encodeHex (xorBufferRepeat input key)

xorBufferRepeat :: B.ByteString -> B.ByteString -> B.ByteString
xorBufferRepeat input key =
    B.pack $ map (\ (i, w) -> xor w (B.index key (i `mod` n))) $ zip [0..] $ B.unpack input
        where
            n = B.length key

chall06 :: Test
chall06 =
    "Challenge 06" ~: map (uncurry3 hammingTc)
        [ ( string2bs "xyz"
          , string2bs "xyz"
          , 0)
        , ( string2bs "this is a test"
          , string2bs "wokka wokka!!!"
          , 37
          )
        ]
    ++ map (uncurry decode64tc)
        [ ("cGxlYXN1cmUu", "pleasure.")
        , ("bGVhc3VyZS4=", "leasure.")
        , ("ZWFzdXJlLg==", "easure.")
        ]
    ++ [acceptance]
    where
        hammingTc a b spec =
            spec ~=? hammingDistance a b
        decode64tc input spec =
            spec ~=? bs2string (decodeBase64 input)
        acceptance = TestCase $ do
            d <- readFileBase64 "challenge-data/6.txt"
            let (key, _, _) = breakRepeatXorAuto $ d
            assertEqual "Vigenere" (string2bs "Terminator X: Bring the noise") key

hammingDistance :: B.ByteString -> B.ByteString -> Int
hammingDistance a b = sum $ B.zipWith hammingDistanceWord8 a b

hammingDistanceWord8 :: Word8 -> Word8 -> Int
hammingDistanceWord8 a b =
    popCount $ a `xor` b

tryKeySize :: Int -> B.ByteString -> Int
tryKeySize n bs =
    sum $ map (\ (x, y) -> hammingDistance x y) chunkPairs
        where
            a = chunk 0
            b = chunk 1
            c = chunk 2
            d = chunk 3
            chunk i = B.take n $ B.drop (i * n) bs
            chunkPairs = [ (a, b), (a, c), (a, d) , (b, c), (c, d) , (c, d) ]


scoreKeySize :: Int -> B.ByteString -> Float
scoreKeySize ks b =
    fromIntegral dist / fromIntegral ks
        where
            dist = tryKeySize ks b

keysizeCandidates :: B.ByteString -> [Int]
keysizeCandidates b =
    take 4 $ map fst $ sortBy (comparing snd) $ map (\ ks -> (ks, scoreKeySize ks b)) [2..40]

breakRepeatXorAuto :: B.ByteString -> (B.ByteString, Float, B.ByteString)
breakRepeatXorAuto b =
    maximumBy (comparing snd3) $ map f $ map (breakRepeatXor b) $ keysizeCandidates b
        where
            f k =
                let plain = xorBufferRepeat b k in
                (k, englishness plain, plain)

breakRepeatXor :: B.ByteString -> Int -> B.ByteString
breakRepeatXor b ks =
    key
        where
            cs = chunksOfSize ks b
            tcs = transposeChunks cs
            keyWords = map (\ tc -> fst3 (findXorKey tc)) tcs
            key = B.pack keyWords

transposeChunks :: [B.ByteString] -> [B.ByteString]
transposeChunks cs =
    map (\ i -> B.pack $ map (\ c -> index0 c i) cs) [0..m - 1]
        where
            m = B.length (head cs)

index0 :: B.ByteString -> Int -> Word8
index0 b i =
    if i < B.length b
        then B.index b i
        else 0

chall07 :: Test
chall07 = "Challenge 07" ~: do
    cipher <- readFileBase64 "challenge-data/7.txt"
    let key = string2bs "YELLOW SUBMARINE"
        plain = aes128decryptECB key cipher
    assert $ string2bs "Play that funky music" `B.isInfixOf` plain

main :: IO ()
main = do
    void $ runTestTT $ TestList
        [ chall01
        , chall02
        , chall03
        , chall05
        , chall06
        , aesTests
        , chall07
        ]
