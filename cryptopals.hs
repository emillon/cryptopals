import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split hiding (chunk)
import Data.Maybe
import Data.Ord
import System.Environment
import System.Random
import Test.HUnit

import qualified Data.ByteString as B
import qualified Data.Map as M

import AES
import Base64
import ByteAtATime
import KeyValue
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

chall07 :: Test
chall07 = "Challenge 07" ~: do
    cipher <- readFileBase64 "challenge-data/7.txt"
    let key = string2bs "YELLOW SUBMARINE"
        plain = aes128decryptECB key cipher
    assert $ string2bs "Play that funky music" `B.isInfixOf` plain

isECB :: B.ByteString -> Bool
isECB =
    any (> 1) . M.elems . foldr go M.empty . chunksOfSize 16
        where
            go chunk m =
                M.insertWith (+) chunk (1::Int) m

chall08 :: Test
chall08 = "Challenge 08" ~: do
    c <- readFile "challenge-data/8.txt"
    let r = map fst $ filter (isECB . snd) $ zip [0::Int ..] $ map decodeBase64 $ lines c
    assert $ r == [132]

chall09 :: Test
chall09 = "Challenge 09" ~: map (uncurry tc)
    [ (""                , "10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10")
    , ("A"               , "41 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f")
    , ("AA"              , "41 41 0e 0e 0e 0e 0e 0e 0e 0e 0e 0e 0e 0e 0e 0e")
    , ("AAA"             , "41 41 41 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d")
    , ("AAAA"            , "41 41 41 41 0c 0c 0c 0c 0c 0c 0c 0c 0c 0c 0c 0c")
    , ("AAAAB"           , "41 41 41 41 42 0b 0b 0b 0b 0b 0b 0b 0b 0b 0b 0b")
    , ("AAAABB"          , "41 41 41 41 42 42 0a 0a 0a 0a 0a 0a 0a 0a 0a 0a")
    , ("AAAABBB"         , "41 41 41 41 42 42 42 09 09 09 09 09 09 09 09 09")
    , ("AAAABBBB"        , "41 41 41 41 42 42 42 42 08 08 08 08 08 08 08 08")
    , ("AAAABBBBC"       , "41 41 41 41 42 42 42 42 43 07 07 07 07 07 07 07")
    , ("AAAABBBBCC"      , "41 41 41 41 42 42 42 42 43 43 06 06 06 06 06 06")
    , ("AAAABBBBCCC"     , "41 41 41 41 42 42 42 42 43 43 43 05 05 05 05 05")
    , ("AAAABBBBCCCC"    , "41 41 41 41 42 42 42 42 43 43 43 43 04 04 04 04")
    , ("AAAABBBBCCCCD"   , "41 41 41 41 42 42 42 42 43 43 43 43 44 03 03 03")
    , ("AAAABBBBCCCCDD"  , "41 41 41 41 42 42 42 42 43 43 43 43 44 44 02 02")
    , ("AAAABBBBCCCCDDD" , "41 41 41 41 42 42 42 42 43 43 43 43 44 44 44 01")
    , ("AAAABBBBCCCCDDDD", "41 41 41 41 42 42 42 42 43 43 43 43 44 44 44 44"
                        ++ "10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10")
    ]
    where
        tc input spec =
            unHex spec ~=? padPkcs7 (string2bs input)

padSpaces :: String -> String
padSpaces l =
    l ++ replicate n ' '
        where
            n = case length l `mod` 16 of
                0 -> 0
                x -> 16 - x

chall10 :: Test
chall10 = "Challenge 10" ~: do
    cipher <- readFileBase64 "challenge-data/10.txt"
    let key = string2bs "YELLOW SUBMARINE"
        iv = B.replicate 16 0
        plain = aes128decryptCBC key iv cipher
    assert $ string2bs "Let the witch doctor" `B.isInfixOf` plain

data AESMode = ECB | CBC deriving (Eq, Show)

aesRandomMode :: B.ByteString -> IO (B.ByteString, AESMode)
aesRandomMode input = do
    modeIsECB <- randomIO
    key <- randomBytes 16
    nbefore <- randomRIO (5, 10)
    nafter <- randomRIO (5, 10)
    before <- randomBytes nbefore
    after <- randomBytes nafter
    let plain = padPkcs7 $ B.concat [before, input, after]
    if modeIsECB
        then
            return (aes128cryptECB key plain, ECB)
        else do
            iv <- randomBytes 16
            return (aes128cryptCBC key iv plain, CBC)

aesECBoracle :: IO Bool
aesECBoracle = do
    let nplain = 3 * 16 -- so that we fully control blocks 2 & 3
        plain = B.replicate nplain 0
    (cipher, actualMode) <- aesRandomMode plain
    let detectedMode = if isECB cipher then ECB else CBC
    return $ actualMode == detectedMode

chall11 :: Test
chall11 = "Challenge 11" ~: do
    results <- mapM (\ _ -> aesECBoracle) [1::Int ..100]
    assert $ and results

chall12target :: B.ByteString
chall12target = decodeBase64 $ concat
    [ "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg"
    , "aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq"
    , "dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg"
    , "YnkK"
    ]

chall12 :: Test
chall12 = "Challenge 12" ~:
    unknownBytes (aesUnknown chall12target) ~=? chall12target

chall13 :: Test
chall13 = "Challenge 13" ~: map (uncurry tc)
    [ ( ""
      , M.empty
      )
    , ( "foo=bar&baz=qux&zap=zazzle"
      , M.fromList
          [ ("foo", "bar")
          , ("baz", "qux")
          , ("zap", "zazzle")
          ]
      )
    ]
    ++ [chall13accept]
        where
            tc input spec =
                spec ~=? parseKeyValue input

chall13key :: B.ByteString
chall13key = unHex "2598 42aa dc49 cc6f 4b18 68e1 8c2f 5d25"

c13encode :: String -> B.ByteString
c13encode =
    aes128cryptECB chall13key . string2bs . padSpaces . profileFor

c13decode :: B.ByteString -> M.Map String String
c13decode bs =
    parseKeyValue $ bs2string $ aes128decryptECB chall13key bs

chall13accept :: Test
chall13accept = "Challenge 13 acceptance" ~: do
    cookie M.! "role" ~=? "admin"
        where
            egen f = head $ mapMaybe (gen f) [1..16]
            gen ok n = do
                let email = replicate n 'A' ++ "admin"
                i <- findIndex ok $ chunksOf 16 $ profileFor email
                return (email, i)
            (e1, i1) = egen $ \ b -> "role=" `isSuffixOf` b
            (e2, i2) = egen $ \ b -> "admin" `isPrefixOf` b
            (e3, i3) = egen $ \ b -> "uid=" `isPrefixOf` b
            c1 = B.take (16*(i1+1)) $ c13encode e1
            c2 = nthChunk 16 i2 $ c13encode e2
            c3 = nthChunk 16 i3 $ c13encode e3
            cookie = c13decode $ B.concat [c1, c2, c3]

chall14 :: Test
chall14 = "Challenge 14" ~: do
    target <- unknownBytesRandom
    assert $ target == chall14target

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-c"] -> void $ checkAESProps
        _ -> void $ runTestTT $ TestList
            [ chall01
            , chall02
            , chall03
            , chall05
            , chall06
            , aesTests
            , chall07
            , chall08
            , chall09
            , chall10
            , chall11
            , chall12
            , chall13
            ]
