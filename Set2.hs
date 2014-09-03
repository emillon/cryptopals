module Set2 ( chall09
            , chall10
            , chall11
            , chall12
            , chall13
            , chall14
            , chall15
            , chall16
            ) where

import Data.Bits
import Data.List
import Data.List.Split
import Data.Maybe
import System.Random
import Test.HUnit

import qualified Data.ByteString as B
import qualified Data.Map as M

import AES
import Base64
import ByteAtATime
import KeyValue
import Misc

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
        plain = aes128decryptCBC key zeroIV cipher
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

chall14target :: B.ByteString
chall14target = decodeBase64 $ concat
    [ "T3UgdCdldGFpcz8KSW52aXRlIHBhciBkZXMgcG90ZXMgYSB1bmUgdGV1ZiBldCBvbiBzJ2FtdXNh"
    , "aXQgYmllbgpKZSBuJ2FpIHBhcyB2dSBsJ2hldXJlIHF1J2lsIGV0YWl0Cg=="
    ]

chall14 :: Test
chall14 = "Challenge 14" ~: do
    target <- unknownBytesRandom $ aesUnknownRandom chall14target
    assert $ target == chall14target

chall15 :: Test
chall15 =
    "Challenge 15" ~: map (uncurry tc)
        [ ("49 43 45 20 49 43 45 20 42 41 42 59 04 04 04 04", Just "4943 4520 4943 4520 4241 4259")
        , ("49 43 45 20 49 43 45 20 42 41 42 59 05 05 05 05", Nothing)
        , ("49 43 45 20 49 43 45 20 42 41 42 59 01 02 03 04", Nothing)
        , ("4c 4f 4c 20 4c 4f 4c 20 4c 4f 4c 20 4c 4f 4c 20", Nothing)
        , ("4c 4f 4c 20 4c 4f 4c 20 4c 4f 4c 20 4c 4f 4c 20"
        ++ "10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10"
          , Just "4c 4f 4c 20 4c 4f 4c 20 4c 4f 4c 20 4c 4f 4c 20"
          )
        , ("4c 4f 4c 20 4c 4f 4c 20 4c 4f 4c 20 4c 4f 4c 00", Nothing)
        ]
    where
        tc input spec =
            fmap unHex spec ~=? checkPadPkcs7 (unHex input)

makeCookie :: B.ByteString -> B.ByteString
makeCookie input =
    aes128cryptCBC chall16key zeroIV $ padPkcs7 $ B.concat [before, quote input, after]
        where
            before = string2bs "comment1=cooking%20MCs;userdata="
            after = string2bs ";comment2=%20like%20a%20pound%20of%20bacon"
            quote = B.concatMap quoteChar
            quoteChar 59 = string2bs "%3B" -- ';'
            quoteChar 61 = string2bs "%3D" -- '='
            quoteChar c = B.singleton c

chall16key :: B.ByteString
chall16key = unHex "c8a8 07d3 19c3 00a7 ac4e fcd9 e8da c71a"

validateCookie :: B.ByteString -> Bool
validateCookie cookie =
    case checkPadPkcs7 $ aes128decryptCBC chall16key zeroIV cookie of
        Nothing -> False
        Just msg -> string2bs ";admin=true;" `B.isInfixOf` msg

bitFlip :: Int -> Int -> B.ByteString -> B.ByteString
bitFlip n b bs =
    bsMapIdx f bs
        where
            f i w | i /= n  = w
            f _ w =  w `complementBit` b

chall16 :: Test
chall16 =
    "Challenge 16" ~: assert $ validateCookie c'
        where
            c = makeCookie $ string2bs "AAAAAAAAAAAAAAAA:admin<true"
            c' = bitFlip 32 0 $ bitFlip 38 0 c
