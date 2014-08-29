import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Bits
import Data.Char
import Data.List
import Data.List.Split hiding (chunk)
import Data.Maybe
import Data.Ord
import Data.Word
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
import MersenneTwister
import Misc
import PaddingOracle
import SHA1
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

chall17key :: B.ByteString
chall17key = unHex "02f9 142f 6d14 c366 fd2c 8735 1fff 89a7"

c17checkPad :: B.ByteString -> B.ByteString -> Bool
c17checkPad cipher iv =
    isJust $ checkPadPkcs7 $ aes128decryptCBC chall17key iv cipher

chall17 :: Test
chall17 = "Challenge 17" ~: map tc
    [ "MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc="
    , "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic="
    , "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw=="
    , "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg=="
    , "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl"
    , "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA=="
    , "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw=="
    , "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8="
    , "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g="
    , "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"
    ]
        where
            tc plainb =
                let plain = decodeBase64 plainb in
                let iv = zeroIV in
                let cipher = aes128cryptCBC chall17key iv $ padPkcs7 plain in
                Just plain ~=? checkPadPkcs7 (padOracleAttack c17checkPad cipher iv)

chall18 :: Test
chall18 =
    "Challenge 18" ~: spec ~=? bs2string plain
        where
            cipher = decodeBase64 "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="
            key = string2bs "YELLOW SUBMARINE"
            nonce = zeroNonce
            plain = aes128decryptCTR key nonce cipher
            spec = "Yo, VIP Let's kick it Ice, Ice, baby Ice, Ice, baby "

chall19 :: Test
chall19 =
    "Challenge 19" ~:
        expectedRes ~=? plainsRecovered
            where
                expectedRes = map (B.take 16) plains
                -- It's hard to guess the case
                plainsRecovered = map (bsUcFirst . xorBufferRepeat keyRecovered) ciphers
                keyRecovered = B.pack $ map getByte [0..15]
                getByte i = fst $ maximumBy (comparing snd) (results i)
                results i = map (\ w -> let b = try i w in (w, englishness b)) [0..]
                try i w = B.map (xor w) (nthBytes i)
                nthBytes i = B.pack $ map (\ b -> B.index b i) ciphers
                key = unHex "42d4 5eb4 402a aac7 55b3 b7bc a21a 4098"
                nonce = zeroNonce
                ciphers = map (aes128cryptCTR key nonce) plains
                plains = map decodeBase64
                    [ "SSBoYXZlIG1ldCB0aGVtIGF0IGNsb3NlIG9mIGRheQ=="
                    , "Q29taW5nIHdpdGggdml2aWQgZmFjZXM="
                    , "RnJvbSBjb3VudGVyIG9yIGRlc2sgYW1vbmcgZ3JleQ=="
                    , "RWlnaHRlZW50aC1jZW50dXJ5IGhvdXNlcy4="
                    , "SSBoYXZlIHBhc3NlZCB3aXRoIGEgbm9kIG9mIHRoZSBoZWFk"
                    , "T3IgcG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA=="
                    , "T3IgaGF2ZSBsaW5nZXJlZCBhd2hpbGUgYW5kIHNhaWQ="
                    , "UG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA=="
                    , "QW5kIHRob3VnaHQgYmVmb3JlIEkgaGFkIGRvbmU="
                    , "T2YgYSBtb2NraW5nIHRhbGUgb3IgYSBnaWJl"
                    , "VG8gcGxlYXNlIGEgY29tcGFuaW9u"
                    , "QXJvdW5kIHRoZSBmaXJlIGF0IHRoZSBjbHViLA=="
                    , "QmVpbmcgY2VydGFpbiB0aGF0IHRoZXkgYW5kIEk="
                    , "QnV0IGxpdmVkIHdoZXJlIG1vdGxleSBpcyB3b3JuOg=="
                    , "QWxsIGNoYW5nZWQsIGNoYW5nZWQgdXR0ZXJseTo="
                    , "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4="
                    , "VGhhdCB3b21hbidzIGRheXMgd2VyZSBzcGVudA=="
                    , "SW4gaWdub3JhbnQgZ29vZCB3aWxsLA=="
                    , "SGVyIG5pZ2h0cyBpbiBhcmd1bWVudA=="
                    , "VW50aWwgaGVyIHZvaWNlIGdyZXcgc2hyaWxsLg=="
                    , "V2hhdCB2b2ljZSBtb3JlIHN3ZWV0IHRoYW4gaGVycw=="
                    , "V2hlbiB5b3VuZyBhbmQgYmVhdXRpZnVsLA=="
                    , "U2hlIHJvZGUgdG8gaGFycmllcnM/"
                    , "VGhpcyBtYW4gaGFkIGtlcHQgYSBzY2hvb2w="
                    , "QW5kIHJvZGUgb3VyIHdpbmdlZCBob3JzZS4="
                    , "VGhpcyBvdGhlciBoaXMgaGVscGVyIGFuZCBmcmllbmQ="
                    , "V2FzIGNvbWluZyBpbnRvIGhpcyBmb3JjZTs="
                    , "SGUgbWlnaHQgaGF2ZSB3b24gZmFtZSBpbiB0aGUgZW5kLA=="
                    , "U28gc2Vuc2l0aXZlIGhpcyBuYXR1cmUgc2VlbWVkLA=="
                    , "U28gZGFyaW5nIGFuZCBzd2VldCBoaXMgdGhvdWdodC4="
                    , "VGhpcyBvdGhlciBtYW4gSSBoYWQgZHJlYW1lZA=="
                    , "QSBkcnVua2VuLCB2YWluLWdsb3Jpb3VzIGxvdXQu"
                    , "SGUgaGFkIGRvbmUgbW9zdCBiaXR0ZXIgd3Jvbmc="
                    , "VG8gc29tZSB3aG8gYXJlIG5lYXIgbXkgaGVhcnQs"
                    , "WWV0IEkgbnVtYmVyIGhpbSBpbiB0aGUgc29uZzs="
                    , "SGUsIHRvbywgaGFzIHJlc2lnbmVkIGhpcyBwYXJ0"
                    , "SW4gdGhlIGNhc3VhbCBjb21lZHk7"
                    , "SGUsIHRvbywgaGFzIGJlZW4gY2hhbmdlZCBpbiBoaXMgdHVybiw="
                    , "VHJhbnNmb3JtZWQgdXR0ZXJseTo="
                    , "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4="
                    ]

chall20 :: Test
chall20 = "Challenge 20" ~: do
    plains <- readFileLinesBase64 "challenge-data/20.txt"
    let key = unHex "e135 ffa9 6610 66d7 62ec 854a 4a09 b3d4"
        nonce = zeroNonce
        ciphers = map (aes128cryptCTR key nonce) plains
        n = B.length $ minimumBy (comparing B.length) ciphers
        cutCiphers = map (B.take n) ciphers
        bigCipher = B.concat cutCiphers
        (_, _, result) = breakRepeatXor bigCipher n
        expectedResult = B.concat $ map (B.take n) plains
        dist = hammingDistance expectedResult result
        diffScore :: Float
        diffScore = fromIntegral dist / fromIntegral (B.length result)
    assert $ diffScore < 0.06

chall22 :: Test
chall22 =
    "Challenge 22" ~: Just 1408957948 ~=? recoverSeed (1408958118-1000) 1408958118 37359908

chall23 :: Test
chall23 = "Challenge 23" ~: do
    n <- (randomRIO (minBound, maxBound) :: IO Word32)
    let mt = createMT n
        mt' = execState nextMT mt
        l = map untemper $ evalState m mt'
        m = forM [1::Int ..624] $ \ _ -> nextMT
        mt2 = createMTfromState l
        go 0 _ _ = return ()
        go r mta mtb = do
            let (na, mta') = runState nextMT mta
                (nb, mtb') = runState nextMT mtb
            assertEqual "MT state is the same" na nb
            go (r-1) mta' mtb'
    go (10::Int) (execState nextMT mt') mt2

cc :: IO ()
cc = do
    n <- randomRIO (4, 20)
    prefix <- randomBytes n
    key <- randomIO
    let as = nTimesA 14
        plain = B.append prefix as
        cipher = mt19937cryptCTR key plain
    print cipher
    let try w = as `B.isSuffixOf` mt19937cryptCTR w cipher
    print $ head $ filter try [0..]

editCTR :: B.ByteString -> B.ByteString -> B.ByteString -> Int -> B.ByteString -> B.ByteString
editCTR key nonce cipher offset newtext =
    aes128cryptCTR key nonce plain'
        where
            plain' = spliceBS plain offset newtext
            plain = aes128decryptCTR key nonce cipher

chall25prepare :: IO (B.ByteString, B.ByteString -> Int -> B.ByteString -> B.ByteString)
chall25prepare = do
    let key0 = string2bs "YELLOW SUBMARINE"
        key = unHex "04d4 c154 c7f7 7650 cf37 64ed 10c4 e8c5"
        nonce = zeroNonce
    Just plain <- checkPadPkcs7
              <$> aes128decryptECB key0
              <$> readFileBase64 "challenge-data/25.txt"
    let cipher = aes128cryptCTR key nonce plain
    return (cipher, editCTR key nonce)

chall25 :: Test
chall25 = TestCase $ do
    (cipher, edit) <- chall25prepare
    let res = map (chr . fromIntegral) $ take 16 $ breakCTRedit cipher edit
    assertEqual "Challenge 25" "I'm back and I'm" res

chall29 :: Test
chall29 = "Challenge 29" ~:
    True ~=? ( checkSha1PrefixMac key extendedMsg extendedMac
            && extension `B.isSuffixOf` extendedMsg )
        where
            key = unHex "5940 986f 2ab6 27c6 0489 1290 a0e7 fa0c"
            message = string2bs "comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon"
            mac = sha1PrefixMac key message
            extension = string2bs ";admin=true"
            extendedMsg = B.append message suffix
            oracle = checkSha1PrefixMac key
            (suffix, extendedMac) = sha1Extend oracle message mac extension

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-c"] -> do
            checkAESProps
            checkMTProps
            checkMiscProps
            checkSHA1Props
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
            , chall15
            , chall16
            , chall17
            , chall18
            , chall19
            , chall20
            , mtTests
            , chall22
            , chall23
            , chall25
            , sha1Tests
            , chall29
            ]
