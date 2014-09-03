import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import System.Environment
import Test.HUnit

import qualified Data.ByteString as B

import AES
import Base64
import Digest
import MersenneTwister
import Misc
import XOR

import Set1
import Set2
import Set3

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

c27key :: B.ByteString
c27key = unHex "8880 68a8 5e95 2022 897c 3268 a30e 2fdc"

c27encrypt :: B.ByteString -> B.ByteString
c27encrypt input =
    aes128cryptCBC c27key c27key input

c27decrypt :: B.ByteString -> Maybe B.ByteString
c27decrypt cookie =
    let plain = aes128decryptCBC c27key c27key cookie in
    if B.any (>= 0x80) plain then
       Just plain
    else
        Nothing

chall27 :: Test
chall27 =
    "Challenge 27" ~: c27key ~=? recKey
        where
            plain = B.concat [ B.replicate 16 0x41
                             , B.replicate 16 0x42
                             ]
            cipher = c27encrypt plain
            c1 = nthChunk 16 0 cipher
            cipher' = B.concat [ c1
                               , B.replicate 16 0x00
                               , c1
                               ]
            plain' = fromJust $ c27decrypt cipher'
            p'1 = nthChunk 16 0 plain'
            p'3 = nthChunk 16 2 plain'
            recKey = p'1 `xorBuffer` p'3

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

chall30 :: Test
chall30 = "Challenge 30" ~:
    True ~=? ( checkMd4PrefixMac key extendedMsg extendedMac
            && extension `B.isSuffixOf` extendedMsg )
        where
            key = unHex "fd8a b5ec f235 c17a fa58 7134 2544 15e7"
            message = string2bs "comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon"
            mac = md4PrefixMac key message
            extension = string2bs ";admin=true"
            extendedMsg = B.append message suffix
            oracle = checkMd4PrefixMac key
            (suffix, extendedMac) = md4Extend oracle message mac extension

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-c"] -> do
            checkAESProps
            checkMTProps
            checkMiscProps
            checkDigestProps
        [n] -> void $ runTestTT $ fromJust $ lookup n tests
        _ -> void $ runTestTT $ TestList $ map snd tests
        where
            tests = [ ("1", chall01)
                    , ("2", chall02)
                    , ("3", chall03)
                    , ("5", chall05)
                    , ("6", chall06)
                    , ("aes", aesTests)
                    , ("7", chall07)
                    , ("8", chall08)
                    , ("9", chall09)
                    , ("10", chall10)
                    , ("11", chall11)
                    , ("12", chall12)
                    , ("13", chall13)
                    , ("15", chall15)
                    , ("16", chall16)
                    , ("17", chall17)
                    , ("18", chall18)
                    , ("19", chall19)
                    , ("20", chall20)
                    , ("mt", mtTests)
                    , ("22", chall22)
                    , ("23", chall23)
                    , ("25", chall25)
                    , ("27", chall27)
                    , ("sha1", sha1Tests)
                    , ("29", chall29)
                    , ("md4", md4Tests)
                    ]
