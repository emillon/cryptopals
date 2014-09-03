module Set3 ( chall17
            , chall18
            , chall19
            , chall20
            , chall22
            , chall23
            ) where

import Control.Monad.State
import Data.Bits
import Data.List
import Data.Maybe
import Data.Ord
import Data.Word
import System.Random
import Test.HUnit

import qualified Data.ByteString as B

import AES
import Base64
import LetterFreq
import MersenneTwister
import Misc
import PaddingOracle
import XOR

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
