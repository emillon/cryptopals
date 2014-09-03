import Control.Monad
import Data.Maybe
import System.Environment
import Test.HUnit

import AES
import Digest
import MersenneTwister
import Misc

import Set1
import Set2
import Set3
import Set4

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
