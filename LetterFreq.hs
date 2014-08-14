module LetterFreq (englishness) where

import Data.Char
import Data.Word

import qualified Data.ByteString as B
import qualified Data.Map as M

englishness :: B.ByteString -> Float
englishness s =
    similarity (freq s) englishFreq

type Freq = Letter -> Float

data Letter = LetterA
            | LetterB
            | LetterC
            | LetterD
            | LetterE
            | LetterF
            | LetterG
            | LetterH
            | LetterI
            | LetterJ
            | LetterK
            | LetterL
            | LetterM
            | LetterN
            | LetterO
            | LetterP
            | LetterQ
            | LetterR
            | LetterS
            | LetterT
            | LetterU
            | LetterV
            | LetterW
            | LetterX
            | LetterY
            | LetterZ
            | LetterSpace
    deriving (Eq, Enum, Ord)

letters :: [Letter]
letters = [toEnum 0..]

toLetter :: Word8 -> Maybe Letter
toLetter w =
    let c = toLower (chr (fromIntegral w)) in
    case c of
        'a' -> Just LetterA
        'b' -> Just LetterB
        'c' -> Just LetterC
        'd' -> Just LetterD
        'e' -> Just LetterE
        'f' -> Just LetterF
        'g' -> Just LetterG
        'h' -> Just LetterH
        'i' -> Just LetterI
        'j' -> Just LetterJ
        'k' -> Just LetterK
        'l' -> Just LetterL
        'm' -> Just LetterM
        'n' -> Just LetterN
        'o' -> Just LetterO
        'p' -> Just LetterP
        'q' -> Just LetterQ
        'r' -> Just LetterR
        's' -> Just LetterS
        't' -> Just LetterT
        'u' -> Just LetterU
        'v' -> Just LetterV
        'w' -> Just LetterW
        'x' -> Just LetterX
        'y' -> Just LetterY
        'z' -> Just LetterZ
        ' ' -> Just LetterSpace
        _ -> Nothing

englishFreq :: Freq
englishFreq LetterSpace = 0.19182
englishFreq LetterE = 0.13000
englishFreq LetterT = 0.09056
englishFreq LetterA = 0.08167
englishFreq LetterO = 0.07507
englishFreq LetterI = 0.06966
englishFreq LetterN = 0.06749
englishFreq LetterS = 0.06327
englishFreq LetterH = 0.06094
englishFreq LetterR = 0.05987
englishFreq LetterD = 0.04253
englishFreq LetterL = 0.04025
englishFreq LetterC = 0.02782
englishFreq LetterU = 0.02758
englishFreq LetterM = 0.02406
englishFreq LetterW = 0.02360
englishFreq LetterF = 0.02228
englishFreq LetterG = 0.02015
englishFreq LetterY = 0.01974
englishFreq LetterP = 0.01929
englishFreq LetterB = 0.01492
englishFreq LetterV = 0.00978
englishFreq LetterK = 0.00772
englishFreq LetterJ = 0.00153
englishFreq LetterX = 0.00150
englishFreq LetterQ = 0.00095
englishFreq LetterZ = 0.00074

freq :: B.ByteString -> Freq
freq b l =
    M.findWithDefault 0.0 l letterCount / n
        where
            letterCount = B.foldr go M.empty b
            go w m =
                case toLetter w of
                    Nothing -> m
                    Just le -> M.insertWith (+) le 1.0 m
            n = fromIntegral $ B.length b

norm :: Freq -> Float
norm f = sqrt $ sum $ map (\ l -> (f l)^(2::Int)) letters

similarity  :: Freq -> Freq -> Float
similarity a b =
    let d = dot a b in
    if d == 0.0
        then -1.0
        else d / (norm a * norm b)

dot :: Freq -> Freq -> Float
dot a b =
    sum $ map (\ l -> a l * b l) letters
