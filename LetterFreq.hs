-- | Analyze the frequency of letters in a bytestring.

module LetterFreq ( englishness
                  , printability
                  ) where

import Data.Char
import Data.Word

import qualified Data.ByteString as B
import qualified Data.Map as M

-- | Score how the likelihood that a bytestring is english test.
-- The value does not matter, but higher is more likely.
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
            | LetterOther
    deriving (Eq, Enum, Ord)

letters :: [Letter]
letters = [toEnum 0..]

toLetter :: Word8 -> Letter
toLetter w =
    let c = toLower (chr (fromIntegral w)) in
    case c of
        'a' -> LetterA
        'b' -> LetterB
        'c' -> LetterC
        'd' -> LetterD
        'e' -> LetterE
        'f' -> LetterF
        'g' -> LetterG
        'h' -> LetterH
        'i' -> LetterI
        'j' -> LetterJ
        'k' -> LetterK
        'l' -> LetterL
        'm' -> LetterM
        'n' -> LetterN
        'o' -> LetterO
        'p' -> LetterP
        'q' -> LetterQ
        'r' -> LetterR
        's' -> LetterS
        't' -> LetterT
        'u' -> LetterU
        'v' -> LetterV
        'w' -> LetterW
        'x' -> LetterX
        'y' -> LetterY
        'z' -> LetterZ
        ' ' -> LetterSpace
        _ -> LetterOther

letterFreq :: Float
letterFreq = 0.95

englishFreq :: Freq
englishFreq LetterA = letterFreq * 0.0651738
englishFreq LetterB = letterFreq * 0.0124248
englishFreq LetterC = letterFreq * 0.0217339
englishFreq LetterD = letterFreq * 0.0349835
englishFreq LetterE = letterFreq * 0.1041442
englishFreq LetterF = letterFreq * 0.0197881
englishFreq LetterG = letterFreq * 0.0158610
englishFreq LetterH = letterFreq * 0.0492888
englishFreq LetterI = letterFreq * 0.0558094
englishFreq LetterJ = letterFreq * 0.0009033
englishFreq LetterK = letterFreq * 0.0050529
englishFreq LetterL = letterFreq * 0.0331490
englishFreq LetterM = letterFreq * 0.0202124
englishFreq LetterN = letterFreq * 0.0564513
englishFreq LetterO = letterFreq * 0.0596302
englishFreq LetterP = letterFreq * 0.0137645
englishFreq LetterQ = letterFreq * 0.0008606
englishFreq LetterR = letterFreq * 0.0497563
englishFreq LetterS = letterFreq * 0.0515760
englishFreq LetterT = letterFreq * 0.0729357
englishFreq LetterU = letterFreq * 0.0225134
englishFreq LetterV = letterFreq * 0.0082903
englishFreq LetterW = letterFreq * 0.0171272
englishFreq LetterX = letterFreq * 0.0013692
englishFreq LetterY = letterFreq * 0.0145984
englishFreq LetterZ = letterFreq * 0.0007836
englishFreq LetterSpace = letterFreq * 0.1918182
englishFreq LetterOther = 1 - letterFreq

freq :: B.ByteString -> Freq
freq b l =
    M.findWithDefault 0.0 l letterCount / n
        where
            letterCount = B.foldr go M.empty b
            go w m =
                M.insertWith (+) (toLetter w) 1.0 m
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

-- | Return the proportion of printable characters (interpreted as ASCII)
-- in a bytestring.
printability :: B.ByteString -> Float
printability b =
    fromIntegral printable / fromIntegral total
        where
            printable = B.foldr (\ w n -> if isPrint (chr (fromIntegral w)) then n + 1 else n) (0::Int) b
            total = B.length b
