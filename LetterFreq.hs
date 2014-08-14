module LetterFreq (englishness) where

import Data.Char
import Data.Word

import qualified Data.ByteString as B
import qualified Data.Map as M

englishness :: B.ByteString -> Float
englishness s =
    similarity (freq s) englishFreq

toCommonLetter :: Word8 -> Word8
toCommonLetter = fromIntegral . ord . toLower . chr . fromIntegral

type Freq = M.Map Word8 Float

englishFreq :: Freq
englishFreq = M.fromList $ map (\ (c, f) -> (fromIntegral (ord c), f))
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
    ++ map (\ w -> (w, -0.2)) ([0..31] ++ [127..255])

freq :: B.ByteString -> Freq
freq b =
    M.map (\ c -> c / n) letterCount
        where
            letterCount = B.foldr go M.empty b
            go w m = M.insertWith (+) (toCommonLetter w) 1.0 m
            n = fromIntegral $ B.length b

norm :: Freq -> Float
norm f = sqrt $ sum $ map (\ l -> (M.findWithDefault 0.0 (fromIntegral (ord l)) f)^(2::Int)) ['a'..'z']

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
