module XOR ( xorBuffer
           , xorBufferRepeat
           , findXorKey
           , hammingDistance
           , breakRepeatXorAuto
           ) where

import Data.Bits
import Data.List
import Data.Ord
import Data.Word

import qualified Data.ByteString as B

import LetterFreq
import Misc

xorBuffer :: B.ByteString -> B.ByteString -> B.ByteString
xorBuffer a b =
    snd $ B.mapAccumL (\ i w -> (i+1, w `xor` (B.index b i))) 0 a

xorBufferRepeat :: B.ByteString -> B.ByteString -> B.ByteString
xorBufferRepeat input key =
    snd $ B.mapAccumL go 0 input
        where
            go i w =
                (i + 1, xor w (B.index key (i `mod` n)))
            n = B.length key

findXorKey :: B.ByteString -> (Word8, Float, B.ByteString)
findXorKey b =
    maximumBy (comparing snd3) $ map f [0..]
        where
            n = B.length b
            makeKey k = B.replicate n k
            f k = let plain = xorBuffer b (makeKey k) in
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

hammingDistance :: B.ByteString -> B.ByteString -> Int
hammingDistance a b = sum $ B.zipWith hammingDistanceWord8 a b

hammingDistanceWord8 :: Word8 -> Word8 -> Int
hammingDistanceWord8 a b =
    popCount $ a `xor` b

tryKeySize :: Int -> B.ByteString -> Int
tryKeySize n bs =
    sum $ map (\ (x, y) -> hammingDistance x y) chunkPairs
        where
            a = nthChunk n 0 bs
            b = nthChunk n 1 bs
            c = nthChunk n 2 bs
            d = nthChunk n 3 bs
            chunkPairs = [ (a, b), (a, c), (a, d) , (b, c), (c, d) , (c, d) ]

scoreKeySize :: Int -> B.ByteString -> Float
scoreKeySize ks b =
    fromIntegral dist / fromIntegral ks
        where
            dist = tryKeySize ks b

keysizeCandidates :: B.ByteString -> [Int]
keysizeCandidates b =
    take 4 $ map fst $ sortBy (comparing snd) $ map (\ ks -> (ks, scoreKeySize ks b)) [2..40]

index0 :: B.ByteString -> Int -> Word8
index0 b i =
    if i < B.length b
        then B.index b i
        else 0

breakRepeatXorAuto :: B.ByteString -> (B.ByteString, Float, B.ByteString)
breakRepeatXorAuto b =
    maximumBy (comparing snd3) $ map f $ map (breakRepeatXor b) $ keysizeCandidates b
        where
            f k =
                let plain = xorBufferRepeat b k in
                (k, englishness plain, plain)
