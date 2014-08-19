module ByteAtATime ( aesUnknown
                   , unknownBytes
                   , aesUnknownRandom
                   , unknownBytesRandom
                   ) where

import Data.Char
import Data.Word
import System.Random

import qualified Data.ByteString as B
import qualified Data.Map as M

import AES
import Base64
import Misc

aesUnknown :: B.ByteString -> B.ByteString -> B.ByteString
aesUnknown after input =
    aes128cryptECB key $ padPkcs7 $ B.append input after
        where
            key = unHex "be0c fb9c 2c27 b79b e358 a079 4271 3776"

nTimesA :: Int -> B.ByteString
nTimesA n =
    B.replicate n $ fromIntegral $ ord 'A'

detectBlockSize :: (B.ByteString -> B.ByteString) -> Int
detectBlockSize f =
    (1+) $ findIndex2 same $ map (\n -> B.take n $ f $ nTimesA n) [1..30]
        where
            same x y =
                x == B.init y

nextByte :: (B.ByteString -> B.ByteString) -> B.ByteString -> Maybe Word8
nextByte f start =
    M.lookup goal m
        where
            m = M.fromList $ map (\ w -> (block w, w)) [0..]
            bs = detectBlockSize f
            n = B.length start
            shortBlock = nTimesA ((blockNum + 1) * bs - 1 - n)
            goal = nthChunk bs blockNum $ f shortBlock
            blockNum = n `div` bs
            block w =
                nthChunk bs blockNum $ f $ B.concat [shortBlock, start, B.singleton w]

unknownBytes :: (B.ByteString -> B.ByteString) -> B.ByteString
unknownBytes f =
    B.init $ B.unfoldr go []
        where
            go l = do
                r <- nextByte f $ B.pack l
                return (r, l++[r])

aesUnknownRandom :: B.ByteString -> B.ByteString -> IO B.ByteString
aesUnknownRandom after input = do
    n <- randomRIO (5, 10)
    before <- randomBytes n
    return $ aes128cryptECB key $ padPkcs7 $ B.concat [before, input, after]
        where
            key = unHex "5930 13d5 6952 3d3a 83ad 1143 90e5 64a1"

unknownBytesRandom :: (B.ByteString -> IO B.ByteString) -> IO B.ByteString
unknownBytesRandom _ = return B.empty
