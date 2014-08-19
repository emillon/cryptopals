module ByteAtATime ( afterAesUnknown
                   , unknownBytes
                   ) where

import Data.Char
import Data.Word

import qualified Data.ByteString as B
import qualified Data.Map as M

import AES
import Base64
import Misc

aesUnknown :: B.ByteString -> B.ByteString
aesUnknown input =
    aes128cryptECB key $ padPkcs7 $ B.append input afterAesUnknown
        where
            key = unHex "be0c fb9c 2c27 b79b e358 a079 4271 3776"

afterAesUnknown :: B.ByteString
afterAesUnknown = decodeBase64 $ concat
    [ "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg"
    , "aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq"
    , "dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg"
    , "YnkK"
    ]

nTimesA :: Int -> B.ByteString
nTimesA n =
    B.replicate n $ fromIntegral $ ord 'A'

detectBlockSize :: Int
detectBlockSize =
    (1+) $ findIndex2 same $ map f [1..30]
        where
            f n =
                B.take n $ aesUnknown $ nTimesA n
            same x y =
                x == B.init y

nextByte :: B.ByteString -> Maybe Word8
nextByte start =
    M.lookup goal m
        where
            m = M.fromList $ map (\ w -> (block w, w)) [0..]
            bs = detectBlockSize
            n = B.length start
            shortBlock = nTimesA ((blockNum + 1) * bs - 1 - n)
            goal = nthChunk bs blockNum $ aesUnknown shortBlock
            blockNum = n `div` bs
            block w =
                nthChunk bs blockNum $ aesUnknown $ B.concat [shortBlock, start, B.singleton w]

unknownBytes :: B.ByteString
unknownBytes =
    B.init $ B.unfoldr go []
        where
            go l = do
                r <- nextByte $ B.pack l
                return (r, l++[r])
