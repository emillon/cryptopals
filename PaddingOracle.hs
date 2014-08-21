-- | Implementation of a CBC padding oracle attack

module PaddingOracle (getBlock) where

import Control.Monad.State
import Data.Bits
import Data.Word

import qualified Data.ByteString as B

import Misc
import XOR

byteBefore :: (B.ByteString -> B.ByteString -> Bool) -> B.ByteString -> B.ByteString -> B.ByteString -> (Word8, Word8)
byteBefore oracle cipher iv int =
    (lastPlain, byteInter)
        where
            n = B.length int + 1
            wn = fromIntegral n
            c1 = nthChunk 16 0 cipher
            c2 = nthChunk 16 1 cipher
            byteCipher = B.index c1 (16-n)
            int' = xorBufferRepeat int (B.singleton wn)
            cipher' w = B.concat [B.replicate (16-n) 0, B.singleton w, int', c2]
            check w = oracle (cipher' w) iv
            byteCipher' = fst $ head $ filter snd $ map (\ w -> (w, check w)) [0..0xff]
            byteInter = byteCipher' `xor` wn
            lastPlain = byteCipher `xor` byteInter

getBlock :: (B.ByteString -> B.ByteString -> Bool) -> B.ByteString -> B.ByteString -> [Word8]
getBlock oracle cipher iv =
    reverse $ evalState (mapM go [1::Int ..16]) B.empty
        where
            go _ = do
                int <- get
                let (b, ib) = byteBefore oracle cipher iv int
                put $ B.cons ib int
                return b
