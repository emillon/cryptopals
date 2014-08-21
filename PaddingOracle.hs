-- | Implementation of a CBC padding oracle attack

module PaddingOracle (padOracleAttack) where

import Control.Monad.State
import Data.Bits
import Data.Word

import qualified Data.ByteString as B

import Misc
import XOR

byteBefore :: (B.ByteString -> B.ByteString -> Bool) -> B.ByteString -> B.ByteString -> B.ByteString -> Int -> (Word8, Word8)
byteBefore oracle cipher iv int bn =
    (lastPlain, byteInter)
        where
            n = B.length int + 1
            wn = fromIntegral n
            previousBlocks = B.concat $ map blockNum [0..bn-2]
            blockNum (-1) = iv
            blockNum i = nthChunk 16 i cipher
            c1 = blockNum (bn-1)
            c2 = blockNum bn
            byteCipher = B.index c1 (16-n)
            int' = xorBufferRepeat int (B.singleton wn)
            cipher' w = B.concat [previousBlocks, B.replicate (16-n) 0, B.singleton w, int', c2]
            check w = oracle (cipher' w) iv
            byteCipher' = fst $ head $ filter snd $ map (\ w -> (w, check w)) [0..0xff]
            byteInter = byteCipher' `xor` wn
            lastPlain = byteCipher `xor` byteInter

getBlock :: (B.ByteString -> B.ByteString -> Bool) -> B.ByteString -> B.ByteString -> Int -> [Word8]
getBlock oracle cipher iv n =
    reverse $ evalState (mapM go [1::Int ..16]) B.empty
        where
            go _ = do
                int <- get
                let (b, ib) = byteBefore oracle cipher iv int n
                put $ B.cons ib int
                return b

-- | Retrieve the plaintext, given a ciphertext.
padOracleAttack :: (B.ByteString -> B.ByteString -> Bool) -- ^ Oracle (takes cipher & iv)
                -> B.ByteString -- ^ Ciphertext
                -> B.ByteString -- ^ IV
                -> B.ByteString
padOracleAttack oracle cipher iv =
    B.concat $ map block [0..nblocks-1]
        where
            nblocks = B.length cipher `div` 16
            block n = B.pack $ getBlock oracle cipher iv n
