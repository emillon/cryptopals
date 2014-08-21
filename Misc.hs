-- | Miscellaneous functions that are used all around the code.
module Misc ( chunksOfSize
            , nthChunk
            , fst3
            , snd3
            , thr3
            , uncurry3
            , randomBytes
            , bsMapIdx
            ) where

import Control.Monad
import Data.Word
import System.Random

import qualified Data.ByteString as B

-- |Split a bytestring into chunks of a given length.
chunksOfSize :: Int -> B.ByteString -> [B.ByteString]
chunksOfSize n b | B.length b <= n = [b]
chunksOfSize n b =
    h:chunksOfSize n t
        where
            (h, t) = B.splitAt n b

-- |Access the nth chunk of a bytestring.
nthChunk :: Int           -- ^ size of chunks
         -> Int           -- ^ chunk index
         -> B.ByteString
         -> B.ByteString
nthChunk sz i b =
    B.take sz $ B.drop (i * sz) b

-- |Get the first element of a triplet.
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- |Get the second element of a triplet.
snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

-- |Get the third element of a triplet.
thr3 :: (a, b, c) -> c
thr3 (_, _, z) = z

-- |Generalization of 'uncurry' to functions with three arguments.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

-- |Generate a random bytestring of the given length.
randomBytes :: Int -> IO B.ByteString
randomBytes n = do
    bytes <- forM [1..n] $ \ _ -> randomIO
    return $ B.pack bytes

-- |Generalization of 'Data.ByteString.map',
-- passing the index as an extra argument.
bsMapIdx :: (Int -> Word8 -> Word8) -> B.ByteString -> B.ByteString
bsMapIdx f bs =
    snd $ B.mapAccumL (\ i w -> (i+1, f i w)) 0 bs
