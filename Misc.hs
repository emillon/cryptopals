module Misc ( chunksOfSize
            , fst3
            , snd3
            , thr3
            , uncurry3
            ) where

import qualified Data.ByteString as B

chunksOfSize :: Int -> B.ByteString -> [B.ByteString]
chunksOfSize n b | B.length b <= n = [b]
chunksOfSize n b =
    h:chunksOfSize n t
        where
            (h, t) = B.splitAt n b

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

thr3 :: (a, b, c) -> c
thr3 (_, _, z) = z

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z
