-- | Miscellaneous functions that are used all around the code.

module Misc ( chunksOfSize
            , nthChunk
            , fst3
            , snd3
            , thr3
            , uncurry3
            , randomBytes
            , bsMapIdx
            , randomWithin
            , bsUcFirst
            , w64LEtoBS
            , w64BEtoBS
            , nTimesA
            , bsToNthW32BE
            , w32BEtoBS
            , checkMiscProps
            , spliceBS
            , GeneratedBS16(..)
            , GeneratedBSA(..)
            ) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import Data.Word
import System.Random
import Test.QuickCheck hiding ((.&.))

import qualified Data.ByteString as B

-- | Split a bytestring into chunks of a given length.
chunksOfSize :: Int -> B.ByteString -> [B.ByteString]
chunksOfSize n b | B.length b <= n = [b]
chunksOfSize n b =
    h:chunksOfSize n t
        where
            (h, t) = B.splitAt n b

-- | Access the nth chunk of a bytestring.
nthChunk :: Int           -- ^ size of chunks
         -> Int           -- ^ chunk index
         -> B.ByteString
         -> B.ByteString
nthChunk sz i b =
    B.take sz $ B.drop (i * sz) b

-- | Get the first element of a triplet.
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- | Get the second element of a triplet.
snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

-- | Get the third element of a triplet.
thr3 :: (a, b, c) -> c
thr3 (_, _, z) = z

-- | Generalization of 'uncurry' to functions with three arguments.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

-- | Generate a random bytestring of the given length.
randomBytes :: Int -> IO B.ByteString
randomBytes n = do
    bytes <- forM [1..n] $ \ _ -> randomIO
    return $ B.pack bytes

-- | Generalization of 'Data.ByteString.map',
-- passing the index as an extra argument.
bsMapIdx :: (Int -> Word8 -> Word8) -> B.ByteString -> B.ByteString
bsMapIdx f bs =
    snd $ B.mapAccumL (\ i w -> (i+1, f i w)) 0 bs

-- | Pick an element at random within a list.
randomWithin :: [a] -> IO a
randomWithin l = do
    let n = length l
    i <- randomRIO (0, n-1)
    return $ l !! i

-- | Uppercase the first (ASCII) letter in a Bytestring.
bsUcFirst :: B.ByteString -> B.ByteString
bsUcFirst =
    bsMapIdx go
        where
            go 0 w = upperWord w
            go _ w = w
            upperWord = fromIntegral . ord . toUpper . chr . fromIntegral

-- | Encode a Word64 to a bytestring, in little endian order.
w64LEtoBS :: Word64 -> B.ByteString
w64LEtoBS n =
    B.pack [b0, b1, b2, b3, b4, b5, b6, b7]
        where
            b0 = fromIntegral   (n .&. 0x00000000000000ff)
            b1 = fromIntegral $ (n .&. 0x000000000000ff00) `shiftR` (8*1)
            b2 = fromIntegral $ (n .&. 0x0000000000ff0000) `shiftR` (8*2)
            b3 = fromIntegral $ (n .&. 0x00000000ff000000) `shiftR` (8*3)
            b4 = fromIntegral $ (n .&. 0x000000ff00000000) `shiftR` (8*4)
            b5 = fromIntegral $ (n .&. 0x0000ff0000000000) `shiftR` (8*5)
            b6 = fromIntegral $ (n .&. 0x00ff000000000000) `shiftR` (8*6)
            b7 = fromIntegral $ (n .&. 0xff00000000000000) `shiftR` (8*7)

-- | Encode a Word64 to a bytestring, in big endian order.
w64BEtoBS :: Word64 -> B.ByteString
w64BEtoBS = B.reverse . w64LEtoBS

-- | A bytestring with n times the letter 'A'.
nTimesA :: Int -> B.ByteString
nTimesA n =
    B.replicate n $ fromIntegral $ ord 'A'

-- | Fetch the nth 32-bit word in a bytestring, in big endian order.
bsToNthW32BE :: B.ByteString -> Int -> Word32
bsToNthW32BE bs n =
    bsToW32BE $ B.take 4 $ B.drop (4*n) bs

bsToW32BE :: B.ByteString -> Word32
bsToW32BE bs =
    sum [ 0x0000001 * get 3
        , 0x0000100 * get 2
        , 0x0010000 * get 1
        , 0x1000000 * get 0
        ]
        where
            get i = fromIntegral $ B.index bs i

-- | Convert a Word32 to a bytestring, in big endian order.
w32BEtoBS :: Word32 -> B.ByteString
w32BEtoBS n =
    B.pack [a, b, c, d]
        where
            a = fromIntegral $ (n .&. 0xff000000) `shiftR` (8*3)
            b = fromIntegral $ (n .&. 0x00ff0000) `shiftR` (8*2)
            c = fromIntegral $ (n .&. 0x0000ff00) `shiftR` (8*1)
            d = fromIntegral $ (n .&. 0x000000ff)

prop_w32be_inv :: Word32 -> Bool
prop_w32be_inv w =
    bsToW32BE (w32BEtoBS w) == w

-- | QuickCheck tests for this module.
checkMiscProps :: IO ()
checkMiscProps =
    quickCheck prop_w32be_inv

-- | Replace part of a bytestring with a new bytestring.
-- The replaced bytes start at a given offset.
--
-- @
-- AAAAAAAAAAAAAAAAAAAAAAAAAAAA
--             BBBBBBBB
-- ---- n ---->
-- @
--
-- becomes
--
-- @
-- AAAAAAAAAAAABBBBBBBBAAAAAAAA
--      b1        b2      b3
-- @
spliceBS :: B.ByteString -- ^ Original bytestring
         -> Int          -- ^ Offset
         -> B.ByteString -- ^ Bytestring to insert
         -> B.ByteString
spliceBS bs off sub =
    B.concat [b1, b2, b3]
        where
            b1 = B.take off bs
            b2 = sub
            b3 = B.drop (off + B.length sub) bs

-- | Wrapper with an 'Arbitrary' instance that generates
-- bytestrings of length 16.
newtype GeneratedBS16 = GBS16 B.ByteString
    deriving (Show)

instance Arbitrary GeneratedBS16 where
    arbitrary = GBS16 <$> B.pack <$> vector 16

-- | Wrapper with an 'Arbitrary' instance that generates
-- bytestrings of random length inclusively between 0 and 63.
newtype GeneratedBSA = GBSA B.ByteString
    deriving (Show)

instance Arbitrary GeneratedBSA where
    arbitrary = do
        n <- choose (0, 63)
        GBSA <$> B.pack <$> vector n
