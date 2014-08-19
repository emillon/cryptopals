{-# LANGUAGE TemplateHaskell #-}

module AES ( aesTests
           , aes128decryptECB
           , aes128decryptCBC
           , aes128cryptECB
           , aes128cryptCBC
           , checkAESProps
           ) where

import Control.Monad.RWS hiding (state)
import Data.Array.Unboxed
import Data.Bits
import Data.Word
import Test.HUnit
import Test.QuickCheck.All
import Test.QuickCheck.Arbitrary

import qualified Data.ByteString as B

import Base64
import Misc
import XOR

aesTests :: Test
aesTests = TestList [scheduleTests, mixTests]

scheduleTests :: Test
scheduleTests = "AES Key Schedule" ~: map (uncurry tc)
    [ (   "00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00"
      , [ "00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00"
        , "62 63 63 63 62 63 63 63 62 63 63 63 62 63 63 63"
        , "9b 98 98 c9 f9 fb fb aa 9b 98 98 c9 f9 fb fb aa"
        , "90 97 34 50 69 6c cf fa f2 f4 57 33 0b 0f ac 99"
        , "ee 06 da 7b 87 6a 15 81 75 9e 42 b2 7e 91 ee 2b"
        , "7f 2e 2b 88 f8 44 3e 09 8d da 7c bb f3 4b 92 90"
        , "ec 61 4b 85 14 25 75 8c 99 ff 09 37 6a b4 9b a7"
        , "21 75 17 87 35 50 62 0b ac af 6b 3c c6 1b f0 9b"
        , "0e f9 03 33 3b a9 61 38 97 06 0a 04 51 1d fa 9f"
        , "b1 d4 d8 e2 8a 7d b9 da 1d 7b b3 de 4c 66 49 41"
        , "b4 ef 5b cb 3e 92 e2 11 23 e9 51 cf 6f 8f 18 8e"
        ]
      )
    , (   "ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff"
      , [ "ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff"
        , "e8 e9 e9 e9 17 16 16 16 e8 e9 e9 e9 17 16 16 16"
        , "ad ae ae 19 ba b8 b8 0f 52 51 51 e6 45 47 47 f0"
        , "09 0e 22 77 b3 b6 9a 78 e1 e7 cb 9e a4 a0 8c 6e"
        , "e1 6a bd 3e 52 dc 27 46 b3 3b ec d8 17 9b 60 b6"
        , "e5 ba f3 ce b7 66 d4 88 04 5d 38 50 13 c6 58 e6"
        , "71 d0 7d b3 c6 b6 a9 3b c2 eb 91 6b d1 2d c9 8d"
        , "e9 0d 20 8d 2f bb 89 b6 ed 50 18 dd 3c 7d d1 50"
        , "96 33 73 66 b9 88 fa d0 54 d8 e2 0d 68 a5 33 5d"
        , "8b f0 3f 23 32 78 c5 f3 66 a0 27 fe 0e 05 14 a3"
        , "d6 0a 35 88 e4 72 f0 7b 82 d2 d7 85 8c d7 c3 26"
        ]
      )
    , (   "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f"
      , [ "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f"
        , "d6 aa 74 fd d2 af 72 fa da a6 78 f1 d6 ab 76 fe"
        , "b6 92 cf 0b 64 3d bd f1 be 9b c5 00 68 30 b3 fe"
        , "b6 ff 74 4e d2 c2 c9 bf 6c 59 0c bf 04 69 bf 41"
        , "47 f7 f7 bc 95 35 3e 03 f9 6c 32 bc fd 05 8d fd"
        , "3c aa a3 e8 a9 9f 9d eb 50 f3 af 57 ad f6 22 aa"
        , "5e 39 0f 7d f7 a6 92 96 a7 55 3d c1 0a a3 1f 6b"
        , "14 f9 70 1a e3 5f e2 8c 44 0a df 4d 4e a9 c0 26"
        , "47 43 87 35 a4 1c 65 b9 e0 16 ba f4 ae bf 7a d2"
        , "54 99 32 d1 f0 85 57 68 10 93 ed 9c be 2c 97 4e"
        , "13 11 1d 7f e3 94 4a 17 f3 07 a7 8b 4d 2b 30 c5"
        ]
      )
    , (   "69 20 e2 99 a5 20 2a 6d 65 6e 63 68 69 74 6f 2a"
      , [ "69 20 e2 99 a5 20 2a 6d 65 6e 63 68 69 74 6f 2a"
        , "fa 88 07 60 5f a8 2d 0d 3a c6 4e 65 53 b2 21 4f"
        , "cf 75 83 8d 90 dd ae 80 aa 1b e0 e5 f9 a9 c1 aa"
        , "18 0d 2f 14 88 d0 81 94 22 cb 61 71 db 62 a0 db"
        , "ba ed 96 ad 32 3d 17 39 10 f6 76 48 cb 94 d6 93"
        , "88 1b 4a b2 ba 26 5d 8b aa d0 2b c3 61 44 fd 50"
        , "b3 4f 19 5d 09 69 44 d6 a3 b9 6f 15 c2 fd 92 45"
        , "a7 00 77 78 ae 69 33 ae 0d d0 5c bb cf 2d ce fe"
        , "ff 8b cc f2 51 e2 ff 5c 5c 32 a3 e7 93 1f 6d 19"
        , "24 b7 18 2e 75 55 e7 72 29 67 44 95 ba 78 29 8c"
        , "ae 12 7c da db 47 9b a8 f2 20 df 3d 48 58 f6 b1"
        ]
      )
    ]
    where
        tc input spec =
            map unHex spec ~=? map aesStateJoin (keySchedule (unHex input))

mixTests :: Test
mixTests = TestList
    [ "AES MixColumns Fwd" ~: map (uncurry tcFwd) tcs
    , "AES MixColumns Rev" ~: map (uncurry tcRev) tcs
    ]
        where
            tcFwd input spec = unHex spec ~=? w32ToBs (mixColumn (bsToW32 (unHex input)))
            tcRev spec input = unHex spec ~=? w32ToBs (mixColumnRev (bsToW32 (unHex input)))

            --           <-- mix rev ---
            --           ----- mix ---->
            tcs =
                [ ("db 13 53 45", "8e 4d a1 bc")
                , ("f2 0a 22 5c", "9f dc 58 9d")
                , ("01 01 01 01", "01 01 01 01")
                , ("c6 c6 c6 c6", "c6 c6 c6 c6")
                , ("d4 d4 d4 d5", "d5 d5 d7 d6")
                , ("2d 26 31 4c", "4d 7e bd f8")
                ]

-- Word32s are little endian
type AESState = (Word32, Word32, Word32, Word32)

aesStateJoin :: AESState -> B.ByteString
aesStateJoin (a, b, c, d) =
    B.concat $ map w32ToBs [a, b, c, d]

w32toBytes :: Word32 -> (Word8, Word8, Word8, Word8)
w32toBytes n =
    ( fromIntegral (n .&. 0x000000ff)
    , fromIntegral $ (n .&. 0x0000ff00) `shiftR` 8
    , fromIntegral $ (n .&. 0x00ff0000) `shiftR` 16
    , fromIntegral $ (n .&. 0xff000000) `shiftR` 24
    )

aesStateSplit :: B.ByteString -> AESState
aesStateSplit bs =
    ( bsToW32 $ nthChunk 4 0 bs
    , bsToW32 $ nthChunk 4 1 bs
    , bsToW32 $ nthChunk 4 2 bs
    , bsToW32 $ nthChunk 4 3 bs
    )

bsToW32 :: B.ByteString -> Word32
bsToW32 bs =
    bytesToW32 (a, b, c, d)
        where
            [a, b, c, d] = B.unpack bs

w32ToBs :: Word32 -> B.ByteString
w32ToBs w =
    B.pack [a, b, c, d]
        where
            (a, b, c, d) = w32toBytes w

bytesToW32 :: (Word8, Word8, Word8, Word8) -> Word32
bytesToW32 (a, b, c, d) =
      fromIntegral a
    + 0x0000100 * fromIntegral b
    + 0x0010000 * fromIntegral c
    + 0x1000000 * fromIntegral d

keySchedule :: B.ByteString -> [AESState]
keySchedule k =
    map snd $ take 11 $ iterate go (1, k0)
        where
            go (n, b) = (n+1, keyExpand n b)
            k0 = aesStateSplit k

keyExpand :: Word8 -> AESState -> AESState
keyExpand n (ka, kb, kc, kd) =
    (ka', kb', kc', kd')
        where
            ka' = xor (scheduleCore n kd) ka
            kb' = xor kb ka'
            kc' = xor kc kb'
            kd' = xor kd kc'

rcon :: UArray Word8 Word8
rcon = listArray (0, 255)
    [ 0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a
    , 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef, 0xc5, 0x91, 0x39
    , 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 0x25, 0x4a, 0x94, 0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a
    , 0x74, 0xe8, 0xcb, 0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8
    , 0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef
    , 0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 0x25, 0x4a, 0x94, 0x33, 0x66, 0xcc
    , 0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb, 0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b
    , 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3
    , 0x7d, 0xfa, 0xef, 0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 0x25, 0x4a, 0x94
    , 0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb, 0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20
    , 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35
    , 0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef, 0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f
    , 0x25, 0x4a, 0x94, 0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb, 0x8d, 0x01, 0x02, 0x04
    , 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63
    , 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef, 0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd
    , 0x61, 0xc2, 0x9f, 0x25, 0x4a, 0x94, 0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb, 0x8d
    ]

sbox :: UArray Word8 Word8
sbox = listArray (0, 255)
    [ 0x63, 0x7C, 0x77, 0x7B, 0xF2, 0x6B, 0x6F, 0xC5, 0x30, 0x01, 0x67, 0x2B, 0xFE, 0xD7, 0xAB, 0x76
    , 0xCA, 0x82, 0xC9, 0x7D, 0xFA, 0x59, 0x47, 0xF0, 0xAD, 0xD4, 0xA2, 0xAF, 0x9C, 0xA4, 0x72, 0xC0
    , 0xB7, 0xFD, 0x93, 0x26, 0x36, 0x3F, 0xF7, 0xCC, 0x34, 0xA5, 0xE5, 0xF1, 0x71, 0xD8, 0x31, 0x15
    , 0x04, 0xC7, 0x23, 0xC3, 0x18, 0x96, 0x05, 0x9A, 0x07, 0x12, 0x80, 0xE2, 0xEB, 0x27, 0xB2, 0x75
    , 0x09, 0x83, 0x2C, 0x1A, 0x1B, 0x6E, 0x5A, 0xA0, 0x52, 0x3B, 0xD6, 0xB3, 0x29, 0xE3, 0x2F, 0x84
    , 0x53, 0xD1, 0x00, 0xED, 0x20, 0xFC, 0xB1, 0x5B, 0x6A, 0xCB, 0xBE, 0x39, 0x4A, 0x4C, 0x58, 0xCF
    , 0xD0, 0xEF, 0xAA, 0xFB, 0x43, 0x4D, 0x33, 0x85, 0x45, 0xF9, 0x02, 0x7F, 0x50, 0x3C, 0x9F, 0xA8
    , 0x51, 0xA3, 0x40, 0x8F, 0x92, 0x9D, 0x38, 0xF5, 0xBC, 0xB6, 0xDA, 0x21, 0x10, 0xFF, 0xF3, 0xD2
    , 0xCD, 0x0C, 0x13, 0xEC, 0x5F, 0x97, 0x44, 0x17, 0xC4, 0xA7, 0x7E, 0x3D, 0x64, 0x5D, 0x19, 0x73
    , 0x60, 0x81, 0x4F, 0xDC, 0x22, 0x2A, 0x90, 0x88, 0x46, 0xEE, 0xB8, 0x14, 0xDE, 0x5E, 0x0B, 0xDB
    , 0xE0, 0x32, 0x3A, 0x0A, 0x49, 0x06, 0x24, 0x5C, 0xC2, 0xD3, 0xAC, 0x62, 0x91, 0x95, 0xE4, 0x79
    , 0xE7, 0xC8, 0x37, 0x6D, 0x8D, 0xD5, 0x4E, 0xA9, 0x6C, 0x56, 0xF4, 0xEA, 0x65, 0x7A, 0xAE, 0x08
    , 0xBA, 0x78, 0x25, 0x2E, 0x1C, 0xA6, 0xB4, 0xC6, 0xE8, 0xDD, 0x74, 0x1F, 0x4B, 0xBD, 0x8B, 0x8A
    , 0x70, 0x3E, 0xB5, 0x66, 0x48, 0x03, 0xF6, 0x0E, 0x61, 0x35, 0x57, 0xB9, 0x86, 0xC1, 0x1D, 0x9E
    , 0xE1, 0xF8, 0x98, 0x11, 0x69, 0xD9, 0x8E, 0x94, 0x9B, 0x1E, 0x87, 0xE9, 0xCE, 0x55, 0x28, 0xDF
    , 0x8C, 0xA1, 0x89, 0x0D, 0xBF, 0xE6, 0x42, 0x68, 0x41, 0x99, 0x2D, 0x0F, 0xB0, 0x54, 0xBB, 0x16
    ]

invSbox :: UArray Word8 Word8
invSbox = listArray (0, 255)
    [ 0x52, 0x09, 0x6A, 0xD5, 0x30, 0x36, 0xA5, 0x38, 0xBF, 0x40, 0xA3, 0x9E, 0x81, 0xF3, 0xD7, 0xFB
    , 0x7C, 0xE3, 0x39, 0x82, 0x9B, 0x2F, 0xFF, 0x87, 0x34, 0x8E, 0x43, 0x44, 0xC4, 0xDE, 0xE9, 0xCB
    , 0x54, 0x7B, 0x94, 0x32, 0xA6, 0xC2, 0x23, 0x3D, 0xEE, 0x4C, 0x95, 0x0B, 0x42, 0xFA, 0xC3, 0x4E
    , 0x08, 0x2E, 0xA1, 0x66, 0x28, 0xD9, 0x24, 0xB2, 0x76, 0x5B, 0xA2, 0x49, 0x6D, 0x8B, 0xD1, 0x25
    , 0x72, 0xF8, 0xF6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xD4, 0xA4, 0x5C, 0xCC, 0x5D, 0x65, 0xB6, 0x92
    , 0x6C, 0x70, 0x48, 0x50, 0xFD, 0xED, 0xB9, 0xDA, 0x5E, 0x15, 0x46, 0x57, 0xA7, 0x8D, 0x9D, 0x84
    , 0x90, 0xD8, 0xAB, 0x00, 0x8C, 0xBC, 0xD3, 0x0A, 0xF7, 0xE4, 0x58, 0x05, 0xB8, 0xB3, 0x45, 0x06
    , 0xD0, 0x2C, 0x1E, 0x8F, 0xCA, 0x3F, 0x0F, 0x02, 0xC1, 0xAF, 0xBD, 0x03, 0x01, 0x13, 0x8A, 0x6B
    , 0x3A, 0x91, 0x11, 0x41, 0x4F, 0x67, 0xDC, 0xEA, 0x97, 0xF2, 0xCF, 0xCE, 0xF0, 0xB4, 0xE6, 0x73
    , 0x96, 0xAC, 0x74, 0x22, 0xE7, 0xAD, 0x35, 0x85, 0xE2, 0xF9, 0x37, 0xE8, 0x1C, 0x75, 0xDF, 0x6E
    , 0x47, 0xF1, 0x1A, 0x71, 0x1D, 0x29, 0xC5, 0x89, 0x6F, 0xB7, 0x62, 0x0E, 0xAA, 0x18, 0xBE, 0x1B
    , 0xFC, 0x56, 0x3E, 0x4B, 0xC6, 0xD2, 0x79, 0x20, 0x9A, 0xDB, 0xC0, 0xFE, 0x78, 0xCD, 0x5A, 0xF4
    , 0x1F, 0xDD, 0xA8, 0x33, 0x88, 0x07, 0xC7, 0x31, 0xB1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xEC, 0x5F
    , 0x60, 0x51, 0x7F, 0xA9, 0x19, 0xB5, 0x4A, 0x0D, 0x2D, 0xE5, 0x7A, 0x9F, 0x93, 0xC9, 0x9C, 0xEF
    , 0xA0, 0xE0, 0x3B, 0x4D, 0xAE, 0x2A, 0xF5, 0xB0, 0xC8, 0xEB, 0xBB, 0x3C, 0x83, 0x53, 0x99, 0x61
    , 0x17, 0x2B, 0x04, 0x7E, 0xBA, 0x77, 0xD6, 0x26, 0xE1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0C, 0x7D
    ]

gmul2 :: UArray Word8 Word8
gmul2 = listArray (0, 255)
    [ 0x00, 0x02, 0x04, 0x06, 0x08, 0x0a, 0x0c, 0x0e, 0x10, 0x12, 0x14, 0x16, 0x18, 0x1a, 0x1c, 0x1e
    , 0x20, 0x22, 0x24, 0x26, 0x28, 0x2a, 0x2c, 0x2e, 0x30, 0x32, 0x34, 0x36, 0x38, 0x3a, 0x3c, 0x3e
    , 0x40, 0x42, 0x44, 0x46, 0x48, 0x4a, 0x4c, 0x4e, 0x50, 0x52, 0x54, 0x56, 0x58, 0x5a, 0x5c, 0x5e
    , 0x60, 0x62, 0x64, 0x66, 0x68, 0x6a, 0x6c, 0x6e, 0x70, 0x72, 0x74, 0x76, 0x78, 0x7a, 0x7c, 0x7e
    , 0x80, 0x82, 0x84, 0x86, 0x88, 0x8a, 0x8c, 0x8e, 0x90, 0x92, 0x94, 0x96, 0x98, 0x9a, 0x9c, 0x9e
    , 0xa0, 0xa2, 0xa4, 0xa6, 0xa8, 0xaa, 0xac, 0xae, 0xb0, 0xb2, 0xb4, 0xb6, 0xb8, 0xba, 0xbc, 0xbe
    , 0xc0, 0xc2, 0xc4, 0xc6, 0xc8, 0xca, 0xcc, 0xce, 0xd0, 0xd2, 0xd4, 0xd6, 0xd8, 0xda, 0xdc, 0xde
    , 0xe0, 0xe2, 0xe4, 0xe6, 0xe8, 0xea, 0xec, 0xee, 0xf0, 0xf2, 0xf4, 0xf6, 0xf8, 0xfa, 0xfc, 0xfe
    , 0x1b, 0x19, 0x1f, 0x1d, 0x13, 0x11, 0x17, 0x15, 0x0b, 0x09, 0x0f, 0x0d, 0x03, 0x01, 0x07, 0x05
    , 0x3b, 0x39, 0x3f, 0x3d, 0x33, 0x31, 0x37, 0x35, 0x2b, 0x29, 0x2f, 0x2d, 0x23, 0x21, 0x27, 0x25
    , 0x5b, 0x59, 0x5f, 0x5d, 0x53, 0x51, 0x57, 0x55, 0x4b, 0x49, 0x4f, 0x4d, 0x43, 0x41, 0x47, 0x45
    , 0x7b, 0x79, 0x7f, 0x7d, 0x73, 0x71, 0x77, 0x75, 0x6b, 0x69, 0x6f, 0x6d, 0x63, 0x61, 0x67, 0x65
    , 0x9b, 0x99, 0x9f, 0x9d, 0x93, 0x91, 0x97, 0x95, 0x8b, 0x89, 0x8f, 0x8d, 0x83, 0x81, 0x87, 0x85
    , 0xbb, 0xb9, 0xbf, 0xbd, 0xb3, 0xb1, 0xb7, 0xb5, 0xab, 0xa9, 0xaf, 0xad, 0xa3, 0xa1, 0xa7, 0xa5
    , 0xdb, 0xd9, 0xdf, 0xdd, 0xd3, 0xd1, 0xd7, 0xd5, 0xcb, 0xc9, 0xcf, 0xcd, 0xc3, 0xc1, 0xc7, 0xc5
    , 0xfb, 0xf9, 0xff, 0xfd, 0xf3, 0xf1, 0xf7, 0xf5, 0xeb, 0xe9, 0xef, 0xed, 0xe3, 0xe1, 0xe7, 0xe5
    ]

gmul3 :: UArray Word8 Word8
gmul3 = listArray (0, 255)
    [ 0x00, 0x03, 0x06, 0x05, 0x0c, 0x0f, 0x0a, 0x09, 0x18, 0x1b, 0x1e, 0x1d, 0x14, 0x17, 0x12, 0x11
    , 0x30, 0x33, 0x36, 0x35, 0x3c, 0x3f, 0x3a, 0x39, 0x28, 0x2b, 0x2e, 0x2d, 0x24, 0x27, 0x22, 0x21
    , 0x60, 0x63, 0x66, 0x65, 0x6c, 0x6f, 0x6a, 0x69, 0x78, 0x7b, 0x7e, 0x7d, 0x74, 0x77, 0x72, 0x71
    , 0x50, 0x53, 0x56, 0x55, 0x5c, 0x5f, 0x5a, 0x59, 0x48, 0x4b, 0x4e, 0x4d, 0x44, 0x47, 0x42, 0x41
    , 0xc0, 0xc3, 0xc6, 0xc5, 0xcc, 0xcf, 0xca, 0xc9, 0xd8, 0xdb, 0xde, 0xdd, 0xd4, 0xd7, 0xd2, 0xd1
    , 0xf0, 0xf3, 0xf6, 0xf5, 0xfc, 0xff, 0xfa, 0xf9, 0xe8, 0xeb, 0xee, 0xed, 0xe4, 0xe7, 0xe2, 0xe1
    , 0xa0, 0xa3, 0xa6, 0xa5, 0xac, 0xaf, 0xaa, 0xa9, 0xb8, 0xbb, 0xbe, 0xbd, 0xb4, 0xb7, 0xb2, 0xb1
    , 0x90, 0x93, 0x96, 0x95, 0x9c, 0x9f, 0x9a, 0x99, 0x88, 0x8b, 0x8e, 0x8d, 0x84, 0x87, 0x82, 0x81
    , 0x9b, 0x98, 0x9d, 0x9e, 0x97, 0x94, 0x91, 0x92, 0x83, 0x80, 0x85, 0x86, 0x8f, 0x8c, 0x89, 0x8a
    , 0xab, 0xa8, 0xad, 0xae, 0xa7, 0xa4, 0xa1, 0xa2, 0xb3, 0xb0, 0xb5, 0xb6, 0xbf, 0xbc, 0xb9, 0xba
    , 0xfb, 0xf8, 0xfd, 0xfe, 0xf7, 0xf4, 0xf1, 0xf2, 0xe3, 0xe0, 0xe5, 0xe6, 0xef, 0xec, 0xe9, 0xea
    , 0xcb, 0xc8, 0xcd, 0xce, 0xc7, 0xc4, 0xc1, 0xc2, 0xd3, 0xd0, 0xd5, 0xd6, 0xdf, 0xdc, 0xd9, 0xda
    , 0x5b, 0x58, 0x5d, 0x5e, 0x57, 0x54, 0x51, 0x52, 0x43, 0x40, 0x45, 0x46, 0x4f, 0x4c, 0x49, 0x4a
    , 0x6b, 0x68, 0x6d, 0x6e, 0x67, 0x64, 0x61, 0x62, 0x73, 0x70, 0x75, 0x76, 0x7f, 0x7c, 0x79, 0x7a
    , 0x3b, 0x38, 0x3d, 0x3e, 0x37, 0x34, 0x31, 0x32, 0x23, 0x20, 0x25, 0x26, 0x2f, 0x2c, 0x29, 0x2a
    , 0x0b, 0x08, 0x0d, 0x0e, 0x07, 0x04, 0x01, 0x02, 0x13, 0x10, 0x15, 0x16, 0x1f, 0x1c, 0x19, 0x1a
    ]

gmul9 :: UArray Word8 Word8
gmul9 = listArray (0, 255)
    [ 0x00, 0x09, 0x12, 0x1b, 0x24, 0x2d, 0x36, 0x3f, 0x48, 0x41, 0x5a, 0x53, 0x6c, 0x65, 0x7e, 0x77
    , 0x90, 0x99, 0x82, 0x8b, 0xb4, 0xbd, 0xa6, 0xaf, 0xd8, 0xd1, 0xca, 0xc3, 0xfc, 0xf5, 0xee, 0xe7
    , 0x3b, 0x32, 0x29, 0x20, 0x1f, 0x16, 0x0d, 0x04, 0x73, 0x7a, 0x61, 0x68, 0x57, 0x5e, 0x45, 0x4c
    , 0xab, 0xa2, 0xb9, 0xb0, 0x8f, 0x86, 0x9d, 0x94, 0xe3, 0xea, 0xf1, 0xf8, 0xc7, 0xce, 0xd5, 0xdc
    , 0x76, 0x7f, 0x64, 0x6d, 0x52, 0x5b, 0x40, 0x49, 0x3e, 0x37, 0x2c, 0x25, 0x1a, 0x13, 0x08, 0x01
    , 0xe6, 0xef, 0xf4, 0xfd, 0xc2, 0xcb, 0xd0, 0xd9, 0xae, 0xa7, 0xbc, 0xb5, 0x8a, 0x83, 0x98, 0x91
    , 0x4d, 0x44, 0x5f, 0x56, 0x69, 0x60, 0x7b, 0x72, 0x05, 0x0c, 0x17, 0x1e, 0x21, 0x28, 0x33, 0x3a
    , 0xdd, 0xd4, 0xcf, 0xc6, 0xf9, 0xf0, 0xeb, 0xe2, 0x95, 0x9c, 0x87, 0x8e, 0xb1, 0xb8, 0xa3, 0xaa
    , 0xec, 0xe5, 0xfe, 0xf7, 0xc8, 0xc1, 0xda, 0xd3, 0xa4, 0xad, 0xb6, 0xbf, 0x80, 0x89, 0x92, 0x9b
    , 0x7c, 0x75, 0x6e, 0x67, 0x58, 0x51, 0x4a, 0x43, 0x34, 0x3d, 0x26, 0x2f, 0x10, 0x19, 0x02, 0x0b
    , 0xd7, 0xde, 0xc5, 0xcc, 0xf3, 0xfa, 0xe1, 0xe8, 0x9f, 0x96, 0x8d, 0x84, 0xbb, 0xb2, 0xa9, 0xa0
    , 0x47, 0x4e, 0x55, 0x5c, 0x63, 0x6a, 0x71, 0x78, 0x0f, 0x06, 0x1d, 0x14, 0x2b, 0x22, 0x39, 0x30
    , 0x9a, 0x93, 0x88, 0x81, 0xbe, 0xb7, 0xac, 0xa5, 0xd2, 0xdb, 0xc0, 0xc9, 0xf6, 0xff, 0xe4, 0xed
    , 0x0a, 0x03, 0x18, 0x11, 0x2e, 0x27, 0x3c, 0x35, 0x42, 0x4b, 0x50, 0x59, 0x66, 0x6f, 0x74, 0x7d
    , 0xa1, 0xa8, 0xb3, 0xba, 0x85, 0x8c, 0x97, 0x9e, 0xe9, 0xe0, 0xfb, 0xf2, 0xcd, 0xc4, 0xdf, 0xd6
    , 0x31, 0x38, 0x23, 0x2a, 0x15, 0x1c, 0x07, 0x0e, 0x79, 0x70, 0x6b, 0x62, 0x5d, 0x54, 0x4f, 0x46
    ]

gmul11 :: UArray Word8 Word8
gmul11 = listArray (0, 255)
    [ 0x00, 0x0b, 0x16, 0x1d, 0x2c, 0x27, 0x3a, 0x31, 0x58, 0x53, 0x4e, 0x45, 0x74, 0x7f, 0x62, 0x69
    , 0xb0, 0xbb, 0xa6, 0xad, 0x9c, 0x97, 0x8a, 0x81, 0xe8, 0xe3, 0xfe, 0xf5, 0xc4, 0xcf, 0xd2, 0xd9
    , 0x7b, 0x70, 0x6d, 0x66, 0x57, 0x5c, 0x41, 0x4a, 0x23, 0x28, 0x35, 0x3e, 0x0f, 0x04, 0x19, 0x12
    , 0xcb, 0xc0, 0xdd, 0xd6, 0xe7, 0xec, 0xf1, 0xfa, 0x93, 0x98, 0x85, 0x8e, 0xbf, 0xb4, 0xa9, 0xa2
    , 0xf6, 0xfd, 0xe0, 0xeb, 0xda, 0xd1, 0xcc, 0xc7, 0xae, 0xa5, 0xb8, 0xb3, 0x82, 0x89, 0x94, 0x9f
    , 0x46, 0x4d, 0x50, 0x5b, 0x6a, 0x61, 0x7c, 0x77, 0x1e, 0x15, 0x08, 0x03, 0x32, 0x39, 0x24, 0x2f
    , 0x8d, 0x86, 0x9b, 0x90, 0xa1, 0xaa, 0xb7, 0xbc, 0xd5, 0xde, 0xc3, 0xc8, 0xf9, 0xf2, 0xef, 0xe4
    , 0x3d, 0x36, 0x2b, 0x20, 0x11, 0x1a, 0x07, 0x0c, 0x65, 0x6e, 0x73, 0x78, 0x49, 0x42, 0x5f, 0x54
    , 0xf7, 0xfc, 0xe1, 0xea, 0xdb, 0xd0, 0xcd, 0xc6, 0xaf, 0xa4, 0xb9, 0xb2, 0x83, 0x88, 0x95, 0x9e
    , 0x47, 0x4c, 0x51, 0x5a, 0x6b, 0x60, 0x7d, 0x76, 0x1f, 0x14, 0x09, 0x02, 0x33, 0x38, 0x25, 0x2e
    , 0x8c, 0x87, 0x9a, 0x91, 0xa0, 0xab, 0xb6, 0xbd, 0xd4, 0xdf, 0xc2, 0xc9, 0xf8, 0xf3, 0xee, 0xe5
    , 0x3c, 0x37, 0x2a, 0x21, 0x10, 0x1b, 0x06, 0x0d, 0x64, 0x6f, 0x72, 0x79, 0x48, 0x43, 0x5e, 0x55
    , 0x01, 0x0a, 0x17, 0x1c, 0x2d, 0x26, 0x3b, 0x30, 0x59, 0x52, 0x4f, 0x44, 0x75, 0x7e, 0x63, 0x68
    , 0xb1, 0xba, 0xa7, 0xac, 0x9d, 0x96, 0x8b, 0x80, 0xe9, 0xe2, 0xff, 0xf4, 0xc5, 0xce, 0xd3, 0xd8
    , 0x7a, 0x71, 0x6c, 0x67, 0x56, 0x5d, 0x40, 0x4b, 0x22, 0x29, 0x34, 0x3f, 0x0e, 0x05, 0x18, 0x13
    , 0xca, 0xc1, 0xdc, 0xd7, 0xe6, 0xed, 0xf0, 0xfb, 0x92, 0x99, 0x84, 0x8f, 0xbe, 0xb5, 0xa8, 0xa3
    ]

gmul13 :: UArray Word8 Word8
gmul13 = listArray (0, 255)
    [ 0x00, 0x0d, 0x1a, 0x17, 0x34, 0x39, 0x2e, 0x23, 0x68, 0x65, 0x72, 0x7f, 0x5c, 0x51, 0x46, 0x4b
    , 0xd0, 0xdd, 0xca, 0xc7, 0xe4, 0xe9, 0xfe, 0xf3, 0xb8, 0xb5, 0xa2, 0xaf, 0x8c, 0x81, 0x96, 0x9b
    , 0xbb, 0xb6, 0xa1, 0xac, 0x8f, 0x82, 0x95, 0x98, 0xd3, 0xde, 0xc9, 0xc4, 0xe7, 0xea, 0xfd, 0xf0
    , 0x6b, 0x66, 0x71, 0x7c, 0x5f, 0x52, 0x45, 0x48, 0x03, 0x0e, 0x19, 0x14, 0x37, 0x3a, 0x2d, 0x20
    , 0x6d, 0x60, 0x77, 0x7a, 0x59, 0x54, 0x43, 0x4e, 0x05, 0x08, 0x1f, 0x12, 0x31, 0x3c, 0x2b, 0x26
    , 0xbd, 0xb0, 0xa7, 0xaa, 0x89, 0x84, 0x93, 0x9e, 0xd5, 0xd8, 0xcf, 0xc2, 0xe1, 0xec, 0xfb, 0xf6
    , 0xd6, 0xdb, 0xcc, 0xc1, 0xe2, 0xef, 0xf8, 0xf5, 0xbe, 0xb3, 0xa4, 0xa9, 0x8a, 0x87, 0x90, 0x9d
    , 0x06, 0x0b, 0x1c, 0x11, 0x32, 0x3f, 0x28, 0x25, 0x6e, 0x63, 0x74, 0x79, 0x5a, 0x57, 0x40, 0x4d
    , 0xda, 0xd7, 0xc0, 0xcd, 0xee, 0xe3, 0xf4, 0xf9, 0xb2, 0xbf, 0xa8, 0xa5, 0x86, 0x8b, 0x9c, 0x91
    , 0x0a, 0x07, 0x10, 0x1d, 0x3e, 0x33, 0x24, 0x29, 0x62, 0x6f, 0x78, 0x75, 0x56, 0x5b, 0x4c, 0x41
    , 0x61, 0x6c, 0x7b, 0x76, 0x55, 0x58, 0x4f, 0x42, 0x09, 0x04, 0x13, 0x1e, 0x3d, 0x30, 0x27, 0x2a
    , 0xb1, 0xbc, 0xab, 0xa6, 0x85, 0x88, 0x9f, 0x92, 0xd9, 0xd4, 0xc3, 0xce, 0xed, 0xe0, 0xf7, 0xfa
    , 0xb7, 0xba, 0xad, 0xa0, 0x83, 0x8e, 0x99, 0x94, 0xdf, 0xd2, 0xc5, 0xc8, 0xeb, 0xe6, 0xf1, 0xfc
    , 0x67, 0x6a, 0x7d, 0x70, 0x53, 0x5e, 0x49, 0x44, 0x0f, 0x02, 0x15, 0x18, 0x3b, 0x36, 0x21, 0x2c
    , 0x0c, 0x01, 0x16, 0x1b, 0x38, 0x35, 0x22, 0x2f, 0x64, 0x69, 0x7e, 0x73, 0x50, 0x5d, 0x4a, 0x47
    , 0xdc, 0xd1, 0xc6, 0xcb, 0xe8, 0xe5, 0xf2, 0xff, 0xb4, 0xb9, 0xae, 0xa3, 0x80, 0x8d, 0x9a, 0x97
    ]

gmul14 :: UArray Word8 Word8
gmul14 = listArray (0, 255)
    [ 0x00, 0x0e, 0x1c, 0x12, 0x38, 0x36, 0x24, 0x2a, 0x70, 0x7e, 0x6c, 0x62, 0x48, 0x46, 0x54, 0x5a
    , 0xe0, 0xee, 0xfc, 0xf2, 0xd8, 0xd6, 0xc4, 0xca, 0x90, 0x9e, 0x8c, 0x82, 0xa8, 0xa6, 0xb4, 0xba
    , 0xdb, 0xd5, 0xc7, 0xc9, 0xe3, 0xed, 0xff, 0xf1, 0xab, 0xa5, 0xb7, 0xb9, 0x93, 0x9d, 0x8f, 0x81
    , 0x3b, 0x35, 0x27, 0x29, 0x03, 0x0d, 0x1f, 0x11, 0x4b, 0x45, 0x57, 0x59, 0x73, 0x7d, 0x6f, 0x61
    , 0xad, 0xa3, 0xb1, 0xbf, 0x95, 0x9b, 0x89, 0x87, 0xdd, 0xd3, 0xc1, 0xcf, 0xe5, 0xeb, 0xf9, 0xf7
    , 0x4d, 0x43, 0x51, 0x5f, 0x75, 0x7b, 0x69, 0x67, 0x3d, 0x33, 0x21, 0x2f, 0x05, 0x0b, 0x19, 0x17
    , 0x76, 0x78, 0x6a, 0x64, 0x4e, 0x40, 0x52, 0x5c, 0x06, 0x08, 0x1a, 0x14, 0x3e, 0x30, 0x22, 0x2c
    , 0x96, 0x98, 0x8a, 0x84, 0xae, 0xa0, 0xb2, 0xbc, 0xe6, 0xe8, 0xfa, 0xf4, 0xde, 0xd0, 0xc2, 0xcc
    , 0x41, 0x4f, 0x5d, 0x53, 0x79, 0x77, 0x65, 0x6b, 0x31, 0x3f, 0x2d, 0x23, 0x09, 0x07, 0x15, 0x1b
    , 0xa1, 0xaf, 0xbd, 0xb3, 0x99, 0x97, 0x85, 0x8b, 0xd1, 0xdf, 0xcd, 0xc3, 0xe9, 0xe7, 0xf5, 0xfb
    , 0x9a, 0x94, 0x86, 0x88, 0xa2, 0xac, 0xbe, 0xb0, 0xea, 0xe4, 0xf6, 0xf8, 0xd2, 0xdc, 0xce, 0xc0
    , 0x7a, 0x74, 0x66, 0x68, 0x42, 0x4c, 0x5e, 0x50, 0x0a, 0x04, 0x16, 0x18, 0x32, 0x3c, 0x2e, 0x20
    , 0xec, 0xe2, 0xf0, 0xfe, 0xd4, 0xda, 0xc8, 0xc6, 0x9c, 0x92, 0x80, 0x8e, 0xa4, 0xaa, 0xb8, 0xb6
    , 0x0c, 0x02, 0x10, 0x1e, 0x34, 0x3a, 0x28, 0x26, 0x7c, 0x72, 0x60, 0x6e, 0x44, 0x4a, 0x58, 0x56
    , 0x37, 0x39, 0x2b, 0x25, 0x0f, 0x01, 0x13, 0x1d, 0x47, 0x49, 0x5b, 0x55, 0x7f, 0x71, 0x63, 0x6d
    , 0xd7, 0xd9, 0xcb, 0xc5, 0xef, 0xe1, 0xf3, 0xfd, 0xa7, 0xa9, 0xbb, 0xb5, 0x9f, 0x91, 0x83, 0x8d
    ]

scheduleCore :: Word8 -> Word32 -> Word32
scheduleCore i k =
    bytesToW32 (ka', kb', kc', kd')
        where
            ka' = sbox ! kb `xor` rcon ! i
            kb' = sbox ! kc
            kc' = sbox ! kd
            kd' = sbox ! ka
            (ka, kb, kc, kd) = w32toBytes k

aes128decryptECB :: B.ByteString -> B.ByteString -> B.ByteString
aes128decryptECB key cipher =
    ecbHelper aes128decryptBlock key cipher

ecbHelper :: (B.ByteString -> B.ByteString -> B.ByteString) -> B.ByteString -> B.ByteString -> B.ByteString
ecbHelper fblock key input =
    B.concat $ map (fblock key) $ chunksOfSize 16 input

aes128cryptECB :: B.ByteString -> B.ByteString -> B.ByteString
aes128cryptECB key plain =
    ecbHelper aes128cryptBlock key plain

aes128cryptCBC :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
aes128cryptCBC key iv plain =
    thr3 $ runRWS m key iv
        where
            m = mapM_ aes128cryptCBCblock $ chunksOfSize 16 plain

aes128cryptCBCblock :: B.ByteString -> RWS B.ByteString B.ByteString B.ByteString ()
aes128cryptCBCblock block = do
    key <- ask
    state <- get
    let out = aes128cryptBlock key (xorBuffer block state)
    tell out
    put out

aes128decryptCBC :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
aes128decryptCBC key iv cipher =
    thr3 $ runRWS m key iv
        where
            m = mapM_ aes128decryptCBCblock $ chunksOfSize 16 cipher

aes128decryptCBCblock :: B.ByteString -> RWS B.ByteString B.ByteString B.ByteString ()
aes128decryptCBCblock block = do
    key <- ask
    let out = aes128decryptBlock key block
    state <- get
    tell $ xorBuffer out state
    put block

fullSplit :: [a] -> (a, [a], a)
fullSplit l = (head l, tail $ init l, last l)

aes128cryptBlock :: B.ByteString -> B.ByteString -> B.ByteString
aes128cryptBlock key =
      aesStateJoin
    . addRoundKey finalRoundKey
    . shiftRows
    . subBytes
    . rounds (reverse roundKeys)
    . addRoundKey initialRoundKey
    . aesStateSplit
        where
            (initialRoundKey, roundKeys, finalRoundKey) = fullSplit $ keySchedule key
            rounds ks s = foldr aesRound s ks

aesRound :: AESState -> AESState -> AESState
aesRound k =
    addRoundKey k . mixColumns . shiftRows . subBytes

aes128decryptBlock :: B.ByteString -> B.ByteString -> B.ByteString
aes128decryptBlock key =
      aesStateJoin
    . addRoundKeyRev initialRoundKey
    . rounds roundKeys
    . subBytesRev
    . shiftRowsRev
    . addRoundKeyRev finalRoundKey
    . aesStateSplit
        where
            (initialRoundKey, roundKeys, finalRoundKey) = fullSplit $ keySchedule key
            rounds ks s = foldr aesRoundRev s ks

aesRoundRev :: AESState -> AESState -> AESState
aesRoundRev k = subBytesRev . shiftRowsRev . mixColumnsRev . addRoundKeyRev k

addRoundKey, addRoundKeyRev :: AESState -> AESState -> AESState
addRoundKey = forColumn2 xor
addRoundKeyRev = addRoundKey

-- a0 b0 c0 d0       a0 b0 c0 d0
-- a1 b1 c1 d1       b1 c1 d1 a1
-- a2 b2 c2 d2  -->  c2 d2 a2 b2
-- a3 b3 c3 d3       d3 a3 b3 c3
shiftRows :: AESState -> AESState
shiftRows (a, b, c, d) =
    ( bytesToW32 (a0, b1, c2, d3)
    , bytesToW32 (b0, c1, d2, a3)
    , bytesToW32 (c0, d1, a2, b3)
    , bytesToW32 (d0, a1, b2, c3)
    )
        where
            (a0, a1, a2, a3) = w32toBytes a
            (b0, b1, b2, b3) = w32toBytes b
            (c0, c1, c2, c3) = w32toBytes c
            (d0, d1, d2, d3) = w32toBytes d

-- a0 b0 c0 d0       a0 b0 c0 d0
-- a1 b1 c1 d1       d1 a1 b1 c1
-- a2 b2 c2 d2  -->  c2 d2 a2 b2
-- a3 b3 c3 d3       b3 c3 d3 a3
shiftRowsRev :: AESState -> AESState
shiftRowsRev (a, b, c, d) =
    ( bytesToW32 (a0, d1, c2, b3)
    , bytesToW32 (b0, a1, d2, c3)
    , bytesToW32 (c0, b1, a2, d3)
    , bytesToW32 (d0, c1, b2, a3)
    )
        where
            (a0, a1, a2, a3) = w32toBytes a
            (b0, b1, b2, b3) = w32toBytes b
            (c0, c1, c2, c3) = w32toBytes c
            (d0, d1, d2, d3) = w32toBytes d

subBytes :: AESState -> AESState
subBytes =
    forByte $ \ w -> sbox ! w

subBytesRev :: AESState -> AESState
subBytesRev =
    forByte $ \ w -> invSbox ! w

mixColumnsRev :: AESState -> AESState
mixColumnsRev =
    forColumn mixColumnRev

forByte :: (Word8 -> Word8) -> AESState -> AESState
forByte f =
    forColumn (mapBytes f)

mapBytes :: (Word8 -> Word8) -> Word32 -> Word32
mapBytes f w =
    bytesToW32 (f a, f b, f c, f d)
        where
            (a, b, c, d) = w32toBytes w

forColumn :: (Word32 -> Word32) -> AESState -> AESState
forColumn f (a, b, c, d) =
    (f a, f b, f c, f d)

forColumn2 :: (Word32 -> Word32 -> Word32) -> AESState -> AESState -> AESState
forColumn2 f (xa, xb, xc, xd) (ya, yb, yc, yd) =
    (f xa ya , f xb yb , f xc yc , f xd yd)

mixColumnRev :: Word32 -> Word32
mixColumnRev w =
    bytesToW32 (r0, r1, r2, r3)
        where
            r0 = (gmul14 ! a0) `xor` (gmul9 ! a3) `xor` (gmul13 ! a2) `xor` (gmul11 ! a1)
            r1 = (gmul14 ! a1) `xor` (gmul9 ! a0) `xor` (gmul13 ! a3) `xor` (gmul11 ! a2)
            r2 = (gmul14 ! a2) `xor` (gmul9 ! a1) `xor` (gmul13 ! a0) `xor` (gmul11 ! a3)
            r3 = (gmul14 ! a3) `xor` (gmul9 ! a2) `xor` (gmul13 ! a1) `xor` (gmul11 ! a0)
            (a0, a1, a2, a3) = w32toBytes w

mixColumns :: AESState -> AESState
mixColumns =
    forColumn mixColumn

mixColumn :: Word32 -> Word32
mixColumn bs =
    bytesToW32 (r0, r1, r2, r3)
        where
            r0 = (gmul2 ! a0) `xor` a3 `xor` a2 `xor` (gmul3 ! a1);
	    r1 = (gmul2 ! a1) `xor` a0 `xor` a3 `xor` (gmul3 ! a2);
	    r2 = (gmul2 ! a2) `xor` a1 `xor` a0 `xor` (gmul3 ! a3);
	    r3 = (gmul2 ! a3) `xor` a2 `xor` a1 `xor` (gmul3 ! a0);
            (a0, a1, a2, a3) = w32toBytes bs

prop_addRoundKeyInv :: AESState -> AESState -> Bool
prop_addRoundKeyInv k s =
    addRoundKeyRev k (addRoundKey k s) == s

prop_shiftRowsInv :: AESState -> Bool
prop_shiftRowsInv s =
    shiftRowsRev (shiftRows s) == s

prop_subBytesInv :: AESState -> Bool
prop_subBytesInv s =
    subBytesRev (subBytes s) == s

prop_mixColumnsInv :: AESState -> Bool
prop_mixColumnsInv s =
    mixColumnsRev (mixColumns s) == s

prop_aesRoundInv :: AESState -> AESState -> Bool
prop_aesRoundInv k s =
    aesRoundRev k (aesRound k s) == s

newtype GeneratedBS = GeneratedBS B.ByteString
    deriving (Show)

instance Arbitrary GeneratedBS where
    arbitrary = do
        bytes <- vector 16
        return $ GeneratedBS $ B.pack bytes

prop_joinSplit :: GeneratedBS -> Bool
prop_joinSplit (GeneratedBS b) =
    aesStateJoin (aesStateSplit b) == b

prop_splitJoin :: AESState -> Bool
prop_splitJoin s =
    aesStateSplit (aesStateJoin s) == s

prop_aes128blockInv :: GeneratedBS -> GeneratedBS -> Bool
prop_aes128blockInv (GeneratedBS k) (GeneratedBS b) =
    aes128decryptBlock k (aes128cryptBlock k b) == b

checkAESProps :: IO Bool
checkAESProps = $quickCheckAll
