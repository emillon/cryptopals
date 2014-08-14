module AES (aesTests) where

import Test.HUnit

import qualified Data.ByteString as B

import Base64

aesTests :: Test
aesTests = "AES Key Schedule" ~: map (uncurry tc)
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
    ]
    where
        tc input spec =
            unHexLines spec ~=? keySchedule (unHex input)
        unHex = decodeHex . concat . words
        unHexLines = map unHex

keySchedule :: B.ByteString -> [B.ByteString]
keySchedule b = [b]


{-
For the key ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff, the expanded key is:

ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff 
e8 e9 e9 e9 17 16 16 16 e8 e9 e9 e9 17 16 16 16 
ad ae ae 19 ba b8 b8 0f 52 51 51 e6 45 47 47 f0 
09 0e 22 77 b3 b6 9a 78 e1 e7 cb 9e a4 a0 8c 6e 
e1 6a bd 3e 52 dc 27 46 b3 3b ec d8 17 9b 60 b6 
e5 ba f3 ce b7 66 d4 88 04 5d 38 50 13 c6 58 e6 
71 d0 7d b3 c6 b6 a9 3b c2 eb 91 6b d1 2d c9 8d 
e9 0d 20 8d 2f bb 89 b6 ed 50 18 dd 3c 7d d1 50 
96 33 73 66 b9 88 fa d0 54 d8 e2 0d 68 a5 33 5d 
8b f0 3f 23 32 78 c5 f3 66 a0 27 fe 0e 05 14 a3 
d6 0a 35 88 e4 72 f0 7b 82 d2 d7 85 8c d7 c3 26 

For the key 00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f, the expanded key is:
00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 
d6 aa 74 fd d2 af 72 fa da a6 78 f1 d6 ab 76 fe 
b6 92 cf 0b 64 3d bd f1 be 9b c5 00 68 30 b3 fe 
b6 ff 74 4e d2 c2 c9 bf 6c 59 0c bf 04 69 bf 41 
47 f7 f7 bc 95 35 3e 03 f9 6c 32 bc fd 05 8d fd 
3c aa a3 e8 a9 9f 9d eb 50 f3 af 57 ad f6 22 aa 
5e 39 0f 7d f7 a6 92 96 a7 55 3d c1 0a a3 1f 6b 
14 f9 70 1a e3 5f e2 8c 44 0a df 4d 4e a9 c0 26 
47 43 87 35 a4 1c 65 b9 e0 16 ba f4 ae bf 7a d2 
54 99 32 d1 f0 85 57 68 10 93 ed 9c be 2c 97 4e 
13 11 1d 7f e3 94 4a 17 f3 07 a7 8b 4d 2b 30 c5 

For the key 69 20 e2 99 a5 20 2a 6d 65 6e 63 68 69 74 6f 2a, the expanded key is:

69 20 e2 99 a5 20 2a 6d 65 6e 63 68 69 74 6f 2a 
fa 88 07 60 5f a8 2d 0d 3a c6 4e 65 53 b2 21 4f 
cf 75 83 8d 90 dd ae 80 aa 1b e0 e5 f9 a9 c1 aa 
18 0d 2f 14 88 d0 81 94 22 cb 61 71 db 62 a0 db 
ba ed 96 ad 32 3d 17 39 10 f6 76 48 cb 94 d6 93 
88 1b 4a b2 ba 26 5d 8b aa d0 2b c3 61 44 fd 50 
b3 4f 19 5d 09 69 44 d6 a3 b9 6f 15 c2 fd 92 45 
a7 00 77 78 ae 69 33 ae 0d d0 5c bb cf 2d ce fe 
ff 8b cc f2 51 e2 ff 5c 5c 32 a3 e7 93 1f 6d 19 
24 b7 18 2e 75 55 e7 72 29 67 44 95 ba 78 29 8c 
ae 12 7c da db 47 9b a8 f2 20 df 3d 48 58 f6 b1 
-}
