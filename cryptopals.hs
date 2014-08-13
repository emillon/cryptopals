import Control.Monad
import Test.HUnit

import qualified Data.ByteString as B

encodeB64 :: B.ByteString -> String
encodeB64 _ = ""

hexToB64 :: B.ByteString -> B.ByteString
hexToB64 = id

decodeHex :: String -> B.ByteString
decodeHex _ = B.empty

tc :: String -> String -> Test
tc input spec =
    spec ~=? encodeB64 (hexToB64 (decodeHex input) )

main :: IO ()
main =
    void $ runTestTT $ TestList $ map (uncurry tc)
        [ ( ""
          , ""
          )
        ]
