module KeyValue ( parseKeyValue
                , profileFor
                ) where

import Data.Char
import Data.List
import Data.List.Split

import qualified Data.Map as M

parseKeyValue :: String -> M.Map String String
parseKeyValue "" = M.empty
parseKeyValue s =
    M.fromList $ map parseKV $ splitOn "&" $ s
        where
            parseKV = listToPair . splitOn "="

encodeKeyValue :: M.Map String String -> String
encodeKeyValue m =
    intercalate "&" $ map go $ M.toList m
        where
            go (k, v) =
                k ++ "=" ++ v

listToPair :: Show a => [a] -> (a, a)
listToPair [k, v] = (k, v)
listToPair l = error $ "listToPair " ++ show l

profileFor :: String -> String
profileFor email =
    encodeKeyValue $ M.fromList
        [ ("email", cleanEmail)
        , ("uid", show $ makeUid cleanEmail)
        , ("role", "user")
        ]
        where
            cleanEmail = filter (`notElem` "&=") email
            makeUid = sum . map ord
