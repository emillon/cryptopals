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
    foldr go M.empty $ splitOn "&" s
        where
            go kv m =
                let (k, v) = parseKV kv in
                M.insert k v m

parseKV s =
    case listToPair (splitOn "=" s) of
        Nothing -> error $ "parseKV " ++ show s
        Just x -> x

encodeKeyValue :: M.Map String String -> String
encodeKeyValue m =
    intercalate "&" $ map go $ M.toList m
        where
            go (k, v) =
                k ++ "=" ++ v

listToPair :: [a] -> Maybe (a, a)
listToPair [k, v] = Just (k, v)
listToPair _ = Nothing

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
