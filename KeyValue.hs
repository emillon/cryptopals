module KeyValue (parseKeyValue) where

import Data.List.Split

import qualified Data.Map as M

parseKeyValue :: String -> M.Map String String
parseKeyValue "" = M.empty
parseKeyValue s =
    M.fromList $ map parseKV $ splitOn "&" $ s
        where
            parseKV = listToPair . splitOn "="

listToPair :: Show a => [a] -> (a, a)
listToPair [k, v] = (k, v)
listToPair l = error $ "listToPair " ++ show l
