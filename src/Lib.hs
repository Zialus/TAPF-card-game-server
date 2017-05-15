module Lib (findBy, equalling,maybeRead) where

import           Data.Maybe (listToMaybe)

findBy :: Eq a => a -> [(a, b)] -> Maybe (a,b)
findBy y xs = listToMaybe $ filter ((==) y . fst) xs

equalling :: (Eq a) => (b -> a) -> b -> b -> Bool
equalling p x y = (==) (p x) (p y)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
