module Lib (findBy, equalling) where

findBy :: Eq a => a -> [(a, b)] -> (a,b)
findBy y xs = head $ filter ((==) y . fst) xs

equalling :: (Eq a) => (b -> a) -> b -> b -> Bool
equalling p x y = (==) (p x) (p y)
