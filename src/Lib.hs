module Lib (findBy, equalling,maybeRead,count,iterateNTimes,shuffle') where

import           Data.Maybe (listToMaybe)

import           Control.Monad
import           Control.Monad.ST

import           System.Random

import           Data.Array.ST
import           Data.STRef

findBy :: Eq a => a -> [(a, b)] -> Maybe (a,b)
findBy y xs = listToMaybe $ filter ((==) y . fst) xs

equalling :: (Eq a) => (b -> a) -> b -> b -> Bool
equalling p x y = (==) (p x) (p y)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

iterateNTimes :: Int -> (a -> a) -> a -> a
iterateNTimes n f x = iterate f x !! n

count :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffle' :: [a] -> StdGen -> ([a],StdGen)
shuffle' xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- fmap (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray' n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
        where
            n = length xs
            newArray' :: Int -> [a] -> ST s (STArray s Int a)
            newArray' n' =  newListArray (1,n')
