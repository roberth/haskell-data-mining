{- http://www.codeproject.com/KB/recipes/AprioriAlgorithm.aspx 
   http://fimi.ua.ac.be/data/
-}
module Main where

import Control.Applicative
import Control.Monad.Par as P
import Control.Monad.Random
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Environment

type Transaction a = Set a
type Items a = Set a

instance (NFData a) => NFData (Set a) where  
  rnf = rnf . S.toList
  
instance (NFData a, NFData b) => NFData (Map a b) where
  rnf = rnf . M.toList
  
class (Ord a, NFData a) => Item a where

instance Item Int

{-# SPECIALIZE frequencyBy :: (Set Int -> Set Int -> Bool) -> [Set Int] -> [Set Int] -> [(Set Int, Int)] #-}
frequencyBy :: NFData a => (a -> b -> Bool) -> [a] -> [b] -> [(a,Int)]
frequencyBy f as bs = 
  map (\a ->(a, foldr (\b -> if f a b then (+) 1 else id) 0 bs)) as `using` 
    parListChunk 100 rdeepseq

{-# SPECIALIZE rules :: [Transaction Int] -> Items Int -> Int -> Map (Items Int, Items Int) Double #-}
rules :: Item a => [Transaction a] -> Items a -> Int -> Map (Items a, Items a) Double
rules ts is support = M.fromList $ map (\a -> (a, conf a)) $ possiblerules (M.keys fs) where 
  fs = frequentsets ts is support
  conf (a,b) = (fromIntegral $ fs M.! (a `S.union` b)) / (fromIntegral $ fs M.! a)
  
  join :: Eq a => [[a]] -> [[a]]
  join ls = concat $ map join' $ groupBy (\a b->init a == init b) ls where
    join' [] = []
    join' (x:xs) = [ x ++ [y] | y <- map last xs ] ++ join' xs
      
  frequency :: Item a => [Transaction a] -> [Items a] -> Map (Items a) Int
  frequency ts iss = M.fromList $ (frequencyBy S.isSubsetOf iss ts )

  frequentsets :: Item a => [Transaction a] -> Items a -> Int -> Map (Items a) Int
  frequentsets ts is support = M.unions $ takeWhile (/= M.empty) $ map l [0..] where
    l = (M.filter (>= support)) . (frequency ts) . c
    c 0 = map S.singleton $ S.toAscList is
    c n = join' (M.keys $ l (n-1))
    join' = (map S.fromAscList) . join . (map S.toAscList)

  split :: Item a => Items a -> [(Items a, Items a)]
  split = (map (\(a,b)->(S.fromList a, S.fromList b))) . split' . S.toList where
    split' = init . tail . split'' where
      split'' [] = [([],[])]
      split'' (x:ys) = foldr (\(a,b) r -> (x:a,b):(a,x:b):r) [] (split'' ys)

  possiblerules :: Item a => [Items a] -> [(Items a, Items a)]
  possiblerules = concat . (map split)

-- example

loadDataSet :: String -> IO [Transaction Int]
loadDataSet filename = do
  filedata <- readFile filename
  return $ map (S.fromList . (map read) . words) (lines filedata)

test n = do
  transactions <- loadDataSet "T10I4D100K.dat"
  return $ rules (drop 90000 transactions) (S.fromAscList [1..n]) 125

main = test 1000 >>= print
