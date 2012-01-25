{- http://www.codeproject.com/KB/recipes/AprioriAlgorithm.aspx 
   http://fimi.ua.ac.be/data/
-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Mining.Rules where

import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V

type Transaction a = Set a
type Items a = Set a

instance (NFData a) => NFData (Set a) where  
  rnf = rnf . S.toList
  
class (Ord a, NFData a) => Item a where

instance Item Int

-- | Given transactions on items, derive rules for items
{-# SPECIALIZE rules :: [Transaction Int] -> Items Int -> (Map (Items Int) Int -> Map (Items Int) Int) -> Map (Items Int, Items Int) Double #-}
{-# SPECIALIZE rules :: Vector (Transaction Int) -> Items Int -> (Map (Items Int) Int -> Map (Items Int) Int) -> Map (Items Int, Items Int) Double #-}
rules :: (Item a, Foldable container) 
  => container (Transaction a)                -- ^ all transactions
  -> Items a                                  -- ^ the items that you are interested in
  -> (Map (Items a) Int -> Map (Items a) Int) -- ^ a function which can filter itemsets from depending on the support
  -> Map (Items a, Items a) Double            -- ^ the tuple represents a rule, the double represents the confidence in that rule [0,1]
rules ts is f = M.fromList $ map (\a -> (a, conf a)) $ possiblerules fs where 
  frequencyBy :: (NFData a, Foldable container) => 
    (a -> b -> Bool) -> [a] -> container b -> [(a,Int)]
  frequencyBy f as bs = 
    map (\a ->(a, F.foldr (\b -> if f a b then (+) 1 else id) 0 bs)) as `using` 
      parListChunk 100 rdeepseq

  fs = frequentsets ts is f
  conf (a,b) = (fromIntegral $ fs M.! (a `S.union` b)) / (fromIntegral $ fs M.! a)
  
  join :: Eq a => [[a]] -> [[a]]
  join ls = concat $ map join' $ groupBy (\a b->init a == init b) ls where
    join' [] = []
    join' (x:xs) = [ x ++ [y] | y <- map last xs ] ++ join' xs
      
  frequency :: (Item a, Foldable container) => 
    container (Transaction a) -> [Items a] -> Map (Items a) Int
  frequency ts iss = M.fromAscList $ (frequencyBy S.isSubsetOf iss ts )

  frequentsets :: (Item a, Foldable container) => 
    container (Transaction a) -> Items a -> (Map (Items a) Int -> Map (Items a) Int) -> Map (Items a) Int
  frequentsets ts is f = M.unions $ takeWhile (/= M.empty) $ map l [0..] where
    l = f . (frequency ts) . c
    c 0 = map S.singleton $ S.toAscList is
    c n = join' (M.keys $ l (n-1))
    join' = (map S.fromAscList) . join . (map S.toAscList)

  split :: Item a => Items a -> [(Items a, Items a)]
  split = (map (\(a,b)->(S.fromList a, S.fromList b))) . split' . S.toList where
    split' = init . tail . split'' where
      split'' [] = [([],[])]
      split'' (x:ys) = foldr (\(a,b) r -> (x:a,b):(a,x:b):r) [] (split'' ys)

  possiblerules :: Item a => Map (Items a) Int -> [(Items a, Items a)]
  possiblerules fs = filter (\(a,b)->a `M.member` fs && b `M.member` fs) $ concat $ map split $ (M.keys fs)

-- example

-- | Load a dataset from a file, where each line represents one transaction
loadDataSet :: String -> IO [Transaction Int]
loadDataSet filename = do
  filedata <- readFile filename
  return $ map (S.fromList . (map read) . words) (lines filedata)
  
top :: Item a => ([(Items a, Int)] -> [(Items a, Int)]) -> Map (Items a) Int -> Map (Items a) Int
top f m = M.fromList $ f $ sortBy (\(_,a) (_,b) -> compare b a) $ M.toList m 

test = do
  transactions <- loadDataSet "T10I4D100K.dat"
  let items = S.unions transactions
      -- select only the best 40 each round, as long as the support is at least 60
      res = rules (V.fromList $ transactions) items (top ((take 40) . (filter (\(_,a)->a>= 60))))
  -- only return the (sorted) rules with a confidence of at least 0.5
  return $ takeWhile (\(_,a)->a > 0.5) $ sortBy (\(_,a) (_,b) -> compare b a) $ M.toList res
