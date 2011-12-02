{- http://www.codeproject.com/KB/recipes/AprioriAlgorithm.aspx -}
module Main where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.Random
import Control.Applicative
import Control.Parallel.Strategies
import Control.DeepSeq

type Transaction a = Set a
type Items a = Set a

instance (NFData a) => NFData (Set a) where  
  rnf = rnf . S.toList
  
instance (NFData a, NFData b) => NFData (Map a b) where
  rnf = rnf . M.toList
  
class (Ord a, NFData a) => Item a where

instance Item Int

join :: Eq a => [[a]] -> [[a]]
join ls = concat $ map join' $ groupBy (\a b->init a == init b) ls where
  join' [] = []
  join' (x:xs) = [ x ++ [y] | y <- map last xs ] ++ join' xs
{-  
frequency :: Item a => [Transaction a] -> [Items a] -> Map (Items a) Int
frequency ts iss = M.fromList (map (\is -> (is, foldr (frequency' is) 0 ts)) iss `using` parList rdeepseq) where
  frequency' is t = if is `S.isSubsetOf` t then succ else id
-}  
frequency :: Item a => [Transaction a] -> [Items a] -> Map (Items a) Int
frequency ts iss = M.fromList (map (\is -> (is, sum (map (frequency' is) ts))) iss `using` parList rdeepseq) where
  frequency' is t = if is `S.isSubsetOf` t then 1 else 0

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
possiblerules = concat . (map Main.split)

rules :: Item a => [Transaction a] -> Items a -> Int -> Map (Items a, Items a) Double
rules ts is support = M.fromList $ map (\a -> (a, conf a)) $ possiblerules (M.keys fs) where 
  fs = frequentsets ts is support
  conf (a,b) = (fromIntegral $ fs M.! (a `S.union` b)) / (fromIntegral $ fs M.! a)

-- example

{-
transactions :: [Transaction Int]
transactions = map S.fromList [[1,3,4],[2,3,5],[1,2,3,5],[2,5]]

main = print $ M.filter (>= 0.8) $ rules transactions (S.fromAscList [1..5]) 2
-}

--{-
ts :: (MonadRandom m, Applicative m) => Int -> m [Transaction Int]
ts n = fmap (map S.fromList) $ sequence $ take n $ repeat (take <$> getRandomR (2,20) <*> getRandomRs (1,n))

test n = do
  transactions <- ts n
  return $ rules transactions (S.fromAscList [1..n]) 15

main = test 1000 >>= print
--}
