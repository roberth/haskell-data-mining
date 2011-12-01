{- http://www.codeproject.com/KB/recipes/AprioriAlgorithm.aspx -}
module Rules where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Item = Int

type Transaction = Set Item
type ItemSet = Set Item

items :: [Transaction] -> [ItemSet]
items = (map S.singleton) . S.toList . S.unions

join :: Eq a => Int -> [[a]] -> [[a]]
join n ls = concat $ map join' $ groupBy (\a b->take n a == take n b) ls where
  join' [] = []
  join' (x:xs) = [ x ++ [y] | y <- map (!! n) xs ] ++ join' xs

frequency :: [Transaction] -> [ItemSet] -> Map ItemSet Int
frequency ts is = foldr (\a b -> foldr (frequency' a) b is) (M.fromList $ zip is $ repeat 0) ts where
  frequency' :: Transaction -> ItemSet -> Map ItemSet Int -> Map ItemSet Int
  frequency' t i = if i `S.isSubsetOf` t then M.adjust (+ 1) i else id

prune :: Int -> Map ItemSet Int -> Map ItemSet Int
prune i = M.filter (>= i)

frequentsets :: [Transaction] -> Int -> Map ItemSet Int
frequentsets ts support = M.unions $ takeWhile (/= M.empty) $ map l [0..] where
  l n = prune support (c n)
  c 0 = frequency ts (items ts)
  c n = frequency ts (join' (n-1) (M.keys $ l (n-1)))
  join' n = (map S.fromList) . (join n) . (map S.toList)

split :: ItemSet -> [(ItemSet, ItemSet)]
split = (map (\(a,b)->(S.fromList a, S.fromList b))) . split' . S.toList where
  split' :: [Item] -> [([Item], [Item])]
  split' = init . tail . split'' where
    split'' [] = [([],[])]
    split'' (x:ys) = foldr (\(a,b) r -> (x:a,b):(a,x:b):r) [] (split'' ys)

possiblerules :: [ItemSet] -> [(ItemSet, ItemSet)]
possiblerules = concat . (map split)

rules :: [Transaction] -> Int -> Double -> Map (ItemSet, ItemSet) Double
rules ts support confidence = M.fromList $ filter (\(_,b) -> b >= confidence) $ map (\a -> (a, conf a)) $ possiblerules (M.keys fs) where 
  fs = frequentsets ts support
  conf (a,b) = (fromIntegral $ fs M.! (a `S.union` b)) / (fromIntegral $ fs M.! a)

-- example

newtype Rule = Rule ([Item], [Item])

instance Show Rule where
  show (Rule (a, b)) = show a ++ " -> " ++ show b

transactions :: [Transaction]
transactions = map S.fromList [[1,3,4],[2,3,5],[1,2,3,5],[2,5]]

readable = (map (\((a,b),_) -> Rule (S.toList a, S.toList b))) . M.toList

main = print $ readable $ rules transactions 2 0.8
