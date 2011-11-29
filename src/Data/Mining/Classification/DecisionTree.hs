{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Mining.Classification.DecisionTree where
import Data.List
import Data.Ratio
import Control.Arrow

class Separator separator attr label | separator -> attr label where
    split :: separator -> attr -> label

data (Ord t) => SepOrd t = SepOrd t
     deriving (Show, Read)
data (Eq t) => SepSet t = SepSet [t]
     deriving (Show, Read)

instance (Ord attr) => Separator (SepOrd attr) attr Bool where
    split (SepOrd pivot) at = at < pivot

instance (Eq attr) => Separator (SepSet attr) attr Bool where
    split (SepSet set) at = at `elem` set


class GenSep attr separator | attr -> separator where
    gensep :: [attr] -> [separator]


gensepOrd :: (Ord attr) => [attr] -> [SepOrd attr]
gensepOrd db = map SepOrd $ filter (/= minimum db) db
 gensepEq :: (Ord attr) => [attr] -> [SepSet attr]
gensepEq db = map SepSet $ subsequences {- ;) -} $ uniqSort db

instance GenSep Double (SepOrd Double) where gensep = gensepOrd
instance GenSep Float (SepOrd Float) where gensep = gensepOrd
instance GenSep Int (SepOrd Int) where gensep = gensepOrd
instance (Integral a) => GenSep (Ratio a) (SepOrd (Ratio a)) where gensep = gensepOrd

instance GenSep Char (SepSet Char) where gensep = gensepEq
instance GenSep [Char] (SepSet [Char]) where gensep = gensepEq

uniqSort :: (Ord a) => [a] -> [a]
uniqSort = map head . group . sort

prop_uniqSortIsNubSort a = uniqSort a == nub (sort a)
                       where types = a :: [Int]
-- TODO size constraint (complexity :( )

--splits :: (GenSep attr sep, Separator sep attr label) => [attr] -> x [[(label, [attr])]]
--splits db = map aggregateAL [map (id &&& split sep) db | sep <- gensep db]

aggregate :: (Ord a) => [a] -> [[a]]
aggregate = aggregateBy compare

aggregateBy :: (a -> a -> Ordering) -> [a] -> [[a]]
aggregateBy x = groupBy (\a b -> x a b == EQ) . sortBy x

-- | Aggregate an association list
aggregateAL :: (Ord a) => [(a,b)] -> [(a,[b])]
aggregateAL = map (fst . head &&& map snd) . aggregateBy (\a b -> compare (fst a) (fst b))
-- TODO quickcheck


impurity :: ((Int, Int) -> Double) -> [Bool] -> Double
impurity measure = measure . first length . second length . partition id

nrange :: (Double -> Double) -> ((Int, Int) -> Double)
nrange f (a, b) = let sum = fromIntegral $ a + b
                  in fromIntegral a / sum

gini = nrange $ \a -> (a * (1.0 - a))