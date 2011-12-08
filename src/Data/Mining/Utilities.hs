module Data.Mining.Utilities where
import Data.List
import Control.Arrow

-- | Sort a list and leave out duplicates.
uniqSort :: (Ord a) => [a] -> [a]
uniqSort = map head . group . sort

prop_uniqSortIsNubSort :: [Int] -> Bool
prop_uniqSortIsNubSort a = uniqSort a == nub (sort a)
-- TODO size constraint (complexity :( )

-- | Turn a list of integers into relative frequencies that sum up to 1.
relFreq :: (Integral i, Fractional f) => [i] -> [f]
relFreq items = map divide items  
        where divide nom = fromIntegral nom / fromIntegral (sum items)

-- | Arithmetic mean of fractional values
average :: (Fractional a) => [a] -> a
average xs = (sum xs) / (genericLength xs)

-- | Arithmetic mean of integer values as a fractional value
averageI :: (Integral i, Fractional a) => [i] -> a
averageI = average . map fromIntegral

-- | Like @List.group@, but does not require adjacency
aggregate :: (Ord a) => [a] -> [[a]]
aggregate = aggregateBy compare

-- | Like @List.groupBy@, but does not require adjacency
aggregateBy :: (a -> a -> Ordering) -> [a] -> [[a]]
aggregateBy x = groupBy (\a b -> x a b == EQ) . sortBy x

-- | Aggregate an association list
aggregateAL :: (Ord a) => [(a,b)] -> [(a,[b])]
aggregateAL = map (fst . head &&& map snd) . aggregateBy (\a b -> compare (fst a) (fst b))
-- TODO quickcheck

-- | @compare@ after applying @f@ to both operands
compareBy :: (Ord b) => (a -> b) -> a -> a -> Ordering
compareBy f a b = compare (f a) (f b)

-- | Like @compareBy snd@, but also permits different types for the fst's.
compareSnd :: (Ord a) => (x, a) -> (z, a) -> Ordering
compareSnd (_, a) (_, b) = compare a b

-- | Pair swapping
swap :: (a, b) -> (b, a)
swap = snd &&& fst

-- | Find the most frequently occurring element
majority :: (Ord a) => [a] -> a
majority = head . maximumBy (compareBy length) . aggregate
