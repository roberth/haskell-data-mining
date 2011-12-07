module Data.Mining.Utilities where
import Data.List
import Control.Arrow

uniqSort :: (Ord a) => [a] -> [a]
uniqSort = map head . group . sort

prop_uniqSortIsNubSort :: [Int] -> Bool
prop_uniqSortIsNubSort a = uniqSort a == nub (sort a)
-- TODO size constraint (complexity :( )


relFreq :: (Integral i, Fractional f) => [i] -> [f]
relFreq items = map divide items  
        where divide nom = fromIntegral nom / fromIntegral (sum items)

average :: (Fractional a) => [a] -> a
average xs = (sum xs) / (genericLength xs)

averageI :: (Integral i, Fractional a) => [i] -> a
averageI = average . map fromIntegral

aggregate :: (Ord a) => [a] -> [[a]]
aggregate = aggregateBy compare

aggregateBy :: (a -> a -> Ordering) -> [a] -> [[a]]
aggregateBy x = groupBy (\a b -> x a b == EQ) . sortBy x

-- | Aggregate an association list
aggregateAL :: (Ord a) => [(a,b)] -> [(a,[b])]
aggregateAL = map (fst . head &&& map snd) . aggregateBy (\a b -> compare (fst a) (fst b))
-- TODO quickcheck

swap :: (a, b) -> (b, a)
swap = snd &&& fst
