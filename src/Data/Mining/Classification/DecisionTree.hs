{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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

instance (Separator sepa attra label, Separator sepb attrb label) =>
	  Separator (Either sepa sepb) (attra, attrb) label where
    split (Left sep) = split sep . fst
    split (Right sep) = split sep . snd

class GenSep attr separator | attr -> separator where
    gensep :: [attr] -> [separator]


gensepOrd :: (Ord attr) => [attr] -> [SepOrd attr]
gensepOrd db = map SepOrd $ filter (/= minimum db) db
gensepEq :: (Ord attr) => [attr] -> [SepSet attr]
gensepEq db = map SepSet $ subsequences {- ;) -} $ uniqSort db

instance GenSep Double (SepOrd Double) where gensep = gensepOrd
instance GenSep Float (SepOrd Float) where gensep = gensepOrd
instance GenSep Int (SepOrd Int) where gensep = gensepOrd
instance GenSep Integer (SepOrd Integer) where gensep = gensepOrd
instance (Integral a) => GenSep (Ratio a) (SepOrd (Ratio a)) where gensep = gensepOrd

instance GenSep Char (SepSet Char) where gensep = gensepEq
instance GenSep [Char] (SepSet [Char]) where gensep = gensepEq

instance (GenSep a xa, GenSep b xb) =>
          GenSep (a,b) (Either xa xb)
  where gensep v = (map Left . gensep . map fst) v ++
                  (map Right . gensep . map snd) v

uniqSort :: (Ord a) => [a] -> [a]
uniqSort = map head . group . sort

prop_uniqSortIsNubSort a = uniqSort a == nub (sort a)
                       where types = a :: [Int]
-- TODO size constraint (complexity :( )

--splits :: (GenSep attr sep, Separator sep attr label) => [attr] -> x [[(label, [attr])]]
--splits db = map aggregateAL [map (id &&& split sep) db | sep <- gensep db]


--splits :: (Separator sep attr dist, Ord dist, GenSep attr dist) =>
--        (value -> attr) -> [value] -> [[(dist, [value])]]
splits :: (Ord label, GenSep attr t, Separator t attr label) =>
     (a -> attr) -> [a] -> [[(label, [a])]]
splits toattr db = map aggregateAL
         [map ((split sep . toattr) &&& id) db
          | sep <- gensep (map toattr db)]

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

db1 :: [(Int, Char)]
db1 = [(12345,'c'),(1,'a'),(2345,'b'),(13,'a'),(451,'a'),(235,'b'),(46,'a'),(4,'a'),(235,'b'),(425,'b'),(436,'b'),(324,'b')]

data MyClass = A | B | C
                       deriving (Eq, Ord, Show)

db2 :: [((Int, Int), MyClass)]
db2 = [ ((4, 30), A)
      , ((5,  9), A)
      , ((1, 54), A)
      , ((4,  6), A)
      , ((0,  6), A)
      , ((5, 10), A)
      , ((4, 26), A)
      , ((3, 51), A)
        
      , ((0, 77), B)
      , ((3, 80), B)
      , ((7, 51), B)
      , ((2, 30), B)
      , ((9, 77), B)
      , ((5, 33), B)
      , ((7, 48), B)
      , ((3, 76), B)
        
      , (( 5, 66), C)
      , ((11, 48), C)
      , ((13, 68), C)
      , ((11, 39), C)
      , ((14, 47), C)
      , (( 9, 50), C)
      , (( 9, 39), C)
      , (( 9, 61), C)
      ]
