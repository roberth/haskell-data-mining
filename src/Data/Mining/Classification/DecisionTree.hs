{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
-- |
--
--
-- Decision tree learning, used in statistics, data mining and machine
-- learning, uses a decision tree as a predictive model which maps
-- observations about an item to conclusions about the item's target
-- value. In these tree structures, leaves represent class labels and
-- branches represent conjunctions of features that lead to those
-- class labels.
--
-- In data mining, a decision tree describes data but not decisions;
-- rather the resulting classification tree can be an input for
-- decision making.
--
-- (<https://en.wikipedia.org/wiki/Decision_tree_learning>,
-- Dec 6 2011)
--
-- An example:
--
-- >>> :m Data.Mining.Examples Data.Mining.Classification.DecisionTree
-- >>> prettyDTree $ buildDTree fst snd db2
-- Node Left (SepOrd 9)
--  False: Label C
--  True: Node Right (SepOrd 30)
--        False: Node Right (SepOrd 68)
--                False: Label B
--                True: Node Left (SepOrd 5)
--                       False: Node Right (SepOrd 66)
--                               False: Label C
--                               True: Label B
--                       True: Node Left (SepOrd 3)
--                              False: Label A
--                              True: Node Left (SepOrd 2)
--                                     False: Label B
--                                     True: Label A
--        True: Label A
--
-- A proper demonstration with separate training and evaluation data:
--
-- >>> let db2odd = map snd $ filter (odd . fst) $ zip nat db2
-- >>> let db2even = map snd $ filter (even . fst) $ zip nat db2
-- >>> let tree = buildDTree fst snd db2odd 
-- >>> map ((classify tree . fst) &&& snd) db2
-- [(A,A),(A,A),(A,A),(A,A),(A,A),(A,A),(A,A),(A,A),(B,B),(B,B),(B,B),(A,B),(B,B),(A,B),(B,B),(B,B),(C,C),(C,C),(C,C),(C,C),(C,C),(C,C),(C,C),(C,C)]
--

module Data.Mining.Classification.DecisionTree where
import Data.Mining.Utilities
import Data.Mining.Classification.Class
import Data.List
import Data.Ratio
import Data.Maybe
import Control.Arrow hiding ((<+>))
import Text.PrettyPrint ((<>),Doc,text,($+$),nest,empty)

-- | @separator@ defines the type and semantics of a split. Example: \"attr <= 20\".
--
-- The separator bins values of type @attr@.
-- The bins are identified with values of type @result@.
class Separator separator attr result | separator -> attr result where
    -- | Distinguish values of type @attr@ using @separator@.
    split :: separator -> attr -> result

data (Ord t) => SepOrd t = SepOrd t
     deriving (Show, Read)
data (Eq t) => SepSet t = SepSet [t]
     deriving (Show, Read)

instance (Ord attr) => Separator (SepOrd attr) attr Bool where
    split (SepOrd pivot) at = at <= pivot
        -- <= is easier than < in presence of round-down

instance (Eq attr) => Separator (SepSet attr) attr Bool where
    split (SepSet set) at = at `elem` set

instance (Separator sepa attra result, Separator sepb attrb result) =>
	  Separator (Either sepa sepb) (attra, attrb) result where
    split (Left sep) = split sep . fst
    split (Right sep) = split sep . snd

-- | GenSep is used to generate possible splits based on actual attributes
class GenSep attr separator | attr -> separator where
    gensep :: [attr] -> [separator]

-- | @gensep@ implementation for any ordered data, considers all possible @(<= pivot)@s.
gensepOrd :: (Ord attr) => [attr] -> [SepOrd attr]
gensepOrd window = map SepOrd $ filter (/= maximum window) window

-- | @gensep@ implementation for any ordered data, considers all possible @(<= pivot)@s.
gensepOrdAvg :: (Ord attr) => (attr -> attr -> attr) -> [attr] -> [SepOrd attr]
gensepOrdAvg favg window = map SepOrd $ zipWith favg window' (tail window')
        where window' = uniqSort window

-- | @gensep@ implementation for any categorical data, considers all possible sets.
-- | todo: also implement (==) operator
gensepEq :: (Ord attr) => [attr] -> [SepSet attr]
gensepEq window = map SepSet $ subsequences {- ;) -} $ uniqSort window

avgF :: (Fractional a) => a -> a -> a
avgF a b = (a+b) / 2

avgI :: (Integral a) => a -> a -> a
avgI a b = (a+b) `div` 2

instance GenSep Double (SepOrd Double) where gensep = gensepOrdAvg avgF
instance GenSep Float (SepOrd Float) where gensep = gensepOrdAvg avgF
instance GenSep Int (SepOrd Int) where gensep = gensepOrdAvg avgI
instance GenSep Integer (SepOrd Integer) where gensep = gensepOrdAvg avgI
instance (Integral a) => GenSep (Ratio a) (SepOrd (Ratio a)) where gensep = gensepOrdAvg avgF

instance GenSep Char (SepSet Char) where gensep = gensepEq
instance GenSep [Char] (SepSet [Char]) where gensep = gensepEq

instance (GenSep a xa, GenSep b xb) =>
          GenSep (a,b) (Either xa xb)
  where gensep v = (map Left . gensep . map fst) v ++
                  (map Right . gensep . map snd) v

doSplit :: (Separator separator attr result, Ord result) =>
     (x -> attr) -> separator -> [x] -> [(result, [x])]
doSplit toattr sep = aggregateAL . map ((split sep . toattr) &&& id)

doLabel :: (Ord label) => (x -> label) -> [x] -> [(label, [x])]
doLabel tolabel = aggregateAL . map (tolabel &&& id)

measureImpurity :: (Ord label) => (attr -> label) -> [(result, [attr])] -> Double
measureImpurity tolabel = f . impurityAndCounts
  where f :: [(Double, Int)] -> Double
        f = sum . map (uncurry (*) . second fromIntegral)
        impu = gini . map (length . snd) . doLabel tolabel
        impurityAndCounts = map ((impu &&& length) . snd)

rateSplits :: (Separator separator attr result,
              GenSep attr separator,
              Ord result,
              Ord label)
  => (x -> attr)
  -> (x -> label)
  -> [x]
  -> [(separator, Double)]
rateSplits toattr tolabel window = map (\sep -> (sep,) $ measureImpurity tolabel $ doSplit toattr sep window) . gensep . map toattr $ window
                   
data DTree sep result label = Node sep [(result, DTree sep result label)]
                     | Leaf label
                       deriving (Show, Eq)
                                
data DTreeAlgebra sep result label a =
  DTreeAlgebra { fleaf :: label -> a
               , fnode :: sep -> [(result, a)] -> a
               }

foldD :: DTreeAlgebra sep result label a -> DTree sep result label -> a
foldD (DTreeAlgebra fleaf _) (Leaf label) = fleaf label
foldD a@(DTreeAlgebra _ fnode) (Node sep children) = fnode sep $ map (second (foldD a)) children

--decideAlgebra :: DTreeAlgebra 
predictAlgebra :: (Separator sep attr result, Eq result) =>
                  attr -> DTreeAlgebra sep result label label
predictAlgebra newobservation = DTreeAlgebra { fleaf = fleaf, fnode = fnode }
 where
   fleaf = id
   fnode sep children = error "Incomplete tree"
           `fromMaybe` lookup (split sep newobservation) children

-- | Use a DTree to predict the class label of a (possibly) yet unseen object.
predict :: (Separator sep attr result, Eq result) =>
           attr -> DTree sep result a -> a
predict a = foldD (predictAlgebra a)

instance (Separator sep attr result, Eq result) => Classifier (DTree sep result label) attr label where
  classify = flip predict

-- | Learn a Decision Tree classifier based on a list of observations.
buildDTree :: (Ord label, Ord result,
      GenSep attr sep,
      Separator sep attr result) =>
     (x -> attr) ->
     (x -> label) ->
     [x] ->
     DTree sep result label
buildDTree toattr tolabel window = case rateSplits toattr tolabel window of
  [] -> case window of
    [] -> error "Empty window"
    window -> Leaf . majority . map tolabel $ window
  splits -> case uniqSort (map tolabel window) of
    [x] -> Leaf x
    _ -> let 
            (best, _) = minimumBy compareSnd splits
            subwins = doSplit toattr best window
        in Node best $ map (second (buildDTree toattr tolabel)) subwins

-- | Pretty-print a decision tree. Indentation relates to depth.
prettyDTree :: (Show sep, Show res, Show lab) => DTree sep res lab -> Doc
prettyDTree (Node sep children) = text "Node " <> text (show sep)
                      $+$ nest 3 (
                      foldr ($+$) empty (map printChild children)
                      )
  where printChild = uncurry (<>) . first (text . (++": ") .  show) . second (prettyDTree)
prettyDTree (Leaf x) = text "Label " <> text (show x)

-- | Calculate the gini impurity based on the real class label frequencies.
gini :: (Integral i, Fractional f) => [i] -> f
gini = sum . map (\x -> x * (1 - x)) . relFreq
