-- Ops.hs ---
--
-- Filename: Ops.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sat Jan  3 22:25:43 2015 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun Mar  1 13:28:17 2015 (+0100)
--           By: Manuel Schneckenreither
--     Update #: 74
-- URL:
-- Doc URL:
-- Keywords:
-- Compatibility:
--
--

-- Commentary:
--
--
--
--

-- Change Log:
--
--
--
--
--
--
--

-- Code:

-- | TODO: comment this module
module Data.ML.DecisionTree.Ops
    ( fitTree
    , fitTreeUniform
    , fitTreeLeaves
    , decide
    , attribute
    , mapI
    , sumEntropy
    )
    where

import Data.ML.Util.Util


import Data.ML.DecisionTree.Type
import Data.Map (Map, (!))
import Data.Ord (comparing)
import qualified Data.List as L
import qualified Data.Map as M
-- import Control.Arrow
-- import Control.Applicative
-- import Debug.Trace


-- Map over the info part of the whole decision tree.
mapI :: (i -> j) -> DTree a i b -> DTree a j b
mapI _ (Result b) = Result b
mapI f (Decision att i branches) = Decision att (f i) (fmap (mapI f) branches)


-- Drop the info of the given decision tree.
dropInfo :: DTree a i b -> DTree a () b
dropInfo (Result b) = Result b
dropInfo (Decision a _ b) = Decision a () (fmap dropInfo b)

-- | Create a simple decision tree attribute.
attribute :: (Enum b, Bounded b, Show b) => (a -> b) -> String -> DTree a () b
attribute f label = Decision (attr f label) () tree
  where
    tree = M.fromList $ zip [0..] (map Result enum)

-- |Run the decision tree on an example
decide :: DTree a i b -> a -> b
decide (Result b) _ = b
decide (Decision att _ branches) a = decide (branches ! test att a) a


fitTree :: (Ord a, Ord b) =>
           (a -> b)              -- Function to get target attribute
           -> [Attr a]           -- Attributes
           -> MinMax             -- Max/Min objective function
           -> ([[b]] -> Float)    -- objective function
           -> Pruning [b]        -- pruning setting
           -> [a]                -- data
           -> DTree a () b
fitTree target atts minMax objFun pr as =
    dropInfo $ fmap mode $ doPrune pr $ decisionTreeLearning target atts minMax objFun [] as


fitTreeUniform :: (Ord a, Ord b) =>
                  b               -- value to compare to
               -> (a -> b)          -- function to get target attribute
               -> [Attr a]         -- attributes
               -> MinMax           -- Max/Min objective function
               -> ([[b]] -> Float)  -- objective function
               -> Pruning [b]      -- pruning setting
               -> [a]              -- data
               -> DTree a () Float
fitTreeUniform val target atts minMax objFun pr as =
    dropInfo $ fmap (uniform val) $ doPrune pr $
    decisionTreeLearning target atts minMax objFun [] as


fitTreeLeaves :: (Ord a, Ord b) =>
                 (a -> b)         -- function to get target attribute
              -> [Attr a]         -- attributes
              -> MinMax           -- Max/Min objective function
              -> ([[b]] -> Float)  -- objective function
              -> Pruning [b]      -- pruning setting
              -> [a]              -- data
              -> DTree a () [b]
fitTreeLeaves target atts minMax objFun pr as =
    dropInfo $ doPrune pr $ decisionTreeLearning target atts minMax objFun [] as


--------------------- Data Split ---------------------

-- |The decision-tree learning algorithm. This returns a list of elements at
-- each leaf. You can 'fmap' the 'mode' function over the leaves to get the
-- plurality value at that leaf, or the 'uniform' function to get a probability
-- distribution.
decisionTreeLearning :: Ord b =>
                        (a -> b) -- Target function
                     -> [Attr a] -- Attributes to split on
                     -> MinMax   -- Max/Min objective function
                     -> ([[b]] -> Float)         -- objective function
                     -> [a]      -- Examples from the parent node
                     -> [a]      -- Examples to be split at this node
                     -> DTree a [b] [b]
decisionTreeLearning target atts minMax objFun ps as
  | null as = Result ps'
  | null atts || allEqual as' = Result as'
  | otherwise =
    Decision att as' (fmap (decisionTreeLearning target atts' minMax objFun as) m)

  where ps' = map target ps
        as' = map target as

        (att,atts',m) =
          (case minMax of
            Minimize -> L.minimumBy
            Maximize -> L.maximumBy)
          (comparing (\(_,_,m') -> objFun (map (map target) $ M.elems m')))
          choices

        choices = [(att, atts', partition att as) | (att,atts') <- points atts]

-- |Partition a list based on a function that maps elements of the list to
-- integers.
partition :: Attr a -> [a] -> Map Int [a]
partition att = L.foldl' fun initial
  where
    fun m a = M.insertWith' (++) (test att a) [a] m
    initial = mkUniversalMap (vals att) []


------------------ Object Functions ------------------


entropy :: Ord a => [a] -> Float
entropy as = entropy' probs
  where
    entropy' ps = negate . sum $ map (\p -> if p == 0 then 0 else p * log p) ps
    probs = map ((/len) . fromIntegral) $ M.elems $ L.foldl' go M.empty as
    len = fromIntegral (length as)
    go :: Ord k => Map k Int -> k -> Map k Int
    go m a = M.insertWith' (const (+1)) a 1 m

-- |When given a target function, this can be used as an input to the 'minSplit'
-- routine.
sumEntropy :: (Ord b) => [[b]] -> Float
sumEntropy as = sum $ map entropy as


---------------------- Pruning -----------------------

doPrune :: Pruning b -> DTree a b b -> DTree a b b
doPrune NoPrune dt = dt
doPrune (MaxDecisions nr) dt = maxDecisions nr dt
doPrune (Predicate f) dt = prune f dt


-- |Prune a tree to have a maximum depth of decisions.
maxDecisions :: Int -> DTree a b b -> DTree a b b
maxDecisions i (Decision att as ts) =
  if i == 0
  then Result as
  else Decision att as $ fmap (maxDecisions (i-1)) ts
maxDecisions _ r = r


-- |Prune decisions using a predicate.
prune :: (b -> Bool) -> DTree a b b -> DTree a b b
prune _ (Result b) = Result b
prune p (Decision att i ts) =
  if p i
  then Result i
  else Decision att i (fmap (prune p) ts)


---------------------- Testing -----------------------


-- |Compute the misclassification rate (MCR) of a particular decision tree
-- on a data set.
mcr :: Eq b =>
       (a -> b)                  -- Classification algorithm
    -> [a]                       -- List of elements to be classified
    -> [b]                       -- List of correct classifications
    -> Double                    -- Misclassification rate
mcr predfun as bs =
  let bsPred = map predfun as
      numCorrect = countIf id (zipWith (==) bs bsPred)
      numTotal = length as
  in fromIntegral (numTotal - numCorrect) / fromIntegral numTotal


predfun xtrain ytrain xtest ytest = undefined


--
-- Ops.hs ends here
