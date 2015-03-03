-- Ops.hs ---
--
-- Filename: Ops.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sat Jan  3 22:25:43 2015 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Tue Mar  3 13:48:16 2015 (+0100)
--           By: Manuel Schneckenreither
--     Update #: 244
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

-- | This module implements a decision tree.
module Data.ML.DecisionTree.Ops
    -- ( fitTree
    -- , fitTreeUniform
    -- , fitTreeLeaves
    -- , decide
    -- , attribute
    -- , mapI
    -- , entropy
    -- , missclassificationError
    -- , giniIndex
    -- , impurity
    -- , by

    -- )
    where

import Data.ML.Util.Util


import Data.ML.DecisionTree.Type
import Data.Ord (comparing)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Function (on)
import Control.Arrow
import Data.Maybe (fromJust)


import Debug.Trace

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
attribute f l = Decision (attr f l) () tree
  where
    tree = M.fromList $ zip [0..] (map Result enum)

-- |Run the decision tree on an example
decide :: DTree a i b -> a -> b
decide (Result b) _ = b
decide (Decision att _ branches) a =
  case att of
    Attr{} -> decide (branches M.! test att a) a
    AttrNr _ (Just v) _ -> decide (branches M.! nr) a
      where nr = if testNr att a <= v then 0 else 1
    AttrNr _ _ _ -> error "this should not happen!!!"

-- fitTree :: (Ord a, Ord b) =>
--            (a -> b)              -- Function to get target attribute
--            -> [Attr a]           -- Attributes
--            -> MinMax             -- Max/Min objective function
--            -> ([[b]] -> Float)    -- objective function
--            -> Pruning [b]        -- pruning setting
--            -> [a]                -- data
--            -> DTree a () (Maybe b)
fitTree target atts minMax objFun pr as =
    dropInfo $ fmap mode $ doPrune pr $ decisionTreeLearning target atts minMax objFun [] as


-- fitTreeUniform :: (Ord a, Ord b) =>
--                   b               -- value to compare to
--                -> (a -> b)          -- function to get target attribute
--                -> [Attr a]         -- attributes
--                -> MinMax           -- Max/Min objective function
--                -> ([[b]] -> Float)  -- objective function
--                -> Pruning [b]      -- pruning setting
--                -> [a]              -- data
--                -> DTree a () Float
fitTreeUniform val target atts minMax objFun pr as =
    dropInfo $ fmap (uniform val) $ doPrune pr $
    decisionTreeLearning target atts minMax objFun [] as


-- fitTreeLeaves :: (Ord a, Ord b) =>
--                  (a -> b)         -- function to get target attribute
--               -> [Attr a]         -- attributes
--               -> MinMax           -- Max/Min objective function
--               -> ([[b]] -> Float)  -- objective function
--               -> Pruning [b]      -- pruning setting
--               -> [a]              -- data
--               -> DTree a () [b]
fitTreeLeaves target atts minMax objFun pr as =
    dropInfo $ doPrune pr $ decisionTreeLearning target atts minMax objFun [] as


--------------------- Data Split ---------------------

-- |The decision-tree learning algorithm. This returns a list of elements at
-- each leaf. You can 'fmap' the 'mode' function over the leaves to get the
-- plurality value at that leaf, or the 'uniform' function to get a probability
-- distribution.
-- decisionTreeLearning :: Ord b =>
--                         (a -> b) -- Target function
--                      -> [Attr a] -- Attributes to split on
--                      -> MinMax   -- Max/Min objective function
--                      -> ([[b]] -> Float) -- objective function
--                      -> [a]      -- Examples from the parent node
--                      -> [a]      -- Examples to be split at this node
--                      -> DTree a [b] [b]
decisionTreeLearning target atts minMax objFun ps as
  | null as = Result []
  | null atts || allEqual as' = Result as'
  | otherwise =
    -- trace ("choices: " ++ show (points atts)) $
    Decision att as' (fmap (decisionTreeLearning target atts' minMax objFun as) m)

  where -- ps' = map target ps
        as' = map target as

        (att,atts',m) =
          (case minMax of
            Minimize -> L.minimumBy
            Maximize -> L.maximumBy)
          (comparing (\(_,_,m') -> objFun (map (map target) $ M.elems m')))
          choices

        choices = concat [[(att'', atts', split) | (att'',split) <- partition att' as]
                         | (att',atts') <- points atts]

-- |Partition a list based on a function that maps elements of the list to
-- integers.
-- partition :: Eq a => Attr a -> [a] -> [(Attr a, M.Map Int [a])]
partition _ [] = []
partition att as =
  case att of
    Attr {} -> [(att, L.foldl' fun initial as)]
      where
        fun m a = M.insertWith' (++) (test att a) [a] m
        initial = mkUniversalMap (vals att) []
    AttrNr{} ->
                -- filter (not . hasEmptyList) $ -- filter out elements at start and end
               --  map (\(a, rs) -> (insertVal a, L.foldl' (fun a) (initial a) rs)) (points as)
      map (\(a, rs) -> (insertVal a, let nr = foldl isLeq 0 rs
                                         isLeq acc x' = if testNr att x' <= testNr att a
                                                       then acc + 1
                                                       else acc
                                         (x,y) = L.splitAt nr rs
                                     in
                                      M.fromList [(0,a : x), (1,y)]
                       )) (map (second (++ asMaxs)) $ points asSorted')
      where
        insertVal a = att { val = Just $ testNr att a}
        -- fun e m a = M.insertWith' (++) (if (testNr att a) <= (testNr att e)
        --                                 then 0
        --                                 else 1
        --                              ) [a] m
        -- initial a = M.fromList [(0,[a]),(1,[])]
        -- hasEmptyList (_, m) = any null (M.elems m)

        asSorted = -- map head $ L.groupBy ((==) `on` testNr att) $
                   L.sortBy ((compare `on` testNr att)) as
        asSorted' = filter (not . (== testNr att asMax) . testNr att) asSorted
        asMax = last asSorted
        asMaxs = filter ((== testNr att asMax) . testNr att) asSorted


----------------- Impurity measures ------------------

impurity :: Ord a => ([Float] -> Float) -> [[a]] -> Float
impurity impFun as = (sum $ map (weightedImpurityLeaf impFun) as) / len
  where len = fromIntegral (length $ concat as)

by :: (t1 -> t) -> t1 -> t
by f g = f g

weightedImpurityLeaf :: (Ord a) => ([Float] -> Float) -> [a] -> Float
weightedImpurityLeaf impFun as = impFun probs
  where
    probs = map ((/len) . fromIntegral) $ M.elems $ L.foldl' go M.empty as
    len = fromIntegral (length as)
    go :: Ord k => M.Map k Int -> k -> M.Map k Int
    go m a = M.insertWith' (const (+1)) a 1 m


-- | This function is an impurity measure. It takes as input a list of floats
-- (the ratio of occurrence [0,1], e.g. 1/3 = 0.333) and returns the entropy.
entropy :: (Ord a, Floating a) => [a] -> a
entropy p = sum (map (\x -> if x <= 0.00
                            then 0.00
                            else x * logBase 2 (1/x)) p)


-- | This function is an impurity measure.
missclassificationError :: (Ord a, Num a) => [a] -> a
missclassificationError p = 1 - maximum p


-- | This function is an impurity measure.
giniIndex :: Floating a => [a] -> a
giniIndex p = 1 - sum (map sqrt p)


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
prune                       :: (b -> Bool) -> DTree a b b -> DTree a b b
prune _ (Result b)          = Result b
prune p (Decision att i ts) =
                   if p i
                   then Result i
                   else Decision att i (fmap (prune p) ts)


--
-- Ops.hs ends here
