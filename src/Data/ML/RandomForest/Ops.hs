-- Ops.hs ---
--
-- Filename: Ops.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sat Feb 28 20:07:10 2015 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun Mar  1 13:30:40 2015 (+0100)
--           By: Manuel Schneckenreither
--     Update #: 87
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
module Data.ML.RandomForest.Ops
    ( randomForest
    , decide
    , decideLeaves
    , randomForestLeaves
    , randomForestUniform
    , decideUniform
    )
    where

import Data.ML.RandomForest.Type
import qualified Data.ML.DecisionTree as DT
import Data.ML.Util.Util

import Control.Monad.Random hiding (uniform)

-- | This is a default random forest using the minimization of the sum of the
-- entropy for splitting. See randomForest' for customization of the splitting
-- procedure. There will be no pruning.
randomForest :: (Ord a, Ord b, RandomGen g) =>
               Int              -- Number of trees in the forest
             -> (a -> b)          -- Target attribute
             -> [DT.Attr a]      -- Attributes to classify on
             -> [a]              -- List of observations
             -> Rand g (Forest a b)
randomForest nTrees target attrs =
  randomForest' nTrees target attrs DT.Minimize DT.sumEntropy DT.NoPrune


-- | Advanced method for specifying own objective function.
randomForest' :: (Ord a, Ord b, RandomGen g) =>
                Int              -- Number of trees in the forest
              -> (a -> b)          -- Target attribute
              -> [DT.Attr a]      -- Attributes to classify on
              -> DT.MinMax        -- Max/Min objective function
              -> ([[b]] -> Float)  -- objective function
              -> DT.Pruning [b]   -- pruning setting
              -> [a]              -- List of observations
              -> Rand g (Forest a b)
randomForest' = fun DT.fitTree

-- | Simple caller method, which creates the decision trees.
fun :: MonadRandom m =>
     (t -> t1 -> t2 -> t3 -> t4 -> [a] -> DT.DTree a1 () b)
     -> Int -> t -> t1 -> t2 -> t3 -> t4 -> [a] -> m (Forest a1 b)
fun ff nTrees target attrs minMax objFun pr as = do
  let minLen = length as `div` (2 * nTrees)
  sps <- splits (nTrees-1) minLen as
  return $ Forest $ map (ff target attrs minMax objFun pr) sps

-- | Create decision trees with percentages at leaves.
randomForestUniform :: (Ord a, Ord b, RandomGen g) =>
                       b                -- value to compare to
                    -> Int               -- Number of trees in the forest
                    -> (a -> b)           -- Target attribute
                    -> [DT.Attr a]       -- Attributes to classify on
                    -> DT.MinMax         -- Max/Min objective function
                    -> ([[b]] -> Float)   -- objective function
                    -> DT.Pruning [b]    -- pruning setting
                    -> [a]               -- List of observations
                    -> Rand g (Forest a Float)
randomForestUniform val = fun (DT.fitTreeUniform val)

-- | Do no collapse leaves, rather return a list of observed leaves.
randomForestLeaves :: (Ord a, Ord b, RandomGen g) =>
                      Int             -- Number of trees in the forest
                   -> (a -> b)          -- Target attribute
                   -> [DT.Attr a]      -- Attributes to classify on
                   -> DT.MinMax        -- Max/Min objective function
                   -> ([[b]] -> Float)  -- objective function
                   -> DT.Pruning [b]   -- pruning setting
                   -> [a]              -- List of observations
                   -> Rand g (Forest a [b])
randomForestLeaves = fun DT.fitTreeLeaves

-- | This function is used to get the most likely decision on an input according
-- to the given forest. See @decideLeaves@ for manual processing of the leaves.
decide                  :: Ord b => Forest a b -> a -> b
decide (Forest trees) a = mode $ map (`DT.decide` a) trees

-- | This function returns all leaves of the forest for further manual
-- processing.
decideLeaves                  :: Forest a b -> a -> [b]
decideLeaves (Forest trees) a = map (`DT.decide` a) trees

-- | This function calculates the @b@'s inside the leaves of the decision trees in
-- the @forest@ by an input of @a@.
decideUniform                    :: (Ord b) => b -> Forest a b -> a -> Float
decideUniform b (Forest trees) a = uniform b $ map (`DT.decide` a) trees


{- | Example

data Patrons = Empty | Some | Full deriving (Show,Eq,Ord,Enum,Bounded)
data Price = Cheap | Medium | Expensive deriving (Show,Eq,Ord,Enum,Bounded)
data Type = French | Thai | Burger | Italian deriving (Show,Eq,Ord,Enum,Bounded)
data Wait = None | Short | Med | Long deriving (Show,Eq,Ord,Enum,Bounded)

data Restaurant = Restaurant {
    alt :: Bool,        -- is there an alternative?
    bar :: Bool,        -- is there a bar?
    fri :: Bool,        -- is it a friday?
    hun :: Bool,        -- are you hungry?
    pat :: Patrons,     -- how many patrons are there?
    price :: Price,     -- how cheap is it?
    rain :: Bool,       -- is it raining?
    res :: Bool,        -- do you have a reservation?
    food :: Type,       -- what type of food is it?
    wait :: Wait,       -- what is the wait?
    willWait :: Bool    -- will you wait?
} deriving (Show,Eq,Ord)


atts :: [DT.Attr Restaurant]
atts = [ DT.attr alt "Alternative"
       , DT.attr bar "Bar"
       , DT.attr fri "Friday"
       , DT.attr hun "Hungry"
       , DT.attr pat "Patrons"
       , DT.attr price "Price"
       , DT.attr rain "Raining"
       , DT.attr res "Reservation"
       , DT.attr food "Food"
       , DT.attr wait "Wait" ]


restaurants :: [Restaurant]
restaurants =
 [ Restaurant True  False False True  Some  Expensive False True  French  None  True
 , Restaurant True  False False True  Full  Cheap     False False Thai    Med   False
 , Restaurant False True  False False Some  Cheap     False False Burger  None  True
 , Restaurant True  False True  True  Full  Cheap     True  False Thai    Short True
 , Restaurant True  False True  False Full  Expensive False True  French  Long  False
 , Restaurant False True  False True  Some  Medium    True  True  Italian None  True
 , Restaurant False True  False False Empty Cheap     True  False Burger  None  False
 , Restaurant False False False True  Some  Medium    True  True  Thai    None  True
 , Restaurant False True  True  False Full  Cheap     True  False Burger  Long  True
 , Restaurant True  True  True  True  Full  Expensive False True  Italian Short False
 , Restaurant False False False False Empty Cheap     False False Thai    None  False
 , Restaurant True  True  True  True  Full  Cheap     False False Burger  Med   True ]


restaurant :: Restaurant
restaurant = Restaurant False True False True Some Cheap False True Burger None True

forestIO :: IO ()
forestIO = do
  fs <- evalRandIO $ randomForest 3 willWait atts restaurants
  print $ pretty fs


decideForest   :: Restaurant -> IO ()
decideForest r = do
  fs <- evalRandIO $ randomForest 3 willWait atts restaurants
  print $ pretty fs
  print r
  print $ decideLeaves fs r


-}

--
-- Ops.hs ends her
