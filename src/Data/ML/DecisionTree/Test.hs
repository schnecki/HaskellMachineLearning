-- Test.hs ---
--
-- Filename: Test.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sat Jan  3 23:25:42 2015 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun Mar  1 13:28:06 2015 (+0100)
--           By: Manuel Schneckenreither
--     Update #: 56
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


module Data.ML.DecisionTree.Test where


import Data.ML.DecisionTree.Ops
import Data.ML.DecisionTree.Type
import Data.ML.DecisionTree.Pretty()

-- import qualified AI.Learning.RandomForest as RF
import Data.ML.Util.Util
import System.Random
import           Text.PrettyPrint.ANSI.Leijen
import Control.Monad
import Control.Monad.Random
import qualified Data.List as L

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


atts :: [Attr Restaurant]
atts = [ attr alt "Alternative"
       , attr bar "Bar"
       , attr fri "Friday"
       , attr hun "Hungry"
       , attr pat "Patrons"
       , attr price "Price"
       , attr rain "Raining"
       , attr res "Reservation"
       , attr food "Food"
       , attr wait "Wait" ]

randomRestaurantNoisy :: RandomGen g => Float -> Rand g Restaurant
randomRestaurantNoisy noise = do
  alt   <- getRandom
  bar   <- getRandom
  fri   <- getRandom
  hun   <- getRandom
  pat   <- getRandomEnum 3
  price <- getRandomEnum 3
  rain  <- getRandom
  res   <- getRandom
  food  <- getRandomEnum 4
  wait  <- getRandomEnum 4
  let mkR ww   = Restaurant alt bar fri hun pat price rain res food wait ww
      willWait = decide actualTree (mkR False)
  p <- getRandomR (0,1)
  return $ if p > noise
    then mkR willWait
    else mkR (not willWait)

randomRestaurant :: RandomGen g => Rand g Restaurant
randomRestaurant = do
  alt   <- getRandom
  bar   <- getRandom
  fri   <- getRandom
  hun   <- getRandom
  pat   <- getRandomEnum 3
  price <- getRandomEnum 3
  rain  <- getRandom
  res   <- getRandom
  food  <- getRandomEnum 4
  wait  <- getRandomEnum 4
  let mkR ww   = Restaurant alt bar fri hun pat price rain res food wait ww
      willWait = decide actualTree (mkR False)
  return (mkR willWait)

randomDataSetNoisy :: RandomGen g => Float -> Int -> Rand g [Restaurant]
randomDataSetNoisy noise n = replicateM n (randomRestaurantNoisy noise)

randomDataSet :: RandomGen g => Int -> Rand g [Restaurant]
randomDataSet n = replicateM n randomRestaurant


actualTree :: DTree Restaurant () Bool
actualTree = do
  patrons <- attribute pat "Patrons"
  case patrons of
    Empty -> return False
    Some  -> return True
    Full  -> do
      time <- attribute wait "WaitTime"
      case time of
        None  -> return True
        Short -> do
          hungry <- attribute hun "Hungry"
          if not hungry
            then return True
            else do
              alternative <- attribute alt "Alternative"
              if not alternative
                then return True
                else do
                  raining <- attribute rain "Rain"
                  return (if raining then True else False)
        Med   -> do
          alternative <- attribute alt "Alternative"
          if not alternative
            then do
              reservation <- attribute res "Reservation"
              if reservation
                then return True
                else do
                  hasBar <- attribute bar "Bar"
                  return (if hasBar then True else False)
            else do
              friday <- attribute fri "Fri/Sat"
              return (if friday then True else False)
        Long  -> return False


fittedTreeRandom :: IO ()
fittedTreeRandom = do
  rs <- evalRandIO (randomDataSetNoisy 0.23 120)
  let tree = fitTree willWait atts Minimize sumEntropy NoPrune rs
  print $ pretty tree

fittedTreeLeavesRandom :: IO ()
fittedTreeLeavesRandom = do
  rs <- evalRandIO (randomDataSetNoisy 0.23 120)
  let tree = fitTreeLeaves willWait atts Minimize sumEntropy NoPrune rs
  print $ pretty tree


fittedTreeUniformRandom :: IO ()
fittedTreeUniformRandom = do
  rs <- evalRandIO (randomDataSetNoisy 0.23 120)
  let tree = fitTreeUniform True willWait atts Minimize sumEntropy NoPrune rs
  print $ pretty tree


------------------------------------------

restaurant :: Restaurant
restaurant = Restaurant False True False True Some Cheap False True Burger None True

    -- alt :: Bool,        -- is there an alternative?
    -- bar :: Bool,        -- is there a bar?
    -- fri :: Bool,        -- is it a friday?
    -- hun :: Bool,        -- are you hungry?
    -- pat :: Patrons,     -- how many patrons are there?
    -- price :: Price,     -- how cheap is it?
    -- rain :: Bool,       -- is it raining?
    -- res :: Bool,        -- do you have a reservation?
    -- food :: Type,       -- what type of food is it?
    -- wait :: Wait,       -- what is the wait?


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


fittedTree :: DTree Restaurant () Bool
fittedTree = fitTree willWait atts Minimize sumEntropy NoPrune restaurants


fittedTreeUniform :: DTree Restaurant () Float
fittedTreeUniform = fitTreeUniform True willWait atts Minimize sumEntropy NoPrune restaurants


fittedTreeLeaves :: DTree Restaurant () [Bool]
fittedTreeLeaves = fitTreeLeaves willWait atts Minimize sumEntropy NoPrune restaurants

--
-- Test.hs ends here
