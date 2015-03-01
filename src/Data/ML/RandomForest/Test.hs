-- Test.hs ---
--
-- Filename: Test.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sat Jan  3 23:25:42 2015 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun Mar  1 13:32:01 2015 (+0100)
--           By: Manuel Schneckenreither
--     Update #: 95
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
module Data.ML.RandomForest.Test where


import qualified Data.ML.DecisionTree as DT
import Data.ML.RandomForest.Pretty ()
import Data.ML.RandomForest.Type
import Data.ML.RandomForest.Ops

import Data.ML.Util.Util
import System.Random
import           Text.PrettyPrint.ANSI.Leijen
import Control.Monad
import Control.Monad.Random

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
      willWait = DT.decide actualTree (mkR False)
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
      willWait = DT.decide actualTree (mkR False)
  return (mkR willWait)

randomDataSetNoisy :: RandomGen g => Float -> Int -> Rand g [Restaurant]
randomDataSetNoisy noise n = replicateM n (randomRestaurantNoisy noise)

randomDataSet :: RandomGen g => Int -> Rand g [Restaurant]
randomDataSet n = replicateM n randomRestaurant


actualTree :: DT.DTree Restaurant () Bool
actualTree = do
  patrons <- DT.attribute pat "Patrons"
  case patrons of
    Empty -> return False
    Some  -> return True
    Full  -> do
      time <- DT.attribute wait "WaitTime"
      case time of
        None  -> return True
        Short -> do
          hungry <- DT.attribute hun "Hungry"
          if not hungry
            then return True
            else do
              alternative <- DT.attribute alt "Alternative"
              if not alternative
                then return True
                else do
                  raining <- DT.attribute rain "Rain"
                  return (if raining then True else False)
        Med   -> do
          alternative <- DT.attribute alt "Alternative"
          if not alternative
            then do
              reservation <- DT.attribute res "Reservation"
              if reservation
                then return True
                else do
                  hasBar <- DT.attribute bar "Bar"
                  return (if hasBar then True else False)
            else do
              friday <- DT.attribute fri "Fri/Sat"
              return (if friday then True else False)
        Long  -> return False


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


forest :: IO (Forest Restaurant Bool)
forest = evalRandIO $ randomForest 3 willWait atts restaurants


forestRand :: IO (Forest Restaurant Bool)
forestRand = do
  rs <- evalRandIO (randomDataSetNoisy 0.23 120)
  evalRandIO $ randomForest 3 willWait atts rs


forestLeavesIO :: IO ()
forestLeavesIO = do
  fs <- evalRandIO $ randomForestLeaves 3
        willWait atts DT.Minimize DT.sumEntropy DT.NoPrune restaurants
  print $ pretty fs


forestUniformIO :: IO ()
forestUniformIO = do
  fs <- evalRandIO $ randomForestUniform True 3
        willWait atts DT.Minimize DT.sumEntropy DT.NoPrune restaurants
  print $ pretty fs


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


decideForestRand   :: Restaurant -> IO ()
decideForestRand r = do
  fs <- evalRandIO $ randomForest 3 willWait atts restaurants
  print $ pretty fs
  print r
  print $ decide fs r

decideLeavesForestRand   :: Restaurant -> IO ()
decideLeavesForestRand r = do
  fs <- evalRandIO $ randomForest 3 willWait atts restaurants
  print $ pretty fs
  print r
  print $ decideLeaves fs r

decideUniformForestRand   :: Restaurant -> IO ()
decideUniformForestRand r = do
  fs <- evalRandIO $ randomForest 3 willWait atts restaurants
  print $ pretty fs
  print r
  print $ decideUniform True fs r


--
-- Test.hs ends here
