

module Data.ML.TestData.Disease where

import Data.ML.DecisionTree

data Disease = Disease {
    markerA :: Marker
  , markerB :: Marker
  , symptomC :: Bool
  , symptomD :: Bool
  , disease :: Bool
  } deriving (Show, Eq, Ord)


data Marker = High | Low | Normal
             deriving (Show, Eq, Enum, Ord, Bounded)


attrDisease :: [Attr Disease]
attrDisease = [
    attr markerA "markerA"
  , attr markerB "markerB"
  , attr symptomC "symptomC"
  , attr symptomD "symptomD"
  -- , attr disease "disease"
  ]

dataDisease :: [Disease]
dataDisease = [
     Disease High Low True False False
   , Disease High Low True True False
   , Disease Normal Low True False True
   , Disease Low Normal True False True
   , Disease Low High False False True
   , Disease Low High False True False
   , Disease Normal High False True True
   , Disease High Normal True False False
   , Disease High High False False True
   , Disease Low Normal False False True
   , Disease High Normal False True True
   , Disease Normal Normal True True True
   , Disease Normal Low False False True
   , Disease Low Normal True True False
     ]
