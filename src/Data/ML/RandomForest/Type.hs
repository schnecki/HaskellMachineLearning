-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sat Feb 28 16:30:58 2015 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sat Feb 28 20:08:39 2015 (+0100)
--           By: Manuel Schneckenreither
--     Update #: 13
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
module Data.ML.RandomForest.Type
    ( Forest (..)
    )
    where


import qualified Data.ML.DecisionTree as DT

newtype Forest a b = Forest [DT.DTree a () b] deriving (Show)


--
-- Type.hs ends here
