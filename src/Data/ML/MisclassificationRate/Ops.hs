-- Ops.hs ---
--
-- Filename: Ops.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sun Mar  1 17:23:50 2015 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun Mar  1 17:24:19 2015 (+0100)
--           By: Manuel Schneckenreither
--     Update #: 2
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
module Data.ML.MisclassificationRate.Ops
    ( mcr

    )
    where

import Data.ML.Util.Util

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


--
-- Ops.hs ends here
