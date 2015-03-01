-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sun Feb 22 11:46:54 2015 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sat Feb 28 16:28:03 2015 (+0100)
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
module Data.ML.CrossValidation.Type
    (

    )
    where

import Control.Monad.Random
import Foreign.Storable (Storable)
import Numeric.LinearAlgebra
import qualified Data.List as L
-- import AI.Util.Matrix
import Data.ML.Util.Util
import Data.ML.DecisionTree.Ops

class Indexable c where
  index :: c -> Index -> c
  nobs :: c -> Int

-- instance Storable a => Indexable (Vector a) where
--   index = subRefVec
--   nobs = dim

-- instance Element a => Indexable (Matrix a) where
--   index = subRefRows
--   nobs = rows

instance Indexable [a] where
  index = map . (!!)
  nobs = length

type Index = [Int]

type CVPartition = [(Index, Index)]

data CVType = LeaveOneOut
            | KFold Int

-- | Prediction function. A prediction function should take a training and a
-- test set, and use the training set to build a model whose performance is
-- evaluated on the test set, returning a final score as a 'Double'.
type PredFun a b = a            -- Training set predictors
                 -> b            -- Training set target
                 -> a            -- Test set predictors
                 -> b            -- Test set target
                 -> Double       -- Performance score


-- | Partition into test and training set.
cvPartition :: RandomGen g => Int -> CVType -> Rand g CVPartition
cvPartition sz cvtype =
  case cvtype of
    KFold i -> cvp sz i
    LeaveOneOut -> cvp sz sz

cvp :: RandomGen g => Int -> Int -> Rand g CVPartition
cvp n k = do
  is <- go i (k - i) idx
  return $ map (\x -> (idx L.\\x, x)) is
  where idx = [0..n-1]
        i = n `mod` k           -- rest of division
        s = n `div` k           -- number of elements to pick

        go 0 0 _   = return []
        go 0 j idx = do
          (is, idx') <- selectMany' s idx
          iss <- go 0 (j-1) idx'
          return (is : iss)
        go i j idx = do
          (is, idx') <- selectMany' (s+1) idx
          iss <- go (i-1) j idx'
          return (is:iss)

-- |Perform k-fold cross-validation. Given a 'CVPartition' containing a list
-- of training and test sets, we repeatedly fit a model on the training set
-- and test its performance on the test set/
kFoldCV_ :: (Indexable a, Indexable b) =>
            CVPartition -> PredFun a b -> a -> b -> [Double]
kFoldCV_ partition predfun x y = map go partition
  where go (trainIdx,testIdx) = predfun xTrain yTrain xTest yTest
          where
            xTrain = x `index` trainIdx
            yTrain = y `index` trainIdx
            xTest = x `index` testIdx
            yTest = y `index` testIdx

-- |Perform k-fold cross-validation, randomly generating the training and
-- test sets first.
kFoldCV :: (RandomGen g, Indexable a, Indexable b) =>
          CVType                -- What type of cross-validation?
        -> PredFun a b           -- Prediction function
        -> a                     -- Predictors
        -> b                     -- Targets
        -> Rand g [Double]       -- List of scores
kFoldCV cvtype predfun x y =
  if nobs x /= nobs y
  then error "Inconsistent dimensions -- KFOLDCV"
  else do
    cp <- cvPartition (nobs x) cvtype
    return (kFoldCV_ cp predfun x y)


--
-- Type.hs ends here
