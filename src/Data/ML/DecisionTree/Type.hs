{-# LANGUAGE ScopedTypeVariables #-}
-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sat Jan  3 22:03:50 2015 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Tue Mar  3 09:29:52 2015 (+0100)
--           By: Manuel Schneckenreither
--     Update #: 100
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
module Data.ML.DecisionTree.Type
    ( Attr (..)
    , attr
    , attrNr
    , DTree (..)
    , MinMax (..)
    , Pruning (..)
    )
    where


import Data.ML.Util.Util


import Data.Map (Map)
import qualified Data.Map as M
import Control.Applicative


data MinMax = Minimize | Maximize
data Pruning b = NoPrune | MaxDecisions Int | Predicate (b -> Bool)


-- | Attribute is anything that can split the data into a number of classes.
data Attr a = Attr
              { test :: a -> Int -- ^
              , names :: [String]
              , vals :: [Int] -- ^
              , label :: String
              } |
              AttrNr
              {
                testNr :: a -> Double -- convert numbers to doubles !!!
              , val :: Maybe Double
              , labelNr :: String
              }

instance Show (Attr a) where
  show (Attr _ _ _ l) = "Attr(" ++ l ++ ") "
  show (AttrNr _ Nothing l) = "AttrNr(" ++ l ++ ")"
  show (AttrNr _ (Just x) l) = "AttrNr(" ++ l ++ ") <= " ++ show x


instance Eq (Attr a) where
  x1 == x2 = getLabel x1 == getLabel x2
    where getLabel (Attr _ _ _ l) = l
          getLabel (AttrNr _ _ l) = l

-- |A decision tree which makes decisions based on attributes of type @a@ and
-- returns results of type @b@. We store information of type @i@ at the nodes,
-- which is useful for pruning the tree later.
data DTree a i b = Result b
                 | Decision (Attr a) i (Map Int (DTree a i b))


instance Show b => Show (DTree a i b) where
  show (Result b) = show b
  show (Decision attr _ ts) =
    "Decision " ++ show attr ++ " " ++
    case attr of
      Attr{} -> show (zip (names attr) (M.elems ts))
      AttrNr{} -> show (M.elems ts)


instance Functor (DTree a i) where
  fmap f (Result b) = Result (f b)
  fmap f (Decision attr i branches) = Decision attr i (fmap (fmap f) branches)


instance Applicative (DTree a i) where
  pure = Result
  Result f <*> Result b = Result (f b)
  Result f <*> Decision attr i branches = Decision attr i (fmap (Result f <*>) branches)
  Decision{} <*> _ = error "Not implemented"


instance Monad (DTree a i) where
  return = Result
  Result b >>= f = f b
  Decision attr i ts >>= f = Decision attr i (fmap (>>=f) ts)


-- |Create an attribution from a function and its name.
attr :: forall a b. (Show b, Enum b, Bounded b) => (a -> b) -> String -> Attr a
attr f = Attr (fromEnum . f) (map show vs) (map fromEnum vs)
  where
    vs = enum :: [b]

attrNr   :: Show b => (a -> b) -> String -> Attr a
attrNr f = AttrNr (read . show . f) Nothing

--
-- Type.hs ends here

