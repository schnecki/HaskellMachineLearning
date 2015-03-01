-- Pretty.hs ---
--
-- Filename: Pretty.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sat Feb 28 19:47:39 2015 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sat Feb 28 20:05:38 2015 (+0100)
--           By: Manuel Schneckenreither
--     Update #: 11
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
module Data.ML.RandomForest.Pretty where

import Data.ML.RandomForest.Type

import qualified Data.Map as M
import           Text.PrettyPrint.ANSI.Leijen


instance (Pretty b) => Pretty (Forest a b) where
  pretty (Forest dts) =
    vcat $ punctuate line $
    fmap (\(n,dt) -> text ("Decision Tree No. " ++ show n) <$$>
                     text "---------------------" <$$> line <> pretty dt) $ zip [1..] dts


--
-- Pretty.hs ends here
