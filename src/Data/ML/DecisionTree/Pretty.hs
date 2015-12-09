-- Pretty.hs ---
--
-- Filename: Pretty.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Feb 26 18:16:39 2015 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Wed Dec  9 13:52:53 2015 (+0100)
--           By: Manuel Schneckenreither
--     Update #: 12
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
module Data.ML.DecisionTree.Pretty
    ( -- prettyDecisionTree
    )
    where

import Data.ML.DecisionTree.Type

import qualified Data.Map as M
import           Text.PrettyPrint.ANSI.Leijen

-- prettyDecisionTree = undefined

instance (Pretty b) => Pretty (DTree a i b) where
  pretty (Result b) = pretty b
  pretty (Decision (Attr _ n _ l) _ m) =
                    hang 2 $ text l <> line <>
                    -- hang 2 $
                     vcat (map (\(x,y) -> pretty x <> text ":" <+> pretty y)
                                                    (zip n $ M.elems m))
  pretty (Decision (AttrNr _ v l) _ m) =
                    hang 2 $ text l <+> text "<= " <+> text (show v) <> line <>
                     vcat (map pretty (M.elems m))


--
-- Pretty.hs ends here
