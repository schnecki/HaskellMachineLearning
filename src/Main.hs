

-- | Example usage of the library.
module Main
    where
import Data.ML.DecisionTree
import Data.ML.TestData.Disease


import           Text.PrettyPrint.ANSI.Leijen

main :: IO ()
main = do
  let t = fitTreeUniform True disease attrDisease Minimize (impurity `by` giniIndex)
          NoPrune dataDisease
  print (pretty t)

  putStrLn $ "\n\nDecision on following tuple: " ++ show (head dataDisease)
  print (decide t (head dataDisease))
