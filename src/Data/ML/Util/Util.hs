-- util.hs ---
--
-- Filename: Util.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sat Jan  3 22:57:24 2015 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun Mar  1 19:22:39 2015 (+0100)
--           By: Manuel Schneckenreither
--     Update #: 61
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
module Data.ML.Util.Util where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import Control.Arrow ((&&&))
import Control.Monad.Random
import Control.Monad (liftM)

enum :: (Enum b, Bounded b) => [b]
enum = [minBound .. maxBound]

-- |Return the most common value in a list.
mode :: Ord b => [b] -> Maybe b
mode [] = Nothing
mode xs = return $ fst $ L.maximumBy (O.comparing snd) $
          map (head &&& length) $ L.group $ L.sort xs

uniform :: Ord b => b -> [b] -> Float
uniform val xs = fromIntegral (length $ L.elemIndices val xs) / fromIntegral (length xs)

-- |Return 'True' if all elements of the list are equal.
allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (a:as) = all (==a) as


-- |Count the number of elements in a list that satisfy a predicate.
countIf :: (a -> Bool) -> [a] -> Int
countIf p xs = length (filter p xs)


-- |Returns a list of pairs. Each pair consists of an element from the list,
-- and the rest of the list with the element removed. This is useful for
-- deleting elements from a list where no 'Eq' instance is defined on elements
-- (eg function types).
--
-- >>> points [1,2,3]
-- [(1,[2,3]),(2,[1,3]),(3,[1,2])]
-- >>> points [1,2,3,4]
-- [(1,[2,3,4]),(2,[1,3,4]),(3,[1,2,4]),(4,[1,2,3])]
points :: [a] -> [(a,[a])]
points [] = []
points (a:as) = (a,as) : [ (b,a:bs) | (b,bs) <- points as ]


-- |Generate a random variable from the 'Enum' and 'Bounded' type class. The
-- 'Int' input specifies how many values are in the enumeration.
getRandomEnum :: (RandomGen g, Enum a, Bounded a) => Int -> Rand g a
getRandomEnum i = liftM toEnum (getRandomR (0,i-1))

-------------------
-- Map Functions --
-------------------
-- |A universal map maps all keys to the same value.
mkUniversalMap :: Ord k => [k] -> a -> M.Map k a
mkUniversalMap ks a = M.fromList $ zip ks (repeat a)


--------------------
-- Random Numbers --
--------------------

splits :: MonadRandom m => Int -> Int -> [a] -> m [[a]]
splits splts minSplitLen xs = do
  splitsIdices <- splitIdx splts (length xs-1) minSplitLen
  return $ fst $ foldl (\(ls,as') nr -> let (x, rest) = splitAt (nr - sum (map length ls)) as'
                               in (ls ++ [x], rest)
              ) ([], xs) (L.sort splitsIdices ++ [length xs])
  where


splitIdx :: (MonadRandom m) => Int -> Int -> Int -> m [Int]
splitIdx 0 _ _ = return . return $ 0
splitIdx nr upper minLen = if nr > (upper - 2 * minLen)
                                   then error "not possible"
                                   else randNumsDistinct' 0 nr []
  where randNumsDistinct' 100 _ _ = error "could not find splits"
        randNumsDistinct' _ 0 list = return list
        randNumsDistinct' tries nr' list = do
          n <- getRandomR (minLen, upper - minLen)
          if n `elem` list || inRange n list
             then randNumsDistinct' (tries + 1) nr' list
            else randNumsDistinct' 0 (nr'-1) (n:list)

        inRange x xs = any (\y -> (x + minLen > y && x < y) ||
                                  (x - minLen < y && x > y)) xs


selectOne :: Eq a => RandomGen g => [a] -> Rand g (a, [a])
selectOne xs = do
  let n = length xs
  i <- getRandomR (0, n-1)       -- randomly select an index
  let x = xs !! i                -- get element
  return (x, L.delete x xs)

-- | Select a number of elements at random and return the elements not chosen.
selectMany'      :: (RandomGen g, Eq a) => Int -> [a] -> Rand g ([a], [a])
selectMany' 0 xs = return ([], xs)
selectMany' n xs = do
  (y,xs') <- selectOne xs              -- select one element
  (ys, xs'') <- selectMany' (n-1) xs'  -- select all others
  return (y:ys, xs'')                 -- combine results


selectMany :: (RandomGen g, Eq a) => Int -> [a] -> Rand g [a]
selectMany nr = fmap fst . selectMany' nr


-- |Choose a random element from a list.
sampleOne :: RandomGen g => [a] -> Rand g a
sampleOne [] = error "Empty list -- SAMPLEONE"
sampleOne xs = do
  n <- getRandomR (0, length xs - 1)
  return (xs !! n)


-- |Choose @n@ elements with replacement from a list.
sampleWithReplacement      :: RandomGen g => Int -> [a] -> Rand g [a]
sampleWithReplacement 0 xs = return []
sampleWithReplacement n xs = do
  y <- sampleOne xs
  ys <- sampleWithReplacement (n-1) xs
  return (y:ys)


--
-- Util.hs ends here

