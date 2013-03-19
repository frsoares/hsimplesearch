-----------------------------------------------------------------------------
--
-- Module      :  Search.Utils
-- Copyright   :  Francisco Soares
-- License     :  GPL (Just (Version {versionBranch = [2], versionTags = []}))
--
-- Maintainer  :  Francisco Soares
-- Stability   :  experimental
-- Portability :
--
-----------------------------------------------------------------------------

module Search.Utils where

import Data.Char (isAlphaNum)

-- | Function used as filter for breaking a file's content into words.
isDesirableChar :: Char -> Bool
isDesirableChar x = isAlphaNum x  || (x =='-')


breakInto :: [a] -> (a -> Bool) -> (a -> Bool) -> [[a]]
breakInto [] _ _ = []
breakInto list selFunc cleanFunc = filter cleanFunc (Prelude.takeWhile selFunc list) : breakInto tail selFunc cleanFunc where
        tail' = Prelude.dropWhile selFunc list
        tail  = Prelude.dropWhile (not.selFunc) tail'


eqFst :: Eq a => (a, b) -> (a, c) -> Bool
eqFst (a, _) (b, _) = a == b


-- | Decreasingly sorts tuples according to the second element, then the first.
decreasingSort :: (Ord a, Ord o) => (a, o) -> (a, o) -> Ordering
decreasingSort (s1, x1) (s2, x2)
        | x1 > x2   = LT
        | x1 < x2   = GT
        | otherwise =
            if s1 > s2 then GT
            else
                if s1 < s2 then LT
                else EQ

