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


