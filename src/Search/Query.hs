{-# LANGUAGE DoAndIfThenElse #-}
-----------------------------------------------------------------------------
--
-- Module      :  Search.Query
-- Copyright   :  Francisco Soares
-- License     :  GPL (Just (Version {versionBranch = [2], versionTags = []}))
--
-- Maintainer  :  Francisco Soares
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Search.Query (
   queryAnd
 , queryPhrase
 , queryExcludeWords
) where


import Data.List (sort, sortBy, groupBy, deleteFirstsBy, isSuffixOf, isPrefixOf)
import Data.Char (isAlphaNum, toLower)
import Data.Maybe (isJust, fromJust, isNothing)

import Search.Utils


-- | Queries a word from an indexed repository
queryWord :: String                            -- ^Word to query
          -> [(String, [(String, [Integer])])] -- ^Indexed files to search in.
          -> [(String, Int)]                   -- ^The result [(file, #occurrences of the word)]
queryWord word matches =
    let
        -- words with a hyphen are connected (e.g., camel-case == camelcase)
        validateWord = filter (/= '-') word

        -- recognizes if the word has a wildcard.
        (wildcardWord, eqFunction) 
          | head validateWord == '*' = (tail validateWord, Just isSuffixOf)
          | last validateWord == '*' = (init validateWord, Just isPrefixOf)
          | otherwise                = (validateWord, Nothing)


        hasWildcard = isJust eqFunction

        wordList :: [(String, [(String, [Integer])])] -- single occurrence of match
        wordList = filter (\x -> validateWord == fst x) matches

        filePositions :: [(String, [Integer])]
        filePositions =
            if not $ null wordList then snd $ head wordList 
            else []

        positionCount :: (String, [Integer]) -> (String, Int)
        positionCount (w, pos) = (w, length pos)
    in
        if not hasWildcard then
            map positionCount filePositions
        else
            queryWildcard wildcardWord (fromJust eqFunction) matches

-- | Query for a word with a wildcard. Results in querying all possible words derived from the wildcard.
queryWildcard :: String -> (String -> String -> Bool) -> [(String, [(String, [Integer])])] -> [(String, Int)]
queryWildcard wildcardWord comparisonFunction matches = 
    let
        shouldWork =  '*' `notElem` wildcardWord

        wordList :: [(String, [(String, [Integer])])]
        wordList = filter (\x -> wildcardWord `comparisonFunction` fst x) matches

        words = map fst wordList

        unifiedResults = preservingUnion $ map (`queryWord` matches) words
    in
        if shouldWork then
            map (foldr1 sumCounts) unifiedResults

        else error "Queried word attempts to use more than one wildcard, or wildcard in the middle of words."


-- | Query files for multiple words' presence. If a file doesn't have all words, it won't be included.
queryAnd :: [String] -> [(String, [(String, [Integer])])] -> [(String, Int)]
queryAnd words matches = 
    let
        resultsPerWord = map (`queryWord` matches) words
        
        intersectedLists = preservingIntersect resultsPerWord

    in
        map (foldr1 sumCounts) intersectedLists


-- | Helper function for queryExcludeWords. This is the function which does the actual exclusion of results.
queryExcludeWord :: String -> [(String, [(String, [Integer])])] -> [(String, Int)] -> [(String, Int)]
queryExcludeWord wordToExclude matches previousResult = 
    let
        exclusionResults = queryWord wordToExclude matches
        
    in
        deleteFirstsBy eqFst previousResult exclusionResults


-- | Queries for files which match an exclusion criteria, and fixes a previous result to exclude those files.
queryExcludeWords :: [String] -> [(String, [(String, [Integer])])] -> [(String, Int)] -> [(String, Int)]
queryExcludeWords [] _ results = results
queryExcludeWords (h:t) matches previousResult = 
    queryExcludeWords t matches results where
        results = queryExcludeWord h matches previousResult


-- | Queries a phrase -- a sequence of words, as might have been indexed. Does not match actual phrase 
-- structure of the original file, since all non-alphanumeric characters are lost. Only alphanumeric 
-- sequences are compared.
queryPhrase :: String -> [(String, [(String, [Integer])])] -> [(String, Int)]
queryPhrase phrase matches =
    let 
        -- these are the words we want
        brokenPhrase = breakInto phrase isDesirableChar (/='-')

        wordList :: [(String, [(String, [Integer])])] -- matches all the words
        wordList = filter (\x -> fst x `elem` brokenPhrase) matches

        orderedWordList = [ (x ,snd $ head $ filter (\z -> fst z == x) wordList ) | x <- brokenPhrase ]

        andQuery = queryAnd brokenPhrase matches

        andFiles  = map fst andQuery

        strippedWords = map (filter (\y -> fst y `elem` andFiles) . snd) orderedWordList

        filesAndAllPos = [( filename , map snd $ filter (\x -> fst x == filename) $ concat strippedWords ) | filename <- andFiles ]
    in
        filter ((/=0) . snd) $ map (\x -> (fst x, findSequences $ snd x)) filesAndAllPos

-- | Counts the amount of sequences found in the list of lists.
-- e.g., [[1,6,9],[2,7,10],[3,8,12]] would only count 2 sequences (1-2-3 and 6-7-8)
-- Not considering empty lists. Those should never be passed as arguments.
findSequences :: (Eq a, Num a) => [[a]] -> Int
findSequences (h:t) = length [ x | x <- h , inLists (x+1) t ] where
    inLists :: (Eq a, Num a) => a -> [[a]] -> Bool
    inLists _ [] = True
    inLists x (h:t)
            | x `elem` h = inLists (x+1) t
            | otherwise  = False


-- = Utility functions =

-- | Intersects lists, but keeps all elements.
preservingIntersect :: (Ord b, Ord a) => [[(a, b)]] -> [[(a, b)]]
preservingIntersect lists = filter (\x -> length x == length lists) $ groupBy eqFst $ sort $ concat lists

-- | Unites lists, also keeping every element.
preservingUnion :: (Ord b, Ord a) => [[(a, b)]] -> [[(a, b)]]
preservingUnion lists = groupBy eqFst $ sort $ concat lists


sumCounts :: (String, Int) -> (String, Int) -> (String, Int)
sumCounts (x, a) (y, b) = (x, a+b)



