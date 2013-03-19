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

module Search.Query {-(
   interpretQuery
)-} where


import Data.List (sort, sortBy, groupBy, deleteFirstsBy, isSuffixOf, isPrefixOf)
import Data.Char (isAlphaNum, toLower)
import Data.Maybe (isJust, fromJust, isNothing)

import Search.Utils
--import Debug.Trace (trace)

{-
-- == Query interpretation ==

-- | Receives a query String, interprets it according to the query criteria, and executes it.
--interpretQuery :: String -> [(String, [(String, [Integer])])] -> [(String, Integer)]
interpretQuery originalquery indexes = 
    let
        query = map toLower originalquery
        splitQuery = interpreterMachine query
        qwords = filter isWord splitQuery
        qphrases = filter isPhrase splitQuery
        qexcludes = filter isExclude splitQuery
        shouldRun = (not (null qwords) && null qphrases) || (not (null qphrases) && null qwords && (length qphrases <= 1))
        words = map queryString qwords
        excludes = map queryString qexcludes
        phrases = map queryString qphrases
    in
        sortBy decreasingSort $ queryExcludeWords excludes indexes $ 
            if shouldRun then 
              if not $ null words then
                  queryAnd words indexes 
              else
                if not $ null phrases then
                    queryPhrase (head phrases) indexes
                else []
            else 
              if null words && null phrases then
                []
              else error "Both phrase and words search in the same query. Please separate both."


interpreterMachine :: String -> [QueryElem]
interpreterMachine [] = []
interpreterMachine queryText =
    let 
        (result, resultType, rest) = interpreterMachineTuples queryText Nothing
    in
        if isNothing resultType || null result then
            interpreterMachine rest 
        else 
            let
                resultElem = case resultType of
                           (Just Word)    -> QWord result
                           (Just Phrase)  -> QPhrase result
                           (Just Exclude) -> QExclude result
            in
                resultElem : interpreterMachine rest


-- ADT for signaling the type of a Query being processed
data QueryType = Word | Phrase | Exclude deriving (Eq, Show)

-- ADT for passing an argument tagged as a specific type of Query
data QueryElem = QWord String | QPhrase String | QExclude String deriving (Eq, Show)

queryString :: QueryElem -> String
queryString (QWord x) = x
queryString (QPhrase x) = x
queryString (QExclude x) = x

isWord :: QueryElem -> Bool
isWord (QWord _) = True
isWord _ = False

isPhrase (QPhrase _) = True
isPhrase _ = False

isExclude (QExclude _) = True
isExclude _ = False

interpreterMachineTuples :: String -> Maybe QueryType-> (String, Maybe QueryType, String)
interpreterMachineTuples [] (Just Phrase) = error "End of query input before the end of a phrase."
interpreterMachineTuples [] queryType = ([], queryType,[])
interpreterMachineTuples (queryHead:queryTail) (Just Word) = 
    if isAlphaNum queryHead || queryHead `elem` "*-" then
        let (result, resultType, rest) = interpreterMachineTuples queryTail (Just Word)
        in (queryHead:result, resultType, rest)
    else case queryHead of
        '"'       -> error "Attempt to start a phrase in the middle of a word."
        queryHead -> ([], Just Word, queryTail)
interpreterMachineTuples (queryHead:queryTail) (Just Exclude) = 
    if isAlphaNum queryHead then
        let (result, resultType, rest) = interpreterMachineTuples queryTail (Just Exclude)
        in (queryHead:result, resultType, rest)
    else ([], Just Exclude, queryTail) 
interpreterMachineTuples (queryHead:queryTail) (Just Phrase) =
    if queryHead == '"' then
       ([], Just Phrase, queryTail)
    else
        let
            (result, resultType, rest) = interpreterMachineTuples queryTail (Just Phrase)
        in
            (queryHead:result, resultType, rest)
interpreterMachineTuples (queryHead:queryTail) Nothing =
        if isAlphaNum queryHead || queryHead `elem` "\"-*" then             
            let
                (queryType, addHead) = 
                    case queryHead of 
                      '-'       -> (Just Exclude, False)
                      '"'       -> (Just Phrase, False)
                      queryHead -> (Just Word, True)
                (result, resultType, rest) = interpreterMachineTuples queryTail queryType
            in
                if addHead then (queryHead:result, resultType, rest)
                else (result, resultType, rest)
        else
            ([], Nothing, queryTail)

-- == end of query interpretation
-}

-- | Queries a word from an indexed repository
queryWord :: String                            -- ^Word to query
          -> [(String, [(String, [Integer])])] -- ^Indexed files to search in.
          -> [(String, Int)]                   -- ^The result [(file, #occurrences of the word)]
queryWord word matches =
    let
        -- words with a hyphen are connected (e.g., camel-case == camelcase)
        validateWord = filter (/= '-') word

        -- recognizes if the word has a wildcard.
        (wildcardWord, eqFunction) -- =
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


-- | Queries for files which match an exclusion criteria, and fixes a previous result to exclude those files.
queryExcludeWord :: String -> [(String, [(String, [Integer])])] -> [(String, Int)] -> [(String, Int)]
queryExcludeWord wordToExclude matches previousResult = 
    let
        exclusionResults = queryWord wordToExclude matches
        
    in
        deleteFirstsBy eqFst previousResult exclusionResults

-- | Helper function for queryExcludeWord. This is the function which does the actual exclusion of results.
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
findSequences (h:t) = length [ x | x <- h , inLists (x+1) t ]

inLists :: (Eq a, Num a) => a -> [[a]] -> Bool
inLists _ [] = True
inLists x (h:t)
        | x `elem` h = inLists (x+1) t
        | otherwise  = False


-- = Utility functions =

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


-- | Intersects lists, but keeps all elements.
preservingIntersect :: (Ord b, Ord a) => [[(a, b)]] -> [[(a, b)]]
preservingIntersect lists = filter (\x -> length x == length lists) $ groupBy eqFst $ sort $ concat lists

-- | Unites lists, also keeping every element.
preservingUnion :: (Ord b, Ord a) => [[(a, b)]] -> [[(a, b)]]
preservingUnion lists = groupBy eqFst $ sort $ concat lists


sumCounts :: (String, Int) -> (String, Int) -> (String, Int)
sumCounts (x, a) (y, b) = (x, a+b)



