{-# LANGUAGE DoAndIfThenElse #-}
-----------------------------------------------------------------------------
--
-- Module      :  Search.Interpreter
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

module Search.Interpreter (
   interpretQuery
 , doInterpret
) where


import Data.List (sort, sortBy, groupBy, deleteFirstsBy, isSuffixOf, isPrefixOf)
import Data.Char (isAlphaNum, toLower)
import Data.Maybe (isJust, fromJust, isNothing)
import Search.Query

import Search.Utils
--import Debug.Trace (trace)


doInterpret query indexes = return $! interpretQuery query indexes

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

