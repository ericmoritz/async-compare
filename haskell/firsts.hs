{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Network.HTTP
import Control.Monad
import Text.JSON
import Text.JSON.Types
import Text.JSON.Generic
import Control.Concurrent.Async
import System.Environment
import Data.List (sort)
import Data.Time
import Debug.Trace (trace, traceShow)

data Comment = Comment {
  body :: String,
  created :: Int
} deriving (Show, Eq)

instance Ord Comment where
  compare x y = descending $ compare (created x) (created y)

descending :: Ordering -> Ordering
descending LT = GT
descending GT = LT
descending EQ = EQ

{-==============================================================================
JSON processors
==============================================================================-}
-- Converts the subreddit data into a list of permalinks
parseSubReddit :: String -> Result [String]
parseSubReddit = decode >=> children >=> return . permalinks

-- Converts the post data into the first comment
parseFirst :: String -> Result [Comment]
parseFirst = decode >=> comments >=> return . firstComment

{-==============================================================================
#  Entity accessors
==============================================================================-}
children :: JSObject JSValue -> Result [JSObject JSValue]
children = valFromObj "data" >=> valFromObj "children"

permalinks :: [JSObject JSValue] -> [String]
permalinks = concatMap (resultToList . permalink)

permalink :: JSObject JSValue -> Result String
permalink = valFromObj "data" >=> valFromObj "permalink"

-- Converts a comment into one or zero comments (zero if invalid)
comment :: JSObject JSValue -> [Comment]
comment obj = resultToList $ do
  dat <- valFromObj "data" obj
  body <- valFromObj "body" dat
  created <- valFromObj "created" dat
  return Comment{body=body, created=created}

comments :: [JSObject JSValue] -> Result [Comment]
comments (_:commentListing:_) =
  valFromObj "data" commentListing >>= valFromObj "children" >>= return . concatMap comment

firstComment :: [Comment] -> [Comment]
firstComment = take 1 

{-==============================================================================
# IO 
==============================================================================-}
data Mode = Async | Sync

downloadAll mode subreddits = do
  links <- concat `liftM` mapper downloadAndParseSubReddit subreddits
  comments <- concat `liftM` mapper downloadAndParseFirstComment links
  return $ sort comments
  where
    mapper = case mode of
      Async -> mapConcurrently
      Sync  -> mapM
    -- Note: the concat is here because paresSubReddit return a Result [String]
    --       resultToList turns that into [[String]]
    downloadAndParseSubReddit    = liftM (concat . resultToList . parseSubReddit) . downloadSubReddit
    downloadAndParseFirstComment = liftM (concat . resultToList . parseFirst) . downloadPost

tc f = do
  start <- getCurrentTime
  ret <- f
  end <- getCurrentTime
  return (diffUTCTime end start, ret)

main = print . fst <=< tc $ do
  subreddits <- getArgs
  mode <- getMode `liftM` getProgName 
  permalinks <- downloadAll mode subreddits
  return ()
  where
    getMode "firsts-async" = Async
    getMode "firsts-sync"  = Sync
{-==============================================================================
# Utilities
==============================================================================-}
resultToList (Ok v)    = [v]
resultToList (Error _) = [ ]

redditURL = "http://www.reddit.com"
subredditURL subreddit = redditURL ++ "/r/" ++ subreddit ++ ".json"
postURL permalink = redditURL ++ permalink ++ ".json"


downloadBody = getResponseBody <=< simpleHTTP . getRequest
downloadSubReddit = downloadBody . subredditURL
downloadPost :: String -> IO String
downloadPost = downloadBody . postURL

traceArg msg x =
  trace msg' x
  where
    msg' = msg ++ ": " ++ show x


