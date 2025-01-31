{-# LANGUAGE OverloadedStrings #-}

module Core.RollingShadow where

import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.Text as T
import qualified System.Random as Random
import qualified Database.SQLite.Simple as SQL

import Types
import Core.Matching

shadowUserId :: UserID
shadowUserId = -1

shadowUserName :: T.Text
shadowUserName = "void"

shadowUserEmail :: T.Text
shadowUserEmail = "void@anorby.com"

trackUnmatchedUser :: SQL.Connection -> UserID -> IO ()
trackUnmatchedUser conn uid = do
  now <- POSIXTime.getPOSIXTime
  SQL.execute conn
    "INSERT OR REPLACE INTO unmatched_users VALUES (?, ?)"
    (uid, floor now :: Integer)

clearUnmatchedUser :: SQL.Connection -> UserID -> IO ()
clearUnmatchedUser conn uid =
  SQL.execute conn
    "DELETE FROM unmatched_users WHERE user_id = ?"
    (SQL.Only uid)

getUnmatchedUsersPriority :: SQL.Connection -> IO [UserID]
getUnmatchedUsersPriority conn = do
  results <- SQL.query_ conn $ SQL.Query $ T.unwords
    [ "SELECT user_id FROM unmatched_users"
    , "ORDER BY unmatched_since ASC"
    ]
  return $ map SQL.fromOnly results

createShadowUser :: SQL.Connection -> IO UserSubmission
createShadowUser conn = do
  aorbIds <- SQL.query_ conn
    "SELECT id FROM aorb ORDER BY id" :: IO [SQL.Only AorbID]
  gen <- Random.newStdGen
  let answers = take (length aorbIds) $ Random.randoms gen
      (mainIdx, _) = Random.randomR (0, length aorbIds - 1) gen
      mainAorbId = maybe 1 SQL.fromOnly $ atMay aorbIds mainIdx
  return (answers, mainAorbId, Swing)
  where
    atMay xs n = if n < length xs then Just (xs !! n) else Nothing

addShadowUserIfNeeded :: SQL.Connection -> Rankings -> IO Rankings
addShadowUserIfNeeded conn rankings = do
  if odd $ Map.size rankings
    then do
      unmatched <- getUnmatchedUsersPriority conn
      shadowRanking <- generateShadowRanking rankings unmatched
      return $ Map.insert shadowUserId shadowRanking rankings
    else return rankings
  where
    generateShadowRanking :: Rankings -> [UserID] -> IO [UserID]
    generateShadowRanking rnks unmatchedUsers = do
      gen <- Random.newStdGen
      let allUsers = Map.keys rnks
          remainingUsers = allUsers List.\\ unmatchedUsers
          (shuffled, _) = fisherYatesShuffle gen remainingUsers
      return $ shuffled ++ unmatchedUsers

updateUnmatchedStatus :: SQL.Connection -> Marriages -> IO ()
updateUnmatchedStatus conn marriages = do
  let realMatches = Map.toList $
        Map.filter (/= Just shadowUserId) marriages
  Monad.forM_ realMatches $ \(uid, _) ->
    clearUnmatchedUser conn uid
  let unmatched = Map.keys $
        Map.filter (== Just shadowUserId) marriages
  Monad.forM_ unmatched $ \uid ->
    trackUnmatchedUser conn uid

ensureShadowUser :: SQL.Connection -> IO ()
ensureShadowUser conn = do
  existing <- SQL.query conn
    "SELECT 1 FROM users WHERE id = ?"
    (SQL.Only shadowUserId) :: IO [SQL.Only UserID]
  Monad.when (null existing) $ do
    let userInsert = ( shadowUserId :: Int
                     , shadowUserName :: T.Text
                     , shadowUserEmail :: T.Text
                     , "void" :: T.Text
                     )
    SQL.execute conn
      "INSERT INTO users (id, name, email, uuid) VALUES (?, ?, ?, ?)"
      userInsert
    (answers, mainAorb, assoc) <- createShadowUser conn
    now <- POSIXTime.getPOSIXTime
    Monad.forM_ (zip [(1::AorbID)..] answers) $ \(aid, ans) ->
      let answerInsert = ( shadowUserId :: UserID
                         , aid :: AorbID
                         , ans :: AorbAnswer
                         , floor now :: Integer
                         )
      in SQL.execute conn
        ( SQL.Query $ T.unwords
          [ "INSERT INTO aorb_answers (user_id, aorb_id, answer, answered_on)"
          , "VALUES (?, ?, ?, ?)"
          ]
        ) answerInsert
    let updateUserInsert = ( mainAorb :: AorbID
                           , assoc :: AssociationScheme
                           , shadowUserId :: UserID
                           )
    SQL.execute conn
      "UPDATE users SET aorb_id = ?, assoc = ? WHERE id = ?"
      updateUserInsert

matchWithShadow :: SQL.Connection -> Rankings -> IO Marriages
matchWithShadow conn rankings = do
  ensureShadowUser conn
  rankingsWithShadow <- addShadowUserIfNeeded conn rankings
  let (group1Rankings, group2Rankings) = splitRankings rankingsWithShadow
  marriages <- galeShapley group1Rankings group2Rankings
  updateUnmatchedStatus conn marriages
  return marriages

isShadowMatch :: Match -> Bool
isShadowMatch match =
  matchUserId match == shadowUserId || matchTargetId match == shadowUserId

getRealUserFromShadowMatch :: Match -> UserID
getRealUserFromShadowMatch match =
  if matchUserId match == shadowUserId
    then matchTargetId match
    else matchUserId match
