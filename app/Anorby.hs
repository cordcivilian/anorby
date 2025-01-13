{-# LANGUAGE OverloadedStrings #-}

module Anorby where

import qualified Data.Text as T

import qualified Database.SQLite.Simple as SQL

data User = User
  { userId :: Int
  , userName :: T.Text
  , userEmail :: T.Text
  } deriving (Show)

data Aorb = Aorb
  { aorbId :: Int
  , aorbCtx :: T.Text
  , aorbStx :: T.Text
  , aorbA :: T.Text
  , aorbB :: T.Text
  } deriving (Show)

instance SQL.FromRow User where
  fromRow =
    User
    <$> SQL.field
    <*> SQL.field
    <*> SQL.field

instance SQL.FromRow Aorb where
  fromRow =
    Aorb
    <$> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
