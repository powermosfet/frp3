{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import Data.Text            (Text)
import Data.Time.Clock      (UTCTime)
import Database.Persist.TH  (persistLowerCase, share, mkPersist, mkMigrate, sqlSettings)
import qualified Data.ByteString.Char8 as BS

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Session
  created UTCTime
  userId UserId
  deriving Show 
User
  username Text
  password BS.ByteString
  UniqueUsername username
  deriving Show Eq
Cat json
  name Text
  age Int
  deriving Show
|]
