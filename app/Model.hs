{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}

module Model where

import Data.Text                       (Text)
import Data.Time.Clock                 (UTCTime)
import qualified Data.ByteString.Char8 as BS
import Database.Persist.Sqlite hiding (get)
import Database.Persist.TH

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
