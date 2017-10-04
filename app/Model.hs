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
Category json
  name Text
  owner UserId
  deriving Show
Budget json
  name Text
  created UTCTime
  owner UserId
BudgetItem json
  budget BudgetId
  category CategoryId
  amount Double
  frequency Int
  owner UserId
Transaction json
  timestamp UTCTime
  category CategoryId
  amount Double
  owner UserId
Cat json
  name Text
  age Int
  deriving Show
|]
