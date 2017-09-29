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

import           Web.Spock
import           Web.Spock.Config
import           Control.Monad.Logger    (runStdoutLoggingT)
import           Database.Persist.Sqlite hiding (get)
import           System.Environment

import Config          (fromEnvironment, Config(..), makeDbPool)
import Config.DbConfig (DbConfig(SqliteConfig))
import Api             (app)
import Model           (migrateAll)

main :: IO ()
main = do
  env <- getEnvironment 
  let config = fromEnvironment env (Config 8080 (SqliteConfig "api.db"))
  print config
  pool <- makeDbPool config 
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  runSpock (configServerPort config) (spock spockCfg app)
