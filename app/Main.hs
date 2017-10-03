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
  let defaults = Config { configServerPort = 8080
                        , configDbConfig = SqliteConfig "api.db"
                        , configSessionLifetime = 5 * 3600
                        , configPasswordStrength = 17
                        }
  env <- getEnvironment 
  let config = fromEnvironment env defaults
  print config
  pool <- makeDbPool config 
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  spockCfg <- defaultSpockCfg Nothing (PCPool pool) config
  runSpock (configServerPort config) (spock spockCfg app)
