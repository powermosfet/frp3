import Control.Applicative  ((<$>))
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Sql (runSqlPool, runMigration)
import System.Environment   (getEnvironment)
import Web.Spock            (runSpock, spock)
import Web.Spock.Config     (defaultSpockCfg, PoolOrConn(PCPool))

import Api                  (app)
import Config               (fromEnvironment, Config(..), makeDbPool)
import Config.DbConfig      (DbConfig(SqliteConfig))
import Model                (migrateAll)

main :: IO ()
main = do
  let defaults = Config { configServerPort = 8080
                        , configDbConfig = SqliteConfig "api.db"
                        , configSessionLifetime = 5 * 3600
                        , configPasswordStrength = 17
                        }
  config <- fromEnvironment defaults <$> getEnvironment 
  print config
  pool <- makeDbPool config 
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  spockCfg <- defaultSpockCfg Nothing (PCPool pool) config
  runSpock (configServerPort config) (spock spockCfg app)
