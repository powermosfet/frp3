module Config where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Database.Persist.Sqlite
import Database.Persist.Postgresql
import Data.String.Conversions
import Control.Monad.Logger (runStdoutLoggingT)

import Config.DbConfig
import Config.DbUrl

data Config = Config 
    { configServerPort :: Int
    , configDbConfig :: DbConfig
    }
    deriving (Show)

fromEnvironment :: [(String, String)] -> Config -> Config
fromEnvironment env (Config defaultPort defaultDb) = 
    let
        port = fromMaybe defaultPort $ fmap read $ lookup "PORT" env

        dbConfig = either (const defaultDb) id
                    $ parseDbUrl
                    $ fromMaybe ""
                    $ lookup "DATABASE_URL" env
    in
        Config 
            { configServerPort = port
            , configDbConfig = dbConfig
            }

makeDbPool :: Config -> IO ConnectionPool
makeDbPool cfg =
    let
        dbConfig = configDbConfig cfg

        pool = case dbConfig of
            SqliteConfig filename -> createSqlitePool (cs filename) 5
            PostgresqlConfig {} -> createPostgresqlPool (toConnectionString dbConfig) 5 
    in
        runStdoutLoggingT pool


toConnectionString :: DbConfig -> ConnectionString
toConnectionString PostgresqlConfig
    { postgresqlUser     = user
    , postgresqlPassword = pass
    , postgresqlHost     = host
    , postgresqlPort     = port
    , postgresqlDatabase = database
    }
    = 
    let
        fields = [ "host="
                 , "port="
                 , "user="
                 , "password="
                 , "dbname="
                 ]
        values = [ host
                 , show port
                 , user
                 , pass
                 , database
                 ]
    in
        BS.pack $ unwords $ zipWith (++) fields values
toConnectionString x = error $ "toConnection string only makes sense for posqgresql! Receved: " ++ show x
    