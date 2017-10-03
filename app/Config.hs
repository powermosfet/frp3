module Config where

import Control.Applicative         ((<$>))
import Control.Monad.Logger        (runStdoutLoggingT)
import Data.Maybe                  (fromMaybe)
import Data.String.Conversions     (cs)
import Data.Time                   (NominalDiffTime)
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool)
import Database.Persist.Sqlite     (ConnectionPool, createSqlitePool)
import qualified Data.ByteString.Char8 as BS

import Config.DbConfig             (DbConfig(PostgresqlConfig, SqliteConfig))
import Config.DbUrl                (parseDbUrl)

data Config = Config 
    { configServerPort :: Int
    , configDbConfig :: DbConfig
    , configSessionLifetime :: NominalDiffTime
    , configPasswordStrength :: Int
    }
    deriving (Show)

fromEnvironment :: Config -> [(String, String)] -> Config
fromEnvironment dflt env = 
    let
        readWithDefault field name fn = fromMaybe (field dflt) $ fn <$> lookup name env
        port = readWithDefault configServerPort "PORT" read
        life = readWithDefault configSessionLifetime "SESSION_LIFETIME" (fromRational . read)
        strength = readWithDefault configPasswordStrength "PASSWORD_STRENGTH" read
        dbConfig = either (const (configDbConfig dflt)) id
                    $ parseDbUrl
                    $ fromMaybe ""
                    $ lookup "DATABASE_URL" env
    in
        Config 
            { configServerPort = port
            , configDbConfig = dbConfig
            , configSessionLifetime = life
            , configPasswordStrength = strength
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
toConnectionString (PostgresqlConfig user pass host port database) =
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
    
