{-# LANGUAGE TypeFamilies               #-}

module Utils.Database where

import Control.Monad.Logger         (NoLoggingT, runNoLoggingT)
import Database.Persist.Sql         (SqlBackend, SqlPersistT, runSqlConn)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Web.Spock                    (HasSpock, SpockConn, runQuery)

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSQL action =
  runQuery $ \conn ->
    runResourceT $ runNoLoggingT $ runSqlConn action conn
