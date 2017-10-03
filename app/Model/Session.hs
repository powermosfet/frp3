module Model.Session where

import Database.Persist.Sql (SqlPersistM, insert)
import Control.Monad.Trans
import Data.Time

import Model (Session(Session), SessionId, sessionCreated, UserId)

createSession :: UserId -> SqlPersistM SessionId
createSession userId =
    do now <- liftIO getCurrentTime
       insert (Session now userId)

validateSession :: Session -> NominalDiffTime -> UTCTime -> Bool
validateSession session sessionLiftetime now = addUTCTime sessionLiftetime (sessionCreated session) > now
