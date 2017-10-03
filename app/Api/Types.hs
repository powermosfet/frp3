module Api.Types where

import Web.Spock                (SpockCtxM, SpockActionCtx)
import Database.Persist.Sqlite  (SqlBackend)
-- import Web.Spock.SessionActions (SessionId)

import Model                    (SessionId)
import Config                   (Config)

type SessionVal = Maybe SessionId

type Api ctx = SpockCtxM ctx SqlBackend SessionVal Config ()

type ApiAction ctx a = SpockActionCtx ctx SqlBackend SessionVal Config a

