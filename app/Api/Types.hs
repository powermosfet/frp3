module Api.Types where

import           Web.Spock
import           Database.Persist.Sqlite hiding (get)


type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

