{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DataKinds                  #-}

module Model.User where

import Crypto.PasswordStore (makePassword, verifyPassword)
import Data.Aeson           (FromJSON(parseJSON), withObject, (.:))
import Database.Persist.Sql (Entity(Entity), (==.), selectFirst, insert)
import Web.Spock            (writeSession)
import Data.HVect           (HVect)
import Data.Text            (Text)
import Control.Monad.Trans  (liftIO)
import qualified Data.ByteString.Char8 as BS

import Api.Types        (ApiAction)
import Model            ( User(User)
                        , EntityField(UserUsername)
                        )
import Model.Session    (createSession)
import Utils.Json       (errorJson, ErrorType(LoginFailed, NoCredentials, ParseError), succesWithId, succesWithMessage)
import Utils.Database   (runSQL)

data Credentials = Credentials
  { newUserName :: Text
  , newUserPassword :: String
  } deriving (Show)

instance FromJSON Credentials where
  parseJSON = withObject "Credentials" $ \ o -> do
    username <- o .: "username"
    password <- o .: "password"
    return $ Credentials username password

type NewUser = Credentials

loginAction :: Maybe Credentials -> ApiAction (HVect '[]) ()
loginAction mCredentials =
  case mCredentials of
    Just (Credentials username password) -> do
      mUser <- runSQL $ selectFirst [UserUsername ==. username] []
      case mUser of
        Just (Entity userId (User _ hash)) -> if verifyPassword (BS.pack password) hash
          then do
            sessId <- runSQL $ createSession userId
            writeSession (Just sessId)
            succesWithMessage "Login successful"
          else errorJson LoginFailed
        Nothing -> errorJson LoginFailed
    Nothing -> errorJson NoCredentials

registerAction :: Maybe NewUser -> ApiAction (HVect '[]) ()
registerAction mUser =
  case mUser of
    Nothing -> errorJson (ParseError "user")
    Just newUser -> do
      theUser <- liftIO $ makeUser newUser 
      newId <- runSQL $ insert theUser
      succesWithId newId

makeUser :: Credentials -> IO User
makeUser (Credentials username password) = do
    hash <- liftIO $ makePassword (BS.pack password) 17
    return (User username hash)

