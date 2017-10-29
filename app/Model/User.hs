{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DataKinds                  #-}

module Model.User where

import Crypto.PasswordStore (makePassword, verifyPassword)
import Data.Aeson           (FromJSON(parseJSON), withObject, (.:))
import Database.Persist.Sql (Entity(Entity), (==.), selectFirst, insert)
import Web.Spock            (jsonBody, writeSession)
import Data.HVect           (HVect, ListContains)
import Data.Text            (Text)
import Control.Monad.Trans  (liftIO)
import qualified Data.ByteString.Char8 as BS

import Auth             (maybeUser)
import Api.Types        (ApiAction)
import Model            ( User(User)
                        , UserId
                        , userPassword
                        , EntityField(UserUsername)
                        )
import Model.Session    (createSession)
import Utils.Json       (errorJson, ErrorType(LoginFailed, NoCredentials, Unauthenticated), successJson, SuccessType(Created, Found))
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

profileGetAction :: ApiAction (HVect '[]) ()
profileGetAction = do
  maybeUser $ \mUser -> case mUser of
    Just (userId, user) -> do
            successJson Found $ Entity userId user
    Nothing -> errorJson Unauthenticated

loginAction :: ApiAction (HVect '[]) ()
loginAction = do
  mCredentials <- jsonBody
  case mCredentials of
    Just (Credentials username password) -> do
      mUser <- runSQL $ selectFirst [UserUsername ==. username] []
      case mUser of
        Just (Entity userId user) -> if verifyPassword (BS.pack password) (userPassword user)
          then do
            sessId <- runSQL $ createSession userId
            writeSession (Just sessId)
            successJson Found $ Entity userId user
          else errorJson LoginFailed
        Nothing -> errorJson LoginFailed
    Nothing -> errorJson NoCredentials

registerAction :: ApiAction (HVect '[]) ()
registerAction = do
  mUser <- jsonBody
  case mUser of
    Nothing -> errorJson NoCredentials
    Just newUser -> do
      theUser <- liftIO $ makeUser newUser 
      newId <- runSQL $ insert theUser
      successJson Created $ Entity newId theUser

makeUser :: Credentials -> IO User
makeUser (Credentials username password) = do
    hash <- liftIO $ makePassword (BS.pack password) 17
    return (User username "" hash)

