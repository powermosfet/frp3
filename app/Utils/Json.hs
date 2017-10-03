{-# LANGUAGE DataKinds                  #-}
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

module Utils.Json where

import           Web.Spock        (json, setStatus)
import           Data.Aeson       (Value(String), object, (.=), ToJSON)
import qualified Network.HTTP.Types.Status as Http

import           Api.Types (ApiAction)

data ErrorType 
  = LoginFailed
  | SessionExpired
  | ObjectNotFound
  | NoCredentials
  | ParseErrorUser
  | Unauthenticated
  deriving (Enum)

errorMessage :: ErrorType -> String
errorMessage err = case err of
  LoginFailed -> "Username or password is wrong"
  SessionExpired -> "Session expired"
  ObjectNotFound -> "Object not found"
  NoCredentials -> "Could not parse credentials"
  ParseErrorUser -> "Could not parse user"
  Unauthenticated -> "Please log in to access this resource"

statusCode :: ErrorType -> Http.Status
statusCode Unauthenticated = Http.status401
statusCode _ = Http.status200

errorJson :: ErrorType -> ApiAction ctx a
errorJson err =
  let
    code = fromEnum err
    message = errorMessage err
  in do
    setStatus (statusCode err)
    json $
      object
        [ "result" .= String "failure"
        , "error" .= object ["code" .= code, "message" .= message]
        ]

succesWithId :: (ToJSON a) => a -> ApiAction ctx b
succesWithId theId = 
  json $ object ["result" .= String "success", "id" .= theId]

succesWithMessage :: String -> ApiAction ctx a
succesWithMessage msg = 
  json $ object ["result" .= String "success", "id" .= msg]
