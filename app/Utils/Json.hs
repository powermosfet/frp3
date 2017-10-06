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
  | ParseError String
  | Unauthenticated
  | NotFound
  | Forbidden

errorCode :: ErrorType -> Int
errorCode LoginFailed     = 1
errorCode SessionExpired  = 2
errorCode ObjectNotFound  = 3
errorCode NoCredentials   = 4
errorCode (ParseError _)  = 5
errorCode Unauthenticated = 6
errorCode NotFound        = 7
errorCode Forbidden       = 8

errorMessage :: ErrorType -> String
errorMessage err = case err of
  LoginFailed -> "Username or password is wrong"
  SessionExpired -> "Session expired"
  ObjectNotFound -> "Object not found"
  NoCredentials -> "Could not parse credentials"
  ParseError model -> "Could not parse " ++ model
  Unauthenticated -> "Please log in to access this resource"
  NotFound -> "Could not find resource"
  Forbidden -> "Action is not allowed"

statusCode :: ErrorType -> Http.Status
statusCode Unauthenticated = Http.status401
statusCode NotFound = Http.status404
statusCode Forbidden = Http.status403
statusCode _ = Http.status200

errorJson :: ErrorType -> ApiAction ctx a
errorJson err =
  let
    code = errorCode err
    message = errorMessage err
  in do
    setStatus (statusCode err)
    json $
      object
        [ "result" .= String "failure"
        , "error" .= object ["code" .= code, "message" .= message]
        ]

data SuccessType a
  = Created a

successJson :: (ToJSON a) => SuccessType a -> ApiAction ctx b
successJson (Created ob) = do
  setStatus Http.status201
  json ob
successJson (Changed ob) = json ob

succesWithMessage :: String -> ApiAction ctx a
succesWithMessage msg = 
  json $ object ["result" .= String "success", "message" .= msg]
