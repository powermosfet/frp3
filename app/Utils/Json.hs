{-# LANGUAGE OverloadedStrings          #-}

module Utils.Json where

import           Web.Spock        (json, setStatus)
import           Data.Text        (Text)
import           Data.Monoid      ((<>))
import           Database.Persist (entityFields, HaskellName(HaskellName), unHaskellName, entityHaskell, fieldHaskell, EntityDef)
import           Data.Aeson       (Value(String), object, (.=), ToJSON)
import qualified Network.HTTP.Types.Status as Http

import           Api.Types        (ApiAction)

data ErrorType
  = LoginFailed
  | SessionExpired
  | ObjectNotFound
  | NoCredentials
  | ParseError EntityDef
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

errorMessage :: ErrorType -> Text
errorMessage err = case err of
  LoginFailed -> "Username or password is wrong"
  SessionExpired -> "Session expired"
  ObjectNotFound -> "Object not found"
  NoCredentials -> "Could not parse credentials"
  ParseError entity -> 
    let
      modelName = (unHaskellName . entityHaskell) entity
    in
      "Could not parse " <> modelName
  Unauthenticated -> "Please log in to access this resource"
  NotFound -> "Could not find resource"
  Forbidden -> "Action is not allowed"

statusCode :: ErrorType -> Http.Status
statusCode Unauthenticated = Http.status401
statusCode LoginFailed = Http.status401
statusCode NotFound = Http.status404
statusCode Forbidden = Http.status403
statusCode _ = Http.status200

metadata :: ErrorType -> Maybe Value
metadata (ParseError entity) =
  let
    fields = entityFields entity
    fieldStrings = map ((\(HaskellName name) -> name) . fieldHaskell) fields
  in
    Just $ object [ "fields" .= fieldStrings ]
metadata _ = Nothing

errorJson :: ErrorType -> ApiAction ctx a
errorJson err =
  let
    code = errorCode err
    message = errorMessage err
    errorObject = object
      ( [ "code" .= code
        , "message" .= message
        ] ++ case metadata err of
              Just theMetadata -> [ "metadata" .= theMetadata ]
              Nothing -> []
      )
  in do
    setStatus (statusCode err)
    json $
      object
        [ "result" .= String "failure"
        , "error" .= errorObject
        ]

data SuccessType
  = Created
  | Changed
  | Found

successJson :: (ToJSON a) => SuccessType -> a -> ApiAction ctx b
successJson successType ob = 
    let
        status = case successType of
            Created -> Http.status201
            _ -> Http.status200
    in do
        setStatus status
        json ob

successWithMessage :: String -> ApiAction ctx a
successWithMessage msg = 
  json $ object ["result" .= String "success", "message" .= msg]
