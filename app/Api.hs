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

module Api where

import           Web.Spock
import           Data.Aeson       hiding (json)
import           Database.Persist        hiding (get) -- To avoid a naming clash with Web.Spock.get
-- import qualified Database.Persist        as P         -- We'll be using P.get later for GET /people/<id>.

import           DatabaseUtils (runSQL)
import           JsonUtils     (errorJson)
import           Api.Types     (Api, ApiAction)
import           Model         (Cat, EntityField(CatName))

app :: Api
app = do
  get "cats" $ do
    cats <- runSQL $ selectList [] [Asc CatName]
    json cats
  post "cats" $ do
    maybeCat <- jsonBody :: ApiAction (Maybe Cat)
    case maybeCat of
      Nothing -> errorJson 1 "Failed to parse request body as Cat"
      Just theCat -> do
        newId <- runSQL $ insert theCat
        json $ object ["result" .= String "success", "id" .= newId]
