{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Api where

import           Web.Spock
import           Network.Wai.Middleware.Static        (staticPolicy, addBase)
import           Data.Aeson                    hiding (json)
import           Database.Persist              hiding (get)

import           DatabaseUtils (runSQL)
import           JsonUtils     (errorJson)
import           Api.Types     (Api, ApiAction)
import           Model         (Cat, EntityField(CatName))

app :: Api
app =
  do middleware (staticPolicy (addBase "static"))
     do get "cats" $ do
          cats <- runSQL $ selectList [] [Asc CatName]
          json cats
        post "cats" $ do
          maybeCat <- jsonBody :: ApiAction (Maybe Cat)
          case maybeCat of
            Nothing -> errorJson 1 "Failed to parse request body as Cat"
            Just theCat -> do
              newId <- runSQL $ insert theCat
              json $ object ["result" .= String "success", "id" .= newId]
