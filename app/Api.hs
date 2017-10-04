{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Api where

import Data.Aeson                    (object, (.=), Value(String))
import Data.HVect                    (HVect(HNil))
import Database.Persist              (insert, selectList, SelectOpt(Asc))
import Network.Wai.Middleware.Static (staticPolicy, addBase, only, (<|>))
import Web.Spock                     (json, get, post, prehook, jsonBody, middleware)

import Api.Types                     (Api, ApiAction)
import Auth                          (authHook)
import Model                         (Cat , EntityField(CatName))
import Model.Category                (categoryListAction, categoryCreateAction)
import Model.User                    (loginAction, registerAction)
import Utils.Database                (runSQL)

baseHook :: ApiAction () (HVect '[])
baseHook = return HNil

app :: Api ()
app =
  prehook baseHook $
  do middleware (staticPolicy (only [("", "static/index.html")] <|> addBase "static"))
     do post "login" $ do
          mCredentials <- jsonBody
          loginAction mCredentials
        post "register" $ do
          mUser <- jsonBody
          registerAction mUser
        prehook authHook $
          do get  "category" categoryListAction
             post "category" categoryCreateAction
             post "cats" $ do
               (maybeCat :: Maybe Cat) <- jsonBody
               case maybeCat of
                 Nothing -> error "Failed to parse request body as Cat"
                 Just theCat -> do
                   newId <- runSQL $ insert theCat
                   json $ object ["result" .= String "success", "id" .= newId]
             get "cats" $ do
               cats <- runSQL $ selectList [] [Asc CatName]
               json cats
