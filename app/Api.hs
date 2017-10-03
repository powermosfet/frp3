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

import Data.Aeson                    (object, (.=), Value(String))
import Data.HVect                    (HVect(HNil))
import Database.Persist              (insert, selectList, SelectOpt(Asc))
import Network.Wai.Middleware.Static (staticPolicy, addBase, only, (<|>))
import Web.Spock                     (json, get, post, readSession, prehook, jsonBody, middleware)

import Api.Types                     (Api, ApiAction)
import Auth                          (authHook)
import Model                         (Cat , EntityField(CatName))
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
          do post "cats" $ do
               (maybeCat :: Maybe Cat) <- jsonBody
               case maybeCat of
                 Nothing -> error "Failed to parse request body as Cat"
                 Just theCat -> do
                   newId <- runSQL $ insert theCat
                   json $ object ["result" .= String "success", "id" .= newId]
             get "cats" $ do
               cats <- runSQL $ selectList [] [Asc CatName]
               json cats
             get "cookie" $ do
                 session <- readSession
                 json $ object ["result" .= String "success", "id" .= (show session)]
