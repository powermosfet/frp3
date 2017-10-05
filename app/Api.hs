{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Api where

import Data.HVect                    (HVect(HNil))
import Network.Wai.Middleware.Static (staticPolicy, addBase, only, (<|>))
import Web.Spock                     (get, post, put, var, prehook, middleware, (<//>))

import Api.Types                     (Api, ApiAction)
import Auth                          (authHook)
import Model.Category                (categoryListAction, categoryCreateAction, categoryGetAction, categoryChangeAction)
import Model.User                    (loginAction, registerAction)

baseHook :: ApiAction () (HVect '[])
baseHook = return HNil

app :: Api ()
app = prehook baseHook $
  do middleware (staticPolicy (only [("", "static/index.html")] <|> addBase "static"))
     do post "login" loginAction
        post "register" registerAction 
        prehook authHook $
          do get  "category" categoryListAction
             post "category" categoryCreateAction
             get ("category" <//> var) categoryGetAction
             put ("category" <//> var) categoryChangeAction
