{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DataKinds                  #-}

module Model.Category where

import Database.Persist.Sql ((==.), selectList, insert, SelectOpt(Asc))
import Web.Spock            (json, jsonBody)
import Data.HVect           (HVect, ListContains)

import Auth             (userIdFromSession)
import Api.Types        (ApiAction)
import Model            (User, UserId, Category, EntityField(CategoryOwner, CategoryName))
import Utils.Database   (runSQL)
import Utils.Json       (errorJson, succesWithId, ErrorType(ParseError))

categoryListAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
categoryListAction = do
  userId <- userIdFromSession
  categories <- runSQL $ selectList [CategoryOwner ==. userId] [Asc CategoryName]
  json categories

categoryCreateAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
categoryCreateAction = do 
  (maybeCategory :: Maybe Category) <- jsonBody
  case maybeCategory of
    Nothing -> errorJson (ParseError "category")
    Just theCategory -> do
      newId <- runSQL $ insert theCategory
      succesWithId newId
