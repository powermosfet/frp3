{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}

module Model.Category where

import Database.Persist.Sql ((==.), selectList, insert, replace, SelectOpt(Asc))
import Web.Spock            (json, jsonBody)
import Data.HVect           (HVect, ListContains)

import Auth             (userIdFromSession, checkOwner)
import Api.Types        (ApiAction)
import Model            (User, UserId, categoryOwner, CategoryId, EntityField(CategoryOwner, CategoryName))
import Utils.Database   (runSQL)
import Utils.Json       (errorJson, succesWithId, ErrorType(ParseError))

categoryListAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
categoryListAction = do
  userId <- userIdFromSession
  categories <- runSQL $ selectList [CategoryOwner ==. userId] [Asc CategoryName]
  json categories

categoryCreateAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
categoryCreateAction = do 
  maybeCategory <- jsonBody
  case maybeCategory of
    Nothing -> errorJson (ParseError "category")
    Just theCategory -> do
      userId <- userIdFromSession
      let theCategory' = theCategory { categoryOwner = userId }
      newId <- runSQL $ insert theCategory'
      succesWithId newId

categoryGetAction :: ListContains n (UserId, User) xs => CategoryId -> ApiAction (HVect xs) ()
categoryGetAction categoryId =
  checkOwner categoryId $ \_ category -> json category

categoryChangeAction :: ListContains n (UserId, User) xs => CategoryId -> ApiAction (HVect xs) ()
categoryChangeAction categoryId = do
  maybePutCategory <- jsonBody
  case maybePutCategory of
    Just putCategory -> checkOwner categoryId $ \owner _ -> do
        let newCategory = putCategory { categoryOwner = owner }
        runSQL $ replace categoryId newCategory
        json newCategory
    _ -> errorJson $ ParseError "category"

