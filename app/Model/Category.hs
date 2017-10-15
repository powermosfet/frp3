{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}

module Model.Category where

import Database.Persist.Sql ((==.), selectList, get, insert, replace, delete, SelectOpt(Asc), Entity(Entity), entityDef)
import Web.Spock            (json, jsonBody)
import Data.HVect           (HVect, ListContains)

import Auth             (eAction, userIdFromSession, checkOwner)
import Api.Types        (ApiAction)
import Model            (User, UserId, Category, categoryOwner, CategoryId, EntityField(CategoryOwner, CategoryName))
import Utils.Database   (runSQL)
import Utils.Json       (errorJson, ErrorType(ParseError), successJson, SuccessType(Created, Changed))

categoryListAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
categoryListAction = do
  userId <- userIdFromSession
  categories <- runSQL $ selectList [CategoryOwner ==. userId] [Asc CategoryName]
  json categories

categoryCreateAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
categoryCreateAction = do 
    (userId, eCategory) <- checkOwner jsonBody
    case eCategory of
        Left x -> x
        Right theCategory -> do
            let theCategory' = theCategory { categoryOwner = userId }
            newId <- runSQL $ insert theCategory'
            successJson $ Created $ Entity newId theCategory'

categoryGetAction :: ListContains n (UserId, User) xs => CategoryId -> ApiAction (HVect xs) ()
categoryGetAction categoryId = do
    (_, eCategory) <- checkOwner $ runSQL $ get categoryId
    eAction $ json . Entity categoryId <$> eCategory

categoryChangeAction :: ListContains n (UserId, User) xs => CategoryId -> ApiAction (HVect xs) ()
categoryChangeAction categoryId = do
    (_, eCategory) <- checkOwner jsonBody
    case eCategory of
        Right putCategory -> do
            runSQL $ replace categoryId putCategory
            successJson $ Changed $ Entity categoryId putCategory
        _ -> errorJson $ ParseError (entityDef (undefined :: Maybe Category))

categoryDeleteAction :: ListContains n (UserId, User) xs => CategoryId -> ApiAction (HVect xs) ()
categoryDeleteAction categoryId = do
    (_, eCategory) <- checkOwner $ runSQL $ get categoryId
    case eCategory of
        Left x -> x
        Right _ -> do
            runSQL $ delete categoryId

