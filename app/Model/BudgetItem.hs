{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}

module Model.BudgetItem where

import Database.Persist.Sql ((==.), selectList, insert, replace, delete, Entity(Entity))
import Web.Spock            (json, jsonBody)
import Data.HVect           (HVect, ListContains)

import Auth             (userIdFromSession, checkOwner)
import Api.Types        (ApiAction)
import Model            (User, UserId, budgetItemOwner, BudgetItemId, budgetItemBudget, EntityField(BudgetItemOwner))
import Utils.Database   (runSQL)
import Utils.Json       (errorJson, ErrorType(ParseError), successJson, SuccessType(Created, Changed))

budgetItemListAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
budgetItemListAction = do
  userId <- userIdFromSession
  categories <- runSQL $ selectList [BudgetItemOwner ==. userId] []
  json categories

budgetItemCreateAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
budgetItemCreateAction = do 
  maybeBudgetItem <- jsonBody
  case maybeBudgetItem of
    Nothing -> errorJson (ParseError "budgetItem")
    Just theBudgetItem -> checkOwner (budgetItemBudget theBudgetItem) $ \owner _ -> do
      let theBudgetItem' = theBudgetItem { budgetItemOwner = owner }
      newId <- runSQL $ insert theBudgetItem'
      successJson $ Created $ Entity newId theBudgetItem'

budgetItemGetAction :: ListContains n (UserId, User) xs => BudgetItemId -> ApiAction (HVect xs) ()
budgetItemGetAction budgetItemId =
  checkOwner budgetItemId $ \_ budgetItem -> json $ Entity budgetItemId budgetItem

budgetItemChangeAction :: ListContains n (UserId, User) xs => BudgetItemId -> ApiAction (HVect xs) ()
budgetItemChangeAction budgetItemId = do
  maybePutBudgetItem <- jsonBody
  case maybePutBudgetItem of
    Just putBudgetItem -> 
      checkOwner budgetItemId $ \owner _ -> 
        checkOwner (budgetItemBudget putBudgetItem) $ \_ _ -> do
          let newBudgetItem = putBudgetItem { budgetItemOwner = owner }
          runSQL $ replace budgetItemId newBudgetItem
          successJson $ Changed $ Entity budgetItemId newBudgetItem
    _ -> errorJson $ ParseError "budgetItem"

budgetItemDeleteAction :: ListContains n (UserId, User) xs => BudgetItemId -> ApiAction (HVect xs) ()
budgetItemDeleteAction budgetItemId =
  checkOwner budgetItemId $ \_ _ -> runSQL $ delete budgetItemId

