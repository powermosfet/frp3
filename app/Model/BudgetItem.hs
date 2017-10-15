{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Model.BudgetItem where

import Database.Persist.Sql ((==.), selectList, get, insert, replace, delete, Entity(Entity), entityDef)
import Web.Spock            (json, jsonBody)
import Data.HVect           (HVect, ListContains)

import Auth             (userIdFromSession, checkOwner, checkOwner', checkOwner, eAction)
import Api.Types        (ApiAction)
import Model            (User, UserId, budgetItemOwner, BudgetItemId, budgetItemBudget, EntityField(BudgetItemOwner))
import Utils.Database   (runSQL)
import Utils.Json       (errorJson, ErrorType(ParseError, Forbidden), successJson, SuccessType(Created, Changed))

budgetItemListAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
budgetItemListAction = do
    userId <- userIdFromSession
    categories <- runSQL $ selectList [BudgetItemOwner ==. userId] []
    json categories

budgetItemCreateAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
budgetItemCreateAction = do
    (userId, eBudgetItem) <- checkOwner jsonBody
    case eBudgetItem of
        Left x -> x
        Right budgetItem -> do
            eBudget   <- checkOwner' userId <$> runSQL (get (budgetItemBudget budgetItem))
            case eBudget of
              Right _ -> do
                budgetItemId <- runSQL $ insert budgetItem
                successJson $ Created $ Entity budgetItemId budgetItem
              _ -> errorJson Forbidden

budgetItemGetAction :: ListContains n (UserId, User) xs => BudgetItemId -> ApiAction (HVect xs) ()
budgetItemGetAction budgetItemId = do
    (_, eBudgetItem) <- checkOwner $ runSQL $ get budgetItemId
    eAction $ json . Entity budgetItemId <$> eBudgetItem
    
budgetItemChangeAction :: ListContains n (UserId, User) xs => BudgetItemId -> ApiAction (HVect xs) ()
budgetItemChangeAction budgetItemId = do
    maybePutBudgetItem <- jsonBody
    case maybePutBudgetItem of
        Just putBudgetItem -> do
            (userId, eBudgetItem) <- checkOwner $ runSQL $ get budgetItemId 
            case eBudgetItem of 
                Left x -> x
                Right budgetItem -> do
                    eBudget   <- checkOwner' userId <$> runSQL (get (budgetItemBudget budgetItem))
                    case eBudget of
                        Right _ -> do
                            let newBudgetItem = putBudgetItem { budgetItemOwner = userId }
                            runSQL $ replace budgetItemId newBudgetItem
                            successJson $ Changed $ Entity budgetItemId newBudgetItem
                        _ -> errorJson Forbidden
        _ -> errorJson $ ParseError (entityDef maybePutBudgetItem)

budgetItemDeleteAction :: ListContains n (UserId, User) xs => BudgetItemId -> ApiAction (HVect xs) ()
budgetItemDeleteAction budgetItemId = do
    (_, eBudgetItem) <- checkOwner $ runSQL $ get budgetItemId 
    case eBudgetItem of
        Left x -> x
        Right _ -> do
            runSQL $ delete budgetItemId

