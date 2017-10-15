{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}

module Model.Budget where

import Database.Persist.Sql ((==.), selectList, get, insert, replace, delete, SelectOpt(Asc), Entity(Entity), entityDef)
import Web.Spock            (json, jsonBody)
import Data.HVect           (HVect, ListContains)
import Data.Time            (getCurrentTime)
import Control.Monad.Trans  (liftIO)

import Auth             (eAction, userIdFromSession, checkOwner)
import Api.Types        (ApiAction)
import Model            (User, UserId, Budget, budgetOwner, budgetCreated, BudgetId, EntityField(BudgetOwner, BudgetName))
import Utils.Database   (runSQL)
import Utils.Json       (errorJson, ErrorType(ParseError), successJson, SuccessType(Created, Changed))

budgetListAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
budgetListAction = do
  userId <- userIdFromSession
  categories <- runSQL $ selectList [BudgetOwner ==. userId] [Asc BudgetName]
  json categories

budgetCreateAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
budgetCreateAction = do 
    (userId, eBudget) <- checkOwner jsonBody
    case eBudget of
        Left x -> x
        Right theBudget -> do
            now <- liftIO getCurrentTime
            let theBudget' = theBudget { budgetOwner = userId
                                       , budgetCreated = now
                                       }
            newId <- runSQL $ insert theBudget'
            successJson $ Created $ Entity newId theBudget'

budgetGetAction :: ListContains n (UserId, User) xs => BudgetId -> ApiAction (HVect xs) ()
budgetGetAction budgetId = do
    (_, eBudget) <- checkOwner $ runSQL $ get budgetId
    eAction $ json . Entity budgetId <$> eBudget

budgetChangeAction :: ListContains n (UserId, User) xs => BudgetId -> ApiAction (HVect xs) ()
budgetChangeAction budgetId = do
    (userId, eBudget) <- checkOwner jsonBody
    case eBudget of
        Right putBudget -> do
            let newBudget = putBudget { budgetOwner = userId }
            runSQL $ replace budgetId newBudget
            successJson $ Changed $ Entity budgetId newBudget
        _ -> errorJson $ ParseError (entityDef (undefined :: Maybe Budget))

budgetDeleteAction :: ListContains n (UserId, User) xs => BudgetId -> ApiAction (HVect xs) ()
budgetDeleteAction budgetId = do
    (_, eBudget) <- checkOwner $ runSQL $ get budgetId
    case eBudget of
        Left x -> x
        Right _ -> do
            runSQL $ delete budgetId

