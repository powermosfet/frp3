{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}

module Model.Budget where

import Database.Persist.Sql ((==.), selectList, insert, replace, delete, SelectOpt(Asc), Entity(Entity))
import Web.Spock            (json, jsonBody)
import Data.HVect           (HVect, ListContains)
import Data.Time            (getCurrentTime)
import Control.Monad.Trans  (liftIO)

import Auth             (userIdFromSession, checkOwner)
import Api.Types        (ApiAction)
import Model            (User, UserId, budgetOwner, budgetCreated, BudgetId, EntityField(BudgetOwner, BudgetName))
import Utils.Database   (runSQL)
import Utils.Json       (errorJson, ErrorType(ParseError), successJson, SuccessType(Created, Changed))

budgetListAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
budgetListAction = do
  userId <- userIdFromSession
  categories <- runSQL $ selectList [BudgetOwner ==. userId] [Asc BudgetName]
  json categories

budgetCreateAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
budgetCreateAction = do 
  maybeBudget <- jsonBody
  case maybeBudget of
    Nothing -> errorJson (ParseError "budget")
    Just theBudget -> do
      userId <- userIdFromSession
      now <- liftIO getCurrentTime
      let theBudget' = theBudget { budgetOwner = userId
                                 , budgetCreated = now
                                 }
      newId <- runSQL $ insert theBudget'
      successJson $ Created $ Entity newId theBudget'

budgetGetAction :: ListContains n (UserId, User) xs => BudgetId -> ApiAction (HVect xs) ()
budgetGetAction budgetId =
  checkOwner budgetId $ \_ budget -> json $ Entity budgetId budget

budgetChangeAction :: ListContains n (UserId, User) xs => BudgetId -> ApiAction (HVect xs) ()
budgetChangeAction budgetId = do
  maybePutBudget <- jsonBody
  case maybePutBudget of
    Just putBudget -> checkOwner budgetId $ \owner _ -> do
        let newBudget = putBudget { budgetOwner = owner }
        runSQL $ replace budgetId newBudget
        successJson $ Changed $ Entity budgetId newBudget
    _ -> errorJson $ ParseError "budget"

budgetDeleteAction :: ListContains n (UserId, User) xs => BudgetId -> ApiAction (HVect xs) ()
budgetDeleteAction budgetId =
  checkOwner budgetId $ \_ _ -> runSQL $ delete budgetId

