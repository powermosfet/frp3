{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}

module Model.Transaction where

import Database.Persist.Sql ((==.), selectList, insert, replace, delete, SelectOpt(Asc), Entity(Entity))
import Web.Spock            (json, jsonBody)
import Data.HVect           (HVect, ListContains)

import Auth             (userIdFromSession, checkOwner)
import Api.Types        (ApiAction)
import Model            (User, UserId, transactionOwner, TransactionId, EntityField(TransactionOwner, TransactionName))
import Utils.Database   (runSQL)
import Utils.Json       (errorJson, ErrorType(ParseError), successJson, SuccessType(Created, Changed))

transactionListAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
transactionListAction = do
  userId <- userIdFromSession
  categories <- runSQL $ selectList [TransactionOwner ==. userId] [Asc TransactionName]
  json categories

transactionCreateAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
transactionCreateAction = do 
  maybeTransaction <- jsonBody
  case maybeTransaction of
    Nothing -> errorJson (ParseError "transaction")
    Just theTransaction -> do
      userId <- userIdFromSession
      let theTransaction' = theTransaction { transactionOwner = userId }
      newId <- runSQL $ insert theTransaction'
      successJson $ Created $ Entity newId theTransaction'

transactionGetAction :: ListContains n (UserId, User) xs => TransactionId -> ApiAction (HVect xs) ()
transactionGetAction transactionId =
  checkOwner transactionId $ \_ transaction -> json $ Entity transactionId transaction

transactionChangeAction :: ListContains n (UserId, User) xs => TransactionId -> ApiAction (HVect xs) ()
transactionChangeAction transactionId = do
  maybePutTransaction <- jsonBody
  case maybePutTransaction of
    Just putTransaction -> checkOwner transactionId $ \owner _ -> do
        let newTransaction = putTransaction { transactionOwner = owner }
        runSQL $ replace transactionId newTransaction
        successJson $ Changed $ Entity transactionId newTransaction
    _ -> errorJson $ ParseError "transaction"

transactionDeleteAction :: ListContains n (UserId, User) xs => TransactionId -> ApiAction (HVect xs) ()
transactionDeleteAction transactionId =
  checkOwner transactionId $ \_ _ -> runSQL $ delete transactionId

