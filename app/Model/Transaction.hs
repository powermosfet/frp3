{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Model.Transaction where

import Database.Persist.Sql ((==.), selectList, get, insert, replace, delete, Entity(Entity), entityDef)
import Web.Spock            (json, jsonBody)
import Data.HVect           (HVect, ListContains)
import Data.Time            (getCurrentTime)
import Control.Monad.Trans  (liftIO)

import Auth             (userIdFromSession, checkOwner, checkOwner', checkOwner'', eAction)
import Api.Types        (ApiAction)
import Model            (User, UserId, transactionOwner, TransactionId, transactionBudget, Transaction, transactionTimestamp, transactionCategory, Category, Budget, EntityField(TransactionOwner))
import Utils.Database   (runSQL)
import Utils.Json       (errorJson, ErrorType(ParseError, NotFound), successJson, SuccessType(Created, Changed))
import Utils.Utils      (orLeft, (|>))

transactionListAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
transactionListAction = do
    userId <- userIdFromSession
    categories <- runSQL $ selectList [TransactionOwner ==. userId] []
    json categories

transactionCreateAction :: ListContains n (UserId, User) xs => ApiAction (HVect xs) ()
transactionCreateAction = do
    (userId, eTransaction) <- checkOwner'' jsonBody
    now <- liftIO getCurrentTime
    case eTransaction of
        Left x -> x
        Right transaction -> do
            eBudget   <- checkOwner' userId <$> runSQL (get (transactionBudget transaction))
            eCategory <- checkOwner' userId <$> runSQL (get (transactionCategory transaction))
            eAction $ eBudget >> eCategory >> do
                let modifiedTransaction = transaction { transactionTimestamp = now }
                transactionId <- runSQL $ insert modifiedTransaction
                Right $ successJson $ Created $ Entity transactionId modifiedTransaction

transactionGetAction :: ListContains n (UserId, User) xs => TransactionId -> ApiAction (HVect xs) ()
transactionGetAction transactionId = do
    (_, eTransaction) <- checkOwner'' $ runSQL $ get transactionId
    eAction $ json . Entity transactionId <$> eTransaction
    
transactionChangeAction :: ListContains n (UserId, User) xs => TransactionId -> ApiAction (HVect xs) ()
transactionChangeAction transactionId = do
    maybePutTransaction <- jsonBody
    case maybePutTransaction of
        Just putTransaction -> checkOwner transactionId $ \owner _ -> do
              let newTransaction = putTransaction { transactionOwner = owner }
              runSQL $ replace transactionId newTransaction
              successJson $ Changed $ Entity transactionId newTransaction
        _ -> errorJson $ ParseError (entityDef maybePutTransaction)

transactionDeleteAction :: ListContains n (UserId, User) xs => TransactionId -> ApiAction (HVect xs) ()
transactionDeleteAction transactionId =
    checkOwner transactionId $ \_ _ -> runSQL $ delete transactionId

