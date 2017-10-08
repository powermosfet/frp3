{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Api where

import Data.HVect                    (HVect(HNil))
import Network.Wai.Middleware.Static (staticPolicy, addBase, only, (<|>))
import Web.Spock                     (get, post, put, delete, var, prehook, middleware, (<//>))

import Api.Types                     (Api, ApiAction)
import Auth                          (authHook)
import Model.Category                (categoryListAction, categoryCreateAction, categoryGetAction, categoryChangeAction, categoryDeleteAction)
import Model.Budget                  (budgetListAction, budgetCreateAction, budgetGetAction, budgetChangeAction, budgetDeleteAction)
import Model.BudgetItem              (budgetItemListAction, budgetItemCreateAction, budgetItemGetAction, budgetItemChangeAction, budgetItemDeleteAction)
import Model.Transaction             (transactionListAction, transactionCreateAction, transactionGetAction, transactionChangeAction, transactionDeleteAction)
import Model.User                    (loginAction, registerAction)

baseHook :: ApiAction () (HVect '[])
baseHook = return HNil

app :: Api ()
app = prehook baseHook $
  do middleware (staticPolicy (only [("", "static/index.html")] <|> addBase "static"))
     do post "login" loginAction
        post "register" registerAction 
        prehook authHook $
          do get     "category"              categoryListAction
             post    "category"              categoryCreateAction
             get    ("category" <//> var)    categoryGetAction
             put    ("category" <//> var)    categoryChangeAction
             delete ("category" <//> var)    categoryDeleteAction
             get     "budget"                budgetListAction
             post    "budget"                budgetCreateAction
             get    ("budget" <//> var)      budgetGetAction
             put    ("budget" <//> var)      budgetChangeAction
             delete ("budget" <//> var)      budgetDeleteAction
             get     "budgetitem"            budgetItemListAction
             post    "budgetitem"            budgetItemCreateAction
             get    ("budgetitem" <//> var)  budgetItemGetAction
             put    ("budgetitem" <//> var)  budgetItemChangeAction
             delete ("budgetitem" <//> var)  budgetItemDeleteAction
             get     "transaction"           transactionListAction
             post    "transaction"           transactionCreateAction
             get    ("transaction" <//> var) transactionGetAction
             put    ("transaction" <//> var) transactionChangeAction
             delete ("transaction" <//> var) transactionDeleteAction
