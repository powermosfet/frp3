{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}

module Auth where

import Web.Spock            (readSession, getState, getContext)
import Database.Persist.Sql (get, SqlPersistM, PersistEntity, PersistEntityBackend, SqlBackend)
import Data.HVect           (HVect((:&:)), findFirst, ListContains)
import Data.Time            (NominalDiffTime, getCurrentTime)
import Control.Monad.Trans  (liftIO)

import Api.Types      (ApiAction)
import Config         (configSessionLifetime)
import Model          (User, UserId, SessionId, sessionUserId, HasOwner, getOwner)
import Model.Session  (validateSession)
import Utils.Database (runSQL)
import Utils.Json     (errorJson, ErrorType(Unauthenticated, Forbidden, NotFound))

authHook :: ApiAction (HVect xs) (HVect ((UserId, User) ': xs))
authHook =
    maybeUser $ \mUser ->
      do oldCtx <- getContext
         case mUser of
           Nothing ->
               errorJson Unauthenticated
           Just val ->
               return (val :&: oldCtx)

userIdFromSession :: ListContains n (UserId, User) xs => ApiAction (HVect xs) UserId
userIdFromSession = do
  ctx <- getContext
  return $ fst (findFirst ctx :: (UserId, User))

maybeUser :: (Maybe (UserId, User) -> ApiAction ctx a) -> ApiAction ctx a
maybeUser action =
    do sess <- readSession
       cfg <- getState
       let sessionLifetime = configSessionLifetime cfg
       case sess of
         Nothing ->
             action Nothing
         Just sid ->
             do mUser <- runSQL $ loadUser sid sessionLifetime
                action mUser

loadUser :: SessionId -> NominalDiffTime -> SqlPersistM (Maybe (UserId, User))
loadUser sessId sessionLifetime =
    do mSess <- get sessId
       now <- liftIO getCurrentTime
       case mSess of
         Just sess | validateSession sess sessionLifetime now ->
             do mUser <- get (sessionUserId sess)
                return $ fmap (\user -> (sessionUserId sess, user)) mUser
         _ ->
             return Nothing

data OwnerCheckResult a
  = Ok UserId a
  | Failed

checkOwner' :: (HasOwner a, PersistEntity a, PersistEntityBackend a ~ SqlBackend) =>
                UserId -> Maybe a -> Either (ApiAction (HVect xs) ()) a
checkOwner' owner (Just object) 
    | getOwner object == owner = Right object
    | otherwise = Left $ errorJson Forbidden
checkOwner' _ _ = Left $ errorJson NotFound

checkOwner :: (HasOwner a, PersistEntity a, PersistEntityBackend a ~ SqlBackend, ListContains n (UserId, User) xs) =>
    ApiAction (HVect xs) (Maybe a) -> ApiAction (HVect xs) (UserId, Either (ApiAction (HVect xs) ()) a)
checkOwner getAction = do
    owner <- userIdFromSession
    maybeObject <- getAction
    return $ (,) owner $ case maybeObject of
        Just object | getOwner object == owner -> Right object
        Nothing -> Left $ errorJson NotFound
        _ -> Left $ errorJson Forbidden

eAction :: Either a a -> a
eAction (Left  action) = action
eAction (Right action) = action

