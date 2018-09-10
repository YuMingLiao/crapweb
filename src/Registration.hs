
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Registration where
import Prelude hiding (lookup)
import Control.Applicative
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Acid
import Data.Function
import Data.IntMap (IntMap)
import Data.List hiding (lookup)
import Data.Ord
import Data.SafeCopy hiding (Version)
import Data.Typeable
import Data.Time
import Data.Text
import qualified Data.IntMap as IntMap
import Control.Lens

--data
data User = User
    { _userName :: Text
    , _userMail :: Text
    , _registrationTime :: Maybe UTCTime
    } deriving (Show, Typeable)
makeFieldsNoPrefix ''User 

def = User "Dummy" "dummy@gmail.com" Nothing

--database
data Db = Db { allUsers :: IntMap User }
  deriving (Typeable)

--view
usersOverRegistrationTime :: Query Db [User]
usersOverRegistrationTime =
  sortBy (comparing _registrationTime) . IntMap.elems . allUsers <$> ask

getAllUsers :: Query Db (IntMap User)
getAllUsers = allUsers <$> ask

lookup :: Int -> Query Db (Maybe User)
lookup identifier = IntMap.lookup identifier . allUsers <$> ask


--update
addUser :: User -> Update Db ()
addUser user = modify go
 where
  go (Db db) = Db $
    case IntMap.maxViewWithKey db of
      Just ((max, _), _) -> IntMap.insert (max + 1) user db
      Nothing            -> IntMap.singleton 1 user

deleteAllUsers :: Update Db ()
deleteAllUsers = modify go
 where
  go (Db db) = Db (IntMap.empty)

replaceDb :: Db -> Update Db ()
replaceDb newdb = modify go
  where
    go (Db _) = newdb

--data
--
$(deriveSafeCopy 0 'base ''User)
--database
$(deriveSafeCopy 0 'base ''Db)
--view & update
makeAcidic ''Db ['getAllUsers, 'usersOverRegistrationTime, 'addUser, 'deleteAllUsers, 'lookup]

addDummy :: IO ()
addDummy = do
  state <- openLocalState (Db IntMap.empty)

  -- Record a new user
  now <- getCurrentTime
  update state (AddUser $ set registrationTime (Just now) def)

  -- Query for all users
  allUsers <- query state UsersOverRegistrationTime
  closeAcidState state 
  mapM_ print allUsers
--  update state DeleteAllUsers

--ghc-pkg unregister --force mtl-2.2.*
--when there is mtl conflicts.

lookupUser :: Int -> IO (Maybe User)
lookupUser identifier = do
    state <- openLocalState (Db IntMap.empty)
    user <- query state (Lookup identifier)
    closeAcidState state 
    return user

addUser' :: User -> IO () 
addUser' user = do
    state <- openLocalState (Db IntMap.empty)
    user <- update state (AddUser user)
    closeAcidState state 
    return ()

deleteAllUsers' :: IO () 
deleteAllUsers' = do
    state <- openLocalState (Db IntMap.empty)
    user <- update state DeleteAllUsers
    closeAcidState state 
    return ()



allUsers' :: IO [User]
allUsers' = do
  state <- openLocalState (Db IntMap.empty)
  allUsers <- query state UsersOverRegistrationTime
  closeAcidState state 
  return allUsers
