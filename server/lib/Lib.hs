{-# LANGUAGE OverloadedStrings #-}

module Lib (Account, lookupAccount, lookupAccountBasic, requestMiddlewares) where

import Data.ByteString (ByteString)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Data.Text (Text)
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant (throwError)
import Servant.API.BasicAuth (BasicAuthData (..))
import Servant.Server (Handler, err403, errBody)

newtype Account = Account {unAccount :: Text}

-- API key or Bearer:
lookupAccount :: ByteString -> Handler Account
lookupAccount key = case Map.lookup key database of
  Nothing -> throwError (err403 {errBody = "Invalid key"})
  Just usr -> return usr

database :: Map ByteString Account
database =
  fromList
    [ ("key1", Account "Anne Briggs"),
      ("key2", Account "Bruce Cockburn"),
      ("key3", Account "Ghédalia Tazartès")
    ]

-- Basic auth:
lookupAccountBasic :: BasicAuthData -> Handler Account
lookupAccountBasic (BasicAuthData user password) = do
  case Map.lookup (user, password) databaseBasic of
    Nothing -> throwError (err403 {errBody = "Invalid key"})
    Just usr -> return usr

databaseBasic :: Map (ByteString, ByteString) Account
databaseBasic =
  fromList
    [ (("user1", "password1"), Account "Anne Briggs"),
      (("user2", "password2"), Account "Bruce Cockburn"),
      (("user3", "password3"), Account "Ghédalia Tazartès")
    ]

requestMiddlewares :: Middleware
requestMiddlewares = logStdout
