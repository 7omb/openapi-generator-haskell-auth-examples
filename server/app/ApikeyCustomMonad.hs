{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import ApikeyCustomMonadSpec.API
import ApikeyCustomMonadSpec.Types
import Control.Monad.Except (ExceptT (ExceptT), MonadError, MonadIO, runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT, runReaderT), reader)
import Data.Text (Text)
import Lib
import Servant
import Servant.API.ContentTypes (NoContent (..))
import Servant.Server.Experimental.Auth (AuthServerData)

type instance AuthServerData Protected = Account

-- | Set up authentication
auth :: ApikeyCustomMonadSpecAuth
auth =
  ApikeyCustomMonadSpecAuth
    { lookupUser = lookupAccount,
      authError = const $ err401 {errBody = "Missing header"}
    }

type AppServer a = ServerT a AppHandler

newtype AppEnv = AppEnv {entry :: Text}

newtype AppHandler a = AppHandler {runAppHandler :: ExceptT ServerError (ReaderT AppEnv IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError ServerError,
      MonadIO,
      MonadReader AppEnv
    )

appHandlerToHandler :: AppEnv -> AppHandler a -> Handler a
appHandlerToHandler env = Handler . ExceptT . flip runReaderT env . runExceptT . runAppHandler

-- | Add dummy endpoints for spec
backend :: ApikeyCustomMonadSpecBackend Account AppHandler
backend =
  ApikeyCustomMonadSpecBackend
    { healthGet = return NoContent,
      usersGet = \account -> return [],
      usersIdGet = usersIdGetH
    }

usersIdGetH :: Account -> Int -> AppHandler User
usersIdGetH _ id = do
  name <- reader entry
  return User {userName = name, userRole = "admin"}

main :: IO ()
main = do
  let config = Config "http://localhost:8080/"
      env = AppEnv "String in Custom Monad"
   in runApikeyCustomMonadSpecMiddlewareServer config requestMiddlewares (appHandlerToHandler env) auth backend
