{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (ExceptT (ExceptT), MonadError, MonadIO, runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT, runReaderT), reader)
import CustomMonadSpec.API
import CustomMonadSpec.Types
import Data.Text (Text)
import Lib
import Servant
import Servant.API.ContentTypes (NoContent (..))

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
backend :: CustomMonadSpecBackend AppHandler
backend =
  CustomMonadSpecBackend
    { healthGet = return NoContent,
      usersGet = return [],
      usersIdGet = usersIdGetH
    }

usersIdGetH :: Int -> AppHandler User
usersIdGetH id = do
  name <- reader entry
  return User {userName = name, userRole = "admin"}

main :: IO ()
main = do
  let config = Config "http://localhost:8080/"
      env = AppEnv "String in Custom Monad"
   in runCustomMonadSpecMiddlewareServer config requestMiddlewares (appHandlerToHandler env) backend
