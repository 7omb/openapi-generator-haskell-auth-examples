{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import BearerSpec.API
import BearerSpec.Types
import Lib
import Servant.API.ContentTypes (NoContent (..))
import Servant.Server (err401, errBody)
import Servant.Server.Experimental.Auth (AuthServerData)

type instance AuthServerData Protected = Account

-- | Set up authentication
auth :: BearerSpecAuth
auth =
  BearerSpecAuth
    { lookupUser = lookupAccount,
      authError = const $ err401 {errBody = "Missing header"}
    }

-- | Add dummy endpoints for spec
backend :: Monad m => BearerSpecBackend a m
backend =
  BearerSpecBackend
    { healthGet = return NoContent,
      usersGet = \account -> return [],
      usersIdGet = \account _ -> return User {userName = "foo", userRole = "admin"}
    }

main :: IO ()
main = do
  let config = Config "http://localhost:8080/"
   in runBearerSpecMiddlewareServer config requestMiddlewares auth backend
