{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import BasicSpec.API
import BasicSpec.Types
import Lib
import Servant.API.ContentTypes (NoContent (..))
import Servant.Server (err401, errBody)
import Servant.Server.Experimental.Auth (AuthServerData)

type instance AuthServerData Protected = Account

-- | Set up authentication
auth :: BasicSpecAuth
auth =
  BasicSpecAuth
    { lookupUser = lookupAccountBasic,
      authError = const $ err401 {errBody = "Missing header"}
    }

-- | Add dummy endpoints for spec
backend :: Monad m => BasicSpecBackend a m
backend =
  BasicSpecBackend
    { healthGet = return NoContent,
      usersGet = \account -> return [],
      usersIdGet = \account _ -> return User {userName = "foo", userRole = "admin"}
    }

main :: IO ()
main = do
  let config = Config "http://localhost:8080/"
   in runBasicSpecMiddlewareServer config requestMiddlewares auth backend
