{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import NoauthSpec.API
import NoauthSpec.Types
import Servant.API.ContentTypes (NoContent (..))

-- | Add dummy endpoints for spec
backend :: Monad m => NoauthSpecBackend m
backend =
  NoauthSpecBackend
    { healthGet = return NoContent,
      usersGet = return [],
      usersIdGet = \_ -> return User {userName = "foo", userRole = "admin"}
    }

main :: IO ()
main = do
  let config = Config "http://localhost:8080/"
   in runNoauthSpecMiddlewareServer config requestMiddlewares backend
