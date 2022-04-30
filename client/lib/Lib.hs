{-# LANGUAGE OverloadedStrings #-}

module Lib (main) where

import ApikeySpec.API
import ApikeySpec.Types
import Control.Monad.Trans.Except (runExceptT)

main :: IO ()
main = do
  let config = Config "http://localhost:8080/"
      client = createApikeySpecClient
      auth = clientAuth "key1"
  res <- runExceptT $ runApikeySpecClient config (usersIdGet client auth 1)
  print res
