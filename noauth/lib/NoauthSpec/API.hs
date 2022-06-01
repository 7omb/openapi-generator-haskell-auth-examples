{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module NoauthSpec.API
  ( -- * Client and Server
    Config(..)
  , NoauthSpecBackend(..)
  , createNoauthSpecClient
  , runNoauthSpecServer
  , runNoauthSpecMiddlewareServer
  , runNoauthSpecClient
  , runNoauthSpecClientWithManager
  , callNoauthSpec
  , NoauthSpecClient
  , NoauthSpecClientError(..)
  -- ** Servant
  , NoauthSpecAPI
  -- ** Plain WAI Application
  , serverWaiApplicationNoauthSpec
  ) where

import           NoauthSpec.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Proxy                         (Proxy (..))
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
import           Data.UUID                          (UUID)
import           GHC.Exts                           (IsString (..))
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client                (Manager, newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Method          (methodOptions)
import           Network.Wai                        (Middleware)
import qualified Network.Wai.Handler.Warp           as Warp
import           Servant                            (ServerError, serveWithContext)
import           Servant.API                        hiding (addHeader)
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..), Application, Context (EmptyContext))
import           Servant.Server.StaticFiles         (serveDirectoryFileServer)
import           Web.FormUrlEncoded
import           Web.HttpApiData




-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Servant type-level API, generated from the OpenAPI spec for NoauthSpec.
type NoauthSpecAPI
    =    "health" :> Verb 'GET 200 '[JSON] NoContent -- 'healthGet' route
    :<|> "users" :> Verb 'GET 200 '[JSON] [User] -- 'usersGet' route
    :<|> "users" :> Capture "id" Int :> Verb 'GET 200 '[JSON] User -- 'usersIdGet' route
    :<|> Raw


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype NoauthSpecClientError = NoauthSpecClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for NoauthSpec.
-- The backend can be used both for the client and the server. The client generated from the NoauthSpec OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createNoauthSpecClient@). Alternatively, provided
-- a backend, the API can be served using @runNoauthSpecMiddlewareServer@.
data NoauthSpecBackend m = NoauthSpecBackend
  { healthGet :: m NoContent{- ^  -}
  , usersGet :: m [User]{- ^  -}
  , usersIdGet :: Int -> m User{- ^  -}
  }


newtype NoauthSpecClient a = NoauthSpecClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative NoauthSpecClient where
  pure x = NoauthSpecClient (\_ -> pure x)
  (NoauthSpecClient f) <*> (NoauthSpecClient x) =
    NoauthSpecClient (\env -> f env <*> x env)

instance Monad NoauthSpecClient where
  (NoauthSpecClient a) >>= f =
    NoauthSpecClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO NoauthSpecClient where
  liftIO io = NoauthSpecClient (\_ -> liftIO io)

createNoauthSpecClient :: NoauthSpecBackend NoauthSpecClient
createNoauthSpecClient = NoauthSpecBackend{..}
  where
    ((coerce -> healthGet) :<|>
     (coerce -> usersGet) :<|>
     (coerce -> usersIdGet) :<|>
     _) = client (Proxy :: Proxy NoauthSpecAPI)

-- | Run requests in the NoauthSpecClient monad.
runNoauthSpecClient :: Config -> NoauthSpecClient a -> ExceptT ClientError IO a
runNoauthSpecClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runNoauthSpecClientWithManager manager clientConfig cl

-- | Run requests in the NoauthSpecClient monad using a custom manager.
runNoauthSpecClientWithManager :: Manager -> Config -> NoauthSpecClient a -> ExceptT ClientError IO a
runNoauthSpecClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a NoauthSpecClientError
callNoauthSpec
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> NoauthSpecClient a -> m a
callNoauthSpec env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (NoauthSpecClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the NoauthSpec server at the provided host and port.
runNoauthSpecServer
  :: (MonadIO m, MonadThrow m)
  => Config -> NoauthSpecBackend (ExceptT ServerError IO) -> m ()
runNoauthSpecServer config backend = runNoauthSpecMiddlewareServer config requestMiddlewareId backend

-- | Run the NoauthSpec server at the provided host and port.
runNoauthSpecMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> NoauthSpecBackend (ExceptT ServerError IO) -> m ()
runNoauthSpecMiddlewareServer Config{..} middleware backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationNoauthSpec backend

-- | Plain "Network.Wai" Application for the NoauthSpec server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationNoauthSpec :: NoauthSpecBackend (ExceptT ServerError IO) -> Application
serverWaiApplicationNoauthSpec backend = serveWithContext (Proxy :: Proxy NoauthSpecAPI) context (serverFromBackend backend)
  where
    context = serverContext
    serverFromBackend NoauthSpecBackend{..} =
      (coerce healthGet :<|>
       coerce usersGet :<|>
       coerce usersIdGet :<|>
       serveDirectoryFileServer "static")


serverContext :: Context ('[])
serverContext = EmptyContext
