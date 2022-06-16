{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module CustomMonadSpec.API
  ( -- * Client and Server
    Config(..)
  , CustomMonadSpecBackend(..)
  , createCustomMonadSpecClient
  , runCustomMonadSpecServer
  , runCustomMonadSpecMiddlewareServer
  , runCustomMonadSpecClient
  , runCustomMonadSpecClientWithManager
  , callCustomMonadSpec
  , CustomMonadSpecClient
  , CustomMonadSpecClientError(..)
  -- ** Servant
  , CustomMonadSpecAPI
  -- ** Plain WAI Application
  , serverWaiApplicationCustomMonadSpec
  ) where

import           CustomMonadSpec.Types

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
import           Servant                            (ServerError, serveWithContextT)
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


-- | Servant type-level API, generated from the OpenAPI spec for CustomMonadSpec.
type CustomMonadSpecAPI
    =    "health" :> Verb 'GET 200 '[JSON] NoContent -- 'healthGet' route
    :<|> "users" :> Verb 'GET 200 '[JSON] [User] -- 'usersGet' route
    :<|> "users" :> Capture "id" Int :> Verb 'GET 200 '[JSON] User -- 'usersIdGet' route
    :<|> Raw


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype CustomMonadSpecClientError = CustomMonadSpecClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for CustomMonadSpec.
-- The backend can be used both for the client and the server. The client generated from the CustomMonadSpec OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createCustomMonadSpecClient@). Alternatively, provided
-- a backend, the API can be served using @runCustomMonadSpecMiddlewareServer@.
data CustomMonadSpecBackend m = CustomMonadSpecBackend
  { healthGet :: m NoContent{- ^  -}
  , usersGet :: m [User]{- ^  -}
  , usersIdGet :: Int -> m User{- ^  -}
  }


newtype CustomMonadSpecClient a = CustomMonadSpecClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative CustomMonadSpecClient where
  pure x = CustomMonadSpecClient (\_ -> pure x)
  (CustomMonadSpecClient f) <*> (CustomMonadSpecClient x) =
    CustomMonadSpecClient (\env -> f env <*> x env)

instance Monad CustomMonadSpecClient where
  (CustomMonadSpecClient a) >>= f =
    CustomMonadSpecClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO CustomMonadSpecClient where
  liftIO io = CustomMonadSpecClient (\_ -> liftIO io)

createCustomMonadSpecClient :: CustomMonadSpecBackend CustomMonadSpecClient
createCustomMonadSpecClient = CustomMonadSpecBackend{..}
  where
    ((coerce -> healthGet) :<|>
     (coerce -> usersGet) :<|>
     (coerce -> usersIdGet) :<|>
     _) = client (Proxy :: Proxy CustomMonadSpecAPI)

-- | Run requests in the CustomMonadSpecClient monad.
runCustomMonadSpecClient :: Config -> CustomMonadSpecClient a -> ExceptT ClientError IO a
runCustomMonadSpecClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runCustomMonadSpecClientWithManager manager clientConfig cl

-- | Run requests in the CustomMonadSpecClient monad using a custom manager.
runCustomMonadSpecClientWithManager :: Manager -> Config -> CustomMonadSpecClient a -> ExceptT ClientError IO a
runCustomMonadSpecClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a CustomMonadSpecClientError
callCustomMonadSpec
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> CustomMonadSpecClient a -> m a
callCustomMonadSpec env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (CustomMonadSpecClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the CustomMonadSpec server at the provided host and port.
runCustomMonadSpecServer
  :: (MonadIO m, MonadThrow m)
  => Config -> (forall x . n x -> Handler x) -> CustomMonadSpecBackend n -> m ()
runCustomMonadSpecServer config nat backend = runCustomMonadSpecMiddlewareServer config requestMiddlewareId nat backend

-- | Run the CustomMonadSpec server at the provided host and port.
runCustomMonadSpecMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> (forall x . n x -> Handler x) -> CustomMonadSpecBackend n -> m ()
runCustomMonadSpecMiddlewareServer Config{..} middleware nat backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationCustomMonadSpec nat backend

-- | Plain "Network.Wai" Application for the CustomMonadSpec server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationCustomMonadSpec :: (forall x . n x -> Handler x) -> CustomMonadSpecBackend n -> Application
serverWaiApplicationCustomMonadSpec nat backend = serveWithContextT (Proxy :: Proxy CustomMonadSpecAPI) context nat (serverFromBackend backend)
  where
    context = serverContext
    serverFromBackend CustomMonadSpecBackend{..} =
      (coerce healthGet :<|>
       coerce usersGet :<|>
       coerce usersIdGet :<|>
       serveDirectoryFileServer "static")


serverContext :: Context ('[])
serverContext = EmptyContext
