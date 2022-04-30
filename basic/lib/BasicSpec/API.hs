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

module BasicSpec.API
  ( -- * Client and Server
    Config(..)
  , BasicSpecBackend(..)
  , createBasicSpecClient
  , runBasicSpecServer
  , runBasicSpecMiddlewareServer
  , runBasicSpecClient
  , runBasicSpecClientWithManager
  , callBasicSpec
  , BasicSpecClient
  , BasicSpecClientError(..)
  -- ** Servant
  , BasicSpecAPI
  -- ** Plain WAI Application
  , serverWaiApplicationBasicSpec
  -- ** Authentication
  , BasicSpecAuth(..)
  , clientAuth
  , Protected
  ) where

import           BasicSpec.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
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
import           Network.Wai                        (Middleware, Request, requestHeaders)
import qualified Network.Wai.Handler.Warp           as Warp
import           Network.Wai.Middleware.HttpAuth    (extractBasicAuth)
import           Servant                            (ServerError, serveWithContext, throwError)
import           Servant.API                        hiding (addHeader)
import           Servant.API.BasicAuth              (BasicAuthData (..))
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.API.Experimental.Auth      (AuthProtect)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost, basicAuthReq, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..), Application, Context ((:.), EmptyContext))
import           Servant.Server.Experimental.Auth   (AuthHandler, AuthServerData, mkAuthHandler)
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


-- | Servant type-level API, generated from the OpenAPI spec for BasicSpec.
type BasicSpecAPI
    =    "health" :> Verb 'GET 200 '[JSON] NoContent -- 'healthGet' route
    :<|> Protected :> "users" :> Verb 'GET 200 '[JSON] [User] -- 'usersGet' route
    :<|> Protected :> "users" :> Capture "id" Int :> Verb 'GET 200 '[JSON] User -- 'usersIdGet' route
    :<|> Raw


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype BasicSpecClientError = BasicSpecClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for BasicSpec.
-- The backend can be used both for the client and the server. The client generated from the BasicSpec OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createBasicSpecClient@). Alternatively, provided
-- a backend, the API can be served using @runBasicSpecMiddlewareServer@.
data BasicSpecBackend a m = BasicSpecBackend
  { healthGet :: m NoContent{- ^  -}
  , usersGet :: a -> m [User]{- ^  -}
  , usersIdGet :: a -> Int -> m User{- ^  -}
  }

-- | Authentication settings for BasicSpec.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data BasicSpecAuth = BasicSpecAuth
  { lookupUser :: BasicAuthData -> Handler AuthServer
  , authError :: Request -> ServerError
  }

newtype BasicSpecClient a = BasicSpecClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative BasicSpecClient where
  pure x = BasicSpecClient (\_ -> pure x)
  (BasicSpecClient f) <*> (BasicSpecClient x) =
    BasicSpecClient (\env -> f env <*> x env)

instance Monad BasicSpecClient where
  (BasicSpecClient a) >>= f =
    BasicSpecClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO BasicSpecClient where
  liftIO io = BasicSpecClient (\_ -> liftIO io)

createBasicSpecClient :: BasicSpecBackend AuthClient BasicSpecClient
createBasicSpecClient = BasicSpecBackend{..}
  where
    ((coerce -> healthGet) :<|>
     (coerce -> usersGet) :<|>
     (coerce -> usersIdGet) :<|>
     _) = client (Proxy :: Proxy BasicSpecAPI)

-- | Run requests in the BasicSpecClient monad.
runBasicSpecClient :: Config -> BasicSpecClient a -> ExceptT ClientError IO a
runBasicSpecClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runBasicSpecClientWithManager manager clientConfig cl

-- | Run requests in the BasicSpecClient monad using a custom manager.
runBasicSpecClientWithManager :: Manager -> Config -> BasicSpecClient a -> ExceptT ClientError IO a
runBasicSpecClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a BasicSpecClientError
callBasicSpec
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> BasicSpecClient a -> m a
callBasicSpec env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (BasicSpecClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the BasicSpec server at the provided host and port.
runBasicSpecServer
  :: (MonadIO m, MonadThrow m)
  => Config -> BasicSpecAuth -> BasicSpecBackend AuthServer (ExceptT ServerError IO) -> m ()
runBasicSpecServer config auth backend = runBasicSpecMiddlewareServer config requestMiddlewareId auth backend

-- | Run the BasicSpec server at the provided host and port.
runBasicSpecMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> BasicSpecAuth -> BasicSpecBackend AuthServer (ExceptT ServerError IO) -> m ()
runBasicSpecMiddlewareServer Config{..} middleware auth backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationBasicSpec auth backend

-- | Plain "Network.Wai" Application for the BasicSpec server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationBasicSpec :: BasicSpecAuth -> BasicSpecBackend AuthServer (ExceptT ServerError IO) -> Application
serverWaiApplicationBasicSpec auth backend = serveWithContext (Proxy :: Proxy BasicSpecAPI) context (serverFromBackend backend)
  where
    context = serverContext auth
    serverFromBackend BasicSpecBackend{..} =
      (coerce healthGet :<|>
       coerce usersGet :<|>
       coerce usersIdGet :<|>
       serveDirectoryFileServer "static")

-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: BasicSpecAuth -> AuthHandler Request AuthServer
authHandler BasicSpecAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "Authorization" (requestHeaders req) of
      Just header -> case extractBasicAuth header of
        Just (user, password) -> lookupUser (BasicAuthData user password)
        Nothing -> throwError (authError req)
      Nothing -> throwError (authError req)

type Protected = AuthProtect "basic"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = BasicAuthData

clientAuth :: BasicAuthData -> AuthClient
clientAuth key = mkAuthenticatedRequest key basicAuthReq

serverContext :: BasicSpecAuth -> Context (AuthHandler Request AuthServer ': '[])
serverContext auth = authHandler auth :. EmptyContext
