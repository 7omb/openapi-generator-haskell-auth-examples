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

module ApikeySpec.API
  ( -- * Client and Server
    Config(..)
  , ApikeySpecBackend(..)
  , createApikeySpecClient
  , runApikeySpecServer
  , runApikeySpecMiddlewareServer
  , runApikeySpecClient
  , runApikeySpecClientWithManager
  , callApikeySpec
  , ApikeySpecClient
  , ApikeySpecClientError(..)
  -- ** Servant
  , ApikeySpecAPI
  -- ** Plain WAI Application
  , serverWaiApplicationApikeySpec
  -- ** Authentication
  , ApikeySpecAuth(..)
  , clientAuth
  , Protected
  ) where

import           ApikeySpec.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.ByteString                    (ByteString)
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
import           Network.Wai                        (Middleware, Request, requestHeaders)
import qualified Network.Wai.Handler.Warp           as Warp
import           Servant                            (ServerError, serveWithContextT, throwError)
import           Servant.API                        hiding (addHeader)
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.API.Experimental.Auth      (AuthProtect)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost, AuthClientData, AuthenticatedRequest, addHeader, mkAuthenticatedRequest)
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


-- | Servant type-level API, generated from the OpenAPI spec for ApikeySpec.
type ApikeySpecAPI
    =    "health" :> Verb 'GET 200 '[JSON] NoContent -- 'healthGet' route
    :<|> Protected :> "users" :> Verb 'GET 200 '[JSON] (Headers '[Header "X-Header-1" Text] [User]) -- 'usersGet' route
    :<|> Protected :> "users" :> Capture "id" Int :> Verb 'GET 200 '[JSON] (Headers '[Header "X-Header-1" Int, Header "Set-Cookie" Text, Header "Set-Cookie" Text, Header "Set-Cookie" Text] User) -- 'usersIdGet' route
    :<|> Raw


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype ApikeySpecClientError = ApikeySpecClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for ApikeySpec.
-- The backend can be used both for the client and the server. The client generated from the ApikeySpec OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createApikeySpecClient@). Alternatively, provided
-- a backend, the API can be served using @runApikeySpecMiddlewareServer@.
data ApikeySpecBackend a m = ApikeySpecBackend
  { healthGet :: m NoContent{- ^  -}
  , usersGet :: a -> m (Headers '[Header "X-Header-1" Text] [User]){- ^  -}
  , usersIdGet :: a -> Int -> m (Headers '[Header "X-Header-1" Int, Header "Set-Cookie" Text, Header "Set-Cookie" Text, Header "Set-Cookie" Text] User){- ^  -}
  }

-- | Authentication settings for ApikeySpec.
-- lookupUser is used to retrieve a user given a header value. The data type can be specified by providing an
-- type instance for AuthServerData. authError is a function that given a request returns a custom error that
-- is returned when the header is not found.
data ApikeySpecAuth = ApikeySpecAuth
  { lookupUser :: ByteString -> Handler AuthServer
  , authError :: Request -> ServerError
  }

newtype ApikeySpecClient a = ApikeySpecClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative ApikeySpecClient where
  pure x = ApikeySpecClient (\_ -> pure x)
  (ApikeySpecClient f) <*> (ApikeySpecClient x) =
    ApikeySpecClient (\env -> f env <*> x env)

instance Monad ApikeySpecClient where
  (ApikeySpecClient a) >>= f =
    ApikeySpecClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO ApikeySpecClient where
  liftIO io = ApikeySpecClient (\_ -> liftIO io)

createApikeySpecClient :: ApikeySpecBackend AuthClient ApikeySpecClient
createApikeySpecClient = ApikeySpecBackend{..}
  where
    ((coerce -> healthGet) :<|>
     (coerce -> usersGet) :<|>
     (coerce -> usersIdGet) :<|>
     _) = client (Proxy :: Proxy ApikeySpecAPI)

-- | Run requests in the ApikeySpecClient monad.
runApikeySpecClient :: Config -> ApikeySpecClient a -> ExceptT ClientError IO a
runApikeySpecClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runApikeySpecClientWithManager manager clientConfig cl

-- | Run requests in the ApikeySpecClient monad using a custom manager.
runApikeySpecClientWithManager :: Manager -> Config -> ApikeySpecClient a -> ExceptT ClientError IO a
runApikeySpecClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a ApikeySpecClientError
callApikeySpec
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> ApikeySpecClient a -> m a
callApikeySpec env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (ApikeySpecClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the ApikeySpec server at the provided host and port.
runApikeySpecServer
  :: (MonadIO m, MonadThrow m)
  => Config -> ApikeySpecAuth -> ApikeySpecBackend AuthServer (ExceptT ServerError IO) -> m ()
runApikeySpecServer config auth backend = runApikeySpecMiddlewareServer config requestMiddlewareId auth backend

-- | Run the ApikeySpec server at the provided host and port.
runApikeySpecMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> ApikeySpecAuth -> ApikeySpecBackend AuthServer (ExceptT ServerError IO) -> m ()
runApikeySpecMiddlewareServer Config{..} middleware auth backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationApikeySpec auth backend

-- | Plain "Network.Wai" Application for the ApikeySpec server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationApikeySpec :: ApikeySpecAuth -> ApikeySpecBackend AuthServer (ExceptT ServerError IO) -> Application
serverWaiApplicationApikeySpec auth backend = serveWithContextT (Proxy :: Proxy ApikeySpecAPI) context id (serverFromBackend backend)
  where
    context = serverContext auth
    serverFromBackend ApikeySpecBackend{..} =
      (coerce healthGet :<|>
       coerce usersGet :<|>
       coerce usersIdGet :<|>
       serveDirectoryFileServer "static")

-- Authentication is implemented with servants generalized authentication:
-- https://docs.servant.dev/en/stable/tutorial/Authentication.html#generalized-authentication

authHandler :: ApikeySpecAuth -> AuthHandler Request AuthServer
authHandler ApikeySpecAuth{..} = mkAuthHandler handler
  where
    handler req = case lookup "X-API-KEY" (requestHeaders req) of
      Just header -> lookupUser header
      Nothing -> throwError (authError req)

type Protected = AuthProtect "apikey"
type AuthServer = AuthServerData Protected
type AuthClient = AuthenticatedRequest Protected
type instance AuthClientData Protected = Text

clientAuth :: Text -> AuthClient
clientAuth key = mkAuthenticatedRequest key (addHeader "X-API-KEY")

serverContext :: ApikeySpecAuth -> Context (AuthHandler Request AuthServer ': '[])
serverContext auth = authHandler auth :. EmptyContext
