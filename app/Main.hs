{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson (FromJSON)
import Data.Data (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HTTP.Client
import Network.HTTP.Client.TLS qualified as HTTP.Client.TLS
import Servant.API
import Servant.Client qualified as Client
import System.Environment qualified as Environment
import Web.FormUrlEncoded (ToForm)

newtype Token = Token {access_token :: Text}
  deriving (Generic, FromJSON)

data Credentials = Credentials
  { client_id :: Text,
    client_secret :: Text,
    grant_type :: Text
  }
  deriving (Generic, ToForm)

-- curl -X POST "https://accounts.spotify.com/api/token" \
--      -H "Content-Type: application/x-www-form-urlencoded" \
--      -d "grant_type=client_credentials&client_id=your-client-id&client_secret=your-client-secret"

type OAuth2 =
  "token"
    :> ReqBody '[FormUrlEncoded] Credentials
    :> Post '[JSON] Token

oauth2 = Client.client @OAuth2 Proxy

-- curl "https://api.spotify.com/v1/artists/4Z8W4fKeB5YxbusRsdQVPb" \
--      -H "Authorization: Bearer  BQDBKJ5eo5jxbtpWjVOj7ryS84khybFpP_lTqzV7uV-T_m0cTfwvdn5BnBSKPxKgEb11"

data Artist = Artist
  { name :: Text,
    genres :: [Text]
  }
  deriving (Generic, FromJSON, Show)

type GetArtist =
  "v1"
    :> "artists"
    :> Capture "id" Text
    :> Header' '[Required, Strict] "Authorization" Text
    :> Get '[JSON] Artist

getArtist = Client.client @GetArtist Proxy

getCredentials :: IO Credentials
getCredentials = do
  client_id <- Text.pack <$> Environment.getEnv "SPOTIFY_CLIENT_ID"
  client_secret <- Text.pack <$> Environment.getEnv "SPOTIFY_CLIENT_SECRET"
  let grant_type = "client_credentials"
  pure Credentials {..}

main :: IO ()
main = do
  accountsBaseUrl <- Client.parseBaseUrl "https://accounts.spotify.com/api"
  apiBaseUrl <- Client.parseBaseUrl "https://api.spotify.com"
  manager <- HTTP.Client.newManager HTTP.Client.TLS.tlsManagerSettings
  let accountsClientEnv = Client.mkClientEnv manager accountsBaseUrl
      apiClientEnv = Client.mkClientEnv manager apiBaseUrl
  credentials <- getCredentials
  tokenResponse <- Client.runClientM (oauth2 credentials) accountsClientEnv
  token <- case tokenResponse of
    Left err -> error (show err)
    Right token -> do
      pure token
  artist <- Client.runClientM (getArtist "4Z8W4fKeB5YxbusRsdQVPb" ("Bearer " <> access_token token)) apiClientEnv
  print artist
