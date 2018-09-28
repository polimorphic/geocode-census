{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications, TypeOperators #-}

module Geocode.Census (geocode) where

import Prelude hiding (id, zip)

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Proxy (Proxy(Proxy))
import Data.Traversable (for)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API (Get, JSON, QueryParam, (:>))
import Servant.Client (BaseUrl(BaseUrl), ClientM, Scheme(Https), client, mkClientEnv, runClientM)

geocode :: String -> IO (Maybe (Double, Double))
geocode addr = do
    mgr <- newTlsManager
    let env = mkClientEnv mgr baseUrl
    r <- runClientM (geocodeClient (Just "Public_AR_Current") (Just addr)) env
    pure $ case r of
        Right (Output (x : _)) -> Just x
        _ -> Nothing

geocodeClient :: Maybe String -> Maybe String -> ClientM Output
geocodeClient = client $ Proxy @Api

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "geocoding.geo.census.gov" 443 "geocoder/locations/onelineaddress"

type Api = QueryParam "benchmark" String
        :> QueryParam "address" String
        :> Get '[JSON] Output

newtype Output = Output [(Double, Double)]
    deriving Show

instance FromJSON Output where
    parseJSON = withObject "Output" $ \o -> do
        r <- o .: "result"
        ms <- r .: "addressMatches"
        cs <- for ms $ \m -> do
            c <- m .: "coordinates"
            x <- c .: "x"
            y <- c .: "y"
            pure (x, y)
        pure $ Output cs
