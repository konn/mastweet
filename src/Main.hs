{-# LANGUAGE DeriveGeneric, ExtendedDefaultRules, LambdaCase #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards              #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Yaml                  (decodeFileEither)
import           GHC.Generics               (Generic)
import           Network.WebSockets
import           System.Environment
import           Text.HTML.TagSoup
import           Web.Hastodon               (Status (..), accountUsername)
import qualified Web.Twitter.Conduit        as Tw
import           Wuss                       hiding (Config)

default (String)


data Twitter = Twitter { twitAccessToken       :: String
                       , twitAccessTokenSecret :: String
                       , twitConsumerKey       :: String
                       , twitConsumerSecret    :: String
                       }
             deriving (Read, Show, Eq, Ord, Generic)

data Mastodon = Mastodon { mastInstance     :: String
                         , mastOauthUri     :: String
                         , mastApiUri       :: String
                         , mastStreamingUri :: String
                         , mastUserName     :: String
                         , mastClientId     :: Integer
                         , mastClientKey    :: String
                         , mastClientSecret :: String
                         , mastAccessToken  :: String
                         }
              deriving (Read, Show, Eq, Ord, Generic)

data Config = Config { twitter  :: Twitter
                     , mastodon :: Mastodon
                     }
            deriving (Read, Show, Eq, Ord, Generic)


mastOpts, cnfOpts :: Options
mastOpts = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4}
cnfOpts = defaultOptions { fieldLabelModifier = id}

instance FromJSON Mastodon where
  parseJSON = genericParseJSON mastOpts

instance ToJSON Mastodon where
  toJSON = genericToJSON mastOpts
  toEncoding = genericToEncoding mastOpts

instance FromJSON Twitter where
  parseJSON = genericParseJSON mastOpts

instance ToJSON Twitter where
  toJSON = genericToJSON mastOpts
  toEncoding = genericToEncoding mastOpts

instance FromJSON Config where
  parseJSON = genericParseJSON cnfOpts

instance ToJSON Config where
  toJSON = genericToJSON mastOpts
  toEncoding = genericToEncoding cnfOpts

main :: IO ()
main = do
  fp <- fromMaybe "settings.yaml" . listToMaybe <$> getArgs
  decodeFileEither fp >>= \case
    Left err -> throwIO err
    Right Config{mastodon = Mastodon{..}, twitter = Twitter{..}} ->
      ignoreParseError $ runSecureClientWith
          mastInstance
          443
          "/api/v1/streaming?stream=user"
          defaultConnectionOptions
          [("Authorization", BS.pack $ "Bearer " ++ mastAccessToken)] $ \conn ->
          forever $ do
            mst <- getStatus <$> receiveData conn
            forM_ mst $ \Status{..} -> do
              man <- Tw.newManager Tw.tlsManagerSettings
              let oauth = Tw.twitterOAuth { Tw.oauthConsumerKey = BS.pack twitConsumerKey
                                          , Tw.oauthConsumerSecret = BS.pack twitConsumerSecret
                                          }
                  creds = Tw.Credential [("oauth_token", BS.pack twitAccessToken)
                                        ,("oauth_token_secret", BS.pack twitAccessTokenSecret)
                                        ]
                  toot  = fromTagText =<< filter isTagText (parseTags statusContent)
                  tweet = T.pack $ toot ++ " [Mastodonより: " ++ statusUrl ++ "]"
                  go    = null statusMentions && accountUsername statusAccount == mastUserName
              when go $ discardValue $
                Tw.call' (Tw.setCredential oauth creds Tw.def) man $
                Tw.update tweet



discardValue :: Functor f => f Value -> f ()
discardValue = void

ignoreParseError :: IO a -> IO ()
ignoreParseError act =
  let loop = try act >>= \case
        Right _ -> loop
        Left ParseException{} -> loop
        _ -> return ()
  in loop

getStatus :: BS.ByteString -> Maybe Status
getStatus obj = obj ^? key "payload" . _String . to T.encodeUtf8 >>= decode . LBS.fromStrict
