{-# LANGUAGE DeriveGeneric, ExtendedDefaultRules, LambdaCase    #-}
{-# LANGUAGE NamedFieldPuns, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables                                #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-orphans #-}
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
import qualified Data.Text.Lazy             as LT
import           Data.Yaml                  (decodeFileEither)
import           GHC.Generics               (Generic)
import           Network.WebSockets
import           System.Environment
import           Text.HTML.TagSoup
import           Text.Mustache
-- import           Web.Hastodon               (Status (..), accountAcct)
import qualified Web.Twitter.Conduit as Tw
import           Wuss                hiding (Config, defaultConfig)

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
                     , template :: Template
                     }
            deriving (Show, Eq, Ord, Generic)


instance FromJSON Template where
  parseJSON = withText "valid mustache template" $ \t ->
    either (fail . show) return $
    compileMustacheText "template" (LT.fromStrict t)

mastOpts, cnfOpts, statOpts, accountOpts :: Options
mastOpts = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4}
cnfOpts = defaultOptions { fieldLabelModifier = id}
statOpts = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6}
accountOpts = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 7}

instance FromJSON Mastodon where
  parseJSON = genericParseJSON mastOpts

instance ToJSON Mastodon where
  toJSON = genericToJSON mastOpts
  toEncoding = genericToEncoding mastOpts

instance FromJSON Twitter where
  parseJSON = genericParseJSON mastOpts

instance FromJSON Config where
  parseJSON = genericParseJSON cnfOpts

data Status = Status { statusContent  :: String
                     , statusMentions :: [String]
                     , statusAccount  :: Account
                     }
            deriving (Read, Show, Eq, Ord, Generic)
data Account = Account { accountAcct :: String
                       }
             deriving (Read, Show, Eq, Ord, Generic)


instance FromJSON Account where
  parseJSON = genericParseJSON accountOpts

instance FromJSON Status where
  parseJSON = genericParseJSON statOpts

main :: IO ()
main = do
  fp <- fromMaybe "settings.yaml" . listToMaybe <$> getArgs
  decodeFileEither fp >>= \case
    Left err -> throwIO err
    Right Config{template, mastodon = Mastodon{..}, twitter = Twitter{..}} ->
      ignoreParseError $ ignoreHandshakeException $
      runSecureClientWith
          mastInstance
          443
          "/api/v1/streaming?stream=user"
          defaultConnectionOptions
          [("Authorization", BS.pack $ "Bearer " ++ mastAccessToken)] $ \conn ->
          forever $ do
            mst <- getStatus <$> receiveData conn
            forM_ mst $ \(val, Status{..}) -> do
              man <- Tw.newManager Tw.tlsManagerSettings
              let oauth = Tw.twitterOAuth { Tw.oauthConsumerKey = BS.pack twitConsumerKey
                                          , Tw.oauthConsumerSecret = BS.pack twitConsumerSecret
                                          }
                  creds = Tw.Credential [("oauth_token", BS.pack twitAccessToken)
                                        ,("oauth_token_secret", BS.pack twitAccessTokenSecret)
                                        ]
                  trivial = LT.length $ renderMustache template (val & key "content" . _String .~ "")
                  threshold = (140 - fromIntegral trivial)
                  cont  = fromTagText =<< filter isTagText (parseTags statusContent)
                  toot | length cont > threshold = take (threshold - 4) cont ++ "..."
                       | otherwise = cont
                  go    = null statusMentions && accountAcct statusAccount == mastUserName
                  tweet = renderMustache template (val & key "content" . _String .~ T.pack toot)
              when go $ ignoreTwitterError $ discardValue $
                Tw.call' (Tw.setCredential oauth creds Tw.def) man $
                Tw.update $ LT.toStrict tweet

discardValue :: Functor f => f Value -> f ()
discardValue = void

ignoreTwitterError :: IO a -> IO ()
ignoreTwitterError act = try act >>= \case
  Left (_ :: Tw.TwitterError) -> return ()
  Right _ -> return ()

ignoreParseError :: IO a -> IO ()
ignoreParseError act =
  let loop = try act >>= \case
        Right _ -> loop
        Left ParseException{} -> loop
        _ -> return ()
  in loop


ignoreHandshakeException :: IO a -> IO ()
ignoreHandshakeException act =
  let loop = try act >>= \case
        Right _ -> loop
        Left MalformedResponse{} -> loop
        _ -> return ()
  in loop

getStatus :: BS.ByteString -> Maybe (Value, Status)
getStatus obj = do
  src <- obj ^? key "payload" . _String . to (LBS.fromStrict . T.encodeUtf8)
  (,) <$> decode src <*> decode src
