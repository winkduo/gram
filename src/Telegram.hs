{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram where

import qualified Telegram.Client as TDLib
import qualified Telegram.Database.JSON        as TDLib
import qualified Telegram.Database.API.Authorization as TDLib
import qualified Telegram.Database.API.Update as TDLib
import qualified Telegram.Database.API.Messages as TDLib
import System.Environment
import qualified Data.Text as T
import qualified Data.Aeson as JSON
import qualified Control.Concurrent.Privileged as PC
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import Data.Foldable (for_)
import Data.Functor (void)
import Control.Concurrent.Async
import Control.Monad (forever)
import Data.Int (Int64)
import System.Posix.Signals

setVerbosity :: TDLib.Client -> IO ()
setVerbosity =
  TDLib.send $ JSON.object [
    "@type"          JSON..= JSON.String ("setLogVerbosityLevel" :: T.Text),
    "new_verbosity_level_" JSON..= JSON.toJSON (0 :: Int)
  ]

data TelegramController m update = TelegramController
  { _tcSubscribeToUpdates :: m (PC.ReadOnlyChan update)
  , _tcSendMessage :: Int64 -> T.Text -> m ()
  }

data PubSub m update = PubSub
  { pub :: update -> m ()
  , sub :: m (PC.ReadOnlyChan update)
  }

mkPubSub :: MonadIO m => m (PubSub m update)
mkPubSub = do
  subscribers <- PC.newMVar []
  let pub update = do
        subs <- liftIO $ readMVar subscribers
        for_ subs $ flip PC.writeChan update
  let sub = do
        chan <- PC.newChan
        liftIO $ modifyMVar_ subscribers (pure . (PC.toWriteOnlyChan chan :))
        pure $ PC.toReadOnlyChan chan
  pure $ PubSub pub sub

readAuthConfig :: IO TDAuthConfig
readAuthConfig =
  TDAuthConfig
    <$> (read <$> getEnv "API_ID")
    <*> getEnv "API_HASH"
    <*> (T.pack <$> getEnv "API_PHONE_NUMBER")

sendMessage :: TDLib.Client -> Int64 -> T.Text -> IO ()
sendMessage client chat_id message =
  TDLib.send (JSON.object [
    "@type"          JSON..= JSON.String ("sendMessage" :: T.Text),
    "chat_id_"       JSON..= JSON.toJSON chat_id,
    "input_message_content_" JSON..= JSON.toJSON message
  ]) client

mkTelegramController :: IO (TelegramController IO TDLib.Message, IO ())
mkTelegramController = do
  client <- TDLib.create
  let destroy = TDLib.destroy client
  void $ installHandler sigINT (Catch destroy) Nothing
  config <- readAuthConfig
  PubSub pub sub <- mkPubSub
  listener <- async $ forever $ TDLib.receiveEither client >>= handleUpdate client config pub
  pure $ (TelegramController sub (sendMessage client), destroy)

data TDAuthConfig = TDAuthConfig
  { _acApiId :: Int
  , _acApiHash :: String
  , _acPhoneNumber :: T.Text
  } deriving (Show, Eq)

handleUpdate :: TDLib.Client -> TDAuthConfig -> (TDLib.Message -> IO ()) -> Either String TDLib.Update -> IO ()
handleUpdate client auth_config pub = \case
  Right (TDLib.UpdateAuthorizationState auth_state) ->
    handle_auth_state auth_config auth_state
  Right (TDLib.UpdateNewMessage message _ _) ->
    pub message
  Left "NULL" ->
    pure ()
  unknown ->
    print unknown
  where
    handle_auth_state :: TDAuthConfig -> TDLib.AuthorizationState -> IO ()
    handle_auth_state TDAuthConfig {..} = \case
      TDLib.AuthorizationStateWaitTdlibParameters ->
        TDLib.setTdlibParameters (TDLib.defaultTdlibParameters { TDLib.api_id = _acApiId, TDLib.api_hash = _acApiHash }) client
      (TDLib.AuthorizationStateWaitEncryptionKey _) ->
        TDLib.checkDatabaseEncryptionKey "" client
      TDLib.AuthorizationStateWaitPhoneNumber -> do
        phoneNumber <- T.pack <$> getEnv "API_PHONE_NUMBER"
        TDLib.setAuthenticationPhoneNumber phoneNumber False False client
      TDLib.AuthorizationStateWaitCode {} -> do
        putStrLn "Please, enter verification code:"
        code <- T.pack <$> getLine
        TDLib.checkAuthenticationCode code "" "" client
      TDLib.AuthorizationStateReady ->
        pure ()
      unknown ->
        print $ "Unknown auth state: " <> show unknown
