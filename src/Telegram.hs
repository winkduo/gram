{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoMonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Telegram (TelegramController (..), mkTelegramController, UserId (..), ChatId (..)) where

import           Control.Concurrent.Async            (async)
import           Control.Concurrent.MVar             (modifyMVar_, readMVar)
import qualified Control.Concurrent.Privileged       as PC
import           Control.Monad                       (forever)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import qualified Data.Aeson                          as JSON
import           Data.Foldable                       (for_)
import           Data.Functor                        (void)
import           Data.Int                            (Int64)
import           Data.Int                            (Int32)
import qualified Data.Text                           as T
import           System.Environment                  (getEnv)
import           System.Posix.Signals                (Handler (Catch),
                                                      installHandler, sigINT)
import qualified Telegram.Database.API.Authorization as TDLib
import qualified Telegram.Database.API.Update        as TDLib
import qualified Telegram.Database.JSON              as TDLib

newtype UserId = UserId { unUserId :: Int32 }
  deriving (Show, Eq, Ord)
newtype ChatId = ChatId { unChatId :: Int64 }
  deriving (Show, Eq, Ord)

setVerbosity :: TDLib.Client -> IO ()
setVerbosity =
  TDLib.send $ JSON.object [
    "@type"          JSON..= JSON.String ("setLogVerbosityLevel" :: T.Text),
    "new_verbosity_level_" JSON..= JSON.toJSON (0 :: Int)
  ]

data TelegramController m update = TelegramController
  { _tcSubscribeToUpdates :: m (PC.ReadOnlyChan update)
  , _tcSendMessage        :: ChatId -> T.Text -> m ()
  }

data PubSub m update = PubSub
  { _psPub :: update -> m ()
  , _psSub :: m (PC.ReadOnlyChan update)
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

sendMessage :: TDLib.Client -> ChatId -> T.Text -> IO ()
sendMessage client (ChatId chat_id) message =
  TDLib.send (JSON.object [
    "@type"          JSON..= JSON.String ("sendMessage" :: T.Text),
    "chat_id"       JSON..= JSON.toJSON chat_id,
    "input_message_content" JSON..= JSON.object [
      "@type" JSON..= JSON.String "inputMessageText",
      "text" JSON..= JSON.object [
        "@type" JSON..= JSON.String "formattedText",
        "text" JSON..= JSON.String message
      ]
    ]
  ]) client

mkTelegramController :: IO (TelegramController IO TDLib.Update, IO ())
mkTelegramController = do
  client <- TDLib.create
  let destroy = TDLib.destroy client
  void $ installHandler sigINT (Catch destroy) Nothing

  setVerbosity client
  config <- readAuthConfig
  PubSub pub sub <- mkPubSub
  void $ async $ forever $ TDLib.receiveEither client >>= handleUpdate client config pub
  pure $ (TelegramController sub (sendMessage client), destroy)

data TDAuthConfig = TDAuthConfig
  { _acApiId       :: Int
  , _acApiHash     :: String
  , _acPhoneNumber :: T.Text
  } deriving (Show, Eq)

handleUpdate :: TDLib.Client -> TDAuthConfig -> (TDLib.Update -> IO ()) -> Either String TDLib.Update -> IO ()
handleUpdate client auth_config pub = \case
  Right (TDLib.UpdateAuthorizationState auth_state) ->
    handle_auth_state auth_config auth_state
  Right update -> do
    putStrLn $ "Got update: " <> show update
    pub update
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
