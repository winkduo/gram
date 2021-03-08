{-# LANGUAGE DerivingStrategies #-}

-- |
module Gram.Telegram
  ( -- *
    mkTelegramController,
  )
where

------------------------------------------------------------------------------

import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (modifyMVar_, readMVar)
import qualified Control.Concurrent.Privileged as PC
import Control.Concurrent.Timeout (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as A
import Data.Duration (DurationUnit (..), (#))
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Maybe
import Data.String (IsString)
import Data.Text (Text, pack)
import Gram.Types
import Safe
import System.Directory (doesFileExist, removeFile)
import System.Environment (getEnv)
import System.Posix.Signals (Handler (..), installHandler, sigINT)
import qualified Telegram.Database.API.Authorization as TDLib
import qualified Telegram.Database.API.Update as TDLib
import qualified Telegram.Database.JSON as TDLib

------------------------------------------------------------------------------

-- |
setVerbosity :: Int -> TDLib.Client -> IO ()
setVerbosity verbosityLevel =
  TDLib.send $
    A.object
      [ "@type" A..= A.String "setLogVerbosityLevel",
        "new_verbosity_level_" A..= A.toJSON verbosityLevel
      ]

------------------------------------------------------------------------------

-- |
mkUpdateController :: MonadIO m => m (UpdateController m update)
mkUpdateController = do
  subscribers <- PC.newMVar []
  let _updateController_publish update = do
        -- TODO(dalp): huh?
        subs <- liftIO $ readMVar subscribers
        for_ subs $ flip PC.writeChan update
      _updateController_subscribe = do
        chan <- PC.newChan
        liftIO $ modifyMVar_ subscribers (pure . (PC.toWriteOnlyChan chan :))
        pure $ PC.toReadOnlyChan chan
   in pure $ UpdateController {..}

------------------------------------------------------------------------------

-- |
readAuthConfig :: IO TDAuthConfig
readAuthConfig = do
  -- TODO(dalp): Error handling
  _tdAuthConfig_apiId <- (fromMaybe $ error "API_ID exists but isn't valid.") <$> readMay <$> getEnv "API_ID"
  _tdAuthConfig_apiHash <- getEnv "API_HASH"
  _tdAuthConfig_phoneNumber <- pack <$> getEnv "API_PHONE_NUMBER"
  pure $
    TDAuthConfig {..}

------------------------------------------------------------------------------

-- |
sendMessage :: TDLib.Client -> ChatId -> Text -> IO ()
sendMessage client (ChatId chat_id) message =
  TDLib.send
    ( A.object
        [ "@type" A..= A.String "sendMessage",
          "chat_id" A..= A.toJSON chat_id,
          "input_message_content"
            A..= A.object
              [ "@type" A..= A.String "inputMessageText",
                "text"
                  A..= A.object
                    [ "@type" A..= A.String "formattedText",
                      "text" A..= A.String message
                    ]
              ]
        ]
    )
    client

------------------------------------------------------------------------------

-- |
mkTelegramController :: IO (TelegramController IO TDLib.Update, IO ())
mkTelegramController = do
  client <- TDLib.create
  let destroy = TDLib.destroy client
  void $ installHandler sigINT (Catch destroy) Nothing
  setVerbosity verbosityLevel client
  config <- readAuthConfig
  UpdateController publish subscribe <- mkUpdateController
  void $ async $ forever $ TDLib.receiveEither client >>= handleUpdate client config publish
  pure (TelegramController subscribe (sendMessage client), destroy)
  where
    verbosityLevel = 0

------------------------------------------------------------------------------

-- |
verificationCodeFilePath :: IsString s => s
verificationCodeFilePath = "/tmp/code"

------------------------------------------------------------------------------

-- |
readVerificationCode :: IO Text
readVerificationCode = do
  putStrLn $ "Trying to read verification code from " <> verificationCodeFilePath
  doesFileExist verificationCodeFilePath >>= \case
    True -> pack <$> readFile verificationCodeFilePath <* removeFile verificationCodeFilePath
    False -> threadDelay (5 # Second) >> readVerificationCode

------------------------------------------------------------------------------

-- |
handleUpdate ::
  TDLib.Client ->
  TDAuthConfig ->
  (TDLib.Update -> IO ()) ->
  Either String TDLib.Update ->
  IO ()
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
        TDLib.setTdlibParameters
          TDLib.defaultTdlibParameters
            { TDLib.api_id = _tdAuthConfig_apiId,
              TDLib.api_hash = _tdAuthConfig_apiHash
            }
          client
      (TDLib.AuthorizationStateWaitEncryptionKey _) ->
        TDLib.checkDatabaseEncryptionKey "" client
      TDLib.AuthorizationStateWaitPhoneNumber -> do
        phoneNumber <- pack <$> getEnv "API_PHONE_NUMBER"
        TDLib.setAuthenticationPhoneNumber phoneNumber False False client
      TDLib.AuthorizationStateWaitCode {} -> do
        code <- readVerificationCode
        TDLib.checkAuthenticationCode code "" "" client
      TDLib.AuthorizationStateReady ->
        pure ()
      unknown ->
        print $ "Unknown auth state: " <> show unknown
