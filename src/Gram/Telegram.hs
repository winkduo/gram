{-# LANGUAGE DerivingStrategies #-}
-- |
module Gram.Telegram
  ( -- *
    mkTelegramController,
  )
where

------------------------------------------------------------------------------

import qualified Data.Aeson as A
import Data.Text (Text, pack)
import Gram.Types
import Gram.Prelude
import qualified Gram.Prelude as PC
import Telegram.Database.API.Authorization as TDLib
import Telegram.Database.API.Update as TDLib
import Telegram.Database.JSON as TDLib

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
          "input_message_content" A..=
            commonTextSerialization "inputMessageText"
            (commonTextSerialization "formattedText" (A.String message))
        ]
    )
    client
  where
    commonTextSerialization typeStr textStr =
      A.object
      [ "@type" A..= A.String typeStr,
        "text" A..= textStr
      ]

------------------------------------------------------------------------------

-- |
mkTelegramController :: IO (TelegramController IO TDLib.Update, IO ())
mkTelegramController = do
  client <- TDLib.create
  let destroy' = TDLib.destroy client
  void $ installHandler sigINT (Catch destroy') Nothing
  setVerbosity verbosityLevel client
  config <- readAuthConfig
  UpdateController publish subscribe <- mkUpdateController
  void $ async $ forever $ TDLib.receiveEither client >>= handleUpdate client config publish
  pure (TelegramController subscribe (sendMessage client), destroy')
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
handleUpdate client config pub = \case
  Right (TDLib.UpdateAuthorizationState authState) ->
    handleAuthState config client authState
  Right update -> do
    -- TODO(dalp): These should just be logs
    putStrLn $ "Got update: " <> show update
    pub update
  Left unknown ->
    print unknown

------------------------------------------------------------------------------

-- |
handleAuthState ::
  TDAuthConfig ->
  TDLib.Client ->
  TDLib.AuthorizationState ->
  IO ()
handleAuthState TDAuthConfig {..} client = \case
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
