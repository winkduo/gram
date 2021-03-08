{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
module Gram.Spam.Worker
  ( mkSpamDetector,
  )
where

------------------------------------------------------------------------------

import Control.Concurrent.Async (async)
import qualified Control.Concurrent.Privileged as PC
import Control.Concurrent.Timeout (threadDelay)
import Control.Monad (forever)
import Data.Functor (void)
import Data.IORef
  ( IORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
  )
import qualified Data.Map as M
import Data.Time.Clock (getCurrentTime)
import Gram.Spam.Debug (showSpamState)
import Gram.Spam.Detector (detectSpams)
import Gram.Spam.Notifications (Notification, notifySpams)
import Gram.Spam.Options (SpamOptions (SpamOptions, _soSpamCheckInterval))
import Gram.Types
  ( TelegramController (..),
    UserId (..),
  )
import qualified Telegram.Database.API.Messages as TDLib
import qualified Telegram.Database.API.Update as TDLib
import qualified Telegram.Database.API.User as TDLibUser

------------------------------------------------------------------------------

-- |
mkSpamDetector :: SpamOptions -> TelegramController IO TDLib.Update -> IO ()
mkSpamDetector opts@SpamOptions {_soSpamCheckInterval} (TelegramController sub send_message) = do
  messages <- newIORef []
  users <- newIORef M.empty
  void $ async $ process_updates messages users
  detect_spams [] messages users
  where
    detect_spams :: [Notification] -> IORef [TDLib.Message] -> IORef (M.Map UserId TDLibUser.User) -> IO ()
    detect_spams notifications messages users = do
      n <- readIORef messages
      u <- readIORef users
      putStrLn $ "Detecting spams... Messages state: " <> showSpamState u n
      curr_time <- getCurrentTime
      spams <- atomicModifyIORef' messages $ detectSpams opts curr_time
      updated_notifications <- notifySpams opts spams send_message u curr_time notifications
      threadDelay _soSpamCheckInterval
      detect_spams updated_notifications messages users
    process_updates :: IORef [TDLib.Message] -> IORef (M.Map UserId TDLibUser.User) -> IO ()
    process_updates messages users = do
      chan <- sub
      forever $
        PC.readChan chan >>= \case
          TDLib.UpdateNewMessage message _ _ ->
            atomicModifyIORef' messages $ \xs -> (message : xs, ())
          TDLib.UpdateUser user ->
            atomicModifyIORef' users $ \m -> (M.insert (UserId (TDLibUser.id user)) user m, ())
          _ ->
            pure ()
