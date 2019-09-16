{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import           Control.Applicative             (liftA2)
import           Control.Concurrent.Async        (async, wait, waitAny, cancel)
import           Data.Functor                    (void)
import           Data.Maybe
import qualified Data.Text                       as T
import           Network.HTTP.Client             (newManager)
import           Network.HTTP.Client.TLS         (tlsManagerSettings)
import           Servant.Client.Core.ClientError (ClientError)
import           Spam                            (detectSpams, Spam (..))
import qualified Web.Telegram.API.Bot            as Tgrm
import Telegram
import qualified LoadEnv
import qualified Control.Concurrent.Privileged as PC
import Control.Monad (forever)
import Control.Concurrent.Timeout (threadDelay, Seconds, DurationUnit (Hour, Minute), (#))
import Data.Duration (Duration (..))
import qualified Telegram.Database.API.Messages as TDLib
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import           Data.Time.Clock
import Data.Traversable (for)
import Data.Foldable (for_)
import Text.Printf
import Data.Int (Int32)
import Data.List (find)
import Data.Coerce (coerce)
import System.Posix.Signals

chatId :: Tgrm.ChatId
chatId = Tgrm.ChatId (-1001330413002)

data Notification = Notification
  { _nUserId :: Int32
  , _nDate :: UTCTime
  }

notificationKeepDuration :: Duration
notificationKeepDuration = 1 # Hour

spamCheckInterval :: Duration
spamCheckInterval = 5 # Minute

toNDT :: Duration -> NominalDiffTime
toNDT (Duration us) = fromIntegral $ us `div` (10 ^ 6)

mkSpamDetector :: TelegramController IO TDLib.Message -> IO ()
mkSpamDetector (TelegramController sub send_message) = do
  messages <- newIORef []
  void $ async $ push_messages messages
  detect_spams [] messages
  where
    detect_spams :: [Notification] -> IORef [TDLib.Message] -> IO ()
    detect_spams notifications messages = do
      threadDelay spamCheckInterval
      curr_time <- getCurrentTime
      spams <- atomicModifyIORef' messages $ detectSpams curr_time
      let notifications_in_effect = filter ((> curr_time) . (addUTCTime (toNDT notificationKeepDuration)) . _nDate) notifications
      new_notifications <- fmap catMaybes $ for spams $ \(Spam user spam_messages) -> do
        let message = T.pack $ printf "You've spammed with %d messages" $ length spam_messages
        send_message 0 message
        case find (== user) (map _nUserId notifications_in_effect) of
          Just not ->
            pure $ Nothing
          Nothing -> do
            notification_time <- getCurrentTime
            pure $ Just $ Notification user notification_time
      detect_spams (notifications_in_effect ++ new_notifications) messages

    push_messages :: IORef [TDLib.Message] -> IO ()
    push_messages messages = do
      chan <- sub
      forever $ do
        message <- PC.readChan chan
        atomicModifyIORef' messages $ \xs -> (message:xs, ())

main :: IO ()
main = do
  LoadEnv.loadEnv
  (telegram_ctrl, destroy_telegram_ctrl) <- mkTelegramController
  spam_detector <- async $ mkSpamDetector telegram_ctrl
  let workers = [spam_detector]
  void $ installHandler sigINT (Catch (for_ workers cancel *> destroy_telegram_ctrl)) Nothing
  void $ waitAny workers
  putStrLn "A thread is terminated, exiting."
