{-# LANGUAGE LambdaCase #-}

module Main (main) where

import           Control.Arrow                  ((&&&), (>>>))
import           Control.Concurrent.Async       (async, cancel, waitAny)
import qualified Control.Concurrent.Privileged  as PC
import           Control.Concurrent.Timeout     (DurationUnit (Hour, Second),
                                                 threadDelay, ( # ))
import           Control.Monad                  (forever)
import qualified Data.Aeson                     as JSON
import           Data.Duration                  (Duration (Duration))
import           Data.Foldable                  (for_)
import           Data.Functor                   (void)
import           Data.Int                       (Int64)
import           Data.IORef                     (IORef, atomicModifyIORef',
                                                 newIORef, readIORef)
import           Data.List                      (find)
import qualified Data.Map                       as M
import           Data.Maybe                     (catMaybes, fromMaybe)
import qualified Data.Text                      as T
import           Data.Time.Clock                (NominalDiffTime, UTCTime,
                                                 addUTCTime, getCurrentTime)
import           Data.Traversable               (for)
import qualified LoadEnv
import           Spam                           (Spam (Spam), detectSpams)
import           System.Posix.Signals           (Handler (Catch),
                                                 installHandler, sigINT)
import           Telegram                       (ChatId (ChatId), TelegramController (TelegramController),
                                                 UserId (UserId),
                                                 mkTelegramController)
import qualified Telegram.Database.API.Messages as TDLib
import qualified Telegram.Database.API.Update   as TDLib
import qualified Telegram.Database.API.User     as TDLibUser

import           Text.Printf                    (printf)

-- Yigit - Alp private channel
yigitAlpChatId :: ChatId
yigitAlpChatId = ChatId 160758532

-- private GramBot
grambotChatId :: ChatId
grambotChatId = ChatId 804952120


data Notification = Notification
  { _nUserId :: UserId
  , _nDate   :: UTCTime
  }

notificationKeepDuration :: Duration
notificationKeepDuration = 1 # Hour

spamCheckInterval :: Duration
spamCheckInterval = 1 # Second

toNDT :: Duration -> NominalDiffTime
toNDT (Duration us) = fromIntegral $ us `div` (10 ^ (6 :: Int))

mkSpamDetector :: TelegramController IO TDLib.Update -> IO ()
mkSpamDetector (TelegramController sub send_message) = do
  messages <- newIORef []
  users <- newIORef M.empty
  void $ async $ process_updates messages users
  detect_spams [] messages users
  where
    detect_spams :: [Notification] -> IORef [TDLib.Message] -> IORef (M.Map UserId TDLibUser.User) -> IO ()
    detect_spams notifications messages users = do
      n <- readIORef messages
      u <- readIORef users
      putStrLn $ "Detecting spams... Messages state: " <> show (JSON.encode (message_state u n))
      threadDelay spamCheckInterval
      curr_time <- getCurrentTime
      spams <- atomicModifyIORef' messages $ detectSpams curr_time
      let notifications_in_effect = filter ((> curr_time) . addUTCTime (toNDT notificationKeepDuration) . _nDate) notifications
      new_notifications <- fmap catMaybes $ for spams $ notify_spam u notifications_in_effect
      detect_spams (notifications_in_effect ++ new_notifications) messages users

    notify_spam :: M.Map UserId TDLibUser.User -> [Notification] -> Spam -> IO (Maybe Notification)
    notify_spam users notifications_in_effect (Spam u@(UserId user_id) c@(ChatId chat_id) spam_messages) = do
      let user_id_or_name = uncurry fromMaybe $ (show &&& fmap TDLibUser.first_name . (`M.lookup` users)) $ UserId user_id
      if c == yigitAlpChatId then
        send_message yigitAlpChatId $ T.pack $ printf "[Gram Notifier] Wowowo staph %s! You've spammed with %d messages!" user_id_or_name (length spam_messages)
      else
        send_message grambotChatId $ T.pack $ printf "[Gram Notifier] User %s has spammed channel %s with %d messages!" user_id_or_name (show chat_id) (length spam_messages)

      case find (== u) (map _nUserId notifications_in_effect) of
        Just _ ->
          pure Nothing
        Nothing ->
          Just . Notification u <$> getCurrentTime

    message_state :: M.Map UserId TDLibUser.User -> [TDLib.Message] -> M.Map String (M.Map Int64 Int)
    message_state users =
          map (uncurry fromMaybe . ((show &&& fmap TDLibUser.first_name . flip M.lookup users) . UserId . TDLib.sender_user_id) &&& (:[]))
      -- [(String, [Message])]
      >>> M.fromListWith (<>)
      -- M.Map UserId [Message]
      >>> M.map (
            -- [Message]
            map (TDLib.chat_id &&& const 1)
            -- [(Int64, Int)]
        >>> M.fromListWith (+)
            -- M.Map Int64 Int
      )
      -- M.Map UserId (M.Map Int64 Int)

    process_updates :: IORef [TDLib.Message] -> IORef (M.Map UserId TDLibUser.User) -> IO ()
    process_updates messages users = do
      chan <- sub
      forever $
        PC.readChan chan >>= \case
          TDLib.UpdateNewMessage message _ _ -> do
            putStrLn "Adding message!"
            atomicModifyIORef' messages $ \xs -> (message:xs, ())
          TDLib.UpdateUser user -> do
            putStrLn "Adding user!"
            atomicModifyIORef' users $ \m -> (M.insert (UserId (TDLibUser.id user)) user m, ())
          _ ->
            pure ()

main :: IO ()
main = do
  LoadEnv.loadEnv
  (telegram_ctrl, destroy_telegram_ctrl) <- mkTelegramController
  spam_detector <- async $ mkSpamDetector telegram_ctrl
  let workers = [spam_detector]
  void $ installHandler sigINT (Catch (for_ workers cancel *> destroy_telegram_ctrl)) Nothing
  void $ waitAny workers
  putStrLn "A thread is terminated, exiting."
