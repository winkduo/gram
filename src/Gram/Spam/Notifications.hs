{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Gram.Spam.Notifications
  ( Notification,
    notifySpams,
  )
where

------------------------------------------------------------------------------

import Control.Arrow ((&&&))
import Data.Duration (Duration (Duration))
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock
  ( NominalDiffTime,
    UTCTime,
    addUTCTime,
    getCurrentTime,
  )
import Data.Traversable (for)
import Gram.Spam.Options (SpamOptions (SpamOptions, _soAllowedChatIds, _soNotificationReset, _soTestChatId))
import Gram.Spam.Types
import Gram.Types (ChatId (..), UserId (..))
import qualified Telegram.Database.API.User as TDLibUser
import Text.Printf (printf)

------------------------------------------------------------------------------
-- Yigit - Alp private channel
_yigitAlpChatId :: ChatId
_yigitAlpChatId = ChatId 123456789

------------------------------------------------------------------------------
-- private GramBot
_grambotChatId :: ChatId
_grambotChatId = ChatId 12345679

------------------------------------------------------------------------------

-- |
toNDT :: Duration -> NominalDiffTime
toNDT (Duration us) = fromIntegral $ us `div` (10 ^ (6 :: Int))

------------------------------------------------------------------------------

-- |
notifySpams ::
  SpamOptions ->
  [Spam] ->
  (ChatId -> T.Text -> IO ()) ->
  M.Map UserId TDLibUser.User ->
  UTCTime ->
  [Notification] ->
  IO [Notification]
notifySpams SpamOptions {_soTestChatId, _soAllowedChatIds, _soNotificationReset} spams send_message users time (filter ((> time) . addUTCTime (toNDT _soNotificationReset) . _nDate) -> notifications) =
  catMaybes <$> for spams notify_spam
  where
    notify_spam :: Spam -> IO (Maybe Notification)
    notify_spam (Spam u@(UserId user_id) c@(ChatId chat_id) spam_messages) = do
      let user_id_or_name = uncurry fromMaybe $ (show &&& fmap TDLibUser.first_name . (`M.lookup` users)) $ UserId user_id
      if c `elem` _soAllowedChatIds
        then send_message c $ T.pack $ printf "[Gram Notifier] Wowowo staph %s! You've spammed with %d messages!" user_id_or_name (length spam_messages)
        else send_message _soTestChatId $ T.pack $ printf "[Gram Notifier] User %s has spammed channel %s with %d messages!" user_id_or_name (show chat_id) (length spam_messages)
      case find (== u) (map _nUserId notifications) of
        Just _ ->
          pure Nothing
        Nothing ->
          Just . Notification u <$> getCurrentTime
