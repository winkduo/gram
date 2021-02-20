module Spam.Options (SpamOptions (SpamOptions, _soAllowedChatIds, _soTestChatId, _soSpamCheckInterval, _soMessageSpamWindow, _soSpamMaxMessages, _soNotificationReset), parseSpamOptions) where

import Data.Duration (Duration, DurationUnit (Second), (#))
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import Data.Time.Clock (NominalDiffTime)
import Options.Applicative
  ( Parser,
    auto,
    long,
    metavar,
    option,
    short,
    showDefault,
    strOption,
    value,
  )
import Telegram (ChatId (ChatId))
import Text.Read (readMaybe)

data SpamOptions = SpamOptions
  { _soAllowedChatIds :: [ChatId],
    _soTestChatId :: ChatId,
    _soSpamCheckInterval :: Duration,
    _soMessageSpamWindow :: NominalDiffTime,
    _soSpamMaxMessages :: Int,
    _soNotificationReset :: Duration
  }

parseSpamOptions :: Parser SpamOptions
parseSpamOptions =
  SpamOptions
    <$> chat_ids
    <*> test_chat_id
    <*> spam_check_interval
    <*> message_spam_window
    <*> spam_max_messages
    <*> notification_reset
  where
    chat_ids =
      mapMaybe (fmap ChatId . readMaybe) . words
        <$> strOption
          ( long "allowed_chat_ids" <> value [] <> short 'a' <> metavar "CHAT_ID_LIST" <> showDefault
          )
    test_chat_id =
      ChatId
        <$> option
          auto
          ( long "test_chat_id" <> short 't' <> metavar "CHAT_ID"
          )
    spam_check_interval =
      (# Second)
        <$> option
          auto
          ( long "spam_check_interval" <> short 's' <> metavar "DURATION_SEC" <> value 10 <> showDefault
          )
    message_spam_window =
      (fromIntegral :: Integer -> NominalDiffTime)
        <$> option
          auto
          ( long "message_spam_window" <> short 'w' <> metavar "DURATION_SEC" <> value 60 <> showDefault
          )
    spam_max_messages =
      option
        auto
        ( long "spam_max_messages" <> short 'm' <> metavar "COUNT" <> value 10 <> showDefault
        )
    notification_reset =
      (# Second)
        <$> option
          auto
          ( long "notification_reset" <> short 'n' <> metavar "DURATION_SEC" <> value (60 * 60) <> showDefault
          )
