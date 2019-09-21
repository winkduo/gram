{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spam.Detector (detectSpams, Spam (..)) where

import           Control.Arrow                  ((&&&), (>>>))
import           Data.List                      (partition)
import qualified Data.Map                       as M
import           Data.Time.Clock                (UTCTime, diffUTCTime)
import           Data.Time.Clock.POSIX          (posixSecondsToUTCTime)
import           Spam.Options                   (SpamOptions (SpamOptions, _soMessageSpamWindow, _soSpamMaxMessages))
import           Telegram                       (ChatId (ChatId),
                                                 UserId (UserId))
import qualified Telegram.Database.API.Messages as TDLib

data Spam = Spam UserId ChatId [TDLib.Message]

messageDate :: TDLib.Message -> UTCTime
messageDate = posixSecondsToUTCTime . fromIntegral . TDLib.date

detectSpams :: SpamOptions -> UTCTime -> [TDLib.Message] -> ([TDLib.Message], [Spam])
detectSpams SpamOptions { _soMessageSpamWindow, _soSpamMaxMessages } curr_time =
        -- [Message]
        map (UserId . TDLib.sender_user_id &&& (:[]))
        -- [(UserId, [Message]]
    >>> M.fromListWith (<>)
        -- M.Map UserId [Message]
    >>> M.map (
          -- [Message]
          map (ChatId . TDLib.chat_id &&& (:[]))
          -- [(ChatId, Message)]
    >>>   M.fromListWith (<>)
          -- M.Map ChatId Int
    )
    -- M.Map UserId (M.Map ChatId [Message])
    >>> find_spams
    -- ([TDLib.Message], [Spam])
  where
    find_spams :: M.Map UserId (M.Map ChatId [TDLib.Message]) -> ([TDLib.Message], [Spam])
    find_spams = M.foldlWithKey' (\(all_messages, all_spams) user_id chat_to_messages ->
            let (user_messages, user_spams) = find_users_spam user_id chat_to_messages in
            (all_messages ++ user_messages, all_spams ++ user_spams)
          ) ([], [])

    find_user_chat_spams :: UserId -> ChatId -> [TDLib.Message] -> ([TDLib.Message], [Spam])
    find_user_chat_spams user_id chat_id messages =
      let
        (recent_messages, older_messages) = partition message_recent messages
      in
        if length recent_messages > _soSpamMaxMessages then
          (older_messages, [Spam user_id chat_id recent_messages])
        else
          (recent_messages ++ older_messages, [])

    find_users_spam :: UserId -> M.Map ChatId [TDLib.Message] -> ([TDLib.Message], [Spam])
    find_users_spam user_id =
      M.foldlWithKey' (\(non_spams, spams) chat_id messages ->
        let (user_chat_nonspams, user_chat_spams) = find_user_chat_spams user_id chat_id messages
        in (non_spams ++ user_chat_nonspams, spams ++ user_chat_spams)
      ) ([], [])

    message_recent :: TDLib.Message -> Bool
    message_recent message =
      diffUTCTime curr_time (messageDate message) < _soMessageSpamWindow
