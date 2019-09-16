{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ConstraintKinds            #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Spam (detectSpams, Spam (..)) where

import           Control.Applicative        (liftA2)
import           Control.Arrow              ((&&&), (>>>))
import           Control.Concurrent.Timeout
import           Control.Lens               hiding (( # ), (<<%=))
import           Control.Lens.Combinators   (both, over)
import           Control.Lens.Operators     ((%=))
import           Control.Monad              (forever, (>=>))
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Retry
import           Data.Either                (isLeft)
import           Data.Foldable              (for_)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Debug.Trace                (trace)
import           Utils
import Data.List (partition)
import qualified Telegram.Database.API.Messages as TDLib
import Data.Int (Int32)
import Control.Arrow
import Data.Time.Clock

data Spam = Spam Int32 [TDLib.Message]

messageStalenessDuration :: NominalDiffTime
messageStalenessDuration = nominalDay

messageSpamWindow :: NominalDiffTime
messageSpamWindow = nominalDay

spamMaxMessages :: Num a => a
spamMaxMessages = 10

messageDate :: TDLib.Message -> UTCTime
messageDate = posixSecondsToUTCTime . fromIntegral . TDLib.date

detectSpams :: UTCTime -> [TDLib.Message] -> ([TDLib.Message], [Spam])
detectSpams curr_time = map (TDLib.sender_user_id &&& (:[]))
          >>> M.fromListWith (<>)
          >>> M.foldlWithKey' decide ([], [])
  where
    decide :: ([TDLib.Message], [Spam]) -> Int32 -> [TDLib.Message] -> ([TDLib.Message], [Spam])
    decide (non_spams, spams) user messages =
      let
        (_stale_messages, in_scope_messages) = partition message_stale messages
        (recent_messages, older_messages) = partition message_recent in_scope_messages
      in
        if length recent_messages > spamMaxMessages then
          (non_spams ++ older_messages, Spam user recent_messages : spams)
        else
          (non_spams ++ recent_messages ++ older_messages, spams)

    message_stale :: TDLib.Message -> Bool
    message_stale message =
      diffUTCTime curr_time (messageDate message) > messageStalenessDuration

    message_recent :: TDLib.Message -> Bool
    message_recent message =
      diffUTCTime curr_time (messageDate message) > messageSpamWindow
