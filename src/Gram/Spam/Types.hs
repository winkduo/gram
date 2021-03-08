-- |
module Gram.Spam.Types
  ( -- *
    Spam (..),
    Notification (..),
  )
where

------------------------------------------------------------------------------

import Data.Time.Clock (UTCTime)
import Gram.Types
import qualified Telegram.Database.API.Messages as TDLib

------------------------------------------------------------------------------

-- |
data Spam = Spam UserId ChatId [TDLib.Message]

------------------------------------------------------------------------------

-- |
data Notification = Notification
  { _nUserId :: UserId,
    _nDate :: UTCTime
  }
