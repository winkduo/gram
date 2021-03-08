{-# LANGUAGE DerivingStrategies #-}

-- |
module Gram.Types
  ( -- * Newtype wrappers for IDs
    ChatId (..),
    UserId (..),

    -- * Controller Types
    TelegramController (..),
    UpdateController (..),

    -- * Misc.
    TDAuthConfig (..),
  )
where

------------------------------------------------------------------------------

import qualified Control.Concurrent.Privileged as PC
import Data.Int (Int32, Int64)
import Data.Text (Text)

------------------------------------------------------------------------------

-- | Newtype wrapper for user ids.
newtype UserId = UserId {userId :: Int32}
  deriving stock (Eq, Show, Ord)

------------------------------------------------------------------------------

-- |
newtype ChatId = ChatId {chatId :: Int64}
  deriving stock (Eq, Show, Ord)

------------------------------------------------------------------------------

-- |
data TelegramController m update = TelegramController
  { _telegramController_subscribe :: m (PC.ReadOnlyChan update),
    _telegramController_send :: ChatId -> Text -> m ()
  }

------------------------------------------------------------------------------

-- |
data UpdateController m update = UpdateController
  { _updateController_publish :: update -> m (),
    _updateController_subscribe :: m (PC.ReadOnlyChan update)
  }

------------------------------------------------------------------------------

-- |
data TDAuthConfig = TDAuthConfig
  { _tdAuthConfig_apiId :: Int,
    _tdAuthConfig_apiHash :: String,
    _tdAuthConfig_phoneNumber :: Text
  }
  deriving stock (Eq, Show)
