module Spam.Debug (showSpamState) where

import Control.Arrow ((&&&), (>>>))
import qualified Data.Aeson as JSON
import Data.Int (Int64)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Telegram (UserId (UserId))
import qualified Telegram.Database.API.Messages as TDLib
import qualified Telegram.Database.API.User as TDLibUser

showSpamState ::
  M.Map UserId TDLibUser.User ->
  [TDLib.Message] ->
  String
showSpamState u n = show (JSON.encode (message_state u n))
  where
    message_state :: M.Map UserId TDLibUser.User -> [TDLib.Message] -> M.Map String (M.Map Int64 Int)
    message_state users =
      map (uncurry fromMaybe . ((show &&& fmap TDLibUser.first_name . flip M.lookup users) . UserId . TDLib.sender_user_id) &&& (: []))
        -- [(String, [Message])]
        >>> M.fromListWith (<>)
        -- M.Map UserId [Message]
        >>> M.map
          ( -- [Message]
            map (TDLib.chat_id &&& const 1)
              -- [(Int64, Int)]
              >>> M.fromListWith (+)
              -- M.Map Int64 Int
          )

-- M.Map UserId (M.Map Int64 Int)
