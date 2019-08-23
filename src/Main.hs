{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot
import Data.Maybe
import Servant.Client.Core.Internal.Request

token :: Token
token = Token "<token>"

getMessages ::  IO (Either ServantError [Message]) 
getMessages = do
  manager <- newManager tlsManagerSettings
  r1 <- getUpdates token Nothing Nothing Nothing manager
  return $ mapMaybe message . result <$> r1

main :: IO ()
main = do
  print $ "wow"
--  void $ runTelegramClient token manager $ do
--    sendMessageM (SendMessageRequest (ChatChannel "-1001330413002") "" Nothing Nothing Nothing Nothing Nothing)

