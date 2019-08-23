{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot
import Data.Maybe

token :: Token
token = Token "token"

getMessages = do
  manager <- newManager tlsManagerSettings
  r1 <- getUpdates token Nothing Nothing Nothing manager
  return $ mapMaybe text . mapMaybe message . result <$> r1

main :: IO ()
main = do
  print $ "wow"

--  void $ runTelegramClient token manager $ do
--    sendMessageM (SendMessageRequest (ChatChannel "-1001330413002") "" Nothing Nothing Nothing Nothing Nothing)

