{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot
import Control.Arrow
import Data.Maybe
import Data.Functor (void)

token :: Token
token = Token "<token>"

fromJust (Just x) = x

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  void $ runTelegramClient token manager $ do
    sendMessageM (SendMessageRequest (ChatChannel "-1001330413002") "yo" Nothing Nothing Nothing Nothing Nothing)
