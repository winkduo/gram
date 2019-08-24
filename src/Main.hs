{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import qualified Data.Text as T
import qualified Web.Telegram.API.Bot as Tgrm
import           Data.Functor (void)
import Data.Maybe
import Control.Applicative (liftA2)
import Servant.Client.Core.ClientError (ClientError)
import Control.Concurrent.Async (async, wait)
import Spam (mkSpamDetector, SpamResult)

token :: Tgrm.Token
token = Tgrm.Token "<token>"

runTgrm :: Tgrm.TelegramClient a -> IO (Either ClientError a)
runTgrm f = do
  manager <- newManager tlsManagerSettings
  Tgrm.runClient f token manager

getMessages :: IO (Either ClientError [Tgrm.Message])
getMessages = runTgrm $ do
  r1 <- Tgrm.getUpdatesM (Tgrm.GetUpdatesRequest Nothing Nothing Nothing Nothing)
  return $ mapMaybe Tgrm.message . Tgrm.result $ r1

chatId :: Tgrm.ChatId
chatId = Tgrm.ChatId (-1001330413002)

sendMessage :: T.Text -> IO ()
sendMessage message = void $ runTgrm $
  Tgrm.sendMessageM $ Tgrm.SendMessageRequest chatId message Nothing Nothing Nothing Nothing Nothing

main :: IO ()
main = do
  spam_detector <- async $ mkSpamDetector getMessages sendMessage
  wait spam_detector
  putStrLn "Every thread is terminated."
