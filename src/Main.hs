{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import           Control.Applicative             (liftA2)
import           Control.Concurrent.Async        (async, wait, waitAny)
import           Data.Functor                    (void)
import           Data.Maybe
import qualified Data.Text                       as T
import           Network.HTTP.Client             (newManager)
import           Network.HTTP.Client.TLS         (tlsManagerSettings)
import           Servant.Client.Core.ClientError (ClientError)
import           Spam                            (SpamResult, mkSpamDetector)
import qualified Web.Telegram.API.Bot            as Tgrm
import Telegram
import qualified LoadEnv
import qualified Control.Concurrent.Privileged as PC
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import qualified Telegram.Database.API.Messages as TDLib

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

mkTelegramProcessor :: TelegramController IO TDLib.Message -> IO ()
mkTelegramProcessor (TelegramController sub) = do
  chan <- sub
  forever $ PC.readChan chan >>= print

main :: IO ()
main = do
  LoadEnv.loadEnv
  telegram_ctrl <- mkTelegramController
  telegram_processor <- async $ mkTelegramProcessor telegram_ctrl
  spam_detector <- async $ mkSpamDetector getMessages sendMessage
  void $ waitAny [spam_detector, telegram_processor]
  putStrLn "A thread is terminated, exiting."
