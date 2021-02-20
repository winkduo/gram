module Main (main) where

import Control.Concurrent.Async (async, cancel, waitAny)
import Data.Foldable (for_)
import Data.Functor (void)
import qualified LoadEnv
import Options (Options (Options), askOptions)
import Spam.Worker (mkSpamDetector)
import System.Posix.Signals
  ( Handler (Catch),
    installHandler,
    sigINT,
  )
import Telegram (mkTelegramController)

main :: IO ()
main = do
  LoadEnv.loadEnv
  Options spam_options <- askOptions
  (telegram_ctrl, destroy_telegram_ctrl) <- mkTelegramController
  spam_detector <- async $ mkSpamDetector spam_options telegram_ctrl
  let workers = [spam_detector]
  void $ installHandler sigINT (Catch (for_ workers cancel *> destroy_telegram_ctrl)) Nothing
  void $ waitAny workers
  putStrLn "A thread is terminated, exiting."
