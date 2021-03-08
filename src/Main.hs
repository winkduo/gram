module Main
  ( main,
  )
where

------------------------------------------------------------------------------

import Control.Concurrent.Async (async, cancel, waitAny)
import Control.Monad (void)
import Data.Foldable (for_)
import Gram.Options (CommandLineOptions (..), getCommandLineOptions)
import Gram.Spam.Worker (mkSpamDetector)
import Gram.Telegram (mkTelegramController)
import LoadEnv (loadEnv)
import qualified System.Posix.Signals as SPS

------------------------------------------------------------------------------

main :: IO ()
main = do
  loadEnv
  CommandLineOptions {..} <- getCommandLineOptions
  (telegram_ctrl, destroy_telegram_ctrl) <- mkTelegramController
  spam_detector <- async $ mkSpamDetector _commandLineOptions_spamOptions telegram_ctrl
  let workers = [spam_detector]
  void $ SPS.installHandler SPS.sigINT (SPS.Catch $ for_ workers cancel >> destroy_telegram_ctrl) Nothing
  void $ waitAny workers
  putStrLn "A thread is terminated, exiting."
