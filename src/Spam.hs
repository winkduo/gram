{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Spam (mkSpamDetector, SpamResult) where

import           Control.Applicative        (liftA2)
import           Control.Arrow              ((&&&), (>>>))
import           Control.Concurrent.Timeout
import           Control.Lens               hiding (( # ))
import           Control.Lens.Combinators   (both, over)
import           Control.Lens.Operators     ((%=))
import           Control.Monad              (forever, (>=>))
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Retry
import           Data.Either                (isLeft)
import           Data.Foldable              (for_)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Data.Time.Clock
import           Debug.Trace                (trace)
import           Utils
import qualified Web.Telegram.API.Bot       as Tgrm

-- * Types
newtype SpamM a = SpamM { runSpamM :: StateT SpamState (ExceptT SpamError IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError SpamError, MonadState SpamState)

data Spam = Spam
  { _spamUser     :: T.Text
  , _spamMessages :: [Tgrm.Message]
  } deriving (Show)

type SpamPredicate = [Tgrm.Message] -> Bool

newtype SpamResult = SpamResult [Spam]
newtype SpamError = SpamError T.Text

newtype SpamState = SpamState
  { _spamNotifications :: [(T.Text, UTCTime)]
  } deriving Show
makeLenses ''SpamState

-- * Parameters
spamInterval :: NominalDiffTime
spamInterval = 60 * 60 * 2 -- 2 hours

minConsecutiveMessages :: Num a => a
minConsecutiveMessages = 3

consecutiveMessageTimeWindow :: NominalDiffTime
consecutiveMessageTimeWindow = 3 * nominalDay

pollingInterval :: Duration
pollingInterval = 10 # Seconds

runningCheck :: Int -> (NE.NonEmpty a -> Bool) -> [a] -> Bool
runningCheck window p xs =
  case splitAt window xs of
    ([], _) ->
      False
    (c : cx, []) ->
      p (c NE.:| cx)
    (c : cx, r : rx) ->
      p (c NE.:| cx) || runningCheck window p (cx ++ [r])

consecutiveMessages :: SpamPredicate
consecutiveMessages =
  runningCheck minConsecutiveMessages $ \messages ->
    let (date_fst, date_lst) = over both (fromIntegral . Tgrm.date) $ NE.head &&& NE.last $ messages in
    date_lst - date_fst < consecutiveMessageTimeWindow

-- | Retries an IO action that can return an error using exponential backoff forever.
withExponentialBackoff
  :: MonadIO m
  => m (Either a b)
  -> m (Either a b)
withExponentialBackoff =
  retrying
    (limitRetries 120 <> capDelay (durationUs $ 30 # Seconds) (exponentialBackoff 1000000))
    (const (pure . isLeft))
  . const

detectSpam :: [Tgrm.Message] -> [Spam]
detectSpam = mapByMaybe (fmap Tgrm.user_first_name . Tgrm.from)
         >>> M.filter consecutiveMessages
         >>> M.foldrWithKey (((:) .) . Spam) []

initSpamState :: SpamState
initSpamState = SpamState []

mkSpamDetector :: Show err => IO (Either err [Tgrm.Message]) -> (T.Text -> IO ()) -> IO ()
mkSpamDetector get_messages send_message = do
  putStrLn "Starting spam detector thread..."
  void $
    runExceptT $
    flip execStateT initSpamState $
    forever $
      runSpamM $ do
        run_spam_detector
        liftIO $ threadDelay pollingInterval
  where
    run_spam_detector :: SpamM ()
    run_spam_detector = do
      spam_state <- get
      liftIO $ putStrLn "Running spam detector."
      liftIO $ print spam_state
      spams <- detectSpam <$> read_messages
      evict_outdated_spam_notifications
      liftIO $ print spams
      notified_users <- map fst <$> gets _spamNotifications
      for_ (filter (not . (`elem` notified_users) . _spamUser) spams) $ \(Spam user _messages) -> do
        liftIO $ send_message $ "Hey @" <> user <> ", you've spammed!"
        add_user_to_notified_users user

    add_user_to_notified_users :: (MonadIO m, MonadState SpamState m) => T.Text -> m ()
    add_user_to_notified_users user = do
      curr_time <- liftIO getCurrentTime
      spamNotifications %= ((user, curr_time):)

    evict_outdated_spam_notifications :: (MonadIO m, MonadState SpamState m) => m ()
    evict_outdated_spam_notifications = do
      curr_time <- liftIO getCurrentTime
      spamNotifications %= filter ((< spamInterval) . diffUTCTime curr_time . snd)

    read_messages :: SpamM [Tgrm.Message]
    read_messages =
      liftIO (withExponentialBackoff get_messages) >>= \case
        Left err ->
          throwError $ SpamError (T.pack $ show err)
        Right result ->
          pure result
