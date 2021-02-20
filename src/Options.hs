module Options (askOptions, Options (..)) where

import Options.Applicative (Parser, briefDesc, execParser, info)
import Spam.Options (SpamOptions, parseSpamOptions)

newtype Option = Options
  { _oSpamOptions :: SpamOptions
  }

askOptions :: IO Options
askOptions =
  execParser $ info parse_options briefDesc
  where
    parse_options :: Parser Options
    parse_options =
      Options
        <$> parseSpamOptions
