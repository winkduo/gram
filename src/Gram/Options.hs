-- |
module Gram.Options
  ( -- *
    CommandLineOptions (..),

    -- *
    getCommandLineOptions,
  )
where

------------------------------------------------------------------------------

import Gram.Spam.Options (SpamOptions, parseSpamOptions)
import Options.Applicative (Parser, briefDesc, execParser, info)

------------------------------------------------------------------------------

-- | All the different commands gram offers through the CLI.
newtype CommandLineOptions = CommandLineOptions
  { _commandLineOptions_spamOptions :: SpamOptions
  }

-- | Parse the CLI input to get a command to execute.
getCommandLineOptions :: IO CommandLineOptions
getCommandLineOptions =
  execParser $ info parseCommandLineOptions briefDesc
  where
    parseCommandLineOptions :: Parser CommandLineOptions
    parseCommandLineOptions =
      CommandLineOptions
        <$> parseSpamOptions
