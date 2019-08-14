{-# LANGUAGE OverloadedStrings #-}

module Playlist where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (status200)

import Network.Wai
import Network.Wai.Handler.Warp (run)

--import Web.Telegram.API.Bot

application _ respond = respond $
  responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

serve = run 3000 application

