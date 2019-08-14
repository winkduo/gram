{-# LANGUAGE OverloadedStrings #-}

module Wrap where

import Network.Curl

type Token = String

getUpdates :: URLString -> Token -> IO ()
getUpdates api bot_token = curlGet (api ++ bot_token ++ "/getUpdates") []

sendMessage :: URLString -> Token -> Token -> String ->  IO ()
sendMessage api bot_token chat_id msg = curlPost (api ++ bot_token ++ "/sendMessage") ["chat_id=" ++ chat_id, "text=" ++ msg]

getHistory api bot_token chat_id = curlGet (api ++ bot_token ++ "/getHistory" ++ " -d chat_id=" ++ chat_id) []
