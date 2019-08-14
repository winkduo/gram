module Main where

import Wrap

api = "https://api.telegram.org/"
bot_token = "bot605822040:AAFKOnPC7MnSbEOwNpTdKQG6hS_cVbAGwfE"
chat_id = "160758532"

main :: IO ()
main = do
  getHistory api bot_token chat_id
--  serve
--  getUpdates api bot_token
--  sendMessage api bot_token chat_id "yo"
