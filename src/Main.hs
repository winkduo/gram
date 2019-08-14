module Main where

import Wrap

import System.Environment   

api = "https://api.telegram.org/"
  

main :: IO ()
main = do
  let bot_token = " "
  let chat_id   = " "

  getHistory api bot_token chat_id
--  serve
--  getUpdates api bot_token
--  sendMessage api bot_token chat_id "yo"
