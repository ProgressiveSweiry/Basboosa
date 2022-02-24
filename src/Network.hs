{-# LANGUAGE OverloadedStrings #-}

module Basboosa.Network where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

mainNetwork :: IO ()
mainNetwork = do
    run 8080 app


app :: Application
app _ respond = do
    putStrLn "I've done some IO here"
    respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, Web!"




