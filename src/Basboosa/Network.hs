{-# LANGUAGE OverloadedStrings #-}

module Basboosa.Network where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import qualified Data.ByteString.Lazy.Char8 as BSLC

import Basboosa.Chain
import Basboosa.Ledger

mainNetwork :: IO ()
mainNetwork = do
    run 8080 app


app :: Application
app _ respond = do
    putStrLn "I've done some IO here"
    bc <- loadChain
    respond $ responseLBS status200 [("Content-Type", "text/plain")] $ BSLC.pack $ show $ buildLedgerList bc




