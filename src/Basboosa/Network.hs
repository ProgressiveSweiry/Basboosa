{-# LANGUAGE OverloadedStrings #-}

module Basboosa.Network where

import Network.Wai hiding (defaultRequest, requestBody)
import Network.HTTP.Types
import Network.HTTP.Client
import Network.HTTP.Simple
import Network.Wai.Handler.Warp (run)

import Data.Binary as B
import Data.ByteString.Lazy.Char8 hiding (putStrLn)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import qualified Data.ByteString.Char8 as BS

import Basboosa.Types
import Basboosa.Chain
import Basboosa.Ledger
import Basboosa.PubKey

{-
TODO:
Send:
- Genesis Tx - V
- Full Blockchain - V
- Full Txs - V
- Full Headers - V
Recieve Request:
- Specific Txs, Headers
- Add Transactions
- Add Block (To Existing Blockchain)
Network List:
- Create List Of Nodes
- Share List Of Nodes
-}

mainNetwork :: IO ()
mainNetwork = do
    run 8080 nodeApp

{-
app :: Application
app request respond = do
    putStrLn "I've done some IO here"
    let m = ("Message" :: ByteString)
    r <- strictRequestBody request
    if r == (B.encode ReqTxList) then do
        bc <- loadChain
        respond $ responseLBS status200 [("Content-Type", "text/plain")] $ B.encode $ ResTxList $ getTxFull bc
        else
            respond $ responseLBS status200 [("Content-Type", "text/plain")] $ pack "ERROR"
-}


nodeApp :: Application
nodeApp request respond = do
    r <- strictRequestBody request
    res <- constructRespond $ B.decode r
    respond $ responseLBS status200 [("Content-Type", "text/plain")] $ B.encode res

constructRespond :: NodeRequest -> IO NodeRespond
constructRespond req = case req of
    ReqFullBlockchain -> do
        fbc <- fullBlockchain
        return (ResFullBlockchain fbc)
    ReqNewBlock block header -> do
        bc <- loadChain
        newBC <- mergeNewBlock block header bc
        if (hashBlockchain bc) == (hashBlockchain newBC) -- TODO : CHECK IT!!!!!!!!!!!!!!!!!!!!!!!!!!!
            then
                return (ResError)
            else
                return (ResNewBlock)
    ReqNewTx stx -> do 
        verifyStx stx
        return (ResNewTx) --VERIFY SIGNATURE AND ADD TO QUEUE
    ReqTxList -> do
        bc <- loadChain
        return (ResTxList $ getTxFull bc)
    

sendReq :: IO ()
sendReq = do 
    tx <- loadSignedTx
    let request = defaultRequest {
        method = "POST",
        host = encodeUtf8 $ T.pack "localhost",
        port = 8080,
        requestBody = RequestBodyLBS (B.encode $ ReqNewTx $ convertSignedTxToInteger tx)
        }
    resBs <- httpLBS request
    print $ show (B.decode $ getResponseBody resBs :: NodeRespond)

------------------------------------------------

verifyStx :: SignedTxInteger -> IO ()
verifyStx stxi = do
    bc <- loadChain
    stx <- convertSignedTxFromInteger stxi
    let stxL = stx : []
    txs <- filterSignedTransactions (return stxL)
    print $ show txs


-- Get -------------------------------

genesisTx :: IO ByteString
genesisTx = do
    bc <- loadChain
    return $ pack $ show $ getTxsAt bc $ getChainLength bc 0  

fullBlockchain :: IO ByteString
fullBlockchain = do
    bc <- loadChain
    return $ B.encode bc


fullTXs :: IO ByteString
fullTXs = do
    bc <- loadChain
    return $ pack $ blockchainToTxString bc


fullHeader :: IO ByteString
fullHeader = do
    bc <- loadChain
    return $ pack $ blockchainToHeaderString bc

