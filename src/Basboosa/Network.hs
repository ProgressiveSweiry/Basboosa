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

import Control.Comonad.Cofree

import Basboosa.Types
import Basboosa.Chain
import Basboosa.Ledger
import Basboosa.PubKey

{-
TODO:
Fix File Reading - V
Fix Duplicate Txs - Create List Of Tx ID TO Check Duplicates - V
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

-- Constants -------------------------

nPort :: Int
nPort = 8080

nHost :: String
nHost = "localhost"

-- Network ---------------------------

mainNetwork :: IO ()
mainNetwork = do
    run 8080 nodeApp


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
        if (hashBlockchain bc) == (hashBlockchain newBC)
            then 
                return (ResError)
            else do
                saveChain newBC
                return (ResNewBlock)
    ReqNewTx stx -> do 
        b <- verifyStx stx
        if b then return ResNewTx else return ResError -- TODO: Create Txs Queue
    ReqTxList -> do
        bc <- loadChain
        return (ResTxList $ getTxFull bc)
    
-- Send Requests -------------------------------

sendBlockchainReq :: IO Blockchain
sendBlockchainReq = do
    let request = defaultRequest {
        method = "POST",
        host = encodeUtf8 $ T.pack nHost,
        port = nPort,
        requestBody = RequestBodyLBS (B.encode $ ReqFullBlockchain)
        }
    resBs <- httpLBS request
    return $ (\(ResFullBlockchain bs) -> (B.decode bs :: Blockchain)) $ (B.decode $ getResponseBody resBs :: NodeRespond)

sendNewBlockReq :: Block -> BlockHeader -> IO NodeRespond
sendNewBlockReq block header = do
    let request = defaultRequest {
        method = "POST",
        host = encodeUtf8 $ T.pack nHost,
        port = nPort,
        requestBody = RequestBodyLBS (B.encode $ ReqNewBlock block header)
        }
    resBs <- httpLBS request
    return $ (B.decode $ getResponseBody resBs :: NodeRespond)

sendTxListReq :: TransactionPool
sendTxListReq = do
    let request = defaultRequest {
        method = "POST",
        host = encodeUtf8 $ T.pack nHost,
        port = nPort,
        requestBody = RequestBodyLBS (B.encode $ ReqTxList)
        }
    resBs <- httpLBS request
    return $ (\(ResTxList txs) -> txs) $ (B.decode $ getResponseBody resBs :: NodeRespond)


sendNewTxReq :: SignedTx -> IO ()
sendNewTxReq sTx = do 
    let request = defaultRequest {
        method = "POST",
        host = encodeUtf8 $ T.pack nHost,
        port = nPort,
        requestBody = RequestBodyLBS (B.encode $ ReqNewTx $ convertSignedTxToInteger sTx)
        }
    resBs <- httpLBS request
    print $ show (B.decode $ getResponseBody resBs :: NodeRespond)

------------------------------------------------

verifyStx :: SignedTxInteger -> IO Bool
verifyStx stxi = do
    bc <- loadChain
    stx <- convertSignedTxFromInteger stxi
    acc <- loadAccount
    let stxL = stx : []
    chain <- mineBlock acc (return stxL) bc
    printBlockchainTxs chain
    return $ not $ (hashBlockchain chain) == (hashBlockchain $ ((Block []) :< Genesis)) 


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

