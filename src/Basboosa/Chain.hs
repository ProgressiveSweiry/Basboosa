{-# LANGUAGE JavaScriptFFI, PackageImports, OverloadedStrings, GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances, TypeSynonymInstances, DeriveTraversable, DeriveGeneric, FlexibleInstances  #-}

module Basboosa.Chain where

import Basboosa.Types
--import Basboosa.PubKey
--import qualified Basboosa.Ledger as L

import Control.Comonad.Cofree

import Crypto.Hash 

import Data.Binary as B
import qualified Data.ByteString.Char8 as C
import Data.Time.Clock.POSIX

import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS


-- Print

printBlockchainTxs :: Blockchain -> IO ()
printBlockchainTxs (block :< Genesis) = do
  print $ _txs block
printBlockchainTxs (block :< Node header parent) = do 
  print $ _txs block
  printBlockchainTxs parent

blockchainToTxString :: Blockchain -> String
blockchainToTxString (block :< Genesis) = show block
blockchainToTxString (block :< Node header parent) = show block ++ "  " ++ show header ++ "--------" ++ (blockchainToTxString parent)

-- Genesis

makeGenesis :: IO Blockchain
makeGenesis = do
  return (Block ([]) :< Genesis)

makeGenesisFund :: Address -> IO Blockchain
makeGenesisFund addr = do
  return (Block [addrFund] :< Genesis)
    where
      addrFund = Transaction {
        _from   = Account $ C.pack "A_Genesis",
        _to     = addr ,
        _amount = 1000000000000,
        _text   = "Genesis Block - Distribution Of Funds",
        _fee    = 0 ,
        _id     = Hash $ "0"
      }

--Storage 
-- TODO: Create Storage Function To Files
{-
saveToLocalStorage :: Blockchain -> IO()
saveToLocalStorage bc = do 
  let s = S.localStorage
  S.setItem (JSS.pack "BlockChainEncoded") (JSS.pack $ CL.unpack $ B.encode bc) s

loadFromLocalStorage :: IO Blockchain
loadFromLocalStorage = do 
  let s = S.localStorage
  i <- S.getItem (JSS.pack "BlockChainEncoded") s
  let i1 = ( CL.pack $ JSS.unpack $ maybe (JSS.pack "\NUL") (\x -> x) i )
  i2 <- decodeToChain i1
  return i2
-}


decodeToChain :: CL.ByteString -> IO Blockchain
decodeToChain = return . B.decode


-- Chaining
hashToString :: String -> String
hashToString = byteToStringH . C.pack


byteToStringH :: C.ByteString -> String
byteToStringH bs = show (hash bs :: Digest SHA256)


-- Get Header At 0 returns Header Of Latest Block
-- Higher Integer To Get Earlier Blocks
getHeaderAt :: Blockchain -> Integer -> BlockHeader
getHeaderAt (_ :< Genesis) 0 = BlockHeader {
          _miner = Address $ C.pack "0",
          _parentHash = Hash $ "0000",
          _nonce = 0,
          _minedAt = 0,
          _blockNumber = 0
        }
getHeaderAt (_ :< Node header _) 0 = header
getHeaderAt (_ :< Node _ parent) n = getHeaderAt parent $ (n-1)

-- Like getHeaderAt For Transactions
getTxsAt :: Blockchain -> Integer -> TransactionPool
getTxsAt (_ :< Genesis) _ = return []
getTxsAt (block :< _) 0 = return $ _txs block
getTxsAt (_ :< Node _ parent) i = getTxsAt parent $ (i - 1)

-- Iterating through the Blockchain to get number of blocks
getChainLength :: Blockchain -> Integer -> IO Integer
getChainLength (_ :< Genesis) i = return i
getChainLength (_ :< Node _ parent) i = getChainLength parent (i+1)

-- Calculates the sum of fees on blockchain txs
getChainFees :: Blockchain -> Integer -> Integer
getChainFees (_ :< Genesis) i = i
getChainFees (block :< Node _ parent) i = getChainFees parent ((blockFees block) + i)
  where
    blockFees b = loop 0 $ _txs b
    loop a [] = a
    loop a (x:xs) = loop (a + (_fee x)) xs


-- TODO: Fix Filters:
{-
filterSignedTransactions :: SignedTransactionPool -> TransactionPool
filterSignedTransactions signedTxPool = do
  s <- signedTxPool
  return $ loopCheck s
    where
      loopCheck [] = []
      loopCheck (x:xs) = if verify x then (fst x) : loopCheck xs else loopCheck xs

      verify x = L.verifyTransactionXY (fst x) (pub $ fst x) (snd x)

      pub = convertTransactionToPublicKey

-- Check Every Account For Adequate Balance In Transaction
filterInadequteBalanceTransaction :: Blockchain -> TransactionPool -> TransactionPool
filterInadequteBalanceTransaction chain tx = do 
  txList <- tx
  let ledgerList = L.buildLedgerList chain
  return $ loopTxs ledgerList txList
    where
      loopTxs _ [] = []
      loopTxs l (x : xs) = if verifyBalance l x then x : (loopTxs l xs) else loopTxs l xs

      verifyBalance l x = if (_from x == (Account $ C.pack "REWARD")) then True else (L.accountBalance $ L.filterLedgerToAccount (_from x) l) >= ((_amount x ) + (_fee x)) && (_amount x > 0 ) && (_to x) /= (Address $ C.empty)                                             
-}


checkReward :: Blockchain -> Bool   
checkReward (_ :< Genesis) = True
checkReward (block :< Node header parent) = (isMinerInHeader (_txs block) header) && checkReward parent
  where
    isMinerInHeader [] _ = True
    isMinerInHeader (x:xs) h = if (_from x) == (Account $ C.pack "REWARD") then ( _to x) == (_miner h) && isMinerInHeader xs h else True && isMinerInHeader xs h

hashBlockchain :: Blockchain -> Hash
hashBlockchain = Hash . hashToString . CL.unpack . B.encode

checkBlockchainHash :: Blockchain -> Bool
checkBlockchainHash (_ :< Genesis) = True
checkBlockchainHash (_ :< Node header parent) = (hashBlockchain parent) == (_parentHash header)


-- Miner -> IO Transactions -> Blockchain -> updated Blockchain (IO)
mineBlock :: Account -> SignedTransactionPool -> Blockchain -> IO Blockchain
mineBlock acc signedTxPool parentChain = do
  -- Filtering TransactionPool and parent Blockchain
  if checkReward parentChain && checkBlockchainHash parentChain then do
    txs <- filterInadequteBalanceTransaction parentChain $ filterSignedTransactions $ (\x -> take blockTXs x) <$> signedTxPool
    -- Getting Current Time:
    now <- getPOSIXTime
    parentBlockNum <- getChainLength parentChain 0
    let currentBlockNum = parentBlockNum + 1
    loop txs 0 now currentBlockNum   
  else return ((Block []) :< Genesis)
        where
        --Difficulty Check:
        checkValidation c = (Prelude.take 1 $ hashToString $ CL.unpack $ B.encode c) == "0"
        --Hash of parent blockchain:
        parentHash = Hash $ hashToString $ CL.unpack $ B.encode parentChain
        --Generates Miner Address For Miner Account
        minerAddress = convertAccountToMinerAddress acc
        --Reward Tx:
        minerRewardTx = Transaction {_from = (Account $ C.pack "REWARD") , _to = minerAddress , _amount = minerReward, _text = "REWARD", _fee = 0, _id = Hash "0000" } 
        --Mining Loop:
        loop tx nonce n currentBlockNum = do
          -- Constructing BlockHeader:
          let header = BlockHeader {
            _miner = minerAddress,
            _parentHash = parentHash,
            _nonce = nonce,
            _minedAt = n,
            _blockNumber = currentBlockNum
          }
          let chain = ((Block $ minerRewardTx : tx) :< Node header parentChain)
          if checkValidation chain
            then return chain
            else loop tx (nonce + 1) n currentBlockNum 









































