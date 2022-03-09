{-# LANGUAGE OverloadedStrings #-}

module Basboosa.Chain where

import Basboosa.Types
import Basboosa.PubKey
import Basboosa.Ledger 

import System.IO
import Control.DeepSeq

import Control.Comonad.Cofree

import Crypto.Hash

import Data.Binary as B
import qualified Data.ByteString.Char8 as C
import Data.Time.Clock.POSIX

import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS


-- Print Blockchain --------------------------------------------------------

printBlockchainTxs :: Blockchain -> IO ()
printBlockchainTxs (block :< Genesis) = do
  print $ _txs block
  putStrLn "BLOCK #0" 
printBlockchainTxs (block :< Node header parent) = do
  print $ _txs block
  putStrLn $ "BLOCK #" ++ (show $ _blockNumber header)
  printBlockchainTxs parent

blockchainToTxString :: Blockchain -> String
blockchainToTxString (block :< Genesis) = show block
blockchainToTxString (block :< Node header parent) = show block ++ "  " ++ show header ++ "--------" ++ (blockchainToTxString parent)

blockchainToHeaderString :: Blockchain -> String
blockchainToHeaderString (_ :< Genesis) = " "
blockchainToHeaderString (_ :< Node header parent) = show header ++ "--------" ++ (blockchainToHeaderString parent)

-- Genesis ---------------------------------------------------------------------

makeGenesis :: IO Blockchain
makeGenesis = do
  return (Block ([]) :< Genesis)

makeGenesisFund :: Address -> IO Blockchain
makeGenesisFund addr = do
  return (Block [(addrFund, (0,0))] :< Genesis)
    where
      addrFund = Transaction {
        _from   = Account "A_Genesis",
        _to     = addr ,
        _amount = 1000000000000,
        _text   = "Genesis Block - Distribution Of Funds",
        _fee    = 0 ,
        _id     = Hash $ "0"
      }

-- Storage -----------------------------------------------------------------

saveChain :: Blockchain -> IO ()
saveChain bc = do
  handle <- openFile chainFile WriteMode
  hPutStr handle $ CL.unpack $ B.encode bc
  hClose handle
  

loadChain :: IO Blockchain
loadChain = do
  handle <- openFile chainFile ReadMode
  bc <- hGetContents handle
  bc `deepseq` hClose handle 
  return $ B.decode $ CL.pack bc

-- Blockchain Getters ---------------------------------------------------------

-- Get Header At 0 returns Header Of Latest Block
-- Higher Integer To Get Earlier Blocks
getHeaderAt :: Blockchain -> Integer -> BlockHeader
getHeaderAt (_ :< Genesis) 0 = BlockHeader {
          _miner = Address "0",
          _parentHash = Hash $ "0000",
          _nonce = 0,
          _minedAt = 0,
          _blockNumber = 0
        }
getHeaderAt (_ :< Node header _) 0 = header
getHeaderAt (_ :< Node _ parent) n = getHeaderAt parent $ (n-1)

-- Like getHeaderAt For Transactions
getTxsAt :: Blockchain -> Integer -> [Transaction]
getTxsAt (block :< Genesis) _ = fstList $ _txs block
getTxsAt (block :< _) 0 = fstList $ _txs block
getTxsAt (_ :< Node _ parent) i = getTxsAt parent $ (i - 1)

getTxFull :: Blockchain -> [Transaction]
getTxFull bc = loop bc (getChainLength bc 0)
  where
    loop bc 0 = getTxsAt bc 0
    loop bc i = (getTxsAt bc i) ++ (loop bc (i - 1))

-- Iterating through the Blockchain to get number of blocks
getChainLength :: Blockchain -> Integer -> Integer
getChainLength (_ :< Genesis) i = i
getChainLength (_ :< Node _ parent) i = getChainLength parent (i+1)

-- Calculates the sum of fees on blockchain txs
getChainFees :: Blockchain -> Integer -> Integer
getChainFees (_ :< Genesis) i = i
getChainFees (block :< Node _ parent) i = getChainFees parent ((blockFees block) + i)
  where
    blockFees b = loop 0 $ fstList $ _txs b
    loop a [] = a
    loop a (x:xs) = loop (a + (_fee x)) xs

-- Merge / Extract ---------------------------------------------------

mergeNewBlock :: Block -> BlockHeader -> Blockchain -> IO Blockchain
mergeNewBlock block header bc = do
  txs <- filterDuplicates bc $ filterInadequteBalanceTransaction bc $ filterSignedTransactions $ convertSignedTxFromIntegerList $ _txs block
  tmpTxs <- (convertSignedTxFromIntegerList $ _txs block)
  let verifyTxs = txs == tmpTxs
      isHashCorrent = (_parentHash header ) == (hashBlockchain bc)
      isBlockNumberCorrect = (\(block :< Node chainHeader parent) h -> (_blockNumber chainHeader + 1) == _blockNumber h) bc header
  if verifyTxs && isHashCorrent && isBlockNumberCorrect 
    then do
      let newChain = (block :< Node header bc)
      return newChain
    else 
      return bc

extractNewBlock :: Blockchain -> (Block, BlockHeader)
extractNewBlock (block :< Node header _) = (block, header)

-- Filters -------------------------------------------------------------

filterDuplicates :: Blockchain -> SignedTransactionPool -> SignedTransactionPool
filterDuplicates bc sTx = do
  s <- sTx
  let hashList = buildTxList bc
  return $ loopCheck s $ removeDuplicate hashList
    where
      removeDuplicate [] = []
      removeDuplicate (x : xs) = if x `elem` xs then removeDuplicate xs else x : (removeDuplicate xs)

      loopCheck [] _ = []
      loopCheck (x : xs) l = if (_id $ fst x) `elem` l then loopCheck xs l else x : (loopCheck xs l)

filterSignedTransactions :: SignedTransactionPool -> SignedTransactionPool
filterSignedTransactions signedTxPool = do
  s <- signedTxPool
  loopCheck s
    where
      loopCheck [] = return []
      loopCheck (x:xs) = do
        v <- verify x
        ls <- loopCheck xs
        if v then return $ x : ls else return ls

      verify x = do
        if ((_from $ fst x) == (Account "REWARD"))
          then do
            return True
          else do
            p <- (pub $ fst x)
            return $ verifyTx p (fst x) (snd x)

      pub = convertTransactionToPublicKey


-- Check Every Account For Adequate Balance In Transaction
filterInadequteBalanceTransaction :: Blockchain -> SignedTransactionPool -> SignedTransactionPool
filterInadequteBalanceTransaction chain tx = do
  txList <- tx
  let ledgerList = buildLedgerList chain
  return $ loopTxs ledgerList txList
    where
      loopTxs _ [] = []
      loopTxs l (x : xs) = if verifyBalance l (fst x) then x : (loopTxs l xs) else loopTxs l xs

      verifyBalance l x = if (_from x == (Account "REWARD")) then True else (accountBalance $ filterLedgerToAccount (_from x) l) >= ((_amount x ) + (_fee x)) && (_amount x > 0 ) && (_to x) /= (Address "")


checkReward :: Blockchain -> Bool
checkReward (_ :< Genesis) = True
checkReward (block :< Node header parent) = (isMinerInHeader (fstList $ _txs block) header) 
  where
    isMinerInHeader [] _ = True
    isMinerInHeader (x:xs) h = if (_from x) == (Account "REWARD") then ( _to x) == (_miner h) && (_amount x) == minerReward && isMinerInHeader xs h else True && isMinerInHeader xs h

checkBlockchainHash :: Blockchain -> Bool
checkBlockchainHash (_ :< Genesis) = True
checkBlockchainHash (_ :< Node header parent) = (hashBlockchain parent) == (_parentHash header)

-- Mining ------------------------------------------------------------------------------------------

-- Miner -> IO Transactions -> Blockchain -> updated Blockchain (IO)
mineBlock :: Account -> SignedTransactionPool -> Blockchain -> IO Blockchain
mineBlock acc signedTxPool parentChain = do
  -- Filtering TransactionPool and parent Blockchain
  if checkReward parentChain && checkBlockchainHash parentChain then do
    txs <- filterDuplicates parentChain $ filterInadequteBalanceTransaction parentChain $ filterSignedTransactions $ (\x -> take blockTXs x) <$> signedTxPool
    let txsInteger = convertSignedTxToIntegerList txs
    -- Getting Current Time:
    now <- getPOSIXTime
    let parentBlockNum = getChainLength parentChain 0
    let currentBlockNum = parentBlockNum + 1
    loop txsInteger 0 now currentBlockNum
  else return ((Block []) :< Genesis)
        where
        --Difficulty Check:
        checkValidation c = (Prelude.take 2 $ hashToString $ CL.unpack $ B.encode c) == "00"
        --Hash of parent blockchain:
        parentHash = hashBlockchain parentChain
        --Generates Miner Address For Miner Account
        minerAddress = convertAccountToMinerAddress acc
        --Reward Tx:
        minerRewardTx = Transaction {_from = Account "REWARD" , _to = minerAddress , _amount = minerReward, _text = "REWARD", _fee = 0, _id = parentHash }
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
          let chain = ((Block $ (minerRewardTx, (0,0)) : tx) :< Node header parentChain)
          if checkValidation chain
            then do
              saveChain chain -- SAVE CHAIN TO FILE
              return chain
            else loop tx (nonce + 1) n currentBlockNum

-- Transactions List ------------------------------------------

buildTxList :: Blockchain -> TxHashList
buildTxList (_ :< Genesis) = []
buildTxList (block :< Node _ parent) = (hashList $ fstList $ _txs block) ++ (buildTxList parent)
  where
    hashList [] = []
    hashList (x : xs) = _id x : (hashList xs)



