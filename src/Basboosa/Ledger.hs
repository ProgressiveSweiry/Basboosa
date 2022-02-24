{-# LANGUAGE JavaScriptFFI, PackageImports, OverloadedStrings, GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances, TypeSynonymInstances, DeriveTraversable, DeriveGeneric, FlexibleInstances  #-}


module Basboosa.Ledger where

import Basboosa.Types
import Basboosa.PubKey

import System.IO

import Control.Comonad.Cofree
import Crypto.Hash 

import Data.Binary as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.List 

import qualified Crypto.Error as ERR

import Crypto.PubKey.ECDSA (signatureFromIntegers, signatureToIntegers)

-- Transactions --------------------------------------------

buildAndSignTx :: Address -> Integer -> String -> PrivateKey -> IO SignedTx
buildAndSignTx addr amt txt priv = do
  let tx = buildTransaction (convertPrivateKeyToAccount priv) addr amt txt
  txID <- injectTransactionId tx
  signature <- signTx priv txID
  return (txID, signature)


-- Build Transactions : Input : Account (_from) , Address (_to), Amount
buildTransaction :: Account -> Address -> Integer -> String -> Transaction
buildTransaction acc addr amt txt = injectTransactionFee $ Transaction {_from = acc , _to = addr , _amount = amt, _text = txt, _fee = 0, _id = Hash $ ""}

calculateTransactionFee :: Transaction -> Integer
calculateTransactionFee tx = fromIntegral $ BSL.length $ B.encode tx

injectTransactionFee :: Transaction -> Transaction
injectTransactionFee tx = Transaction { _from = (_from tx), _to = (_to tx), _amount = (_amount tx), _text = (_text tx), _fee = (calculateTransactionFee tx), _id = (_id tx)}

injectTransactionId :: Transaction -> IO Transaction
injectTransactionId tx = do
  rs <- generateRandomSeed
  return $ Transaction { _from = (_from tx), _to = (_to tx), _amount = (_amount tx), _text = (_text tx), _fee = (calculateTransactionFee tx), _id = Hash $ take 10 rs}

-- Storage --------------------------------------------------

convertSignedTxToInteger :: SignedTx -> SignedTxInteger
convertSignedTxToInteger (tx, sign) = (tx, signatureToIntegers proxy sign)

convertSignedTxFromInteger :: SignedTxInteger -> IO SignedTx 
convertSignedTxFromInteger (tx, signInteger) = do
  sign <- ERR.throwCryptoErrorIO $ signatureFromIntegers proxy signInteger
  return (tx,sign)

saveSignedTx :: SignedTx -> IO ()
saveSignedTx sTx = writeFile txFile $ CL.unpack $ B.encode $ convertSignedTxToInteger sTx

loadSignedTx :: IO SignedTx 
loadSignedTx = do
  sTxI <- readFile txFile
  convertSignedTxFromInteger $ B.decode $ CL.pack sTxI

-- Ledger Utility -------------------------------------------

-- Creatring LedgerList of balances from existing Blockchain
buildLedgerList :: Blockchain -> LedgerList
buildLedgerList (block :< Genesis) = LedgerList $ buildMultiAccountBalance $ _txs block
buildLedgerList (block :< Node _ parent) =  combineLedgerList (LedgerList $ buildMultiAccountBalance $ _txs block) (buildLedgerList parent)
  where
    combineLedgerList = (\(LedgerList a) (LedgerList b) -> (LedgerList $ mergeLedgerList a b))

mergeLedgerList :: [Account_Balance] -> [Account_Balance] -> [Account_Balance]
mergeLedgerList [] l = l
mergeLedgerList ((x,y):xs) l = 
  if (elemIndexAt x l) == Nothing
    then mergeLedgerList xs ((x,y) : l)
    else mergeLedgerList xs ((combineValues (x,y) (l !! (elemIndexJust $ elemIndexAt x l)) ) : (dropNthElem l (elemIndexJust $ elemIndexAt x l)))
  where
    elemIndexAt a b = elemIndex a (fstList b)
    combineValues = (\(n,b) (_,b2) -> (n, b + b2))
    elemIndexJust = maybe 9999 (\a -> a) 
    dropNthElem a b = (take b a) ++ (drop (b+1) a)

    fstList [] = []
    fstList (a:as) = (fst a) : (fstList as)


buildMultiAccountBalance :: [Transaction] -> [Account_Balance]
buildMultiAccountBalance [] = []
buildMultiAccountBalance (x:xs) = mergeLedgerList (buildAccountBalance x) (buildMultiAccountBalance xs)  

buildAccountBalance :: Transaction -> [Account_Balance]
buildAccountBalance t = if f == to then (f, (0 - fee)) : [] else (f, (0 - a - fee)) : (to, a) : []
  where
    f = _from t
    a = _amount t
    to = convertAddressToAccount $ _to t
    fee = _fee t

-- Find Account Info In LedgerList
filterLedgerToAccount :: Account -> LedgerList -> LedgerList
filterLedgerToAccount acc led = LedgerList $ filter checkAcc $ (\(LedgerList l) -> l) led
  where
    checkAcc = (\(ac , _) -> ac == acc)


-- Calculate Total Balance Of Coins In LedgerList
accountBalance :: LedgerList -> Integer
accountBalance led = loop $ (\(LedgerList l) -> l) led
  where
    loop [] = 0
    loop ((_, b) : xs) = b + (loop xs)

