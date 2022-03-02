{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances, TypeSynonymInstances, DeriveTraversable, DeriveGeneric, FlexibleInstances  #-}

module Basboosa.Types where

import System.IO

import Control.Comonad.Cofree
import qualified Crypto.ECC as ECC
import qualified Crypto.PubKey.ECDSA as ECDSA

import Crypto.Hash

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Binary as B
import Data.Time

import Data.Time.Clock.POSIX

import GHC.Generics (Generic)

-- Sorting Account, Address And Hash To newtype 
newtype Account = Account String deriving (Eq, Show, Generic)
newtype Hash = Hash String deriving (Eq, Show, Generic)
newtype Address = Address String deriving (Eq, Show, Generic)

type PrivateKey = ECC.Scalar ECC.Curve_P256R1
type PublicKey = ECC.Point ECC.Curve_P256R1
type Signature = ECDSA.Signature ECC.Curve_P256R1
type SignatureInteger = (Integer, Integer)

type TransactionPool =  IO [Transaction]
type SignedTx = (Transaction, Signature)
type SignedTxInteger = (Transaction, SignatureInteger)
type SignedTransactionPool = IO [SignedTx] 

type TxHashList = [Hash]

-- Transaction Data
data Transaction = Transaction {
  _from   :: Account,
  _to     :: Address,
  _amount :: Integer,
  _text   :: String,
  _fee    :: Integer,
  _id     :: Hash
} deriving (Eq, Show, Generic)

-- Block data type holding list of Transactions
data Block = Block { _txs :: [SignedTxInteger]} deriving (Eq, Show, Generic)

-- BlockHeader data type
data BlockHeader = BlockHeader {
  _miner :: Address,
  _parentHash :: Hash,
  _nonce :: Integer,
  _minedAt :: POSIXTime,
  _blockNumber :: Integer
}  deriving (Eq, Show)

-- Chain type, holding the blockchain recursively
data ChainT a = Genesis |
  Node BlockHeader a deriving (Eq, Show, Functor, Traversable, Foldable)

type Blockchain = Cofree ChainT Block
-- (block :< Node BlockHeader parnet)

type Account_Balance = (Account, Integer)
newtype LedgerList = LedgerList [Account_Balance] deriving (Eq, Show, Generic)

data NodeRequest = ReqFullBlockchain
                |  ReqNewBlock Block BlockHeader
                |  ReqNewTx SignedTxInteger
                |  ReqTxList
                deriving (Eq, Show, Generic) 

data NodeRespond = ResFullBlockchain CL.ByteString    --Blockchain
                |  ResNewBlock
                |  ResNewTx
                |  ResTxList [Transaction]
                |  ResError
                deriving (Eq, Show, Generic) 


instance Binary NodeRequest 
instance Binary NodeRespond

instance Binary LedgerList 
instance Binary Address
instance Binary Account
instance Binary Transaction
instance Binary Block
instance Binary Hash 
instance (Binary (f (Cofree f a)), Binary a) => Binary (Cofree f a) 
instance (Binary a) => Binary (ChainT a) where
instance Binary BlockHeader where

instance Binary POSIXTime where
  get = fromInteger <$> (get :: Get Integer)
  put x = put $ (round x :: Integer)

deriving instance Generic BlockHeader
deriving instance Generic (ChainT a)

-- Helper Functions ------------------------------

fstList :: [(a,b)] -> [a]
fstList lst = foldr (\x acc -> (fst x):acc) [] lst


-- Global Constants ---------------------------


-- Global Number Of Txs Per Block
blockTXs :: Int
blockTXs = 100

-- Global Amount Of Reward To Miner
minerReward :: Integer
minerReward = 100

privateFile :: FilePath
privateFile = "privatekey.prv"

txFile :: FilePath
txFile = "SignedTransaction.tx"

chainFile :: FilePath
chainFile = "Blockchain.chain"


-- Hashing -------------------------------------------------------------------

-- Chaining
hashToString :: String -> String
hashToString = byteToStringH . C.pack

byteToStringH :: C.ByteString -> String
byteToStringH bs = show (hash bs :: Digest SHA256)

hashBlockchain :: Blockchain -> Hash
hashBlockchain = Hash . hashToString . CL.unpack . B.encode








