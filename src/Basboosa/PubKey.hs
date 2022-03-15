{-# LANGUAGE OverloadedStrings, PackageImports, DeriveGeneric #-}

module Basboosa.PubKey where

import Basboosa.Types

import System.IO
import System.Directory

import Control.DeepSeq

import qualified Crypto.PubKey.ECDSA as ECDSA
import qualified Crypto.Error as ERR
import Crypto.ECC
import Crypto.ECC.Edwards25519
import Crypto.Random
import "cipher-aes" Crypto.Cipher.AES
import Crypto.Hash 

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

import qualified Data.Binary as B
import Data.Proxy
import Data.Either
import Data.ByteArray
import Data.ByteArray.Encoding

import Data.List.Split

import qualified Data.Mnemonic.Electrum as M


-- Proxy Of Curve ----------------------------------
proxy :: Proxy Curve_P256R1
proxy = Proxy :: Proxy (Curve_P256R1)

-- Generate / Convert ------------------------------
generatePrivateKey :: IO PrivateKey
generatePrivateKey = curveGenerateScalar proxy

convertToPublicKey :: PrivateKey -> PublicKey
convertToPublicKey = pointBaseSmul proxy

convertPrivateKeyToWords :: PrivateKey -> String
convertPrivateKeyToWords = M.encode . scalarToInteger proxy 

convertWordsToPrivateKey :: String -> IO PrivateKey
convertWordsToPrivateKey = ERR.throwCryptoErrorIO . scalarFromInteger proxy . maybe 0 (\x -> x) . M.decode

convertToHex :: PublicKey -> String
convertToHex ba = UTF8.toString (convertToBase Base16 (encodePoint proxy ba :: C.ByteString) :: C.ByteString)

convertStringToPublicKey :: String -> IO PublicKey
convertStringToPublicKey s = ERR.throwCryptoErrorIO $ decodePoint proxy $ fromRight (C.pack "ERROR") (convertFromBase Base16 (C.pack s) :: Either String C.ByteString)

convertPublicKeyToAccount :: PublicKey -> Account
convertPublicKeyToAccount pub = Account $ "A_" ++ (convertToHex pub)

convertAccountToPublicKey :: Account -> IO PublicKey
convertAccountToPublicKey acc =  convertStringToPublicKey $ (splitOn "_" $ getString acc) !! 1
    where
        getString = (\(Account x) -> x)

convertPublicKeyToAddress :: PublicKey -> IO Address
convertPublicKeyToAddress pub = do
    rs <- generateRandomSeed
    return $ Address $ "K_" ++ (convertToHex pub) ++ "_" ++ (Prelude.take 10 rs)

convertAddressToPublicKey :: Address -> IO PublicKey
convertAddressToPublicKey addr =  convertStringToPublicKey $ (splitOn "_" $ getString addr) !! 1
    where
        getString = (\(Address x) -> x)

convertAddressToAccount :: Address -> Account
convertAddressToAccount addr = Account $ "A_" ++ (splitOn "_" $ getString addr) !! 1
    where
        getString = (\(Address x) -> x)

convertPrivateKeyToAccount :: PrivateKey -> Account
convertPrivateKeyToAccount = convertPublicKeyToAccount . convertToPublicKey

convertTransactionToPublicKey :: Transaction -> IO PublicKey
convertTransactionToPublicKey = convertAccountToPublicKey . _from

convertAccountToMinerAddress :: Account -> Address
convertAccountToMinerAddress acc = Address $ "M_" ++ (splitOn "_" $ getString acc) !! 1
    where
        getString = (\(Account x) -> x)
-- Signature Convertions -------------------------------------------

convertSignatureToIntegers :: Signature -> SignatureInteger
convertSignatureToIntegers = ECDSA.signatureToIntegers proxy

-- Generate Random String For Address -------------------------------

generateRandomSeed :: IO String
generateRandomSeed = do
    a <- seedNew
    return $ show $ seedToInteger a 

-- Encode/Sign/Verify ------------------------------------------------

encodeTx :: Transaction -> C.ByteString
encodeTx = C.pack . BSLC.unpack . B.encode 

signTx :: PrivateKey -> Transaction -> IO Signature
signTx priv tx = ECDSA.sign proxy priv SHA256 (encodeTx tx) 

verifyTx :: PublicKey -> Transaction -> Signature -> Bool
verifyTx pub tx sign = ECDSA.verify proxy SHA256 pub sign (encodeTx tx)

-- Encrypt / Decrypt -------------------------------------------------

-- Input Key , Msg
encryptWithKey :: String -> String -> String
encryptWithKey key msg = C.unpack $ encryptECB (initAES $ C.pack $ padding key) $ C.pack $ padding msg
    where
        len s = (C.length $ C.pack s) `mod` 16
        padding s = if (len s) == 0 then s else padding (s ++ " ")  

-- Input Key , Encrypted Msg
decryptWithKey :: String -> String -> String
decryptWithKey key msg = filterSpace $ C.unpack $ decryptECB (initAES $ C.pack $ padding key) $ C.pack msg
    where
        len s = (C.length $ C.pack s) `mod` 16
        padding s = if (len s) == 0 then s else padding (s ++ " ")
        filterSpace = filter (/= ' ')


-- Storage ----------------------------------------------------------

savePrivateKey :: PrivateKey -> String -> IO ()
savePrivateKey prv key = do
    handle <- openFile privateFile WriteMode
    hPutStr handle $ encryptWithKey key $ show $ scalarToInteger proxy prv
    hClose handle

loadPrivateKey :: String -> IO PrivateKey
loadPrivateKey key = do
    handle <- openFile privateFile ReadMode
    f <- hGetContents handle
    f `deepseq` hClose handle
    ERR.throwCryptoErrorIO $ scalarFromInteger proxy (read $ decryptWithKey key f :: Integer)

saveAccount :: PrivateKey -> IO ()
saveAccount priv = do
    handle <- openFile accountFile WriteMode
    hPutStr handle $ (\(Account x ) -> x) $ convertPublicKeyToAccount $ convertToPublicKey priv
    hClose handle

loadAccount :: IO Account
loadAccount = do
    handle <- openFile accountFile ReadMode
    f <- hGetContents handle
    f `deepseq` hClose handle
    return $ Account f
    

-- Login / Logout -----------------------------------------------------------

loginWithMnemonic :: String -> String -> IO ()
loginWithMnemonic m key = do
    priv <- convertWordsToPrivateKey m
    savePrivateKey priv key

erasePrivateKeyFile :: IO ()
erasePrivateKeyFile = do
    putStrLn "THIS WILL DELETE YOUR PRIVATEKEY FILE(type Y to delete):"
    line <- getLine 
    if line == "Y" 
        then do
            putStrLn "Deleting File..."
            removeFile privateFile 
        else do
            putStrLn "Skiping..."

-- Testing----------------------------------------------------------

exampleTx :: Transaction
exampleTx = Transaction {
  _from   = Account "Acc",
  _to     = Address "Add",
  _amount = 100 ,
  _text   = "TXT",
  _fee    = 0,
  _id     = Hash "0000"
}

exampleTx2 :: Transaction
exampleTx2 = Transaction {
  _from   = Account "Acc",
  _to     = Address "Add",
  _amount = 1000 ,
  _text   = "TXT",
  _fee    = 0,
  _id     = Hash "0000"
}

exampleKey :: IO ((PrivateKey, PublicKey), (Account , Address))
exampleKey = do
    priv <- generatePrivateKey
    let pub = convertToPublicKey priv
    let acc = convertPublicKeyToAccount pub
    addr <- convertPublicKeyToAddress pub
    return ((priv, pub),( acc, addr))








