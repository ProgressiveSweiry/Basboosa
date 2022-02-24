{-# LANGUAGE JavaScriptFFI, PackageImports, OverloadedStrings, GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances, TypeSynonymInstances, DeriveTraversable, DeriveGeneric, FlexibleInstances  #-}

module Basboosa.PubKey where

import Basboosa.Types

import qualified Data.ByteString.Char8 as C
import qualified Data.JSString as JSS
import qualified Data.JSVal.Promise as P
import qualified JavaScript.Web.Storage as S
import Data.Either

import GHCJS.Types


-- MUST INCLUDE IN FINAL INDEX.HTML
-- <script src="https://unpkg.com/bn.js@4.11.8/lib/bn.js" type="text/javascript"></script>
-- <script src="https://unpkg.com/@enumatech/secp256k1-js@1.0.0/src/secp256k1.js" type="text/javascript"></script>

-- Javascript Imports

foreign import javascript "crypto.getRandomValues(new Uint8Array(20))" generateRandomArrayJS :: JSVal
foreign import javascript "Secp256k1.uint256($1, 16)" generatePrivateKeyJS :: JSVal -> IO PrivateKey
-- Input Random Array (generateRandomArrayJS) ^

foreign import javascript "new Mnemonic(160)" generateMnemonicJS :: IO JSVal
foreign import javascript "$1.toWords().join(' ')" generateWordsM_JS :: JSVal -> JSString
foreign import javascript "new BN($1.toHex(),16)" convertPrivateKeyM_JS :: JSVal -> PrivateKey

foreign import javascript "new BN(Mnemonic.fromWords($1.split(' ')).toHex(),16)" mnemonicToPrivateKeyJS :: JSString -> PrivateKey

foreign import javascript "Secp256k1.generatePublicKeyFromPrivateKeyData($1)" generatePublicKeyFromPrivateKeyJS :: PrivateKey -> PublicKey
-- Input privatekey object (generatePrivateKeyJS) ^
foreign import javascript "Secp256k1.uint256($1.x, 16)" getPublicKeyX_JS :: PublicKey -> JSVal
foreign import javascript "Secp256k1.uint256($1.y, 16)" getPublicKeyY_JS :: PublicKey -> JSVal

foreign import javascript "Secp256k1.uint256($1, 16)" generateDigestJS :: JSString -> JSVal
foreign import javascript "Secp256k1.ecsign($1, $2)" generateSignatureJS :: JSVal -> JSVal -> IO JSVal
-- Inputs ($1 - privateKey , $2 - digest) ^
foreign import javascript "Secp256k1.uint256($1.r,16)" getSigR_JS :: JSVal -> IO JSVal
foreign import javascript "Secp256k1.uint256($1.s,16)" getSigS_JS :: JSVal -> IO JSVal
-- Input signature (generateSignatureJS) ^

foreign import javascript "Secp256k1.uint256($1,16)" getSig_JS :: JSString -> IO JSVal

foreign import javascript "$1.r" getSigR_StringJS :: JSVal -> JSString
foreign import javascript "$1.s" getSigS_StringJS :: JSVal -> JSString

foreign import javascript "Secp256k1.ecverify($1, $2, $3, $4, $5)" verifyDigestJS :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> Bool
-- Input pubX, pubY, sigR, sigS, digest (getPublicKeyX_JS, getPublicKeyY_JS, getSigR_JS, getSigS_JS, generateDigestJS (digest))

foreign import javascript "console.log($1)" toLogJS :: JSVal -> IO ()
-- Simple printing object to JS log ^

foreign import javascript "$1.toString()" privateKeyToStringJS :: PrivateKey -> JSString
foreign import javascript "new BN($1)" privateKeyFromStringJS :: JSString -> PrivateKey

foreign import javascript "\"K\" + \"_\" + $1.x.toString(36) + \"_\" + $1.y.toString(36) + \"_\" + crypto.getRandomValues(new Uint32Array(1)).toString(36)" publicKeyToAddressJS :: PublicKey -> JSString
foreign import javascript " \"A\" + \"_\" + $1.x.toString(36) + \"_\" + $1.y.toString(36)" publicKeyToAccountJS :: PublicKey -> JSString 
foreign import javascript " 'A' + '_' + $1.split('_')[1] + '_' + $1.split('_')[2]" addressToAccountJS :: JSString -> JSString
foreign import javascript " 'M' + '_' + $1.split('_')[1] + '_' + $1.split('_')[2]" accountToMinerAddressJS :: JSString -> JSString

foreign import javascript "Secp256k1.uint256($1.split('_')[1],16)" accountToPublicKeyX_JS :: JSString -> JSVal
foreign import javascript "Secp256k1.uint256($1.split('_')[2],16)" accountToPublicKeyY_JS :: JSString -> JSVal

foreign import javascript "{name:'PBKDF2',hash:'SHA-256',salt:new TextEncoder().encode('a-unique-salt'),iterations:1000}" algoSaltJS :: JSVal
foreign import javascript "crypto.subtle.importKey('raw',new TextEncoder().encode($1),{name:$2.name},false,['deriveKey'])" importDeriveKeyJS :: JSString -> JSVal  -> P.Promise
foreign import javascript "crypto.subtle.deriveKey($2,$1,{name:'AES-GCM',length:256},false,['encrypt','decrypt'])" deriveKeyJS :: JSVal -> JSVal -> P.Promise
-- Input importDeriveKeyJS, algo ^

foreign import javascript "{name:'AES-GCM',length:256,iv:crypto.getRandomValues(new Uint8Array(12))}" encryptAlgoJS :: JSVal
foreign import javascript "crypto.subtle.encrypt($3,$1,new TextEncoder().encode($2))" encryptTextJS :: JSVal -> JSString -> JSVal -> P.Promise
-- Input deriveKey, text ^
foreign import javascript "{cipherText:$1,iv:$2.iv}" encryptJS :: JSVal -> JSVal -> JSVal
-- Input encryptTextJS, algo ^

foreign import javascript "{name:'AES-GCM',length:256,iv:$1.iv}" decryptAlgoJS :: JSVal -> JSVal
-- Input encrypt object ^
foreign import javascript "crypto.subtle.decrypt($1,$2,$3.cipherText)" decryptTextJS :: JSVal -> JSVal -> JSVal -> P.Promise
-- Input algo, deriveKey, encrypt object^
foreign import javascript "new TextDecoder().decode($1)" decryptJS :: JSVal -> JSString
-- Input decryptTextJS^

foreign import javascript "(Math.random()).toString(16).substring(2)" randomSaltJS :: JSString

foreign import javascript "JSON.stringify(Array.from(new Uint8Array($1.cipherText)))" stringifyCipherJS :: JSVal -> JSString
foreign import javascript "JSON.stringify(Array.from(new Uint8Array($1.iv)))" stringifyIvJS :: JSVal -> JSString
-- Input encrypt object^
foreign import javascript "new Uint8Array(JSON.parse($1)).buffer" parseJS :: JSString -> JSVal
-- Input encrypt object as string
foreign import javascript "{cipherText: $1, iv: $2}" constructEncryptObjectJS :: JSVal -> JSVal -> JSVal


-- Generate / Convert --------------------------------------------------------

generatePrivateKey :: IO PrivateKey
generatePrivateKey = do 
  m <- generatePrivateKeyM
  return $ fst m

-- Returns PrivateKey , Mnemonics of PrivateKey
generatePrivateKeyM :: IO (PrivateKey, String)
generatePrivateKeyM = do
  m <- generateMnemonicJS
  return $ (convertPrivateKeyM_JS m, JSS.unpack $ generateWordsM_JS m )

generatePublicKeyFromPrivateKey :: PrivateKey -> PublicKey
generatePublicKeyFromPrivateKey priv = generatePublicKeyFromPrivateKeyJS priv 

generateMnemonic :: IO String
generateMnemonic = do
  m <- generatePrivateKeyM
  return $ snd m

generateMnemonicSaveToStorage :: String -> IO String
generateMnemonicSaveToStorage pass = do
  (priv,m) <- generatePrivateKeyM
  savePrivateKeyToLocalStorage priv pass
  return m 



generateAddressFromPrivateKey :: PrivateKey -> Address
generateAddressFromPrivateKey = generateAddressFromPublicKey . generatePublicKeyFromPrivateKey

generateAccountFromPrivateKey :: PrivateKey -> Account
generateAccountFromPrivateKey = generateAccountFromPublicKey . generatePublicKeyFromPrivateKey

generateAddressFromPublicKey :: PublicKey -> Address
generateAddressFromPublicKey = Address . C.pack . JSS.unpack . publicKeyToAddressJS

generateAccountFromPublicKey :: PublicKey -> Account
generateAccountFromPublicKey = Account . C.pack . JSS.unpack . publicKeyToAccountJS

convertAddressToAccount :: Address -> Account
convertAddressToAccount = Account . C.pack . JSS.unpack . addressToAccountJS . JSS.pack . C.unpack . (\(Address x) -> x)

convertAccountToMinerAddress :: Account -> Address
convertAccountToMinerAddress = Address . C.pack . JSS.unpack . accountToMinerAddressJS . JSS.pack. C.unpack . (\(Account x) -> x)


-- Returns (PubX,PubY)
convertTransactionToPublicKey :: Transaction -> (JSVal, JSVal)
convertTransactionToPublicKey tx = 
  let pubJSS = JSS.pack $ C.unpack $ (\(Account x) -> x) (_from tx) in 
  (accountToPublicKeyX_JS pubJSS, accountToPublicKeyY_JS pubJSS)


mnemonicToPrivateKey :: String -> PrivateKey
mnemonicToPrivateKey = mnemonicToPrivateKeyJS . JSS.pack

-- Sign / Verify --------------------------------------------------------

signWithPrivateKey :: PrivateKey -> String -> IO Signature
signWithPrivateKey priv d = do
  let digest = generateDigestJS $ JSS.pack d
  sig <- generateSignatureJS priv digest
  r <- getSigR_JS sig
  s <- getSigS_JS sig
  return (r, s)

signWithPrivateKeyToString :: PrivateKey -> String -> IO SignatureString
signWithPrivateKeyToString priv d = do
  let digest = generateDigestJS $ JSS.pack d
  sig <- generateSignatureJS priv digest
  return (JSS.unpack $ getSigR_StringJS sig, JSS.unpack $ getSigS_StringJS sig)


verifyWithPublicKey :: PublicKey -> Signature -> String -> Bool
verifyWithPublicKey pub sig st = verifyDigestJS (getPublicKeyX_JS pub) (getPublicKeyY_JS pub) (fst sig) (snd sig) (generateDigestJS $ JSS.pack st)

-- INPUT IS (PubX,PubY)
verifyWithPublicXY :: (JSVal,JSVal) -> Signature -> String -> Bool
verifyWithPublicXY (x,y) sig st = verifyDigestJS x y (fst sig) (snd sig) (generateDigestJS $ JSS.pack st)


-- Key Functions --------------------------------------------------------

deriveKey :: String -> IO JSVal
deriveKey s = do
  let jss = JSS.pack s
  let algo = algoSaltJS
  d <- P.await $ importDeriveKeyJS jss algo
  let d1 = fromRight nullRef d
  dKey <- P.await $deriveKeyJS d1 algo
  return $ fromRight nullRef dKey 


-- Key, Text
encryptKey :: String -> String -> IO JSVal
encryptKey key txt = do
  let algo = encryptAlgoJS
  dk <- deriveKey key
  e <- P.await $ encryptTextJS dk (JSS.pack txt) algo
  let e1 = fromRight nullRef e
  return $ encryptJS e1 algo


-- Key , Encrypted Object
decryptKey ::  String -> JSVal -> IO String
decryptKey key enc = do 
  let algo = decryptAlgoJS enc
  dk <- deriveKey key
  dt <- P.await $ decryptTextJS algo dk enc
  let dt1 = fromRight nullRef dt
  return $ JSS.unpack $ decryptJS dt1


-- Local Storage --------------------------------------------------------

-- String is the password to lock privatekey
savePrivateKeyToLocalStorage :: PrivateKey -> String -> IO ()
savePrivateKeyToLocalStorage priv key = do
  let s = JSS.unpack $ privateKeyToStringJS priv
  e <- encryptKey key s
  let cipher = stringifyCipherJS e
  let iv = stringifyIvJS e
  let l = S.localStorage
  S.setItem (JSS.pack "cipher") cipher l
  S.setItem (JSS.pack "iv") iv l
  
localStorageToPrivateKey :: String -> IO PrivateKey
localStorageToPrivateKey key = do
  let l = S.localStorage
  cipher <- S.getItem (JSS.pack "cipher")  l
  iv <- S.getItem (JSS.pack "iv")  l
  let cipher1 = parseJS $ maybe (JSS.pack "\NUL") (\x -> x) cipher
  let iv1 = parseJS $ maybe (JSS.pack "\NUL") (\x -> x) iv
  let e = constructEncryptObjectJS cipher1 iv1
  d <- decryptKey key e
  return $ privateKeyFromStringJS $ JSS.pack d


-- Login / Logout  --------------------------------------------------------

-- Saves PrivateKey to localstorge (Input is Mnemonics, Password)
loginMnemonic :: String -> String -> IO ()
loginMnemonic m pass = do
    let priv = mnemonicToPrivateKey m 
    savePrivateKeyToLocalStorage priv pass

-- Saves and prints new Mnemonics to localstorage and
loginMnemonicNew :: String -> IO ()
loginMnemonicNew pass = do 
    m <- generateMnemonic
    loginMnemonic m pass
    print m

-- Clears localStorage
logout :: IO ()
logout = do
    S.clear S.localStorage





