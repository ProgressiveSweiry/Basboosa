# Basboosa

Basboosa Is a simple "Proof Of Work" Haskell Blockchain Concept  

Miners hash the blockchain in order to find specific hash (Like Bitcoin)

<br>

# Usage

Script can be run using `cabal new-repl` <br>
Tested on Windows, Linux , MacOS
<br>

### To generate new blockchain (Genesis Chain): <br>
1. Generate new key (can be done using exampleKey in Basboosa.PubKey, will generate PrivateKey, PublicKey, Account and Address)
2. Generate new GenesisChain, use makeGenesisFund (from Basboosa.Chain) and pass Address to fund it.
3. Save Blockchain to file.
4. Run node (use mainServer from Basboosa.Network)
5. Node will refer to the Blockchain in the file.

```Haskell
> newKey <- exampleKey
> addr = snd $ snd newKey
> addr
Address "K_045f10ac761b6d17fa2bc22b70f8034503844f244a3bf63fcbc2b2f89c180a64dd1d92d6b1e9288a98d5f7942f12a3f44e183c4c83261d8c93a2a039d9f7813387_5811878054"
> newChain <- makeGenesisFund addr
> saveChain newChain
```

### Load Existing Blockchain

```Haskell
*Basboosa.Network> blockchain <- loadChain
```

### Generate And Sign Transaction

Use Basboosa.Ledger for generating and signing Txs <br>
Example : <br>

```Haskell
buildAndSignTx :: Address -> Integer -> String -> PrivateKey -> IO SignedTx
```

# On Chain Info

Basboosa.Chain consists functions to explore blockchain objects

Examples: <br>

```Haskell
getHeaderAt :: Blockchain -> Integer -> BlockHeader
getTxsAt :: Blockchain -> Integer -> [Transaction]
getTxFull :: Blockchain -> [Transaction]
getChainLength :: Blockchain -> Integer -> Integer
getChainFees :: Blockchain -> Integer -> Integer
```

# Ledger

Conversion to Ledger List (a list of tuples consisting Account, Value) is possible from blockchain (in Basboosa.Ledger):

```Haskell 
buildLedgerList :: Blockchain -> LedgerList
```

Some helper functions are provided to filter/merge Ledger Lists

# PubKey

[Cryptonite](https://hackage.haskell.org/package/cryptonite) is used in this project to generate keys.

PublicKeys are generated from corresponding PrivateKeys.<br>

Accounts are strings that resembles PublicKey, that way only PrivateKey corresponding with the Account can sign Txs. <br>

Addresses are Accounts with random integer added in the end of the string (to generate diversity of address to share with senders)





### Windows Shell Encoding Fix 
> chcp 65001

