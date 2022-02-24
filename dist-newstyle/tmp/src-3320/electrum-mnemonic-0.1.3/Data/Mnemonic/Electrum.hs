{- Implementation of the Electrum mnemonic, in haskell.
 -
 - Copyright (C) 2014 Joey Hess <id@joeyh.name>
 -
 - Derived from Electrum's mnemonic.py
 - Copyright (C) 2011 thomasv@gitorious
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 - 
 - This program is distributed in the hope that it will be useful
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program. If not, see <http://www.gnu.org/licenses/>.
 -}

module Data.Mnemonic.Electrum
	( encode
	, decode
	, encode'
	, decode'
	, Hex
	, encodeHex
	, decodeHex
	, encodeHex'
	, decodeHex'
	) where

import Data.Char
import Data.List
import Data.Maybe
import Text.Printf
import Control.Applicative
import Prelude

import Data.Mnemonic.Electrum.Types
import Data.Mnemonic.Electrum.WordList.Poetic

{- 
 - Note about US patent no 5892470:
 - Here each word does not represent a given digit.
 - Instead, the digit represented by a word is variable, it depends
 - on the previous word. -- thomasv
 -}

-- | Generate a mnemonic for an arbitrary Integer using the Electrum word list.
--
-- Note about Electrum compatability:
-- When encoding, Electrum only operates on inputs in chunks of 8 hex digits.
-- This encoder is more flexible, supporting input Integers of any size
-- (although they cannot be negative). However, it will not always yeild
-- the same result as Electrum's encoder. For that, use `encodeHex`
encode :: Integer -> String
encode = encode' poeticWords

encode' :: WordList -> Integer -> String
encode' wl = encodeChunks wl . chunkInteger

-- | Generates a mnemonic for a hexidecimal string using the Electrum word list.
-- Fully compatable with Electrum.
encodeHex :: Hex -> String
encodeHex = encodeHex' poeticWords

encodeHex' :: WordList -> Hex -> String
encodeHex' wl = encodeChunks wl . chunkHex

-- | Generate a mnemonic using a word list of 8 digit hex numbers.
encodeChunks :: WordList -> [Int] -> String
encodeChunks (WordList len wl) = unwords .  map (wl !!) . go []
  where
  	go c [] = reverse c
	go c (x:xs) =
		let w1 = modwl x
		    w2 = modwl $ (divwl x) + w1
		    w3 = modwl $ (divwl (divwl x)) + w2
		in go (w3:w2:w1:c) xs
	modwl n = n `mod` len
	divwl n = n `div` len

chunkInteger :: Integer -> [Int]
chunkInteger 0 = [0]
chunkInteger i = go [] i
  where
	go coll n
		| n > 0 = 
			let (rest, c) = n `divMod` chunkSize
			in go (fromIntegral c:coll) rest
		| otherwise = coll

chunkSize :: Integer
chunkSize = 0x100000000

-- Only works on chunks of 8 chars, ignoring any small chunks,
-- for Electrum compatability.
chunkHex :: Hex -> [Int]
chunkHex = go []
  where
	go coll s
		| length s >= 8 =
			let (c, rest) = splitAt 8 s
			in go (readHex c:coll) rest
		| otherwise = reverse coll

-- | Decode an electrum mnemonic
decode :: String -> Maybe Integer
decode = decode' poeticWords

decode' :: WordList -> String -> Maybe Integer
decode' wl s = sum . map calc . zip pows . reverse <$> decodeChunks wl s
  where
	calc (pow, n) = fromIntegral n * pow
	pows = map (chunkSize ^) ([0..] :: [Integer])

decodeHex :: String -> Maybe Hex
decodeHex = decodeHex' poeticWords

decodeHex' :: WordList -> String -> Maybe Hex
decodeHex' wl s = concatMap showHex <$> decodeChunks wl s

decodeChunks :: WordList -> String -> Maybe [Int]
decodeChunks (WordList len wl) s
	| any isNothing l || null l = Nothing
	| otherwise = go [] (catMaybes l)
  where
	l = map (`elemIndex` wl) $ map (map toLower) $ words s

	go coll [] = Just (reverse coll)
	go coll (w1:w2:w3:ws) =
		let x = sum
			[ w1
			, len * calc w2 w1
			, len2 * calc w3 w2
			]
		in go (x:coll) ws
	go _ _ = Nothing

	len2 = len * len
	calc a b = (a-b) `mod` len

-- | A string that represents a hexadecimal number.
type Hex = String

readHex :: Hex -> Int
readHex h = read ("0x" ++ takeWhile (`elem` "012345678ABCDEFabcdef") h)

showHex :: Int -> Hex
showHex = printf "%08x"
