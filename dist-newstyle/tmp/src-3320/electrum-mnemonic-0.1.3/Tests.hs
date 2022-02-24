{- Copyright (C) 2014 Joey Hess <id@joeyh.name>
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

import Data.Mnemonic.Electrum

import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties =  testGroup "Properties"
	[ localOption (QuickCheckTests 10000) $
		testProperty "roundtrip" prop_roundrip
	, testProperty "stable" prop_stable
	]

prop_roundrip :: Integer -> Bool
prop_roundrip i
	| i >= 0 = decode (encode i) == Just i
	| otherwise = True

prop_stable :: Bool
prop_stable = and
	[ encode n == s
	, decode s == Just n
	]
  where
	n = 0x12345678ffffffffaaaa2222
	s = "memory string board fail husband howl board desert sign"
