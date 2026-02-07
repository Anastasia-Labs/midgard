{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Data.ByteString qualified as BS
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Testing.Eval (passertEval)

-- import Midgard.Utils (pand'List)

import Data.Word (Word8)
import Plutarch.Core.Utils
import Plutarch.MerkleTree.Helpers
import Plutarch.MerkleTree.Merkling
import Plutarch.Monadic qualified as P
import Testing.Crypto qualified as Crypto
import Testing.MembershipValidator qualified as MembershipValidator
import Testing.MerklePatriciaForestry qualified as MPF

genByteString :: Gen BS.ByteString
genByteString = do
  len <- choose (0, 100) -- You can choose the length range you prefer
  bytes <- vectorOf len (arbitrary :: Gen Word8)
  return $ BS.pack bytes

genFourBytearrays :: Gen [BS.ByteString]
genFourBytearrays = vectorOf 4 arbitrary

instance Arbitrary BS.ByteString where
  arbitrary = genByteString

main :: IO ()
main = defaultMain tests

merkle_4_test :: Property
merkle_4_test = forAll genFourBytearrays $ \nodes ->
  plift $ pmerkle_4_test # (pconstant @(PBuiltinList PByteString) nodes)

pmerkle_4_test :: ClosedTerm (PBuiltinList PByteString :--> PBool)
pmerkle_4_test = plam $ \nodes -> P.do
  a <- plet $ phead # nodes
  aRest <- plet (ptail # nodes)
  b <- plet $ phead # aRest
  bRest <- plet (ptail # aRest)
  c <- plet $ phead # bRest
  d <- plet $ phead # (ptail # bRest)

  root <- plet $ pcombine # (pcombine # a # b) # (pcombine # c # d)

  pand'List
    [ pmerkle_4 # 0 # a # (pcombine # c # d) # b #== root
    , pmerkle_4 # 1 # b # (pcombine # c # d) # a #== root
    , pmerkle_4 # 2 # c # (pcombine # a # b) # d #== root
    , pmerkle_4 # 3 # d # (pcombine # a # b) # c #== root
    ]

combineNullHash :: Term s PBool
combineNullHash =
  pand'List
    [ pcombine # pnull_hash # pnull_hash #== pnull_hash_2
    , pcombine # pnull_hash_2 # pnull_hash_2 #== pnull_hash_4
    , pcombine # pnull_hash_4 # pnull_hash_4 #== pnull_hash_8
    ]

examplesSuffix :: Term s PBool
examplesSuffix =
  pand'List
    [ (psuffix # phexByteStr "abcd456789" # 0 #== phexByteStr "ffabcd456789")
    , (psuffix # phexByteStr "abcd456789" # 1 #== phexByteStr "000bcd456789")
    , (psuffix # phexByteStr "abcd456789" # 2 #== phexByteStr "ffcd456789")
    , (psuffix # phexByteStr "abcd456789" # 4 #== phexByteStr "ff456789")
    , (psuffix # phexByteStr "abcd456789" # 5 #== phexByteStr "00056789")
    , (psuffix # phexByteStr "abcd456789" # 10 #== phexByteStr "ff")
    ]

examplesNibbles :: Term s PBool
examplesNibbles =
  pand'List
    [ (pnibbles # phexByteStr "0123456789" # 2 # 2 #== pconstant (BS.pack []))
    , (pnibbles # phexByteStr "0123456789" # 2 # 3 #== pconstant (BS.pack [2]))
    , (pnibbles # phexByteStr "0123456789" # 4 # 8 #== pconstant (BS.pack [4, 5, 6, 7]))
    , (pnibbles # phexByteStr "0123456789" # 3 # 6 #== pconstant (BS.pack [3, 4, 5]))
    , (pnibbles # phexByteStr "0123456789" # 1 # 7 #== pconstant (BS.pack [1, 2, 3, 4, 5, 6]))
    ]

examplesNibble :: Term s PBool
examplesNibble =
  pand'List
    [ pnibble # phexByteStr "ab" # 0 #== 10
    , pnibble # phexByteStr "ab" # 1 #== 11
    ]

tests :: TestTree
tests =
  testGroup
    "Helper Tests"
    [ testGroup
        "combine tests"
        [ testCase "combine null hashes" $
            passertEval combineNullHash
        ]
    , testGroup
        "suffix tests"
        [ testCase "suffix examples" $ do
            passertEval examplesSuffix
        ]
    , testGroup
        "nibbles tests"
        [ testCase "nibbles examples" $ do
            passertEval examplesNibbles
        ]
    , testGroup
        "nibble tests"
        [ testCase "nibble examples" $
            passertEval examplesNibble
        ]
    , testGroup
        "Merkle tests"
        [ QC.testProperty "merkle_4 property" merkle_4_test
        ]
    , MPF.tests
    , Crypto.tests
    , MembershipValidator.tests
    ]
