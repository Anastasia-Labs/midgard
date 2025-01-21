{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE OverloadedStrings #-}

module Testing.Crypto where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Midgard.Utils (pand'List, pintToByteString)
import Testing.Eval (psucceeds, passert, toHexString)
import Plutarch.Crypto 
import Midgard.Crypto (pethereumPubKeyToPubKeyHash, pcompressPublicKey)
import Plutarch.Prelude
import PlutusCore.Crypto.Hash qualified as Hash

tests :: TestTree
tests = testGroup "PlutarchV3 Builtins"
  [ testCase "keccak_256" $
      passert $
        (pkeccak_256 # (phexByteStr "4dd6d57ca8cb7ac8c3b219366754a392ba9e4e43b6b3ae59d89be3f878ba8fb6")) 
            #== phexByteStr "1994466d51e80d6dc32c8f41f142b47b2d03b4f5fa869aced6157c016b2b331d"
  , testCase "pblake2b_224" $
      passert $
        (pblake2b_224 # (phexByteStr "4dd6d57ca8cb7ac8c3b219366754a392ba9e4e43b6b3ae59d89be3f878ba8fb6")) 
            #== pconstant (Hash.blake2b_224 $ toHexString "4dd6d57ca8cb7ac8c3b219366754a392ba9e4e43b6b3ae59d89be3f878ba8fb6")
  , testCase "pripemd_160" $
      passert $
        (pripemd_160 # (phexByteStr "4dd6d57ca8cb7ac8c3b219366754a392ba9e4e43b6b3ae59d89be3f878ba8fb6")) 
            #== pconstant (Hash.ripemd_160 $ toHexString "4dd6d57ca8cb7ac8c3b219366754a392ba9e4e43b6b3ae59d89be3f878ba8fb6")
  , testCase "pethereumPubKeyToPubKeyHash" $
      passert $ 
        (pethereumPubKeyToPubKeyHash # phexByteStr "c4468787fea5550ad252706bd788f7bf41f3815a41d6311bc2d459cf081278abe774cb40d195544a6a25f84e18e26c8359b33be357fbb5ffff0f235e854c2b92")
          #== phexByteStr "3c8e73a95798dbeb720709820a66f77ee13502a9"
  , testCase "pintToByteString" $ 
      passert $ 
        (pintToByteString # 299562462973035008) #== (pencodeUtf8 # pconstant "299562462973035008")
  , testCase "compressEthereumPublicKey" $
      passert $
        (pcompressPublicKey (phexByteStr "c4468787fea5550ad252706bd788f7bf41f3815a41d6311bc2d459cf081278abe774cb40d195544a6a25f84e18e26c8359b33be357fbb5ffff0f235e854c2b92"))
         #== phexByteStr "02c4468787fea5550ad252706bd788f7bf41f3815a41d6311bc2d459cf081278ab"
  ]
