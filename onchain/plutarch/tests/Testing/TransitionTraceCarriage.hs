{-# LANGUAGE OverloadedStrings #-}

module Testing.TransitionTraceCarriage (tests) where

import Data.ByteString qualified as BS
import Midgard.FraudProofs.TransitionTrace.FinalYield qualified as Yield
import Midgard.FraudProofs.TransitionTrace.ProofCarriage qualified as Carriage
import Plutarch.Prelude
import PlutusCore.Data qualified as D
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V3
import Test.Tasty
import Test.Tasty.HUnit
import Testing.Eval (passertEval, pfails)
import Testing.FraudProofsFixture (blake2b256)
import Testing.ScriptContextBuilder (mkAdaValue)

tests :: TestTree
tests =
  testGroup "Transition trace carriage parity" $
    [ testCase "reads an exact committed transport" $
        passertEval $
          Carriage.pread # commitment transport # pconstant [0] # pconstant (refs [transport]) #== pconstant (D.List [D.I 1])
    , testCase "accepts definite transport independently of serialiseData" $
        passertEval $
          Carriage.pread # commitment "\x81\x01" # pconstant [0] # pconstant (refs ["\x81\x01"]) #== pconstant (D.List [D.I 1])
    , testCase "rejects a substituted commitment" $
        pfails $
          Carriage.pread # commitment "\x81\x01" # pconstant [0] # pconstant (refs [transport])
    , testCase "rejects malformed CBOR" $
        pfails $
          Carriage.popen # pconstant [0] # pconstant (refs ["\xff"])
    , testCase "rejects no chunks" $ pfails $ chunks [] []
    , testCase "rejects empty chunk" $ pfails $ chunks [0] [""]
    , testCase "rejects negative index" $ pfails $ chunks [-1] [transport]
    , testCase "rejects missing index" $ pfails $ chunks [1] [transport]
    , testCase "accepts a 4096-byte final chunk" $ passertEval $ chunks [0] [full] #== pconstant full
    , testCase "rejects oversized final chunk" $ pfails $ chunks [0] [full <> "x"]
    , testCase "requires full-sized nonterminal chunks" $ pfails $ chunks [0, 1] ["x", "y"]
    , testCase "concatenates full nonterminal and bounded final chunks" $ passertEval $ chunks [0, 1] [full, "z"] #== pconstant (full <> "z")
    , testCase "preserves supplied chunk ordering" $ passertEval $ chunks [1, 0] ["z", full] #== pconstant (full <> "z")
    , testCase "reads exactly the header and fault without a constructor-tag gate" $
        passertEval $
          pmatch (Carriage.pfields # pconstant (D.Constr 7 [D.I 0, D.I 1, D.I 2])) $
            \(PPair header fault) -> header #== pconstant (D.I 1) #&& fault #== pconstant (D.I 2)
    , testCase "rejects surplus envelope fields" $ pfails $ Carriage.pfields # pconstant (D.Constr 0 [D.I 0, D.I 1, D.I 2, D.I 3])
    , testCase "extracts only the requested one-step witness" $ passertEval $ Yield.poneStepWitness # pconstant (D.Constr 4 [D.Constr 3 [D.I 9]]) # 3 #== pconstant [D.I 9]
    , testCase "rejects a substituted one-step witness tag" $ pfails $ Yield.poneStepWitness # pconstant (D.Constr 4 [D.Constr 3 []]) # 4
    , testCase "rejects a substituted fault tag" $ pfails $ Yield.poneStepWitness # pconstant (D.Constr 9 [D.Constr 3 []]) # 3
    , testCase "rejects surplus one-step witnesses" $ pfails $ Yield.poneStepWitness # pconstant (D.Constr 4 [D.Constr 3 [], D.Constr 3 []]) # 3
    ]
      <> map initialTest [0, 1, 2]
 where
  transport = "\x9f\x01\xff"
  full = BS.replicate 4096 1

initialTest :: Integer -> TestTree
initialTest kind =
  testCase ("initial state wire for kind " <> show kind) $
    passertEval $
      pforgetData (pdata $ Yield.pinitial # pconstant kind # commitment "proof")
        #== pconstant
          ( D.Constr
              0
              [ D.I kind
              , D.I (if kind == 2 then 0 else 6)
              , D.Constr 0 [D.B $ blake2b256 "proof"]
              , D.Constr 0 [D.List [], D.List []]
              , D.I 0
              , D.I 0
              , D.B ""
              , D.Constr 0 [D.List []]
              , D.B ""
              , D.B ""
              , D.I 0
              , D.I 0
              , D.Constr 1 []
              , D.B ""
              , D.I 0
              , D.B ""
              , D.I 0
              ]
          )

commitment :: BS.ByteString -> Term s Carriage.PCommitment
commitment bytes = pcon $ Carriage.PCommitment $ pdata $ pconstant $ blake2b256 bytes

chunks :: [Integer] -> [BS.ByteString] -> Term s PByteString
chunks indices values = Carriage.pchunks # pconstant indices # pconstant (refs values)

refs :: [BS.ByteString] -> [TxInInfo]
refs values = zipWith reference [0 ..] values
 where
  reference index bytes =
    TxInInfo
      (TxOutRef (TxId "fixture") index)
      ( TxOut
          (scriptHashAddress $ ScriptHash "fixture")
          (mkAdaValue 2_000_000)
          (OutputDatum $ Datum $ dataToBuiltinData $ D.B bytes)
          Nothing
      )
