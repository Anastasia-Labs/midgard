{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsNativeScriptDecodingStep04 (tests) where

import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (singleton)
import PlutusLedgerApi.V3 (ScriptContext (..), TokenName (..), TxInfo (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.NativeScriptDecoding.Step04 (nativeScriptDecodingStep04Validator)
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests = testGroup "Native-script decoding step 04"
  [ testCase "finalizes a wrongful acceptance" $ psucceeds $ run $ finalizeContext $ provenState 0 0 (-1) 0
  , testCase "finalizes forced limit refusals" $ do
      psucceeds $ run $ finalizeContext $ provenState 0 1 (-1) 1
      psucceeds $ run $ finalizeContext $ provenState 0 1 (-1) 2
  , testCase "finalizes a wrongful rejection" $ psucceeds $ run $ finalizeContext $ provenState 1 1 2 0
  , testCase "rejects a pending class" $ pfails $ run $ finalizeContext $ provenState 0 0 (-1) (-1)
  , testCase "rejects an out-of-domain class" $ pfails $ run $ finalizeContext $ provenState 0 0 (-1) 3
  , testCase "rejects direction B without the marker" $ pfails $ run $ finalizeContext $ provenState 1 1 1 1
  , testCase "rejects direction B on a normal source" $ pfails $ run $ finalizeContext $ provenState 1 0 0 0
  , testCase "rejects an out-of-domain accusation" $ pfails $ run $ finalizeContext $ provenState 1 1 3 0
  , testCase "cancels under the prover signature" $ psucceeds $ run cancellationContext
  , testCase "rejects an unsigned cancellation" $ pfails $ run $ withoutSignatories cancellationContext
  ]

run :: forall s. ScriptContext -> Term s PUnit
run ctx = nativeScriptDecodingStep04Validator
  # pdata (pconstant ctPolicy)
  # pdata (pconstant fpPolicy)
  # pdata (pconstant fraudProofAddress)
  # pconstant ctx

provenState :: Integer -> Integer -> Integer -> Integer -> PD.Data
provenState direction sourceKind scanReason refusal = PD.Constr 0
  [ PD.I direction, PD.I sourceKind, PD.B $ hash32 0x33
  , PD.B $ if sourceKind == 1 then hash32 0x44 else "", PD.I scanReason
  , PD.B $ hash32 0x55, PD.I 0, PD.I 0
  , PD.B "", PD.I (-2), PD.I (-1), PD.I (-1), PD.B "", PD.B ""
  , PD.I refusal
  ]

finalizeContext :: PD.Data -> ScriptContext
finalizeContext state = spendContext
  (stepDatum $ Just state)
  (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0]])
  [threadInput]
  [convictionOutput fraudProofAddress threadName]
  []
  [fraudProofMintEntry threadName]
  (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)

cancellationContext :: ScriptContext
cancellationContext = spendContext
  (stepDatum $ Just $ provenState 0 0 (-1) (-1))
  cancelRedeemer
  [threadInput]
  []
  []
  [cancelMintEntry threadName]
  mempty

withoutSignatories :: ScriptContext -> ScriptContext
withoutSignatories (ScriptContext txInfo redeemer scriptInfo) =
  ScriptContext txInfo {txInfoSignatories = []} redeemer scriptInfo
