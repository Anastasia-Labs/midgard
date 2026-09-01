{-# LANGUAGE OverloadedStrings #-}

module Testing.ValidationTraceInputSets (tests) where

import Data.ByteString qualified as BS
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.Validators.FraudProofs.ValidationTrace.InputSets (
  inputSetsEmptySemanticV1Validator,
  inputSetsItemSemanticV1Validator,
  inputSetsV1Validator,
 )
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Validation Trace InputSets Validators"
    [ testCase "phase validator cancels its own computation thread" $
        psucceeds $ cancelledPreparationWith threadName
    , testCase "empty semantic validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith inputSetsEmptySemanticV1Validator threadName
    , testCase "item semantic validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith inputSetsItemSemanticV1Validator threadName
    , testCase "item semantic validator rejects cancellation of another thread" $
        pfails $ cancelledSemanticWith inputSetsItemSemanticV1Validator otherThreadName
    ]

cancelledPreparationWith :: forall s. BS.ByteString -> Term s PUnit
cancelledPreparationWith cancellationName =
  inputSetsV1Validator
    # resolverHashes
    # pdata (pconstant ctPolicy)
    # pconstant (cancelContext cancellationName)

cancelledSemanticWith :: forall s.
  Term s
    ( PAsData PScriptHash :--> PAsData PCurrencySymbol
        :--> PScriptContext :--> PUnit
    ) ->
  BS.ByteString ->
  Term s PUnit
cancelledSemanticWith validator cancellationName =
  validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pconstant (cancelContext cancellationName)

cancelContext :: BS.ByteString -> ScriptContext
cancelContext cancellationName =
  spendContext
    (stepDatum Nothing)
    cancelRedeemer
    [threadInput]
    []
    []
    [cancelMintEntry cancellationName]
    mempty

resolverHashes :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash)))
resolverHashes =
  pdata $
    pcons
      # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
      # ( pcons
            # pdata (pconstant $ ScriptHash $ toBuiltin otherScript)
            # pnil
        )
