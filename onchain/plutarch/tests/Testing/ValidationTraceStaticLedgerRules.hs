{-# LANGUAGE OverloadedStrings #-}

module Testing.ValidationTraceStaticLedgerRules (tests) where

import Data.ByteString qualified as BS
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.V3 (PScriptHash)
import Plutarch.Prelude

import Midgard.Validators.FraudProofs.ValidationTrace.StaticLedgerRules (
  staticLedgerRulesSemanticV1Validator,
  staticLedgerRulesV1Validator,
 )
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Validation Trace StaticLedgerRules Validators"
    [ testCase "phase validator cancels its own computation thread" $
        psucceeds $ cancelledPreparationWith threadName
    , testCase "semantic validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith threadName
    , testCase "semantic validator rejects cancellation of another thread" $
        pfails $ cancelledSemanticWith otherThreadName
    ]

cancelledPreparationWith :: forall s. BS.ByteString -> Term s PUnit
cancelledPreparationWith cancellationName =
  staticLedgerRulesV1Validator
    # resolverHashes
    # pdata (pconstant ctPolicy)
    # pconstant (cancelContext cancellationName)

cancelledSemanticWith :: forall s. BS.ByteString -> Term s PUnit
cancelledSemanticWith cancellationName =
  staticLedgerRulesSemanticV1Validator
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
      # pnil
