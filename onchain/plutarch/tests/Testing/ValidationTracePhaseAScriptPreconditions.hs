{-# LANGUAGE OverloadedStrings #-}

module Testing.ValidationTracePhaseAScriptPreconditions (tests) where

import Data.ByteString qualified as BS
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.V3 (PScriptHash)
import Plutarch.Prelude

import Midgard.Validators.FraudProofs.ValidationTrace.PhaseAScriptPreconditions (
  phaseAScriptPreconditionsV1Validator,
 )
import Testing.Eval (psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Validation Trace Phase-A Script Preconditions Validators"
    [ testCase "phase validator cancels its own computation thread" $
        psucceeds $ cancelledPreparationWith threadName
    ]

cancelledPreparationWith :: forall s. BS.ByteString -> Term s PUnit
cancelledPreparationWith cancellationName =
  phaseAScriptPreconditionsV1Validator
    # resolverHashes
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
