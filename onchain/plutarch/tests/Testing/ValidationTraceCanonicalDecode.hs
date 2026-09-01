{-# LANGUAGE OverloadedStrings #-}

module Testing.ValidationTraceCanonicalDecode (tests) where

import Data.ByteString qualified as BS
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)

import Midgard.Validators.FraudProofs.ValidationTrace.CanonicalDecodeEmpty (
  canonicalDecodeEmptySemanticV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.CanonicalDecodeItem (
  canonicalDecodeItemProofV1Validator,
  canonicalDecodeItemSettlementV1Validator,
  canonicalDecodeItemSourceV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.CanonicalDecodeItemEvidence (
  canonicalDecodeItemObserveV1Validator,
  canonicalDecodeItemSemanticV1Validator,
  canonicalDecodeProofItemV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.CanonicalDecodePrepare (
  canonicalDecodeV1Validator,
 )
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Validation Trace CanonicalDecode Validators"
    [ testCase "empty semantic validator cancels its own computation thread" $
        psucceeds $ cancelledWith canonicalDecodeEmptySemanticV1Validator threadName
    , testCase "empty semantic validator rejects cancellation of another thread" $
        pfails $ cancelledWith canonicalDecodeEmptySemanticV1Validator otherThreadName
    , testCase "item source validator cancels its own computation thread" $
        psucceeds $ cancelledWith canonicalDecodeItemSourceV1Validator threadName
    , testCase "item proof validator cancels its own computation thread" $
        psucceeds $ cancelledWith canonicalDecodeItemProofV1Validator threadName
    , testCase "item settlement validator cancels its own computation thread" $
        psucceeds $ cancelledWith canonicalDecodeItemSettlementV1Validator threadName
    , testCase "item observation validator cancels its own computation thread" $
        psucceeds $ cancelledEvidenceWith canonicalDecodeItemObserveV1Validator threadName
    , testCase "item semantic validator cancels its own computation thread" $
        psucceeds $ cancelledEvidenceWith canonicalDecodeItemSemanticV1Validator threadName
    , testCase "proof-item validator is append-only" $
        pfails $ canonicalDecodeProofItemV1Validator # pconstant proofItemSpendContext
    , testCase "CanonicalDecode preparation validator cancels its own computation thread" $
        psucceeds $ cancelledPrepareWith threadName
    ]

cancelledWith :: forall s.
  Term s
    ( PAsData PScriptHash :--> PAsData PCurrencySymbol
        :--> PScriptContext :--> PUnit
    ) ->
  BS.ByteString ->
  Term s PUnit
cancelledWith validator cancellationName =
  validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pconstant
      ( spendContext
          (stepDatum Nothing)
          cancelRedeemer
          [threadInput]
          []
          []
          [cancelMintEntry cancellationName]
          mempty
      )

cancelledPrepareWith :: forall s. BS.ByteString -> Term s PUnit
cancelledPrepareWith cancellationName =
  canonicalDecodeV1Validator
    # resolverHashes
    # pdata (pconstant ctPolicy)
    # pconstant
      ( spendContext
          (stepDatum Nothing)
          cancelRedeemer
          [threadInput]
          []
          []
          [cancelMintEntry cancellationName]
          mempty
      )

cancelledEvidenceWith :: forall s.
  Term s
    ( PAsData PScriptHash :--> PAsData PCurrencySymbol
        :--> PAsData PScriptHash :--> PScriptContext :--> PUnit
    ) ->
  BS.ByteString ->
  Term s PUnit
cancelledEvidenceWith validator cancellationName =
  validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant $ ScriptHash $ toBuiltin otherScript)
    # pconstant
      ( spendContext
          (stepDatum Nothing)
          cancelRedeemer
          [threadInput]
          []
          []
          [cancelMintEntry cancellationName]
          mempty
      )

proofItemSpendContext :: ScriptContext
proofItemSpendContext =
  spendContext
    (stepDatum Nothing)
    cancelRedeemer
    [threadInput]
    []
    []
    []
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
