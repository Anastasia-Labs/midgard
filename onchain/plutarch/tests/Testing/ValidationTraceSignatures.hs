{-# LANGUAGE OverloadedStrings #-}

module Testing.ValidationTraceSignatures (tests) where

import Data.ByteString qualified as BS
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.Validators.FraudProofs.ValidationTrace.Signatures (
  signaturesAddressItemSemanticV1Validator,
  signaturesAdvanceSemanticV1Validator,
  signaturesHandoffSemanticV1Validator,
  signaturesRequiredItemSemanticV1Validator,
  signaturesV1Validator,
 )
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Validation Trace Signatures Validators"
    [ testCase "phase validator cancels its own computation thread" $
        psucceeds $ cancelledPreparationWith threadName
    , testCase "advance semantic validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith signaturesAdvanceSemanticV1Validator threadName
    , testCase "handoff semantic validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith signaturesHandoffSemanticV1Validator threadName
    , testCase "address-item semantic validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith signaturesAddressItemSemanticV1Validator threadName
    , testCase "required-item semantic validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith signaturesRequiredItemSemanticV1Validator threadName
    , testCase "required-item validator rejects cancellation of another thread" $
        pfails $ cancelledSemanticWith signaturesRequiredItemSemanticV1Validator otherThreadName
    ]

cancelledPreparationWith :: forall s. BS.ByteString -> Term s PUnit
cancelledPreparationWith cancellationName =
  signaturesV1Validator
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
    pcons # scriptHash 1 #$
      pcons # scriptHash 2 #$
        pcons # scriptHash 3 #$
          pcons # scriptHash 4 # pnil
  where
    scriptHash byte = pdata (pconstant $ ScriptHash $ toBuiltin $ BS.singleton byte)
