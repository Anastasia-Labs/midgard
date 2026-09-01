{-# LANGUAGE OverloadedStrings #-}

module Testing.ValidationTracePhaseANativeScripts (tests) where

import Data.ByteString qualified as BS
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativeScripts (
  phaseANativeAdvanceSemanticV1Validator,
  phaseANativeFrameSemanticV1Validator,
  phaseANativeItemSemanticV1Validator,
  phaseANativeScriptsV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativePayloads (
  phaseANativeAllOrAnyContainerFramePayloadSemanticV1Validator,
  phaseANativeAllOrAnyEmptyContainerPayloadSemanticV1Validator,
  phaseANativeAtLeastContainerFramePayloadSemanticV1Validator,
  phaseANativeAtLeastEmptyContainerPayloadSemanticV1Validator,
  phaseANativeTimelockPayloadSemanticV1Validator,
  phaseANativeTokenHeadSemanticV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativeSignatures (
  phaseANativeSignatureAboveLastPayloadSemanticV1Validator,
  phaseANativeSignatureBelowFirstPayloadSemanticV1Validator,
  phaseANativeSignatureBetweenPayloadSemanticV1Validator,
  phaseANativeSignatureEmptyPayloadSemanticV1Validator,
  phaseANativeSignatureMembershipPayloadSemanticV1Validator,
 )
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Validation Trace Phase-A Native Scripts Validators"
    [ testCase "phase validator cancels its own computation thread" $
        psucceeds $ cancelledPreparationWith threadName
    , testCase "advance semantic validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith phaseANativeAdvanceSemanticV1Validator threadName
    , testCase "item semantic validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith phaseANativeItemSemanticV1Validator threadName
    , testCase "frame semantic validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith phaseANativeFrameSemanticV1Validator threadName
    , testCase "frame semantic validator rejects cancellation of another thread" $
        pfails $ cancelledSemanticWith phaseANativeFrameSemanticV1Validator otherThreadName
    , testCase "token-head semantic validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith phaseANativeTokenHeadSemanticV1Validator threadName
    , testCase "timelock payload validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith phaseANativeTimelockPayloadSemanticV1Validator threadName
    , testCase "all/any framed payload validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith phaseANativeAllOrAnyContainerFramePayloadSemanticV1Validator threadName
    , testCase "all/any empty payload validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith phaseANativeAllOrAnyEmptyContainerPayloadSemanticV1Validator threadName
    , testCase "at-least framed payload validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith phaseANativeAtLeastContainerFramePayloadSemanticV1Validator threadName
    , testCase "at-least empty payload validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith phaseANativeAtLeastEmptyContainerPayloadSemanticV1Validator threadName
    , testCase "signature membership payload validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith phaseANativeSignatureMembershipPayloadSemanticV1Validator threadName
    , testCase "empty signer-set payload validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith phaseANativeSignatureEmptyPayloadSemanticV1Validator threadName
    , testCase "below-first signer payload validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith phaseANativeSignatureBelowFirstPayloadSemanticV1Validator threadName
    , testCase "above-last signer payload validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith phaseANativeSignatureAboveLastPayloadSemanticV1Validator threadName
    , testCase "between-signers payload validator cancels its own computation thread" $
        psucceeds $ cancelledSemanticWith phaseANativeSignatureBetweenPayloadSemanticV1Validator threadName
    ]

cancelledPreparationWith :: forall s. BS.ByteString -> Term s PUnit
cancelledPreparationWith cancellationName =
  phaseANativeScriptsV1Validator
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
resolverHashes = pdata $ go 14
  where
    go :: forall t. Integer -> Term t (PBuiltinList (PAsData PScriptHash))
    go 0 = pnil
    go count =
      pcons
        # pdata (pconstant $ ScriptHash $ toBuiltin $ BS.replicate 28 $ fromIntegral count)
        # go (count - 1)
