{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsNativeScriptDecodingStep01 (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.NativeScriptDecoding.Step01 (nativeScriptDecodingStep01Validator)
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests = testGroup "Native-script decoding step 01"
  [ testCase "binds an accepted normal transaction" $ psucceeds $ run normalContext
  , testCase "records a forced source for either direction" $ do
      psucceeds $ run (forcedContext 0)
      psucceeds $ run (forcedContext 1)
  , testCase "rejects an unknown direction" $ pfails $ run (forcedContext 2)
  , testCase "rejects a normal leaf with an invalid scalar" $ pfails $ run invalidNormalContext
  , testCase "rejects a forged transactions root" $ pfails $ run forgedRootContext
  , testCase "cancels under the prover signature" $ psucceeds $ run cancelContext
  , testCase "rejects an unsigned cancellation" $ pfails $ run unsignedCancelContext
  ]

run :: forall s. ScriptContext -> Term s PUnit
run ctx = nativeScriptDecodingStep01Validator
  # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
  # pdata (pconstant ctPolicy)
  # pdata (pconstant hubOracleHash)
  # pconstant ctx

bindState :: Integer -> Integer -> BS.ByteString -> PD.Data
bindState direction sourceKind verifiedTxId = PD.Constr 0 [PD.I direction, PD.I sourceKind, PD.B verifiedTxId]

bindNormalRedeemer :: BS.ByteString -> BS.ByteString -> BS.ByteString -> PD.Data
bindNormalRedeemer txId compact root =
  PD.Constr 1 [PD.Constr 0 [inclusionArgs txId compact root]]

recordForcedRedeemer :: Integer -> PD.Data
recordForcedRedeemer direction = PD.Constr 1 [PD.Constr 1 [PD.I direction, PD.I 0, PD.I 0]]

acceptedCbor, rejectedCbor, acceptedRoot, rejectedRoot :: BS.ByteString
acceptedCbor = sourceCborWithValidity txScriptSpend 0
rejectedCbor = sourceCborWithValidity txScriptSpend 1
acceptedRoot = singleEntryPhasRoot txScriptSpendId acceptedCbor
rejectedRoot = singleEntryPhasRoot txScriptSpendId rejectedCbor

normalContext :: ScriptContext
normalContext = normalContextWith acceptedCbor acceptedRoot acceptedRoot

invalidNormalContext :: ScriptContext
invalidNormalContext = normalContextWith rejectedCbor rejectedRoot rejectedRoot

forgedRootContext :: ScriptContext
forgedRootContext = normalContextWith acceptedCbor otherRoot acceptedRoot

normalContextWith :: BS.ByteString -> BS.ByteString -> BS.ByteString -> ScriptContext
normalContextWith compact proofRoot headerRoot =
  spendContext
    (stepDatum Nothing)
    (bindNormalRedeemer txScriptSpendId compact proofRoot)
    [threadInput]
    [stepOutput nextScript (Just $ bindState 0 0 txScriptSpendId)]
    (referenceInputsWithTransactionsRoot $ commitCountedRoot transactionsDomain headerRoot l2Count)
    [phasEntry proofRoot txScriptSpendId compact]
    mempty

forcedContext :: Integer -> ScriptContext
forcedContext direction =
  spendContext
    (stepDatum Nothing)
    (recordForcedRedeemer direction)
    [threadInput]
    [stepOutput nextScript (Just $ bindState direction 1 "")]
    []
    []
    mempty

cancelContext, unsignedCancelContext :: ScriptContext
cancelContext = cancellationContext threadName
unsignedCancelContext = cancellationContext otherThreadName

cancellationContext :: BS.ByteString -> ScriptContext
cancellationContext burnedName =
  spendContext
    (stepDatum Nothing)
    cancelRedeemer
    [threadInput]
    []
    []
    [cancelMintEntry burnedName]
    mempty
