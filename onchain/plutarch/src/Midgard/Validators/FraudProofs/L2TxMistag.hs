module Midgard.Validators.FraudProofs.L2TxMistag (
  l2TxMistagStep01Validator,
  l2TxMistagStep02Validator,
) where

import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.L2TxMistag (PStep02Args (..), PStep02State (..))
import Midgard.FraudProofs.NativeTx.Types (PNativeTxCompact (..), PVerifiedMidgardNativeTxCompact (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstateIsAbsent, pstep)

l2TxMistagStep01Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
l2TxMistagStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch computationThreadPolicy datum redeemer ownOutRef txInfo $ \carriage -> P.do
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      ppassNativeTxToNextStepCarried
        computationThreadPolicy
        hubOracle
        datum
        carriage
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'referenceInputs)
        (pfromData ptxInfo'outputs)
        (pto $ pto $ pfromData ptxInfo'redeemers)
        $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData _header badTxId badTxView -> P.do
          PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch badTxView
          PNativeTxCompact {pcompact'validityCode} <- pmatch pverified'txCompact
          expectedState <- plet $ pcon $ PStep02State (pdata badTxId) (pdata pcompact'validityCode)
          pexpecting (pstateIsAbsent inputState) $
            pexpecting (pcompact'validityCode #/= 0) $
              pexpecting (outputScriptHash #== step02ScriptHash) $
                pexpecting (outputStateData #== pforgetData (pdata expectedState)) (pconstant True)

l2TxMistagStep02Validator ::
  forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
l2TxMistagStep02Validator = plam $ \fraudProofPolicy fraudProofAddress computationThreadPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep02Args {pstep02Args'inputIndex, pstep02Args'outputIndex, pstep02Args'fraudProofMintRedeemerIndex} <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      pfinalize
        computationThreadPolicy
        fraudProofPolicy
        fraudProofAddress
        (pexpectDatum datum)
        (pfromData pstep02Args'inputIndex)
        (pfromData pstep02Args'outputIndex)
        (pfromData pstep02Args'fraudProofMintRedeemerIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        (pto $ pto $ pfromData ptxInfo'redeemers)
        $ \_ownScriptHash _threadName _prover inputState -> P.do
          PStep02State {pstep02State'committedValidityCode} <- pmatch $ pexpectStateAs @PStep02State inputState
          pexpecting (pfromData pstep02State'committedValidityCode #/= 0) (pconstant True)
