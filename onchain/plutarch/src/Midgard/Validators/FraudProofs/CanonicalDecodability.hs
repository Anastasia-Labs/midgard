module Midgard.Validators.FraudProofs.CanonicalDecodability (
  canonicalDecodabilityStep01Validator,
  canonicalDecodabilityStep02Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.CanonicalDecodability
import Midgard.FraudProofs.Common (pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.NativeTx.Types (PNativeTxCompact (..), PVerifiedMidgardNativeTxCompact (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstep)

canonicalDecodabilityStep01Validator ::
  forall s.
  Term s
    ( PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash
        :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit
    )
canonicalDecodabilityStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep01Args {pstep01Args'inclusion, pstep01Args'claim} <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      ppassNativeTxToNextStepCarried computationThreadPolicy hubOracle datum
        (pfromData pstep01Args'inclusion) ownOutRef
        (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs)
        (pto $ pto $ pfromData ptxInfo'redeemers) $
        \_ownScriptHash _threadName _prover _inputState outputScriptHash outputStateData _header badTxId verified -> P.do
          PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch verified
          PNativeTxCompact {pcompact'validityCode} <- pmatch pverified'txCompact
          derived <- plet $ pcommittedFieldVerdictV1 verified (pfromData pstep01Args'claim)
            (pfromData ptxInfo'referenceInputs) certificatePolicy
          PCommittedFieldVerdictV1 {pcommittedVerdict'fieldIndex, pcommittedVerdict'verdict} <- pmatch derived
          expected <- plet $ pcon $ PStep02State (pdata badTxId) pcommittedVerdict'fieldIndex pcommittedVerdict'verdict
          pexpecting (pcompact'validityCode #== 0) $
            pexpecting (outputScriptHash #== step02ScriptHash) $
              pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

canonicalDecodabilityStep02Validator ::
  forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
canonicalDecodabilityStep02Validator = plam $ \fraudProofPolicy fraudProofAddress computationThreadPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep02Args {pstep02Args'inputIndex, pstep02Args'outputIndex, pstep02Args'fraudProofMintRedeemerIndex} <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      pfinalize computationThreadPolicy fraudProofPolicy fraudProofAddress (pexpectDatum datum)
        (pfromData pstep02Args'inputIndex) (pfromData pstep02Args'outputIndex)
        (pfromData pstep02Args'fraudProofMintRedeemerIndex) ownOutRef
        (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $
        \_ownScriptHash _threadName _prover inputState -> P.do
          PStep02State {pstep02State'fieldIndex, pstep02State'verdict} <-
            pmatch $ pexpectStateAs @PStep02State inputState
          pexpecting
            (pisCanonicalDecodabilityViolationV1 # pfromData pstep02State'fieldIndex # pfromData pstep02State'verdict)
            (pconstant True)
