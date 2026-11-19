module Midgard.Validators.FraudProofs.DoubleWithdraw (
  doubleWithdrawStep01Validator,
  doubleWithdrawStep02Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..), PTxOutRef)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize)
import Midgard.FraudProofs.DoubleWithdraw
import Midgard.LedgerState (PWithdrawalBody (..), PWithdrawalInfo (..), PWithdrawalValidity (..))
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstateIsAbsent, pstep)

doubleWithdrawStep01Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
doubleWithdrawStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep01Args
        { pstep01Args'inputIndex
        , pstep01Args'outputIndex
        , pstep01Args'hubRefInputIndex
        , pstep01Args'stateQueueNodeRefInputIndex
        , pstep01Args'committedWithdrawal
        } <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
      membership <- plet $ pfromData pstep01Args'committedWithdrawal
      pcontinue computationThreadPolicy (pexpectDatum datum)
        (pfromData pstep01Args'inputIndex) (pfromData pstep01Args'outputIndex) ownOutRef
        (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $
        \_ownScriptHash threadName _prover inputState outputScriptHash outputStateData ->
          pverifyChallengedHeaderV1 threadName hubOracle
            (pfromData pstep01Args'hubRefInputIndex)
            (pfromData pstep01Args'stateQueueNodeRefInputIndex)
            (pfromData ptxInfo'referenceInputs) $ \header headerHash -> P.do
              PRootMembershipProof {prootMembership'key, prootMembership'value} <- pmatch membership
              PWithdrawalInfo {pwithdrawalInfo'body, pwithdrawalInfo'validity} <-
                pmatch $ pfromData $ punsafeCoerce @(PAsData PWithdrawalInfo) prootMembership'value
              PWithdrawalBody {pwithdrawalBody'l2Outref} <- pmatch $ pfromData pwithdrawalInfo'body
              expected <- plet $ pcon $ PStep02State
                (pdata headerHash)
                (punsafeCoerce @(PAsData PTxOutRef) prootMembership'key)
                (punsafeCoerce @(PAsData PTxOutRef) pwithdrawalBody'l2Outref)
              pexpecting (pstateIsAbsent inputState) $
                pexpecting (pverifyCommittedWithdrawalMembershipV1 header membership) $
                  pexpecting
                    (pmatch (pfromData pwithdrawalInfo'validity) (\case PWithdrawalIsValid -> pconstant True; _ -> pconstant False)) $
                    pexpecting (outputScriptHash #== step02ScriptHash) $
                      pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

doubleWithdrawStep02Validator ::
  forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
doubleWithdrawStep02Validator = plam $ \fraudProofPolicy fraudProofAddress computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep02Args
        { pstep02Args'inputIndex
        , pstep02Args'outputIndex
        , pstep02Args'fraudProofMintRedeemerIndex
        , pstep02Args'hubRefInputIndex
        , pstep02Args'stateQueueNodeRefInputIndex
        , pstep02Args'committedWithdrawal
        } <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      membership <- plet $ pfromData pstep02Args'committedWithdrawal
      pfinalize computationThreadPolicy fraudProofPolicy fraudProofAddress (pexpectDatum datum)
        (pfromData pstep02Args'inputIndex) (pfromData pstep02Args'outputIndex)
        (pfromData pstep02Args'fraudProofMintRedeemerIndex) ownOutRef
        (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $
        \_ownScriptHash threadName _prover inputState -> P.do
          state@PStep02State {pstep02State'challengedHeaderHash} <-
            pmatch $ pexpectStateAs @PStep02State inputState
          pverifyChallengedHeaderV1 threadName hubOracle
            (pfromData pstep02Args'hubRefInputIndex)
            (pfromData pstep02Args'stateQueueNodeRefInputIndex)
            (pfromData ptxInfo'referenceInputs) $ \header headerHash ->
              pexpecting (pfromData pstep02State'challengedHeaderHash #== headerHash) $
                pexpecting (pverifyCommittedWithdrawalMembershipV1 header membership) $
                  pexpecting (pdoubleWithdrawFaultIsEstablishedV1 (pcon state) membership) (pconstant True)
