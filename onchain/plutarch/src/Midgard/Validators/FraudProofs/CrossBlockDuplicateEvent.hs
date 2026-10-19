module Midgard.Validators.FraudProofs.CrossBlockDuplicateEvent (
  crossBlockDuplicateEventStep01Validator,
  crossBlockDuplicateEventStep02Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pcontinue, pfinalize)
import Midgard.FraudProofs.CrossBlockDuplicateEvent
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstep)

crossBlockDuplicateEventStep01Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
crossBlockDuplicateEventStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep01Args
        { pstep01Args'inputIndex
        , pstep01Args'outputIndex
        , pstep01Args'hubRefInputIndex
        , pstep01Args'stateQueueNodeRefInputIndex
        , pstep01Args'committedEvent
        } <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
      committedEvent <- plet $ pfromData pstep01Args'committedEvent
      pcontinue
        computationThreadPolicy
        (pexpectDatum datum)
        (pfromData pstep01Args'inputIndex)
        (pfromData pstep01Args'outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs) $ \_ownScriptHash threadName _prover _inputState outputScriptHash outputStateData ->
          pverifyChallengedHeaderV1
            threadName
            hubOracle
            (pfromData pstep01Args'hubRefInputIndex)
            (pfromData pstep01Args'stateQueueNodeRefInputIndex)
            (pfromData ptxInfo'referenceInputs) $ \header headerHash hubDatum -> P.do
              PHubOracleDatum {phubOracle'settlement} <- pmatch hubDatum
              expected <- plet $ pcon $ PStep02State
                (pdata headerHash)
                phubOracle'settlement
                (pcommittedEventKindV1 committedEvent)
                (pcommittedEventKeyV1 committedEvent)
              pexpecting (pverifyCommittedEventMembershipV1 header committedEvent) $
                pexpecting (outputScriptHash #== step02ScriptHash) $
                  pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

crossBlockDuplicateEventStep02Validator ::
  forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
crossBlockDuplicateEventStep02Validator = plam $ \fraudProofPolicy fraudProofAddress computationThreadPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep02Args
        { pstep02Args'inputIndex
        , pstep02Args'outputIndex
        , pstep02Args'fraudProofMintRedeemerIndex
        , pstep02Args'settlementRefInputIndex
        , pstep02Args'settledEvent
        } <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      settledEvent <- plet $ pfromData pstep02Args'settledEvent
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
        (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ownScriptHash threadName _prover inputState -> P.do
          state@PStep02State {pstep02State'challengedHeaderHash} <-
            pmatch $ pexpectStateAs @PStep02State inputState
          pexpecting (pfraudCategoryIsCrossBlockDuplicateEventV1 # threadName) $
            pexpecting
              (pfromData pstep02State'challengedHeaderHash #== pchallengedHeaderHashOfV1 # threadName) $
                plet
                  ( pverifyConfirmedDuplicateV1
                      (pcon state)
                      (pfromData pstep02Args'settlementRefInputIndex)
                      settledEvent
                      (pfromData ptxInfo'referenceInputs)
                  )
                  (\_settledHeaderHash -> pconstant True)
