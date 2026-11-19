module Midgard.Validators.FraudProofs.RedeemerCanonicity (
  redeemerCanonicityStep01Validator,
  redeemerCanonicityStep02Validator,
  redeemerCanonicityStep03Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (PNativeTxAnchorV1 (PWitnessAnchor), popenedFieldView)
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Types (PNativeTxCompact (..), PVerifiedMidgardNativeTxCompact (..))
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.RedeemerCanonicity
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.NativeTxFieldAccess (pfieldItemAt)
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pstep)

redeemerCanonicityStep01Validator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PScriptHash
        :--> PScriptContext
        :--> PUnit
    )
redeemerCanonicityStep01Validator = plam $ \step02Hash computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep01Args{pstep01Args'source, pstep01Args'redeemerIndex} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      pmatch (pfromData pstep01Args'source) $ \case
        PAcceptedSource inclusion ->
          ppassNativeTxToNextStepCarried
            computationThreadPolicy
            hubOracle
            datum
            (pfromData inclusion)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'referenceInputs)
            (pfromData ptxInfo'outputs)
            (pto $ pto $ pfromData ptxInfo'redeemers)
            $ \_ _ _ _ nextHash nextState _ _ verified -> P.do
              PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
              PNativeTxCompact{pcompact'witnessSetHash} <- pmatch pverified'txCompact
              let expected =
                    pbindRedeemerV1
                      # (Subject.pbindAcceptedSubject # verified)
                      # pcompact'witnessSetHash
                      # pfromData pstep01Args'redeemerIndex
              nextHash #== step02Hash #&& nextState #== pforgetData (pdata expected)
        PForcedSource inputIndex outputIndex header membership direction ->
          pcontinue
            computationThreadPolicy
            (pexpectDatum datum)
            (pfromData inputIndex)
            (pfromData outputIndex)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_ threadName _ _ nextHash nextState -> P.do
              subject <-
                plet $
                  Subject.pbindForcedSubjectToThread
                    # pto (pfromData threadName)
                    # pfromData header
                    # pfromData membership
                    # pfromData direction
              PRootMembershipProof{prootMembership'value} <- pmatch $ pfromData membership
              PForcedInclusionTxV1{pforcedTx'txId, pforcedTx'source} <-
                pmatch $ pfromData $ punsafeCoerce @(PAsData PForcedInclusionTxV1) prootMembership'value
              PNativeTxProofSourceV1{..} <- pmatch $ pfromData pforcedTx'source
              PPair verified _ <-
                pmatch $
                  pverifyNativeTxProofSourceV1
                    # pfromData pforcedTx'txId
                    # pfromData pnativeSource'compactCbor
                    # pfromData pnativeSource'witnessSetCompactCbor
                    # pfromData pnativeSource'fieldPreimageLengthsCbor
              PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
              PNativeTxCompact{pcompact'witnessSetHash} <- pmatch pverified'txCompact
              let expected =
                    pbindRedeemerV1
                      # subject
                      # pcompact'witnessSetHash
                      # pfromData pstep01Args'redeemerIndex
              nextHash #== step02Hash #&& nextState #== pforgetData (pdata expected)

redeemerCanonicityStep02Validator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
redeemerCanonicityStep02Validator = plam $ \step03Hash computationThreadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep02Args{pstep02Args'inputIndex, pstep02Args'outputIndex, pstep02Args'opening} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
      pcontinue
        computationThreadPolicy
        (pexpectDatum datum)
        (pfromData pstep02Args'inputIndex)
        (pfromData pstep02Args'outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_ _ _ inputState nextHash nextState -> P.do
          bound@PBoundRedeemerV1{pboundRedeemer'subject, pboundRedeemer'witnessSetHash, pboundRedeemer'redeemerIndex} <-
            pmatch $ pexpectStateAs @PBoundRedeemerV1 inputState
          Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData pboundRedeemer'subject
          view <-
            plet $
              popenedFieldView
                # pfromData pstep02Args'opening
                # pcon
                  ( PWitnessAnchor
                      psubject'transactionId
                      pboundRedeemer'witnessSetHash
                  )
                # predeemerFieldIndex
                # pfromData ptxInfo'referenceInputs
                # certificatePolicy
          let expected =
                pauthenticateItemV1
                  # pcon bound
                  # (pfieldItemAt # view # pfromData pboundRedeemer'redeemerIndex)
          nextHash #== step03Hash #&& nextState #== pforgetData (pdata expected)

redeemerCanonicityStep03Validator ::
  forall s.
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
redeemerCanonicityStep03Validator = plam $ \fraudPolicy fraudAddress computationThreadPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep03Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep03Args{pstep03Args'inputIndex, pstep03Args'outputIndex, pstep03Args'fraudProofMintRedeemerIndex} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      pfinalize
        computationThreadPolicy
        fraudPolicy
        fraudAddress
        (pexpectDatum datum)
        (pfromData pstep03Args'inputIndex)
        (pfromData pstep03Args'outputIndex)
        (pfromData pstep03Args'fraudProofMintRedeemerIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        (pto $ pto $ pfromData ptxInfo'redeemers)
        $ \_ _ _ inputState ->
          pterminalContradictionV1 # pexpectStateAs @PTerminalStateV1 inputState
