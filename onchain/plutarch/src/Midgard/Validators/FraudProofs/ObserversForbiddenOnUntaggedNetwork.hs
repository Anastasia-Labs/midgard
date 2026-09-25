module Midgard.Validators.FraudProofs.ObserversForbiddenOnUntaggedNetwork (
  observersForbiddenOnUntaggedNetworkStep01Validator,
  observersForbiddenOnUntaggedNetworkStep02Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (
  PFieldOpeningV1 (..),
  PNativeTxAnchorV1 (PBodyAnchor),
  popenedFieldView,
  prequiredObserversFieldIndex,
 )
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxCompactCborV1, pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.FraudProofs.ObserversForbiddenOnUntaggedNetwork
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.NativeTxFieldAccess (pfieldItemCount)
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pstep)

observersForbiddenOnUntaggedNetworkStep01Validator ::
  forall s.
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PScriptHash
        :--> PScriptContext
        :--> PUnit
    )
observersForbiddenOnUntaggedNetworkStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep01Args{pstep01Args'source} <- pmatch args
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
            $ \_ _ _ _ nextScriptHash nextState _ _ verified ->
              pmatch verified $ \PVerifiedMidgardNativeTxCompact{pverified'txCompact} ->
                pmatch pverified'txCompact $ \PNativeTxCompact{pcompact'body} ->
                  pmatch pcompact'body $ \PNativeTxBodyCompact{pbodyCompact'networkId} ->
                    let expected =
                          pbindStateV1
                            # (Subject.pbindAcceptedSubject # verified)
                            # pbodyCompact'networkId
                     in nextScriptHash
                          #== step02ScriptHash
                          #&& nextState
                          #== pforgetData (pdata expected)
        PForcedSource inputIndex outputIndex header membership direction ->
          pcontinue
            computationThreadPolicy
            (pexpectDatum datum)
            (pfromData inputIndex)
            (pfromData outputIndex)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_ threadName _ _ nextScriptHash nextState -> P.do
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
              PNativeTxCompact{pcompact'body} <- pmatch pverified'txCompact
              PNativeTxBodyCompact{pbodyCompact'networkId} <- pmatch pcompact'body
              let expected = pbindStateV1 # subject # pbodyCompact'networkId
              nextScriptHash
                #== step02ScriptHash
                #&& nextState
                #== pforgetData (pdata expected)

observersForbiddenOnUntaggedNetworkStep02Validator ::
  forall s.
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
observersForbiddenOnUntaggedNetworkStep02Validator = plam $ \fraudProofPolicy fraudProofAddress computationThreadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep02Args{..} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
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
        $ \_ _ _ inputState -> P.do
          state@PStateV1{pstate'subject, pstate'networkId} <-
            pmatch $ pexpectStateAs @PStateV1 inputState
          Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData pstate'subject
          PBodyFieldOpening{pbodyOpening'nativeTxCompactCbor} <- pmatch $ pfromData pstep02Args'observerOpening
          PVerifiedMidgardNativeTxCompact{pverified'txCompact} <-
            pmatch $
              pverifyNativeTxCompactCborV1
                # pfromData psubject'transactionId
                # pfromData pbodyOpening'nativeTxCompactCbor
          PNativeTxCompact{pcompact'body} <- pmatch pverified'txCompact
          PNativeTxBodyCompact{pbodyCompact'scriptIntegrityHash, pbodyCompact'networkId} <- pmatch pcompact'body
          view <-
            plet $
              popenedFieldView
                # pfromData pstep02Args'observerOpening
                # pcon (PBodyAnchor psubject'transactionId)
                # prequiredObserversFieldIndex
                # pfromData ptxInfo'referenceInputs
                # certificatePolicy
          pbodyCompact'networkId
            #== pfromData pstate'networkId
            #&& pterminalContradictionV1
            # pcon state
            # (pfieldItemCount # view)
            # pbodyCompact'scriptIntegrityHash
