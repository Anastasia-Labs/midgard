{- | Direction-complete zero-input thread: bind the authenticated subject, then
open field 0 and apply the subject's contradiction polarity.
-}
module Midgard.Validators.FraudProofs.ZeroInput (
  zeroInputStep01Validator,
  zeroInputStep02Validator,
) where

import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (
  PNativeTxAnchorV1 (..),
  popenedFieldView,
  pspendInputsFieldIndex,
 )
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Types (
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.ZeroInput
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.NativeTxFieldAccess (pfieldItemCount)
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pstep,
 )
import Plutarch.Unsafe (punsafeCoerce)

{- | Aiken @validators/fraud-proofs/zero-input/step-01.ak@.

Binds the disputed transaction to a committed block and forwards its subject. No block
roots travel: step-02 concludes from the transaction alone.

Unlike the double-spend family's first step, this one does __not__ require the
prior state to be absent. That is the Aiken original's shape and it is not an
oversight: the thread's own token asset name pins which block and which category
the thread belongs to, and a thread carrying stale state still has to produce the
exact output state below.
-}
zeroInputStep01Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-02's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
zeroInputStep01Validator = plam $
  \step02ValidatorScriptHash computationThreadTokenPolicyId hubOracle ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PStep01Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PStep01Args source <- pmatch args
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
          pmatch (pfromData source) $ \case
            PAcceptedSource carriage ->
              ppassNativeTxToNextStepCarried
                computationThreadTokenPolicyId
                hubOracle
                datum
                (pfromData carriage)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'referenceInputs)
                (pfromData ptxInfo'outputs)
                (pto (pto (pfromData ptxInfo'redeemers)))
                $ \_ _ _ _ outputScriptHash outputStateData _ _ verified ->
                  outputScriptHash
                    #== step02ValidatorScriptHash
                    #&& outputStateData
                    #== pforgetData (pdata $ pbindState # (Subject.pbindAcceptedSubject # verified))
            PForcedSource inputIndex outputIndex header membership direction ->
              pcontinue
                computationThreadTokenPolicyId
                (pexpectDatum datum)
                (pfromData inputIndex)
                (pfromData outputIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                $ \_ threadName _ _ outputScriptHash outputStateData -> P.do
                  subject <-
                    plet $
                      Subject.pbindForcedSubjectToThread
                        # pto (pfromData threadName)
                        # pfromData header
                        # pfromData membership
                        # pfromData direction
                  PRootMembershipProof {prootMembership'value} <- pmatch $ pfromData membership
                  PForcedInclusionTxV1 {pforcedTx'txId, pforcedTx'source} <-
                    pmatch $
                      pfromData (punsafeCoerce prootMembership'value)
                  PNativeTxProofSourceV1 {..} <- pmatch $ pfromData pforcedTx'source
                  PPair verified _ <-
                    pmatch $
                      pverifyNativeTxProofSourceV1
                        # pfromData pforcedTx'txId
                        # pfromData pnativeSource'compactCbor
                        # pfromData pnativeSource'witnessSetCompactCbor
                        # pfromData pnativeSource'fieldPreimageLengthsCbor
                  PVerifiedMidgardNativeTxCompact {pverified'txId} <- pmatch verified
                  Subject.PVerdictSubject {Subject.psubject'transactionId} <- pmatch subject
                  pverified'txId
                    #== pfromData psubject'transactionId
                    #&& outputScriptHash
                    #== step02ValidatorScriptHash
                    #&& outputStateData
                    #== pforgetData (pdata $ pbindState # subject)

{- | Aiken @validators/fraud-proofs/zero-input/step-02.ak@.

Concludes the proof from field 0 and the authenticated verdict direction.
-}
zeroInputStep02Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- fraud proof token policy
        :--> PAsData PAddress -- fraud proof token address
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PCurrencySymbol -- field preimage certificate policy
        :--> PScriptContext
        :--> PUnit
    )
zeroInputStep02Validator =
  plam $
    \fraudProofTokenPolicyId
     fraudProofTokenAddress
     computationThreadTokenPolicyId
     fieldPreimageCertificatePolicyId
     ctx ->
        pstep ctx $ \datum redeemer ownOutRef txInfo ->
          pdispatch @_ @PStep02Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
            \args -> P.do
              PStep02Args
                { pstep02Args'inputIndex
                , pstep02Args'outputIndex
                , pstep02Args'fraudProofMintRedeemerIndex
                , pstep02Args'spendInputsOpening
                } <-
                pmatch args
              PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <-
                pmatch txInfo
              referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
              -- 1. The thread's own input must be authentic and reproduced at the
              --    fraud proof's spending address.
              pfinalize
                computationThreadTokenPolicyId
                fraudProofTokenPolicyId
                fraudProofTokenAddress
                (pexpectDatum datum)
                (pfromData pstep02Args'inputIndex)
                (pfromData pstep02Args'outputIndex)
                (pfromData pstep02Args'fraudProofMintRedeemerIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                (pto (pto (pfromData ptxInfo'redeemers)))
                $ \_ownScriptHash _threadTokenAssetName _fraudProver mInputStateData -> P.do
                  state <- plet $ pexpectStateAs @PStep02State mInputStateData
                  PStep02State {pstep02State'subject} <- pmatch state
                  Subject.PVerdictSubject {Subject.psubject'transactionId} <- pmatch $ pfromData pstep02State'subject
                  -- 2. Field 0 must hold no items.
                  spendInputsView <-
                    plet $
                      popenedFieldView
                        # pfromData pstep02Args'spendInputsOpening
                        # pcon (PBodyAnchor {pbodyAnchor'txId = psubject'transactionId})
                        # pspendInputsFieldIndex
                        # referenceInputs
                        # fieldPreimageCertificatePolicyId
                  pterminalContradiction # state # (pfieldItemCount # spendInputsView)
