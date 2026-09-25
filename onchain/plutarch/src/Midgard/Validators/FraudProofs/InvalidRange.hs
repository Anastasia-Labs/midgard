-- | Bind accepted or forced carriage, then adjudicate the exact interval claim.
module Midgard.Validators.FraudProofs.InvalidRange (
  invalidRangeStep01Validator,
  invalidRangeStep02Validator,
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

import DesignPatterns.ValidityRangeNormalization (PNormalizedTimeRange (..))
import Midgard.Env qualified as Env
import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.InvalidRange
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PHeaderV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pstateIsAbsent,
  pstep,
 )
import Plutarch.Unsafe (punsafeCoerce)

{- | Aiken @validators/fraud-proofs/invalid-range/step-01.ak@'s
@normalize_native_validity_range@.

The two sentinels decide the shape, and the exclusive upper becomes inclusive on
the way in. A bounded range whose lower exceeds its upper is @InvalidRange@ rather
than a @ClosedRange@ nothing satisfies — which is what lets step-02 convict it
without a separate emptiness test.
-}
pnormalizeNativeValidityRange ::
  forall (s :: S). Term s (PNativeTxBodyCompact :--> PNormalizedTimeRange)
pnormalizeNativeValidityRange = phoistAcyclic $
  plam $ \body -> P.do
    PNativeTxBodyCompact {pbodyCompact'validityIntervalStart, pbodyCompact'validityIntervalEnd} <-
      pmatch body
    lower <- plet pbodyCompact'validityIntervalStart
    exclusiveUpper <- plet pbodyCompact'validityIntervalEnd
    lowerAbsent <- plet $ lower #== Env.pposixTimeNone
    upperAbsent <- plet $ exclusiveUpper #== Env.pposixTimeNone
    pif
      lowerAbsent
      ( pif
          upperAbsent
          (pcon PAlways)
          (pcon (PFromNegInf {pntr'upperOnly = pdata (exclusiveUpper - 1)}))
      )
      ( pif
          upperAbsent
          (pcon (PToPosInf {pntr'lowerOnly = pdata lower}))
          ( plet (exclusiveUpper - 1) $ \upper ->
              pif
                (lower #> upper)
                (pcon PInvalidRange)
                (pcon (PClosedRange {pntr'lower = pdata lower, pntr'upper = pdata upper}))
          )
      )

{- | Aiken @validators/fraud-proofs/invalid-range/step-01.ak@.

Binds an accepted transaction and writes the block's canonical replay slot
alongside its normalised range.

Unlike its siblings this step requires the thread's incoming state to be
__absent__ — Aiken writes @expect None = m_input_state_data@ where the other
families bind it and ignore it. The port keeps the check where Aiken has it.
-}
invalidRangeStep01Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-02's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
invalidRangeStep01Validator = plam $
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
                $ \_ _ _ inputState outputScriptHash outputStateData header _ verified ->
                  pstateIsAbsent inputState
                    #&& outputScriptHash
                    #== step02ValidatorScriptHash
                    #&& outputStateData
                    #== expectedState (Subject.pbindAcceptedSubject # verified) (pfromData header) verified
            PForcedSource inputIndex outputIndex header membership direction ->
              pcontinue
                computationThreadTokenPolicyId
                (pexpectDatum datum)
                (pfromData inputIndex)
                (pfromData outputIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                $ \_ threadName _ inputState outputScriptHash outputStateData -> P.do
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
                  pstateIsAbsent inputState
                    #&& pverified'txId
                    #== pfromData psubject'transactionId
                    #&& outputScriptHash
                    #== step02ValidatorScriptHash
                    #&& outputStateData
                    #== expectedState subject (pfromData header) verified
  where
    expectedState subject header verified =
      pmatch header $ \PHeaderV1 {pheader'blockSlot} ->
        pmatch verified $ \PVerifiedMidgardNativeTxCompact {pverified'txCompact} ->
          pmatch pverified'txCompact $ \PNativeTxCompact {pcompact'body} ->
            pforgetData $
              pdata $
                pcon $
                  PStep02State
                    (pdata subject)
                    pheader'blockSlot
                    (pdata $ pnormalizeNativeValidityRange # pcompact'body)

{- | Aiken @validators/fraud-proofs/invalid-range/step-02.ak@.

The conviction, a case analysis over the four shapes a normalised range can take
that assert anything at all.
-}
invalidRangeStep02Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- fraud proof token policy
        :--> PAsData PAddress -- fraud proof token address
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PScriptContext
        :--> PUnit
    )
invalidRangeStep02Validator = plam $
  \fraudProofTokenPolicyId fraudProofTokenAddress computationThreadTokenPolicyId ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PStep02Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PStep02Args
            { pstep02Args'inputIndex
            , pstep02Args'outputIndex
            , pstep02Args'fraudProofMintRedeemerIndex
            } <-
            pmatch args
          PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
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
              PStep02State
                { pstep02State'subject
                , pstep02State'blockSlot
                , pstep02State'badTxNormalizedValidityRange
                } <-
                pmatch (pexpectStateAs @PStep02State mInputStateData)
              pterminalContradiction
                # pfromData pstep02State'subject
                # pfromData pstep02State'badTxNormalizedValidityRange
                # pfromData pstep02State'blockSlot
