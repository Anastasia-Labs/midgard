{- | Plutarch port of Aiken
@lib/midgard/fraud-proofs/transition-trace/proof.ak@.

A block's transition trace is a chain of steps: each step applies one event, and
each step's post-ledger root is the next one's pre-root. The trace is committed as
a counted root, and so are the four event sources and the event-to-step map. This
module holds the faults that can be stated about those commitments alone, without
replaying any event:

* __boundary__ — the trace does not begin at the block's prior ledger root, or
  does not end at its new one;
* __link__ — two adjacent steps do not meet;
* __event-to-step mismatch__ — a step and the map disagree about where an event
  is, or a step's phase disagrees with its own index;
* __source membership mismatch__ — an event is in the trace but not in any
  source, in a source but not the trace, or in a phase that is not its own.

The same module also implements the event transitions, L1-event faults, count and
duplicate faults, accepted-transaction mismatch, and the proof entry points.

=== Phase is derived twice, and both derivations are checked

An event's phase can be read three ways: off the step, off the step's /index/
against the header's four counts, and off the event key's own constructor. The
Aiken checks all three agree, and 'ptraceHasBadPhase' is the fault that fires when
they do not. That redundancy is deliberate — the index derivation is what ties a
step to the header's counts, and the key derivation is what stops a withdrawal
being filed as a deposit.

Note that @phase_for_step_index@ __aborts__ on an index outside the block, so it
sits behind a lazy @#||@ rather than inside a strict conjunction.

=== Four arms carrying one payload

@SourceMembershipProof@ and @SourceNonMembershipProof@ each have four
constructors carrying the same thing, differing only in which tree the proof is
against. In Aiken the type parameters distinguish them; on-chain those erase, and
the ported @RootMembershipProof@ carries its key and value as @Data@, so all four
arms are structurally identical.

Every dispatch here therefore reads the constructor tag with
'Midgard.Common.Utils.pconstrOf' rather than matching. Some of these arms happen
to have different bodies and would survive a @pmatch@ today; writing them all the
same way means none of them turns into the two-arms-one-body shape when somebody
factors a common body out later.

=== The L2 tree keys itself differently

Three of the four source trees are keyed by structured ids and store structured
values, so their tree bytes are CBOR. The transactions tree is keyed by a raw
transaction id and stores raw bytes, and both go in as themselves. Serialising
them would prepend a header and address a different slot, so the L2 arm passes
@pasByteStr@ where the others pass @pserialiseData@.
-}
module Midgard.FraudProofs.TransitionTrace.Proof (
  -- * Types
  PTraceBoundarySide (..),
  PSourceMembershipProof (..),
  PSourceNonMembershipProof (..),
  PSourceMembershipMismatchWitness (..),
  PLedgerDeleteWitness (..),
  PLedgerInsertWitness (..),
  PCountFaultWitness (..),
  PAcceptedTransactionTransitionMismatchWitness (..),

  -- * Header arithmetic
  pheaderCountsAreNonNegative,
  psourceCountSum,
  pphaseForStepIndex,
  peventKeyPhase,

  -- * Reading a source proof
  peventKeyFromSourceMembership,
  psourceMembershipPhase,
  psourceNonMembershipEventKey,

  -- * Membership
  pverifyEventToStepMembership,
  pverifyEventToStepNonMembership,
  pverifySourceMembership,
  pverifySourceNonMembership,
  pverifyTraceBindingToEventToStep,

  -- * Faults
  pvalidateTraceBoundary,
  pvalidateTraceLink,
  ptraceHasBadPhase,
  pvalidateEventToStepMismatch,
  pvalidateSourceMembershipMismatch,
  pvalidateDuplicateTraceEvent,
  pvalidateCountFault,
  pterminalAcceptancePostRoot,
  pvalidateAcceptedTransactionTransitionMismatch,

  -- * The ledger trie
  pledgerOutrefKey,
  pverifyLedgerMembership,
  pverifyLedgerNonMembership,
  pinsertRoot,
  pdeleteRoot,
  papplyDeleteWitness,
  papplyInsertWitness,

  -- * One-step transitions
  pvalidateWithdrawalOneStepBinding,
  pvalidateForcedTransactionOneStepBinding,
  pvalidateDepositOneStepBinding,
  pvalidateValidWithdrawalTransition,
  pvalidateInvalidWithdrawalNoOpTransition,
  pvalidateInvalidForcedTransactionNoOpTransition,
  pverifyL2SourceMembership,
  pvalidateL2OneStepBinding,
  pdoorBodyFieldItems,
  papplyL2Spends,
  papplyL2Outputs,
  pvalidateL2TransactionTransition,

  -- * The deposit projection
  PAuthenticatedDepositReference (..),
  pcardanoCredentialToMidgard,
  pcardanoStakeCredentialToMidgard,
  pdepositAddressToMidgard,
  pcardanoAssetPairsToMidgard,
  pprojectedDepositValue,
  pdepositDatumCbor,
  pprojectedDepositOutputCbor,
  pgetAuthenticatedDepositReference,
  pvalidateValidDepositTransition,

  -- * The L1-event faults
  POmittedDueL1EventWitness (..),
  POutOfWindowSourceEventWitness (..),
  ptimedL1EventIsDue,
  pforcedTxIsDue,
  pforcedEventIsDue,
  ptxOrderCompactBody,
  pgetWithdrawalDatumWithNft,
  pgetTxOrderDatumWithNft,
  pvalidateOmittedDueL1Event,
  pvalidateOutOfWindowSourceEvent,

  -- * The one-step dispatcher
  PInvalidOneStepTransitionWitness (..),
  pvalidateInvalidOneStepTransition,

  -- * The fault proof and its nine entry points
  PTransitionFault (..),
  PTransitionFaultProof (..),
  ptransitionTraceFraudCategoryId,
  pvalidateTransitionFaultProofEnvelope,
  pvalidateTransitionFault,
  pvalidateControlFaultProof,
  pvalidateSourceFaultProof,
  pvalidateWithdrawalFaultProof,
  pvalidateForcedFaultProof,
  pvalidateAcceptedTransactionFaultProof,
  pvalidateDepositFaultProof,
  pvalidateL1EventFaultProof,
  pvalidateDuplicateFaultProof,
  pvalidateTransitionFaultProof,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.Crypto (pblake2b_224)
import Plutarch.Builtin.Data (pasByteStr, pasConstr, pasInt, pasList, pconstrBuiltin, pserialiseData)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Plutarch.LedgerApi.AssocMap (PAssocMap (..))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  PCurrencySymbol (..),
  POutputDatum (..),
  PStakingCredential (..),
  PTokenName,
  PTxInInfo (..),
  PTxOut (..),
 )
import Plutarch.LedgerApi.Value (padaSymbolData, padaToken)
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.List (preverse)
import Plutarch.Repr.Scott (DeriveAsScottRec (..))

import Aiken.Cbor (pdeserialise)
import Midgard.Common.Types (PProof)
import Midgard.Common.Utils (pconstrOf, pgetAuthenticInputDatumWithNftAt)
import Midgard.Common.Value (pquantityOfValue, pvalueWithoutAsset)
import Midgard.FraudProofCatalogue (pidByteCount)
import Midgard.FraudProofs.FieldOpening (
  PAnchoredNativeTxV1,
  PNativeTxAnchorV1 (..),
  PNativeTxOpeningV1 (..),
  panchoredFieldWalk,
  panchoredNativeTx,
  panchoredNativeTxVersion,
  pfoldOpenedField,
  poutputsFieldIndex,
  pspendInputsFieldIndex,
  punanchoredValidityCodeOf,
 )
import Midgard.MpfProof qualified as MpfProof
import Midgard.NativeTxFieldAccess (PFieldCarriageV1 (..))
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxCompactCborV1)
import Midgard.FraudProofs.NativeTx.Components (
  pdecodeMidgardTxInputCbor,
  pdecodeMidgardTxOutputCbor,
  pencodeMidgardTxInput,
  pencodeMidgardTxOutput,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
  PMidgardAddress (..),
  PMidgardAssets,
  PMidgardCredential (..),
  PMidgardTxInput (..),
  PMidgardTxOutput (..),
  PMidgardValue (..),
 )
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.LedgerState (
  PDepositInfo (..),
  PHeaderHash,
  PEventToStepValue (..),
  PForcedInclusionTxV1 (..),
  PHeaderV1 (..),
  PMidgardTxValidity (..),
  PNativeTxProofSourceV1 (..),
  PTransitionPhase,
  PTransitionStep (..),
  PTxOrderEventV1 (..),
  PTxOrderPayloadV1 (..),
  PWithdrawalEvent (..),
  PWithdrawalBody (..),
  PWithdrawalInfo (..),
  PWithdrawalValidity (..),
  punsafeEventToKeyValuePair,
 )
import Midgard.TransitionTrace (
  PAdjacentTraceProof (..),
  PEventToStepProof (..),
  PIndexedTraceProof,
  PRootDomain (..),
  PRootCountProof (..),
  PRootMembershipProof (..),
  PRootNonMembershipProof (..),
  pmpfFromMidgardRoot,
  pverifyAdjacentTraceProof,
  pverifyIndexedTraceProof,
  pverifyRootCountProof,
  pverifyRootMembershipWithBytes,
  pverifyRootNonMembershipWithKeyBytes,
 )
import Midgard.UserEvents.TxOrder (PTxOrderDatum (..))
import Midgard.UserEvents.Withdrawal (PWithdrawalDatum (..))
import Midgard.ValidationClaim (
  PValidationClaimWitnessV1 (..),
  pcommittedClaimIsValid,
  pphaseTagForEventKeyTag,
 )
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase (PTerminal),
  PValidationTraceDescriptorV1 (..),
  PValidationVerdict (PAccepted),
  phashWorkWitness,
 )

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Aiken @proof.TraceBoundarySide@ — which end of the trace a fault is about.
data PTraceBoundarySide (s :: S)
  = PTraceStart
  | PTraceEnd
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTraceBoundarySide)

{- | Aiken @proof.SourceMembershipProof@ — an event is in one of the four source
trees. The arm says which tree; see this module's header for why they are read by
tag.
-}
data PSourceMembershipProof (s :: S)
  = PWithdrawalSourceMembership
      {pwithdrawalSource'membership :: Term s (PAsData PRootMembershipProof)}
  | PForcedTransactionSourceMembership
      {pforcedSource'membership :: Term s (PAsData PRootMembershipProof)}
  | PL2TransactionSourceMembership
      {pl2Source'membership :: Term s (PAsData PRootMembershipProof)}
  | PDepositSourceMembership
      {pdepositSource'membership :: Term s (PAsData PRootMembershipProof)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSourceMembershipProof)

-- | Aiken @proof.SourceNonMembershipProof@ — an event is in /no/ source tree.
data PSourceNonMembershipProof (s :: S)
  = PWithdrawalSourceNonMembership
      {pwithdrawalSource'nonMembership :: Term s (PAsData PRootNonMembershipProof)}
  | PForcedTransactionSourceNonMembership
      {pforcedSource'nonMembership :: Term s (PAsData PRootNonMembershipProof)}
  | PL2TransactionSourceNonMembership
      {pl2Source'nonMembership :: Term s (PAsData PRootNonMembershipProof)}
  | PDepositSourceNonMembership
      {pdepositSource'nonMembership :: Term s (PAsData PRootNonMembershipProof)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSourceNonMembershipProof)

{- | Aiken @proof.SourceMembershipMismatchWitness@.

The three ways a block's trace and its sources can fail to describe the same set
of events: an event mapped to a step that no source contains, an event in a
source that no step mentions, and an event that appears in both but under
different phases.
-}
data PSourceMembershipMismatchWitness (s :: S)
  = PMappedEventMissingFromSource
      { pmappedEvent'traceProof :: Term s (PAsData PIndexedTraceProof)
      , pmappedEvent'eventToStep :: Term s (PAsData PRootMembershipProof)
      , pmappedEvent'sourceNonMembership :: Term s (PAsData PSourceNonMembershipProof)
      }
  | PSourceEventMissingTrace
      { psourceEvent'sourceMembership :: Term s (PAsData PSourceMembershipProof)
      , psourceEvent'eventToStepNonMembership :: Term s (PAsData PRootNonMembershipProof)
      }
  | PSourcePhaseMismatch
      { pphaseMismatch'traceProof :: Term s (PAsData PIndexedTraceProof)
      , pphaseMismatch'sourceMembership :: Term s (PAsData PSourceMembershipProof)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSourceMembershipMismatchWitness)

{- | Aiken @proof.LedgerDeleteWitness@ — one UTxO leaving the ledger trie.

Two proofs for one key, and both are needed: the inclusion path pins the /value/
being removed, and the exclusion path is what the deletion is computed from.
-}
data PLedgerDeleteWitness (s :: S) = PLedgerDeleteWitness
  { pledgerDelete'key :: Term s (PAsData PByteString)
  , pledgerDelete'value :: Term s (PAsData PByteString)
  , pledgerDelete'membershipProof :: Term s (PAsData PProof)
  , pledgerDelete'deleteProof :: Term s (PAsData PProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerDeleteWitness)

-- | Aiken @proof.LedgerInsertWitness@ — one UTxO entering the ledger trie.
data PLedgerInsertWitness (s :: S) = PLedgerInsertWitness
  { pledgerInsert'key :: Term s (PAsData PByteString)
  , pledgerInsert'value :: Term s (PAsData PByteString)
  , pledgerInsert'nonMembershipProof :: Term s (PAsData PProof)
  , pledgerInsert'insertProof :: Term s (PAsData PProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerInsertWitness)

{- | Aiken @proof.CountFaultWitness@ — the five ways a block's counts can fail to
agree with each other or with the roots they are published beside.
-}
data PCountFaultWitness (s :: S)
  = PHeaderTotalCountMismatch
  | PHeaderTransitionStepCountMismatch
  | PSourceRootCountMismatch {psourceRootCount'proof :: Term s (PAsData PRootCountProof)}
  | PEventToStepRootCountMismatch {peventToStepRootCount'proof :: Term s (PAsData PRootCountProof)}
  | PTransitionTraceRootCountMismatch {ptraceRootCount'proof :: Term s (PAsData PRootCountProof)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCountFaultWitness)

-- | Aiken @proof.AcceptedTransactionTransitionMismatchWitness@.
data PAcceptedTransactionTransitionMismatchWitness (s :: S) = PAcceptedTransactionTransitionMismatchWitness
  { pacceptedMismatch'claim :: Term s (PAsData PValidationClaimWitnessV1)
  , pacceptedMismatch'terminalAcceptanceWitnessCbor :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAcceptedTransactionTransitionMismatchWitness)

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

-- | 'pconstrOf' for a value already erased to @Data@.
pconstrOfData ::
  forall (s :: S). Term s PData -> (Term s PInteger, Term s (PBuiltinList PData))
pconstrOfData d =
  let pair = pasConstr # d
   in (pfstBuiltin # pair, psndBuiltin # pair)

-- | Reinterpret raw @Data@ as a known data-encoded type.
pcoerceData ::
  forall (a :: S -> Type) (s :: S). (PIsData a) => Term s PData -> Term s a
pcoerceData d = pfromData (punsafeCoerce @(PAsData a) d)

-- | The single field of a one-field constructor, without matching on which.
ponlyField :: forall (a :: S -> Type) (s :: S). Term s (PAsData a) -> Term s PData
ponlyField x = phead # snd (pconstrOf x)

-- | A nullary constructor of a data-encoded enum, built from its tag.
pnullaryOfTag ::
  forall (a :: S -> Type) (s :: S). Term s PInteger -> Term s (PAsData a)
pnullaryOfTag tag = punsafeCoerce (pconstrBuiltin # tag # pcon PNil)

--------------------------------------------------------------------------------
-- Header arithmetic
--------------------------------------------------------------------------------

{- | Aiken @proof.header_counts_are_non_negative@.

Six of the header's seven counts. The seventh — the validation-trace count — is
not a source of steps, so it plays no part in the index arithmetic below.
-}
pheaderCountsAreNonNegative :: forall (s :: S). Term s PHeaderV1 -> Term s PBool
pheaderCountsAreNonNegative header = pmatch header $
  \( PHeaderV1
      { pheader'withdrawalCount
      , pheader'forcedTransactionCount
      , pheader'l2TransactionCount
      , pheader'depositCount
      , pheader'totalEventCount
      , pheader'transitionStepCount
      }
    ) ->
      pfromData pheader'withdrawalCount
        #>= 0
        #&& pfromData pheader'forcedTransactionCount
        #>= 0
        #&& pfromData pheader'l2TransactionCount
        #>= 0
        #&& pfromData pheader'depositCount
        #>= 0
        #&& pfromData pheader'totalEventCount
        #>= 0
        #&& pfromData pheader'transitionStepCount
        #>= 0

-- | Aiken @proof.source_count_sum@ — the four event counts, added up.
psourceCountSum :: forall (s :: S). Term s PHeaderV1 -> Term s PInteger
psourceCountSum header = pmatch header $
  \( PHeaderV1
      { pheader'withdrawalCount
      , pheader'forcedTransactionCount
      , pheader'l2TransactionCount
      , pheader'depositCount
      }
    ) ->
      pfromData pheader'withdrawalCount
        + pfromData pheader'forcedTransactionCount
        + pfromData pheader'l2TransactionCount
        + pfromData pheader'depositCount

{- | Aiken @proof.phase_for_step_index@.

Where a step index falls in the block's four-phase layout. The phases are laid out
back to back in declaration order — withdrawals, then forced transactions, then L2
transactions, then deposits — so an index alone determines a phase, given the
header's counts.

__Aborts__ on an index outside the block, and on a header with a negative count.
That is Aiken's @expect@, and it is why callers guard this behind a lazy
disjunction: a fault proof that names a bad index should fail rather than be
silently read as "the phase does not match".

The final @expect@ is not redundant with the first: @total_event_count@ is a field
of the header, not a sum of the other four, so an index inside the total can still
fall past the last deposit.
-}
pphaseForStepIndex ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PInteger ->
  Term s (PAsData PTransitionPhase)
pphaseForStepIndex header stepIndex = P.do
  PHeaderV1
    { pheader'withdrawalCount
    , pheader'forcedTransactionCount
    , pheader'l2TransactionCount
    , pheader'depositCount
    , pheader'totalEventCount
    } <-
    pmatch header
  withdrawalsEnd <- plet (pfromData pheader'withdrawalCount)
  forcedEnd <- plet (withdrawalsEnd + pfromData pheader'forcedTransactionCount)
  l2End <- plet (forcedEnd + pfromData pheader'l2TransactionCount)
  pif
    ( pheaderCountsAreNonNegative header
        #&& stepIndex
        #>= 0
        #&& stepIndex
        #< pfromData pheader'totalEventCount
    )
    ( pif (stepIndex #< withdrawalsEnd) (pnullaryOfTag 0) $
        pif (stepIndex #< forcedEnd) (pnullaryOfTag 1) $
          pif (stepIndex #< l2End) (pnullaryOfTag 2) $
            pif
              (stepIndex #< l2End + pfromData pheader'depositCount)
              (pnullaryOfTag 3)
              perror
    )
    perror

{- | Aiken @proof.event_key_phase@.

The same function as @validation_claim_v1.phase_for_event_key@ — Aiken carries a
copy in each file; the port has one, and this is the alias the transition-trace
side reads it under.
-}
peventKeyPhase :: forall (s :: S). Term s PData -> Term s (PAsData PTransitionPhase)
peventKeyPhase eventKey =
  pnullaryOfTag (pphaseTagForEventKeyTag (pfstBuiltin #$ pasConstr # eventKey))

--------------------------------------------------------------------------------
-- Reading a source proof
--------------------------------------------------------------------------------

{- | Aiken @proof.event_key_from_source_membership@.

The event key a source proof is about: the proof's own tree key, wrapped in the
@EventKey@ constructor for that tree. The four source arms and the four event-key
arms are declared in the same order, so this is a tag-preserving rewrap — spelled
out arm by arm anyway, for the reason given at 'pphaseTagForEventKeyTag'.
-}
peventKeyFromSourceMembership ::
  forall (s :: S). Term s (PAsData PSourceMembershipProof) -> Term s PData
peventKeyFromSourceMembership source =
  pmatch (pcoerceData @PRootMembershipProof (ponlyField source)) $
    \PRootMembershipProof {prootMembership'key} ->
      pforgetData
        ( pconstrBuiltin
            # peventKeyTagForSourceTag (fst (pconstrOf source))
            # (pcons # prootMembership'key # pcon PNil)
        )

-- | The @EventKey@ constructor a source arm maps to.
peventKeyTagForSourceTag :: forall (s :: S). Term s PInteger -> Term s PInteger
peventKeyTagForSourceTag tag =
  pif (tag #== 0) 0 $
    pif (tag #== 1) 1 $
      pif (tag #== 2) 2 $
        pif (tag #== 3) 3 perror

-- | Aiken @proof.source_membership_phase@.
psourceMembershipPhase ::
  forall (s :: S).
  Term s (PAsData PSourceMembershipProof) ->
  Term s (PAsData PTransitionPhase)
psourceMembershipPhase = peventKeyPhase . peventKeyFromSourceMembership

-- | Aiken @proof.source_non_membership_event_key@.
psourceNonMembershipEventKey ::
  forall (s :: S). Term s (PAsData PSourceNonMembershipProof) -> Term s PData
psourceNonMembershipEventKey source =
  pmatch (pcoerceData @PRootNonMembershipProof (ponlyField source)) $
    \PRootNonMembershipProof {prootNonMembership'key} ->
      pforgetData
        ( pconstrBuiltin
            # peventKeyTagForSourceTag (fst (pconstrOf source))
            # (pcons # prootNonMembership'key # pcon PNil)
        )

--------------------------------------------------------------------------------
-- Membership
--------------------------------------------------------------------------------

-- | Aiken @proof.verify_event_to_step_membership@.
pverifyEventToStepMembership ::
  forall (s :: S). Term s PHeaderV1 -> Term s PRootMembershipProof -> Term s PBool
pverifyEventToStepMembership header membership = P.do
  PHeaderV1 {pheader'eventToStepRoot, pheader'totalEventCount} <- pmatch header
  PRootMembershipProof {prootMembership'key, prootMembership'value} <- pmatch membership
  pverifyRootMembershipWithBytes
    membership
    (pdata (pcon PEventToStepRootDomain))
    (pfromData pheader'eventToStepRoot)
    (pfromData pheader'totalEventCount)
    (pserialiseData # prootMembership'key)
    (pserialiseData # prootMembership'value)

-- | Aiken @proof.verify_event_to_step_non_membership@.
pverifyEventToStepNonMembership ::
  forall (s :: S). Term s PHeaderV1 -> Term s PRootNonMembershipProof -> Term s PBool
pverifyEventToStepNonMembership header nonMembership = P.do
  PHeaderV1 {pheader'eventToStepRoot, pheader'totalEventCount} <- pmatch header
  PRootNonMembershipProof {prootNonMembership'key} <- pmatch nonMembership
  pverifyRootNonMembershipWithKeyBytes
    nonMembership
    (pdata (pcon PEventToStepRootDomain))
    (pfromData pheader'eventToStepRoot)
    (pfromData pheader'totalEventCount)
    (pserialiseData # prootNonMembership'key)

{- | Aiken @proof.verify_source_membership@.

One of four trees, chosen by the proof's own arm. The L2 arm is the odd one: its
key and value go into the tree raw rather than as CBOR.
-}
pverifySourceMembership ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s (PAsData PSourceMembershipProof) ->
  Term s PBool
pverifySourceMembership header source = P.do
  PHeaderV1
    { pheader'withdrawalsRoot
    , pheader'withdrawalCount
    , pheader'forcedTransactionsRoot
    , pheader'forcedTransactionCount
    , pheader'transactionsRoot
    , pheader'l2TransactionCount
    , pheader'depositsRoot
    , pheader'depositCount
    } <-
    pmatch header
  tag <- plet (fst (pconstrOf source))
  membership <- plet (pcoerceData @PRootMembershipProof (ponlyField source))
  PRootMembershipProof {prootMembership'key, prootMembership'value} <- pmatch membership
  let against domain root count keyBytes valueBytes =
        pverifyRootMembershipWithBytes
          membership
          (pdata (pcon domain))
          (pfromData root)
          (pfromData count)
          keyBytes
          valueBytes
      cborKey = pserialiseData # prootMembership'key
      cborValue = pserialiseData # prootMembership'value
  pif
    (tag #== 0)
    ( against
        PWithdrawalsRootDomain
        pheader'withdrawalsRoot
        pheader'withdrawalCount
        cborKey
        cborValue
    )
    $ pif
      (tag #== 1)
      ( against
          PForcedTransactionsV1RootDomain
          pheader'forcedTransactionsRoot
          pheader'forcedTransactionCount
          cborKey
          cborValue
      )
    $ pif
      (tag #== 2)
      ( against
          PTransactionsV1RootDomain
          pheader'transactionsRoot
          pheader'l2TransactionCount
          (pasByteStr # prootMembership'key)
          (pasByteStr # prootMembership'value)
      )
    $ pif
      (tag #== 3)
      ( against
          PDepositsRootDomain
          pheader'depositsRoot
          pheader'depositCount
          cborKey
          cborValue
      )
      perror

-- | Aiken @proof.verify_source_non_membership@.
pverifySourceNonMembership ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s (PAsData PSourceNonMembershipProof) ->
  Term s PBool
pverifySourceNonMembership header source = P.do
  PHeaderV1
    { pheader'withdrawalsRoot
    , pheader'withdrawalCount
    , pheader'forcedTransactionsRoot
    , pheader'forcedTransactionCount
    , pheader'transactionsRoot
    , pheader'l2TransactionCount
    , pheader'depositsRoot
    , pheader'depositCount
    } <-
    pmatch header
  tag <- plet (fst (pconstrOf source))
  nonMembership <- plet (pcoerceData @PRootNonMembershipProof (ponlyField source))
  PRootNonMembershipProof {prootNonMembership'key} <- pmatch nonMembership
  let against domain root count keyBytes =
        pverifyRootNonMembershipWithKeyBytes
          nonMembership
          (pdata (pcon domain))
          (pfromData root)
          (pfromData count)
          keyBytes
      cborKey = pserialiseData # prootNonMembership'key
  pif
    (tag #== 0)
    (against PWithdrawalsRootDomain pheader'withdrawalsRoot pheader'withdrawalCount cborKey)
    $ pif
      (tag #== 1)
      ( against
          PForcedTransactionsV1RootDomain
          pheader'forcedTransactionsRoot
          pheader'forcedTransactionCount
          cborKey
      )
    $ pif
      (tag #== 2)
      ( against
          PTransactionsV1RootDomain
          pheader'transactionsRoot
          pheader'l2TransactionCount
          (pasByteStr # prootNonMembership'key)
      )
    $ pif
      (tag #== 3)
      (against PDepositsRootDomain pheader'depositsRoot pheader'depositCount cborKey)
      perror

{- | Aiken @proof.verify_trace_binding_to_event_to_step@.

A step and the event-to-step map, checked to be talking about the same event in
the same place. Three agreements are required, not one: the same event key, the
same step index, and the same phase. Dropping any of them lets a prover open a
genuine step and a genuine map entry that have nothing to do with each other.
-}
pverifyTraceBindingToEventToStep ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PIndexedTraceProof ->
  Term s PRootMembershipProof ->
  Term s PBool
pverifyTraceBindingToEventToStep header traceProof eventToStep = P.do
  PHeaderV1 {pheader'transitionTraceRoot, pheader'transitionStepCount} <- pmatch header
  PRootMembershipProof
    { prootMembership'key = eventToStepKey
    , prootMembership'value = eventToStepValue
    } <-
    pmatch eventToStep
  PRootMembershipProof {prootMembership'value = stepValue} <- pmatch traceProof
  PTransitionStep
    { ptransitionStep'eventKey
    , ptransitionStep'stepIndex
    , ptransitionStep'phase
    } <-
    pmatch (pcoerceData stepValue)
  PEventToStepValue {peventToStepValue'stepIndex, peventToStepValue'phase} <-
    pmatch (pcoerceData eventToStepValue)
  pverifyIndexedTraceProof
    traceProof
    (pfromData pheader'transitionTraceRoot)
    (pfromData pheader'transitionStepCount)
    #&& pverifyEventToStepMembership header eventToStep
    #&& eventToStepKey
    #== pforgetData ptransitionStep'eventKey
    #&& peventToStepValue'stepIndex
    #== ptransitionStep'stepIndex
    #&& peventToStepValue'phase
    #== ptransitionStep'phase

--------------------------------------------------------------------------------
-- Faults
--------------------------------------------------------------------------------

{- | Aiken @proof.validate_trace_boundary@.

The trace must start where the block says the ledger was and end where the block
says it now is. A fault is proven by opening the first or last step and showing
its outward-facing root is /not/ the header's.

Requires a non-empty trace: a block with no steps has no boundary to be wrong
about, and stating this fault against one would open a hole where the two @!=@
checks are vacuous.
-}
pvalidateTraceBoundary ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s (PAsData PTraceBoundarySide) ->
  Term s PIndexedTraceProof ->
  Term s PBool
pvalidateTraceBoundary header side traceProof = P.do
  PHeaderV1
    { pheader'prevUtxosRoot
    , pheader'utxosRoot
    , pheader'transitionTraceRoot
    , pheader'transitionStepCount
    } <-
    pmatch header
  PRootMembershipProof {prootMembership'key, prootMembership'value} <- pmatch traceProof
  PTransitionStep {ptransitionStep'preUtxosRoot, ptransitionStep'postUtxosRoot} <-
    pmatch (pcoerceData prootMembership'value)
  stepCount <- plet (pfromData pheader'transitionStepCount)
  key <- plet (pasInt # prootMembership'key)
  pif
    ( stepCount
        #> 0
        #&& pverifyIndexedTraceProof
          traceProof
          (pfromData pheader'transitionTraceRoot)
          stepCount
    )
    ( pif
        (fst (pconstrOf side) #== 0)
        ( key
            #== 0
            #&& pnot
            # (ptransitionStep'preUtxosRoot #== pheader'prevUtxosRoot)
        )
        ( key
            #== stepCount - 1
            #&& pnot
            # (ptransitionStep'postUtxosRoot #== pheader'utxosRoot)
        )
    )
    perror

{- | Aiken @proof.validate_trace_link@.

Two adjacent steps that do not meet. The adjacency itself is established by
'pverifyAdjacentTraceProof'; all this adds is the disagreement.
-}
pvalidateTraceLink ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PAdjacentTraceProof ->
  Term s PBool
pvalidateTraceLink header adjacent = P.do
  PHeaderV1 {pheader'transitionTraceRoot, pheader'transitionStepCount} <- pmatch header
  PAdjacentTraceProof {padjacentTrace'lower, padjacentTrace'upper} <- pmatch adjacent
  PRootMembershipProof {prootMembership'value = lowerValue} <-
    pmatch (pfromData padjacentTrace'lower)
  PRootMembershipProof {prootMembership'value = upperValue} <-
    pmatch (pfromData padjacentTrace'upper)
  PTransitionStep {ptransitionStep'postUtxosRoot} <- pmatch (pcoerceData lowerValue)
  PTransitionStep {ptransitionStep'preUtxosRoot} <- pmatch (pcoerceData upperValue)
  pverifyAdjacentTraceProof
    adjacent
    (pfromData pheader'transitionTraceRoot)
    (pfromData pheader'transitionStepCount)
    #&& pnot
    # (ptransitionStep'postUtxosRoot #== ptransitionStep'preUtxosRoot)

{- | Aiken @proof.trace_has_bad_phase@.

A step whose phase disagrees with either derivation of it — from its index, or
from its own event key.

The disjunction is lazy because 'pphaseForStepIndex' aborts on an index outside
the block. Aiken's @or { }@ short-circuits for the same reason.
-}
ptraceHasBadPhase ::
  forall (s :: S). Term s PHeaderV1 -> Term s PIndexedTraceProof -> Term s PBool
ptraceHasBadPhase header traceProof = P.do
  PRootMembershipProof {prootMembership'value} <- pmatch traceProof
  PTransitionStep
    { ptransitionStep'eventKey
    , ptransitionStep'stepIndex
    , ptransitionStep'phase
    } <-
    pmatch (pcoerceData prootMembership'value)
  pnot
    # ( ptransitionStep'phase
          #== pphaseForStepIndex header (pfromData ptransitionStep'stepIndex)
      )
    #|| pnot
    # (peventKeyPhase (pforgetData ptransitionStep'eventKey) #== ptransitionStep'phase)

{- | Aiken @proof.validate_event_to_step_mismatch@.

Either the map places the step's event somewhere the step does not agree with, or
the map does not contain the step's event at all. Both are faults, and the
membership arm folds in the bad-phase fault too — a step whose phase is wrong is
a mismatch even when the map agrees with it exactly.
-}
pvalidateEventToStepMismatch ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PIndexedTraceProof ->
  Term s PEventToStepProof ->
  Term s PBool
pvalidateEventToStepMismatch header traceProof eventToStep = P.do
  PHeaderV1 {pheader'transitionTraceRoot, pheader'transitionStepCount} <- pmatch header
  PRootMembershipProof {prootMembership'value} <- pmatch traceProof
  PTransitionStep
    { ptransitionStep'eventKey
    , ptransitionStep'stepIndex
    , ptransitionStep'phase
    } <-
    pmatch (pcoerceData prootMembership'value)
  pif
    ( pverifyIndexedTraceProof
        traceProof
        (pfromData pheader'transitionTraceRoot)
        (pfromData pheader'transitionStepCount)
    )
    ( pmatch eventToStep $ \case
        PEventToStepMembership {peventToStepMembership'membership} -> P.do
          membership <- plet (pfromData peventToStepMembership'membership)
          PRootMembershipProof
            { prootMembership'key = mapKey
            , prootMembership'value = mapValue
            } <-
            pmatch membership
          PEventToStepValue {peventToStepValue'stepIndex, peventToStepValue'phase} <-
            pmatch (pcoerceData mapValue)
          pif
            (pverifyEventToStepMembership header membership)
            ( mapKey
                #== pforgetData ptransitionStep'eventKey
                #&& ( pnot
                        # (peventToStepValue'stepIndex #== ptransitionStep'stepIndex)
                        #|| pnot
                        # (peventToStepValue'phase #== ptransitionStep'phase)
                        #|| ptraceHasBadPhase header traceProof
                    )
            )
            perror
        PEventToStepNonMembership {peventToStepNonMembership'nonMembership} -> P.do
          nonMembership <- plet (pfromData peventToStepNonMembership'nonMembership)
          PRootNonMembershipProof {prootNonMembership'key} <- pmatch nonMembership
          pverifyEventToStepNonMembership header nonMembership
            #&& prootNonMembership'key
            #== pforgetData ptransitionStep'eventKey
    )
    perror

{- | Aiken @proof.validate_source_membership_mismatch@.

The three-armed source fault. Each arm authenticates both sides of a disagreement
before stating it — which is the whole difficulty of a non-membership fault: it
is only evidence if the tree it is absent from is the one the block committed.
-}
pvalidateSourceMembershipMismatch ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s (PAsData PSourceMembershipMismatchWitness) ->
  Term s PBool
pvalidateSourceMembershipMismatch header witness = P.do
  PHeaderV1 {pheader'transitionTraceRoot, pheader'transitionStepCount} <- pmatch header
  pmatch (pfromData witness) $ \case
    PMappedEventMissingFromSource
      { pmappedEvent'traceProof
      , pmappedEvent'eventToStep
      , pmappedEvent'sourceNonMembership
      } -> P.do
        traceProof <- plet (pfromData pmappedEvent'traceProof)
        PRootMembershipProof {prootMembership'value} <- pmatch traceProof
        PTransitionStep {ptransitionStep'eventKey} <- pmatch (pcoerceData prootMembership'value)
        pverifyTraceBindingToEventToStep
          header
          traceProof
          (pfromData pmappedEvent'eventToStep)
          #&& pverifySourceNonMembership header pmappedEvent'sourceNonMembership
          #&& psourceNonMembershipEventKey pmappedEvent'sourceNonMembership
          #== pforgetData ptransitionStep'eventKey
    PSourceEventMissingTrace
      { psourceEvent'sourceMembership
      , psourceEvent'eventToStepNonMembership
      } -> P.do
        nonMembership <- plet (pfromData psourceEvent'eventToStepNonMembership)
        PRootNonMembershipProof {prootNonMembership'key} <- pmatch nonMembership
        pverifySourceMembership header psourceEvent'sourceMembership
          #&& pverifyEventToStepNonMembership header nonMembership
          #&& prootNonMembership'key
          #== peventKeyFromSourceMembership psourceEvent'sourceMembership
    PSourcePhaseMismatch {pphaseMismatch'traceProof, pphaseMismatch'sourceMembership} -> P.do
      traceProof <- plet (pfromData pphaseMismatch'traceProof)
      PRootMembershipProof {prootMembership'value} <- pmatch traceProof
      PTransitionStep {ptransitionStep'eventKey, ptransitionStep'phase} <-
        pmatch (pcoerceData prootMembership'value)
      pverifyIndexedTraceProof
        traceProof
        (pfromData pheader'transitionTraceRoot)
        (pfromData pheader'transitionStepCount)
        #&& pverifySourceMembership header pphaseMismatch'sourceMembership
        #&& pforgetData ptransitionStep'eventKey
        #== peventKeyFromSourceMembership pphaseMismatch'sourceMembership
        #&& pnot
        # ( ptransitionStep'phase
              #== psourceMembershipPhase pphaseMismatch'sourceMembership
          )

--------------------------------------------------------------------------------
-- The ledger trie
--------------------------------------------------------------------------------

{- | Aiken @proof.ledger_outref_key@.

A UTxO's key in the ledger trie is the __native-transaction encoding__ of the
output reference — @82 ‖ definite(tx_id) ‖ cbor(index)@ — and not a serialised
Plutus constructor. The same encoding keys the input sets a transaction's field
preimages are built from, which is what lets a spend witness and a ledger
deletion be about the same UTxO without either side re-deriving the other's key.
-}
pledgerOutrefKey :: forall (s :: S). Term s PData -> Term s PByteString
pledgerOutrefKey outref =
  plet (snd (pconstrOfData outref)) $ \fields ->
    pencodeMidgardTxInput
      #$ pcon
      $ PMidgardTxInput
        { ptxInput'txId =
            pdata (pasByteStr #$ phead #$ snd (pconstrOfData (phead # fields)))
        , ptxInput'outputIndex = pdata (pasInt #$ phead #$ ptail # fields)
        }

-- | Aiken @proof.verify_ledger_membership@.
pverifyLedgerMembership ::
  forall (s :: S).
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PProof ->
  Term s PBool
pverifyLedgerMembership root key value proof =
  MpfProof.phasV1 # pto (pmpfFromMidgardRoot root) # key # value # proof

{- | Aiken @proof.verify_ledger_non_membership@.

Absence stated as an insertion that succeeds. @insert@ requires the key's
/exclusion/ path to reproduce the current root, so a key already in the trie
makes it abort — which is why Aiken writes this as @expect _inserted = ...@ and
discards the result, and why this returns True rather than a comparison.

__Aborts__ rather than returning False. A malformed proof and a present key are
the same outcome here, and both are "this witness is not evidence" rather than
"this fault does not hold".
-}
pverifyLedgerNonMembership ::
  forall (s :: S).
  Term s PByteString ->
  Term s PByteString ->
  Term s PProof ->
  Term s PBool
pverifyLedgerNonMembership root key proof =
  pmatch (MpfProof.pinsertRoot # pto (pmpfFromMidgardRoot root) # key # pconstant "" # proof) $ \case
    PJust _ -> pconstant True
    PNothing -> perror

-- | Aiken @proof.insert_root@ — the trie's root after an insertion.
pinsertRoot ::
  forall (s :: S).
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PProof ->
  Term s PByteString
pinsertRoot root key value proof =
  pmatch (MpfProof.pinsertRoot # pto (pmpfFromMidgardRoot root) # key # value # proof) $ \case
    PJust newRoot -> newRoot
    PNothing -> perror

-- | Aiken @proof.delete_root@ — the trie's root after a deletion.
pdeleteRoot ::
  forall (s :: S).
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PProof ->
  Term s PByteString
pdeleteRoot root key value proof =
  pmatch (MpfProof.pdeleteRoot # pto (pmpfFromMidgardRoot root) # key # value # proof) $ \case
    PJust newRoot -> newRoot
    PNothing -> perror

{- | Aiken @proof.apply_delete_witness@.

The root a step /should/ have produced by spending one UTxO. Membership is
checked before the deletion rather than left to @delete@'s own inclusion check,
because the two carry different proofs: an inclusion path and an exclusion path
for the same key. Requiring both is what pins the value being removed as well as
the key.
-}
papplyDeleteWitness ::
  forall (s :: S).
  Term s PByteString ->
  Term s PLedgerDeleteWitness ->
  Term s PByteString
papplyDeleteWitness root witness = pmatch witness $
  \( PLedgerDeleteWitness
      { pledgerDelete'key
      , pledgerDelete'value
      , pledgerDelete'membershipProof
      , pledgerDelete'deleteProof
      }
    ) ->
      plet (pfromData pledgerDelete'key) $ \key ->
        plet (pfromData pledgerDelete'value) $ \value ->
          pif
            ( pverifyLedgerMembership
                root
                key
                value
                (pfromData pledgerDelete'membershipProof)
            )
            (pdeleteRoot root key value (pfromData pledgerDelete'deleteProof))
            perror

-- | Aiken @proof.apply_insert_witness@.
papplyInsertWitness ::
  forall (s :: S).
  Term s PByteString ->
  Term s PLedgerInsertWitness ->
  Term s PByteString
papplyInsertWitness root witness = pmatch witness $
  \( PLedgerInsertWitness
      { pledgerInsert'key
      , pledgerInsert'value
      , pledgerInsert'nonMembershipProof
      , pledgerInsert'insertProof
      }
    ) ->
      plet (pfromData pledgerInsert'key) $ \key ->
        pif
          ( pverifyLedgerNonMembership
              root
              key
              (pfromData pledgerInsert'nonMembershipProof)
          )
          ( pinsertRoot
              root
              key
              (pfromData pledgerInsert'value)
              (pfromData pledgerInsert'insertProof)
          )
          perror

--------------------------------------------------------------------------------
-- One-step bindings
--------------------------------------------------------------------------------

{- | The shared half of the three one-step bindings.

Each ties a step to the event-to-step map and to the source tree the event came
out of, then requires the step to name /that/ event under /that/ phase. Aiken
writes the three out separately because the source arm and the event-key
constructor differ; here they differ only in a tag and a domain, so they share a
body and the arms supply the two constants.
-}
poneStepBinding ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Integer ->
  Term s PIndexedTraceProof ->
  Term s PRootMembershipProof ->
  Term s PRootMembershipProof ->
  Term s PBool
poneStepBinding header sourceTag traceProof eventToStep sourceMembership = P.do
  PRootMembershipProof {prootMembership'value = stepValue} <- pmatch traceProof
  PRootMembershipProof {prootMembership'key = sourceKey} <- pmatch sourceMembership
  PTransitionStep {ptransitionStep'eventKey, ptransitionStep'phase} <-
    pmatch (pcoerceData stepValue)
  source <-
    plet $
      punsafeCoerce @(PAsData PSourceMembershipProof)
        ( pconstrBuiltin
            # pconstant sourceTag
            # (pcons # pforgetData (pdata sourceMembership) # pcon PNil)
        )
  pverifyTraceBindingToEventToStep header traceProof eventToStep
    #&& pverifySourceMembership header source
    #&& pforgetData ptransitionStep'eventKey
    #== pforgetData
      ( pconstrBuiltin
          # peventKeyTagForSourceTag (pconstant sourceTag)
          # (pcons # sourceKey # pcon PNil)
      )
    #&& ptransitionStep'phase
    #== pnullaryOfTag (pconstant sourceTag)

-- | Aiken @proof.validate_withdrawal_one_step_binding@.
pvalidateWithdrawalOneStepBinding ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PIndexedTraceProof ->
  Term s PRootMembershipProof ->
  Term s PRootMembershipProof ->
  Term s PBool
pvalidateWithdrawalOneStepBinding header = poneStepBinding header 0

-- | Aiken @proof.validate_forced_transaction_one_step_binding@.
pvalidateForcedTransactionOneStepBinding ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PIndexedTraceProof ->
  Term s PRootMembershipProof ->
  Term s PRootMembershipProof ->
  Term s PBool
pvalidateForcedTransactionOneStepBinding header = poneStepBinding header 1

-- | Aiken @proof.validate_deposit_one_step_binding@.
pvalidateDepositOneStepBinding ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PIndexedTraceProof ->
  Term s PRootMembershipProof ->
  Term s PRootMembershipProof ->
  Term s PBool
pvalidateDepositOneStepBinding header = poneStepBinding header 3

--------------------------------------------------------------------------------
-- One-step transitions
--------------------------------------------------------------------------------

{- | Aiken @proof.validate_valid_withdrawal_transition@.

A valid withdrawal must remove exactly the UTxO it names, so the step's post-root
is determined: it is the pre-root with that one key deleted. The fault is that the
step published something else.

Three of the four clauses __abort__ rather than refusing — the validity must be
@WithdrawalIsValid@, the witness must name the withdrawal's own output reference,
and the deletion must be well-proven. That is deliberate: a witness that fails any
of them is not a claim about this transition at all.
-}
pvalidateValidWithdrawalTransition ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PIndexedTraceProof ->
  Term s PRootMembershipProof ->
  Term s PRootMembershipProof ->
  Term s PLedgerDeleteWitness ->
  Term s PBool
pvalidateValidWithdrawalTransition header traceProof eventToStep sourceMembership spentUtxo = P.do
  PRootMembershipProof {prootMembership'value = stepValue} <- pmatch traceProof
  PRootMembershipProof {prootMembership'value = withdrawalValue} <- pmatch sourceMembership
  PTransitionStep {ptransitionStep'preUtxosRoot, ptransitionStep'postUtxosRoot} <-
    pmatch (pcoerceData stepValue)
  PWithdrawalInfo {pwithdrawalInfo'body, pwithdrawalInfo'validity} <-
    pmatch (pcoerceData withdrawalValue)
  PWithdrawalBody {pwithdrawalBody'l2Outref} <- pmatch (pfromData pwithdrawalInfo'body)
  PLedgerDeleteWitness {pledgerDelete'key} <- pmatch spentUtxo
  pif
    ( pwithdrawalInfo'validity
        #== pdata (pcon PWithdrawalIsValid)
        #&& pfromData pledgerDelete'key
        #== pledgerOutrefKey pwithdrawalBody'l2Outref
    )
    ( plet (papplyDeleteWitness (pfromData ptransitionStep'preUtxosRoot) spentUtxo) $
        \expectedPostRoot ->
          pvalidateWithdrawalOneStepBinding header traceProof eventToStep sourceMembership
            #&& pnot
            # (expectedPostRoot #== pfromData ptransitionStep'postUtxosRoot)
    )
    perror

{- | Aiken @proof.validate_invalid_withdrawal_no_op_transition@.

An invalid withdrawal must change nothing, so the fault is a step that moved the
ledger anyway. The validity check aborts: this witness has nothing to say about a
withdrawal the operator called valid.
-}
pvalidateInvalidWithdrawalNoOpTransition ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PIndexedTraceProof ->
  Term s PRootMembershipProof ->
  Term s PRootMembershipProof ->
  Term s PBool
pvalidateInvalidWithdrawalNoOpTransition header traceProof eventToStep sourceMembership = P.do
  PRootMembershipProof {prootMembership'value = stepValue} <- pmatch traceProof
  PRootMembershipProof {prootMembership'value = withdrawalValue} <- pmatch sourceMembership
  PTransitionStep {ptransitionStep'preUtxosRoot, ptransitionStep'postUtxosRoot} <-
    pmatch (pcoerceData stepValue)
  PWithdrawalInfo {pwithdrawalInfo'validity} <- pmatch (pcoerceData withdrawalValue)
  pif
    (pnot # (pwithdrawalInfo'validity #== pdata (pcon PWithdrawalIsValid)))
    ( pvalidateWithdrawalOneStepBinding header traceProof eventToStep sourceMembership
        #&& pnot
        # (ptransitionStep'preUtxosRoot #== ptransitionStep'postUtxosRoot)
    )
    perror

{- | Aiken @proof.validate_invalid_forced_transaction_no_op_transition@.

The forced-inclusion twin of the above, keyed off the operator's recorded verdict
rather than a withdrawal validity.
-}
pvalidateInvalidForcedTransactionNoOpTransition ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PIndexedTraceProof ->
  Term s PRootMembershipProof ->
  Term s PRootMembershipProof ->
  Term s PBool
pvalidateInvalidForcedTransactionNoOpTransition header traceProof eventToStep sourceMembership = P.do
  PRootMembershipProof {prootMembership'value = stepValue} <- pmatch traceProof
  PRootMembershipProof {prootMembership'value = forcedValue} <- pmatch sourceMembership
  PTransitionStep {ptransitionStep'preUtxosRoot, ptransitionStep'postUtxosRoot} <-
    pmatch (pcoerceData stepValue)
  PForcedInclusionTxV1 {pforcedTx'operatorValidity} <- pmatch (pcoerceData forcedValue)
  pif
    (pnot # (pforcedTx'operatorValidity #== pdata (pcon PTxIsValid)))
    ( pvalidateForcedTransactionOneStepBinding header traceProof eventToStep sourceMembership
        #&& pnot
        # (ptransitionStep'preUtxosRoot #== ptransitionStep'postUtxosRoot)
    )
    perror

{- | Aiken @proof.validate_duplicate_trace_event@.

One event applied twice. Two openings of the /same/ committed trace at different
indices that name the same event key — which is a fault however well-formed each
step is on its own, because the trace is meant to be a permutation of the block's
events rather than a multiset over them.

Both proofs are checked against the same root and count, which is the whole
argument: two steps out of two different trees would say nothing at all.
-}
pvalidateDuplicateTraceEvent ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PIndexedTraceProof ->
  Term s PIndexedTraceProof ->
  Term s PBool
pvalidateDuplicateTraceEvent header left right = P.do
  PHeaderV1 {pheader'transitionTraceRoot, pheader'transitionStepCount} <- pmatch header
  root <- plet (pfromData pheader'transitionTraceRoot)
  count <- plet (pfromData pheader'transitionStepCount)
  PRootMembershipProof
    { prootMembership'key = leftKey
    , prootMembership'value = leftValue
    } <-
    pmatch left
  PRootMembershipProof
    { prootMembership'key = rightKey
    , prootMembership'value = rightValue
    } <-
    pmatch right
  PTransitionStep {ptransitionStep'eventKey = leftEvent} <- pmatch (pcoerceData leftValue)
  PTransitionStep {ptransitionStep'eventKey = rightEvent} <- pmatch (pcoerceData rightValue)
  pverifyIndexedTraceProof left root count
    #&& pverifyIndexedTraceProof right root count
    #&& pnot
    # (leftKey #== rightKey)
    #&& leftEvent
    #== rightEvent

--------------------------------------------------------------------------------
-- Count faults
--------------------------------------------------------------------------------

{- | Aiken @proof.validate_count_fault@.

A block publishes seven counts and five of them are checkable against something
else: the event total against the four sources' sum, the step count against the
event total, and each committed root against the count published beside it. This
is the fault for any of those disagreeing.

The root arms need a @RootCountProof@ — a root's own @(domain, phas root, count)@
triple — because a counted root does not reveal its count. The proof is checked
against the header's /root/ using the count the proof itself carries, and only
then is that count compared with the header's. Checking it against the header's
count instead would make the fault unprovable: the proof would have to agree with
the very number it is there to contradict.

The @SourceRootCountMismatch@ arm dispatches on the proof's own domain and
returns False for the three non-source domains, which is the one place in this
file where a wrong domain is a refusal rather than an abort — Aiken writes it as a
@_ -> False@ catch-all, and a fault stated under the wrong domain is a fault that
does not hold.
-}
pvalidateCountFault ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s (PAsData PCountFaultWitness) ->
  Term s PBool
pvalidateCountFault header witness = P.do
  PHeaderV1
    { pheader'withdrawalsRoot
    , pheader'withdrawalCount
    , pheader'forcedTransactionsRoot
    , pheader'forcedTransactionCount
    , pheader'transactionsRoot
    , pheader'l2TransactionCount
    , pheader'depositsRoot
    , pheader'depositCount
    , pheader'eventToStepRoot
    , pheader'totalEventCount
    , pheader'transitionTraceRoot
    , pheader'transitionStepCount
    } <-
    pmatch header
  tag <- plet (fst (pconstrOf witness))
  let countProofData = phead # snd (pconstrOf witness)
      countProof = pcoerceData @PRootCountProof countProofData
      countDomainTag = fst (pconstrOfData (phead # snd (pconstrOfData countProofData)))
      against domain root published =
        pmatch countProof $ \PRootCountProof {prootCount'count} ->
          plet (pfromData prootCount'count) $ \count ->
            pverifyRootCountProof countProof (pdata (pcon domain)) (pfromData root) count
              #&& pnot
              # (count #== pfromData published)
  pif
    (tag #== 0)
    ( pnot
        # (pfromData pheader'totalEventCount #== psourceCountSum header)
    )
    $ pif
      (tag #== 1)
      ( pnot
          # ( pfromData pheader'transitionStepCount
                #== pfromData pheader'totalEventCount
            )
      )
    $ pif
      (tag #== 2)
      ( plet countDomainTag $
          \domainTag ->
            pif
              (domainTag #== 0)
              (against PWithdrawalsRootDomain pheader'withdrawalsRoot pheader'withdrawalCount)
              $ pif
                (domainTag #== 1)
                ( against
                    PForcedTransactionsV1RootDomain
                    pheader'forcedTransactionsRoot
                    pheader'forcedTransactionCount
                )
              $ pif
                (domainTag #== 2)
                ( against
                    PTransactionsV1RootDomain
                    pheader'transactionsRoot
                    pheader'l2TransactionCount
                )
              $ pif
                (domainTag #== 3)
                (against PDepositsRootDomain pheader'depositsRoot pheader'depositCount)
                (pconstant False)
      )
    $ pif
      (tag #== 3)
      (against PEventToStepRootDomain pheader'eventToStepRoot pheader'totalEventCount)
    $ pif
      (tag #== 4)
      ( against
          PTransitionTraceRootDomain
          pheader'transitionTraceRoot
          pheader'transitionStepCount
      )
      perror

--------------------------------------------------------------------------------
-- Accepted-transaction transition mismatch
--------------------------------------------------------------------------------

-- | Aiken @proof.terminal_acceptance_post_root@.
pterminalAcceptancePostRoot :: forall (s :: S). Term s (PByteString :--> PByteString)
pterminalAcceptancePostRoot = phoistAcyclic $
  plam $ \witnessCbor ->
    pmatch (pdeserialise # witnessCbor) $ \case
      PNothing -> perror
      PJust witnessData ->
        plet (pasList # witnessData) $ \fields ->
          pif (plength # fields #== 4)
            ( plet (pasByteStr # (pelemAt # 2 # fields)) $ \postRoot ->
                pif
                  ( pasInt # (pelemAt # 0 # fields) #== 1
                      #&& pasByteStr # (pelemAt # 1 # fields) #== pconstant ""
                      #&& plengthBS # postRoot #== 32
                  )
                  postRoot
                  perror
            )
            perror

-- | Aiken @proof.validate_accepted_transaction_transition_mismatch@.
pvalidateAcceptedTransactionTransitionMismatch ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PAcceptedTransactionTransitionMismatchWitness ->
  Term s PBool
pvalidateAcceptedTransactionTransitionMismatch header witness = P.do
  PAcceptedTransactionTransitionMismatchWitness
    { pacceptedMismatch'claim
    , pacceptedMismatch'terminalAcceptanceWitnessCbor
    } <-
    pmatch witness
  claim <- plet (pfromData pacceptedMismatch'claim)
  PValidationClaimWitnessV1
    { pclaim'descriptorMembership
    , pclaim'transitionStepMembership
    , pclaim'terminalState
    } <-
    pmatch claim
  PRootMembershipProof {prootMembership'value = descriptorData} <-
    pmatch (pfromData pclaim'descriptorMembership)
  PRootMembershipProof {prootMembership'value = transitionStepData} <-
    pmatch (pfromData pclaim'transitionStepMembership)
  PValidationTraceDescriptorV1 {pdescriptor'verdict} <-
    pmatch (pcoerceData descriptorData)
  PTransitionStep {ptransitionStep'postUtxosRoot} <-
    pmatch (pcoerceData transitionStepData)
  PValidationMachineStateV1
    { pmachineState'programCounter
    , pmachineState'workRoot
    } <-
    pmatch (pfromData pclaim'terminalState)
  terminalWitnessCbor <- plet (pfromData pacceptedMismatch'terminalAcceptanceWitnessCbor)
  postRoot <- plet (pterminalAcceptancePostRoot # terminalWitnessCbor)
  pcommittedClaimIsValid # header # claim
    #&& pfromData pdescriptor'verdict #== pcon PAccepted
    #&& pfromData pmachineState'workRoot
      #== phashWorkWitness # pcon PTerminal # pfromData pmachineState'programCounter # terminalWitnessCbor
    #&& pnot
      # (postRoot #== pfromData ptransitionStep'postUtxosRoot)

--------------------------------------------------------------------------------
-- The L2 transaction transition
--------------------------------------------------------------------------------

{- | Aiken @proof.verify_l2_source_membership@.

The transactions tree keys by a raw transaction id and stores raw bytes, so
neither side is serialised on the way in — unlike every other source tree.
-}
pverifyL2SourceMembership ::
  forall (s :: S). Term s PHeaderV1 -> Term s PRootMembershipProof -> Term s PBool
pverifyL2SourceMembership header membership = P.do
  PHeaderV1 {pheader'transactionsRoot, pheader'l2TransactionCount} <- pmatch header
  PRootMembershipProof {prootMembership'key, prootMembership'value} <- pmatch membership
  pverifyRootMembershipWithBytes
    membership
    (pdata (pcon PTransactionsV1RootDomain))
    (pfromData pheader'transactionsRoot)
    (pfromData pheader'l2TransactionCount)
    (pasByteStr # prootMembership'key)
    (pasByteStr # prootMembership'value)

{- | Aiken @proof.validate_l2_one_step_binding@.

Unlike its three siblings this one __aborts__ when the step's event key is not an
L2 transaction's, because the key's payload — the transaction id — is what the
source membership is then required to be filed under. There is nothing to compare
against if the key is of another class.
-}
pvalidateL2OneStepBinding ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PIndexedTraceProof ->
  Term s PRootMembershipProof ->
  Term s PRootMembershipProof ->
  Term s PBool
pvalidateL2OneStepBinding header traceProof eventToStep sourceMembership = P.do
  PRootMembershipProof {prootMembership'value = stepValue} <- pmatch traceProof
  PRootMembershipProof {prootMembership'key = sourceKey} <- pmatch sourceMembership
  PTransitionStep {ptransitionStep'eventKey, ptransitionStep'phase} <-
    pmatch (pcoerceData stepValue)
  eventKey <- plet (pforgetData ptransitionStep'eventKey)
  txId <-
    plet $
      pif
        (fst (pconstrOfData eventKey) #== 2)
        (pasByteStr #$ phead # snd (pconstrOfData eventKey))
        perror
  pverifyTraceBindingToEventToStep header traceProof eventToStep
    #&& pverifyL2SourceMembership header sourceMembership
    #&& (pasByteStr # sourceKey)
    #== txId
    #&& ptransitionStep'phase
    #== pnullaryOfTag 2

{- | Aiken @proof.door_body_field_items@.

One §8.8 door opening, walked into the field's items in order.

The reference-input list and certificate policy are empty because the carriage is
@Inline@ — the preimage travels in the redeemer, so no tier-2 or tier-3 lookup
happens. Passing anything else would be dead weight, and Aiken passes @[]@ and
@#""@ for the same reason.

The fold accumulates in reverse and the list is turned back at the end, which is
the shape Aiken uses; the alternative — folding the ledger application straight
into the walk — would fuse two aborts into one order and buy nothing, since every
failure on either side is an abort.
-}
pdoorBodyFieldItems ::
  forall (s :: S).
  Term s PAnchoredNativeTxV1 ->
  Term s PInteger ->
  Term s PByteString ->
  Term s (PBuiltinList PByteString)
pdoorBodyFieldItems anchored fieldIndex preimage =
  preverse
    #$ pfoldOpenedField @(PBuiltinList PByteString)
    # ( panchoredFieldWalk
          # anchored
          # fieldIndex
          # pcon (PInline {pinline'preimage = pdata preimage})
          # (pcon PNil :: Term s (PBuiltinList (PAsData PTxInInfo)))
          # pdata (pcon (PCurrencySymbol (pconstant "")))
      )
    # pcon PNil
    # plam (\acc _index item -> pcons # item # acc)

{- | Aiken @proof.apply_l2_spends@.

Every spend input the transaction declares, removed from the ledger in order. The
witness list must be exactly as long as the input list — a shorter one aborts on
the head, a longer one on the final emptiness check — so a prover cannot omit a
spend and publish the root that omission would produce.

Each witness's key is required to be the encoding of the /declared/ input rather
than merely a well-formed key, which is what ties the ledger deletion to the
transaction's own field 0 rather than to a UTxO of the prover's choosing.
-}
papplyL2Spends ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PBuiltinList PByteString
        :--> PBuiltinList (PAsData PLedgerDeleteWitness)
        :--> PByteString
    )
papplyL2Spends = phoistAcyclic $
  pfix $ \self -> plam $ \root items witnesses ->
    pelimList
      ( \item rest -> P.do
          witness <- plet (pfromData (phead # witnesses))
          PLedgerDeleteWitness {pledgerDelete'key} <- pmatch witness
          expectedKey <-
            plet (pencodeMidgardTxInput #$ pdecodeMidgardTxInputCbor # item)
          pif
            (pfromData pledgerDelete'key #== expectedKey)
            (self # papplyDeleteWitness root witness # rest # (ptail # witnesses))
            perror
      )
      (pif (pnull # witnesses) root perror)
      items

{- | Aiken @proof.apply_l2_outputs@.

Every output the transaction declares, inserted into the ledger under the
transaction's own id and the output's position. The index counts from zero and
climbs with the list, so an output's ledger key is fixed by where it sits in
field 2 — not by anything the witness says.

Both the key and the value are pinned here, where a spend pins only the key: an
output's ledger value /is/ the encoded output, and there is nothing else it could
be.
-}
papplyL2Outputs ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PInteger
        :--> PBuiltinList PByteString
        :--> PBuiltinList (PAsData PLedgerInsertWitness)
        :--> PByteString
    )
papplyL2Outputs = phoistAcyclic $
  pfix $ \self -> plam $ \root txId outputIndex items witnesses ->
    pelimList
      ( \item rest -> P.do
          witness <- plet (pfromData (phead # witnesses))
          PLedgerInsertWitness {pledgerInsert'key, pledgerInsert'value} <- pmatch witness
          expectedKey <-
            plet $
              pencodeMidgardTxInput
                #$ pcon
                $ PMidgardTxInput
                  { ptxInput'txId = pdata txId
                  , ptxInput'outputIndex = pdata outputIndex
                  }
          expectedValue <-
            plet (pencodeMidgardTxOutput #$ pdecodeMidgardTxOutputCbor # item)
          pif
            ( pfromData pledgerInsert'key
                #== expectedKey
                #&& pfromData pledgerInsert'value
                #== expectedValue
            )
            ( self
                # papplyInsertWitness root witness
                # txId
                # (outputIndex + 1)
                # rest
                # (ptail # witnesses)
            )
            perror
      )
      (pif (pnull # witnesses) root perror)
      items

{- | Aiken @proof.validate_l2_transaction_transition@.

The fault that an L2 transaction the block declared valid did not move the ledger
the way its own body says it should. Field 0 is spent, field 2 is produced, and
the resulting root is compared with the one the step published.

=== The source leaf is supplied decoded, not decoded here

Aiken deserialises the leaf bytes, casts the result to an @L2TransactionSourceV1@
and then requires @cbor.serialise(source) == source_membership.value@. Plutus has
no deserialising builtin, and the canonicality clause is what makes one
unnecessary: those three steps hold together exactly when the leaf bytes are the
canonical serialisation of a well-formed source value, so this port has the
prover supply the proof-source triple and rebuilds the leaf from it —

@serialiseData(Constr 0 [B key, triple]) == leaf bytes@

— which is Aiken's canonicality clause and its @source.tx_id == key@ clause at
once. The accepted set is identical: serialisation is injective, so the equality
admits exactly one triple per leaf, and a malformed shape has no preimage at all.
The cost is one extra redeemer field.

=== The anchor is derived once

Three things are read off this one transaction — the scalar validity code, field 0
and field 2 — and re-deriving the transaction id for each costs about 211k memory
units apiece. 'panchoredNativeTx' pays it once and hands back a handle that keeps
the §2.5 pairing and the tier check, both of which the door re-runs per field.

The two field indices are literals, never witness values. §4 removed field-index
domain separation, so a field-0 and a field-1 preimage over the same items commit
identically — a prover who could choose the index could steer an outputs read onto
the reference-inputs commitment.
-}
pvalidateL2TransactionTransition ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PIndexedTraceProof ->
  Term s PRootMembershipProof ->
  Term s PRootMembershipProof ->
  Term s (PAsData PNativeTxProofSourceV1) ->
  Term s PByteString ->
  Term s PByteString ->
  Term s (PBuiltinList (PAsData PLedgerDeleteWitness)) ->
  Term s (PBuiltinList (PAsData PLedgerInsertWitness)) ->
  Term s PBool
pvalidateL2TransactionTransition
  header
  traceProof
  eventToStep
  sourceMembership
  sourceTriple
  spendInputsPreimage
  outputsPreimage
  spentUtxos
  producedUtxos = P.do
    PRootMembershipProof
      { prootMembership'key = sourceKey
      , prootMembership'value = sourceValue
      } <-
      pmatch sourceMembership
    PRootMembershipProof {prootMembership'value = stepValue} <- pmatch traceProof
    PTransitionStep {ptransitionStep'preUtxosRoot, ptransitionStep'postUtxosRoot} <-
      pmatch (pcoerceData stepValue)
    txId <- plet (pasByteStr # sourceKey)
    PNativeTxProofSourceV1 {pnativeSource'compactCbor} <- pmatch (pfromData sourceTriple)
    anchored <-
      plet $
        panchoredNativeTx
          # pcon (PBodyTxOpening (pfromData pnativeSource'compactCbor))
          # pcon (PBodyAnchor (pdata txId))
    pif
      (punanchoredValidityCodeOf # anchored #== 0)
      ( P.do
          spendItems <-
            plet (pdoorBodyFieldItems anchored pspendInputsFieldIndex spendInputsPreimage)
          outputItems <-
            plet (pdoorBodyFieldItems anchored poutputsFieldIndex outputsPreimage)
          afterSpends <-
            plet
              ( papplyL2Spends
                  # pfromData ptransitionStep'preUtxosRoot
                  # spendItems
                  # spentUtxos
              )
          expectedPostRoot <-
            plet (papplyL2Outputs # afterSpends # txId # 0 # outputItems # producedUtxos)
          leafBytes <-
            plet $
              pserialiseData
                #$ pforgetData
                $ pconstrBuiltin
                  # 0
                  #$ pcons
                  # pforgetData (pdata txId)
                  #$ pcons
                  # pforgetData sourceTriple
                  # pcon PNil
          pvalidateL2OneStepBinding header traceProof eventToStep sourceMembership
            #&& leafBytes
            #== (pasByteStr # sourceValue)
            #&& (panchoredNativeTxVersion # anchored)
            #== 1
            #&& pnot
            # (expectedPostRoot #== pfromData ptransitionStep'postUtxosRoot)
      )
      perror

--------------------------------------------------------------------------------
-- Projecting an L1 deposit onto L2
--------------------------------------------------------------------------------

{- | Aiken @proof.cardano_credential_to_midgard@.

The hash is re-wrapped rather than re-read: @PPubKeyHash@, @PScriptHash@ and the
Midgard credential's payload are all a byte string under the data encoding, so
the coercion is a retyping and not a conversion.
-}
pcardanoCredentialToMidgard ::
  forall (s :: S). Term s PCredential -> Term s PMidgardCredential
pcardanoCredentialToMidgard credential = pmatch credential $ \case
  PPubKeyCredential hash ->
    pcon (PMidgardPubKeyCredential (punsafeCoerce @(PAsData PByteString) hash))
  PScriptCredential hash ->
    pcon (PMidgardScriptCredential (punsafeCoerce @(PAsData PByteString) hash))

{- | Aiken @proof.cardano_stake_credential_to_midgard@.

A pointer stake credential __aborts__. Midgard's address encoding has no pointer
form, so there is no projection to produce — an operator that accepted a deposit
to such an address cannot be convicted through this rule, which is Aiken's
behaviour and not an omission.
-}
pcardanoStakeCredentialToMidgard ::
  forall (s :: S). Term s PStakingCredential -> Term s PMidgardCredential
pcardanoStakeCredentialToMidgard stakeCredential = pmatch stakeCredential $ \case
  PStakingHash credential -> pcardanoCredentialToMidgard credential
  PStakingPtr _ _ _ -> perror

{- | Aiken @proof.deposit_address_to_midgard@.

@protected@ is always False: a deposit lands at an ordinary L2 address, and the
protected bit is reserved for addresses no ordinary L2 transaction may spend.
Projecting one as protected would produce an output the depositor could never
move.

The network-id check is here as well as inside 'pencodeMidgardAddress' because
Aiken writes it in both places, and the two are not redundant in the direction
that matters: this one refuses before the address is built.
-}
pdepositAddressToMidgard ::
  forall (s :: S). Term s PAddress -> Term s PInteger -> Term s PMidgardAddress
pdepositAddressToMidgard address networkId = P.do
  PAddress {paddress'credential, paddress'stakingCredential} <- pmatch address
  pif (networkId #== 0 #|| networkId #== 1) `flip` perror $
    pcon $
      PMidgardAddress
        { paddress'protected = pdata (pconstant False)
        , paddress'networkId = pdata networkId
        , paddress'paymentCredential =
            pdata (pcardanoCredentialToMidgard paddress'credential)
        , paddress'stakeCredential =
            pdata $
              pmatch paddress'stakingCredential $ \case
                PDJust stake ->
                  pcon . PDJust . pdata $
                    pcardanoStakeCredentialToMidgard (pfromData stake)
                PDNothing -> pcon PDNothing
        }

{- | Aiken @assets.flatten@ followed by @proof.cardano_asset_pairs_to_midgard@.

Fused, because the intermediate is a list of @(policy, name, quantity)@ triples
and Plutus has no cheap representation of one. Nothing is lost: @flatten@ is a
@foldr@ over the sorted policy map and then over each sorted token map, so it
emits exactly the sequence this walk visits, and the conversion is a per-entry
map with a skip.

Two things it does, both load-bearing:

  * __Ada is dropped.__ An L2 value carries its lovelace in its own field, so an
    Ada entry left in the asset list would be counted twice.
  * __A non-positive quantity aborts.__ After the deposit NFT is removed nothing
    non-positive can remain in a well-formed L1 value, so this is unreachable for
    an honest reference input; it is what stops a crafted one from projecting to
    an output the encoder would then refuse.
-}
pcardanoAssetPairsToMidgard ::
  forall (s :: S). Term s (Value.PSortedValue :--> PMidgardAssets)
pcardanoAssetPairsToMidgard = phoistAcyclic $
  plam $ \value ->
    pcon . PAssocMap $
      pfoldr
        # plam
          ( \policyEntry acc -> P.do
              policyId <- plet (pto (pfromData (pfstBuiltin # policyEntry)))
              pfoldr
                # plam
                  ( \tokenEntry inner -> P.do
                      assetName <- plet (pto (pfromData (pfstBuiltin # tokenEntry)))
                      quantity <- plet (psndBuiltin # tokenEntry)
                      pif
                        ( plengthBS
                            # policyId
                            #== 0
                            #&& plengthBS
                            # assetName
                            #== 0
                        )
                        inner
                        ( pif
                            (pfromData quantity #> 0)
                            ( pcons
                                # (ppairDataBuiltin # pdata (policyId <> assetName) # quantity)
                                # inner
                            )
                            perror
                        )
                  )
                # acc
                # pto (pto (pfromData (psndBuiltin # policyEntry)))
          )
        # pcon PNil
        # pto (pto (pto value))

{- | Aiken @proof.projected_deposit_value@.

The L1 value a deposit UTxO holds, minus the deposit NFT that authenticated it,
read as an L2 value. The NFT must be there exactly once — a reference input
holding two of them, or none, is not this deposit's UTxO — and it must not
survive the projection, or the L2 ledger would mint an L1 authentication token.

The lovelace floor is Aiken's @expect lovelace >= 0@. It cannot fire for a value
that came off the chain; it is here because the same arithmetic is the only thing
standing between a hand-built value and a negative L2 balance.
-}
pprojectedDepositValue ::
  forall (s :: S).
  Term s Value.PSortedValue ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PTokenName) ->
  Term s PMidgardValue
pprojectedDepositValue value depositPolicyId eventAssetName =
  pif
    (pquantityOfValue # value # depositPolicyId # eventAssetName #== 1)
    ( P.do
        projected <- plet (pvalueWithoutAsset # value # depositPolicyId # eventAssetName)
        lovelace <- plet (pquantityOfValue # projected # padaSymbolData # pdata padaToken)
        pif (lovelace #>= 0) `flip` perror $
          pcon $
            PMidgardValue
              { pvalue'lovelace = pdata lovelace
              , pvalue'assets = pdata (pcardanoAssetPairsToMidgard # projected)
              }
    )
    perror

{- | Aiken @proof.deposit_datum_cbor@.

The depositor's L2 datum, carried into the projected output as raw CBOR. The
field is an @Option<Data>@, which on the wire is @Constr 0 [datum]@ or
@Constr 1 []@, so the tag is read directly rather than through a decode.

Anything else __aborts__. Aiken's @expect@ on the surrounding @DepositInfo@ would
have refused a third tag before this function ever saw it; the port's cast is
unchecked, so the refusal has to be here instead.
-}
pdepositDatumCbor ::
  forall (s :: S). Term s PData -> Term s (PMaybeData PByteString)
pdepositDatumCbor datum =
  let (tag, fields) = pconstrOfData datum
   in pif (tag #== 0) (pcon (PDJust (pdata (pserialiseData # (phead # fields))))) $
        pif (tag #== 1) (pcon PDNothing) perror

{- | Aiken @proof.projected_deposit_output_cbor@.

The one L2 output a deposit is supposed to produce, in the encoding the ledger
trie stores. Every part of it is determined — the address and datum by the
committed @DepositInfo@, the value by the reference input the NFT authenticated —
so there is nothing here for a prover to choose, which is what makes the root
comparison in 'pvalidateValidDepositTransition' a statement about the operator.

No script reference: a deposit projects funds, never a script.
-}
pprojectedDepositOutputCbor ::
  forall (s :: S).
  Term s PDepositInfo ->
  Term s Value.PSortedValue ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PTokenName) ->
  Term s PByteString
pprojectedDepositOutputCbor depositInfo depositValue depositPolicyId eventAssetName = P.do
  PDepositInfo {pdepositInfo'l2Address, pdepositInfo'l2NetworkId, pdepositInfo'l2Datum} <-
    pmatch depositInfo
  pencodeMidgardTxOutput
    #$ pcon
    $ PMidgardTxOutput
      { ptxOutput'address =
          pdata
            ( pdepositAddressToMidgard
                (pfromData pdepositInfo'l2Address)
                (pfromData pdepositInfo'l2NetworkId)
            )
      , ptxOutput'value =
          pdata (pprojectedDepositValue depositValue depositPolicyId eventAssetName)
      , ptxOutput'datumCbor = pdata (pdepositDatumCbor pdepositInfo'l2Datum)
      , ptxOutput'scriptRef = pdata (pcon PDNothing)
      }

{- | Aiken @proof.AuthenticatedDepositReference@.

Scott-encoded: it is a return shape, not a wire shape, so nothing here ever
crosses a data boundary.

The event is kept as its two raw @Data@ fields rather than as a @DepositEvent@.
Aiken casts them with @expect@, which validates the shape; the port's cast does
not, so a typed field here would claim a check it has not made. Both consumers
compare the id against a committed key and read the info through the projection,
and both of those refuse a malformed value on their own.
-}
data PAuthenticatedDepositReference (s :: S) = PAuthenticatedDepositReference
  { pauthDeposit'id :: Term s PData
  , pauthDeposit'info :: Term s PData
  , pauthDeposit'inclusionTime :: Term s PInteger
  , pauthDeposit'value :: Term s Value.PSortedValue
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PAuthenticatedDepositReference)

{- | Aiken @proof.get_authenticated_deposit_reference@.

The deposit event this fault is about, read off a reference input the caller
points at by index.

__Not__ 'Midgard.Common.Utils.pgetAuthenticInputWithNftAt', which is the usual
way to authenticate a state UTxO and would be wrong here: that one requires the
input to hold /exactly one/ non-Ada asset, and a deposit UTxO holds the user's
funds alongside its NFT. Authenticity rests on the NFT being present exactly
once, and the rest of the value is the thing being projected.

The datum's first two fields are read positionally, as Aiken does — every
optimistic event datum begins with the event and its inclusion time, so the read
does not need to know which event this is.
-}
pgetAuthenticatedDepositReference ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PInteger
        :--> PAuthenticatedDepositReference
    )
pgetAuthenticatedDepositReference = phoistAcyclic $
  plam $ \referenceInputs depositPolicyId eventAssetName eventRefInputIndex -> P.do
    PTxInInfo {ptxInInfo'resolved} <-
      pmatch (pfromData (pelemAt # eventRefInputIndex # referenceInputs))
    PTxOut {ptxOut'value, ptxOut'datum} <- pmatch ptxInInfo'resolved
    datumData <-
      plet
        ( pmatch ptxOut'datum $ \case
            POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
            _ -> perror
        )
    value <- plet (pto (pfromData ptxOut'value))
    pif (pquantityOfValue # value # depositPolicyId # eventAssetName #== 1) `flip` perror $ P.do
      fields <- plet (psndBuiltin # (pasConstr # datumData))
      let (depositIdData, depositInfoData) =
            punsafeEventToKeyValuePair (phead # fields)
      pcon $
        PAuthenticatedDepositReference
          { pauthDeposit'id = depositIdData
          , pauthDeposit'info = depositInfoData
          , pauthDeposit'inclusionTime = pasInt #$ phead #$ ptail # fields
          , pauthDeposit'value = value
          }

{- | Aiken @proof.validate_valid_deposit_transition@.

A deposit's step is fully determined: it inserts one UTxO, keyed by the deposit's
own L1 output reference and valued at the projection of the L1 funds. The fault
is that the step published a different root.

=== The witness is checked, then rebuilt

The prover supplies a @LedgerInsertWitness@, and both its key and its value are
required to equal the derived ones — but the insertion is applied to a witness
rebuilt from the /derived/ key and value, keeping only the prover's two proofs.
That is Aiken's shape and it is worth keeping: the equalities and the rebuild say
the same thing, so the root being compared is not one a prover could have steered
by supplying a key the equality happened not to cover.

=== Where the reference input's value comes from

The deposit NFT is what ties the reference input to the committed event, and it
does so twice over — the input must hold it exactly once (or
'pgetAuthenticatedDepositReference' aborts), and the id and info read out of that
input must equal the ones the deposits tree committed. Without the second the
prover could point at any UTxO carrying the same asset name.
-}
pvalidateValidDepositTransition ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PHubOracleDatum ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PIndexedTraceProof ->
  Term s PRootMembershipProof ->
  Term s PRootMembershipProof ->
  Term s PInteger ->
  Term s (PAsData PTokenName) ->
  Term s PLedgerInsertWitness ->
  Term s PBool
pvalidateValidDepositTransition
  header
  hubDatum
  referenceInputs
  traceProof
  eventToStep
  sourceMembership
  eventRefInputIndex
  eventAssetName
  projectedUtxo = P.do
    PHubOracleDatum {phubOracle'deposit} <- pmatch hubDatum
    PRootMembershipProof
      { prootMembership'key = sourceKey
      , prootMembership'value = sourceValue
      } <-
      pmatch sourceMembership
    PRootMembershipProof {prootMembership'value = stepValue} <- pmatch traceProof
    PTransitionStep {ptransitionStep'preUtxosRoot, ptransitionStep'postUtxosRoot} <-
      pmatch (pcoerceData stepValue)
    PLedgerInsertWitness
      { pledgerInsert'key
      , pledgerInsert'value
      , pledgerInsert'nonMembershipProof
      , pledgerInsert'insertProof
      } <-
      pmatch projectedUtxo
    PAuthenticatedDepositReference
      { pauthDeposit'id
      , pauthDeposit'info
      , pauthDeposit'value
      } <-
      pmatch
        ( pgetAuthenticatedDepositReference
            # referenceInputs
            # phubOracle'deposit
            # eventAssetName
            # eventRefInputIndex
        )
    expectedKey <- plet (pledgerOutrefKey sourceKey)
    expectedValue <-
      plet
        ( pprojectedDepositOutputCbor
            (pcoerceData sourceValue)
            pauthDeposit'value
            phubOracle'deposit
            eventAssetName
        )
    checkedProjectedUtxo <-
      plet $
        pcon $
          PLedgerInsertWitness
            { pledgerInsert'key = pdata expectedKey
            , pledgerInsert'value = pdata expectedValue
            , pledgerInsert'nonMembershipProof = pledgerInsert'nonMembershipProof
            , pledgerInsert'insertProof = pledgerInsert'insertProof
            }
    expectedPostRoot <-
      plet
        ( papplyInsertWitness
            (pfromData ptransitionStep'preUtxosRoot)
            checkedProjectedUtxo
        )
    pvalidateDepositOneStepBinding header traceProof eventToStep sourceMembership
      #&& pauthDeposit'id
      #== sourceKey
      #&& pauthDeposit'info
      #== sourceValue
      #&& pledgerInsert'key
      #== pdata expectedKey
      #&& pledgerInsert'value
      #== pdata expectedValue
      #&& pnot
      # (expectedPostRoot #== pfromData ptransitionStep'postUtxosRoot)

--------------------------------------------------------------------------------
-- Whether an L1 event was due
--------------------------------------------------------------------------------

{- | Aiken @proof.timed_l1_event_is_due@.

The half-open window a block owes: an event included after the block began and
no later than it ended. Half-open at the bottom because two adjacent blocks must
not both owe the same event — an event at exactly @start_time@ belongs to the
block that ended there.
-}
ptimedL1EventIsDue ::
  forall (s :: S). Term s PHeaderV1 -> Term s PInteger -> Term s PBool
ptimedL1EventIsDue header inclusionTime = P.do
  PHeaderV1 {pheader'startTime, pheader'endTime} <- pmatch header
  pfromData pheader'startTime
    #< inclusionTime
    #&& inclusionTime
    #<= pfromData pheader'endTime

{- | Aiken @proof.forced_tx_is_due@.

A forced transaction is due when its validity interval overlaps the block's. Both
ends are optional and spelled @-1@, which is why this is three cases rather than
one pair of comparisons — an open end is not a large number, and treating it as
one would make an unbounded transaction due in every block from now on.

Note the closed comparisons throughout: unlike the inclusion window above, an
interval touching the block's boundary does overlap it.
-}
pforcedTxIsDue ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PBool
pforcedTxIsDue header validityIntervalStart validityIntervalEnd = P.do
  PHeaderV1 {pheader'startTime, pheader'endTime} <- pmatch header
  startTime <- plet (pfromData pheader'startTime)
  endTime <- plet (pfromData pheader'endTime)
  pif
    (validityIntervalStart #== -1)
    (validityIntervalEnd #== -1 #|| startTime #<= validityIntervalEnd)
    $ pif
      (validityIntervalEnd #== -1)
      (validityIntervalStart #<= endTime)
      ( validityIntervalStart
          #<= validityIntervalEnd
          #&& validityIntervalStart
          #<= endTime
          #&& startTime
          #<= validityIntervalEnd
      )

{- | Aiken @proof.forced_event_is_due@ — both windows at once.

A forced transaction owes two things: it reached L1 inside the block's inclusion
window, and its own validity interval overlaps the block. Either alone is not a
debt — an order that arrived in time but expired before the block began is not
one the operator had to include.
-}
pforcedEventIsDue ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PInteger ->
  Term s PNativeTxBodyCompact ->
  Term s PBool
pforcedEventIsDue header inclusionTime compactBody = P.do
  PNativeTxBodyCompact
    {pbodyCompact'validityIntervalStart, pbodyCompact'validityIntervalEnd} <-
    pmatch compactBody
  ptimedL1EventIsDue header inclusionTime
    #&& pforcedTxIsDue
      header
      pbodyCompact'validityIntervalStart
      pbodyCompact'validityIntervalEnd

{- | Aiken @proof.tx_order_compact_body@.

The scalar body of a forced transaction, re-derived from the order's own compact
bytes under its own transaction id. §4/§8.8: only the validity interval is read,
so the id the compact bytes re-derive is the whole anchor and the retired
proof-source commitment is not consulted.

The verification is not incidental. Without it the validity interval would be
whatever the order's datum said, and an operator could be convicted of omitting a
transaction whose window was invented by the prover.
-}
ptxOrderCompactBody ::
  forall (s :: S). Term s PTxOrderPayloadV1 -> Term s PNativeTxBodyCompact
ptxOrderCompactBody payload = P.do
  PTxOrderPayloadV1 {ptxOrderPayload'txId, ptxOrderPayload'source} <- pmatch payload
  PNativeTxProofSourceV1 {pnativeSource'compactCbor} <-
    pmatch (pfromData ptxOrderPayload'source)
  PVerifiedMidgardNativeTxCompact {pverified'txCompact} <-
    pmatch
      ( pverifyNativeTxCompactCborV1
          # pfromData ptxOrderPayload'txId
          # pfromData pnativeSource'compactCbor
      )
  pmatch pverified'txCompact $ \PNativeTxCompact {pcompact'body} -> pcompact'body

{- | Aiken @proof.get_deposit_event_with_nft@, as its two read fields.

Aiken returns a @TimedDepositEvent@; here the pair falls out of
'pgetAuthenticatedDepositReference' directly, since the only difference between
the two is which fields of the same read the caller goes on to use.
-}
pgetWithdrawalDatumWithNft ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PInteger
        :--> PWithdrawalDatum
    )
pgetWithdrawalDatumWithNft = phoistAcyclic $
  plam $ \referenceInputs policyId assetName index ->
    pcoerceData (pgetAuthenticInputDatumWithNftAt # referenceInputs # policyId # assetName # index)

{- | Aiken @proof.get_tx_order_datum_with_nft@.

Unlike the deposit read, this one /does/ go through
'Midgard.Common.Utils.pgetAuthenticInputDatumWithNftAt': an order UTxO holds its
NFT and nothing else, so the single-asset check that would be wrong for a deposit
is right here, and it is Aiken's own choice of helper.
-}
pgetTxOrderDatumWithNft ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PInteger
        :--> PTxOrderDatum
    )
pgetTxOrderDatumWithNft = phoistAcyclic $
  plam $ \referenceInputs policyId assetName index ->
    pcoerceData (pgetAuthenticInputDatumWithNftAt # referenceInputs # policyId # assetName # index)

--------------------------------------------------------------------------------
-- The L1-event faults
--------------------------------------------------------------------------------

{- | Aiken @proof.OmittedDueL1EventWitness@ — an event the block owed and skipped.

Each arm points at the L1 UTxO carrying the event and at the source tree the
event should have been in. The forced arm carries a @validity_override@ that
nothing reads: an omission is an omission whatever verdict the operator would
have recorded, and the field is kept only because the redeemer's shape is shared
with the out-of-window witness below, where it does matter.
-}
data POmittedDueL1EventWitness (s :: S)
  = POmittedDueDeposit
      { pomittedDeposit'eventRefInputIndex :: Term s (PAsData PInteger)
      , pomittedDeposit'eventAssetName :: Term s (PAsData PTokenName)
      , pomittedDeposit'sourceNonMembership :: Term s (PAsData PRootNonMembershipProof)
      }
  | POmittedDueWithdrawal
      { pomittedWithdrawal'eventRefInputIndex :: Term s (PAsData PInteger)
      , pomittedWithdrawal'eventAssetName :: Term s (PAsData PTokenName)
      , pomittedWithdrawal'sourceNonMembership :: Term s (PAsData PRootNonMembershipProof)
      }
  | POmittedDueForcedTransaction
      { pomittedForced'eventRefInputIndex :: Term s (PAsData PInteger)
      , pomittedForced'eventAssetName :: Term s (PAsData PTokenName)
      , pomittedForced'validityOverride :: Term s (PAsData PMidgardTxValidity)
      , pomittedForced'sourceNonMembership :: Term s (PAsData PRootNonMembershipProof)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POmittedDueL1EventWitness)

{- | Aiken @proof.OutOfWindowSourceEventWitness@ — an event the block took early
or late.

The mirror image of the witness above: the event is /in/ the source tree, and the
fault is that it was not due. The two non-deposit arms carry a validity override
because the tree stores the operator's verdict alongside the event, and the fault
must be stated against the leaf the block actually committed — whatever verdict
that leaf records.
-}
data POutOfWindowSourceEventWitness (s :: S)
  = POutOfWindowDeposit
      { poutOfWindowDeposit'eventRefInputIndex :: Term s (PAsData PInteger)
      , poutOfWindowDeposit'eventAssetName :: Term s (PAsData PTokenName)
      , poutOfWindowDeposit'sourceMembership :: Term s (PAsData PRootMembershipProof)
      }
  | POutOfWindowWithdrawal
      { poutOfWindowWithdrawal'eventRefInputIndex :: Term s (PAsData PInteger)
      , poutOfWindowWithdrawal'eventAssetName :: Term s (PAsData PTokenName)
      , poutOfWindowWithdrawal'validityOverride :: Term s (PAsData PWithdrawalValidity)
      , poutOfWindowWithdrawal'sourceMembership :: Term s (PAsData PRootMembershipProof)
      }
  | POutOfWindowForcedTransaction
      { poutOfWindowForced'eventRefInputIndex :: Term s (PAsData PInteger)
      , poutOfWindowForced'eventAssetName :: Term s (PAsData PTokenName)
      , poutOfWindowForced'validityOverride :: Term s (PAsData PMidgardTxValidity)
      , poutOfWindowForced'sourceMembership :: Term s (PAsData PRootMembershipProof)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POutOfWindowSourceEventWitness)

{- | Aiken @proof.validate_omitted_due_l1_event@.

An L1 event the block was obliged to apply and did not. Each arm reads the event
off the UTxO its own NFT authenticates, establishes that the block owed it, and
proves it absent from the source tree the header committed.

All three clauses are load-bearing together. The absence alone says nothing — a
block owes only the events in its window — and the due-ness alone says nothing
either, since the event may well be in the tree. The key equality is what ties
the absence being proven to the event just read, rather than to some other key
the prover chose.
-}
pvalidateOmittedDueL1Event ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PHubOracleDatum ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData POmittedDueL1EventWitness) ->
  Term s PBool
pvalidateOmittedDueL1Event header hubDatum referenceInputs witness = P.do
  PHubOracleDatum {phubOracle'deposit, phubOracle'withdrawal, phubOracle'txOrder} <-
    pmatch hubDatum
  PHeaderV1
    { pheader'withdrawalsRoot
    , pheader'withdrawalCount
    , pheader'forcedTransactionsRoot
    , pheader'forcedTransactionCount
    , pheader'depositsRoot
    , pheader'depositCount
    } <-
    pmatch header
  let absentFrom domain root count nonMembership key =
        pverifyRootNonMembershipWithKeyBytes
          nonMembership
          (pdata (pcon domain))
          (pfromData root)
          (pfromData count)
          (pserialiseData # key)
  pmatch (pfromData witness) $ \case
    POmittedDueDeposit
      { pomittedDeposit'eventRefInputIndex
      , pomittedDeposit'eventAssetName
      , pomittedDeposit'sourceNonMembership
      } -> P.do
        PAuthenticatedDepositReference {pauthDeposit'id, pauthDeposit'inclusionTime} <-
          pmatch
            ( pgetAuthenticatedDepositReference
                # referenceInputs
                # phubOracle'deposit
                # pomittedDeposit'eventAssetName
                # pfromData pomittedDeposit'eventRefInputIndex
            )
        nonMembership <- plet (pfromData pomittedDeposit'sourceNonMembership)
        PRootNonMembershipProof {prootNonMembership'key} <- pmatch nonMembership
        ptimedL1EventIsDue header pauthDeposit'inclusionTime
          #&& prootNonMembership'key
          #== pauthDeposit'id
          #&& absentFrom
            PDepositsRootDomain
            pheader'depositsRoot
            pheader'depositCount
            nonMembership
            prootNonMembership'key
    POmittedDueWithdrawal
      { pomittedWithdrawal'eventRefInputIndex
      , pomittedWithdrawal'eventAssetName
      , pomittedWithdrawal'sourceNonMembership
      } -> P.do
        PWithdrawalDatum {pwithdrawalDatum'event, pwithdrawalDatum'inclusionTime} <-
          pmatch
            ( pgetWithdrawalDatumWithNft
                # referenceInputs
                # phubOracle'withdrawal
                # pomittedWithdrawal'eventAssetName
                # pfromData pomittedWithdrawal'eventRefInputIndex
            )
        PWithdrawalEvent {pwithdrawalEvent'id} <- pmatch (pfromData pwithdrawalDatum'event)
        nonMembership <- plet (pfromData pomittedWithdrawal'sourceNonMembership)
        PRootNonMembershipProof {prootNonMembership'key} <- pmatch nonMembership
        ptimedL1EventIsDue header (pfromData pwithdrawalDatum'inclusionTime)
          #&& prootNonMembership'key
          #== pforgetData pwithdrawalEvent'id
          #&& absentFrom
            PWithdrawalsRootDomain
            pheader'withdrawalsRoot
            pheader'withdrawalCount
            nonMembership
            prootNonMembership'key
    POmittedDueForcedTransaction
      { pomittedForced'eventRefInputIndex
      , pomittedForced'eventAssetName
      , pomittedForced'sourceNonMembership
      } -> P.do
        PTxOrderDatum {ptxOrderDatum'event, ptxOrderDatum'inclusionTime} <-
          pmatch
            ( pgetTxOrderDatumWithNft
                # referenceInputs
                # phubOracle'txOrder
                # pomittedForced'eventAssetName
                # pfromData pomittedForced'eventRefInputIndex
            )
        PTxOrderEventV1 {ptxOrderEvent'id, ptxOrderEvent'tx} <-
          pmatch (pfromData ptxOrderDatum'event)
        nonMembership <- plet (pfromData pomittedForced'sourceNonMembership)
        PRootNonMembershipProof {prootNonMembership'key} <- pmatch nonMembership
        pforcedEventIsDue
          header
          (pfromData ptxOrderDatum'inclusionTime)
          (ptxOrderCompactBody (pfromData ptxOrderEvent'tx))
          #&& prootNonMembership'key
          #== pforgetData ptxOrderEvent'id
          #&& absentFrom
            PForcedTransactionsV1RootDomain
            pheader'forcedTransactionsRoot
            pheader'forcedTransactionCount
            nonMembership
            prootNonMembership'key

{- | Aiken @proof.validate_out_of_window_source_event@.

An event the block applied that it had no business applying — included before it
arrived, or after its window closed. The event is read off L1 the same way as
above, and the fault is that the source tree holds it anyway.

=== Why the two non-deposit arms rebuild the leaf

A source tree stores the operator's verdict beside the event, and the L1 datum
does not carry one. So the leaf being proven present cannot be compared against
the L1 event directly; the prover supplies the verdict, the arm rebuilds the leaf
the block would have committed under it, and the equality does the rest. That
leaves the prover free to choose the verdict — which costs nothing, because any
verdict at all still convicts a block that took an event out of its window, and a
wrong guess simply fails to match the committed leaf.
-}
pvalidateOutOfWindowSourceEvent ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PHubOracleDatum ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData POutOfWindowSourceEventWitness) ->
  Term s PBool
pvalidateOutOfWindowSourceEvent header hubDatum referenceInputs witness = P.do
  PHubOracleDatum {phubOracle'deposit, phubOracle'withdrawal, phubOracle'txOrder} <-
    pmatch hubDatum
  pmatch (pfromData witness) $ \case
    POutOfWindowDeposit
      { poutOfWindowDeposit'eventRefInputIndex
      , poutOfWindowDeposit'eventAssetName
      , poutOfWindowDeposit'sourceMembership
      } -> P.do
        PAuthenticatedDepositReference
          {pauthDeposit'id, pauthDeposit'info, pauthDeposit'inclusionTime} <-
          pmatch
            ( pgetAuthenticatedDepositReference
                # referenceInputs
                # phubOracle'deposit
                # poutOfWindowDeposit'eventAssetName
                # pfromData poutOfWindowDeposit'eventRefInputIndex
            )
        PRootMembershipProof {prootMembership'key, prootMembership'value} <-
          pmatch (pfromData poutOfWindowDeposit'sourceMembership)
        pnot
          # ptimedL1EventIsDue header pauthDeposit'inclusionTime
          #&& prootMembership'key
          #== pauthDeposit'id
          #&& prootMembership'value
          #== pauthDeposit'info
          #&& pverifySourceMembership header (psourceArm 3 poutOfWindowDeposit'sourceMembership)
    POutOfWindowWithdrawal
      { poutOfWindowWithdrawal'eventRefInputIndex
      , poutOfWindowWithdrawal'eventAssetName
      , poutOfWindowWithdrawal'validityOverride
      , poutOfWindowWithdrawal'sourceMembership
      } -> P.do
        PWithdrawalDatum {pwithdrawalDatum'event, pwithdrawalDatum'inclusionTime} <-
          pmatch
            ( pgetWithdrawalDatumWithNft
                # referenceInputs
                # phubOracle'withdrawal
                # poutOfWindowWithdrawal'eventAssetName
                # pfromData poutOfWindowWithdrawal'eventRefInputIndex
            )
        PWithdrawalEvent {pwithdrawalEvent'id, pwithdrawalEvent'info} <-
          pmatch (pfromData pwithdrawalDatum'event)
        PWithdrawalInfo {pwithdrawalInfo'body, pwithdrawalInfo'signature} <-
          pmatch (pfromData pwithdrawalEvent'info)
        expectedInfo <-
          plet $
            pdata $
              pcon $
                PWithdrawalInfo
                  { pwithdrawalInfo'body = pwithdrawalInfo'body
                  , pwithdrawalInfo'signature = pwithdrawalInfo'signature
                  , pwithdrawalInfo'validity = poutOfWindowWithdrawal'validityOverride
                  }
        PRootMembershipProof {prootMembership'key, prootMembership'value} <-
          pmatch (pfromData poutOfWindowWithdrawal'sourceMembership)
        pnot
          # ptimedL1EventIsDue header (pfromData pwithdrawalDatum'inclusionTime)
          #&& prootMembership'key
          #== pforgetData pwithdrawalEvent'id
          #&& prootMembership'value
          #== pforgetData expectedInfo
          #&& pverifySourceMembership header (psourceArm 0 poutOfWindowWithdrawal'sourceMembership)
    POutOfWindowForcedTransaction
      { poutOfWindowForced'eventRefInputIndex
      , poutOfWindowForced'eventAssetName
      , poutOfWindowForced'validityOverride
      , poutOfWindowForced'sourceMembership
      } -> P.do
        PTxOrderDatum {ptxOrderDatum'event, ptxOrderDatum'inclusionTime} <-
          pmatch
            ( pgetTxOrderDatumWithNft
                # referenceInputs
                # phubOracle'txOrder
                # poutOfWindowForced'eventAssetName
                # pfromData poutOfWindowForced'eventRefInputIndex
            )
        PTxOrderEventV1 {ptxOrderEvent'id, ptxOrderEvent'tx} <-
          pmatch (pfromData ptxOrderDatum'event)
        PTxOrderPayloadV1 {ptxOrderPayload'txId, ptxOrderPayload'source} <-
          pmatch (pfromData ptxOrderEvent'tx)
        expectedValue <-
          plet $
            pdata $
              pcon $
                PForcedInclusionTxV1
                  { pforcedTx'txId = ptxOrderPayload'txId
                  , pforcedTx'source = ptxOrderPayload'source
                  , pforcedTx'operatorValidity = poutOfWindowForced'validityOverride
                  }
        PRootMembershipProof {prootMembership'key, prootMembership'value} <-
          pmatch (pfromData poutOfWindowForced'sourceMembership)
        pnot
          # pforcedEventIsDue
            header
            (pfromData ptxOrderDatum'inclusionTime)
            (ptxOrderCompactBody (pfromData ptxOrderEvent'tx))
          #&& prootMembership'key
          #== pforgetData ptxOrderEvent'id
          #&& prootMembership'value
          #== pforgetData expectedValue
          #&& pverifySourceMembership header (psourceArm 1 poutOfWindowForced'sourceMembership)

{- | One arm of 'PSourceMembershipProof', built around a membership proof.

The four arms are structurally identical — see this module's header — so the arm
is a tag and its only field, and building it this way keeps the three call sites
above from each spelling out a constructor whose choice is the only thing that
distinguishes them.
-}
psourceArm ::
  forall (s :: S).
  Integer ->
  Term s (PAsData PRootMembershipProof) ->
  Term s (PAsData PSourceMembershipProof)
psourceArm tag membership =
  punsafeCoerce
    (pconstrBuiltin # pconstant tag # (pcons # pforgetData membership # pcon PNil))

--------------------------------------------------------------------------------
-- The one-step dispatcher
--------------------------------------------------------------------------------

{- | Aiken @proof.InvalidOneStepTransitionWitness@ — which transition is being
disputed, and the evidence for it.

The L2 arm carries __one field Aiken's does not__: the proof-source triple. That
is the redeemer cost of the decoder collapse described on
'pvalidateL2TransactionTransition' — Aiken deserialises the committed leaf and
this port rebuilds it from a supplied triple instead. It sits last so that every
field before it is positionally the Aiken one, which is what a cross-checking
reader will want.
-}
data PInvalidOneStepTransitionWitness (s :: S)
  = PValidWithdrawalTransition
      { pvalidWithdrawal'traceProof :: Term s (PAsData PIndexedTraceProof)
      , pvalidWithdrawal'eventToStep :: Term s (PAsData PRootMembershipProof)
      , pvalidWithdrawal'sourceMembership :: Term s (PAsData PRootMembershipProof)
      , pvalidWithdrawal'spentUtxo :: Term s (PAsData PLedgerDeleteWitness)
      }
  | PInvalidWithdrawalNoOpTransition
      { pnoOpWithdrawal'traceProof :: Term s (PAsData PIndexedTraceProof)
      , pnoOpWithdrawal'eventToStep :: Term s (PAsData PRootMembershipProof)
      , pnoOpWithdrawal'sourceMembership :: Term s (PAsData PRootMembershipProof)
      }
  | PInvalidForcedTransactionNoOpTransition
      { pnoOpForced'traceProof :: Term s (PAsData PIndexedTraceProof)
      , pnoOpForced'eventToStep :: Term s (PAsData PRootMembershipProof)
      , pnoOpForced'sourceMembership :: Term s (PAsData PRootMembershipProof)
      }
  | PValidDepositTransition
      { pvalidDeposit'traceProof :: Term s (PAsData PIndexedTraceProof)
      , pvalidDeposit'eventToStep :: Term s (PAsData PRootMembershipProof)
      , pvalidDeposit'sourceMembership :: Term s (PAsData PRootMembershipProof)
      , pvalidDeposit'eventRefInputIndex :: Term s (PAsData PInteger)
      , pvalidDeposit'eventAssetName :: Term s (PAsData PTokenName)
      , pvalidDeposit'projectedUtxo :: Term s (PAsData PLedgerInsertWitness)
      }
  | PL2TransactionTransition
      { pl2Transition'traceProof :: Term s (PAsData PIndexedTraceProof)
      , pl2Transition'eventToStep :: Term s (PAsData PRootMembershipProof)
      , pl2Transition'sourceMembership :: Term s (PAsData PRootMembershipProof)
      , pl2Transition'spendInputsPreimage :: Term s (PAsData PByteString)
      , pl2Transition'outputsPreimage :: Term s (PAsData PByteString)
      , pl2Transition'spentUtxos :: Term s (PAsData (PBuiltinList (PAsData PLedgerDeleteWitness)))
      , pl2Transition'producedUtxos :: Term s (PAsData (PBuiltinList (PAsData PLedgerInsertWitness)))
      , pl2Transition'source :: Term s (PAsData PNativeTxProofSourceV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PInvalidOneStepTransitionWitness)

{- | Aiken @proof.validate_invalid_one_step_transition@.

The five one-step transitions behind one redeemer. Nothing here decides anything
— each arm hands its own evidence to the rule that owns it — but the arm /is/ the
choice of rule, and that is why the witness is a sum rather than a record with
optional fields: there is no shape in which a prover disputes a withdrawal with a
deposit's evidence.

Only two arms need the hub datum and the reference inputs, and only because a
deposit's funds live on L1. The other three are answered entirely out of what the
block committed.
-}
pvalidateInvalidOneStepTransition ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s PHubOracleDatum ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PInvalidOneStepTransitionWitness) ->
  Term s PBool
pvalidateInvalidOneStepTransition header hubDatum referenceInputs witness =
  pmatch (pfromData witness) $ \case
    PValidWithdrawalTransition
      { pvalidWithdrawal'traceProof
      , pvalidWithdrawal'eventToStep
      , pvalidWithdrawal'sourceMembership
      , pvalidWithdrawal'spentUtxo
      } ->
        pvalidateValidWithdrawalTransition
          header
          (pfromData pvalidWithdrawal'traceProof)
          (pfromData pvalidWithdrawal'eventToStep)
          (pfromData pvalidWithdrawal'sourceMembership)
          (pfromData pvalidWithdrawal'spentUtxo)
    PInvalidWithdrawalNoOpTransition
      { pnoOpWithdrawal'traceProof
      , pnoOpWithdrawal'eventToStep
      , pnoOpWithdrawal'sourceMembership
      } ->
        pvalidateInvalidWithdrawalNoOpTransition
          header
          (pfromData pnoOpWithdrawal'traceProof)
          (pfromData pnoOpWithdrawal'eventToStep)
          (pfromData pnoOpWithdrawal'sourceMembership)
    PInvalidForcedTransactionNoOpTransition
      { pnoOpForced'traceProof
      , pnoOpForced'eventToStep
      , pnoOpForced'sourceMembership
      } ->
        pvalidateInvalidForcedTransactionNoOpTransition
          header
          (pfromData pnoOpForced'traceProof)
          (pfromData pnoOpForced'eventToStep)
          (pfromData pnoOpForced'sourceMembership)
    PValidDepositTransition
      { pvalidDeposit'traceProof
      , pvalidDeposit'eventToStep
      , pvalidDeposit'sourceMembership
      , pvalidDeposit'eventRefInputIndex
      , pvalidDeposit'eventAssetName
      , pvalidDeposit'projectedUtxo
      } ->
        pvalidateValidDepositTransition
          header
          hubDatum
          referenceInputs
          (pfromData pvalidDeposit'traceProof)
          (pfromData pvalidDeposit'eventToStep)
          (pfromData pvalidDeposit'sourceMembership)
          (pfromData pvalidDeposit'eventRefInputIndex)
          pvalidDeposit'eventAssetName
          (pfromData pvalidDeposit'projectedUtxo)
    PL2TransactionTransition
      { pl2Transition'traceProof
      , pl2Transition'eventToStep
      , pl2Transition'sourceMembership
      , pl2Transition'spendInputsPreimage
      , pl2Transition'outputsPreimage
      , pl2Transition'spentUtxos
      , pl2Transition'producedUtxos
      , pl2Transition'source
      } ->
        pvalidateL2TransactionTransition
          header
          (pfromData pl2Transition'traceProof)
          (pfromData pl2Transition'eventToStep)
          (pfromData pl2Transition'sourceMembership)
          pl2Transition'source
          (pfromData pl2Transition'spendInputsPreimage)
          (pfromData pl2Transition'outputsPreimage)
          (pfromData pl2Transition'spentUtxos)
          (pfromData pl2Transition'producedUtxos)

--------------------------------------------------------------------------------
-- The fault proof and its envelope
--------------------------------------------------------------------------------

{- | Aiken @proof.transition_trace_fraud_category_id@ — this family's four-byte
catalogue id, which every one of its computation threads carries as the prefix of
its token name.
-}
ptransitionTraceFraudCategoryId :: forall (s :: S). Term s PByteString
ptransitionTraceFraudCategoryId = phexByteStr "00000004"

-- | Aiken @proof.TransitionFault@ — the ten faults a transition trace can carry.
data PTransitionFault (s :: S)
  = PTraceBoundaryFault
      { ptraceBoundaryFault'side :: Term s (PAsData PTraceBoundarySide)
      , ptraceBoundaryFault'traceProof :: Term s (PAsData PIndexedTraceProof)
      }
  | PTraceLinkFault {ptraceLinkFault'adjacent :: Term s (PAsData PAdjacentTraceProof)}
  | PEventToStepMismatchFault
      { peventToStepMismatch'traceProof :: Term s (PAsData PIndexedTraceProof)
      , peventToStepMismatch'eventToStep :: Term s (PAsData PEventToStepProof)
      }
  | PSourceMembershipMismatchFault
      {psourceMismatchFault'witness :: Term s (PAsData PSourceMembershipMismatchWitness)}
  | PInvalidOneStepTransitionFault
      {poneStepFault'witness :: Term s (PAsData PInvalidOneStepTransitionWitness)}
  | POmittedDueL1EventFault
      {pomittedFault'witness :: Term s (PAsData POmittedDueL1EventWitness)}
  | PDuplicateTraceEventFault
      { pduplicateFault'leftTrace :: Term s (PAsData PIndexedTraceProof)
      , pduplicateFault'rightTrace :: Term s (PAsData PIndexedTraceProof)
      }
  | POutOfWindowSourceEventFault
      {poutOfWindowFault'witness :: Term s (PAsData POutOfWindowSourceEventWitness)}
  | PCountFault {pcountFault'witness :: Term s (PAsData PCountFaultWitness)}
  | PAcceptedTransactionTransitionMismatchFault
      {pacceptedMismatchFault'witness :: Term s (PAsData PAcceptedTransactionTransitionMismatchWitness)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTransitionFault)

{- | Aiken @proof.TransitionFaultProof@ — the header being convicted, its hash,
and the fault.

The header travels in full because every rule reads its roots and counts; the
hash travels beside it because the computation thread's token name is what binds
the whole proof to one block, and the envelope checks the two against each other.
-}
data PTransitionFaultProof (s :: S) = PTransitionFaultProof
  { ptransitionProof'challengedHeaderHash :: Term s (PAsData PHeaderHash)
  , ptransitionProof'header :: Term s (PAsData PHeaderV1)
  , ptransitionProof'fault :: Term s (PAsData PTransitionFault)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTransitionFaultProof)

{- | Aiken @proof.validate_transition_fault_proof_envelope@.

What every one of the nine entry points below checks before it looks at a fault
at all: the header in the redeemer really is the block the thread is about.

Three clauses, and none is redundant. The hash check ties the supplied header to
the hash; the length and prefix checks tie the token name to /this/ fault
category; the suffix check ties the token name to that same hash. Drop the last
and a prover could convict block A on a thread opened against block B.
-}
pvalidateTransitionFaultProofEnvelope ::
  forall (s :: S).
  Term s PTransitionFaultProof ->
  Term s (PAsData PTokenName) ->
  Term s PBool
pvalidateTransitionFaultProofEnvelope proof computationThreadTokenAssetName = P.do
  PTransitionFaultProof {ptransitionProof'challengedHeaderHash, ptransitionProof'header} <-
    pmatch proof
  challengedHeaderHash <- plet (pfromData ptransitionProof'challengedHeaderHash)
  nameBytes <- plet (pto (pfromData computationThreadTokenAssetName))
  (pblake2b_224 #$ pserialiseData # pforgetData ptransitionProof'header)
    #== challengedHeaderHash
    #&& plengthBS
    # nameBytes
    #== pidByteCount + 28
    #&& (psliceBS # 0 # pidByteCount # nameBytes)
    #== ptransitionTraceFraudCategoryId
    #&& (psliceBS # pidByteCount # 28 # nameBytes)
    #== challengedHeaderHash

{- | Aiken @proof.validate_transition_fault@ — every fault behind one redeemer.

Dispatch is by constructor tag rather than by 'pmatch'. Several arms would
otherwise share a body — see this module's header — and reading the tag keeps the
shape that survives a later refactor.
-}
pvalidateTransitionFault ::
  forall (s :: S).
  Term s PHeaderV1 ->
  Term s (PAsData PTransitionFault) ->
  Term s PHubOracleDatum ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PBool
pvalidateTransitionFault header fault hubDatum referenceInputs = P.do
  tag <- plet (fst (pconstrOf fault))
  fields <- plet (snd (pconstrOf fault))
  first <- plet (phead # fields)
  pif (tag #== 0) (pvalidateTraceBoundary header (punsafeCoerce first) (psecondOf fields)) $
    pif (tag #== 1) (pvalidateTraceLink header (pcoerceData first)) $
      pif
        (tag #== 2)
        (pvalidateEventToStepMismatch header (pcoerceData first) (psecondOf fields))
        $ pif (tag #== 3) (pvalidateSourceMembershipMismatch header (punsafeCoerce first))
        $ pif
          (tag #== 4)
          ( pvalidateInvalidOneStepTransition
              header
              hubDatum
              referenceInputs
              (punsafeCoerce first)
          )
        $ pif
          (tag #== 5)
          ( pvalidateOmittedDueL1Event
              header
              hubDatum
              referenceInputs
              (punsafeCoerce first)
          )
        $ pif
          (tag #== 6)
          (pvalidateDuplicateTraceEvent header (pcoerceData first) (psecondOf fields))
        $ pif
          (tag #== 7)
          ( pvalidateOutOfWindowSourceEvent
              header
              hubDatum
              referenceInputs
              (punsafeCoerce first)
          )
        $ pif (tag #== 8) (pvalidateCountFault header (punsafeCoerce first)) $
          pif
            (tag #== 9)
            (pvalidateAcceptedTransactionTransitionMismatch header (pcoerceData first))
            perror

-- | The second field of a constructor, decoded.
psecondOf ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a) =>
  Term s (PBuiltinList PData) ->
  Term s a
psecondOf fields = pcoerceData (phead #$ ptail # fields)

{- | The shared shape of the nine entry points: envelope, then one selection.

Aiken writes each of them as @and { envelope, when fault is { … _ -> False } }@,
and the wildcard is the part worth naming — an entry point accepts only the
faults its own validator is for, so pointing a control-fault thread at a deposit
fault is refused rather than answered.
-}
pfaultProofEntry ::
  forall (s :: S).
  Term s PTransitionFaultProof ->
  Term s (PAsData PTokenName) ->
  ( Term s PHeaderV1 ->
    Term s PInteger ->
    Term s (PAsData PTransitionFault) ->
    Term s (PBuiltinList PData) ->
    Term s PBool
  ) ->
  Term s PBool
pfaultProofEntry proof assetName selected = P.do
  PTransitionFaultProof {ptransitionProof'header, ptransitionProof'fault} <- pmatch proof
  pvalidateTransitionFaultProofEnvelope proof assetName
    #&& selected
      (pfromData ptransitionProof'header)
      (fst (pconstrOf ptransitionProof'fault))
      ptransitionProof'fault
      (snd (pconstrOf ptransitionProof'fault))

-- | Aiken @proof.validate_control_fault_proof@ — the four commitment-only faults.
pvalidateControlFaultProof ::
  forall (s :: S).
  Term s PTransitionFaultProof ->
  Term s (PAsData PTokenName) ->
  Term s PBool
pvalidateControlFaultProof proof assetName =
  pfaultProofEntry proof assetName $ \header tag _fault fields ->
    plet (phead # fields) $ \first ->
      pif (tag #== 0) (pvalidateTraceBoundary header (punsafeCoerce first) (psecondOf fields)) $
        pif (tag #== 1) (pvalidateTraceLink header (pcoerceData first)) $
          pif
            (tag #== 2)
            (pvalidateEventToStepMismatch header (pcoerceData first) (psecondOf fields))
            $ pif
              (tag #== 8)
              (pvalidateCountFault header (punsafeCoerce first))
              (pconstant False)

-- | Aiken @proof.validate_source_fault_proof@.
pvalidateSourceFaultProof ::
  forall (s :: S).
  Term s PTransitionFaultProof ->
  Term s (PAsData PTokenName) ->
  Term s PBool
pvalidateSourceFaultProof proof assetName =
  pfaultProofEntry proof assetName $ \header tag _fault fields ->
    pif
      (tag #== 3)
      (pvalidateSourceMembershipMismatch header (punsafeCoerce (phead # fields)))
      (pconstant False)

{- | Aiken @proof.validate_withdrawal_fault_proof@.

Two of the five one-step arms, which is why this reaches past the fault's own tag
into the witness's: a withdrawal validator answers for withdrawal transitions and
nothing else, and the one-step witness is where that distinction lives.

It calls the two rules directly rather than going through
'pvalidateInvalidOneStepTransition', as Aiken does — which is also what keeps the
hub datum and the reference inputs out of this entry point's signature. Neither
withdrawal rule reads L1 state.
-}
pvalidateWithdrawalFaultProof ::
  forall (s :: S).
  Term s PTransitionFaultProof ->
  Term s (PAsData PTokenName) ->
  Term s PBool
pvalidateWithdrawalFaultProof proof assetName =
  poneStepFaultProofEntry proof assetName $ \header witnessTag wf ->
    pif
      (witnessTag #== 0)
      ( pvalidateValidWithdrawalTransition
          header
          (pcoerceData (pfieldAt 0 wf))
          (pcoerceData (pfieldAt 1 wf))
          (pcoerceData (pfieldAt 2 wf))
          (pcoerceData (pfieldAt 3 wf))
      )
      $ pif
        (witnessTag #== 1)
        ( pvalidateInvalidWithdrawalNoOpTransition
            header
            (pcoerceData (pfieldAt 0 wf))
            (pcoerceData (pfieldAt 1 wf))
            (pcoerceData (pfieldAt 2 wf))
        )
        (pconstant False)

-- | Aiken @proof.validate_forced_fault_proof@ — the forced no-op arm alone.
pvalidateForcedFaultProof ::
  forall (s :: S).
  Term s PTransitionFaultProof ->
  Term s (PAsData PTokenName) ->
  Term s PBool
pvalidateForcedFaultProof proof assetName =
  poneStepFaultProofEntry proof assetName $ \header witnessTag wf ->
    pif
      (witnessTag #== 2)
      ( pvalidateInvalidForcedTransactionNoOpTransition
          header
          (pcoerceData (pfieldAt 0 wf))
          (pcoerceData (pfieldAt 1 wf))
          (pcoerceData (pfieldAt 2 wf))
      )
      (pconstant False)

-- | Aiken @proof.validate_accepted_transaction_fault_proof@.
pvalidateAcceptedTransactionFaultProof ::
  forall (s :: S).
  Term s PTransitionFaultProof ->
  Term s (PAsData PTokenName) ->
  Term s PBool
pvalidateAcceptedTransactionFaultProof proof assetName =
  pfaultProofEntry proof assetName $ \header tag _fault fields ->
    pif
      (tag #== 9)
      (pvalidateAcceptedTransactionTransitionMismatch header (pcoerceData $ phead # fields))
      $ pif (tag #== 4) `flip` pconstant False $ P.do
        witness <- plet (punsafeCoerce @(PAsData PInvalidOneStepTransitionWitness) (phead # fields))
        let witnessTag = fst (pconstrOf witness)
            wf = snd (pconstrOf witness)
        pif
          (witnessTag #== 4)
          ( pvalidateL2TransactionTransition
              header
              (pcoerceData (pfieldAt 0 wf))
              (pcoerceData (pfieldAt 1 wf))
              (pcoerceData (pfieldAt 2 wf))
              (punsafeCoerce (pfieldAt 7 wf))
              (pasByteStr # pfieldAt 3 wf)
              (pasByteStr # pfieldAt 4 wf)
              (punsafeCoerce (pasList # pfieldAt 5 wf))
              (punsafeCoerce (pasList # pfieldAt 6 wf))
          )
          (pconstant False)

{- | Aiken @proof.validate_deposit_fault_proof@ — the deposit transition arm.

The only one-step entry point that takes the hub datum and the reference inputs,
because the deposit transition is the only one-step rule that reads L1 state.
-}
pvalidateDepositFaultProof ::
  forall (s :: S).
  Term s PTransitionFaultProof ->
  Term s (PAsData PTokenName) ->
  Term s PHubOracleDatum ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PBool
pvalidateDepositFaultProof proof assetName hubDatum referenceInputs =
  poneStepFaultProofEntry proof assetName $ \header witnessTag wf ->
    pif
      (witnessTag #== 3)
      ( pvalidateValidDepositTransition
          header
          hubDatum
          referenceInputs
          (pcoerceData (pfieldAt 0 wf))
          (pcoerceData (pfieldAt 1 wf))
          (pcoerceData (pfieldAt 2 wf))
          (pasInt # pfieldAt 3 wf)
          (punsafeCoerce (pfieldAt 4 wf))
          (pcoerceData (pfieldAt 5 wf))
      )
      (pconstant False)

{- | The four entry points that answer for one arm of the one-step witness.

Each refuses twice over: once if the fault is not an @InvalidOneStepTransition@
at all, and once if it is but names a transition this validator is not for.
-}
poneStepFaultProofEntry ::
  forall (s :: S).
  Term s PTransitionFaultProof ->
  Term s (PAsData PTokenName) ->
  ( Term s PHeaderV1 ->
    Term s PInteger ->
    Term s (PBuiltinList PData) ->
    Term s PBool
  ) ->
  Term s PBool
poneStepFaultProofEntry proof assetName selected =
  pfaultProofEntry proof assetName $ \header tag _fault fields ->
    pif (tag #== 4) `flip` pconstant False $ P.do
      witness <- plet (punsafeCoerce @(PAsData PInvalidOneStepTransitionWitness) (phead # fields))
      selected header (fst (pconstrOf witness)) (snd (pconstrOf witness))

-- | The @n@th field of a constructor, still as @Data@.
pfieldAt ::
  forall (s :: S). Integer -> Term s (PBuiltinList PData) -> Term s PData
pfieldAt n fields = pelemAt # pconstant n # fields

-- | Aiken @proof.validate_l1_event_fault_proof@ — both L1-event faults.
pvalidateL1EventFaultProof ::
  forall (s :: S).
  Term s PTransitionFaultProof ->
  Term s (PAsData PTokenName) ->
  Term s PHubOracleDatum ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PBool
pvalidateL1EventFaultProof proof assetName hubDatum referenceInputs =
  pfaultProofEntry proof assetName $ \header tag _fault fields ->
    plet (phead # fields) $ \first ->
      pif
        (tag #== 5)
        (pvalidateOmittedDueL1Event header hubDatum referenceInputs (punsafeCoerce first))
        $ pif
          (tag #== 7)
          (pvalidateOutOfWindowSourceEvent header hubDatum referenceInputs (punsafeCoerce first))
          (pconstant False)

-- | Aiken @proof.validate_duplicate_fault_proof@.
pvalidateDuplicateFaultProof ::
  forall (s :: S).
  Term s PTransitionFaultProof ->
  Term s (PAsData PTokenName) ->
  Term s PBool
pvalidateDuplicateFaultProof proof assetName =
  pfaultProofEntry proof assetName $ \header tag _fault fields ->
    pif
      (tag #== 6)
      (pvalidateDuplicateTraceEvent header (pcoerceData (phead # fields)) (psecondOf fields))
      (pconstant False)

{- | Aiken @proof.validate_transition_fault_proof@ — the envelope and any fault.

The catch-all entry point, kept because Aiken keeps it. Note what it does /not/
do: the eight entry points above each refuse the faults they are not for, and
this one refuses none, so it is only sound where the thread it answers for is not
category-specific.
-}
pvalidateTransitionFaultProof ::
  forall (s :: S).
  Term s PTransitionFaultProof ->
  Term s (PAsData PTokenName) ->
  Term s PHubOracleDatum ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PBool
pvalidateTransitionFaultProof proof assetName hubDatum referenceInputs = P.do
  PTransitionFaultProof {ptransitionProof'header, ptransitionProof'fault} <- pmatch proof
  pvalidateTransitionFaultProofEnvelope proof assetName
    #&& pvalidateTransitionFault
      (pfromData ptransitionProof'header)
      ptransitionProof'fault
      hubDatum
      referenceInputs
