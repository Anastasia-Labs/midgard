{- |
Module      : Midgard.Validators.FraudProofs.TransitionTrace
Description : Plutarch port of @validators/fraud-proofs/transition-trace/@.

The dispatch layer above "Midgard.FraudProofs.TransitionTrace.Proof": one router
and eight final validators, each of which ends a computation thread in a
conviction if the fault its own entry point recognises holds.

=== Why eight scripts and not one

Every entry point is a different subset of the same ten-armed fault type, and a
single script answering all ten would have to compile all ten rules — the
deposit projection chain, the L2 transition's two preimage walks, the L1-event
readers — into one budget. Splitting them means a challenger pays only for the
rule they are actually invoking. The cost is that the fault has to be routed to
the right script before it can be adjudicated, which is what @route-v1@ is for.

=== The router decides nothing about guilt

@route-v1@ reads the fault's constructor and nothing else. It does not open the
witness, check the header, or look at the thread's asset name; a proof it routes
is still entirely unadjudicated, and the final validator it lands at is what
convicts or refuses. Routing is a challenger-only step precisely because it
cannot go wrong in the operator's favour: sending a fault to the wrong script
gets it refused there, and sending it to the right one proves nothing by itself.

=== The routing table is a tag read, not a match

@route_index@ is a ten-arm @when@ in which four arms return @0@, two return @2@,
two return @4@ and two return @6@ — the Plutarch branch-selection hazard at its
worst shape (see the README). Both reads below go through 'pconstrOf' for that
reason: the fault's own tag, and then the nested one-step witness's.
-}
module Midgard.Validators.FraudProofs.TransitionTrace (
  -- * The eight final validators
  transitionTraceControlV1Validator,
  transitionTraceSourceV1Validator,
  transitionTraceWithdrawalV1Validator,
  transitionTraceForcedV1Validator,
  transitionTraceAcceptedTransactionV1Validator,
  transitionTraceDepositV1Validator,
  transitionTraceL1EventV1Validator,
  transitionTraceDuplicateV1Validator,

  -- * The router
  PTransitionTraceRouteArgs (..),
  poneStepWitnessRouteIndex,
  ptransitionFaultRouteIndex,
  transitionTraceRouteV1Validator,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTokenName,
  PTxInInfo,
  PTxInfo (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Utils (pconstrOf)
import Midgard.FraudProofs.Common (pcontinue, pfinalize)
import Midgard.FraudProofs.TransitionTrace.FinalV1 (PTransitionTraceFinalArgs (..))
import Midgard.FraudProofs.TransitionTrace.Proof (
  PTransitionFault,
  PTransitionFaultProof (..),
  pvalidateAcceptedTransactionFaultProof,
  pvalidateControlFaultProof,
  pvalidateDepositFaultProof,
  pvalidateDuplicateFaultProof,
  pvalidateForcedFaultProof,
  pvalidateL1EventFaultProof,
  pvalidateSourceFaultProof,
  pvalidateWithdrawalFaultProof,
 )
import Midgard.HubOracle (PHubOracleDatum, pgetDatum)
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pexpecting,
  pstateIsAbsent,
  pstep,
 )

--------------------------------------------------------------------------------
-- The shape the eight share
--------------------------------------------------------------------------------

{- | The whole of a final transition-trace validator except its rule.

All eight Aiken files are the same forty lines with one function name changed:
spend, cancel or continue, @finalize@, @expect Some(transition_proof)@, and then
the entry point. Factoring it here leaves each validator below as the one line
that actually differs.

The rule receives more than most of them use — the hub reference-input index and
the transaction's reference inputs — because @deposit-v1@ and @l1-event-v1@ need
both and the other six ignore them. That mirrors the shared @final_v1.Args@:
one redeemer shape for all eight, so that the router does not have to pick.
-}
ptransitionTraceFinal ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PAddress) ->
  Term s PScriptContext ->
  ( Term s PTransitionFaultProof ->
    Term s (PAsData PTokenName) ->
    Term s PInteger ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s PBool
  ) ->
  Term s PUnit
ptransitionTraceFinal
  computationThreadTokenPolicyId
  fraudProofTokenPolicyId
  fraudProofTokenAddress
  ctx
  rule =
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PTransitionTraceFinalArgs
        computationThreadTokenPolicyId
        datum
        redeemer
        ownOutRef
        txInfo
        $ \args -> P.do
          PTransitionTraceFinalArgs
            { pfinalArgs'inputIndex
            , pfinalArgs'outputIndex
            , pfinalArgs'hubRefInputIndex
            , pfinalArgs'fraudProofMintRedeemerIndex
            } <-
            pmatch args
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <-
            pmatch txInfo
          pfinalize
            computationThreadTokenPolicyId
            fraudProofTokenPolicyId
            fraudProofTokenAddress
            (pexpectDatum datum)
            (pfromData pfinalArgs'inputIndex)
            (pfromData pfinalArgs'outputIndex)
            (pfromData pfinalArgs'fraudProofMintRedeemerIndex)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            (pto (pto (pfromData ptxInfo'redeemers)))
            $ \_ownScriptHash threadTokenAssetName _fraudProver mInputStateData ->
              rule
                (pexpectStateAs @PTransitionFaultProof mInputStateData)
                threadTokenAssetName
                (pfromData pfinalArgs'hubRefInputIndex)
                (pfromData ptxInfo'referenceInputs)

--------------------------------------------------------------------------------
-- The six that need nothing but the proof
--------------------------------------------------------------------------------

-- | Aiken @validators/fraud-proofs/transition-trace/control-v1.ak@.
transitionTraceControlV1Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PScriptContext
        :--> PUnit
    )
transitionTraceControlV1Validator = plam $ \ctPolicy fpPolicy fpAddress ctx ->
  ptransitionTraceFinal ctPolicy fpPolicy fpAddress ctx $ \proof assetName _hubIndex _refs ->
    pvalidateControlFaultProof proof assetName

-- | Aiken @validators/fraud-proofs/transition-trace/source-v1.ak@.
transitionTraceSourceV1Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PScriptContext
        :--> PUnit
    )
transitionTraceSourceV1Validator = plam $ \ctPolicy fpPolicy fpAddress ctx ->
  ptransitionTraceFinal ctPolicy fpPolicy fpAddress ctx $ \proof assetName _hubIndex _refs ->
    pvalidateSourceFaultProof proof assetName

-- | Aiken @validators/fraud-proofs/transition-trace/withdrawal-v1.ak@.
transitionTraceWithdrawalV1Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PScriptContext
        :--> PUnit
    )
transitionTraceWithdrawalV1Validator = plam $ \ctPolicy fpPolicy fpAddress ctx ->
  ptransitionTraceFinal ctPolicy fpPolicy fpAddress ctx $ \proof assetName _hubIndex _refs ->
    pvalidateWithdrawalFaultProof proof assetName

-- | Aiken @validators/fraud-proofs/transition-trace/forced-v1.ak@.
transitionTraceForcedV1Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PScriptContext
        :--> PUnit
    )
transitionTraceForcedV1Validator = plam $ \ctPolicy fpPolicy fpAddress ctx ->
  ptransitionTraceFinal ctPolicy fpPolicy fpAddress ctx $ \proof assetName _hubIndex _refs ->
    pvalidateForcedFaultProof proof assetName

{- | Aiken @validators/fraud-proofs/transition-trace/accepted-transaction-v1.ak@.

Answers the L2 transition arm. The tenth fault —
@AcceptedTransactionTransitionMismatch@ — routes here too, and this port's entry
point refuses it rather than adjudicating it, because its rule waits on a CBOR
decoder. A challenger who routes one here gets a validator that declines to
convict, not one that aborts: see the entry point's own note.
-}
transitionTraceAcceptedTransactionV1Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PScriptContext
        :--> PUnit
    )
transitionTraceAcceptedTransactionV1Validator = plam $ \ctPolicy fpPolicy fpAddress ctx ->
  ptransitionTraceFinal ctPolicy fpPolicy fpAddress ctx $ \proof assetName _hubIndex _refs ->
    pvalidateAcceptedTransactionFaultProof proof assetName

-- | Aiken @validators/fraud-proofs/transition-trace/duplicate-v1.ak@.
transitionTraceDuplicateV1Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PScriptContext
        :--> PUnit
    )
transitionTraceDuplicateV1Validator = plam $ \ctPolicy fpPolicy fpAddress ctx ->
  ptransitionTraceFinal ctPolicy fpPolicy fpAddress ctx $ \proof assetName _hubIndex _refs ->
    pvalidateDuplicateFaultProof proof assetName

--------------------------------------------------------------------------------
-- The two that consult the hub oracle
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/transition-trace/deposit-v1.ak@.

The deposit transition reads the deposit policy out of the hub oracle and then
authenticates one deposit UTxO among the reference inputs against it, so this
validator takes the hub oracle's own policy id as a fourth parameter and passes
both the datum and the reference inputs down.
-}
transitionTraceDepositV1Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PAsData PScriptHash
        :--> PScriptContext
        :--> PUnit
    )
transitionTraceDepositV1Validator = plam $ \ctPolicy fpPolicy fpAddress hubOracle ctx ->
  ptransitionTraceFinal ctPolicy fpPolicy fpAddress ctx $ \proof assetName hubIndex refs ->
    pvalidateDepositFaultProof
      proof
      assetName
      (phubDatumAt hubOracle refs hubIndex)
      refs

{- | Aiken @validators/fraud-proofs/transition-trace/l1-event-v1.ak@.

Both L1-event faults reach for a withdrawal or transaction-order UTxO among the
reference inputs, and both find the policy that authenticates it in the hub
oracle — so this validator has the same shape as @deposit-v1@.
-}
transitionTraceL1EventV1Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PAsData PScriptHash
        :--> PScriptContext
        :--> PUnit
    )
transitionTraceL1EventV1Validator = plam $ \ctPolicy fpPolicy fpAddress hubOracle ctx ->
  ptransitionTraceFinal ctPolicy fpPolicy fpAddress ctx $ \proof assetName hubIndex refs ->
    pvalidateL1EventFaultProof
      proof
      assetName
      (phubDatumAt hubOracle refs hubIndex)
      refs

-- | Aiken @hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)@.
phubDatumAt ::
  forall (s :: S).
  Term s (PAsData PScriptHash) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PInteger ->
  Term s PHubOracleDatum
phubDatumAt hubOracle referenceInputs hubRefInputIndex =
  pgetDatum # referenceInputs # hubOracle # hubRefInputIndex

--------------------------------------------------------------------------------
-- The router
--------------------------------------------------------------------------------

{- | Aiken @route-v1.Args@.

Note the third field: the router is handed the /whole/ proof in its redeemer and
requires the output's state to equal it. That is what stops a challenger from
routing one proof and adjudicating another.
-}
data PTransitionTraceRouteArgs (s :: S) = PTransitionTraceRouteArgs
  { prouteArgs'inputIndex :: Term s (PAsData PInteger)
  , prouteArgs'outputIndex :: Term s (PAsData PInteger)
  , prouteArgs'proof :: Term s (PAsData PTransitionFaultProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTransitionTraceRouteArgs)

{- | The inner half of @route_index@: which validator adjudicates a one-step arm.

Withdrawal takes both of its arms, forced takes one, deposit takes one, and the
L2 transition goes to the accepted-transaction validator alongside the tenth
fault. Read by tag rather than matched, because the first two arms share an
answer.
-}
poneStepWitnessRouteIndex :: forall (s :: S) (a :: S -> Type). Term s (PAsData a) -> Term s PInteger
poneStepWitnessRouteIndex witness =
  plet (fst (pconstrOf witness)) $ \tag ->
    pif (tag #== 0 #|| tag #== 1) 2 $
      pif (tag #== 2) 3 $
        pif (tag #== 3) 5 $
          pif (tag #== 4) 4 perror

{- | Aiken @route-v1.route_index@ — which of the eight scripts adjudicates a fault.

@
  0  control          TraceBoundary, TraceLink, EventToStepMismatch, Count
  1  source           SourceMembershipMismatch
  2  withdrawal       one-step: valid withdrawal, invalid withdrawal no-op
  3  forced           one-step: invalid forced-transaction no-op
  4  accepted-tx      one-step: L2 transition, and AcceptedTransactionMismatch
  5  deposit          one-step: valid deposit
  6  l1-event         OmittedDueL1Event, OutOfWindowSourceEvent
  7  duplicate        DuplicateTraceEvent
@

Aiken's @when@ is total over ten constructors and so has no failure branch; a
tag outside @0..9@ cannot arise there because the redeemer is structurally
decoded before @route_index@ ever sees it. The port decodes positionally, so the
same impossibility is written here as an abort — reachable only on input Aiken
would have rejected at the boundary, never on a well-formed fault.
-}
ptransitionFaultRouteIndex :: forall (s :: S). Term s (PAsData PTransitionFault) -> Term s PInteger
ptransitionFaultRouteIndex fault =
  let (rawTag, fields) = pconstrOf fault
   in plet rawTag $ \tag ->
        pif (tag #== 0 #|| tag #== 1 #|| tag #== 2 #|| tag #== 8) 0 $
          pif (tag #== 3) 1 $
            pif (tag #== 4) (poneStepWitnessRouteIndex (punsafeCoerce (phead # fields))) $
              pif (tag #== 9) 4 $
                pif (tag #== 5 #|| tag #== 7) 6 $
                  pif (tag #== 6) 7 perror

{- | Aiken @validators/fraud-proofs/transition-trace/route-v1.ak@.

Moves a fresh thread from the router to the one final validator that can
adjudicate its fault. Three checks and nothing else: the parameterised list names
exactly eight scripts, the thread arrives with no state, and it leaves at the
routed script carrying the redeemer's proof verbatim.
-}
transitionTraceRouteV1Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
transitionTraceRouteV1Validator = plam $ \finalValidatorScriptHashes ctPolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PTransitionTraceRouteArgs ctPolicy datum redeemer ownOutRef txInfo $
      \args -> P.do
        PTransitionTraceRouteArgs
          {prouteArgs'inputIndex, prouteArgs'outputIndex, prouteArgs'proof} <-
          pmatch args
        PTxInfo {ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
        scriptHashes <- plet $ pfromData finalValidatorScriptHashes
        pexpecting (plength # scriptHashes #== 8) $
          pcontinue
            ctPolicy
            (pexpectDatum datum)
            (pfromData prouteArgs'inputIndex)
            (pfromData prouteArgs'outputIndex)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_inputScriptHash _ctAssetName _fraudProver mInputState outputScriptHash outputStateData ->
              P.do
                PTransitionFaultProof {ptransitionProof'fault} <-
                  pmatch (pfromData prouteArgs'proof)
                pexpecting (pstateIsAbsent mInputState) $
                  outputScriptHash
                    #== (pelemAt # ptransitionFaultRouteIndex ptransitionProof'fault # scriptHashes)
                    #&& outputStateData
                    #== pforgetData prouteArgs'proof
