{- | Fixed-boundary Aiken transition-trace dispatchers. Inline faults route to
direct finals; carried L2, deposit and accepted-claim proofs route to staged
states. Rewarding validators authenticate each delegated phase, while these
spending validators retain thread custody, cancellation and conviction.
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

import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTokenName (..),
  PTxInInfo,
  PTxInfo (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Utils (pconstrOf, pheadSingleton)
import Midgard.ComputationThread (PStepDatum (..))
import Midgard.FraudProofs.Common (pcontinue, pfinalize)
import Midgard.FraudProofs.TransitionTrace.FinalV1 (PTransitionTraceFinalArgs (..))
import Midgard.FraudProofs.TransitionTrace.FinalYield qualified as Yield
import Midgard.FraudProofs.TransitionTrace.Proof (
  PTransitionFault,
  PTransitionFaultProof (..),
  pvalidateControlFaultProof,
  pvalidateDuplicateFaultProof,
  pvalidateForcedFaultProof,
  pvalidateL1EventFaultProof,
  pvalidateSourceFaultProof,
  pvalidateTransitionHeaderEnvelope,
  pvalidateWithdrawalFaultProof,
 )
import Midgard.FraudProofs.TransitionTrace.Proof qualified as Proof
import Midgard.FraudProofs.TransitionTrace.ProofCarriage qualified as Carriage
import Midgard.HubOracle (PHubOracleDatum, pgetDatum)
import Midgard.LedgerState (PHeaderV1)
import Midgard.LedgerState qualified as Ledger
import Midgard.StateQueueYield (prequireAuthenticatedZeroYield)
import Midgard.TransitionTrace qualified as Trace
import Midgard.ValidationClaim qualified as Claim
import Midgard.ValidationTrace qualified as Validation
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

-- | Direct finalization shared by the six non-yielded final validators.
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
          PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <-
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

-- | Aiken staged accepted-transaction dispatcher, including terminal claims.
transitionTraceAcceptedTransactionV1Validator ::
  forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
transitionTraceAcceptedTransactionV1Validator = plam $ \ctPolicy fpPolicy fpAddress authPolicy ctx ->
  ptransitionTraceYieldFinal False ctPolicy fpPolicy fpAddress authPolicy ctx

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

-- | Aiken staged deposit dispatcher. The hub parameter belongs to projection.
transitionTraceDepositV1Validator ::
  forall s.
  Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
transitionTraceDepositV1Validator = plam $ \ctPolicy fpPolicy fpAddress authPolicy ctx ->
  ptransitionTraceYieldFinal True ctPolicy fpPolicy fpAddress authPolicy ctx

{- | Aiken @validators/fraud-proofs/transition-trace/l1-event-v1.ak@.

Both L1-event faults authenticate their event reference through the hub oracle.
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
  , prouteArgs'proof :: Term s (PMaybeData PTransitionFaultProof)
  , prouteArgs'proofRefIndices :: Term s (PAsData (PBuiltinList (PAsData PInteger)))
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
          { prouteArgs'inputIndex
          , prouteArgs'outputIndex
          , prouteArgs'proof
          , prouteArgs'proofRefIndices
          } <-
          pmatch args
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch txInfo
        scriptHashes <- plet $ pfromData finalValidatorScriptHashes
        pcontinue
          ctPolicy
          (pexpectDatum datum)
          (pfromData prouteArgs'inputIndex)
          (pfromData prouteArgs'outputIndex)
          ownOutRef
          (pfromData ptxInfo'inputs)
          (pfromData ptxInfo'outputs)
          $ \_inputHash ctAssetName _prover inputState outputHash outputState ->
            pexpecting (pstateIsAbsent inputState) $
              pmatch prouteArgs'proof $ \case
                PDJust inline -> P.do
                  PTransitionFaultProof{ptransitionProof'fault} <- pmatch $ pfromData inline
                  index <- plet $ ptransitionFaultRouteIndex ptransitionProof'fault
                  pnull
                    # pfromData prouteArgs'proofRefIndices
                    #&& pnot
                    # (index #== 4 #|| index #== 5)
                    #&& outputHash
                    #== (pelemAt # index # scriptHashes)
                    #&& outputState
                    #== pforgetData inline
                PDNothing -> P.do
                  PPair commitment raw <- pmatch $ Carriage.popen # pfromData prouteArgs'proofRefIndices # pfromData ptxInfo'referenceInputs
                  PBuiltinPair proofTag fields <- pmatch $ pasConstr # raw
                  pexpecting (proofTag #== 0 #&& plength # fields #== 3) $ P.do
                    hash <- plet $ pasByteStr # (phead # fields)
                    header <- plet $ pfromData $ punsafeCoerce @(PAsData PHeaderV1) $ pelemAt # 1 # fields
                    PBuiltinPair faultTag faultFields <- pmatch $ pasConstr # (pelemAt # 2 # fields)
                    witness <- plet $ pheadSingleton # faultFields
                    kind <- plet $ pif (faultTag #== 9) 2 $ pexpecting (faultTag #== 4) $ P.do
                      PBuiltinPair witnessTag _ <- pmatch $ pasConstr # witness
                      pif (witnessTag #== 3) 1 $ pif (witnessTag #== 4) 0 perror
                    pvalidateTransitionHeaderEnvelope hash header ctAssetName
                      #&& outputHash
                      #== (pelemAt # (pif (kind #== 1) 5 4) # scriptHashes)
                      #&& outputState
                      #== pforgetData (pdata $ Yield.pinitial # kind # commitment)

{- | The staged accepted-transaction and deposit dispatchers share custody and
cancellation, while their phase tables select distinct authenticated roles.
-}
ptransitionTraceYieldFinal ::
  forall s.
  Bool ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PAddress) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PScriptContext ->
  Term s PUnit
ptransitionTraceYieldFinal deposit ctPolicy fpPolicy fpAddress authPolicy ctx =
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @Yield.PArgs ctPolicy datum redeemer ownRef tx $ \args -> P.do
      stepDatum <- plet $ pexpectDatum datum
      PStepDatum _ stateData <- pmatch stepDatum
      state <- plet $ pexpectStateAs @Yield.PState stateData
      st <- pmatch state
      a <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
      kind <- plet $ pfromData $ Yield.pstate'kind st
      phase <- plet $ pfromData $ Yield.pstate'phase st
      let refs = pfromData ptxInfo'referenceInputs
          fields = Carriage.pfields # (Carriage.pread # pfromData (Yield.pstate'proofCommitment st) # pfromData (Yield.pargs'proofRefIndices a) # refs)
          indices = pfromData $ Yield.pargs'yieldRefInputIndices a
          noYield = pnull # indices
          invoke role = plet (prequireAuthenticatedZeroYield # tx # pfromData authPolicy # pcon (PTokenName role) # pfromData (pheadSingleton # indices)) $ \_ -> pconstant True
          replayRole = pconstant $ if deposit then "V1FpTtF5ReplayYield" else "V1FpTtF4L2ReplayYield"
          role =
            if deposit
              then
                pif (phase #== 0 #|| phase #== 10) (pconstant "V1FpTtF5ProjectionYield") $
                  pif (phase #== 2) (pconstant "V1FpTtF5ScanYield") $
                    pif (phase #== 7) (pconstant "V1FpTtF5ValueYield") $
                      pif (phase #== 8) (pconstant "V1FpTtF5SummariesYield") $
                        pif (phase #== 3) (pconstant "V1FpTtF5AssemblyYield") $
                          pif (phase #== 9 #|| phase #== 4 #|| phase #== 5) replayRole perror
              else
                pif
                  (kind #== 2)
                  ( pif (phase #== 0) (pconstant "V1FpTtF4ClaimStructYield") $
                      pif (phase #== 1) (pconstant "V1FpTtF4ClaimSourceYield") $
                        pif (phase #== 2) (pconstant "V1FpTtF4ClaimEndsYield") perror
                  )
                  ( pif (phase #== 0) (pconstant "V1FpTtF4L2OpenYield") $
                      pif (phase #== 1 #|| phase #== 9 #|| phase #== 4) replayRole $
                        pif (phase #== 2) (pconstant "V1FpTtF4ScanYield") $
                          pif (phase #== 7) (pconstant "V1FpTtF4ValueYield") $
                            pif (phase #== 8) (pconstant "V1FpTtF4L2SummariesYield") $
                              pif (phase #== 3) (pconstant "V1FpTtF4L2AssemblyYield") perror
                  )
          bind = P.do
            PPair headerData fault <- pmatch fields
            witness <- plet $ Yield.poneStepWitness # fault # (if deposit then 3 else 4)
            pif
              (plength # witness #== (if deposit then 6 else 7))
              ( (if deposit then Proof.pvalidateDepositOneStepBinding else Proof.pvalidateL2OneStepBinding)
                  (pfromData $ punsafeCoerce @(PAsData PHeaderV1) headerData)
                  (pfromData $ punsafeCoerce @(PAsData Trace.PRootMembershipProof) $ pelemAt # 0 # witness)
                  (pfromData $ punsafeCoerce @(PAsData Trace.PRootMembershipProof) $ pelemAt # 1 # witness)
                  (pfromData $ punsafeCoerce @(PAsData Trace.PRootMembershipProof) $ pelemAt # 2 # witness)
              )
              perror
          launch = (if deposit then phase #== 6 else kind #== 0 #&& phase #== 6)
          terminal = if deposit then phase #== 5 else pif (kind #== 2) (phase #== 3) (phase #== 5)
          finalize rule =
            pfinalize
              ctPolicy
              fpPolicy
              fpAddress
              stepDatum
              (pfromData $ Yield.pargs'inputIndex a)
              (pfromData $ Yield.pargs'outputIndex a)
              (pfromData $ Yield.pargs'fraudProofMintRedeemerIndex a)
              ownRef
              (pfromData ptxInfo'inputs)
              (pfromData ptxInfo'outputs)
              (pto $ pto $ pfromData ptxInfo'redeemers)
              (\_ _ _ _ -> rule)
          terminalClaim = P.do
            PPair _ fault <- pmatch fields
            raw <- plet $ pheadSingleton # (psndBuiltin # (pasConstr # fault))
            Proof.PAcceptedTransactionTransitionMismatchWitness{Proof.pacceptedMismatch'claim, Proof.pacceptedMismatch'terminalAcceptanceWitnessCbor} <- pmatch $ pfromData $ punsafeCoerce @(PAsData Proof.PAcceptedTransactionTransitionMismatchWitness) raw
            Claim.PValidationClaimWitnessV1{Claim.pclaim'descriptorMembership, Claim.pclaim'transitionStepMembership, Claim.pclaim'terminalState} <- pmatch $ pfromData pacceptedMismatch'claim
            Trace.PRootMembershipProof{Trace.prootMembership'value = descriptor} <- pmatch $ pfromData pclaim'descriptorMembership
            Trace.PRootMembershipProof{Trace.prootMembership'value = transition} <- pmatch $ pfromData pclaim'transitionStepMembership
            Validation.PValidationTraceDescriptorV1{Validation.pdescriptor'verdict} <- pmatch $ pfromData $ punsafeCoerce @(PAsData Validation.PValidationTraceDescriptorV1) descriptor
            Ledger.PTransitionStep{Ledger.ptransitionStep'postUtxosRoot} <- pmatch $ pfromData $ punsafeCoerce @(PAsData Ledger.PTransitionStep) transition
            Validation.PValidationMachineStateV1{Validation.pmachineState'programCounter, Validation.pmachineState'workRoot} <- pmatch $ pfromData pclaim'terminalState
            let bytes = pfromData pacceptedMismatch'terminalAcceptanceWitnessCbor
            noYield
              #&& pfromData pdescriptor'verdict
              #== pcon Validation.PAccepted
              #&& pfromData pmachineState'workRoot
              #== (Validation.phashWorkWitness # pcon Validation.PTerminal # pfromData pmachineState'programCounter # bytes)
              #&& pnot
              # ((Proof.pterminalAcceptancePostRoot # bytes) #== pfromData ptransitionStep'postUtxosRoot)
      (if deposit then kind #== 1 else kind #== 0 #|| kind #== 2)
        #&& pif
          terminal
          (finalize $ if deposit then invoke replayRole else pif (kind #== 2) terminalClaim (invoke replayRole))
          ( pif
              launch
              (noYield #&& Yield.padvance # stepDatum # args # pfromData ptxInfo'outputs # pcon st{Yield.pstate'phase = pdata 0} #&& bind)
              (invoke role)
              #&& pcontinue
                ctPolicy
                stepDatum
                (pfromData $ Yield.pargs'inputIndex a)
                (pfromData $ Yield.pargs'outputIndex a)
                ownRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                (\inputHash _ _ _ outputHash _ -> inputHash #== outputHash)
          )
