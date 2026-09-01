{- |
Module      : Midgard.Validators.Scheduler
Description : Plutarch port of @validators/scheduler.ak@.

The scheduler is one UTxO holding one datum: which operator's shift it is, and
when that shift started. Every block committed to the state queue is checked
against it, so it is the protocol's answer to "whose turn is it".

Its whole job is advancing that datum by exactly one operator, and the seven
'PAdvancingApproach' branches are seven reasons the turn may pass:

  * the shift simply ended,
  * the operator went inactive and is being struck for it,
  * the operator is being removed from the active set entirely,
  * or nobody was scheduled and a first operator is being appointed.

The first three come in @GoToNext@/@Rewind@ pairs. The active-operator set is a
linked list walked in order, so advancing normally means stepping to the next
node — but from the last node the turn wraps to the first, which is a
structurally different transaction: it must show the /root/ rather than an
anchor node, and must additionally prove no registered operator was waiting to
be activated ahead of the wrap.

Two properties are worth stating up front, because they are what most of the
code is defending.

/The scheduler never decides anything on its own authority./ Whether an operator
was inactive is decided by the active set's @StrikeForInactivity@ redeemer;
whether it is being removed, and what remains after, is read out of the active
set's mint redeemer. This validator's role is to check that the two agree, and
in particular that they agree about /this/ scheduler UTxO — every cross-check
compares @scheduler_input_index@ on both sides.

/Advancing is permissionless, but only along a path the lists already imply./
No branch requires a signature. What stops abuse is that each branch has to
exhibit reference inputs whose linked-list structure only admits one successor.
-}
module Midgard.Validators.Scheduler (
  schedulerMintValidator,
  schedulerSpendValidator,
) where

import Data.Kind (Type)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Interval (PInterval)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PPosixTime,
  PPubKeyHash,
  PRedeemer,
  PScriptContext (..),
  PScriptHash (..),
  PScriptInfo (..),
  PScriptPurpose (..),
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import DesignPatterns.SingularUtxoIndexer (poneToOne)
import LinkedList (pgetElementInfo)
import LinkedList.Types (PLink)
import Midgard.Common.Utils (
  pauthenticateInputOutputAndGetOutputDatumData,
  pgetInclusiveBoundsOfAShortValidityRange,
  pgetRedeemerAt,
  pgetSingletonAssetWithPolicy,
  pgetSpendingRedeemerDataAt,
  pisEntirelyAfter,
  pisEntirelyBefore,
 )
import Midgard.Env qualified as Env
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.LedgerState (PConfirmedState (..), PHeaderV1 (..))
import Midgard.OperatorDirectory (PSlashingArguments (..))
import Midgard.OperatorDirectory.ActiveOperators (
  POperatorRemovalSchedulerSync (..),
 )
import Midgard.OperatorDirectory.ActiveOperators qualified as Active
import Midgard.OperatorDirectory.RegisteredOperators qualified as Registered
import Midgard.Scheduler (
  PAdvancingApproach (..),
  PMintRedeemer (..),
  PNeglectedUserEvent (..),
  POperatorRemovalReason (..),
  PSchedDatum (..),
  PSpendRedeemer (..),
  passetName,
  pshiftEndTime,
 )
import Midgard.StateQueue (PStateQueueNode (..), pdecodeHeaderView)
import Midgard.StateQueue qualified as StateQueue
import Midgard.UserEvents.Deposit (PDepositDatum (..))
import Midgard.UserEvents.Deposit qualified as Deposit
import Midgard.UserEvents.TxOrder (PTxOrderDatum (..))
import Midgard.UserEvents.TxOrder qualified as TxOrder
import Midgard.UserEvents.Withdrawal (PWithdrawalDatum (..))
import Midgard.UserEvents.Withdrawal qualified as Withdrawal

--------------------------------------------------------------------------------
-- Small shared helpers
--------------------------------------------------------------------------------

{- | A policy id reinterpreted as a script hash — the same 28 bytes, which Aiken
conflates as @ByteArray@.
-}
pscriptHashOf ::
  forall (s :: S). Term s (PAsData PCurrencySymbol) -> Term s (PAsData PScriptHash)
pscriptHashOf cs = pdata (pcon (PScriptHash (pto (pfromData cs))))

-- | Reinterpret another script's redeemer, read out of @redeemers@, as a known type.
punsafeCoerceRedeemer ::
  forall (a :: S -> Type) (s :: S). Term s (PAsData PRedeemer) -> Term s (PAsData a)
punsafeCoerceRedeemer r = punsafeCoerce (pto (pfromData r))

-- | Reinterpret this script's own redeemer as a known type.
punsafeCoerceOwnRedeemer ::
  forall (a :: S -> Type) (s :: S). Term s PRedeemer -> Term s (PAsData a)
punsafeCoerceOwnRedeemer r = punsafeCoerce (pto r)

-- | Reinterpret raw @Data@ as a known type.
punsafeCoerceData ::
  forall (a :: S -> Type) (s :: S). (PIsData a) => Term s PData -> Term s a
punsafeCoerceData d = pfromData (punsafeCoerce @(PAsData a) d)

-- | @expect Some(input) = list.at(inputs, index)@, then its resolved output.
presolvedOutputAt ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PInteger ->
  Term s PTxOut
presolvedOutputAt inputs index =
  pmatch (pfromData (pelemAt # index # inputs)) $
    \PTxInInfo {ptxInInfo'resolved} -> ptxInInfo'resolved

-- | Aiken @math.max@ (shadowing Plutarch's, which is on 'POrd' terms).
pmaxInt :: forall (s :: S). Term s PInteger -> Term s PInteger -> Term s PInteger
pmaxInt a b = pif (a #< b) b a

--------------------------------------------------------------------------------
-- Mint
--------------------------------------------------------------------------------

{- | Aiken @validators/scheduler.ak@ — @mint@.

The scheduler is a single UTxO, so its policy has only to create and destroy the
one NFT. Both branches bind it to the hub oracle's own NFT moving the same way,
which is what makes the scheduler exist exactly as long as the protocol
instance does — it cannot be spun up beside a live hub, nor left behind when one
is torn down.
-}
schedulerMintValidator ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
schedulerMintValidator = plam $ \hubOracleScriptHash ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  ownPolicyId <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PMintingScript cs -> cs
      _ -> perror
  PTxInfo {ptxInfo'mint} <- pmatch pscriptContext'txInfo
  mint <- plet $ pfromData ptxInfo'mint
  redeemer <-
    plet $ pfromData (punsafeCoerceOwnRedeemer @PMintRedeemer pscriptContext'redeemer)
  -- `and {}` short-circuits in Aiken and `get_singleton_asset_with_policy`
  -- errors on a policy that minted nothing, so these are chained with `#&&`
  -- rather than a strict conjunction.
  amount <-
    plet $ plam $ \n ->
      (pgetSingletonAssetWithPolicy # mint # hubOracleScriptHash #== hubPair n)
        #&& (pgetSingletonAssetWithPolicy # mint # ownPolicyId #== ownPair n)
  pif
    ( pmatch redeemer $ \case
        PInit -> amount # 1
        PDeinit -> amount # (-1)
    )
    (pconstant ())
    perror
  where
    hubPair n = ppairDataBuiltin # Hub.passetName # pdata n
    ownPair n = ppairDataBuiltin # passetName # pdata n

--------------------------------------------------------------------------------
-- Spend
--------------------------------------------------------------------------------

{- | Aiken @validators/scheduler.ak@ — @spend@.

Five parameters, all fixed at deployment: the three operator-set identities the
branches consult, the scheduler's own policy id (used to authenticate its input
and output), and the hub oracle's script hash.

The common prelude to all seven branches is the same three steps: locate the
scheduler's own input and its continuing output by index, refuse if they are not
the two the redeemer names, and require both to carry the scheduler NFT — which
is what @one_to_one@'s double-satisfaction flag and
@authenticate_input_output_and_get_output_datum_data@ do between them. Only then
does the branch's own reason get examined.
-}
schedulerSpendValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- registered operators policy id
        :--> PAsData PAddress -- active operators address
        :--> PAsData PCurrencySymbol -- active operators policy id
        :--> PAsData PCurrencySymbol -- scheduler policy id
        :--> PAsData PCurrencySymbol -- hub oracle script hash
        :--> PScriptContext
        :--> PUnit
    )
schedulerSpendValidator = plam $
  \registeredOperatorsPolicyId
   activeOperatorsAddress
   activeOperatorsPolicyId
   schedulerPolicyId
   hubOracleScriptHash
   ctx -> P.do
      PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
        pmatch ctx
      ownOutRef <-
        plet $ pmatch pscriptContext'scriptInfo $ \case
          PSpendingScript outRef _ -> outRef
          _ -> perror
      -- `expect Some(datum) = m_datum`: a scheduler UTxO without a datum is
      -- unspendable, not permissively spendable.
      datum <-
        plet $ pmatch pscriptContext'scriptInfo $ \case
          PSpendingScript _ mDatum ->
            pmatch mDatum $ \case
              PDJust d -> punsafeCoerceData @PSchedDatum (pto (pfromData d))
              PDNothing -> perror
          _ -> perror

      PTxInfo
        { ptxInfo'inputs
        , ptxInfo'outputs
        , ptxInfo'referenceInputs
        , ptxInfo'redeemers
        , ptxInfo'validRange
        } <-
        pmatch pscriptContext'txInfo
      inputs <- plet $ pfromData ptxInfo'inputs
      outputs <- plet $ pfromData ptxInfo'outputs
      referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
      redeemers <- plet $ pto (pto (pfromData ptxInfo'redeemers))
      validityRange <- plet ptxInfo'validRange

      PSpendRedeemer
        { pschedSpend'schedulerInputIndex
        , pschedSpend'schedulerOutputIndex
        , pschedSpend'advancingApproach
        } <-
        pmatch $ pfromData (punsafeCoerceOwnRedeemer @PSpendRedeemer pscriptContext'redeemer)
      schedulerInputIndex <- plet $ pfromData pschedSpend'schedulerInputIndex

      pif
        ( poneToOne
            schedulerInputIndex
            (pfromData pschedSpend'schedulerOutputIndex)
            ownOutRef
            inputs
            outputs
            (pconstant True)
            $ \input output -> P.do
              outputDatumData <-
                plet $
                  pmatch (pfromData input) $ \PTxInInfo {ptxInInfo'resolved} ->
                    pauthenticateInputOutputAndGetOutputDatumData
                      ptxInInfo'resolved
                      output
                      schedulerPolicyId

              pmatch (pfromData pschedSpend'advancingApproach) $ \case
                --------------------------------------------------------------
                PGoToNextDueToEndOfShift {pgoToNextEnd'newShiftsOperatorNodeRefInputIndex} ->
                  pvalidateEndOfShiftAndGetOperators datum outputDatumData validityRange $
                    \previousOperator newOperator ->
                      pvalidateNewShiftsNodeIsNext
                        activeOperatorsPolicyId
                        (pfromData pgoToNextEnd'newShiftsOperatorNodeRefInputIndex)
                        previousOperator
                        newOperator
                        referenceInputs
                --------------------------------------------------------------
                PRewindDueToEndOfShift
                  { prewindEnd'activeOperatorsRootRefInputIndex
                  , prewindEnd'activeOperatorsLastNodeRefInputIndex
                  , prewindEnd'registeredElementRefInputIndex
                  } ->
                    pvalidateEndOfShiftAndGetOperators datum outputDatumData validityRange $
                      \previousOperator newOperator ->
                        pand'List
                          [ pvalidateRootIsReached
                              activeOperatorsPolicyId
                              previousOperator
                              (pfromData prewindEnd'activeOperatorsRootRefInputIndex)
                              referenceInputs
                          , pvalidateNoRegisteredOperatorsCanActivate
                              registeredOperatorsPolicyId
                              (pfromData prewindEnd'registeredElementRefInputIndex)
                              referenceInputs
                              validityRange
                          , pvalidateOperatorIsTheLastActiveNode
                              activeOperatorsPolicyId
                              (pfromData prewindEnd'activeOperatorsLastNodeRefInputIndex)
                              newOperator
                              referenceInputs
                          ]
                --------------------------------------------------------------
                PGoToNextDueToSkippedOperator
                  { pgoToNextSkipped'newShiftsOperatorNodeRefInputIndex
                  , pgoToNextSkipped'skippedOperatorNodeInputIndex
                  , pgoToNextSkipped'activeOperatorsSpendRedeemerIndex
                  , pgoToNextSkipped'stateQueueRefInputIndex
                  , pgoToNextSkipped'hubOracleRefInputIndex
                  , pgoToNextSkipped'neglectedUserEvent
                  } ->
                    pvalidateActiveDatumsAndGetInfo datum outputDatumData $
                      \previousOperator newOperator previousStartTime newStartTime -> P.do
                        -- The link is discarded here (Aiken binds it to
                        -- `_inactive_operators_link`) but the call must still
                        -- run, so it is forced with `plet`.
                        _ <-
                          plet $
                            pvalidateOperatorInactivityAndGetItsLink
                              activeOperatorsAddress
                              hubOracleScriptHash
                              schedulerInputIndex
                              (pfromData pgoToNextSkipped'skippedOperatorNodeInputIndex)
                              (pfromData pgoToNextSkipped'activeOperatorsSpendRedeemerIndex)
                              (pfromData pgoToNextSkipped'stateQueueRefInputIndex)
                              (pfromData pgoToNextSkipped'hubOracleRefInputIndex)
                              (pfromData pgoToNextSkipped'neglectedUserEvent)
                              previousOperator
                              previousStartTime
                              inputs
                              referenceInputs
                              redeemers
                              validityRange
                        pand'List
                          [ pvalidateUnscheduledNewShiftsStartTime newStartTime validityRange
                          , pvalidateNewShiftsNodeIsNext
                              activeOperatorsPolicyId
                              (pfromData pgoToNextSkipped'newShiftsOperatorNodeRefInputIndex)
                              previousOperator
                              newOperator
                              referenceInputs
                          ]
                --------------------------------------------------------------
                PRewindDueToSkippedOperator
                  { prewindSkipped'activeOperatorsRootRefInputIndex
                  , prewindSkipped'skippedOperatorNodeInputIndex
                  , prewindSkipped'activeOperatorsSpendRedeemerIndex
                  , prewindSkipped'stateQueueRefInputIndex
                  , prewindSkipped'hubOracleRefInputIndex
                  , prewindSkipped'mActiveOperatorsLastNodeRefInputIndex
                  , prewindSkipped'registeredElementRefInputIndex
                  , prewindSkipped'neglectedUserEvent
                  } ->
                    pvalidateActiveDatumsAndGetInfo datum outputDatumData $
                      \previousOperator newOperator previousStartTime newStartTime -> P.do
                        rootReached <-
                          plet $
                            pvalidateRootIsReached
                              activeOperatorsPolicyId
                              previousOperator
                              (pfromData prewindSkipped'activeOperatorsRootRefInputIndex)
                              referenceInputs
                        noneCanActivate <-
                          plet $
                            pvalidateNoRegisteredOperatorsCanActivate
                              registeredOperatorsPolicyId
                              (pfromData prewindSkipped'registeredElementRefInputIndex)
                              referenceInputs
                              validityRange
                        inactiveLink <-
                          plet $
                            pvalidateOperatorInactivityAndGetItsLink
                              activeOperatorsAddress
                              hubOracleScriptHash
                              schedulerInputIndex
                              (pfromData prewindSkipped'skippedOperatorNodeInputIndex)
                              (pfromData prewindSkipped'activeOperatorsSpendRedeemerIndex)
                              (pfromData prewindSkipped'stateQueueRefInputIndex)
                              (pfromData prewindSkipped'hubOracleRefInputIndex)
                              (pfromData prewindSkipped'neglectedUserEvent)
                              previousOperator
                              previousStartTime
                              inputs
                              referenceInputs
                              redeemers
                              validityRange
                        pand'List
                          [ rootReached
                          , noneCanActivate
                          , pvalidateUnscheduledNewShiftsStartTime newStartTime validityRange
                          , pmatch prewindSkipped'mActiveOperatorsLastNodeRefInputIndex $ \case
                              PDJust lastNodeIndex ->
                                pvalidateOperatorIsTheLastActiveNode
                                  activeOperatorsPolicyId
                                  (pfromData lastNodeIndex)
                                  newOperator
                                  referenceInputs
                              -- Omitting the index is a claim that the skipped
                              -- operator is the /only/ active member, so the
                              -- turn returns to it. Both halves of that claim
                              -- are checked: the datum must not have changed
                              -- operator, and the node must be terminal.
                              PDNothing ->
                                pif
                                  (previousOperator #== newOperator)
                                  (inactiveLink #== pcon PDNothing)
                                  perror
                          ]
                --------------------------------------------------------------
                PGoToNextDueToOperatorRemoval
                  { pgoToNextRemoval'activeOperatorsMintRedeemerIndex
                  , pgoToNextRemoval'removalReason
                  } ->
                    pvalidateActiveDatumsAndGetInfo datum outputDatumData $
                      \previousOperator newOperator _previousStartTime newStartTime ->
                        pif
                          (pvalidateUnscheduledNewShiftsStartTime newStartTime validityRange)
                          ( pvalidateActiveOperatorRemoval
                              activeOperatorsPolicyId
                              (pfromData pgoToNextRemoval'activeOperatorsMintRedeemerIndex)
                              schedulerInputIndex
                              (pfromData pgoToNextRemoval'removalReason)
                              previousOperator
                              redeemers
                              $ \anchorElementKey _lastOperatorIsRemoved ->
                                -- Stepping past a removed operator lands on the
                                -- node that anchored it, so the new operator is
                                -- pinned by the removal itself rather than
                                -- chosen here.
                                anchorElementKey #== pcon (PDJust (pdata (pto (pfromData newOperator))))
                          )
                          perror
                --------------------------------------------------------------
                PRewindDueToOperatorRemoval
                  { prewindRemoval'activeOperatorsMintRedeemerIndex
                  , prewindRemoval'mActiveOperatorsLastNodeRefInputIndex
                  , prewindRemoval'removalReason
                  , prewindRemoval'registeredElementRefInputIndex
                  } -> P.do
                    -- Only the input datum is destructured here: which shape
                    -- the /output/ datum must take is decided further down, by
                    -- whether anything is left in the active set.
                    previousOperator <-
                      plet $
                        pmatch datum $ \case
                          PActiveOperator {pschedActive'operator} -> pschedActive'operator
                          PNoActiveOperators -> perror
                    pvalidateActiveOperatorRemoval
                      activeOperatorsPolicyId
                      (pfromData prewindRemoval'activeOperatorsMintRedeemerIndex)
                      schedulerInputIndex
                      (pfromData prewindRemoval'removalReason)
                      previousOperator
                      redeemers
                      $ \anchorElementKey lastOperatorIsRemoved ->
                        pand'List
                          [ -- Rewinding is only meaningful from the front of
                            -- the list, which is what a root anchor means.
                            anchorElementKey #== pcon PDNothing
                          , pvalidateNoRegisteredOperatorsCanActivate
                              registeredOperatorsPolicyId
                              (pfromData prewindRemoval'registeredElementRefInputIndex)
                              referenceInputs
                              validityRange
                          , pmatch prewindRemoval'mActiveOperatorsLastNodeRefInputIndex $ \case
                              -- Nothing left to schedule: the set is empty
                              -- after this removal, and the datum must say so.
                              PDNothing ->
                                pif
                                  lastOperatorIsRemoved
                                  ( pmatch (punsafeCoerceData @PSchedDatum outputDatumData) $ \case
                                      PNoActiveOperators -> pconstant True
                                      PActiveOperator _ _ -> perror
                                  )
                                  perror
                              PDJust lastNodeIndex ->
                                pif
                                  lastOperatorIsRemoved
                                  perror
                                  ( pmatch (punsafeCoerceData @PSchedDatum outputDatumData) $ \case
                                      PNoActiveOperators -> perror
                                      PActiveOperator
                                        { pschedActive'operator = newOperator
                                        , pschedActive'startTime = newStartTime
                                        } ->
                                          pand'List
                                            [ pvalidateUnscheduledNewShiftsStartTime
                                                (pfromData newStartTime)
                                                validityRange
                                            , pvalidateOperatorIsTheLastActiveNode
                                                activeOperatorsPolicyId
                                                (pfromData lastNodeIndex)
                                                newOperator
                                                referenceInputs
                                            ]
                                  )
                          ]
                --------------------------------------------------------------
                PAppointFirstOperator
                  { pappointFirst'newShiftsOperatorNodeRefInputIndex
                  , pappointFirst'registeredElementRefInputIndex
                  } -> P.do
                    -- Recovery from an empty schedule: only reachable when the
                    -- datum says there is no operator at all.
                    _ <-
                      plet $
                        pmatch datum $ \case
                          PNoActiveOperators -> pconstant @PUnit ()
                          PActiveOperator _ _ -> perror
                    pmatch (punsafeCoerceData @PSchedDatum outputDatumData) $ \case
                      PNoActiveOperators -> perror
                      PActiveOperator
                        { pschedActive'operator = newOperator
                        , pschedActive'startTime = newStartTime
                        } ->
                          pand'List
                            [ pvalidateUnscheduledNewShiftsStartTime
                                (pfromData newStartTime)
                                validityRange
                            , pvalidateOperatorIsTheLastActiveNode
                                activeOperatorsPolicyId
                                (pfromData pappointFirst'newShiftsOperatorNodeRefInputIndex)
                                newOperator
                                referenceInputs
                            , pvalidateNoRegisteredOperatorsCanActivate
                                registeredOperatorsPolicyId
                                (pfromData pappointFirst'registeredElementRefInputIndex)
                                referenceInputs
                                validityRange
                            ]
        )
        (pconstant ())
        perror

--------------------------------------------------------------------------------
-- Branch helpers
--------------------------------------------------------------------------------

{- | Aiken @validate_active_datums_and_get_info@.

Both datums must name an operator. The five branches that advance from one
operator to another all start here; the two that do not (@AppointFirstOperator@,
and the emptying case of @RewindDueToOperatorRemoval@) destructure by hand
because one side of theirs is @NoActiveOperators@.
-}
pvalidateActiveDatumsAndGetInfo ::
  forall (s :: S) (r :: S -> Type).
  Term s PSchedDatum ->
  Term s PData ->
  ( Term s (PAsData PPubKeyHash) ->
    Term s (PAsData PPubKeyHash) ->
    Term s PInteger ->
    Term s PInteger ->
    Term s r
  ) ->
  Term s r
pvalidateActiveDatumsAndGetInfo inputDatum outputDatumData k =
  pmatch inputDatum $ \case
    PNoActiveOperators -> perror
    PActiveOperator
      { pschedActive'operator = previousOperator
      , pschedActive'startTime = previousStartTime
      } ->
        pmatch (punsafeCoerceData @PSchedDatum outputDatumData) $ \case
          PNoActiveOperators -> perror
          PActiveOperator
            { pschedActive'operator = newOperator
            , pschedActive'startTime = newStartTime
            } ->
              k
                previousOperator
                newOperator
                (pfromData previousStartTime)
                (pfromData newStartTime)

{- | Aiken @validate_end_of_shift_and_get_operators@.

End-of-shift advancement is permissionless and unsigned. What licenses it is
purely time: the transaction's validity range must begin at or after the
previous shift ended.

The new shift then starts at that lower bound, not at the previous shift's end.
That is deliberate — a schedule left stale for many shifts catches up in one
transaction rather than needing every missed shift replayed. It also means the
lower bound is doing double duty, which is why the range is required to be
short: a transaction could otherwise name a lower bound far in the past and
schedule a shift that has already elapsed.

Aiken takes @extra_signatories@ and ignores it (@_extra_signatories@); the
parameter is dropped here rather than carried unused.
-}
pvalidateEndOfShiftAndGetOperators ::
  forall (s :: S) (r :: S -> Type).
  Term s PSchedDatum ->
  Term s PData ->
  Term s (PInterval PPosixTime) ->
  (Term s (PAsData PPubKeyHash) -> Term s (PAsData PPubKeyHash) -> Term s r) ->
  Term s r
pvalidateEndOfShiftAndGetOperators inputDatum outputDatumData validityRange k =
  pvalidateActiveDatumsAndGetInfo inputDatum outputDatumData $
    \previousOperator newOperator previousStartTime newStartTime ->
      let previousShiftEnd = pshiftEndTime # previousStartTime
          (inclusiveLowerBound, _) =
            pgetInclusiveBoundsOfAShortValidityRange validityRange
       in pif
            ( pand'List
                [ previousShiftEnd #<= inclusiveLowerBound
                , newStartTime #== inclusiveLowerBound
                ]
            )
            (k previousOperator newOperator)
            perror

{- | Aiken @validate_unscheduled_new_shifts_start_time@.

Where end-of-shift advancement starts the new shift at the validity range's
/lower/ bound, every unscheduled advancement starts it at the /upper/ bound.

The asymmetry is the point. End-of-shift is catching up on time that has already
passed, so it anchors to the earliest instant the transaction could be on chain.
The unscheduled branches are reacting to something happening now — a strike, a
removal, an appointment — so they anchor to the latest, giving the incoming
operator the whole of its shift rather than a shift already partly spent.
-}
pvalidateUnscheduledNewShiftsStartTime ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PInterval PPosixTime) ->
  Term s PBool
pvalidateUnscheduledNewShiftsStartTime newStartTime validityRange =
  let (_, inclusiveUpperBound) = pgetInclusiveBoundsOfAShortValidityRange validityRange
   in newStartTime #== inclusiveUpperBound

{- | Aiken @validate_operator_inactivity_and_get_its_link@.

The heart of the skipped-operator branches, and the only place the scheduler
looks outside the operator lists.

It does not decide that the operator was inactive. The active set's
@StrikeForInactivity@ spend redeemer decides that; this checks the two agree on
/who/ and on /which scheduler UTxO/, then checks that the accusation is
chronologically possible.

The inactivity threshold is the later of two instants:

  * the operator's shift start plus @new_shift_inactivity_grace_period@ — a
    newly appointed operator cannot be struck the moment it is appointed; and
  * either the last state-queue element's event end plus
    @max_inactivity_between_block_commitments@ (no specific event neglected), or
    a named user event's inclusion time plus @user_events_negligence_timeout@.

Taking the /later/ of the two is what makes the grace period a floor rather than
a suggestion. The transaction's validity range must then lie entirely after that
threshold, so the accusation cannot be made early.

Two structural conditions matter as much as the arithmetic. The state-queue
element must be the /last/ one (its link is @None@), or an operator could be
accused against an old block while a recent one exists. And the threshold must
fall before the operator's own shift ends, so an operator cannot be struck for
inactivity spanning a period it was never on duty for.

Returns the struck node's link, which @RewindDueToSkippedOperator@ uses to tell
a one-member set from a larger one.
-}
pvalidateOperatorInactivityAndGetItsLink ::
  forall (s :: S).
  Term s (PAsData PAddress) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PNeglectedUserEvent ->
  Term s (PAsData PPubKeyHash) ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s (PInterval PPosixTime) ->
  Term s PLink
pvalidateOperatorInactivityAndGetItsLink
  activeOperatorsAddress
  hubOracleScriptHash
  schedulerInputIndex
  activeOperatorsInputIndex
  activeOperatorsRedeemerIndex
  stateQueueRefInputIndex
  hubOracleRefInputIndex
  neglectedUserEvent
  inactiveOperator
  inactiveOperatorsShiftStartTime
  inputs
  referenceInputs
  redeemers
  validityRange = P.do
    Active.PStrikeForInactivity
      { Active.pstrike'operator
      , Active.pstrike'activeNodeLink
      , Active.pstrike'schedulerInputIndex
      } <-
      pmatch $
        pfromData
          ( punsafeCoerceRedeemer @Active.PSpendRedeemer $
              pgetSpendingRedeemerDataAt
                # pfromData activeOperatorsAddress
                # activeOperatorsInputIndex
                # activeOperatorsRedeemerIndex
                # inputs
                # redeemers
          )
    activeNodeLink <- plet pstrike'activeNodeLink

    hubDatum <-
      plet $
        Hub.pgetDatum
          # referenceInputs
          # pscriptHashOf hubOracleScriptHash
          # hubOracleRefInputIndex
    PHubOracleDatum
      { phubOracle'stateQueue
      , phubOracle'deposit
      , phubOracle'withdrawal
      , phubOracle'txOrder
      } <-
      pmatch hubDatum

    stateQueueOutput <- plet $ presolvedOutputAt referenceInputs stateQueueRefInputIndex

    checksPass <-
      plet $
        StateQueue.pfinalizeLinkedList
          ( pgetElementInfo stateQueueOutput $
              \_address _lovelace mHeaderHash payload stateQueueLink -> P.do
                -- The element must be the tip of the queue.
                _ <-
                  plet $
                    pif (stateQueueLink #== pcon PDNothing) (pconstant @PUnit ()) perror
                lastEndTime <-
                  plet $
                    pmatch mHeaderHash $ \case
                      PDNothing ->
                        pmatch (punsafeCoerceData @PConfirmedState payload) $
                          \PConfirmedState {pconfirmed'endTime} ->
                            pfromData pconfirmed'endTime
                      PDJust _ ->
                        pmatch (punsafeCoerceData @PStateQueueNode payload) $
                          \PStateQueueNode {pstateQueueNode'header} ->
                            pmatch
                              (pfromData (pdecodeHeaderView # pstateQueueNode'header))
                              $ \PHeaderV1 {pheader'endTime} -> pfromData pheader'endTime
                eventThreshold <-
                  plet $
                    pmatch neglectedUserEvent $ \case
                      PNoNeglectedUserEvent ->
                        lastEndTime + Env.pmaxInactivityBetweenBlockCommitments
                      PNeglectedDeposit {pneglected'depositRefInputIndex} ->
                        pinactivityThresholdFromUserEventInclusionTime
                          ( pmatch
                              ( Deposit.pgetDatum
                                  # referenceInputs
                                  # phubOracle'deposit
                                  # pfromData pneglected'depositRefInputIndex
                              )
                              $ \PDepositDatum {pdepositDatum'inclusionTime} ->
                                pfromData pdepositDatum'inclusionTime
                          )
                          lastEndTime
                      PNeglectedWithdrawal {pneglected'withdrawalRefInputIndex} ->
                        pinactivityThresholdFromUserEventInclusionTime
                          ( pmatch
                              ( Withdrawal.pgetDatum
                                  # referenceInputs
                                  # phubOracle'withdrawal
                                  # pfromData pneglected'withdrawalRefInputIndex
                              )
                              $ \PWithdrawalDatum {pwithdrawalDatum'inclusionTime} ->
                                pfromData pwithdrawalDatum'inclusionTime
                          )
                          lastEndTime
                      PNeglectedTxOrder {pneglected'txOrderRefInputIndex} ->
                        pinactivityThresholdFromUserEventInclusionTime
                          ( pmatch
                              ( TxOrder.pgetDatum
                                  # referenceInputs
                                  # phubOracle'txOrder
                                  # pfromData pneglected'txOrderRefInputIndex
                              )
                              $ \PTxOrderDatum {ptxOrderDatum'inclusionTime} ->
                                pfromData ptxOrderDatum'inclusionTime
                          )
                          lastEndTime
                inactivityThreshold <-
                  plet $
                    pmaxInt
                      (inactiveOperatorsShiftStartTime + Env.pnewShiftInactivityGracePeriod)
                      eventThreshold
                pif
                  ( pand'List
                      [ inactivityThreshold #< (pshiftEndTime # inactiveOperatorsShiftStartTime)
                      , pisEntirelyAfter # validityRange # inactivityThreshold
                      ]
                  )
                  (pconstant True)
                  perror
          )
          phubOracle'stateQueue

    pif
      ( pand'List
          [ pstrike'operator #== inactiveOperator
          , pfromData pstrike'schedulerInputIndex #== schedulerInputIndex
          , checksPass
          ]
      )
      activeNodeLink
      perror

{- | Aiken @inactivity_threshold_from_user_event_inclusion_time@.

@expect inclusion_time >= last_state_queue_elements_end_time@ is what stops an
operator being accused over an event that a block it already committed would
have covered.
-}
pinactivityThresholdFromUserEventInclusionTime ::
  forall (s :: S). Term s PInteger -> Term s PInteger -> Term s PInteger
pinactivityThresholdFromUserEventInclusionTime inclusionTime lastEndTime =
  pif
    (lastEndTime #<= inclusionTime)
    (inclusionTime + Env.puserEventsNegligenceTimeout)
    perror

{- | Aiken @validate_root_is_reached@.

The referenced element must be the active set's /root/, and must link to the
previous shift's operator — that is, the outgoing operator is the first node,
so there is nothing before it and the turn has to wrap.
-}
pvalidateRootIsReached ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PPubKeyHash) ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PBool
pvalidateRootIsReached activeOperatorsPolicyId previousOperator index referenceInputs =
  Active.pfinalizeLinkedList
    ( pgetElementInfo (presolvedOutputAt referenceInputs index) $
        \_address _lovelace mKey _data link ->
          pmatch mKey $ \case
            PDJust _ -> perror
            PDNothing -> link #== pcon (PDJust (pdata (pto (pfromData previousOperator))))
    )
    activeOperatorsPolicyId

{- | Aiken @validate_operator_is_the_last_active_node@.

The incoming operator must be the /last/ node of the active set. Every branch
that wraps the turn back to the front of the list uses this, because in a list
walked from the tail towards the head, the last node is where a wrap lands.
-}
pvalidateOperatorIsTheLastActiveNode ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  Term s (PAsData PPubKeyHash) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PBool
pvalidateOperatorIsTheLastActiveNode activeOperatorsPolicyId index newOperator referenceInputs =
  Active.pfinalizeLinkedList
    ( pgetElementInfo (presolvedOutputAt referenceInputs index) $
        \_address _lovelace mKey _data link ->
          pif
            (mKey #== pcon (PDJust (pdata (pto (pfromData newOperator)))))
            (link #== pcon PDNothing)
            perror
    )
    activeOperatorsPolicyId

{- | Aiken @validate_new_shifts_node_is_next@.

The ordinary step: one reference input proves both halves at once, because a
node that is keyed by the incoming operator /and/ links to the outgoing one can
only be the outgoing operator's immediate predecessor in the list. There is
exactly one such node, so the incoming operator is determined rather than
chosen.
-}
pvalidateNewShiftsNodeIsNext ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  Term s (PAsData PPubKeyHash) ->
  Term s (PAsData PPubKeyHash) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PBool
pvalidateNewShiftsNodeIsNext
  activeOperatorsPolicyId
  index
  previousOperator
  newOperator
  referenceInputs =
    Active.pfinalizeLinkedList
      ( pgetElementInfo (presolvedOutputAt referenceInputs index) $
          \_address _lovelace mKey _data link ->
            pif
              ( pand'List
                  [ mKey #== pcon (PDJust (pdata (pto (pfromData newOperator))))
                  , link #== pcon (PDJust (pdata (pto (pfromData previousOperator))))
                  ]
              )
              (pconstant True)
              perror
      )
      activeOperatorsPolicyId

{- | Aiken @validate_active_operator_removal@.

Reads the active set's mint redeemer and hands back the two facts the scheduler
needs from it: the key of the node that anchored the operator being removed
(@None@ if that anchor is the root), and whether the removed operator was the
last member.

Both are taken on trust from the active set, which is the right division of
labour — the active set is the script that actually rewrites the list and can
see the link it is removing. What this function enforces is that the two scripts
are talking about the same operator and the same scheduler UTxO. The retirement
and slashing arms differ only in where the operator's name sits in the redeemer.
-}
pvalidateActiveOperatorRemoval ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s POperatorRemovalReason ->
  Term s (PAsData PPubKeyHash) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  (Term s PLink -> Term s PBool -> Term s PBool) ->
  Term s PBool
pvalidateActiveOperatorRemoval
  activeOperatorsPolicyId
  mintRedeemerIndex
  schedulerInputIndex
  removalReason
  previousOperator
  redeemers
  k = P.do
    mintRedeemer <-
      plet $
        pfromData
          ( punsafeCoerceRedeemer @Active.PMintRedeemer $
              pgetRedeemerAt
                # redeemers
                # pdata (pcon (PMinting activeOperatorsPolicyId))
                # mintRedeemerIndex
          )
    -- Each arm names the operator from a different place in the redeemer, then
    -- both continue identically; the shared tail is factored out so the two
    -- arms cannot drift apart.
    let withSync namedOperator sync =
          pmatch (pfromData sync) $ \case
            PShowOperatorIsInactive _ -> perror
            PShowSchedulerIsAdvancing
              { pshowAdvancing'schedulerInputIndex
              , pshowAdvancing'removingOperatorsAnchorElementKey
              , pshowAdvancing'removingOperatorIsTheLastMember
              } ->
                pif
                  ( pand'List
                      [ previousOperator #== namedOperator
                      , pfromData pshowAdvancing'schedulerInputIndex #== schedulerInputIndex
                      ]
                  )
                  ( k
                      pshowAdvancing'removingOperatorsAnchorElementKey
                      (pfromData pshowAdvancing'removingOperatorIsTheLastMember)
                  )
                  perror
    pmatch removalReason $ \case
      POperatorRetirement ->
        pmatch mintRedeemer $ \case
          Active.PRetireOperator
            { Active.pactiveRetire'activeOperatorKey
            , Active.pactiveRetire'operatorRemovalSchedulerSync
            } ->
              withSync pactiveRetire'activeOperatorKey pactiveRetire'operatorRemovalSchedulerSync
          _ -> perror
      POperatorSlashing ->
        pmatch mintRedeemer $ \case
          Active.PSlashOperator
            { Active.pactiveSlash'slashingArguments
            , Active.pactiveSlash'operatorRemovalSchedulerSync
            } ->
              pmatch (pfromData pactiveSlash'slashingArguments) $
                \PSlashingArguments {pslashArgs'slashedOperator} ->
                  withSync pslashArgs'slashedOperator pactiveSlash'operatorRemovalSchedulerSync
          _ -> perror

{- | Aiken @validate_no_registered_operators_can_activate@.

Every branch that wraps the turn back to the front of the active set must show
this, and the reason is fairness of ordering: a registered operator whose
activation time has arrived is entitled to join before the rota starts over.

One reference input suffices because the registered set is keyed by activation
time and inserted descending, so its /last/ element carries the earliest
activation time. Showing that element is terminal, and that its activation time
has not yet arrived, proves the same of every other. The root is accepted with
no time check because a root-only list has no registered operators at all.
-}
pvalidateNoRegisteredOperatorsCanActivate ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PInterval PPosixTime) ->
  Term s PBool
pvalidateNoRegisteredOperatorsCanActivate
  registeredOperatorsPolicyId
  index
  referenceInputs
  validityRange =
    Registered.pfinalizeLinkedList
      ( pgetElementInfo (presolvedOutputAt referenceInputs index) $
          \_address _lovelace mKey _data link -> P.do
            notYetActivatable <-
              plet $
                pmatch mKey $ \case
                  PDNothing -> pconstant True
                  PDJust nodeKey ->
                    pisEntirelyBefore
                      # validityRange
                      # (Registered.pnodeKeyToActivationTime # pfromData nodeKey)
            pif
              (notYetActivatable #&& (link #== pcon PDNothing))
              (pconstant True)
              perror
      )
      registeredOperatorsPolicyId
