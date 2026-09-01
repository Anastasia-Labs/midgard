{- |
Module      : Midgard.Validators.ActiveOperators
Description : Plutarch port of
              @validators/operator-directory/active-operators.ak@.

The active operators set: the operators currently eligible to commit blocks. It
is the only operator list whose nodes are mutated in place — the spend path
updates a node's bond unlock time and inactivity strike count without touching
the list structure — and the only one that has to stay synchronised with the
scheduler, which points at whichever active operator is currently appointed.
-}
module Midgard.Validators.ActiveOperators (
  activeOperatorsSpendValidator,
  activeOperatorsMintValidator,
) where

import Data.Kind (Type)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Interval (PInterval)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PMintValue,
  PPubKeyHash,
  PRedeemer,
  PScriptContext (..),
  PScriptHash (..),
  PScriptInfo (..),
  PScriptPurpose (..),
  PTxInInfo,
  PTxInfo (..),
  PTxOut,
  PTxOutRef,
 )
import Plutarch.LedgerApi.V3 qualified as LedgerV3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import LinkedList (premove, pspendForAddingOrRemovingAnElement, pspendForUpdatingElementsData)
import LinkedList.Types (PLink, PNodeKey)
import Midgard.Common.Types (PPosixTime)
import Midgard.Common.Utils (
  pconstrOf,
  pgetInclusiveUpperBoundOfInterval,
  pgetInlineDatumAndSpendingRedeemerDataAt,
  pgetRedeemerAt,
  pgetSpendingRedeemerDataAt,
  phasSigned,
 )
import Midgard.Env qualified as Env
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.LedgerState qualified as LedgerState
import Midgard.OperatorDirectory qualified as Dir
import Midgard.OperatorDirectory.ActiveOperators (
  PMintRedeemer (..),
  PNodeData (..),
  POperatorRemovalSchedulerSync (..),
  PSpendRedeemer (..),
  pfinalizeLinkedList,
 )
import Midgard.OperatorDirectory.ActiveOperators qualified as Active
import Midgard.OperatorDirectory.RegisteredOperators qualified as Registered
import Midgard.OperatorDirectory.RetiredOperators qualified as Retired
import Midgard.Scheduler qualified as Scheduler
import Midgard.Settlement qualified as Settlement
import Midgard.StateQueue qualified as StateQueue

{- | A policy id reinterpreted as a script hash — the same 28 bytes, which Aiken
conflates as @ByteArray@.
-}
pscriptHashOf ::
  forall (s :: S). Term s (PAsData PCurrencySymbol) -> Term s (PAsData PScriptHash)
pscriptHashOf cs = pdata (pcon (PScriptHash (pto (pfromData cs))))

-- | Reinterpret another script's redeemer as a known type.
punsafeCoerceRedeemer ::
  forall (a :: S -> Type) (s :: S). Term s (PAsData PRedeemer) -> Term s (PAsData a)
punsafeCoerceRedeemer r = punsafeCoerce (pto (pfromData r))

-- | Reinterpret raw @Data@ as a known type.
punsafeCoerceData ::
  forall (a :: S -> Type) (s :: S). (PIsData a) => Term s PData -> Term s a
punsafeCoerceData d = pfromData (punsafeCoerce @(PAsData a) d)

{- | Aiken @active_operators.max_bond_unlock_time@.

@
when old_bond_unlock_time is {
  None -> new_bond_unlock_time
  Some(old) -> max(new_bond_unlock_time, old)
}
@

The hold is monotonic: an operator that commits a block and then attaches an
older resolution claim must not shorten its own bond lock.
-}
pmaxBondUnlockTime ::
  forall (s :: S).
  Term s PInteger ->
  Term s (PMaybeData PPosixTime) ->
  Term s PInteger
pmaxBondUnlockTime newBondUnlockTime oldBondUnlockTime =
  pmatch oldBondUnlockTime $ \case
    PDNothing -> newBondUnlockTime
    PDJust old ->
      pif (newBondUnlockTime #> pfromData old) newBondUnlockTime (pfromData old)

-- | @NodeData@ as it appears on the wire, for comparison against an output.
pnodeDataOf ::
  forall (s :: S).
  Term s (PMaybeData PPosixTime) ->
  Term s (PAsData PInteger) ->
  Term s PData
pnodeDataOf bondUnlockTime strikes =
  pforgetData (pdata (pcon (PNodeData bondUnlockTime strikes)))

{- | Aiken @active_operators.validate_scheduler_syncs_with_operator_removal@.

An active operator cannot simply vanish: the scheduler may currently have it
appointed, and a dangling appointment would stall block production. The remover
discharges that one of two ways.

@ShowOperatorIsInactive@ references the scheduler and shows it points somewhere
else (or nowhere). @ShowSchedulerIsAdvancing@ spends the scheduler in the same
transaction with a removal-driven advancing approach — and then has to prove the
two scripts agree, since each is relying on the other's redeemer: the anchor key
and the last-member flag are validated here so the /scheduler/ can trust them,
while the scheduler's own input index is cross-checked against its redeemer so a
caller cannot point at a different scheduler input than the one being validated.
-}
pvalidateSchedulerSyncsWithOperatorRemoval ::
  forall (s :: S).
  Term s POperatorRemovalSchedulerSync ->
  Term s (PAsData PPubKeyHash) ->
  Term s (PMaybeData PNodeKey) ->
  Term s PLink ->
  Term s PHubOracleDatum ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s PBool
pvalidateSchedulerSyncsWithOperatorRemoval
  schedulerSync
  subjectOperator
  subjectOperatorsAnchorKey
  subjectOperatorsLink
  hubDatum
  inputs
  referenceInputs
  redeemers = P.do
    PHubOracleDatum {phubOracle'scheduler, phubOracle'schedulerAddr} <- pmatch hubDatum
    pmatch schedulerSync $ \case
      PShowOperatorIsInactive {pshowInactive'schedulerRefInputIndex} ->
        pmatch
          ( Scheduler.pgetDatum
              # referenceInputs
              # phubOracle'scheduler
              # pfromData pshowInactive'schedulerRefInputIndex
          )
          $ \case
            Scheduler.PNoActiveOperators -> pconstant True
            Scheduler.PActiveOperator {pschedActive'operator} ->
              pnot # (pschedActive'operator #== subjectOperator)
      PShowSchedulerIsAdvancing
        { pshowAdvancing'schedulerInputIndex
        , pshowAdvancing'schedulerRedeemerIndex
        , pshowAdvancing'removingOperatorsAnchorElementKey
        , pshowAdvancing'removingOperatorIsTheLastMember
        } -> P.do
          let (schedulerDatumData, schedulerRedeemerData) =
                pgetInlineDatumAndSpendingRedeemerDataAt
                  (pfromData phubOracle'schedulerAddr)
                  (pfromData pshowAdvancing'schedulerInputIndex)
                  (pfromData pshowAdvancing'schedulerRedeemerIndex)
                  inputs
                  redeemers
          schedulerRedeemer <-
            plet $
              pfromData (punsafeCoerceRedeemer @Scheduler.PSpendRedeemer schedulerRedeemerData)
          Scheduler.PSpendRedeemer
            { pschedSpend'schedulerInputIndex
            , pschedSpend'advancingApproach
            } <-
            pmatch schedulerRedeemer
          pand'List
            [ pshowAdvancing'removingOperatorsAnchorElementKey #== subjectOperatorsAnchorKey
            , pmatch (punsafeCoerceData @Scheduler.PSchedDatum schedulerDatumData) $ \case
                Scheduler.PActiveOperator {pschedActive'operator} ->
                  pschedActive'operator #== subjectOperator
                _ -> perror
            , pshowAdvancing'schedulerInputIndex #== pschedSpend'schedulerInputIndex
            , -- @GoToNextDueToOperatorRemoval@ and @RewindDueToOperatorRemoval@
              -- are tags 4 and 5; no field is read, so the tag alone decides.
              -- See 'pconstrOf' for why this is not a two-armed 'pmatch'.
              (fst (pconstrOf pschedSpend'advancingApproach) #== 4)
                #|| (fst (pconstrOf pschedSpend'advancingApproach) #== 5)
            , -- The scheduler needs to know whether the operator it is being
              -- advanced past was the last member, and takes that on trust from
              -- this redeemer; the link is what makes it true.
              pmatch subjectOperatorsLink $ \case
                PDNothing -> pfromData pshowAdvancing'removingOperatorIsTheLastMember
                PDJust _ -> pnot # pfromData pshowAdvancing'removingOperatorIsTheLastMember
            ]

{- | Aiken @active_operators.spend_for_updating_bond_unlock_time@.

Shared by the two bond-hold branches: spend and reproduce one node in place,
require its key to be the operator named in the redeemer, and hand the caller
the fresh unlock time (validity-range upper bound plus the block maturity
duration), the old one, the reproduced node's data, the strike count and the hub
datum.

The caller decides what the reproduced data must be, because the two branches
derive the hold from different sources — a committed block against the maturity
duration, or a settlement's resolution time.
-}
pspendForUpdatingBondUnlockTime ::
  forall (s :: S).
  Term s (PAsData PPubKeyHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PMintValue ->
  -- | The ledger's @PPosixTime@ newtype, not Midgard's @PInteger@ alias.
  Term s (PInterval LedgerV3.PPosixTime) ->
  ( Term s PInteger ->
    Term s (PMaybeData PPosixTime) ->
    Term s PData ->
    Term s (PAsData PInteger) ->
    Term s PHubOracleDatum ->
    Term s PBool
  ) ->
  Term s PBool
pspendForUpdatingBondUnlockTime
  activeOperator
  activeOperatorsMintScriptHash
  hubOracleScriptHash
  activeNodeInputIndex
  activeNodeOutputIndex
  hubOracleRefInputIndex
  ownOutRef
  inputs
  outputs
  referenceInputs
  mint
  validityRange
  callback =
    pfinalizeLinkedList
      ( pspendForUpdatingElementsData
          activeNodeInputIndex
          activeNodeOutputIndex
          ownOutRef
          inputs
          outputs
          mint
          ( \_activeNodeAddress lovelaceChange mOperator activeNodeInputData activeNodeOutputData _activeNodeLink -> P.do
              PNodeData {pactiveNode'bondUnlockTime, pactiveNode'inactivityStrikes} <-
                pmatch (punsafeCoerceData @PNodeData activeNodeInputData)
              validTo <- plet $ pgetInclusiveUpperBoundOfInterval # validityRange
              pand'List
                [ lovelaceChange #>= 0
                , -- @Some(active_operator) == m_operator@: a root element has no
                  -- key and so can never satisfy this.
                  pmatch mOperator $ \case
                    PDNothing -> pconstant False
                    PDJust key -> pfromData key #== pto (pfromData activeOperator)
                , callback
                    (validTo + LedgerState.pblockMaturityDurationV1)
                    pactiveNode'bondUnlockTime
                    activeNodeOutputData
                    pactiveNode'inactivityStrikes
                    ( Hub.pgetDatum
                        # referenceInputs
                        # pscriptHashOf hubOracleScriptHash
                        # hubOracleRefInputIndex
                    )
                ]
          )
      )
      activeOperatorsMintScriptHash

{- | Aiken @validators/operator-directory/active-operators.ak@ — @spend@.

Four branches:

  * @ListStateTransition@ — the plain gate the other two sets use throughout:
    permitted whenever the list policy mints or burns.
  * @UpdateBondHoldNewState@ — the operator commits a block, so its bond is held
    until that block matures. The state queue validates the operator's signature;
    this only checks the two redeemers name the same operator.
  * @UpdateBondHoldNewSettlement@ — the operator attaches a resolution claim, so
    its bond is held until the claim resolves. The redeemer's @resolution_time@
    is recomputed here rather than trusted, because settlement relies on it.
  * @StrikeForInactivity@ — the scheduler skipped this operator, so its strike
    count goes up by one. The ceiling at @max_inactivity_strikes@ is what stops
    an attacker striking an operator forever to keep its UTxO unspendable and
    block its retirement.
-}
activeOperatorsSpendValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
activeOperatorsSpendValidator =
  plam $ \activeOperatorsMintScriptHash hubOracleScriptHash ctx -> P.do
    PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
      pmatch ctx
    ownOutRef <-
      plet $ pmatch pscriptContext'scriptInfo $ \case
        PSpendingScript outRef _ -> outRef
        _ -> perror
    PTxInfo
      { ptxInfo'inputs
      , ptxInfo'outputs
      , ptxInfo'referenceInputs
      , ptxInfo'mint
      , ptxInfo'redeemers
      , ptxInfo'validRange
      } <-
      pmatch pscriptContext'txInfo
    inputs <- plet $ pfromData ptxInfo'inputs
    outputs <- plet $ pfromData ptxInfo'outputs
    referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
    mint <- plet $ pfromData ptxInfo'mint
    redeemerList <- plet $ pto (pto (pfromData ptxInfo'redeemers))

    redeemer <-
      plet $
        pfromData (punsafeCoerce @(PAsData PSpendRedeemer) (pto pscriptContext'redeemer))

    pif
      ( pmatch redeemer $ \case
          PListStateTransition ->
            pspendForAddingOrRemovingAnElement # activeOperatorsMintScriptHash # mint
          PUpdateBondHoldNewState
            { pupdateState'activeOperator
            , pupdateState'activeNodeInputIndex
            , pupdateState'activeNodeOutputIndex
            , pupdateState'hubOracleRefInputIndex
            , pupdateState'stateQueueRedeemerIndex
            } ->
              pspendForUpdatingBondUnlockTime
                pupdateState'activeOperator
                activeOperatorsMintScriptHash
                hubOracleScriptHash
                (pfromData pupdateState'activeNodeInputIndex)
                (pfromData pupdateState'activeNodeOutputIndex)
                (pfromData pupdateState'hubOracleRefInputIndex)
                ownOutRef
                inputs
                outputs
                referenceInputs
                mint
                ptxInfo'validRange
                ( \newBondUnlockTime oldBondUnlockTime outputData strikes hubDatum -> P.do
                    PHubOracleDatum {phubOracle'stateQueue} <- pmatch hubDatum
                    pand'List
                      [ outputData
                          #== pnodeDataOf
                            ( pcon
                                ( PDJust
                                    ( pdata
                                        (pmaxBondUnlockTime newBondUnlockTime oldBondUnlockTime)
                                    )
                                )
                            )
                            strikes
                      , pmatch
                          ( pfromData
                              ( punsafeCoerceRedeemer @StateQueue.PMintRedeemer
                                  ( pgetRedeemerAt
                                      # redeemerList
                                      # pdata (pcon (PMinting phubOracle'stateQueue))
                                      # pfromData pupdateState'stateQueueRedeemerIndex
                                  )
                              )
                          )
                          $ \case
                            StateQueue.PCommitBlockHeader {psqCommit'operator} ->
                              psqCommit'operator #== pupdateState'activeOperator
                            _ -> perror
                      ]
                )
          PUpdateBondHoldNewSettlement
            { pupdateSettlement'activeOperator
            , pupdateSettlement'activeNodeInputIndex
            , pupdateSettlement'activeNodeOutputIndex
            , pupdateSettlement'hubOracleRefInputIndex
            , pupdateSettlement'settlementInputIndex
            , pupdateSettlement'settlementRedeemerIndex
            , pupdateSettlement'resolutionTime
            } ->
              pspendForUpdatingBondUnlockTime
                pupdateSettlement'activeOperator
                activeOperatorsMintScriptHash
                hubOracleScriptHash
                (pfromData pupdateSettlement'activeNodeInputIndex)
                (pfromData pupdateSettlement'activeNodeOutputIndex)
                (pfromData pupdateSettlement'hubOracleRefInputIndex)
                ownOutRef
                inputs
                outputs
                referenceInputs
                mint
                ptxInfo'validRange
                ( \computedResolutionTime oldBondUnlockTime outputData strikes hubDatum -> P.do
                    PHubOracleDatum {phubOracle'settlementAddr} <- pmatch hubDatum
                    resolutionTime <- plet $ pfromData pupdateSettlement'resolutionTime
                    pand'List
                      [ -- Settlement relies on this value, so it is recomputed
                        -- here rather than taken from the redeemer.
                        computedResolutionTime #== resolutionTime
                      , outputData
                          #== pnodeDataOf
                            ( pcon
                                ( PDJust
                                    ( pdata
                                        (pmaxBondUnlockTime resolutionTime oldBondUnlockTime)
                                    )
                                )
                            )
                            strikes
                      , pmatch
                          ( pfromData
                              ( punsafeCoerceRedeemer @Settlement.PSpendRedeemer
                                  ( pgetSpendingRedeemerDataAt
                                      # pfromData phubOracle'settlementAddr
                                      # pfromData pupdateSettlement'settlementInputIndex
                                      # pfromData pupdateSettlement'settlementRedeemerIndex
                                      # inputs
                                      # redeemerList
                                  )
                              )
                          )
                          $ \case
                            Settlement.PAttachResolutionClaim {pstlAttach'operator} ->
                              pstlAttach'operator #== pupdateSettlement'activeOperator
                            _ -> perror
                      ]
                )
          PStrikeForInactivity
            { pstrike'activeNodeInputIndex
            , pstrike'activeNodeOutputIndex
            , pstrike'operator
            , pstrike'activeNodeLink
            , pstrike'schedulerInputIndex
            , pstrike'schedulerRedeemerIndex
            , pstrike'hubOracleRefInputIndex
            } -> P.do
              hubDatum <-
                plet $
                  Hub.pgetDatum
                    # referenceInputs
                    # pscriptHashOf hubOracleScriptHash
                    # pfromData pstrike'hubOracleRefInputIndex
              PHubOracleDatum {phubOracle'schedulerAddr} <- pmatch hubDatum
              schedulerRedeemer <-
                plet $
                  pfromData
                    ( punsafeCoerceRedeemer @Scheduler.PSpendRedeemer
                        ( pgetSpendingRedeemerDataAt
                            # pfromData phubOracle'schedulerAddr
                            # pfromData pstrike'schedulerInputIndex
                            # pfromData pstrike'schedulerRedeemerIndex
                            # inputs
                            # redeemerList
                        )
                    )
              Scheduler.PSpendRedeemer
                { pschedSpend'schedulerInputIndex
                , pschedSpend'advancingApproach
                } <-
                pmatch schedulerRedeemer
              pand'List
                [ {- The scheduler must be skipping *this* node, in *this*
                  scheduler input. Aiken writes the two skipped-operator
                  variants as one or-pattern over a shared body:

                  @
                  scheduler.RewindDueToSkippedOperator { skipped_operator_node_input_index, .. } |
                  scheduler.GoToNextDueToSkippedOperator { skipped_operator_node_input_index, .. } ->
                    and { scheduler_input_index == ..., active_node_input_index == ... }
                  @

                  Both are tag 2 and 3, and in both the skipped index is field 1,
                  so the tag and the field are read directly — see 'pconstrOf'
                  for why a two-armed 'pmatch' must not be used here. The field
                  read sits inside 'pif' rather than 'pand'List' because the
                  latter is strict, and the other approaches have fewer fields.
                  -}
                  P.do
                    (approachTag, approachFields) <-
                      \f -> f (pconstrOf pschedSpend'advancingApproach)
                    pif
                      (approachTag #== 2 #|| approachTag #== 3)
                      ( pand'List
                          [ pstrike'schedulerInputIndex #== pschedSpend'schedulerInputIndex
                          , pforgetData pstrike'activeNodeInputIndex
                              #== (phead #$ ptail # approachFields)
                          ]
                      )
                      (pconstant False)
                , pfinalizeLinkedList
                    ( pspendForUpdatingElementsData
                        (pfromData pstrike'activeNodeInputIndex)
                        (pfromData pstrike'activeNodeOutputIndex)
                        ownOutRef
                        inputs
                        outputs
                        mint
                        ( \_addr lovelaceChange mOperator inputData outputData correctLink -> P.do
                            PNodeData
                              { pactiveNode'bondUnlockTime
                              , pactiveNode'inactivityStrikes
                              } <-
                              pmatch (punsafeCoerceData @PNodeData inputData)
                            newStrikes <-
                              plet $ pfromData pactiveNode'inactivityStrikes + 1
                            pand'List
                              [ lovelaceChange #>= 0
                              , pmatch mOperator $ \case
                                  PDNothing -> pconstant False
                                  PDJust key -> pfromData key #== pto (pfromData pstrike'operator)
                              ,                                 -- The scheduler reads this link out of the
                                -- redeemer, so it is validated here.
                                pstrike'activeNodeLink #== correctLink
                              ,                                   outputData
                                    #== pnodeDataOf pactiveNode'bondUnlockTime (pdata newStrikes)
                              , newStrikes #<= Env.pmaxInactivityStrikes
                              ]
                        )
                    )
                    activeOperatorsMintScriptHash
                ]
      )
      (pconstant ())
      perror

{- | Aiken @validators/operator-directory/active-operators.ak@ — @mint@.

Five branches:

  * @Init@ / @Deinit@ — create or tear down the set's root, gated on the hub
    oracle NFT.
  * @ActivateOperator@ — insert a node transferred from the registered set. The
    new node must carry no bond hold and no strikes, and the redeemer's
    @active_operators_set_was_empty@ flag is validated against the list's actual
    shape, because the registered set relies on it to allow immediate activation.
  * @RetireOperator@ — remove a node and let the retired set add it. The removed
    node's @bond_unlock_time@ must equal the one in the retired set's redeemer,
    which is how the hold survives the transfer. The penalty flag is not taken on
    trust either way: penalising requires the strike count to have reached the
    maximum and the penalty to be paid as fee, while not penalising requires the
    count to be below it and the operator's own signature.
  * @SlashOperator@ — remove a node under the shared slashing logic.

Every removal path also has to discharge the scheduler synchronisation.
-}
activeOperatorsMintValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
activeOperatorsMintValidator =
  plam $ \hubOracleScriptHash registeredOperatorsPolicyId retiredOperatorsPolicyId ctx -> P.do
    PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
      pmatch ctx
    ownPolicyId <-
      plet $ pmatch pscriptContext'scriptInfo $ \case
        PMintingScript cs -> cs
        _ -> perror
    PTxInfo
      { ptxInfo'inputs
      , ptxInfo'outputs
      , ptxInfo'referenceInputs
      , ptxInfo'mint
      , ptxInfo'redeemers
      , ptxInfo'signatories
      , ptxInfo'fee
      } <-
      pmatch pscriptContext'txInfo
    inputs <- plet $ pfromData ptxInfo'inputs
    outputs <- plet $ pfromData ptxInfo'outputs
    referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
    mint <- plet $ pfromData ptxInfo'mint
    redeemerList <- plet $ pto (pto (pfromData ptxInfo'redeemers))

    redeemer <-
      plet $
        pfromData (punsafeCoerce @(PAsData PMintRedeemer) (pto pscriptContext'redeemer))

    pif
      ( pmatch redeemer $ \case
          PInit {pactiveInit'outputIndex} ->
            Dir.pinit
              hubOracleScriptHash
              ownPolicyId
              Active.prootAssetName
              (pfromData pactiveInit'outputIndex)
              outputs
              mint
          PDeinit ->
            Dir.pdeinit
              hubOracleScriptHash
              ownPolicyId
              Active.prootAssetName
              inputs
              mint
          PActivateOperator
            { pactivate'newActiveOperatorKey
            , pactivate'anchorElementOutputIndex
            , pactivate'insertedNodeOutputIndex
            , pactivate'registeredOperatorsRedeemerIndex
            , pactivate'activeOperatorsSetWasEmpty
            } ->
              pfinalizeLinkedList
                ( Dir.pvalidateTransferredOperatorInsertion
                    pactivate'newActiveOperatorKey
                    registeredOperatorsPolicyId
                    (pfromData pactivate'registeredOperatorsRedeemerIndex)
                    pregisteredActivateOperatorAndPenalty
                    (pfromData pactivate'anchorElementOutputIndex)
                    (pfromData pactivate'insertedNodeOutputIndex)
                    inputs
                    outputs
                    mint
                    redeemerList
                    ( \insertedNodeData mAnchorKey insertedLink ->
                        pand'List
                          [ -- A freshly activated operator carries no bond hold
                            -- and a clean strike count.
                            insertedNodeData
                              #== pnodeDataOf (pcon PDNothing) (pdata 0)
                          , -- The registered set trusts this flag to allow
                            -- immediate activation, so it is proven here: the
                            -- node is both first and last in the list.
                            pfromData pactivate'activeOperatorsSetWasEmpty
                              #== pand'List
                                [ pmatch mAnchorKey $ \case
                                    PDNothing -> pconstant True
                                    PDJust _ -> pconstant False
                                , pmatch insertedLink $ \case
                                    PDNothing -> pconstant True
                                    PDJust _ -> pconstant False
                                ]
                          ]
                    )
                )
                ownPolicyId
          PRetireOperator
            { pactiveRetire'activeOperatorKey
            , pactiveRetire'hubOracleRefInputIndex
            , pactiveRetire'anchorElementInputOutref
            , pactiveRetire'anchorElementOutputIndex
            , pactiveRetire'retiredOperatorsRedeemerIndex
            , pactiveRetire'penalizeForInactivity
            , pactiveRetire'operatorRemovalSchedulerSync
            } -> P.do
              retiredRedeemer <-
                plet $
                  pfromData
                    ( punsafeCoerceRedeemer @Retired.PMintRedeemer
                        ( pgetRedeemerAt
                            # redeemerList
                            # pdata (pcon (PMinting retiredOperatorsPolicyId))
                            # pfromData pactiveRetire'retiredOperatorsRedeemerIndex
                        )
                    )
              retiredOperatorKey <-
                plet $ pmatch retiredRedeemer $ \case
                  Retired.PRetireOperator {pretire'newRetiredOperatorKey} ->
                    pretire'newRetiredOperatorKey
                  _ -> perror
              retiredBondUnlockTime <-
                plet $ pmatch retiredRedeemer $ \case
                  Retired.PRetireOperator {pretire'bondUnlockTime} -> pretire'bondUnlockTime
                  _ -> perror
              hubDatum <-
                plet $
                  Hub.pgetDatum
                    # referenceInputs
                    # pscriptHashOf hubOracleScriptHash
                    # pfromData pactiveRetire'hubOracleRefInputIndex
              pand'List
                [ pactiveRetire'activeOperatorKey #== retiredOperatorKey
                , pfinalizeLinkedList
                    ( premove
                        (pfromData pactiveRetire'anchorElementInputOutref)
                        ( pfromData
                            (pelemAt # pfromData pactiveRetire'anchorElementOutputIndex # outputs)
                        )
                        inputs
                        mint
                        ( \_anchorInput anchorLovelaceChange mAnchorKey _anchorData _removedInput _removedLovelace removedKey removedData removedLink -> P.do
                            PNodeData
                              { pactiveNode'bondUnlockTime
                              , pactiveNode'inactivityStrikes
                              } <-
                              pmatch (punsafeCoerceData @PNodeData removedData)
                            strikes <- plet $ pfromData pactiveNode'inactivityStrikes
                            pand'List
                              [ anchorLovelaceChange #>= 0
                              , removedKey #== pto (pfromData pactiveRetire'activeOperatorKey)
                              , pvalidateSchedulerSyncsWithOperatorRemoval
                                  (pfromData pactiveRetire'operatorRemovalSchedulerSync)
                                  pactiveRetire'activeOperatorKey
                                  mAnchorKey
                                  removedLink
                                  hubDatum
                                  inputs
                                  referenceInputs
                                  redeemerList
                              , -- The retired set mints against this unlock
                                -- time, so the two must agree.
                                pactiveNode'bondUnlockTime #== retiredBondUnlockTime
                              , pif
                                  (pfromData pactiveRetire'penalizeForInactivity)
                                  -- Forced retirement: enough strikes, and the
                                  -- penalty paid as fee.
                                  ( pand'List
                                      [ strikes #>= Env.pmaxInactivityStrikes
                                      , pto (pfromData ptxInfo'fee)
                                          #>= Env.pinactivitySlashingPenalty
                                      ]
                                  )
                                  -- Voluntary retirement: below the strike
                                  -- ceiling, and the operator consents.
                                  ( pand'List
                                      [ strikes #< Env.pmaxInactivityStrikes
                                      , phasSigned
                                          # pactiveRetire'activeOperatorKey
                                          # pfromData ptxInfo'signatories
                                      ]
                                  )
                              ]
                        )
                    )
                    ownPolicyId
                ]
          PSlashOperator
            { pactiveSlash'slashingArguments
            , pactiveSlash'operatorRemovalSchedulerSync
            } ->
              pfinalizeLinkedList
                ( Dir.pslashFraudulentOperatorAndGetInfo
                    hubOracleScriptHash
                    (pfromData pactiveSlash'slashingArguments)
                    inputs
                    outputs
                    referenceInputs
                    mint
                    (pto (pfromData ptxInfo'fee))
                    redeemerList
                    ( \slashedOperator mAnchorKey slashedLink hubDatum ->
                        pvalidateSchedulerSyncsWithOperatorRemoval
                          (pfromData pactiveSlash'operatorRemovalSchedulerSync)
                          slashedOperator
                          mAnchorKey
                          slashedLink
                          hubDatum
                          inputs
                          referenceInputs
                          redeemerList
                    )
                )
                ownPolicyId
      )
      (pconstant ())
      perror

{- | Aiken's @registered_operators.ActivateOperator@ reader.

@
fn(registered_operators_redeemer_data) -> (VerificationKeyHash, Bool) {
  expect registered_operators.ActivateOperator { activating_operator, .. } = ...
  (activating_operator, False)
}
@

The penalty flag is a constant @False@ here: an operator moving from registered
to active has never been active, so it can have no inactivity penalty. The
retired set's equivalent reader does read a real flag out of this set's redeemer.
-}
pregisteredActivateOperatorAndPenalty ::
  forall (s :: S).
  Term s (PAsData PRedeemer) ->
  (Term s (PAsData PPubKeyHash), Term s PBool)
pregisteredActivateOperatorAndPenalty rdmr =
  ( pmatch (pfromData (punsafeCoerceRedeemer @Registered.PMintRedeemer rdmr)) $ \case
      Registered.PActivateOperator {pactivate'activatingOperator} ->
        pactivate'activatingOperator
      _ -> perror
  , pconstant False
  )
