{- |
Module      : Midgard.Validators.StateQueue
Description : Plutarch port of @validators/state-queue.ak@.

The state queue is Midgard's chain of blocks: a linked list whose root is the
confirmed state and whose nodes are committed block headers, each keyed by its
own hash. Every other L1 script reads it; this is the only script that writes it.

Its seven mint branches are the whole lifecycle of a block. @InitV1@ and
@Deinit@ create and destroy the queue alongside the hub oracle.
@CommitBlockHeader@ appends a block. The three removal branches handle proven
fraud and the attestation/availability timeouts. @MergeToConfirmedStateV1@
retires the oldest block into the confirmed state after it matures.

Three things are worth knowing before reading it.

/A block's key is its own hash./ The node's NFT asset name is
@blake2b_224(serialise(header))@, minted by this policy, so the key cannot
disagree with the header it names. Everything downstream that identifies a block
by hash — fraud proofs, settlements, the merge — relies on that.

/Appending has two routes, and they are not symmetric./ A block appended after
another block carries over that block's fields directly. A block appended after
the /confirmed state/ must go through
@confirmed_state_next_header_protocol_version_v1@, which authenticates the state
and answers with the version the new header must carry. That indirection is what
keeps the genesis sentinel's protocol version zero from ever reaching a block.

/Removal and merge both walk the list, in opposite directions./ Removal takes a
node out from behind an anchor; merge folds the oldest node into the root. Both
go through the linked-list library, so the structural bookkeeping is shared and
only the payload conditions live here.
-}
module Midgard.Validators.StateQueue (
    stateQueueSpendValidator,
    stateQueueMintValidator,
    pcommitBlockHeaderOutputIsValidV1,
    pcommitBlockHeaderOperatorIsLegitimateV1,
    pcommitBlockHeaderCarriesPreviousBlockV1,
    pcommitBlockHeaderCarriesConfirmedStateV1,
    pstateQueueHeadAllowsAppendV1,
    pauthenticatedQueueHeadForAppendV1,
    punattestedBlockTimeoutElapsedV1,
    ppruneTimedOutBlockDescendantV1,
    premoveUnattestedHeadAfterTimeoutV1,
    ppruneUnavailableBlockDescendantV1,
    premoveUnavailableHeadV1,
    pfraudProverRewardOutputIsExactV1,
    pnoOutputPaysFraudProverRewardV1,
    pfraudProverRewardRoutingIsExactV1,
    prouteFraudProverRewardV1,
    premoveFraudulentBlocksLinkV1,
    premoveLastFraudulentBlockV1,
    pmergeCommitmentsMatchHeader,
    pmergeSettlementBindingMatchesHeader,
    pmergeSettlementIdAtRoute,
    pheaderCarriesL2Material,
    pavailabilityStatusUpdateIsAuthorizedV1,
) where

import Data.Kind (Type)
import Plutarch.Builtin.Crypto (pblake2b_224)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Interval (PInterval)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PAddress (..), PCredential (..), PCurrencySymbol, PMintValue, POutputDatum (..), PPosixTime, PPubKeyHash, PRedeemer, PScriptContext (..), PScriptHash, PScriptInfo (..), PScriptPurpose (..), PTxInInfo (..), PTxInfo (..), PTxOut (..), PTxOutRef)
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import LinkedList (
    pappendUnordered,
    pdeinit,
    pfoldFromRoot,
    pinit,
    premove,
    pspendForAddingOrRemovingAnElement,
 )
import Midgard.AvailabilityChallenge qualified as Availability
import Midgard.Common.Utils (
    pgetInclusiveBoundsOfAShortValidityRange,
    pgetInclusiveLowerBoundOfInterval,
    pgetRedeemerAt,
    pgetSpendingRedeemerDataAt,
    phasSigned,
    pquantityOfMint,
 )
import Midgard.CorrectionLock qualified as Correction
import Midgard.DaAttestation qualified as Da
import Midgard.Env qualified as Env
import Midgard.FraudProof (pgetProvenFraudRecord)
import Midgard.HubOracle qualified as Hub
import Midgard.LedgerState (
    PConfirmedState (..),
    PHeaderV1 (..),
    pblockMaturityDurationV1,
    pconfirmedStateNextHeaderProtocolVersionV1,
    pgenesisConfirmedStateV1,
    pheaderV1IsValid,
    pprotocolVersionV1,
 )
import Midgard.OperatorDirectory (PSlashingReason (..))
import Midgard.OperatorDirectory qualified as Dir
import Midgard.OperatorDirectory.ActiveOperators qualified as Active
import Midgard.OperatorDirectory.RetiredOperators qualified as Retired
import Midgard.Scheduler (PSchedDatum (..))
import Midgard.Scheduler qualified as Scheduler
import Midgard.Settlement (pdecodeMintRedeemer)
import Midgard.Settlement qualified as Settlement
import Midgard.StateQueue (
    PAttestationTimeoutRemovalApproach (..),
    PBlockRemovalApproach (..),
    PMintRedeemer (..),
    PSlashingApproach (..),
    PSpendRedeemer (..),
    PStateQueueNode (..),
    pconfirmedStateAssetName,
    pdaAttestationTimeoutV1,
    pdecodeHeaderView,
    pfinalizeLinkedList,
    pgetConfirmedStateRoot,
    pgetStateQueueNode,
    pnoDaAttestation,
 )

--------------------------------------------------------------------------------
-- Small shared helpers
--------------------------------------------------------------------------------

punsafeCoerceRedeemer ::
    forall (a :: S -> Type) (s :: S). Term s (PAsData PRedeemer) -> Term s (PAsData a)
punsafeCoerceRedeemer r = punsafeCoerce (pto (pfromData r))

punsafeCoerceOwnRedeemer ::
    forall (a :: S -> Type) (s :: S). Term s PRedeemer -> Term s (PAsData a)
punsafeCoerceOwnRedeemer r = punsafeCoerce (pto r)

punsafeCoerceData ::
    forall (a :: S -> Type) (s :: S). (PIsData a) => Term s PData -> Term s a
punsafeCoerceData d = pfromData (punsafeCoerce @(PAsData a) d)

pscriptAddress :: forall s. Term s (PAsData PScriptHash) -> Term s PAddress
pscriptAddress scriptHash =
    pcon $
        PAddress
            (pcon $ PScriptCredential scriptHash)
            (pcon PDNothing)

ppubKeyEnterpriseAddress :: forall s. Term s PByteString -> Term s PAddress
ppubKeyEnterpriseAddress keyHash =
    pcon $
        PAddress
            ( pcon $
                PPubKeyCredential
                    (punsafeCoerce @(PAsData PPubKeyHash) $ pdata keyHash)
            )
            (pcon PDNothing)

--------------------------------------------------------------------------------
-- The pure seams
--------------------------------------------------------------------------------

{- | Aiken @state_queue.commit_block_header_output_is_valid_v1@.

A committed header must cover a non-empty interval and satisfy
'pheaderV1IsValid'. The interval check lives here rather than in
'pheaderV1IsValid' because that predicate is deliberately non-relational; this
is the one thing about a header's times that depends on nothing else.
-}
pcommitBlockHeaderOutputIsValidV1 :: forall (s :: S). Term s (PHeaderV1 :--> PBool)
pcommitBlockHeaderOutputIsValidV1 = phoistAcyclic $
    plam $ \header ->
        pmatch header $ \PHeaderV1{pheader'startTime, pheader'endTime} ->
            (pfromData pheader'startTime #< pfromData pheader'endTime)
                #&& (pheaderV1IsValid # header)

{- | Aiken @state_queue.commit_block_header_operator_is_legitimate_v1@.

Four names of an operator must coincide: the one written into the header, the
one the redeemer claims, the one the scheduler says is on shift, and the one
whose active-set node is being spent.

None of the four is trusted on its own — each is authenticated by whichever
script owns it, and this is only the place they are made to agree. That is what
ties "who signed" to "whose turn it is" to "whose bond is being held".
-}
pcommitBlockHeaderOperatorIsLegitimateV1 ::
    forall (s :: S).
    Term s (PAsData PPubKeyHash) ->
    Term s (PAsData PPubKeyHash) ->
    Term s (PAsData PPubKeyHash) ->
    Term s (PAsData PPubKeyHash) ->
    Term s PBool
pcommitBlockHeaderOperatorIsLegitimateV1 headerOp redeemerOp schedulerOp activeOp =
    pand'List
        [ headerOp #== redeemerOp
        , schedulerOp #== redeemerOp
        , activeOp #== redeemerOp
        ]

{- | Aiken @state_queue.commit_block_header_carries_previous_block_v1@.

Appending after another block. The new header's @prev_utxos_root@ must be the
predecessor's @utxos_root@ and its @start_time@ the predecessor's @end_time@,
which is what makes the queue a chain rather than a set: the ledger state and
the covered interval are continuous across the join.
-}
pcommitBlockHeaderCarriesPreviousBlockV1 ::
    forall (s :: S).
    Term s PHeaderV1 ->
    Term s PByteString ->
    Term s PHeaderV1 ->
    Term s PBool
pcommitBlockHeaderCarriesPreviousBlockV1 outputHeader previousHeaderHash previousHeader =
    pmatch outputHeader $
        \PHeaderV1
            { pheader'prevHeaderHash = outPrevHash
            , pheader'prevUtxosRoot = outPrevUtxos
            , pheader'startTime = outStart
            , pheader'protocolVersion = outVersion
            } ->
                pmatch previousHeader $
                    \PHeaderV1
                        { pheader'utxosRoot = prevUtxos
                        , pheader'endTime = prevEnd
                        , pheader'protocolVersion = prevVersion
                        } ->
                            pand'List
                                [ pfromData outPrevHash #== previousHeaderHash
                                , outPrevUtxos #== prevUtxos
                                , outStart #== prevEnd
                                , outVersion #== prevVersion
                                ]

{- | Aiken @state_queue_head_allows_append_v1@.

An unattested head may be extended only by a transaction whose inclusive upper
bound is strictly before the attachment timeout. Attested and published heads
remain appendable; a challenged head does not.
-}
pstateQueueHeadAllowsAppendV1 ::
    forall (s :: S).
    Term s PStateQueueNode ->
    Term s (PAsData PCurrencySymbol) ->
    Term s (PInterval PPosixTime) ->
    Term s PBool
pstateQueueHeadAllowsAppendV1 headNode _daAttestationPolicyId validityRange =
    pmatch headNode $ \PStateQueueNode{pstateQueueNode'header, pstateQueueNode'daAttestation} -> P.do
        header <- plet $ pfromData (pdecodeHeaderView # pstateQueueNode'header)
        let (_, inclusiveUpperBound) = pgetInclusiveBoundsOfAShortValidityRange validityRange
        pmatch header $ \PHeaderV1{pheader'endTime} ->
            (pheaderV1IsValid # header)
                #&& pmatch
                    (pfromData pstateQueueNode'daAttestation)
                    ( \case
                        Availability.PUnattested ->
                            inclusiveUpperBound #< pfromData pheader'endTime + pdaAttestationTimeoutV1
                        Availability.PAttested _ -> pconstant True
                        Availability.PChallenged _ _ -> pconstant False
                        Availability.PPublished _ -> pconstant True
                    )

{- | Aiken @authenticated_queue_head_for_append_v1@.

The singleton root says which node is the current head. For a one-node queue
the consumed append anchor is already that authenticated node; a deeper queue
must provide the head as a separate authenticated reference input.
-}
pauthenticatedQueueHeadForAppendV1 ::
    forall (s :: S).
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PAsData PCurrencySymbol) ->
    Term s PByteString ->
    Term s PData ->
    Term s PInteger ->
    Term s (PMaybeData PInteger) ->
    Term s PStateQueueNode
pauthenticatedQueueHeadForAppendV1
    referenceInputs
    stateQueuePolicyId
    appendAnchorHeaderHash
    appendAnchorData
    confirmedStateRefInputIndex
    mHeadStateQueueNodeRefInputIndex =
        pgetConfirmedStateRoot referenceInputs stateQueuePolicyId confirmedStateRefInputIndex $
            \confirmedState headLink ->
                pmatch (pconfirmedStateNextHeaderProtocolVersionV1 # confirmedState) $ \case
                    PNothing -> perror
                    PJust _ ->
                        pmatch headLink $ \case
                            PDNothing -> perror
                            PDJust headHeaderHashData -> P.do
                                headHeaderHash <- plet $ pfromData headHeaderHashData
                                headNode <-
                                    plet $
                                        pif
                                            (headHeaderHash #== appendAnchorHeaderHash)
                                            ( pmatch mHeadStateQueueNodeRefInputIndex $ \case
                                                PDNothing -> punsafeCoerceData @PStateQueueNode appendAnchorData
                                                PDJust _ -> perror
                                            )
                                            ( pmatch mHeadStateQueueNodeRefInputIndex $ \case
                                                PDNothing -> perror
                                                PDJust headRefInputIndex ->
                                                    pgetStateQueueNode
                                                        referenceInputs
                                                        stateQueuePolicyId
                                                        (pfromData headRefInputIndex)
                                                        ( \authenticatedHead authenticatedHeadHash ->
                                                            pif
                                                                (authenticatedHeadHash #== headHeaderHash)
                                                                authenticatedHead
                                                                perror
                                                        )
                                            )
                                pmatch headNode $ \PStateQueueNode{pstateQueueNode'header} ->
                                    pif
                                        (headHeaderHash #== pblake2b_224 # (pserialiseData # pforgetData pstateQueueNode'header))
                                        headNode
                                        perror

-- | Aiken @fraud_prover_reward_output_is_exact_v1@.
pfraudProverRewardOutputIsExactV1 ::
    forall (s :: S).
    Term s PTxOut ->
    Term s PByteString ->
    Term s PInteger ->
    Term s PBool
pfraudProverRewardOutputIsExactV1 rewardOutput fraudProver expectedReward =
    pmatch rewardOutput $ \PTxOut{ptxOut'address, ptxOut'value, ptxOut'datum, ptxOut'referenceScript} ->
        pand'List
            [ ptxOut'address #== ppubKeyEnterpriseAddress fraudProver
            , pto (pfromData ptxOut'value)
                #== Value.psingletonSortedValue
                # Value.padaSymbol
                # Value.padaToken
                # expectedReward
            , pmatch ptxOut'datum $ \case
                PNoOutputDatum -> pconstant True
                _ -> pconstant False
            , pmatch ptxOut'referenceScript $ \case
                PDNothing -> pconstant True
                PDJust _ -> pconstant False
            ]

-- | Aiken @no_output_pays_fraud_prover_reward_v1@.
pnoOutputPaysFraudProverRewardV1 ::
    forall (s :: S).
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s PByteString ->
    Term s PInteger ->
    Term s PBool
pnoOutputPaysFraudProverRewardV1 outputs fraudProver expectedReward =
    pall
        # plam
            ( \outputData ->
                pnot # pfraudProverRewardOutputIsExactV1 (pfromData outputData) fraudProver expectedReward
            )
        # outputs

-- | Aiken @fraud_prover_reward_routing_is_exact_v1@.
pfraudProverRewardRoutingIsExactV1 ::
    forall (s :: S).
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s (PMaybeData PInteger) ->
    Term s PByteString ->
    Term s PInteger ->
    Term s PBool
pfraudProverRewardRoutingIsExactV1 outputs mRewardOutputIndex fraudProver expectedReward =
    pmatch mRewardOutputIndex $ \case
        PDNothing -> expectedReward #== 0
        PDJust rewardOutputIndexData -> P.do
            rewardOutput <- plet $ pfromData (pelemAt # pfromData rewardOutputIndexData # outputs)
            expectedAddress <- plet $ ppubKeyEnterpriseAddress fraudProver
            pand'List
                [ pfraudProverRewardOutputIsExactV1 rewardOutput fraudProver expectedReward
                , plength
                    # ( pfilter
                            # plam
                                ( \outputData ->
                                    pmatch (pfromData outputData) $ \PTxOut{ptxOut'address} ->
                                        ptxOut'address #== expectedAddress
                                )
                            # outputs
                      )
                    #== 1
                ]

-- | Aiken's public route wrapper is intentionally an exact alias.
prouteFraudProverRewardV1 ::
    forall (s :: S).
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s (PMaybeData PInteger) ->
    Term s PByteString ->
    Term s PInteger ->
    Term s PBool
prouteFraudProverRewardV1 = pfraudProverRewardRoutingIsExactV1

{- | Aiken @remove_fraudulent_blocks_link_v1@.

The authenticated link is the authority for pruning a descendant. Its operator
may differ after scheduler rotation; only the supported protocol version is
required of the removed node.
-}
premoveFraudulentBlocksLinkV1 ::
    forall (s :: S).
    Term s (PAsData PCurrencySymbol) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s PMintValue ->
    Term s PByteString ->
    Term s PTxOutRef ->
    Term s PInteger ->
    Term s PBool
premoveFraudulentBlocksLinkV1
    stateQueuePolicyId
    inputs
    outputs
    mint
    fraudulentBlocksHeaderHash
    fraudulentNodeInputOutref
    fraudulentNodeOutputIndex = P.do
        fraudulentNodeOutput <- plet $ pfromData (pelemAt # fraudulentNodeOutputIndex # outputs)
        pfinalizeLinkedList
            ( premove fraudulentNodeInputOutref fraudulentNodeOutput inputs mint $
                \_anchorInput _anchorLovelaceChange mAnchorHeaderHash _anchorData _removedInput _removedLovelace _removedHeaderHash removedData _removedLink ->
                    pmatch mAnchorHeaderHash $ \case
                        PDNothing -> perror
                        PDJust authenticatedAnchorHash ->
                            pmatch (punsafeCoerceData @PStateQueueNode removedData) $
                                \PStateQueueNode{pstateQueueNode'header} ->
                                    (pfromData authenticatedAnchorHash #== fraudulentBlocksHeaderHash)
                                        #&& pmatch
                                            (pfromData (pdecodeHeaderView # pstateQueueNode'header))
                                            (\PHeaderV1{pheader'protocolVersion} -> pfromData pheader'protocolVersion #== pprotocolVersionV1)
            )
            stateQueuePolicyId

-- | Aiken @remove_last_fraudulent_block_v1@.
premoveLastFraudulentBlockV1 ::
    forall (s :: S).
    Term s (PAsData PCurrencySymbol) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s PMintValue ->
    Term s (PAsData PPubKeyHash) ->
    Term s PByteString ->
    Term s PTxOutRef ->
    Term s PInteger ->
    Term s PBool
premoveLastFraudulentBlockV1
    stateQueuePolicyId
    inputs
    outputs
    mint
    fraudulentOperator
    fraudulentBlocksHeaderHash
    anchorElementInputOutref
    anchorElementOutputIndex = P.do
        anchorElementOutput <- plet $ pfromData (pelemAt # anchorElementOutputIndex # outputs)
        pfinalizeLinkedList
            ( premove anchorElementInputOutref anchorElementOutput inputs mint $
                \_anchorInput _anchorLovelaceChange _mAnchorKey _anchorData _removedInput _removedLovelace removedHeaderHash removedData removedLink ->
                    pmatch (punsafeCoerceData @PStateQueueNode removedData) $
                        \PStateQueueNode{pstateQueueNode'header} ->
                            pmatch (pfromData (pdecodeHeaderView # pstateQueueNode'header)) $
                                \PHeaderV1{pheader'operatorVkey} ->
                                    pand'List
                                        [ fraudulentBlocksHeaderHash #== removedHeaderHash
                                        , pheader'operatorVkey #== fraudulentOperator
                                        , removedLink #== pcon PDNothing
                                        ]
            )
            stateQueuePolicyId

{- | Aiken @unattested_block_timeout_elapsed_v1@.

The state-queue NFT key authenticates the exact serialized header. The timeout
opens at the boundary itself and only while the node is still unattested.
-}
punattestedBlockTimeoutElapsedV1 ::
    forall (s :: S).
    Term s PByteString ->
    Term s PByteString ->
    Term s PStateQueueNode ->
    Term s (PInterval PPosixTime) ->
    Term s PBool
punattestedBlockTimeoutElapsedV1 timedOutHeaderHash authenticatedHeaderHash node validityRange =
    pmatch node $ \PStateQueueNode{pstateQueueNode'header, pstateQueueNode'daAttestation} -> P.do
        header <- plet $ pfromData (pdecodeHeaderView # pstateQueueNode'header)
        let inclusiveLowerBound = pgetInclusiveLowerBoundOfInterval # validityRange
        pmatch header $ \PHeaderV1{pheader'endTime} ->
            pand'List
                [ authenticatedHeaderHash #== timedOutHeaderHash
                , authenticatedHeaderHash #== pblake2b_224 # (pserialiseData # pforgetData pstateQueueNode'header)
                , pstateQueueNode'daAttestation #== pnoDaAttestation
                , pheaderV1IsValid # header
                , inclusiveLowerBound #>= pfromData pheader'endTime + pdaAttestationTimeoutV1
                ]

pconfirmedStateIsAuthenticatedV1 :: forall (s :: S). Term s PConfirmedState -> Term s PBool
pconfirmedStateIsAuthenticatedV1 confirmedState =
    pmatch (pconfirmedStateNextHeaderProtocolVersionV1 # confirmedState) $ \case
        PNothing -> pconstant False
        PJust _ -> pconstant True

-- | Aiken @prune_timed_out_block_descendant_v1@.
ppruneTimedOutBlockDescendantV1 ::
    forall (s :: S).
    Term s (PAsData PCurrencySymbol) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s PMintValue ->
    Term s (PInterval PPosixTime) ->
    Term s PByteString ->
    Term s PInteger ->
    Term s PTxOutRef ->
    Term s PInteger ->
    Term s PBool
ppruneTimedOutBlockDescendantV1
    stateQueuePolicyId
    inputs
    outputs
    referenceInputs
    mint
    validityRange
    timedOutHeaderHash
    confirmedStateRefInputIndex
    timedOutNodeInputOutref
    timedOutNodeOutputIndex = P.do
        _ <-
            plet $
                pgetConfirmedStateRoot referenceInputs stateQueuePolicyId confirmedStateRefInputIndex $
                    \confirmedState headLink ->
                        pif
                            ( pconfirmedStateIsAuthenticatedV1 confirmedState
                                #&& headLink
                                #== pcon (PDJust (pdata timedOutHeaderHash))
                            )
                            (pconstant @PUnit ())
                            perror
        timedOutNodeOutput <- plet $ pfromData (pelemAt # timedOutNodeOutputIndex # outputs)
        pfinalizeLinkedList
            ( premove timedOutNodeInputOutref timedOutNodeOutput inputs mint $
                \_anchorInput _anchorLovelaceChange mAnchorHeaderHash anchorData _removedInput _removedLovelace _removedHeaderHash removedData _removedLink ->
                    pmatch mAnchorHeaderHash $ \case
                        PDNothing -> perror
                        PDJust authenticatedHeaderHash ->
                            let timedOutNode = punsafeCoerceData @PStateQueueNode anchorData
                                removedNode = punsafeCoerceData @PStateQueueNode removedData
                             in pmatch removedNode $ \PStateQueueNode{pstateQueueNode'header = removedHeaderData} ->
                                    punattestedBlockTimeoutElapsedV1
                                        timedOutHeaderHash
                                        (pfromData authenticatedHeaderHash)
                                        timedOutNode
                                        validityRange
                                        #&& pmatch
                                            (pfromData (pdecodeHeaderView # removedHeaderData))
                                            (\PHeaderV1{pheader'protocolVersion} -> pfromData pheader'protocolVersion #== pprotocolVersionV1)
            )
            stateQueuePolicyId

-- | Aiken @remove_unattested_head_after_timeout_v1@.
premoveUnattestedHeadAfterTimeoutV1 ::
    forall (s :: S).
    Term s (PAsData PCurrencySymbol) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s PMintValue ->
    Term s (PInterval PPosixTime) ->
    Term s PByteString ->
    Term s PTxOutRef ->
    Term s PInteger ->
    Term s PBool
premoveUnattestedHeadAfterTimeoutV1
    stateQueuePolicyId
    inputs
    outputs
    mint
    validityRange
    timedOutHeaderHash
    confirmedStateInputOutref
    confirmedStateOutputIndex = P.do
        confirmedStateOutput <- plet $ pfromData (pelemAt # confirmedStateOutputIndex # outputs)
        pfinalizeLinkedList
            ( premove confirmedStateInputOutref confirmedStateOutput inputs mint $
                \_rootInput _rootLovelaceChange mRootKey rootData _removedInput _removedLovelace authenticatedHeaderHash removedData removedLink ->
                    pmatch mRootKey $ \case
                        PDJust _ -> perror
                        PDNothing ->
                            let confirmedState = punsafeCoerceData @PConfirmedState rootData
                                removedNode = punsafeCoerceData @PStateQueueNode removedData
                             in pconfirmedStateIsAuthenticatedV1 confirmedState
                                    #&& punattestedBlockTimeoutElapsedV1
                                        timedOutHeaderHash
                                        authenticatedHeaderHash
                                        removedNode
                                        validityRange
                                    #&& (removedLink #== pcon PDNothing)
            )
            stateQueuePolicyId

punavailableTargetMatchesV1 ::
    forall (s :: S).
    Term s PByteString ->
    Term s PByteString ->
    Term s PByteString ->
    Term s PStateQueueNode ->
    Term s PBool
punavailableTargetMatchesV1 unavailableHeaderHash authenticatedHeaderHash challengeAssetName node =
    pmatch node $ \PStateQueueNode{pstateQueueNode'header, pstateQueueNode'daAttestation} -> P.do
        header <- plet $ pfromData (pdecodeHeaderView # pstateQueueNode'header)
        pand'List
            [ authenticatedHeaderHash #== unavailableHeaderHash
            , authenticatedHeaderHash #== pblake2b_224 # (pserialiseData # pforgetData pstateQueueNode'header)
            , pmatch (pfromData pstateQueueNode'daAttestation) $ \case
                Availability.PChallenged _ authenticatedChallengeAssetName ->
                    pto (pfromData authenticatedChallengeAssetName) #== challengeAssetName
                _ -> pconstant False
            , pheaderV1IsValid # header
            ]

-- | Aiken @prune_unavailable_block_descendant_v1@.
ppruneUnavailableBlockDescendantV1 ::
    forall (s :: S).
    Term s (PAsData PCurrencySymbol) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s PMintValue ->
    Term s PByteString ->
    Term s PByteString ->
    Term s PInteger ->
    Term s PTxOutRef ->
    Term s PInteger ->
    Term s PBool
ppruneUnavailableBlockDescendantV1
    stateQueuePolicyId
    inputs
    outputs
    referenceInputs
    mint
    unavailableHeaderHash
    challengeAssetName
    confirmedStateRefInputIndex
    unavailableNodeInputOutref
    unavailableNodeOutputIndex = P.do
        _ <-
            plet $
                pgetConfirmedStateRoot referenceInputs stateQueuePolicyId confirmedStateRefInputIndex $
                    \confirmedState headLink ->
                        pif
                            ( pconfirmedStateIsAuthenticatedV1 confirmedState
                                #&& headLink
                                #== pcon (PDJust (pdata unavailableHeaderHash))
                            )
                            (pconstant @PUnit ())
                            perror
        unavailableNodeOutput <- plet $ pfromData (pelemAt # unavailableNodeOutputIndex # outputs)
        pfinalizeLinkedList
            ( premove unavailableNodeInputOutref unavailableNodeOutput inputs mint $
                \_anchorInput _anchorLovelaceChange mAnchorHeaderHash anchorData _removedInput _removedLovelace _removedHeaderHash removedData _removedLink ->
                    pmatch mAnchorHeaderHash $ \case
                        PDNothing -> perror
                        PDJust authenticatedHeaderHash ->
                            let unavailableNode = punsafeCoerceData @PStateQueueNode anchorData
                                removedNode = punsafeCoerceData @PStateQueueNode removedData
                             in pmatch removedNode $ \PStateQueueNode{pstateQueueNode'header = removedHeaderData} ->
                                    punavailableTargetMatchesV1
                                        unavailableHeaderHash
                                        (pfromData authenticatedHeaderHash)
                                        challengeAssetName
                                        unavailableNode
                                        #&& pmatch
                                            (pfromData (pdecodeHeaderView # removedHeaderData))
                                            (\PHeaderV1{pheader'protocolVersion} -> pfromData pheader'protocolVersion #== pprotocolVersionV1)
            )
            stateQueuePolicyId

-- | Aiken @remove_unavailable_head_v1@.
premoveUnavailableHeadV1 ::
    forall (s :: S).
    Term s (PAsData PCurrencySymbol) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s PMintValue ->
    Term s PByteString ->
    Term s PByteString ->
    Term s PTxOutRef ->
    Term s PInteger ->
    Term s PBool
premoveUnavailableHeadV1
    stateQueuePolicyId
    inputs
    outputs
    mint
    unavailableHeaderHash
    challengeAssetName
    confirmedStateInputOutref
    confirmedStateOutputIndex = P.do
        confirmedStateOutput <- plet $ pfromData (pelemAt # confirmedStateOutputIndex # outputs)
        pfinalizeLinkedList
            ( premove confirmedStateInputOutref confirmedStateOutput inputs mint $
                \_rootInput _rootLovelaceChange mRootKey rootData _removedInput _removedLovelace authenticatedHeaderHash removedData removedLink ->
                    pmatch mRootKey $ \case
                        PDJust _ -> perror
                        PDNothing ->
                            let confirmedState = punsafeCoerceData @PConfirmedState rootData
                                removedNode = punsafeCoerceData @PStateQueueNode removedData
                             in pconfirmedStateIsAuthenticatedV1 confirmedState
                                    #&& punavailableTargetMatchesV1
                                        unavailableHeaderHash
                                        authenticatedHeaderHash
                                        challengeAssetName
                                        removedNode
                                    #&& (removedLink #== pcon PDNothing)
            )
            stateQueuePolicyId

{- | Aiken @state_queue.commit_block_header_carries_confirmed_state_v1@.

The same continuity, but appending after the /root/. The difference that matters
is the protocol version: it is not copied from the confirmed state but taken
from 'pconfirmedStateNextHeaderProtocolVersionV1', which authenticates the state
first and answers with the version the /next/ header must carry.

That is what stops the genesis sentinel's version zero reaching a block, and it
is why this returns @False@ rather than erroring when the state fails to
authenticate: an unauthenticated state simply cannot answer.
-}
pcommitBlockHeaderCarriesConfirmedStateV1 ::
    forall (s :: S).
    Term s PHeaderV1 ->
    Term s PConfirmedState ->
    Term s PBool
pcommitBlockHeaderCarriesConfirmedStateV1 outputHeader confirmedState =
    pmatch (pconfirmedStateNextHeaderProtocolVersionV1 # confirmedState) $ \case
        PNothing -> pconstant False
        PJust nextProtocolVersion ->
            pmatch outputHeader $
                \PHeaderV1
                    { pheader'prevHeaderHash = outPrevHash
                    , pheader'prevUtxosRoot = outPrevUtxos
                    , pheader'startTime = outStart
                    , pheader'protocolVersion = outVersion
                    } ->
                        pmatch confirmedState $
                            \PConfirmedState
                                { pconfirmed'headerHash
                                , pconfirmed'utxoRoot
                                , pconfirmed'endTime
                                } ->
                                    pand'List
                                        [ outPrevHash #== pconfirmed'headerHash
                                        , outPrevUtxos #== pconfirmed'utxoRoot
                                        , outStart #== pconfirmed'endTime
                                        , pfromData outVersion #== nextProtocolVersion
                                        ]

{- | Aiken @state_queue.merge_commitments_match_header@.

The merge redeemer restates all seven roots and all seven counts, and this
requires every one to equal the header's. The restatement is not redundancy for
its own sake: the settlement that these commitments spawn reads them from the
redeemer, so if they could drift from the header a settlement could be spawned
against commitments no block ever made.
-}
pmergeCommitmentsMatchHeader ::
    forall (s :: S).
    Term s PHeaderV1 ->
    Term s PMintRedeemer ->
    Term s PBool
pmergeCommitmentsMatchHeader header redeemer =
    pmatch header $
        \PHeaderV1
            { pheader'withdrawalsRoot
            , pheader'forcedTransactionsRoot
            , pheader'transactionsRoot
            , pheader'depositsRoot
            , pheader'transitionTraceRoot
            , pheader'eventToStepRoot
            , pheader'validationTracesRoot
            , pheader'withdrawalCount
            , pheader'forcedTransactionCount
            , pheader'l2TransactionCount
            , pheader'depositCount
            , pheader'totalEventCount
            , pheader'transitionStepCount
            , pheader'validationTraceCount
            } ->
                pmatch redeemer $ \case
                    PMergeToConfirmedStateV1
                        { psqMerge'withdrawalsRoot
                        , psqMerge'forcedTransactionsRoot
                        , psqMerge'transactionsRoot
                        , psqMerge'depositsRoot
                        , psqMerge'transitionTraceRoot
                        , psqMerge'eventToStepRoot
                        , psqMerge'validationTracesRoot
                        , psqMerge'withdrawalCount
                        , psqMerge'forcedTransactionCount
                        , psqMerge'l2TransactionCount
                        , psqMerge'depositCount
                        , psqMerge'totalEventCount
                        , psqMerge'transitionStepCount
                        , psqMerge'validationTraceCount
                        } ->
                            pand'List
                                [ psqMerge'withdrawalsRoot #== pheader'withdrawalsRoot
                                , psqMerge'forcedTransactionsRoot #== pheader'forcedTransactionsRoot
                                , psqMerge'transactionsRoot #== pheader'transactionsRoot
                                , psqMerge'depositsRoot #== pheader'depositsRoot
                                , psqMerge'transitionTraceRoot #== pheader'transitionTraceRoot
                                , psqMerge'eventToStepRoot #== pheader'eventToStepRoot
                                , psqMerge'validationTracesRoot #== pheader'validationTracesRoot
                                , psqMerge'withdrawalCount #== pheader'withdrawalCount
                                , psqMerge'forcedTransactionCount #== pheader'forcedTransactionCount
                                , psqMerge'l2TransactionCount #== pheader'l2TransactionCount
                                , psqMerge'depositCount #== pheader'depositCount
                                , psqMerge'totalEventCount #== pheader'totalEventCount
                                , psqMerge'transitionStepCount #== pheader'transitionStepCount
                                , psqMerge'validationTraceCount #== pheader'validationTraceCount
                                ]
                    _ -> perror

{- | Aiken @state_queue.header_carries_l2_material@.

Whether a block did anything an L2 user could dispute: any of the four event
roots being non-empty. This is the switch that decides whether merging the block
must also spawn a settlement.
-}
pheaderCarriesL2Material :: forall (s :: S). Term s (PHeaderV1 :--> PBool)
pheaderCarriesL2Material = phoistAcyclic $
    plam $ \header ->
        pmatch header $
            \PHeaderV1
                { pheader'transactionsRoot
                , pheader'depositsRoot
                , pheader'withdrawalsRoot
                , pheader'forcedTransactionsRoot
                } ->
                    plet Env.pemptyMerkleTreeRoot $ \emptyRoot ->
                        pnot
                            #$ pand'List
                                [ pfromData pheader'transactionsRoot #== emptyRoot
                                , pfromData pheader'depositsRoot #== emptyRoot
                                , pfromData pheader'withdrawalsRoot #== emptyRoot
                                , pfromData pheader'forcedTransactionsRoot #== emptyRoot
                                ]

{- | Aiken @state_queue.merge_settlement_binding_matches_header@.

A block carrying L2 material must spawn a settlement whose id is the block's own
hash; a block carrying none must spawn no settlement at all.

Both directions matter. The first is what gives users something to dispute
against. The second stops a settlement being spawned for an empty block, which
would let an operator's bond be tied up — or a payout claimed — against a block
that moved nothing.
-}
pmergeSettlementBindingMatchesHeader ::
    forall (s :: S).
    Term s PHeaderV1 ->
    Term s PByteString ->
    Term s (PMaybe PByteString) ->
    Term s PBool
pmergeSettlementBindingMatchesHeader header headerNodeKey mSettlementId =
    pif
        (pheaderCarriesL2Material # header)
        ( pmatch mSettlementId $ \case
            PNothing -> pconstant False
            PJust settlementId -> settlementId #== headerNodeKey
        )
        ( pmatch mSettlementId $ \case
            PNothing -> pconstant True
            PJust _ -> pconstant False
        )

{- | Aiken @state_queue.merge_settlement_id_at_route@.

Reads the settlement id out of the settlement policy's own @Spawn@ redeemer, at
the index the merge redeemer names. @None@ means no settlement is being spawned;
it is not an error, because that is the correct shape for an empty block.
-}
pmergeSettlementIdAtRoute ::
    forall (s :: S).
    Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
    Term s (PAsData PCurrencySymbol) ->
    Term s (PMaybeData PInteger) ->
    Term s (PMaybe PByteString)
pmergeSettlementIdAtRoute redeemers settlementScriptHash mSettlementRedeemerIndex =
    pmatch mSettlementRedeemerIndex $ \case
        PDNothing -> pcon PNothing
        PDJust settlementRedeemerIndex ->
            pmatch
                ( pdecodeMintRedeemer
                    #$ pto
                    $ pfromData
                        ( pgetRedeemerAt
                            # redeemers
                            # pdata (pcon (PMinting settlementScriptHash))
                            # pfromData settlementRedeemerIndex
                        )
                )
                $ \case
                    Settlement.PSpawn{Settlement.pspawn'settlementId} ->
                        pcon (PJust (pto (pfromData pspawn'settlementId)))
                    _ -> perror

--------------------------------------------------------------------------------
-- Spend
--------------------------------------------------------------------------------

{- | Aiken @validators/state-queue.ak@ — @spend@.

Three ways can spend a queue UTxO. @LinkedListMutation@ is the usual gate: any
structural change is permitted whenever the queue's own minting policy runs,
which is where the real decisions are made.

@AttachDaAttestation@ is the exception — the one spend that edits a node without
minting anything. It defers to the DA attestation policy's redeemer, and the
only thing checked here is that both scripts name the same state-queue input.
Without that agreement one attestation could be attached to a different block
than the one the DA policy validated.

@AvailabilityStatusUpdate@ likewise delegates a status-only continuation to
the availability policy, while binding both the consumed queue input and its
continuing output.
-}
stateQueueSpendValidator ::
    forall (s :: S).
    Term
        s
        ( PAsData PCurrencySymbol -- state queue mint script hash
            :--> PAsData PCurrencySymbol -- DA attestation policy id
            :--> PAsData PCurrencySymbol -- availability policy id
            :--> PScriptContext
            :--> PUnit
        )
stateQueueSpendValidator = plam $ \stateQueueMintScriptHash daAttestationPolicyId availabilityPolicyId ctx -> P.do
    PScriptContext{pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
        pmatch ctx
    ownRef <-
        plet $ pmatch pscriptContext'scriptInfo $ \case
            PSpendingScript outRef _ -> outRef
            _ -> perror
    PTxInfo{ptxInfo'inputs, ptxInfo'mint, ptxInfo'redeemers} <- pmatch pscriptContext'txInfo
    redeemer <-
        plet $ pfromData (punsafeCoerceOwnRedeemer @PSpendRedeemer pscriptContext'redeemer)
    pif
        ( pmatch redeemer $ \case
            PLinkedListMutation ->
                pspendForAddingOrRemovingAnElement
                    # stateQueueMintScriptHash
                    # pfromData ptxInfo'mint
            PAttachDaAttestation
                { psqAttach'stateQueueInputIndex
                , psqAttach'daAttestationMintRedeemerIndex
                } -> P.do
                    stateQueueInputIndex <- plet $ pfromData psqAttach'stateQueueInputIndex
                    PTxInInfo{ptxInInfo'outRef} <-
                        pmatch $
                            pfromData (pelemAt # stateQueueInputIndex # pfromData ptxInfo'inputs)
                    pif
                        (pnot # (ptxInInfo'outRef #== ownRef))
                        perror
                        $ pmatch
                            ( pfromData
                                ( punsafeCoerceRedeemer @Da.PMintRedeemer $
                                    pgetRedeemerAt
                                        # pto (pto (pfromData ptxInfo'redeemers))
                                        # pdata (pcon (PMinting daAttestationPolicyId))
                                        # pfromData psqAttach'daAttestationMintRedeemerIndex
                                )
                            )
                        $ \case
                            Da.PApplyToStateQueue{Da.papply'stateQueueInputIndex} ->
                                pfromData papply'stateQueueInputIndex #== stateQueueInputIndex
                            _ -> perror
            PAvailabilityStatusUpdate
                { psqAvailabilityUpdate'stateQueueInputIndex
                , psqAvailabilityUpdate'stateQueueOutputIndex
                , psqAvailabilityUpdate'availabilityMintRedeemerIndex
                } -> P.do
                    stateQueueInputIndex <- plet $ pfromData psqAvailabilityUpdate'stateQueueInputIndex
                    PTxInInfo{ptxInInfo'outRef} <-
                        pmatch $ pfromData (pelemAt # stateQueueInputIndex # pfromData ptxInfo'inputs)
                    pand'List
                        [ ptxInInfo'outRef #== ownRef
                        , pavailabilityStatusUpdateIsAuthorizedV1
                            (pto (pto (pfromData ptxInfo'redeemers)))
                            availabilityPolicyId
                            (pfromData psqAvailabilityUpdate'availabilityMintRedeemerIndex)
                            stateQueueInputIndex
                            (pfromData psqAvailabilityUpdate'stateQueueOutputIndex)
                        ]
        )
        (pconstant ())
        perror

{- | Aiken @availability_status_update_is_authorized_v1@.

Only availability-policy arms that preserve the queue node are admitted, and
their embedded input/output indices must name this exact continuation.
-}
pavailabilityStatusUpdateIsAuthorizedV1 ::
    forall s.
    Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
    Term s (PAsData PCurrencySymbol) ->
    Term s PInteger ->
    Term s PInteger ->
    Term s PInteger ->
    Term s PBool
pavailabilityStatusUpdateIsAuthorizedV1 redeemers availabilityPolicyId redeemerIndex inputIndex outputIndex =
    plet
        ( pfromData $
            punsafeCoerce @(PAsData Availability.PMintRedeemerV1) $
                pto $
                    pfromData $
                        pgetRedeemerAt
                            # redeemers
                            # pdata (pcon (PMinting availabilityPolicyId))
                            # redeemerIndex
        )
        $ \availabilityRedeemer ->
            plet (pasConstr # pforgetData (pdata availabilityRedeemer)) $ \encoded ->
                plet (pmatch encoded $ \(PBuiltinPair pairFirst _) -> pairFirst) $ \tag ->
                    plet (pmatch encoded $ \(PBuiltinPair _ pairSecond) -> pairSecond) $ \fields ->
                        pif
                            (tag #== 0 #|| tag #== 1)
                            ( pfromData (punsafeCoerce @(PAsData PInteger) $ pelemAt # 4 # fields)
                                #== inputIndex
                                #&& pfromData (punsafeCoerce @(PAsData PInteger) $ pelemAt # 5 # fields)
                                #== outputIndex
                            )
                            ( pif
                                (tag #== 3)
                                ( pfromData (punsafeCoerce @(PAsData PInteger) $ pelemAt # 3 # fields)
                                    #== inputIndex
                                    #&& pfromData (punsafeCoerce @(PAsData PInteger) $ pelemAt # 4 # fields)
                                    #== outputIndex
                                )
                                (pconstant False)
                            )

--------------------------------------------------------------------------------
-- Mint
--------------------------------------------------------------------------------

-- | Aiken @validators/state-queue.ak@ — @mint@.
stateQueueMintValidator ::
    forall (s :: S).
    Term
        s
        ( PAsData PCurrencySymbol -- hub oracle script hash
            :--> PAsData PScriptHash -- correction-lock script hash
            :--> PAsData PCurrencySymbol -- active operators script hash
            :--> PAsData PAddress -- active operators address
            :--> PAsData PCurrencySymbol -- retired operators script hash
            :--> PAsData PCurrencySymbol -- scheduler script hash
            :--> PAsData PCurrencySymbol -- fraud proof script hash
            :--> PAsData PCurrencySymbol -- settlement script hash
            :--> PAsData PCurrencySymbol -- DA attestation policy id
            :--> PAsData PCurrencySymbol -- availability policy id
            :--> PScriptContext
            :--> PUnit
        )
stateQueueMintValidator =
    plam $
        \hubOracleScriptHash
         correctionLockScriptHash
         activeOperatorsScriptHash
         activeOperatorsAddr
         retiredOperatorsScriptHash
         schedulerScriptHash
         fraudProofScriptHash
         settlementScriptHash
         daAttestationPolicyId
         availabilityPolicyId
         ctx -> P.do
                PScriptContext{pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
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
                    , ptxInfo'signatories
                    , ptxInfo'redeemers
                    , ptxInfo'validRange
                    } <-
                    pmatch pscriptContext'txInfo
                inputs <- plet $ pfromData ptxInfo'inputs
                outputs <- plet $ pfromData ptxInfo'outputs
                referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
                mint <- plet $ pfromData ptxInfo'mint
                redeemers <- plet $ pto (pto (pfromData ptxInfo'redeemers))
                redeemer <-
                    plet $ pfromData (punsafeCoerceOwnRedeemer @PMintRedeemer pscriptContext'redeemer)
                correctionLockAddress <- plet $ pscriptAddress correctionLockScriptHash

                pif
                    ( pmatch redeemer $ \case
                        ------------------------------------------------------------------
                        PInitV1{psqInit'outputIndex} ->
                            plet
                                (Correction.puniqueOutput # outputs # hubOracleScriptHash # correctionLockAddress)
                                ( \lockOutput ->
                                    pand'List
                                        [ Correction.pdecodeDatum # lockOutput #== pcon Correction.PIdle
                                        , pquantityOfMint # mint # hubOracleScriptHash # Correction.passetName #== 1
                                        , pvalidateInit
                                            ownPolicyId
                                            hubOracleScriptHash
                                            outputs
                                            mint
                                            ptxInfo'validRange
                                            (pfromData psqInit'outputIndex)
                                        ]
                                )
                        ------------------------------------------------------------------
                        PDeinit ->
                            plet
                                (Correction.puniqueInput # inputs # hubOracleScriptHash # correctionLockAddress)
                                ( \lockInput ->
                                    pmatch lockInput $ \PTxInInfo{ptxInInfo'resolved = lockOutput} ->
                                        pand'List
                                            [ Correction.pdecodeDatum # lockOutput #== pcon Correction.PIdle
                                            , pquantityOfMint # mint # hubOracleScriptHash # Correction.passetName #== (-1)
                                            , Correction.phasNoOutput # outputs # hubOracleScriptHash
                                            , pvalidateDeinit ownPolicyId hubOracleScriptHash inputs mint
                                            ]
                                )
                        ------------------------------------------------------------------
                        PCommitBlockHeader
                            { psqCommit'newBlockOutputIndex
                            , psqCommit'continuedLatestBlockOutputIndex
                            , psqCommit'operator
                            , psqCommit'schedulerRefInputIndex
                            , psqCommit'activeOperatorsInputIndex
                            , psqCommit'activeOperatorsRedeemerIndex
                            , psqCommit'mConfirmedStateRefInputIndex
                            , psqCommit'mHeadStateQueueNodeRefInputIndex
                            } ->
                                Correction.preferencesIdle
                                    # referenceInputs
                                    # hubOracleScriptHash
                                    # correctionLockAddress
                                    #&& pvalidateCommitBlockHeader
                                        ownPolicyId
                                        daAttestationPolicyId
                                        activeOperatorsAddr
                                        schedulerScriptHash
                                        inputs
                                        outputs
                                        referenceInputs
                                        mint
                                        redeemers
                                        (pfromData ptxInfo'signatories)
                                        ptxInfo'validRange
                                        (pfromData psqCommit'newBlockOutputIndex)
                                        (pfromData psqCommit'continuedLatestBlockOutputIndex)
                                        psqCommit'operator
                                        (pfromData psqCommit'schedulerRefInputIndex)
                                        (pfromData psqCommit'activeOperatorsInputIndex)
                                        (pfromData psqCommit'activeOperatorsRedeemerIndex)
                                        (pfromData psqCommit'mConfirmedStateRefInputIndex)
                                        (pfromData psqCommit'mHeadStateQueueNodeRefInputIndex)
                        ------------------------------------------------------------------
                        PRemoveFraudulentBlockHeader
                            { psqRemove'fraudulentOperator
                            , psqRemove'fraudulentBlocksHeaderHash
                            , psqRemove'slashingApproach
                            , psqRemove'fraudProofRefInputIndex
                            , psqRemove'blockRemovalApproach
                            } ->
                                plet
                                    (Correction.puniqueInput # inputs # hubOracleScriptHash # correctionLockAddress)
                                    ( \_ ->
                                        pvalidateRemoveFraudulentBlockHeader
                                            ownPolicyId
                                            activeOperatorsScriptHash
                                            retiredOperatorsScriptHash
                                            fraudProofScriptHash
                                            inputs
                                            outputs
                                            referenceInputs
                                            mint
                                            redeemers
                                            psqRemove'fraudulentOperator
                                            (pfromData psqRemove'fraudulentBlocksHeaderHash)
                                            (pfromData psqRemove'slashingApproach)
                                            (pfromData psqRemove'fraudProofRefInputIndex)
                                            (pfromData psqRemove'blockRemovalApproach)
                                    )
                        ------------------------------------------------------------------
                        PRemoveUnattestedBlockAfterTimeout
                            { psqRemoveUnattested'timedOutHeaderHash
                            , psqRemoveUnattested'removalApproach
                            } ->
                                plet
                                    (Correction.puniqueInput # inputs # hubOracleScriptHash # correctionLockAddress)
                                    ( \_ ->
                                        pmatch (pfromData psqRemoveUnattested'removalApproach) $ \case
                                            PPruneTimedOutBlockDescendant
                                                { ppruneTimedOut'confirmedStateRefInputIndex
                                                , ppruneTimedOut'timedOutNodeInputOutref
                                                , ppruneTimedOut'timedOutNodeOutputIndex
                                                } ->
                                                    ppruneTimedOutBlockDescendantV1
                                                        ownPolicyId
                                                        inputs
                                                        outputs
                                                        referenceInputs
                                                        mint
                                                        ptxInfo'validRange
                                                        (pfromData psqRemoveUnattested'timedOutHeaderHash)
                                                        (pfromData ppruneTimedOut'confirmedStateRefInputIndex)
                                                        (pfromData ppruneTimedOut'timedOutNodeInputOutref)
                                                        (pfromData ppruneTimedOut'timedOutNodeOutputIndex)
                                            PRemoveTimedOutHead
                                                { premoveTimedOutHead'confirmedStateInputOutref
                                                , premoveTimedOutHead'confirmedStateOutputIndex
                                                } ->
                                                    premoveUnattestedHeadAfterTimeoutV1
                                                        ownPolicyId
                                                        inputs
                                                        outputs
                                                        mint
                                                        ptxInfo'validRange
                                                        (pfromData psqRemoveUnattested'timedOutHeaderHash)
                                                        (pfromData premoveTimedOutHead'confirmedStateInputOutref)
                                                        (pfromData premoveTimedOutHead'confirmedStateOutputIndex)
                                    )
                        ------------------------------------------------------------------
                        PRemoveUnavailableBlockAfterTimeout
                            { psqRemoveUnavailable'unavailableHeaderHash
                            , psqRemoveUnavailable'challengeAssetName
                            , psqRemoveUnavailable'removalApproach
                            } ->
                                plet
                                    (Correction.puniqueInput # inputs # hubOracleScriptHash # correctionLockAddress)
                                    ( \lockInput ->
                                        pmatch lockInput $ \PTxInInfo{ptxInInfo'resolved = lockOutput} ->
                                            plet
                                                ( pcon $
                                                    Correction.PLocked
                                                        psqRemoveUnavailable'unavailableHeaderHash
                                                        (pdata $ pcon $ Correction.PAvailabilityChallenge psqRemoveUnavailable'challengeAssetName)
                                                )
                                                ( \expectedLock ->
                                                    let lockAuthorized =
                                                            pmatch (Correction.pdecodeDatum # lockOutput) $ \case
                                                                Correction.PIdle ->
                                                                    pquantityOfMint
                                                                        # mint
                                                                        # availabilityPolicyId
                                                                        # psqRemoveUnavailable'challengeAssetName
                                                                        #== (-1)
                                                                current ->
                                                                    pcon current
                                                                        #== expectedLock
                                                                        #&& pquantityOfMint
                                                                        # mint
                                                                        # availabilityPolicyId
                                                                        # psqRemoveUnavailable'challengeAssetName
                                                                        #== 0
                                                     in lockAuthorized
                                                            #&& pmatch
                                                                (pfromData psqRemoveUnavailable'removalApproach)
                                                                ( \case
                                                                    PPruneTimedOutBlockDescendant
                                                                        { ppruneTimedOut'confirmedStateRefInputIndex
                                                                        , ppruneTimedOut'timedOutNodeInputOutref
                                                                        , ppruneTimedOut'timedOutNodeOutputIndex
                                                                        } ->
                                                                            ppruneUnavailableBlockDescendantV1
                                                                                ownPolicyId
                                                                                inputs
                                                                                outputs
                                                                                referenceInputs
                                                                                mint
                                                                                (pfromData psqRemoveUnavailable'unavailableHeaderHash)
                                                                                (pto $ pfromData psqRemoveUnavailable'challengeAssetName)
                                                                                (pfromData ppruneTimedOut'confirmedStateRefInputIndex)
                                                                                (pfromData ppruneTimedOut'timedOutNodeInputOutref)
                                                                                (pfromData ppruneTimedOut'timedOutNodeOutputIndex)
                                                                    PRemoveTimedOutHead
                                                                        { premoveTimedOutHead'confirmedStateInputOutref
                                                                        , premoveTimedOutHead'confirmedStateOutputIndex
                                                                        } ->
                                                                            premoveUnavailableHeadV1
                                                                                ownPolicyId
                                                                                inputs
                                                                                outputs
                                                                                mint
                                                                                (pfromData psqRemoveUnavailable'unavailableHeaderHash)
                                                                                (pto $ pfromData psqRemoveUnavailable'challengeAssetName)
                                                                                (pfromData premoveTimedOutHead'confirmedStateInputOutref)
                                                                                (pfromData premoveTimedOutHead'confirmedStateOutputIndex)
                                                                )
                                                )
                                    )
                        ------------------------------------------------------------------
                        PMergeToConfirmedStateV1{} ->
                            Correction.preferencesIdle
                                # referenceInputs
                                # hubOracleScriptHash
                                # correctionLockAddress
                                #&& pvalidateMergeToConfirmedState
                                    ownPolicyId
                                    daAttestationPolicyId
                                    settlementScriptHash
                                    inputs
                                    outputs
                                    mint
                                    redeemers
                                    ptxInfo'validRange
                                    redeemer
                    )
                    (pconstant ())
                    perror

--------------------------------------------------------------------------------
-- InitV1 / Deinit
--------------------------------------------------------------------------------

{- | Aiken @InitV1@.

Creates the queue with the genesis sentinel as its root, at the transaction's
inclusive upper bound. The hub oracle NFT must be minted in the same
transaction, which is what makes the queue and the protocol instance the same
object.

The root's data must equal the sentinel exactly — 'pgenesisConfirmedStateV1'
builds it here and the output is compared against that, rather than the output
being inspected field by field.
-}
pvalidateInit ::
    forall (s :: S) (a :: S -> Type).
    Term s (PAsData PCurrencySymbol) ->
    Term s (PAsData PCurrencySymbol) ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s PMintValue ->
    Term s a ->
    Term s PInteger ->
    Term s PBool
pvalidateInit ownPolicyId hubOracleScriptHash outputs mint validityRange outputIndex = P.do
    rootOutput <- plet $ pfromData (pelemAt # outputIndex # outputs)
    let (_, currentTimeUpper) =
            pgetInclusiveBoundsOfAShortValidityRange (punsafeCoerce validityRange)
    expectedRoot <-
        plet $
            pmatch (pgenesisConfirmedStateV1 # currentTimeUpper) $ \case
                PNothing -> perror
                PJust st -> pforgetData (pdata st)
    pinit
        ( pquantityOfMint
            # mint
            # hubOracleScriptHash
            # Hub.passetName
            #== 1
        )
        rootOutput
        mint
        (\_address _lovelace rootData -> rootData #== expectedRoot)
        ownPolicyId
        pconfirmedStateAssetName

{- | Aiken @Deinit@.

Tears the queue down. The linked list only permits this when the root is the
sole remaining element, so there is nothing here beyond requiring the hub's NFT
to burn alongside — a queue holding blocks cannot be discarded.
-}
pvalidateDeinit ::
    forall (s :: S).
    Term s (PAsData PCurrencySymbol) ->
    Term s (PAsData PCurrencySymbol) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s PMintValue ->
    Term s PBool
pvalidateDeinit ownPolicyId hubOracleScriptHash inputs mint =
    pif
        (pquantityOfMint # mint # hubOracleScriptHash # Hub.passetName #== (-1))
        ( pdeinit
            inputs
            mint
            (\_input _lovelace _rootData -> pconstant True)
            ownPolicyId
            pconfirmedStateAssetName
        )
        perror

--------------------------------------------------------------------------------
-- CommitBlockHeader
--------------------------------------------------------------------------------

{- | Aiken @CommitBlockHeader@.

Appends a block. The order of the checks is the order of the argument it makes:
the operator signed, the append is a valid list operation, the node's key is the
header's own hash, the header is internally valid and its interval is bound to
this transaction, the scheduler says it is this operator's turn, the active set
is holding this operator's bond, and finally the header carries over correctly
from whatever it was appended to.

The key-is-the-hash step is the one everything else leans on. The asset name is
minted by this policy as @blake2b_224(serialise(header))@, so nothing downstream
that identifies a block by hash can be pointed at a different header.
-}
pvalidateCommitBlockHeader ::
    forall (s :: S).
    Term s (PAsData PCurrencySymbol) ->
    Term s (PAsData PCurrencySymbol) ->
    Term s (PAsData PAddress) ->
    Term s (PAsData PCurrencySymbol) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s PMintValue ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
    Term s (PBuiltinList (PAsData PPubKeyHash)) ->
    Term s (PInterval PPosixTime) ->
    Term s PInteger ->
    Term s PInteger ->
    Term s (PAsData PPubKeyHash) ->
    Term s PInteger ->
    Term s PInteger ->
    Term s PInteger ->
    Term s (PMaybeData PInteger) ->
    Term s (PMaybeData PInteger) ->
    Term s PBool
pvalidateCommitBlockHeader
    ownPolicyId
    daAttestationPolicyId
    activeOperatorsAddr
    schedulerScriptHash
    inputs
    outputs
    referenceInputs
    mint
    redeemers
    signatories
    validityRange
    newBlockOutputIndex
    continuedLatestBlockOutputIndex
    operator
    schedulerRefInputIndex
    activeOperatorsInputIndex
    activeOperatorsRedeemerIndex
    mConfirmedStateRefInputIndex
    mHeadStateQueueNodeRefInputIndex = P.do
        pif
            (pnot #$ phasSigned # operator # signatories)
            perror
            $ P.do
                contAnchorOutput <-
                    plet $ pfromData (pelemAt # continuedLatestBlockOutputIndex # outputs)
                newBlockOutput <- plet $ pfromData (pelemAt # newBlockOutputIndex # outputs)
                pfinalizeLinkedList
                    ( pappendUnordered contAnchorOutput newBlockOutput inputs mint $
                        \_anchorInput _anchorLovelaceChange mAnchorKey anchorData _newLovelace newNodeKey newNodeData -> P.do
                            PStateQueueNode
                                { pstateQueueNode'header = outputHeaderData
                                , pstateQueueNode'daAttestation
                                } <-
                                pmatch (punsafeCoerceData @PStateQueueNode newNodeData)
                            -- A block is committed without an attestation; attaching one is
                            -- a later, separate spend.
                            _ <-
                                plet $
                                    pif
                                        (pstateQueueNode'daAttestation #== pnoDaAttestation)
                                        (pconstant @PUnit ())
                                        perror
                            outputHeader <- plet $ pfromData (pdecodeHeaderView # outputHeaderData)
                            PHeaderV1{pheader'operatorVkey, pheader'startTime, pheader'endTime} <-
                                pmatch outputHeader
                            schedulerOperator <-
                                plet
                                    $ pmatch
                                        ( Scheduler.pgetDatum
                                            # referenceInputs
                                            # schedulerScriptHash
                                            # schedulerRefInputIndex
                                        )
                                    $ \case
                                        PActiveOperator{pschedActive'operator} -> pschedActive'operator
                                        PNoActiveOperators -> perror
                            activeOperator <-
                                plet
                                    $ pmatch
                                        ( pfromData
                                            ( punsafeCoerceRedeemer @Active.PSpendRedeemer $
                                                pgetSpendingRedeemerDataAt
                                                    # pfromData activeOperatorsAddr
                                                    # activeOperatorsInputIndex
                                                    # activeOperatorsRedeemerIndex
                                                    # inputs
                                                    # redeemers
                                            )
                                        )
                                    $ \case
                                        Active.PUpdateBondHoldNewState{Active.pupdateState'activeOperator} ->
                                            pupdateState'activeOperator
                                        _ -> perror
                            pand'List
                                [ -- The node's key is the header's own hash.
                                  newNodeKey
                                    #== (pblake2b_224 #$ pserialiseData # pforgetData outputHeaderData)
                                , pcommitBlockHeaderOutputIsValidV1 # outputHeader
                                , pcommitBoundHeaderTime
                                    (pfromData pheader'startTime)
                                    (pfromData pheader'endTime)
                                    validityRange
                                , pcommitBlockHeaderOperatorIsLegitimateV1
                                    pheader'operatorVkey
                                    operator
                                    schedulerOperator
                                    activeOperator
                                , pmatch mAnchorKey $ \case
                                    PDJust anchorHeaderHash ->
                                        pmatch mConfirmedStateRefInputIndex $ \case
                                            PDNothing -> perror
                                            PDJust confirmedStateRefInputIndex -> P.do
                                                headNode <-
                                                    plet $
                                                        pauthenticatedQueueHeadForAppendV1
                                                            referenceInputs
                                                            ownPolicyId
                                                            (pfromData anchorHeaderHash)
                                                            anchorData
                                                            (pfromData confirmedStateRefInputIndex)
                                                            mHeadStateQueueNodeRefInputIndex
                                                pmatch (punsafeCoerceData @PStateQueueNode anchorData) $
                                                    \PStateQueueNode{pstateQueueNode'header = anchorHeaderData} ->
                                                        pstateQueueHeadAllowsAppendV1
                                                            headNode
                                                            daAttestationPolicyId
                                                            validityRange
                                                            #&& pcommitBlockHeaderCarriesPreviousBlockV1
                                                                outputHeader
                                                                (pfromData anchorHeaderHash)
                                                                (pfromData (pdecodeHeaderView # anchorHeaderData))
                                    PDNothing ->
                                        pmatch mConfirmedStateRefInputIndex $ \case
                                            PDJust _ -> perror
                                            PDNothing ->
                                                pmatch mHeadStateQueueNodeRefInputIndex $ \case
                                                    PDJust _ -> perror
                                                    PDNothing ->
                                                        pcommitBlockHeaderCarriesConfirmedStateV1
                                                            outputHeader
                                                            (punsafeCoerceData @PConfirmedState anchorData)
                                ]
                    )
                    ownPolicyId
      where
        pcommitBoundHeaderTime st en vr =
            let (_, upper) = pgetInclusiveBoundsOfAShortValidityRange vr
             in (st #< en) #&& (en #== upper)

--------------------------------------------------------------------------------
-- RemoveFraudulentBlockHeader
--------------------------------------------------------------------------------

{- | Aiken @RemoveFraudulentBlockHeader@.

Three independent obligations, all required.

The operator must be losing its bond — either slashed out of the active set,
slashed out of the retired set, or shown by non-membership proofs to be in
neither, having been slashed already. In the first two cases the reason is read
back out of the operator set's own redeemer and must be @SlashOperatorForBadState@;
an operator being slashed for something else does not license removing its
blocks.

The block must actually leave the queue, and because a fraudulent block's
successors inherit its fraud, removal walks in from the tail: a successor can be
stripped without its own fraud proof, and only the last block is removed against
the proof itself.

And fraud must have been proved: a reference input carries a fraud-proof token
whose name is the hash of the block being removed.
-}
pvalidateRemoveFraudulentBlockHeader ::
    forall (s :: S).
    Term s (PAsData PCurrencySymbol) ->
    Term s (PAsData PCurrencySymbol) ->
    Term s (PAsData PCurrencySymbol) ->
    Term s (PAsData PCurrencySymbol) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s PMintValue ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
    Term s (PAsData PPubKeyHash) ->
    Term s PByteString ->
    Term s PSlashingApproach ->
    Term s PInteger ->
    Term s PBlockRemovalApproach ->
    Term s PBool
pvalidateRemoveFraudulentBlockHeader
    ownPolicyId
    activeOperatorsScriptHash
    retiredOperatorsScriptHash
    fraudProofScriptHash
    inputs
    outputs
    referenceInputs
    mint
    redeemers
    fraudulentOperator
    fraudulentBlocksHeaderHash
    slashingApproach
    fraudProofRefInputIndex
    blockRemovalApproach = P.do
        fraudProver <-
            plet $
                pgetProvenFraudRecord
                    referenceInputs
                    fraudProofScriptHash
                    fraudProofRefInputIndex
                    ( \referencedHeaderHash authenticatedFraudProver ->
                        pif
                            (referencedHeaderHash #== fraudulentBlocksHeaderHash)
                            authenticatedFraudProver
                            perror
                    )
        slashed <-
            plet $
                pmatch slashingApproach $ \case
                    PSlashActiveOperator
                        { pslashActive'activeOperatorsRedeemerIndex
                        , pslashActive'mFraudProverRewardOutputIndex
                        } ->
                            pisBadStateSlashing
                                ( Dir.pcrossValidateSlashingReason
                                    fraudulentOperator
                                    activeOperatorsScriptHash
                                    (pfromData pslashActive'activeOperatorsRedeemerIndex)
                                    ( \redeemerData ->
                                        pmatch (pfromData (punsafeCoerceRedeemer @Active.PMintRedeemer redeemerData)) $
                                            \case
                                                Active.PSlashOperator{Active.pactiveSlash'slashingArguments} ->
                                                    pfromData pactiveSlash'slashingArguments
                                                _ -> perror
                                    )
                                    redeemers
                                )
                                #&& prouteFraudProverRewardV1
                                    outputs
                                    (pfromData pslashActive'mFraudProverRewardOutputIndex)
                                    fraudProver
                                    Env.pfraudProverReward
                    PSlashRetiredOperator
                        { pslashRetired'retiredOperatorsRedeemerIndex
                        , pslashRetired'mFraudProverRewardOutputIndex
                        } ->
                            pisBadStateSlashing
                                ( Dir.pcrossValidateSlashingReason
                                    fraudulentOperator
                                    retiredOperatorsScriptHash
                                    (pfromData pslashRetired'retiredOperatorsRedeemerIndex)
                                    ( \redeemerData ->
                                        pmatch (pfromData (punsafeCoerceRedeemer @Retired.PMintRedeemer redeemerData)) $
                                            \case
                                                Retired.PSlashOperator{Retired.pretiredSlash'slashingArguments} ->
                                                    pfromData pretiredSlash'slashingArguments
                                                _ -> perror
                                    )
                                    redeemers
                                )
                                #&& prouteFraudProverRewardV1
                                    outputs
                                    (pfromData pslashRetired'mFraudProverRewardOutputIndex)
                                    fraudProver
                                    Env.pfraudProverReward
                    POperatorAlreadySlashed
                        { palreadySlashed'activeElementRefInputIndex
                        , palreadySlashed'retiredElementRefInputIndex
                        } ->
                            -- Neither set holds the operator, so it was slashed out of both
                            -- already; two non-membership proofs, one per set.
                            pnoOutputPaysFraudProverRewardV1 outputs fraudProver Env.pfraudProverReward
                                #&& Active.pfinalizeLinkedList
                                    ( Dir.poperatorIsNotAMember
                                        fraudulentOperator
                                        referenceInputs
                                        (pfromData palreadySlashed'activeElementRefInputIndex)
                                    )
                                    activeOperatorsScriptHash
                                #&& Retired.pfinalizeLinkedList
                                    ( Dir.poperatorIsNotAMember
                                        fraudulentOperator
                                        referenceInputs
                                        (pfromData palreadySlashed'retiredElementRefInputIndex)
                                    )
                                    retiredOperatorsScriptHash

        removed <-
            plet $
                pmatch blockRemovalApproach $ \case
                    PRemoveFraudulentBlocksLink
                        { premoveLink'fraudulentNodeInputOutref
                        , premoveLink'fraudulentNodeOutputIndex
                        } ->
                            premoveFraudulentBlocksLinkV1
                                ownPolicyId
                                inputs
                                outputs
                                mint
                                fraudulentBlocksHeaderHash
                                (pfromData premoveLink'fraudulentNodeInputOutref)
                                (pfromData premoveLink'fraudulentNodeOutputIndex)
                    PRemoveLastFraudulentBlock
                        { premoveLast'anchorElementInputOutref
                        , premoveLast'anchorElementOutputIndex
                        } ->
                            premoveLastFraudulentBlockV1
                                ownPolicyId
                                inputs
                                outputs
                                mint
                                fraudulentOperator
                                fraudulentBlocksHeaderHash
                                (pfromData premoveLast'anchorElementInputOutref)
                                (pfromData premoveLast'anchorElementOutputIndex)

        slashed #&& removed
      where
        pisBadStateSlashing reason =
            pmatch (pfromData reason) $ \case
                PSlashOperatorForBadState _ -> pconstant True
                _ -> perror

--------------------------------------------------------------------------------
-- MergeToConfirmedStateV1
--------------------------------------------------------------------------------

{- | Aiken @state_queue.merge_to_confirmed_state@.

Retires the oldest block into the confirmed state — the point at which a block
stops being disputable and becomes Midgard's settled history.

The conditions are, in order: the block is the one immediately after the root
(the linked-list fold enforces that), it is the block the redeemer names, it
carries a DA attestation, it is a valid v1 header, the redeemer's restated
commitments match it, it has matured, the confirmed state authenticates, the new
confirmed state is exactly the expected one, and the settlement binding is
right.

Two of those deserve attention.

/Maturity/ is @block_maturity_duration_v1@ — seven days — measured from the
block's own @end_time@ to the transaction's inclusive /lower/ bound. Using the
lower bound is what makes it a real wait: a transaction cannot claim maturity it
has not reached by widening its validity range downwards.

/The new confirmed state is constructed here and compared/, rather than being
checked field by field. It keeps the old state's @start_time@ — the confirmed
state covers everything since genesis, not just the merged block — while taking
the block's hash, utxo root, end time and protocol version. Building the
expected value and comparing whole leaves no field unchecked by omission.
-}
pvalidateMergeToConfirmedState ::
    forall (s :: S) (a :: S -> Type).
    Term s (PAsData PCurrencySymbol) ->
    Term s (PAsData PCurrencySymbol) ->
    Term s (PAsData PCurrencySymbol) ->
    Term s (PBuiltinList (PAsData PTxInInfo)) ->
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s PMintValue ->
    Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
    Term s a ->
    Term s PMintRedeemer ->
    Term s PBool
pvalidateMergeToConfirmedState
    ownPolicyId
    _daAttestationPolicyId
    settlementScriptHash
    inputs
    outputs
    mint
    redeemers
    validityRange
    redeemer =
        pmatch redeemer $ \case
            PMergeToConfirmedStateV1
                { psqMerge'headerNodeKey
                , psqMerge'confirmedStateInputOutref
                , psqMerge'confirmedStateOutputIndex
                , psqMerge'mSettlementRedeemerIndex
                } -> P.do
                    headerNodeKey <- plet $ pfromData psqMerge'headerNodeKey
                    contConfirmedStateOutput <-
                        plet $
                            pfromData
                                ( pelemAt
                                    # pfromData psqMerge'confirmedStateOutputIndex
                                    # outputs
                                )
                    pfinalizeLinkedList
                        ( pfoldFromRoot
                            (pfromData psqMerge'confirmedStateInputOutref)
                            contConfirmedStateOutput
                            inputs
                            mint
                            $ \_rootInput _rootLovelaceChange inputConfirmedStateData _headerInput _headerLovelace inputHeaderNodeKey inputHeaderNodeData _inputHeaderLink outputConfirmedStateData -> P.do
                                PStateQueueNode
                                    { pstateQueueNode'header = inputHeaderData
                                    , pstateQueueNode'daAttestation
                                    } <-
                                    pmatch (punsafeCoerceData @PStateQueueNode inputHeaderNodeData)
                                inputHeader <- plet $ pfromData (pdecodeHeaderView # inputHeaderData)
                                PHeaderV1
                                    { pheader'utxosRoot
                                    , pheader'endTime
                                    , pheader'protocolVersion
                                    } <-
                                    pmatch inputHeader
                                inputConfirmedState <-
                                    plet $ punsafeCoerceData @PConfirmedState inputConfirmedStateData
                                PConfirmedState
                                    { pconfirmed'headerHash = inputHeaderHash
                                    , pconfirmed'startTime = inputStartTime
                                    } <-
                                    pmatch inputConfirmedState
                                expectedOutputConfirmedState <-
                                    plet . pforgetData . pdata . pcon $
                                        PConfirmedState
                                            { pconfirmed'headerHash = pdata headerNodeKey
                                            , pconfirmed'prevHeaderHash = inputHeaderHash
                                            , pconfirmed'utxoRoot = pheader'utxosRoot
                                            , pconfirmed'startTime = inputStartTime
                                            , pconfirmed'endTime = pheader'endTime
                                            , pconfirmed'protocolVersion = pheader'protocolVersion
                                            }
                                settlementBinding <-
                                    plet $
                                        pif
                                            (pheaderCarriesL2Material # inputHeader)
                                            ( pmergeSettlementBindingMatchesHeader
                                                inputHeader
                                                headerNodeKey
                                                ( pmergeSettlementIdAtRoute
                                                    redeemers
                                                    settlementScriptHash
                                                    (pfromData psqMerge'mSettlementRedeemerIndex)
                                                )
                                            )
                                            -- An empty block must spawn no settlement at all, and
                                            -- must not even name a redeemer index.
                                            ( ( pfromData psqMerge'mSettlementRedeemerIndex
                                                    #== pcon PDNothing
                                              )
                                                #&& pmergeSettlementBindingMatchesHeader
                                                    inputHeader
                                                    headerNodeKey
                                                    (pcon PNothing)
                                            )
                                pand'List
                                    [ inputHeaderNodeKey #== headerNodeKey
                                    , pmatch (pfromData pstateQueueNode'daAttestation) $ \case
                                        Availability.PAttested _ -> pconstant True
                                        Availability.PPublished _ -> pconstant True
                                        _ -> pconstant False
                                    , pfromData pheader'protocolVersion #== pprotocolVersionV1
                                    , pheaderV1IsValid # inputHeader
                                    , pmergeCommitmentsMatchHeader inputHeader redeemer
                                    , (pfromData pheader'endTime + pblockMaturityDurationV1)
                                        #<= pgetInclusiveLowerBoundOfInterval
                                        # punsafeCoerce validityRange
                                    , pmatch
                                        (pconfirmedStateNextHeaderProtocolVersionV1 # inputConfirmedState)
                                        $ \case
                                            PNothing -> pconstant False
                                            PJust v -> v #== pprotocolVersionV1
                                    , outputConfirmedStateData #== expectedOutputConfirmedState
                                    , settlementBinding
                                    ]
                        )
                        ownPolicyId
            _ -> perror
