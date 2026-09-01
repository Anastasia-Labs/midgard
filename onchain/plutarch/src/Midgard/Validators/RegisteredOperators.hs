{- |
Module      : Midgard.Validators.RegisteredOperators
Description : Plutarch port of
              @validators/operator-directory/registered-operators.ak@.

The registered operators set: the entry point to the operator directory. An
operator registers here, waits out @registration_duration@, and then activates
into the active set.

This is the only operator list keyed by activation time rather than by operator,
which shapes two of its branches. Registration inserts /descending/ by that key,
and activation reads the key back to check the wait has elapsed.
-}
module Midgard.Validators.RegisteredOperators (
  registeredOperatorsSpendValidator,
  registeredOperatorsMintValidator,
) where

import Data.Kind (Type)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PMintValue,
  PPubKeyHash,
  PRedeemer,
  PScriptContext (..),
  PScriptHash (..),
  PScriptInfo (..),
  PScriptPurpose (..),
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut,
  PTxOutRef,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import LinkedList (
  pgetElementInfo,
  pinsertDescending,
  premove,
  pspendForAddingOrRemovingAnElement,
 )
import LinkedList.Types (PLink)
import Midgard.Common.Utils (
  pgetInclusiveUpperBoundOfInterval,
  pgetRedeemerAt,
  phasSigned,
  pisEntirelyAfter,
 )
import Midgard.Env qualified as Env
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.OperatorDirectory qualified as Dir
import Midgard.OperatorDirectory.ActiveOperators qualified as Active
import Midgard.OperatorDirectory.RegisteredOperators (
  PDuplicateOperatorStatus (..),
  PMintRedeemer (..),
  PNodeData (..),
  pactivationTimeToNodeKey,
  pfinalizeLinkedList,
  pnodeKeyToActivationTime,
 )
import Midgard.OperatorDirectory.RegisteredOperators qualified as Registered
import Midgard.OperatorDirectory.RetiredOperators qualified as Retired

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

{- | Aiken @validators/operator-directory/registered-operators.ak@ — @spend@.

The same gate as the other two operator sets: moving a list UTxO is permitted
whenever the list policy mints or burns, and the minting policy below carries the
proof that the transition is legitimate.
-}
registeredOperatorsSpendValidator ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
registeredOperatorsSpendValidator = plam $ \mintScriptHash ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
  _ <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript outRef _ -> outRef
      _ -> perror
  PTxInfo {ptxInfo'mint} <- pmatch pscriptContext'txInfo
  pif
    (pspendForAddingOrRemovingAnElement # mintScriptHash # pfromData ptxInfo'mint)
    (pconstant ())
    perror

{- | Aiken @registered_operators.remove_operator_and_get_node_info@.

Three of the five mint branches remove a node, differing only in what they
additionally require of it, so the shared part is factored out exactly as in the
original: the removal must be a valid list operation, the anchor's Lovelace must
be preserved, and the removed node's data must name the expected operator.

@customValidator@ receives the removed node's key and link — the activation time
and the position in the list — which is what the activation branch needs and the
other two ignore.
-}
premoveOperatorAndGetNodeInfo ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PPubKeyHash) ->
  Term s PTxOutRef ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s PMintValue ->
  (Term s PByteString -> Term s PLink -> Term s PBool) ->
  Term s PBool
premoveOperatorAndGetNodeInfo
  ownPolicyId
  removedOperator
  anchorElementInputOutref
  anchorElementOutputIndex
  inputs
  outputs
  mint
  customValidator =
    pfinalizeLinkedList
      ( premove
          anchorElementInputOutref
          (pfromData (pelemAt # anchorElementOutputIndex # outputs))
          inputs
          mint
          ( \_anchorInput anchorLovelaceChange _mAnchorKey _anchorData _removedInput _removedLovelace removedKey removedData removedLink ->
              pand'List
                [ anchorLovelaceChange #>= 0
                , pmatch (pfromData (punsafeCoerce @(PAsData PNodeData) removedData)) $
                    \(PNodeData operator) -> operator #== removedOperator
                , customValidator removedKey removedLink
                ]
          )
      )
      ownPolicyId

{- | Aiken @validators/operator-directory/registered-operators.ak@ — @mint@.

Five branches:

  * @Init@ / @Deinit@ — create or tear down the set's root, gated on the hub
    oracle NFT.
  * @RegisterOperator@ — insert a new node for an operator that is in neither
    the active nor the retired set. The node's key is the operator's activation
    time: @registration_duration@ past the inclusive upper bound of the
    transaction's validity range. Because that upper bound is the /operator's
    own claim/, it can be set later than necessary; the original inserts
    descending rather than requiring a prepend so that an operator inflating its
    own activation time cannot delay the operators registering behind it.
  * @ActivateOperator@ — remove the node and let the active set add it. The
    activation time must have passed, or — if the active set is empty and this
    is the earliest registered operator — activation is immediate, which is what
    restores liveness when the protocol has no active operators at all.
  * @DeregisterOperator@ — the operator withdraws before activating; needs only
    its signature.
  * @SlashDuplicateOperator@ — remove an operator that already appears in one of
    the three sets, paying the penalty as fee. The referenced duplicate must be
    authenticated against whichever set it is claimed to be in, which is why the
    branch ends in three different namespaces.
-}
registeredOperatorsMintValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
registeredOperatorsMintValidator =
  plam $ \retiredOperatorsMintScriptHash hubOracleScriptHash ctx -> P.do
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
      , ptxInfo'validRange
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
          PInit {pregisteredInit'outputIndex} ->
            Dir.pinit
              hubOracleScriptHash
              ownPolicyId
              Registered.prootAssetName
              (pfromData pregisteredInit'outputIndex)
              outputs
              mint
          PDeinit ->
            Dir.pdeinit
              hubOracleScriptHash
              ownPolicyId
              Registered.prootAssetName
              inputs
              mint
          PRegisterOperator
            { pregister'registeringOperator
            , pregister'rootOutputIndex
            , pregister'registeredNodeOutputIndex
            , pregister'hubOracleRefInputIndex
            , pregister'activeOperatorsElementRefInputIndex
            , pregister'retiredOperatorsElementRefInputIndex
            } -> P.do
              PHubOracleDatum {phubOracle'activeOperators} <-
                pmatch $
                  Hub.pgetDatum
                    # referenceInputs
                    # pscriptHashOf hubOracleScriptHash
                    # pfromData pregister'hubOracleRefInputIndex
              pand'List
                [ phasSigned # pregister'registeringOperator # pfromData ptxInfo'signatories
                , Active.pfinalizeLinkedList
                    ( Dir.poperatorIsNotAMember
                        pregister'registeringOperator
                        referenceInputs
                        (pfromData pregister'activeOperatorsElementRefInputIndex)
                    )
                    phubOracle'activeOperators
                , pfinalizeLinkedList
                    ( pinsertDescending
                        (pfromData (pelemAt # pfromData pregister'rootOutputIndex # outputs))
                        (pfromData (pelemAt # pfromData pregister'registeredNodeOutputIndex # outputs))
                        inputs
                        mint
                        ( \_anchorInput anchorLovelaceChange _mAnchorKey _anchorData insertedLovelace insertedKey insertedData _insertedLink ->
                            pand'List
                              [ anchorLovelaceChange #>= 0
                              , insertedLovelace #>= Env.prequiredBond
                              , insertedKey
                                  #== ( pactivationTimeToNodeKey
                                          #$ Env.pregistrationDuration
                                          + (pgetInclusiveUpperBoundOfInterval # ptxInfo'validRange)
                                      )
                              , Retired.pfinalizeLinkedList
                                  ( Dir.poperatorIsNotAMember
                                      pregister'registeringOperator
                                      referenceInputs
                                      (pfromData pregister'retiredOperatorsElementRefInputIndex)
                                  )
                                  retiredOperatorsMintScriptHash
                              , insertedData
                                  #== pforgetData
                                    (pdata (pcon (PNodeData pregister'registeringOperator)))
                              ]
                        )
                    )
                    ownPolicyId
                ]
          PActivateOperator
            { pactivate'activatingOperator
            , pactivate'anchorElementInputOutref
            , pactivate'anchorElementOutputIndex
            , pactivate'hubOracleRefInputIndex
            , pactivate'retiredOperatorsElementRefInputIndex
            , pactivate'activeOperatorsRedeemerIndex
            } -> P.do
              PHubOracleDatum {phubOracle'activeOperators} <-
                pmatch $
                  Hub.pgetDatum
                    # referenceInputs
                    # pscriptHashOf hubOracleScriptHash
                    # pfromData pactivate'hubOracleRefInputIndex
              activeRedeemer <-
                plet $
                  pfromData
                    ( punsafeCoerceRedeemer @Active.PMintRedeemer
                        ( pgetRedeemerAt
                            # redeemerList
                            # pdata (pcon (PMinting phubOracle'activeOperators))
                            # pfromData pactivate'activeOperatorsRedeemerIndex
                        )
                    )
              -- The active set's own redeemer names the operator it is adding
              -- and says whether it was empty beforehand. Both are read here,
              -- and the active set is what makes them true.
              activatingKeyMatches <-
                plet $ pmatch activeRedeemer $ \case
                  Active.PActivateOperator {pactivate'newActiveOperatorKey} ->
                    pactivate'newActiveOperatorKey #== pactivate'activatingOperator
                  _ -> perror
              setWasEmpty <-
                plet $ pmatch activeRedeemer $ \case
                  Active.PActivateOperator {pactivate'activeOperatorsSetWasEmpty} ->
                    pfromData pactivate'activeOperatorsSetWasEmpty
                  _ -> perror
              pand'List
                [ activatingKeyMatches
                , Retired.pfinalizeLinkedList
                    ( Dir.poperatorIsNotAMember
                        pactivate'activatingOperator
                        referenceInputs
                        (pfromData pactivate'retiredOperatorsElementRefInputIndex)
                    )
                    retiredOperatorsMintScriptHash
                , premoveOperatorAndGetNodeInfo
                    ownPolicyId
                    pactivate'activatingOperator
                    (pfromData pactivate'anchorElementInputOutref)
                    (pfromData pactivate'anchorElementOutputIndex)
                    inputs
                    outputs
                    mint
                    ( \removedNodeKey removedNodeLink ->
                        ( pisEntirelyAfter
                            # ptxInfo'validRange
                            # (pnodeKeyToActivationTime # removedNodeKey)
                        )
                          #|| pand'List
                            [ -- Descending order by activation time means a node
                              -- with no link is the earliest registered one.
                              pmatch removedNodeLink $ \case
                                PDNothing -> pconstant True
                                PDJust _ -> pconstant False
                            , setWasEmpty
                            ]
                    )
                ]
          PDeregisterOperator
            { pderegister'deregisteringOperator
            , pderegister'anchorElementInputOutref
            , pderegister'anchorElementOutputIndex
            } ->
              pand'List
                [ phasSigned
                    # pderegister'deregisteringOperator
                    # pfromData ptxInfo'signatories
                , premoveOperatorAndGetNodeInfo
                    ownPolicyId
                    pderegister'deregisteringOperator
                    (pfromData pderegister'anchorElementInputOutref)
                    (pfromData pderegister'anchorElementOutputIndex)
                    inputs
                    outputs
                    mint
                    (\_removedNodeKey _removedNodeLink -> pconstant True)
                ]
          PSlashDuplicateOperator
            { pslashDuplicate'duplicateOperator
            , pslashDuplicate'anchorElementInputOutref
            , pslashDuplicate'anchorElementOutputIndex
            , pslashDuplicate'duplicateNodeRefInputIndex
            , pslashDuplicate'duplicateOperatorStatus
            } ->
              pand'List
                [ -- The penalty is paid as transaction fee, to the treasury.
                  -- The remainder of the bond goes wherever the signer sends
                  -- it, which is the incentive to report duplicates.
                  pto (pfromData ptxInfo'fee) #>= Env.pslashingPenalty
                , premoveOperatorAndGetNodeInfo
                    ownPolicyId
                    pslashDuplicate'duplicateOperator
                    (pfromData pslashDuplicate'anchorElementInputOutref)
                    (pfromData pslashDuplicate'anchorElementOutputIndex)
                    inputs
                    outputs
                    mint
                    ( \_removedNodeKey _removedNodeLink -> P.do
                        PTxInInfo {ptxInInfo'resolved = duplicateNodeRefUtxo} <-
                          pmatch $
                            pfromData
                              ( pelemAt
                                  # pfromData pslashDuplicate'duplicateNodeRefInputIndex
                                  # referenceInputs
                              )
                        pmatch (pfromData pslashDuplicate'duplicateOperatorStatus) $ \case
                          -- Registered: the operator lives in the node's data.
                          PDuplicateIsRegistered ->
                            pfinalizeLinkedList
                              ( pgetElementInfo
                                  duplicateNodeRefUtxo
                                  ( \_addr _lovelace _mKey nodeData _link ->
                                      pmatch
                                        (pfromData (punsafeCoerce @(PAsData PNodeData) nodeData))
                                        $ \(PNodeData operator) ->
                                          operator #== pslashDuplicate'duplicateOperator
                                  )
                              )
                              ownPolicyId
                          -- Active and retired: the operator is the node's key,
                          -- so a root element can never match.
                          PDuplicateIsActive {pduplicateActive'hubOracleRefInputIndex} -> P.do
                            PHubOracleDatum {phubOracle'activeOperators} <-
                              pmatch $
                                Hub.pgetDatum
                                  # referenceInputs
                                  # pscriptHashOf hubOracleScriptHash
                                  # pfromData pduplicateActive'hubOracleRefInputIndex
                            Active.pfinalizeLinkedList
                              ( pgetElementInfo
                                  duplicateNodeRefUtxo
                                  (pkeyIs pslashDuplicate'duplicateOperator)
                              )
                              phubOracle'activeOperators
                          PDuplicateIsRetired ->
                            Retired.pfinalizeLinkedList
                              ( pgetElementInfo
                                  duplicateNodeRefUtxo
                                  (pkeyIs pslashDuplicate'duplicateOperator)
                              )
                              retiredOperatorsMintScriptHash
                    )
                ]
      )
      (pconstant ())
      perror

{- | The @expect Some(k) = m_key; expect k == operator@ shared by the active and
retired arms of @SlashDuplicateOperator@. 'PDNothing' means the referenced
element is the list root, which is never a match.
-}
pkeyIs ::
  forall (s :: S) (a :: S -> Type) (b :: S -> Type) (c :: S -> Type).
  Term s (PAsData PPubKeyHash) ->
  Term s a ->
  Term s b ->
  Term s (PMaybeData PByteString) ->
  Term s c ->
  Term s PLink ->
  Term s PBool
pkeyIs operator _addr _lovelace mKey _nodeData _link =
  pmatch mKey $ \case
    PDNothing -> perror
    PDJust key -> pfromData key #== pto (pfromData operator)
