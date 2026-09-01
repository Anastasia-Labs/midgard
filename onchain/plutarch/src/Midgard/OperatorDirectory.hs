{- |
Module      : Midgard.OperatorDirectory
Description : Partial Plutarch port of @lib/midgard/operator-directory.ak@.

The three operator sets — registered, active, retired — are each a linked list
keyed by operator verification-key hash. This module holds what they share:
list initialisation and teardown, non-membership proofs, and the generic half of
transferring an operator from one set to the next.

The whole Aiken module is now ported.
-}
module Midgard.OperatorDirectory (
  PSlashingReason (..),
  PSlashingArguments (..),
  pinit,
  pdeinit,
  poperatorIsNotAMember,
  pvalidateTransferredOperatorInsertion,
  pslashFraudulentOperatorAndGetInfo,
  pcrossValidateSlashingReason,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PMintValue,
  PPubKeyHash,
  PRedeemer,
  PScriptHash (..),
  PScriptPurpose (..),
  PTokenName,
  PTxInInfo (..),
  PTxOut,
  PTxOutRef,
 )
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Monadic qualified as P
import Data.Kind (Type)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import LinkedList qualified
import LinkedList (pgetElementInfo, pinsertAscending, premove)
import LinkedList.Types (PLink, PNodeKey, PRootKey)
import Midgard.Common.Utils (pgetRedeemerAt, pgetSpendingRedeemerDataAt)
import Midgard.Env qualified as Env
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.Settlement qualified as Settlement
import Midgard.StateQueue qualified as StateQueue

{- | Aiken @operator_directory.SlashingReason@.

Constructor order fixes the tag: @SlashOperatorForBadState@ is 0,
@SlashOperatorForBadSettlement@ is 1.
-}
data PSlashingReason (s :: S)
  = PSlashOperatorForBadState
      {pslashBadState'stateQueueRedeemerIndex :: Term s (PAsData PInteger)}
  | PSlashOperatorForBadSettlement
      { pslashBadSettlement'settlementInputIndex :: Term s (PAsData PInteger)
      , pslashBadSettlement'settlementRedeemerIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSlashingReason)

-- | Aiken @operator_directory.SlashingArguments@.
data PSlashingArguments (s :: S) = PSlashingArguments
  { pslashArgs'slashedOperator :: Term s (PAsData PPubKeyHash)
  , pslashArgs'hubOracleRefInputIndex :: Term s (PAsData PInteger)
  , pslashArgs'anchorElementInputOutref :: Term s (PAsData PTxOutRef)
  , pslashArgs'anchorElementOutputIndex :: Term s (PAsData PInteger)
  , pslashArgs'slashingReason :: Term s (PAsData PSlashingReason)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSlashingArguments)

{- | Aiken @operator_directory.init@.

Creates a directory's root. The one-time authorisation is the hub oracle NFT
being minted in the same transaction — the hub oracle is itself a one-shot, so
each directory root can only be created at genesis alongside it.

The root's payload must be exactly @env.empty_data@: a directory root carries no
application data, only the link.
-}
pinit ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PRootKey) ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s PMintValue ->
  Term s PBool
pinit hubOracleScriptHash directoryPolicyId rootAssetName outputIndex outputs mint =
  LinkedList.pinit
    ( Value.pvalueOf # pto mint # pfromData hubOracleScriptHash # pfromData Hub.passetName
        #== 1
    )
    (pfromData (pelemAt # outputIndex # outputs))
    mint
    (\_addr _lovelace rootData -> rootData #== Env.pemptyData)
    directoryPolicyId
    rootAssetName

{- | Aiken @operator_directory.deinit@.

Tears a directory down. Requires the hub oracle NFT to be burnt in the same
transaction, which ties every directory's lifetime to the hub's; beyond that the
linked-list teardown carries the conditions (the root must be the only element).
-}
pdeinit ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PRootKey) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PMintValue ->
  Term s PBool
pdeinit hubOracleScriptHash directoryPolicyId rootAssetName inputs mint =
  pif
    ( Value.pvalueOf # pto mint # pfromData hubOracleScriptHash # pfromData Hub.passetName
        #== -1
    )
    ( LinkedList.pdeinit
        inputs
        mint
        (\_in _lovelace _rootData -> pconstant True)
        directoryPolicyId
        rootAssetName
    )
    perror

{- | Aiken @operator_directory.operator_is_not_a_member@.

A non-membership proof: the operator's key must fall strictly between the key of
the referenced element and the key it links to. Because the directory is a
sorted ascending list, a gap that straddles the operator proves the operator is
absent.

An absent neighbour is a list boundary and is accepted, so an operator below the
first node or above the last is proven absent too.
-}
poperatorIsNotAMember ::
  forall (s :: S).
  Term s (PAsData PPubKeyHash) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PInteger ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PRootKey) ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PBool
poperatorIsNotAMember operator referenceInputs elementRefInputIndex = P.do
  \listPolicy rootKey nodePrefix nodePrefixLen -> P.do
    elementInput <- plet $ pelemAt # elementRefInputIndex # referenceInputs
    PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData elementInput
    operatorBytes <- plet $ pto (pfromData operator)
    pgetElementInfo
      ptxInInfo'resolved
      ( \_addr _lovelace mSmallerKey _data largerLink ->
          pand'List
            [ pmatch mSmallerKey $ \case
                PDNothing -> pconstant True
                PDJust smaller -> pfromData smaller #< operatorBytes
            , pmatch largerLink $ \case
                PDNothing -> pconstant True
                PDJust larger -> operatorBytes #< pfromData larger
            ]
      )
      listPolicy
      rootKey
      nodePrefix
      nodePrefixLen

{- | Aiken @operator_directory.validate_transferred_operator_insertion@.

The generic half of moving an operator between sets — registered → active, or
active → retired. The origin set's minting redeemer is read out of the
transaction and must name the same operator; the destination insertion is a
sorted insert keyed by that operator; and the bond must carry across.

@must_penalize@ comes from the origin redeemer, not from this caller: an
operator retired for inactivity keeps its bond less the inactivity penalty,
while a voluntary retirement keeps it whole. Note that in the @default@
environment both constants are zero, so neither check bites as configured.
-}
pvalidateTransferredOperatorInsertion ::
  forall (s :: S).
  Term s (PAsData PPubKeyHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  (Term s (PAsData PRedeemer) -> (Term s (PAsData PPubKeyHash), Term s PBool)) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s PMintValue ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  ( Term s PData ->
    Term s (PMaybeData PNodeKey) ->
    Term s PLink ->
    Term s PBool
  ) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PRootKey) ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PBool
pvalidateTransferredOperatorInsertion
  operator
  originListPolicyId
  originListRedeemerIndex
  originRedeemerToOperatorAndPenaltyFlag
  targetAnchorOutputIndex
  targetInsertedNodeOutputIndex
  inputs
  outputs
  mint
  redeemers
  validateTargetInsertion =
    \listPolicy rootKey nodePrefix nodePrefixLen -> P.do
      originRedeemer <-
        plet $
          pgetRedeemerAt
            # redeemers
            # pdata (pcon (PMinting originListPolicyId))
            # originListRedeemerIndex
      let (operatorFromOrigin, mustPenalize) =
            originRedeemerToOperatorAndPenaltyFlag originRedeemer
      anchorOutput <- plet $ pfromData (pelemAt # targetAnchorOutputIndex # outputs)
      insertedNodeOutput <-
        plet $ pfromData (pelemAt # targetInsertedNodeOutputIndex # outputs)
      pif
        (operator #== operatorFromOrigin)
        ( pinsertAscending
            anchorOutput
            insertedNodeOutput
            inputs
            mint
            ( \_anchorInput anchorLovelaceChange mAnchorKey _anchorData insertedLovelace insertedKey insertedData insertedLink ->
                pand'List
                  [ anchorLovelaceChange #>= 0
                  , pif
                      mustPenalize
                      (insertedLovelace #>= Env.prequiredBond - Env.pinactivitySlashingPenalty)
                      (insertedLovelace #>= Env.prequiredBond)
                  , insertedKey #== pto (pfromData operator)
                  , validateTargetInsertion insertedData mAnchorKey insertedLink
                  ]
            )
            listPolicy
            rootKey
            nodePrefix
            nodePrefixLen
        )
        perror

{- | Aiken @operator_directory.slash_fraudulent_operator_and_get_info@.

Removes a fraudulent operator's node from the directory and checks the penalty
was paid, then hands the caller everything it needs for the set-specific part.

The penalty is paid as /transaction fee/, not to an output — it goes to the
Cardano treasury. The fraud prover is assumed to be the transaction's signer and
so collects the remainder of the bond, which is why nothing here checks where
the bond goes.

The two-branch tail is the cross-validation: slashing must be justified either
by the state queue removing the operator's fraudulent block header, or by a
settlement UTxO being spent to disprove its resolution claim. Either way the
operator named in that other script's redeemer must be the one being slashed
here, which is what stops a caller slashing an operator on someone else's
evidence.

Note the asymmetry between the branches, preserved from the original: the
state-queue side is found by /minting policy/ from the hub datum, while the
settlement side is found by /address/ and input index. They are different script
purposes, so they are located differently.
-}
pslashFraudulentOperatorAndGetInfo ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s PSlashingArguments ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PMintValue ->
  Term s PInteger ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  ( Term s (PAsData PPubKeyHash) ->
    Term s (PMaybeData PNodeKey) ->
    Term s PLink ->
    Term s PHubOracleDatum ->
    Term s PBool
  ) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PRootKey) ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PBool
pslashFraudulentOperatorAndGetInfo
  hubOracleScriptHash
  slashingArguments
  inputs
  outputs
  referenceInputs
  mint
  fee
  redeemers
  furtherValidations =
    \listPolicy rootKey nodePrefix nodePrefixLen -> P.do
      PSlashingArguments
        { pslashArgs'slashedOperator
        , pslashArgs'hubOracleRefInputIndex
        , pslashArgs'anchorElementInputOutref
        , pslashArgs'anchorElementOutputIndex
        , pslashArgs'slashingReason
        } <-
        pmatch slashingArguments
      hubDatum <-
        plet $
          Hub.pgetDatum
            # referenceInputs
            # pcurrencySymbolAsScriptHash hubOracleScriptHash
            # pfromData pslashArgs'hubOracleRefInputIndex
      PHubOracleDatum {phubOracle'stateQueue, phubOracle'settlementAddr} <- pmatch hubDatum
      anchorOutput <-
        plet $ pfromData (pelemAt # pfromData pslashArgs'anchorElementOutputIndex # outputs)
      premove
        (pfromData pslashArgs'anchorElementInputOutref)
        anchorOutput
        inputs
        mint
        ( \_anchorInput anchorLovelaceChange mAnchorKey _anchorData _removedInput removedLovelace removedKey _removedData removedLink ->
            pand'List
              [ anchorLovelaceChange #>= 0
              , removedKey #== pto (pfromData pslashArgs'slashedOperator)
              , -- Partial slashing is already paid as fee, so a node that has
                -- less than the full bond owes only the remainder.
                pif
                  (removedLovelace #< Env.prequiredBond)
                  (fee #>= Env.pslashingPenalty - Env.pinactivitySlashingPenalty)
                  (fee #>= Env.pslashingPenalty)
              , furtherValidations pslashArgs'slashedOperator mAnchorKey removedLink hubDatum
              , pmatch (pfromData pslashArgs'slashingReason) $ \case
                  PSlashOperatorForBadState {pslashBadState'stateQueueRedeemerIndex} ->
                    pmatch
                      ( pfromData
                          ( punsafeCoerceRedeemer @StateQueue.PMintRedeemer
                              ( pgetRedeemerAt
                                  # redeemers
                                  # pdata (pcon (PMinting phubOracle'stateQueue))
                                  # pfromData pslashBadState'stateQueueRedeemerIndex
                              )
                          )
                      )
                      $ \case
                        StateQueue.PRemoveFraudulentBlockHeader {psqRemove'fraudulentOperator} ->
                          psqRemove'fraudulentOperator #== pslashArgs'slashedOperator
                        _ -> perror
                  PSlashOperatorForBadSettlement
                    { pslashBadSettlement'settlementInputIndex
                    , pslashBadSettlement'settlementRedeemerIndex
                    } ->
                      pmatch
                        ( pfromData
                            ( punsafeCoerceRedeemer @Settlement.PSpendRedeemer
                                ( pgetSpendingRedeemerDataAt
                                    # pfromData phubOracle'settlementAddr
                                    # pfromData pslashBadSettlement'settlementInputIndex
                                    # pfromData pslashBadSettlement'settlementRedeemerIndex
                                    # inputs
                                    # redeemers
                                )
                            )
                        )
                        $ \case
                          Settlement.PDisproveResolutionClaim {pstlDisprove'operator} ->
                            pstlDisprove'operator #== pslashArgs'slashedOperator
                          _ -> perror
              ]
        )
        listPolicy
        rootKey
        nodePrefix
        nodePrefixLen

{- | Aiken types the hub oracle's identifier as @PolicyId@ here and passes it to
@hub.get_datum@, which takes a @ScriptHash@ — both are @ByteArray@ in Aiken, and
for the hub oracle they are the same 28 bytes by construction. Plutarch keeps
the two apart, so the reinterpretation has to be written down.
-}
pcurrencySymbolAsScriptHash ::
  forall (s :: S). Term s (PAsData PCurrencySymbol) -> Term s (PAsData PScriptHash)
pcurrencySymbolAsScriptHash cs = pdata (pcon (PScriptHash (pto (pfromData cs))))

-- | Reinterpret another script's redeemer as a known type.
punsafeCoerceRedeemer ::
  forall (a :: S -> Type) (s :: S). Term s (PAsData PRedeemer) -> Term s (PAsData a)
punsafeCoerceRedeemer r = punsafeCoerce (pto (pfromData r))

{- | Aiken @operator_directory.cross_validate_slashing_reason@.

Reads the slashing arguments out of whichever operator set is doing the
slashing, checks that set names the same operator as the caller, and returns the
reason it gave.

The direction matters. A script that wants to know /why/ an operator is being
slashed must not decide that for itself — it reads the answer from the set that
is actually removing the operator, and the only thing it enforces is that the
two agree on /who/. That is what keeps a settlement and an operator set from
disagreeing about the grounds while both accepting the transaction.

@redeemer_to_slashing_arguments@ is supplied by the caller because each operator
set wraps 'PSlashingArguments' in its own redeemer constructor.
-}
pcrossValidateSlashingReason ::
  forall (s :: S).
  Term s (PAsData PPubKeyHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  (Term s (PAsData PRedeemer) -> Term s PSlashingArguments) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s (PAsData PSlashingReason)
pcrossValidateSlashingReason
  fraudulentOperator
  targetOperatorsSetPolicyId
  targetOperatorsSetRedeemerIndex
  redeemerToSlashingArguments
  redeemers = P.do
    PSlashingArguments {pslashArgs'slashedOperator, pslashArgs'slashingReason} <-
      pmatch $
        redeemerToSlashingArguments
          ( pgetRedeemerAt
              # redeemers
              # pdata (pcon (PMinting targetOperatorsSetPolicyId))
              # targetOperatorsSetRedeemerIndex
          )
    pif
      (fraudulentOperator #== pslashArgs'slashedOperator)
      pslashArgs'slashingReason
      perror
