{- |
Module      : Midgard.Validators.RetiredOperators
Description : Plutarch port of @validators/operator-directory/retired-operators.ak@.

The retired operators set: a linked list an operator enters from the active set
and leaves once its bond unlock time has passed.
-}
module Midgard.Validators.RetiredOperators (
  retiredOperatorsSpendValidator,
  retiredOperatorsMintValidator,
) where

import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptContext (..),
  PPubKeyHash,
  PRedeemer,
  PScriptInfo (..),
  PTxInfo (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import LinkedList (premove, pspendForAddingOrRemovingAnElement)
import Midgard.Common.Utils (phasSigned, pisEntirelyAfter)
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Plutarch.LedgerApi.V3 (PScriptHash (..))
import Midgard.OperatorDirectory qualified as Dir
import Midgard.OperatorDirectory.ActiveOperators qualified as Active
import Midgard.OperatorDirectory.RetiredOperators (
  PMintRedeemer (..),
  PNodeData (..),
  pfinalizeLinkedList,
 )
import Midgard.OperatorDirectory.RetiredOperators qualified as RetiredOperators

{- | Aiken's @active_operators.RetireOperator@ reader.

@
fn(active_operators_redeemer_data) -> (VerificationKeyHash, Bool) {
  expect active_operators.RetireOperator { active_operator_key,
                                           penalize_for_inactivity, .. } = ...
  (active_operator_key, penalize_for_inactivity)
}
@

The retired set does not decide whether an inactivity penalty applies — the
active set does, and this reads that decision out of its redeemer. Any other
branch of the active redeemer is a rejection.
-}
pactiveRetireOperatorAndPenalty ::
  forall (s :: S).
  Term s (PAsData PRedeemer) ->
  (Term s (PAsData PPubKeyHash), Term s PBool)
pactiveRetireOperatorAndPenalty rdmr =
  let decoded =
        pfromData (punsafeCoerce @(PAsData Active.PMintRedeemer) (pto (pfromData rdmr)))
      operator = pmatch decoded $ \case
        Active.PRetireOperator {pactiveRetire'activeOperatorKey} ->
          pactiveRetire'activeOperatorKey
        _ -> perror
      penalize = pmatch decoded $ \case
        Active.PRetireOperator {pactiveRetire'penalizeForInactivity} ->
          pfromData pactiveRetire'penalizeForInactivity
        _ -> perror
   in (operator, penalize)

{- | The hub oracle's policy id reinterpreted as its script hash — the same 28
bytes, which Aiken conflates as @ByteArray@.
-}
pscriptHashOf ::
  forall (s :: S). Term s (PAsData PCurrencySymbol) -> Term s (PAsData PScriptHash)
pscriptHashOf cs = pdata (pcon (PScriptHash (pto (pfromData cs))))

{- | Aiken @validators/operator-directory/retired-operators.ak@ — @spend@.

@
linked_list.spend_for_adding_or_removing_an_element(
  retired_operators_mint_script_hash, self.mint)
@

The spend side is only a gate: it permits moving a list UTxO whenever the list
policy mints or burns, and leaves proving the transition to the minting policy
below. It is not standalone authorisation, and the datum and redeemer are
deliberately ignored.
-}
retiredOperatorsSpendValidator ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
retiredOperatorsSpendValidator = plam $ \mintScriptHash ctx -> P.do
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

{- | Aiken @validators/operator-directory/retired-operators.ak@ — @mint@.

Five branches:

  * @Init@ / @Deinit@ — create or tear down the set's root, gated on the hub
    oracle NFT being minted or burnt in the same transaction.
  * @RetireOperator@ — insert a node transferred from the active set. The
    operator and the inactivity-penalty flag both come from the /active/ set's
    own redeemer, and the new node's @bond_unlock_time@ must equal the one in
    this redeemer. That equality is what carries the unlock time across the
    transfer; the active set's @RetireOperator@ branch is what ties it to the
    active-set datum.
  * @RecoverOperatorBond@ — remove a node and release its bond. Requires the
    operator's signature, that the removed node's key is that operator, and
    that the validity range lies entirely after the unlock time. A
    'PDNothing' unlock time means no wait.
  * @SlashOperator@ — remove a node under the shared slashing logic.
-}
retiredOperatorsMintValidator ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
retiredOperatorsMintValidator = plam $ \hubOracleScriptHash ctx -> P.do
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
  mint <- plet $ pfromData ptxInfo'mint
  redeemerList <- plet $ pto (pto (pfromData ptxInfo'redeemers))

  redeemer <-
    plet $
      pfromData (punsafeCoerce @(PAsData PMintRedeemer) (pto pscriptContext'redeemer))

  pif
    ( pmatch redeemer $ \case
        PInit {pretiredInit'outputIndex} ->
          Dir.pinit
            hubOracleScriptHash
            ownPolicyId
            RetiredOperators.prootAssetName
            (pfromData pretiredInit'outputIndex)
            outputs
            mint
        PDeinit ->
          Dir.pdeinit
            hubOracleScriptHash
            ownPolicyId
            RetiredOperators.prootAssetName
            inputs
            mint
        PRetireOperator
          { pretire'newRetiredOperatorKey
          , pretire'bondUnlockTime
          , pretire'hubOracleRefInputIndex
          , pretire'anchorElementOutputIndex
          , pretire'insertedNodeOutputIndex
          , pretire'activeOperatorsRedeemerIndex
          } -> P.do
            PHubOracleDatum {phubOracle'activeOperators} <-
              pmatch $
                Hub.pgetDatum
                  # pfromData ptxInfo'referenceInputs
                  # pscriptHashOf hubOracleScriptHash
                  # pfromData pretire'hubOracleRefInputIndex
            pfinalizeLinkedList
              ( Dir.pvalidateTransferredOperatorInsertion
                  pretire'newRetiredOperatorKey
                  phubOracle'activeOperators
                  (pfromData pretire'activeOperatorsRedeemerIndex)
                  pactiveRetireOperatorAndPenalty
                  (pfromData pretire'anchorElementOutputIndex)
                  (pfromData pretire'insertedNodeOutputIndex)
                  inputs
                  outputs
                  mint
                  redeemerList
                  ( \insertedNodeData _anchorKey _link ->
                      -- The unlock time must survive the transfer intact.
                      insertedNodeData
                        #== pforgetData
                          (pdata (pcon (PNodeData pretire'bondUnlockTime)))
                  )
              )
              ownPolicyId
        PRecoverOperatorBond
          { precover'retiredOperatorKey
          , precover'anchorElementInputOutref
          , precover'anchorElementOutputIndex
          } ->
            pand'List
              [ phasSigned # precover'retiredOperatorKey # pfromData ptxInfo'signatories
              , pfinalizeLinkedList
                  ( premove
                      (pfromData precover'anchorElementInputOutref)
                      (pfromData (pelemAt # pfromData precover'anchorElementOutputIndex # outputs))
                      inputs
                      mint
                      ( \_anchorInput anchorLovelaceChange _anchorKey _anchorData _removedInput _removedLovelace removedKey removedData _removedLink ->
                          pand'List
                            [ anchorLovelaceChange #>= 0
                            , removedKey #== pto (pfromData precover'retiredOperatorKey)
                            , pmatch
                                (pfromData (punsafeCoerce @(PAsData PNodeData) removedData))
                                $ \(PNodeData unlock) ->
                                  pmatch unlock $ \case
                                    PDNothing -> pconstant True
                                    PDJust unlockTime ->
                                      pisEntirelyAfter
                                        # ptxInfo'validRange
                                        # pfromData unlockTime
                            ]
                      )
                  )
                  ownPolicyId
              ]
        PSlashOperator {pretiredSlash'slashingArguments} ->
          pfinalizeLinkedList
            ( Dir.pslashFraudulentOperatorAndGetInfo
                hubOracleScriptHash
                (pfromData pretiredSlash'slashingArguments)
                inputs
                outputs
                (pfromData ptxInfo'referenceInputs)
                mint
                (pto (pfromData ptxInfo'fee))
                redeemerList
                (\_op _anchorKey _link _hub -> pconstant True)
            )
            ownPolicyId
    )
    (pconstant ())
    perror
