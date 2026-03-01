module Midgard.Contracts.ActiveOperators (initActiveOperators, activateOperator) where

import Cardano.Api qualified as C
import Convex.BuildTx (
  MonadBuildTx,
  addReference,
  addRequiredSignature,
  assetValue,
  mintPlutus,
  mintPlutusWithRedeemerFn,
  payToScriptInlineDatum,
  spendPlutusInlineDatum,
 )
import Convex.Class (MonadBlockchain (queryNetworkId), MonadUtxoQuery, utxosByPaymentCredential)

import Control.Monad.Except (MonadError (throwError))
import Convex.Utxos (toTxOut)
import Midgard.Contracts.Utils (findTxInNonMembership, findUtxoWithAsset, findUtxoWithLink, nextOutIx)
import Midgard.ScriptUtils (mintingPolicyId, toMintingPolicy, toValidator, validatorHash)
import Midgard.Scripts (
  MidgardScripts (
    MidgardScripts,
    activeOperatorsPolicy,
    activeOperatorsValidator,
    registeredOperatorsPolicy,
    registeredOperatorsValidator,
    retiredOperatorsPolicy,
    retiredOperatorsValidator
  ),
 )
import Midgard.Types.ActiveOperators qualified as ActiveOperators
import Midgard.Types.LinkedList qualified as LinkedList
import Midgard.Types.RegisteredOperators qualified as RegisteredOperators

initActiveOperators ::
  ( C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  , C.IsBabbageBasedEra era
  ) =>
  C.NetworkId ->
  MidgardScripts ->
  m ()
initActiveOperators netId MidgardScripts {activeOperatorsValidator, activeOperatorsPolicy} = do
  let C.PolicyId policyId = mintingPolicyId activeOperatorsPolicy
  -- The active operators token should be minted.
  mintPlutusWithRedeemerFn
    (toMintingPolicy activeOperatorsPolicy)
    (\txBody -> ActiveOperators.Init {outputIndex = toInteger $ nextOutIx txBody})
    ActiveOperators.rootKey
    1
  -- And sent to the active operators validator.
  let datum :: ActiveOperators.Datum =
        LinkedList.Element
          { elementData = LinkedList.Root mempty
          , elementLink = Nothing
          }
  payToScriptInlineDatum
    netId
    (validatorHash activeOperatorsValidator)
    datum
    C.NoStakeAddress
    -- Must manually add min ada deposit...
    (assetValue policyId ActiveOperators.rootKey 1 <> C.lovelaceToValue 3_000_000)

activateOperator ::
  forall era m.
  ( MonadError String m
  , MonadBlockchain era m
  , MonadUtxoQuery m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  , C.IsBabbageBasedEra era
  ) =>
  MidgardScripts -> C.Hash C.PaymentKey -> m ()
activateOperator
  MidgardScripts
    { registeredOperatorsValidator
    , registeredOperatorsPolicy
    , activeOperatorsValidator
    , activeOperatorsPolicy
    , retiredOperatorsValidator
    , retiredOperatorsPolicy
    }
  operatorPkh = do
    let removalNodeKey = C.UnsafeAssetName $ C.serialiseToRawBytes RegisteredOperators.nodeKeyPrefix <> C.serialiseToRawBytes operatorPkh
        targetNodeKey = C.UnsafeAssetName $ C.serialiseToRawBytes ActiveOperators.nodeKeyPrefix <> C.serialiseToRawBytes operatorPkh
    netId <- queryNetworkId
    registryUtxos <- utxosByPaymentCredential $ C.PaymentCredentialByScript $ validatorHash registeredOperatorsValidator
    (removalRegistryTxIn, (removalUtxoAnyEra, _)) <-
      maybe (throwError "No registered operator found") pure $
        findUtxoWithAsset registryUtxos $
          C.AssetId (mintingPolicyId registeredOperatorsPolicy) removalNodeKey
    (anchorRegistryTxIn, anchorUtxoAnyEra) <-
      maybe (throwError "No anchor utxo found") pure $
        findUtxoWithLink registryUtxos (mintingPolicyId registeredOperatorsPolicy) $
          C.serialiseToRawBytes operatorPkh
    retiredOperatorsUtxos <- utxosByPaymentCredential $ C.PaymentCredentialByScript $ validatorHash retiredOperatorsValidator
    retiredOperatorsNonMemberWitness <-
      maybe (throwError "Operator already exists in retired operator set") pure $
        findTxInNonMembership retiredOperatorsUtxos (mintingPolicyId retiredOperatorsPolicy) $
          C.serialiseToRawBytes operatorPkh
    let C.TxOut _ anchorValue _ _ = toTxOut @era anchorUtxoAnyEra
        C.TxOut _ removalValue _ _ = toTxOut @era removalUtxoAnyEra
    addRequiredSignature operatorPkh
    addReference retiredOperatorsNonMemberWitness
    -- Remove the operator.
    spendPlutusInlineDatum removalRegistryTxIn (toValidator registeredOperatorsValidator) ()
    -- Modify the anchor. TODO: Update the datum.
    spendPlutusInlineDatum anchorRegistryTxIn (toValidator registeredOperatorsValidator) ()
    payToScriptInlineDatum
      netId
      (validatorHash registeredOperatorsValidator)
      ()
      C.NoStakeAddress
      (C.txOutValueToValue anchorValue)
    -- Burn the operator NFT
    mintPlutus (toMintingPolicy registeredOperatorsPolicy) () removalNodeKey (-1)
    -- Add it to the active operators linked list. TODO: Add the correct redeemer.
    -- TODO: Add the proper datum (i.e find the link to insert).
    payToScriptInlineDatum
      netId
      (validatorHash activeOperatorsValidator)
      ()
      C.NoStakeAddress
      (C.txOutValueToValue removalValue)
    -- TODO: Add validity range.
    mintPlutus (toMintingPolicy activeOperatorsPolicy) () targetNodeKey 1
