module Midgard.Contracts.ActiveOperators (initActiveOperators, activateOperator) where

import Cardano.Api qualified as C
import Convex.BuildTx (
  MonadBuildTx,
  addReference,
  addRequiredSignature,
  assetValue,
  mintPlutus,
  payToScriptInlineDatum,
  spendPlutusInlineDatum,
 )
import Convex.Class (MonadBlockchain (queryNetworkId), MonadUtxoQuery, utxosByPaymentCredential)

import Control.Monad.Except (MonadError (throwError))
import Convex.Utxos (toTxOut)
import Midgard.Contracts.Utils (findTxInNonMembership, findUtxoWithAsset, findUtxoWithLink)
import Midgard.ScriptUtils (mintingPolicyId, toMintingPolicy, toValidator, validatorHash)
import Midgard.Scripts (MidgardScripts (MidgardScripts, activeOperatorsPolicy, activeOperatorsValidator, registeredOperatorsPolicy, registeredOperatorsValidator, retiredOperatorsPolicy, retiredOperatorsValidator))
import Midgard.Types.ActiveOperators qualified as ActiveOperators
import Midgard.Types.RegisteredOperators qualified as RegisteredOperators

initActiveOperators ::
  ( MonadBlockchain era m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  , C.IsBabbageBasedEra era
  ) =>
  MidgardScripts -> m ()
initActiveOperators MidgardScripts {activeOperatorsValidator, activeOperatorsPolicy} = do
  netId <- queryNetworkId
  let C.PolicyId policyId = mintingPolicyId activeOperatorsPolicy
  -- The active operators token should be minted.
  mintPlutus (toMintingPolicy activeOperatorsPolicy) ActiveOperators.Init (C.UnsafeAssetName "") 1
  -- And sent to the active operators validator.
  payToScriptInlineDatum
    netId
    (validatorHash activeOperatorsValidator)
    ()
    C.NoStakeAddress
    (assetValue policyId ActiveOperators.rootKey 1)

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
