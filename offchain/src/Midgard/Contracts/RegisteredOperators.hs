module Midgard.Contracts.RegisteredOperators (initRegisteredOperators, registerOperator, deregisterOperator) where

import Control.Monad.Except

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
import Convex.Utxos (toTxOut)

import Midgard.Constants (hubOracleAssetName, hubOracleMintingPolicyId, hubOracleScriptHash, operatorRequiredBond)
import Midgard.Contracts.Utils
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
import Midgard.Types.RegisteredOperators qualified as RegisteredOperators

initRegisteredOperators ::
  ( C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  , C.IsBabbageBasedEra era
  ) =>
  C.NetworkId ->
  MidgardScripts ->
  m ()
initRegisteredOperators netId MidgardScripts {registeredOperatorsValidator, registeredOperatorsPolicy} = do
  let C.PolicyId policyId = mintingPolicyId registeredOperatorsPolicy
  -- The registered operators token should be minted.
  mintPlutus
    (toMintingPolicy registeredOperatorsPolicy)
    RegisteredOperators.Init {outputIndex = 0}
    RegisteredOperators.rootKey
    1
  -- And sent to the registered operators validator.
  -- TODO: Datum should contain link.
  payToScriptInlineDatum
    netId
    (validatorHash registeredOperatorsValidator)
    ()
    C.NoStakeAddress
    (assetValue policyId RegisteredOperators.rootKey 1)

registerOperator ::
  ( MonadError String m
  , MonadBlockchain era m
  , MonadUtxoQuery m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  , C.IsBabbageBasedEra era
  ) =>
  MidgardScripts -> C.Hash C.PaymentKey -> m ()
registerOperator
  MidgardScripts
    { registeredOperatorsValidator
    , registeredOperatorsPolicy
    , activeOperatorsPolicy
    , activeOperatorsValidator
    , retiredOperatorsValidator
    , retiredOperatorsPolicy
    }
  operatorPkh = do
    let newNodeKey = C.UnsafeAssetName $ C.serialiseToRawBytes RegisteredOperators.nodeKeyPrefix <> C.serialiseToRawBytes operatorPkh
    let C.PolicyId policyId = mintingPolicyId registeredOperatorsPolicy
    netId <- queryNetworkId
    hubOracleUtxos <- utxosByPaymentCredential $ C.PaymentCredentialByScript hubOracleScriptHash
    (hubOracleTxIn, _) <-
      maybe (throwError "No hub oracle found") pure $
        findUtxoWithAsset hubOracleUtxos $
          C.AssetId hubOracleMintingPolicyId hubOracleAssetName
    registryUtxos <- utxosByPaymentCredential $ C.PaymentCredentialByScript $ validatorHash registeredOperatorsValidator
    (rootRegistryTxIn, (_rootRegistryUtxoAnyEra, _)) <-
      maybe (throwError "No registry root found") pure $
        findUtxoWithAsset registryUtxos $
          C.AssetId (mintingPolicyId registeredOperatorsPolicy) RegisteredOperators.rootKey
    activeOperatorsUtxos <- utxosByPaymentCredential $ C.PaymentCredentialByScript $ validatorHash activeOperatorsValidator
    activeOperatorsNonMemberWitness <-
      maybe (throwError "Operator already exists in active operator set") pure $
        findTxInNonMembership activeOperatorsUtxos (mintingPolicyId activeOperatorsPolicy) $
          C.serialiseToRawBytes operatorPkh
    retiredOperatorsUtxos <- utxosByPaymentCredential $ C.PaymentCredentialByScript $ validatorHash retiredOperatorsValidator
    retiredOperatorsNonMemberWitness <-
      maybe (throwError "Operator already exists in retired operator set") pure $
        findTxInNonMembership retiredOperatorsUtxos (mintingPolicyId retiredOperatorsPolicy) $
          C.serialiseToRawBytes operatorPkh
    addRequiredSignature operatorPkh
    addReference hubOracleTxIn
    addReference activeOperatorsNonMemberWitness
    addReference retiredOperatorsNonMemberWitness
    -- TODO: Use the proper redeemer once finalized.
    mintPlutus
      (toMintingPolicy registeredOperatorsPolicy)
      ()
      newNodeKey
      1
    -- TODO: Datum should contain original link from root (i.e newly added operator pkh).
    payToScriptInlineDatum
      netId
      (validatorHash registeredOperatorsValidator)
      ()
      C.NoStakeAddress
      (assetValue policyId newNodeKey 1 <> C.lovelaceToValue operatorRequiredBond)
    -- TODO: Datum should contain updated link (i.e newly added operator pkh).
    -- TODO: Add activation time assertion (i.e validity range)
    payToScriptInlineDatum
      netId
      (validatorHash registeredOperatorsValidator)
      ()
      C.NoStakeAddress
      (assetValue policyId RegisteredOperators.rootKey 1)
    spendPlutusInlineDatum
      rootRegistryTxIn
      (toValidator registeredOperatorsValidator)
      ()

deregisterOperator ::
  forall era m.
  ( MonadError String m
  , MonadBlockchain era m
  , MonadUtxoQuery m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  , C.IsBabbageBasedEra era
  ) =>
  MidgardScripts -> C.Hash C.PaymentKey -> m ()
deregisterOperator MidgardScripts {registeredOperatorsValidator, registeredOperatorsPolicy} operatorPkh = do
  let targetNodeKey = C.UnsafeAssetName $ C.serialiseToRawBytes RegisteredOperators.nodeKeyPrefix <> C.serialiseToRawBytes operatorPkh
  netId <- queryNetworkId
  registryUtxos <- utxosByPaymentCredential $ C.PaymentCredentialByScript $ validatorHash registeredOperatorsValidator
  (targetRegistryTxIn, (_rootRegistryUtxoAnyEra, _)) <-
    maybe (throwError "No registered operator found") pure $
      findUtxoWithAsset registryUtxos $
        C.AssetId (mintingPolicyId registeredOperatorsPolicy) targetNodeKey
  (anchorRegistryTxIn, anchorUtxoAnyEra) <-
    maybe (throwError "No anchor utxo found") pure $
      findUtxoWithLink registryUtxos (mintingPolicyId registeredOperatorsPolicy) $
        C.serialiseToRawBytes operatorPkh
  let C.TxOut _ anchorValue _ _ = toTxOut @era anchorUtxoAnyEra
  -- The new anchor output should have its link changed to the link from the target registry utxo (one being removed).
  let newAnchorLink = error "TODO: Need datum structures finalized"
  addRequiredSignature operatorPkh
  -- Remove the operator.
  spendPlutusInlineDatum targetRegistryTxIn (toValidator registeredOperatorsValidator) ()
  -- Modify the anchor. TODO: Update the datum.
  spendPlutusInlineDatum anchorRegistryTxIn (toValidator registeredOperatorsValidator) ()
  payToScriptInlineDatum netId (validatorHash registeredOperatorsValidator) () C.NoStakeAddress (C.txOutValueToValue anchorValue)
  -- Burn the operator NFT
  mintPlutus (toMintingPolicy registeredOperatorsPolicy) () targetNodeKey (-1)
