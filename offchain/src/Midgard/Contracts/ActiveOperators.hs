module Midgard.Contracts.ActiveOperators (initActiveOperators, activateOperator) where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (runReaderT)
import Data.Coerce (coerce)

import Cardano.Api qualified as C
import Convex.BuildTx (
  MonadBuildTx,
  TxBuilder,
  addBtx,
  addReference,
  addRequiredSignature,
  assetValue,
  execBuildTx,
  findIndexReference,
  findIndexSpending,
  mintPlutusRefWithRedeemerFn,
  payToScriptInlineDatum,
  setMinAdaDepositAll,
  spendPlutusInlineDatum,
 )
import Convex.Class (
  MonadBlockchain (queryNetworkId, queryProtocolParameters, querySlotNo),
  MonadUtxoQuery,
  utxosByPaymentCredential,
 )
import Convex.Utxos (toTxOut)
import PlutusLedgerApi.V3 (PubKeyHash (PubKeyHash))

import Debug.Trace (traceShowId)
import Midgard.Constants (hubOracleAssetName, hubOracleMintingPolicyId, hubOracleScriptHash, operatorRequiredBond)
import Midgard.Contracts.Utils (
  LinkedListInfo (..),
  findMintRedeemerIndex,
  findOutputIndexWithAsset,
  findUTxONonMembership,
  findUtxoWithAsset,
  findUtxoWithLink,
  inlineDatumFromUTxO,
  listAssetNameFromUTxO,
  nextOutIx,
  pubKeyHashFromCardano,
 )
import Midgard.ScriptUtils (mintingPolicyId, mintingPolicyId', plutusVersion, toValidator, validatorHash)
import Midgard.Scripts (
  MidgardRefScripts (MidgardRefScripts, activeOperatorsPolicyRef, registeredOperatorsPolicyRef),
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
import Midgard.Types.RetiredOperators qualified as RetiredOperators

initActiveOperators ::
  ( C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  , C.IsBabbageBasedEra era
  ) =>
  C.NetworkId ->
  MidgardScripts ->
  MidgardRefScripts ->
  m ()
initActiveOperators
  netId
  MidgardScripts {activeOperatorsValidator, activeOperatorsPolicy}
  MidgardRefScripts {activeOperatorsPolicyRef} = do
    let C.PolicyId policyId = mintingPolicyId activeOperatorsPolicy
    addReference activeOperatorsPolicyRef
    -- The active operators token should be minted.
    mintPlutusRefWithRedeemerFn
      activeOperatorsPolicyRef
      (plutusVersion activeOperatorsPolicy)
      policyId
      (\txBody -> ActiveOperators.Init {outputIndex = toInteger $ nextOutIx txBody})
      ActiveOperators.rootAssetName
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
      (assetValue policyId ActiveOperators.rootAssetName 1)

activateOperator ::
  forall era m.
  ( MonadError String m
  , MonadBlockchain era m
  , MonadUtxoQuery m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , C.IsBabbageBasedEra era
  ) =>
  MidgardScripts -> MidgardRefScripts -> C.Hash C.PaymentKey -> m (TxBuilder era)
activateOperator
  MidgardScripts
    { registeredOperatorsValidator
    , registeredOperatorsPolicy
    , activeOperatorsValidator
    , activeOperatorsPolicy
    , retiredOperatorsValidator
    , retiredOperatorsPolicy
    }
  MidgardRefScripts {activeOperatorsPolicyRef, registeredOperatorsPolicyRef}
  operatorPkh = do
    let registryNodeAsset = C.UnsafeAssetName $ RegisteredOperators.nodeAssetNamePrefix <> C.serialiseToRawBytes operatorPkh
        targetNodeAsset = C.UnsafeAssetName $ ActiveOperators.nodeAssetNamePrefix <> C.serialiseToRawBytes operatorPkh
        policyId = mintingPolicyId activeOperatorsPolicy
        policyId' = mintingPolicyId' activeOperatorsPolicy
        -- Need to know all the policies that will be invoked beforehand.
        allPolicies = [mintingPolicyId registeredOperatorsPolicy, mintingPolicyId activeOperatorsPolicy]
    params <- queryProtocolParameters
    netId <- queryNetworkId
    (currentSlot, _, _) <- querySlotNo
    -- Find the hub oracle utxo.
    hubOracleUtxos <- utxosByPaymentCredential $ C.PaymentCredentialByScript hubOracleScriptHash
    (hubOracleTxIn, _) <-
      maybe (throwError "No hub oracle found") pure $
        findUtxoWithAsset hubOracleUtxos $
          C.AssetId hubOracleMintingPolicyId hubOracleAssetName
    -- Find the registered operator node and its anchor (node linking to it) to remove it from the list.
    registryUtxos <- utxosByPaymentCredential $ C.PaymentCredentialByScript $ validatorHash registeredOperatorsValidator
    (removalRegistryTxIn, (removalUtxoAnyEra, _)) <-
      maybe (throwError "No registered operator found") pure $
        findUtxoWithAsset registryUtxos $
          C.AssetId (mintingPolicyId registeredOperatorsPolicy) registryNodeAsset
    (anchorRegistryTxIn, (anchorRegistryUtxoAnyEra, _)) <-
      maybe (throwError "No anchor utxo found") pure
        . flip
          runReaderT
          LinkedListInfo
            { ownerPolicyId = mintingPolicyId registeredOperatorsPolicy
            , rootAssetName = RegisteredOperators.rootAssetName
            , nodeAssetNamePrefix = RegisteredOperators.nodeAssetNamePrefix
            }
        $ findUtxoWithLink registryUtxos registryNodeAsset
    -- Find insertion point in active operators for ordered insertion.
    activeOperatorsUtxos <- utxosByPaymentCredential $ C.PaymentCredentialByScript $ validatorHash activeOperatorsValidator
    (activeOperatorsAnchorTxIn, (activeOperatorsAnchorUtxoAnyEra, _)) <-
      maybe (throwError "No insertion point found in active operators set") pure
        . flip
          runReaderT
          LinkedListInfo
            { ownerPolicyId = mintingPolicyId activeOperatorsPolicy
            , rootAssetName = ActiveOperators.rootAssetName
            , nodeAssetNamePrefix = ActiveOperators.nodeAssetNamePrefix
            }
        $ findUTxONonMembership activeOperatorsUtxos targetNodeAsset
    -- Witness non-membership in retired operators.
    retiredOperatorsUtxos <- utxosByPaymentCredential $ C.PaymentCredentialByScript $ validatorHash retiredOperatorsValidator
    (retiredOperatorsNonMemberWitness, _) <-
      maybe (throwError "Operator already exists in retired operator set") pure
        . flip
          runReaderT
          LinkedListInfo
            { ownerPolicyId = mintingPolicyId retiredOperatorsPolicy
            , rootAssetName = RetiredOperators.rootAssetName
            , nodeAssetNamePrefix = RetiredOperators.nodeAssetNamePrefix
            }
        $ findUTxONonMembership retiredOperatorsUtxos targetNodeAsset
    let registeredOperatorsAnchorTxOut = toTxOut @era anchorRegistryUtxoAnyEra
        removalRegistryTxOut = toTxOut @era removalUtxoAnyEra
        activeOperatorsAnchorTxOut = toTxOut @era activeOperatorsAnchorUtxoAnyEra
    -- Extract datums required to patch links and build the new active node.
    removalRegistryDatum <-
      maybe
        (throwError "Invalid registry operator datum")
        pure
        $ inlineDatumFromUTxO @RegisteredOperators.Datum removalRegistryTxOut
    anchorRegistryDatum <-
      maybe
        (throwError "Invalid registry anchor operator datum")
        pure
        $ inlineDatumFromUTxO @RegisteredOperators.Datum registeredOperatorsAnchorTxOut
    activeOperatorsAnchorDatum <-
      maybe
        (throwError "Invalid active operators anchor datum")
        pure
        $ inlineDatumFromUTxO @ActiveOperators.Datum activeOperatorsAnchorTxOut
    -- New registry anchor datum should link to the node that the removed operator node used to link to.
    let updatedRegistryAnchorDatum = anchorRegistryDatum {LinkedList.elementLink = LinkedList.elementLink removalRegistryDatum}
        -- Active operators anchor node should link to the newly added operator node.
        updatedActiveOperatorsAnchorDatum =
          activeOperatorsAnchorDatum
            { LinkedList.elementLink = Just . coerce $ pubKeyHashFromCardano operatorPkh
            }
        -- New active operator node should link to the anchor.
        activeOperatorDatum :: ActiveOperators.Datum
        activeOperatorDatum =
          LinkedList.Element
            { elementData = LinkedList.Node $ ActiveOperators.NodeData {bondUnlockTime = Nothing}
            , elementLink = LinkedList.elementLink activeOperatorsAnchorDatum
            }
    registeredOperatorsAnchorAssetName <-
      maybe
        (throwError "Linked list asset not found for registered operators anchor utxo")
        pure
        $ listAssetNameFromUTxO (mintingPolicyId registeredOperatorsPolicy) registeredOperatorsAnchorTxOut
    activeOperatorsAnchorAssetName <-
      maybe
        (throwError "Linked list asset not found for active operators anchor utxo")
        pure
        $ listAssetNameFromUTxO policyId activeOperatorsAnchorTxOut
    pure . execBuildTx $ do
      -- Must be signed by the activating operator.
      addRequiredSignature operatorPkh
      -- Must witness hub oracle and retired non-membership proof.
      addReference hubOracleTxIn
      addReference retiredOperatorsNonMemberWitness
      -- Use reference scripts for both mints.
      addReference registeredOperatorsPolicyRef
      addReference activeOperatorsPolicyRef
      -- Remove the operator from the registered operators set.
      spendPlutusInlineDatum removalRegistryTxIn (toValidator registeredOperatorsValidator) ()
      -- Update the registered operators anchor link to bypass the removed node.
      spendPlutusInlineDatum anchorRegistryTxIn (toValidator registeredOperatorsValidator) ()
      payToScriptInlineDatum
        netId
        (validatorHash registeredOperatorsValidator)
        updatedRegistryAnchorDatum
        C.NoStakeAddress
        (txOutValue registeredOperatorsAnchorTxOut)
      -- Insert the node into the active operators set.
      spendPlutusInlineDatum
        activeOperatorsAnchorTxIn
        (toValidator activeOperatorsValidator)
        ActiveOperators.ListStateTransition
      payToScriptInlineDatum
        netId
        (validatorHash activeOperatorsValidator)
        updatedActiveOperatorsAnchorDatum
        C.NoStakeAddress
        (txOutValue activeOperatorsAnchorTxOut)
      payToScriptInlineDatum
        netId
        (validatorHash activeOperatorsValidator)
        activeOperatorDatum
        C.NoStakeAddress
        (assetValue policyId' targetNodeAsset 1 <> C.lovelaceToValue operatorRequiredBond)
      -- Burn the removed registered operator NFT.
      mintPlutusRefWithRedeemerFn
        registeredOperatorsPolicyRef
        (plutusVersion registeredOperatorsPolicy)
        (mintingPolicyId' registeredOperatorsPolicy)
        ( \txBody ->
            RegisteredOperators.ActivateOperator
              { activatingOperator = pubKeyHashFromCardano operatorPkh
              , anchorElementInputIndex = toInteger $ findIndexSpending anchorRegistryTxIn txBody
              , removedNodeInputIndex = toInteger $ findIndexSpending removalRegistryTxIn txBody
              , anchorElementOutputIndex =
                  toInteger $ findOutputIndexWithAsset (mintingPolicyId registeredOperatorsPolicy) registeredOperatorsAnchorAssetName txBody
              , hubOracleRefInputIndex = toInteger $ findIndexReference hubOracleTxIn txBody
              , retiredOperatorsElementRefInputIndex =
                  toInteger $ findIndexReference retiredOperatorsNonMemberWitness txBody
              , activeOperatorsRedeemerIndex =
                  toInteger $
                    findMintRedeemerIndex allPolicies txBody (mintingPolicyId activeOperatorsPolicy)
              }
        )
        registryNodeAsset
        (-1)
      -- Mint the NFT for the new active operator node.
      mintPlutusRefWithRedeemerFn
        activeOperatorsPolicyRef
        (plutusVersion activeOperatorsPolicy)
        policyId'
        ( \txBody ->
            ActiveOperators.ActivateOperator
              { newActiveOperatorKey = pubKeyHashFromCardano operatorPkh
              , activeOperatorAnchorElementInputIndex =
                  toInteger $ findIndexSpending activeOperatorsAnchorTxIn txBody
              , activeOperatorAnchorElementOutputIndex =
                  toInteger $ findOutputIndexWithAsset policyId activeOperatorsAnchorAssetName txBody
              , activeOperatorInsertedNodeOutputIndex =
                  toInteger $ findOutputIndexWithAsset policyId targetNodeAsset txBody
              , registeredOperatorsRedeemerIndex =
                  toInteger $
                    findMintRedeemerIndex allPolicies txBody (mintingPolicyId registeredOperatorsPolicy)
              }
        )
        targetNodeAsset
        1
      -- Enforce activation to happen at/after validity lower bound.
      addBtx $ \txBody ->
        txBody
          { C.txValidityLowerBound = C.TxValidityLowerBound (C.allegraBasedEra @era) currentSlot
          }
      setMinAdaDepositAll params
    where
      txOutValue (C.TxOut _ val _ _) = C.txOutValueToValue val
