module Midgard.Contracts.RetiredOperators (initRetiredOperators, retireOperator) where

import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (runReaderT)
import Data.Coerce (coerce)

import Cardano.Api qualified as C
import Convex.BuildTx (
  MonadBuildTx,
  TxBuilder,
  addReference,
  addRequiredSignature,
  assetValue,
  execBuildTx,
  findIndexReference,
  mintPlutusRefWithRedeemerFn,
  payToScriptInlineDatum,
  setMinAdaDepositAll,
  spendPlutusInlineDatum,
 )
import Convex.Class (MonadBlockchain (queryNetworkId, queryProtocolParameters), MonadUtxoQuery, utxosByPaymentCredential)
import Convex.PlutusLedger.V1 (transPubKeyHash)
import Convex.PlutusLedger.V3 (transTxOutRef)
import Convex.Utxos (toTxOut)
import PlutusLedgerApi.V3 (PubKeyHash (PubKeyHash))

import Midgard.Constants (hubOracleAssetName, hubOracleMintingPolicyId, hubOracleScriptHash, operatorRequiredBond)
import Midgard.Contracts.Scheduler (currentScheduleInfo)
import Midgard.Contracts.Utils (
  LinkedListInfo (..),
  findMintRedeemerIndex,
  findOutputIndexWithAsset,
  findUTxONonMembership,
  findUTxOWithAsset,
  findUTxOWithLink,
  inlineDatumFromUTxO,
  listAssetNameFromUTxO,
  mintPlutusRefWithRedeemerFinal,
  nextOutIx,
 )
import Midgard.ScriptUtils (mintingPolicyId, mintingPolicyId', plutusVersion, toValidator, validatorHash)
import Midgard.Scripts (
  MidgardRefScripts (MidgardRefScripts, activeOperatorsPolicyRef, retiredOperatorsPolicyRef),
  MidgardScripts (
    MidgardScripts,
    activeOperatorsPolicy,
    activeOperatorsValidator,
    retiredOperatorsPolicy,
    retiredOperatorsValidator
  ),
 )
import Midgard.Types.ActiveOperators qualified as ActiveOperators
import Midgard.Types.LinkedList qualified as LinkedList
import Midgard.Types.RetiredOperators qualified as RetiredOperators

initRetiredOperators ::
  ( C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  , C.IsBabbageBasedEra era
  ) =>
  C.NetworkId ->
  MidgardScripts ->
  MidgardRefScripts ->
  m ()
initRetiredOperators
  netId
  MidgardScripts {retiredOperatorsValidator, retiredOperatorsPolicy}
  MidgardRefScripts {retiredOperatorsPolicyRef} = do
    let C.PolicyId policyId = mintingPolicyId retiredOperatorsPolicy
    addReference retiredOperatorsPolicyRef
    -- The registered operators token should be minted.
    mintPlutusRefWithRedeemerFn
      retiredOperatorsPolicyRef
      (plutusVersion retiredOperatorsPolicy)
      policyId
      (\txBody -> RetiredOperators.Init {outputIndex = toInteger $ nextOutIx txBody})
      RetiredOperators.rootAssetName
      1
    -- And sent to the registered operators validator.
    let datum :: RetiredOperators.Datum =
          LinkedList.Element
            { elementData = LinkedList.Root mempty
            , elementLink = Nothing
            }
    payToScriptInlineDatum
      netId
      (validatorHash retiredOperatorsValidator)
      datum
      C.NoStakeAddress
      (assetValue policyId RetiredOperators.rootAssetName 1)

{- | Retire an operator, assuming they are not the currently scheduled operator.
Note: This function does not support retiring currently active operator under any scenario, even if scheduler
is advancing in conjunction
-}
retireOperator ::
  forall era m.
  ( MonadError String m
  , MonadBlockchain era m
  , MonadUtxoQuery m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , C.IsBabbageBasedEra era
  ) =>
  MidgardScripts -> MidgardRefScripts -> C.Hash C.PaymentKey -> m (TxBuilder era)
retireOperator
  ms@MidgardScripts
    { activeOperatorsValidator
    , activeOperatorsPolicy
    , retiredOperatorsValidator
    , retiredOperatorsPolicy
    }
  MidgardRefScripts {activeOperatorsPolicyRef, retiredOperatorsPolicyRef}
  operatorPkh = do
    let operatorPkhBytes = C.serialiseToRawBytes operatorPkh
        activeNodeAsset = C.UnsafeAssetName $ ActiveOperators.nodeAssetNamePrefix <> operatorPkhBytes
        retiredNodeAsset = C.UnsafeAssetName $ RetiredOperators.nodeAssetNamePrefix <> operatorPkhBytes
        activePolicyId = mintingPolicyId activeOperatorsPolicy
        retiredPolicyId = mintingPolicyId retiredOperatorsPolicy
        retiredPolicyId' = mintingPolicyId' retiredOperatorsPolicy
        allPolicies = [activePolicyId, retiredPolicyId]
    params <- queryProtocolParameters
    netId <- queryNetworkId
    hubOracleUtxos <- utxosByPaymentCredential $ C.PaymentCredentialByScript hubOracleScriptHash
    (hubOracleTxIn, _) <-
      maybe (throwError "No hub oracle found") pure $
        findUTxOWithAsset hubOracleUtxos $
          C.AssetId hubOracleMintingPolicyId hubOracleAssetName
    activeOperatorsUtxos <-
      utxosByPaymentCredential $
        C.PaymentCredentialByScript $
          validatorHash activeOperatorsValidator
    (removalActiveTxIn, (removalActiveUtxoAnyEra, _)) <-
      maybe (throwError "No active operator found") pure $
        findUTxOWithAsset activeOperatorsUtxos $
          C.AssetId activePolicyId activeNodeAsset
    (anchorActiveTxIn, (anchorActiveUtxoAnyEra, _)) <-
      maybe (throwError "No active operator anchor found") pure
        . flip
          runReaderT
          LinkedListInfo
            { ownerPolicyId = activePolicyId
            , rootAssetName = ActiveOperators.rootAssetName
            , nodeAssetNamePrefix = ActiveOperators.nodeAssetNamePrefix
            }
        $ findUTxOWithLink activeOperatorsUtxos operatorPkhBytes
    retiredOperatorsUtxos <-
      utxosByPaymentCredential $
        C.PaymentCredentialByScript $
          validatorHash retiredOperatorsValidator
    (retiredAnchorTxIn, (retiredAnchorUtxoAnyEra, _)) <-
      maybe (throwError "No insertion point found in retired operators set") pure
        . flip
          runReaderT
          LinkedListInfo
            { ownerPolicyId = retiredPolicyId
            , rootAssetName = RetiredOperators.rootAssetName
            , nodeAssetNamePrefix = RetiredOperators.nodeAssetNamePrefix
            }
        $ findUTxONonMembership retiredOperatorsUtxos operatorPkhBytes
    let anchorActiveTxOut = toTxOut @era anchorActiveUtxoAnyEra
        removalActiveTxOut = toTxOut @era removalActiveUtxoAnyEra
        retiredAnchorTxOut = toTxOut @era retiredAnchorUtxoAnyEra
    removalActiveDatum <-
      maybe (throwError "Invalid active operator datum") pure $
        inlineDatumFromUTxO @ActiveOperators.Datum removalActiveTxOut
    anchorActiveDatum <-
      maybe (throwError "Invalid active operator anchor datum") pure $
        inlineDatumFromUTxO @ActiveOperators.Datum anchorActiveTxOut
    retiredAnchorDatum <-
      maybe (throwError "Invalid retired operator anchor datum") pure $
        inlineDatumFromUTxO @RetiredOperators.Datum retiredAnchorTxOut
    bondUnlockTime <-
      case LinkedList.elementData removalActiveDatum of
        LinkedList.Node ActiveOperators.NodeData {bondUnlockTime} -> pure bondUnlockTime
        LinkedList.Root _ -> throwError "Active operator removal target cannot be root"
    let updatedActiveAnchorDatum =
          anchorActiveDatum {LinkedList.elementLink = LinkedList.elementLink removalActiveDatum}
        updatedRetiredAnchorDatum =
          retiredAnchorDatum {LinkedList.elementLink = Just . coerce $ transPubKeyHash operatorPkh}
        retiredOperatorDatum :: RetiredOperators.Datum
        retiredOperatorDatum =
          LinkedList.Element
            { elementData = LinkedList.Node $ RetiredOperators.NodeData {bondUnlockTime}
            , elementLink = LinkedList.elementLink retiredAnchorDatum
            }
    activeAnchorAssetName <-
      maybe (throwError "Linked list asset not found for active operator anchor utxo") pure $
        listAssetNameFromUTxO activePolicyId anchorActiveTxOut
    retiredAnchorAssetName <-
      maybe (throwError "Linked list asset not found for retired operator anchor utxo") pure $
        listAssetNameFromUTxO retiredPolicyId retiredAnchorTxOut
    (schedulerTxIn, currentScheduleInfo) <- currentScheduleInfo ms
    when (elem (transPubKeyHash operatorPkh) $ fst <$> currentScheduleInfo) $
      throwError "Retiring of currently scheduled operator not allowed"
    pure . execBuildTx $ do
      -- Must be signed by the retiring operator.
      addRequiredSignature operatorPkh
      -- Need to witness the scheduler UTxO to ensure currently schedule operator is not retiring.
      addReference schedulerTxIn
      -- Must witness the hub oracle so the retired-operators mint policy can recover the active-operators policy ID
      -- from protocol state.
      addReference hubOracleTxIn
      -- Both mints use reference scripts.
      addReference activeOperatorsPolicyRef
      addReference retiredOperatorsPolicyRef
      -- Spend the active operator node being retired.
      spendPlutusInlineDatum
        removalActiveTxIn
        (toValidator activeOperatorsValidator)
        ActiveOperators.ListStateTransition
      -- Spend the active-set anchor that points to the retiring node.
      spendPlutusInlineDatum
        anchorActiveTxIn
        (toValidator activeOperatorsValidator)
        ActiveOperators.ListStateTransition
      -- Reproduce the active-set anchor with its link updated to bypass the removed node.
      payToScriptInlineDatum
        netId
        (validatorHash activeOperatorsValidator)
        updatedActiveAnchorDatum
        C.NoStakeAddress
        (txOutValue anchorActiveTxOut)
      -- Spend the retired-set insertion anchor.
      spendPlutusInlineDatum
        retiredAnchorTxIn
        (toValidator retiredOperatorsValidator)
        ()
      -- Reproduce the retired-set anchor so it now points at the inserted retired operator node.
      payToScriptInlineDatum
        netId
        (validatorHash retiredOperatorsValidator)
        updatedRetiredAnchorDatum
        C.NoStakeAddress
        (txOutValue retiredAnchorTxOut)
      -- Insert the retired operator node, preserving the operator bond and the bond unlock time captured from the
      -- active-set node.
      payToScriptInlineDatum
        netId
        (validatorHash retiredOperatorsValidator)
        retiredOperatorDatum
        C.NoStakeAddress
        (assetValue retiredPolicyId' retiredNodeAsset 1 <> C.lovelaceToValue operatorRequiredBond)
      -- Burn the active-operator NFT associated with the removed node.
      mintPlutusRefWithRedeemerFinal
        activeOperatorsPolicyRef
        (plutusVersion activeOperatorsPolicy)
        activePolicyId
        activeNodeAsset
        (-1)
        $ \txBody ->
          ActiveOperators.RetireOperator
            { activeOperatorKey = transPubKeyHash operatorPkh
            , hubOracleRefInputIndex =
                toInteger $ findIndexReference hubOracleTxIn txBody
            , activeOperatorAnchorElementInputOutRef = transTxOutRef anchorActiveTxIn
            , activeOperatorAnchorElementOutputIndex =
                toInteger $ findOutputIndexWithAsset activePolicyId activeAnchorAssetName txBody
            , retiredOperatorsRedeemerIndex =
                toInteger $ findMintRedeemerIndex allPolicies txBody retiredPolicyId
            , penalizeForInactivity = False
            , operatorRemovalSchedulerSync =
                ActiveOperators.ShowOperatorIsInactive
                  . toInteger
                  $ findIndexReference schedulerTxIn txBody
            }
      -- Mint the retired-operator NFT for the newly inserted node.
      mintPlutusRefWithRedeemerFinal
        retiredOperatorsPolicyRef
        (plutusVersion retiredOperatorsPolicy)
        retiredPolicyId
        retiredNodeAsset
        1
        $ \txBody ->
          RetiredOperators.RetireOperator
            { newRetiredOperatorKey = transPubKeyHash operatorPkh
            , bondUnlockTime
            , hubOracleRefInputIndex = toInteger $ findIndexReference hubOracleTxIn txBody
            , retiredOperatorAnchorElementOutputIndex =
                toInteger $ findOutputIndexWithAsset retiredPolicyId retiredAnchorAssetName txBody
            , retiredOperatorInsertedNodeOutputIndex =
                toInteger $ findOutputIndexWithAsset retiredPolicyId retiredNodeAsset txBody
            , activeOperatorsRedeemerIndex =
                toInteger $ findMintRedeemerIndex allPolicies txBody activePolicyId
            }
      -- Ensure all script outputs meet min-ADA requirements after balancing.
      setMinAdaDepositAll params
    where
      txOutValue (C.TxOut _ val _ _) = C.txOutValueToValue val
