module Midgard.Contracts.RegisteredOperators (
  initRegisteredOperators,
  registerOperator,
  deregisterOperator,
) where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (runReaderT)
import Data.Coerce (coerce)
import Data.Time (addUTCTime)

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
  mintPlutus,
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
import Convex.Utils (utcTimeToPosixTime)
import Convex.Utxos (toTxOut)
import PlutusLedgerApi.V3 (PubKeyHash (PubKeyHash))

import Midgard.Constants (
  hubOracleAssetName,
  hubOracleMintingPolicyId,
  hubOracleScriptHash,
  operatorRequiredBond,
  registrationDuration,
 )
import Midgard.Contracts.Utils (
  LinkedListInfo (..),
  findOutputIndexWithAsset,
  findUTxONonMembership,
  findUtxoWithAsset,
  findUtxoWithLink,
  inlineDatumFromUTxO,
  nextOutIx,
  pubKeyHashFromCardano,
  slotToEndUTCTime,
 )
import Midgard.ScriptUtils (
  mintingPolicyId,
  mintingPolicyId',
  plutusVersion,
  toMintingPolicy,
  toValidator,
  validatorHash,
 )
import Midgard.Scripts (
  MidgardRefScripts (MidgardRefScripts, registeredOperatorsPolicyRef),
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

initRegisteredOperators ::
  ( C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  , C.IsBabbageBasedEra era
  ) =>
  C.NetworkId ->
  MidgardScripts ->
  MidgardRefScripts ->
  m ()
initRegisteredOperators
  netId
  MidgardScripts
    { registeredOperatorsValidator
    , registeredOperatorsPolicy
    }
  MidgardRefScripts {registeredOperatorsPolicyRef} = do
    let C.PolicyId policyId = mintingPolicyId registeredOperatorsPolicy
    addReference registeredOperatorsPolicyRef
    -- The registered operators token should be minted.
    mintPlutusRefWithRedeemerFn
      registeredOperatorsPolicyRef
      (plutusVersion registeredOperatorsPolicy)
      policyId
      (\txBody -> RegisteredOperators.Init {outputIndex = toInteger $ nextOutIx txBody})
      RegisteredOperators.rootAssetName
      1
    -- And sent to the registered operators validator.
    let datum :: RegisteredOperators.Datum =
          LinkedList.Element
            { elementData = LinkedList.Root mempty
            , elementLink = Nothing
            }
    payToScriptInlineDatum
      netId
      (validatorHash registeredOperatorsValidator)
      datum
      C.NoStakeAddress
      (assetValue policyId RegisteredOperators.rootAssetName 1)

registerOperator ::
  forall era m.
  ( MonadError String m
  , MonadBlockchain era m
  , MonadUtxoQuery m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , C.IsBabbageBasedEra era
  ) =>
  MidgardScripts -> MidgardRefScripts -> C.Hash C.PaymentKey -> m (TxBuilder era)
registerOperator
  MidgardScripts
    { registeredOperatorsValidator
    , registeredOperatorsPolicy
    , activeOperatorsPolicy
    , activeOperatorsValidator
    , retiredOperatorsValidator
    , retiredOperatorsPolicy
    }
  MidgardRefScripts {registeredOperatorsPolicyRef}
  operatorPkh = do
    let newNodeAsset =
          C.UnsafeAssetName $
            C.serialiseToRawBytes RegisteredOperators.nodeAssetNamePrefix <> C.serialiseToRawBytes operatorPkh
        policyId = mintingPolicyId' registeredOperatorsPolicy
    params <- queryProtocolParameters
    netId <- queryNetworkId
    (currentSlot, _, _) <- querySlotNo
    -- 5 minute grace period.
    -- Note: The upper bound ends _before_ the beginning of this slot. i.e end time of last slot.
    let validityUpperBoundExclusive = C.SlotNo $ C.unSlotNo currentSlot + 300
    validityUpperBoundPosixExclusive <- slotToEndUTCTime $ validityUpperBoundExclusive - 1
    -- Validity upper bound is exclusive in script context. On-chain normalizes it to inclusive with `-1`.
    let activationTime = utcTimeToPosixTime $ addUTCTime registrationDuration validityUpperBoundPosixExclusive
    -- Find the hub oracle utxo.
    hubOracleUtxos <- utxosByPaymentCredential $ C.PaymentCredentialByScript hubOracleScriptHash
    (hubOracleTxIn, _) <-
      maybe (throwError "No hub oracle found") pure $
        findUtxoWithAsset hubOracleUtxos $
          C.AssetId hubOracleMintingPolicyId hubOracleAssetName
    -- Find the root registry utxo.
    registryUtxos <-
      utxosByPaymentCredential $
        C.PaymentCredentialByScript $
          validatorHash registeredOperatorsValidator
    (rootRegistryTxIn, (rootRegistryUtxoAnyEra, _)) <-
      maybe (throwError "No registry root found") pure $
        findUtxoWithAsset registryUtxos $
          C.AssetId (mintingPolicyId registeredOperatorsPolicy) RegisteredOperators.rootAssetName
    -- Note the existing root link so it can be put in the new node (prepend).
    rootOriginalLink <- do
      case inlineDatumFromUTxO @RegisteredOperators.Datum $ toTxOut @era rootRegistryUtxoAnyEra of
        Just LinkedList.Element {elementLink} -> pure elementLink
        Nothing -> throwError "Invalid registry root datum"
    -- Find the active operators utxo witness to prove that the operator does not exist there.
    activeOperatorsUtxos <-
      utxosByPaymentCredential $
        C.PaymentCredentialByScript $
          validatorHash activeOperatorsValidator
    (activeOperatorsNonMemberWitness, _) <-
      maybe (throwError "Operator already exists in active operator set") pure
        $ flip
          runReaderT
          LinkedListInfo
            { ownerPolicyId = mintingPolicyId activeOperatorsPolicy
            , rootAssetName = ActiveOperators.rootAssetName
            , nodeAssetNamePrefix = ActiveOperators.nodeAssetNamePrefix
            }
        $ findUTxONonMembership activeOperatorsUtxos newNodeAsset
    -- Find the retired operators utxo witness to prove that the operator does not exist there.
    retiredOperatorsUtxos <-
      utxosByPaymentCredential $
        C.PaymentCredentialByScript $
          validatorHash retiredOperatorsValidator
    (retiredOperatorsNonMemberWitness, _) <-
      maybe (throwError "Operator already exists in retired operator set") pure
        $ flip
          runReaderT
          LinkedListInfo
            { ownerPolicyId = mintingPolicyId retiredOperatorsPolicy
            , rootAssetName = RetiredOperators.rootAssetName
            , nodeAssetNamePrefix = RetiredOperators.nodeAssetNamePrefix
            }
        $ findUTxONonMembership retiredOperatorsUtxos newNodeAsset
    pure . execBuildTx @era $ do
      -- Must be signed by registering operator.
      addRequiredSignature operatorPkh
      -- Must witness the hub oracle.
      addReference hubOracleTxIn
      -- Must witness a proof of non-membership in active operators set.
      addReference activeOperatorsNonMemberWitness
      -- Must witness a proof of non-membership in retired operators set.
      addReference retiredOperatorsNonMemberWitness
      -- Update the root node's link.
      spendPlutusInlineDatum
        rootRegistryTxIn
        (toValidator registeredOperatorsValidator)
        ()
      let updatedRootDatum :: RegisteredOperators.Datum
          updatedRootDatum =
            LinkedList.Element
              { elementData = LinkedList.Root mempty
              , elementLink = Just . coerce $ pubKeyHashFromCardano operatorPkh
              }
      payToScriptInlineDatum
        netId
        (validatorHash registeredOperatorsValidator)
        updatedRootDatum
        C.NoStakeAddress
        (assetValue policyId RegisteredOperators.rootAssetName 1)
      -- The new node's datum should contain the original root link and proper activation time.
      let registeredOperatorDatum :: RegisteredOperators.Datum
          registeredOperatorDatum =
            LinkedList.Element
              { elementData =
                  LinkedList.Node
                    RegisteredOperators.NodeData
                      { activationTime
                      }
              , elementLink = rootOriginalLink
              }
      -- Prepend the new node.
      payToScriptInlineDatum
        netId
        (validatorHash registeredOperatorsValidator)
        registeredOperatorDatum
        C.NoStakeAddress
        (assetValue policyId newNodeAsset 1 <> C.lovelaceToValue operatorRequiredBond)
      -- Mint the token for the new registering node.
      mintPlutusRefWithRedeemerFn
        registeredOperatorsPolicyRef
        (plutusVersion registeredOperatorsPolicy)
        policyId
        ( \txBody ->
            RegisteredOperators.RegisterOperator
              { registeringOperator = pubKeyHashFromCardano operatorPkh
              , rootInputIndex =
                  toInteger $
                    findIndexSpending rootRegistryTxIn txBody
              , rootOutputIndex =
                  toInteger $
                    findOutputIndexWithAsset
                      (mintingPolicyId registeredOperatorsPolicy)
                      RegisteredOperators.rootAssetName
                      txBody
              , registeredNodeOutputIndex =
                  toInteger $
                    findOutputIndexWithAsset (mintingPolicyId registeredOperatorsPolicy) newNodeAsset txBody
              , hubOracleRefInputIndex = toInteger $ findIndexReference hubOracleTxIn txBody
              , activeOperatorsElementRefInputIndex =
                  toInteger $
                    findIndexReference activeOperatorsNonMemberWitness txBody
              , retiredOperatorsElementRefInputIndex =
                  toInteger $
                    findIndexReference retiredOperatorsNonMemberWitness txBody
              }
        )
        newNodeAsset
        1
      addBtx $ \txBody ->
        txBody
          { C.txValidityLowerBound = C.TxValidityLowerBound (C.allegraBasedEra @era) currentSlot
          , C.txValidityUpperBound = C.TxValidityUpperBound (C.shelleyBasedEra @era) $ Just validityUpperBoundExclusive
          }
      setMinAdaDepositAll params

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
  let targetNodeAsset = C.UnsafeAssetName $ RegisteredOperators.nodeAssetNamePrefix <> C.serialiseToRawBytes operatorPkh
  netId <- queryNetworkId
  registryUtxos <- utxosByPaymentCredential $ C.PaymentCredentialByScript $ validatorHash registeredOperatorsValidator
  (targetRegistryTxIn, (_rootRegistryUtxoAnyEra, _)) <-
    maybe (throwError "No registered operator found") pure $
      findUtxoWithAsset registryUtxos $
        C.AssetId (mintingPolicyId registeredOperatorsPolicy) targetNodeAsset
  (anchorRegistryTxIn, (anchorUtxoAnyEra, _)) <-
    maybe (throwError "No anchor utxo found") pure
      . flip
        runReaderT
        LinkedListInfo
          { ownerPolicyId = mintingPolicyId registeredOperatorsPolicy
          , rootAssetName = RegisteredOperators.rootAssetName
          , nodeAssetNamePrefix = RegisteredOperators.nodeAssetNamePrefix
          }
      $ findUtxoWithLink registryUtxos targetNodeAsset
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
  mintPlutus (toMintingPolicy registeredOperatorsPolicy) () targetNodeAsset (-1)
