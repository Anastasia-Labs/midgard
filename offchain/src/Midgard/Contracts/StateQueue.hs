module Midgard.Contracts.StateQueue (
  initStateQueue,
  NewBlock (..),
  commitBlockHeader,
  mergeToConfirmedState,
) where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (runReaderT)
import Data.Time (addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

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
  payToScriptInlineDatum,
  setMinAdaDepositAll,
  spendPlutusInlineDatum,
 )
import Convex.Class (
  MonadBlockchain (queryNetworkId, queryProtocolParameters, querySlotNo),
  MonadUtxoQuery,
  utxosByPaymentCredential,
 )
import Convex.PlutusLedger.V1 (transPOSIXTime, unTransPubKeyHash)
import Convex.Utils (utcTimeToPosixTime)
import Convex.Utxos (toTxOut)
import PlutusLedgerApi.Common (fromBuiltin, toBuiltin)
import PlutusLedgerApi.V3 (POSIXTime, PubKeyHash (getPubKeyHash))

import Midgard.Constants (hubOracleAssetName, hubOracleMintingPolicyId, hubOracleScriptHash, maturityDuration)
import Midgard.Contracts.Scheduler (currentScheduleInfo)
import Midgard.Contracts.Utils (
  LinkedListInfo (..),
  findFinalUTxONode,
  findMintRedeemerIndex,
  findOutputIndexWithAsset,
  findSpendingRedeemerIndex,
  findUTxOWithAsset,
  hashPlutusData,
  inlineDatumFromUTxO,
  listAssetNameFromUTxO,
  mintPlutusWithRedeemerFinal,
  slotToEndUTCTime,
  slotToEndUTCTimePure,
  spendPlutusInlineDatumWithRedeemerFinal,
 )
import Midgard.ScriptUtils (mintingPolicyId, toMintingPolicy, toValidator, validatorHash)
import Midgard.Scripts (
  MidgardScripts (
    MidgardScripts,
    activeOperatorsPolicy,
    activeOperatorsValidator,
    stateQueuePolicy,
    stateQueueValidator
  ),
 )
import Midgard.Types.ActiveOperators qualified as ActiveOperators
import Midgard.Types.LedgerState qualified as LedgerState
import Midgard.Types.LinkedList qualified as LinkedList
import Midgard.Types.StateQueue qualified as StateQueue

data NewBlock = NewBlock
  { utxosRoot :: LedgerState.MerkleRoot
  , transactionsRoot :: LedgerState.MerkleRoot
  , depositsRoot :: LedgerState.MerkleRoot
  , withdrawalsRoot :: LedgerState.MerkleRoot
  }

initStateQueue ::
  forall era m.
  ( C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  , C.IsBabbageBasedEra era
  ) =>
  C.NetworkId ->
  C.EraHistory ->
  C.SystemStart ->
  C.SlotNo ->
  MidgardScripts ->
  m ()
initStateQueue
  netId
  eraHistory
  systemStart
  currentSlot
  MidgardScripts {stateQueueValidator, stateQueuePolicy} = do
    let C.PolicyId policyId = mintingPolicyId stateQueuePolicy
        -- In 5 minutes.
        validityUpperBoundExclusive = currentSlot + 300
        validityUpperBoundTime =
          transPOSIXTime
            . utcTimeToPOSIXSeconds
            . either error id
            $ slotToEndUTCTimePure eraHistory systemStart
            $ validityUpperBoundExclusive - 1
        datum :: StateQueue.Datum
        datum =
          LinkedList.Element
            { elementData =
                LinkedList.Root $
                  LedgerState.ConfirmedState
                    { headerHash = LedgerState.genesisHeaderHash
                    , prevHeaderHash = LedgerState.genesisHeaderHash
                    , utxoRoot = LedgerState.genesisUtxoRoot
                    , startTime = validityUpperBoundTime
                    , endTime = validityUpperBoundTime
                    , protocolVersion = LedgerState.genesisProtocolVersion
                    }
            , elementLink = Nothing
            }
    mintPlutusWithRedeemerFinal
      (toMintingPolicy stateQueuePolicy)
      (mintingPolicyId stateQueuePolicy)
      StateQueue.confirmedStateAssetName
      1
      -- Constraint: mint the confirmed-state root NFT and point the redeemer
      -- at the produced root output.
      $ \txBody ->
        StateQueue.Init
          { outputIndex =
              toInteger $
                findOutputIndexWithAsset
                  (mintingPolicyId stateQueuePolicy)
                  StateQueue.confirmedStateAssetName
                  txBody
          }
    payToScriptInlineDatum
      netId
      (validatorHash stateQueueValidator)
      datum
      C.NoStakeAddress
      -- Constraint: produce the state-queue root UTxO with genesis confirmed
      -- state data under the state-queue validator.
      (assetValue policyId StateQueue.confirmedStateAssetName 1)
    addBtx $ \txBody ->
      -- Constraint: state-queue init requires a short closed validity range;
      -- the upper bound also determines the initial confirmed-state times.
      txBody
        { C.txValidityLowerBound = C.TxValidityLowerBound (C.allegraBasedEra @era) currentSlot
        , C.txValidityUpperBound =
            C.TxValidityUpperBound (C.shelleyBasedEra @era) $ Just validityUpperBoundExclusive
        }

commitBlockHeader ::
  forall era m.
  ( MonadError String m
  , MonadBlockchain era m
  , MonadUtxoQuery m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , C.IsBabbageBasedEra era
  ) =>
  MidgardScripts ->
  NewBlock ->
  m (TxBuilder era, POSIXTime)
commitBlockHeader
  ms@MidgardScripts
    { stateQueueValidator
    , stateQueuePolicy
    , activeOperatorsValidator
    , activeOperatorsPolicy
    }
  NewBlock {utxosRoot, transactionsRoot, depositsRoot, withdrawalsRoot} = do
    params <- queryProtocolParameters
    netId <- queryNetworkId
    (currentSlot, _, _) <- querySlotNo
    let C.PolicyId stateQueuePolicyId = mintingPolicyId stateQueuePolicy
        validityUpperBoundExclusive = currentSlot + 300
        stateQueuePolicies = [mintingPolicyId stateQueuePolicy]
    validityUpperBoundPosixInclusive <- slotToEndUTCTime $ validityUpperBoundExclusive - 1
    let headerEndTime = utcTimeToPosixTime validityUpperBoundPosixInclusive
        newBondUnlockTime = utcTimeToPosixTime $ addUTCTime maturityDuration validityUpperBoundPosixInclusive

    hubOracleUtxos <- utxosByPaymentCredential $ C.PaymentCredentialByScript hubOracleScriptHash
    (hubOracleTxIn, _) <-
      maybe (throwError "No hub oracle found") pure $
        findUTxOWithAsset hubOracleUtxos $
          C.AssetId hubOracleMintingPolicyId hubOracleAssetName

    -- Need to figure out the currently scheduled operator
    (schedulerTxIn, currentOperator, _) <- currentScheduleInfo ms
    let currentOperatorBytes = fromBuiltin $ getPubKeyHash currentOperator
        activeOperatorAssetName =
          C.UnsafeAssetName $ ActiveOperators.nodeAssetNamePrefix <> currentOperatorBytes

    -- Find the anchor node in the state queue (i.e final node).
    stateQueueUtxos <-
      utxosByPaymentCredential $
        C.PaymentCredentialByScript $
          validatorHash stateQueueValidator
    (latestBlockTxIn, (latestBlockUtxoAnyEra, _)) <-
      maybe (throwError "No final state queue node found") pure
        . flip
          runReaderT
          LinkedListInfo
            { ownerPolicyId = mintingPolicyId stateQueuePolicy
            , rootAssetName = StateQueue.confirmedStateAssetName
            , nodeAssetNamePrefix = StateQueue.blockAssetNamePrefix
            }
        $ findFinalUTxONode stateQueueUtxos
    let latestBlockTxOut = toTxOut @era latestBlockUtxoAnyEra
    latestBlockAssetName <-
      maybe (throwError "Latest state queue node missing NFT") pure $
        listAssetNameFromUTxO (mintingPolicyId stateQueuePolicy) latestBlockTxOut
    latestBlockDatum <-
      maybe (throwError "Invalid state queue datum") pure $
        inlineDatumFromUTxO @StateQueue.Datum latestBlockTxOut

    -- Need to update the active operator's bond hold state.
    activeOperatorsUtxos <-
      utxosByPaymentCredential $
        C.PaymentCredentialByScript $
          validatorHash activeOperatorsValidator
    (activeOperatorTxIn, (activeOperatorUtxoAnyEra, _)) <-
      maybe (throwError "No active operator node found") pure $
        findUTxOWithAsset activeOperatorsUtxos $
          C.AssetId (mintingPolicyId activeOperatorsPolicy) activeOperatorAssetName
    let activeOperatorTxOut = toTxOut @era activeOperatorUtxoAnyEra
    activeOperatorDatum <-
      maybe (throwError "Invalid active operator datum") pure $
        inlineDatumFromUTxO @ActiveOperators.Datum activeOperatorTxOut

    -- Compute the new block header and its hash based on the anchor our new block.
    let (prevHeaderHash, prevUtxosRoot, blockStartTime, blockProtocolVersion) =
          case latestBlockDatum of
            LinkedList.Element {elementData = LinkedList.Root confirmedState} ->
              ( confirmedState.headerHash
              , confirmedState.utxoRoot
              , confirmedState.endTime
              , confirmedState.protocolVersion
              )
            LinkedList.Element {elementData = LinkedList.Node header} ->
              ( toBuiltin $ LinkedList.nodeKeyFromAssetName' StateQueue.blockAssetNamePrefixLen latestBlockAssetName
              , header.utxosRoot
              , header.endTime
              , header.protocolVersion
              )
        headerDatum =
          LedgerState.Header
            { prevUtxosRoot = prevUtxosRoot
            , utxosRoot = utxosRoot
            , transactionsRoot = transactionsRoot
            , depositsRoot = depositsRoot
            , withdrawalsRoot = withdrawalsRoot
            , startTime = blockStartTime
            , endTime = headerEndTime
            , prevHeaderHash = prevHeaderHash
            , operatorVkey = currentOperator
            , protocolVersion = blockProtocolVersion
            }
        newBlockHash = hashPlutusData headerDatum
        newBlockKey = LinkedList.nodeKey newBlockHash
        newBlockAssetName = C.UnsafeAssetName $ StateQueue.blockAssetNamePrefix <> newBlockHash

    pure . (,headerEndTime) . execBuildTx $ do
      -- The committing operator must sign off.
      currentOperatorC <- either (error . show) pure $ unTransPubKeyHash currentOperator
      addRequiredSignature currentOperatorC
      -- The hub oracle needs to be witnessed.
      addReference hubOracleTxIn
      -- The scheduler needs to be witnessed (to ensure current operator).
      addReference schedulerTxIn
      -- Append the new block.
      spendPlutusInlineDatum latestBlockTxIn (toValidator stateQueueValidator) ()
      -- Update the link to point to the new block.
      let continuedLatestBlockDatum = latestBlockDatum {LinkedList.elementLink = Just newBlockKey}
      payToScriptInlineDatum
        netId
        (validatorHash stateQueueValidator)
        continuedLatestBlockDatum
        C.NoStakeAddress
        (txOutValue latestBlockTxOut)
      let newBlockOutputDatum :: StateQueue.Datum =
            LinkedList.Element
              { elementData = LinkedList.Node headerDatum
              , elementLink = Nothing
              }
      payToScriptInlineDatum
        netId
        (validatorHash stateQueueValidator)
        newBlockOutputDatum
        C.NoStakeAddress
        (assetValue stateQueuePolicyId newBlockAssetName 1)
      -- The new block needs its unique NFT minted.
      mintPlutusWithRedeemerFinal
        (toMintingPolicy stateQueuePolicy)
        (mintingPolicyId stateQueuePolicy)
        newBlockAssetName
        1
        $ \txBody ->
          StateQueue.CommitBlockHeader
            { latestBlockInputIndex = toInteger $ findIndexSpending latestBlockTxIn txBody
            , newBlockOutputIndex =
                toInteger $
                  findOutputIndexWithAsset
                    (mintingPolicyId stateQueuePolicy)
                    newBlockAssetName
                    txBody
            , continuedLatestBlockOutputIndex =
                toInteger $
                  findOutputIndexWithAsset
                    (mintingPolicyId stateQueuePolicy)
                    latestBlockAssetName
                    txBody
            , operator = currentOperator
            , schedulerRefInputIndex = toInteger $ findIndexReference schedulerTxIn txBody
            , activeOperatorsInputIndex = toInteger $ findIndexSpending activeOperatorTxIn txBody
            , activeOperatorsRedeemerIndex = toInteger $ findSpendingRedeemerIndex activeOperatorTxIn txBody
            }

      -- Update the active operator node for the current operator.
      spendPlutusInlineDatumWithRedeemerFinal
        (toValidator activeOperatorsValidator)
        activeOperatorTxIn
        $ \txBody ->
          ActiveOperators.UpdateBondHoldNewState
            { activeNodeInputIndex = toInteger $ findIndexSpending activeOperatorTxIn txBody
            , activeNodeOutputIndex =
                toInteger $
                  findOutputIndexWithAsset
                    (mintingPolicyId activeOperatorsPolicy)
                    activeOperatorAssetName
                    txBody
            , hubOracleRefInputIndex = toInteger $ findIndexReference hubOracleTxIn txBody
            , stateQueueInputIndex = toInteger $ findIndexSpending latestBlockTxIn txBody
            , stateQueueRedeemerIndex =
                toInteger $
                  findMintRedeemerIndex
                    stateQueuePolicies
                    txBody
                    (mintingPolicyId stateQueuePolicy)
            }
      let updatedActiveOperatorDatum :: ActiveOperators.Datum =
            activeOperatorDatum
              { LinkedList.elementData =
                  LinkedList.Node $
                    ActiveOperators.NodeData
                      { bondUnlockTime = Just newBondUnlockTime
                      }
              }
      payToScriptInlineDatum
        netId
        (validatorHash activeOperatorsValidator)
        updatedActiveOperatorDatum
        C.NoStakeAddress
        (txOutValue activeOperatorTxOut)

      -- Set the validity bounds.
      addBtx $ \txBody ->
        txBody
          { C.txValidityLowerBound = C.TxValidityLowerBound (C.allegraBasedEra @era) currentSlot
          , C.txValidityUpperBound =
              C.TxValidityUpperBound (C.shelleyBasedEra @era) $ Just validityUpperBoundExclusive
          }
      setMinAdaDepositAll params
    where
      txOutValue (C.TxOut _ val _ _) = C.txOutValueToValue val

mergeToConfirmedState ::
  forall era m.
  ( MonadError String m
  , MonadBlockchain era m
  , MonadUtxoQuery m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , C.IsBabbageBasedEra era
  ) =>
  MidgardScripts ->
  m (TxBuilder era)
mergeToConfirmedState MidgardScripts {stateQueueValidator, stateQueuePolicy} = do
  params <- queryProtocolParameters
  netId <- queryNetworkId
  (currentSlot, _, _) <- querySlotNo
  stateQueueUtxos <-
    utxosByPaymentCredential $
      C.PaymentCredentialByScript $
        validatorHash stateQueueValidator
  (confirmedStateTxIn, (confirmedStateUtxoAnyEra, _)) <-
    maybe (throwError "No confirmed state found") pure $
      findUTxOWithAsset stateQueueUtxos $
        C.AssetId (mintingPolicyId stateQueuePolicy) StateQueue.confirmedStateAssetName
  let confirmedStateTxOut = toTxOut @era confirmedStateUtxoAnyEra
  confirmedStateDatum <-
    maybe (throwError "Invalid confirmed state datum") pure $
      inlineDatumFromUTxO @StateQueue.Datum confirmedStateTxOut
  headerNodeKey <-
    maybe (throwError "No header node linked from confirmed state") pure $
      LinkedList.elementLink confirmedStateDatum
  let headerAssetName = LinkedList.nodeKeyToAssetName StateQueue.blockAssetNamePrefix headerNodeKey
  (headerNodeTxIn, (headerNodeUtxoAnyEra, _)) <-
    maybe (throwError "Linked header node not found") pure $
      findUTxOWithAsset stateQueueUtxos $
        C.AssetId (mintingPolicyId stateQueuePolicy) headerAssetName
  headerNodeDatum <-
    maybe (throwError "Invalid header node datum") pure $
      inlineDatumFromUTxO @StateQueue.Datum (toTxOut @era headerNodeUtxoAnyEra)

  let mergedConfirmedStateDatum :: StateQueue.Datum =
        case (confirmedStateDatum, headerNodeDatum) of
          ( LinkedList.Element {elementData = LinkedList.Root confirmedState}
            , LinkedList.Element
                { elementData = LinkedList.Node header
                , elementLink = remainingLink
                }
            ) ->
              LinkedList.Element
                { elementData =
                    LinkedList.Root $
                      LedgerState.ConfirmedState
                        { headerHash = toBuiltin $ LinkedList.getNodeKey headerNodeKey
                        , prevHeaderHash = confirmedState.headerHash
                        , utxoRoot = header.utxosRoot
                        , startTime = confirmedState.startTime
                        , endTime = header.endTime
                        , protocolVersion = header.protocolVersion
                        }
                , elementLink = remainingLink
                }
          _ -> error "absurd: malformed confirmed-state merge"

  pure . execBuildTx $ do
    -- Constraint: fold the first queued block into the confirmed-state root by
    -- spending both the root and the linked header node.
    spendPlutusInlineDatum confirmedStateTxIn (toValidator stateQueueValidator) ()
    spendPlutusInlineDatum headerNodeTxIn (toValidator stateQueueValidator) ()
    payToScriptInlineDatum
      netId
      (validatorHash stateQueueValidator)
      mergedConfirmedStateDatum
      C.NoStakeAddress
      -- Constraint: reproduce only the confirmed-state root, carrying forward
      -- the root NFT and the merged confirmed-state datum.
      (txOutValue confirmedStateTxOut)
    mintPlutusWithRedeemerFinal
      (toMintingPolicy stateQueuePolicy)
      (mintingPolicyId stateQueuePolicy)
      headerAssetName
      (-1)
      -- Constraint: burn the merged block-header NFT and provide final-body
      -- indices for the root input, header input, and continued root output.
      $ \txBody ->
        StateQueue.MergeToConfirmedState
          { headerNodeKey = toBuiltin $ LinkedList.getNodeKey headerNodeKey
          , headerNodeInputIndex = toInteger $ findIndexSpending headerNodeTxIn txBody
          , confirmedStateInputIndex = toInteger $ findIndexSpending confirmedStateTxIn txBody
          , confirmedStateOutputIndex =
              toInteger $
                findOutputIndexWithAsset
                  (mintingPolicyId stateQueuePolicy)
                  StateQueue.confirmedStateAssetName
                  txBody
          , mSettlementRedeemerIndex = Nothing
          }
    addBtx $ \txBody ->
      -- Constraint: merging requires the tx lower bound to be at or after the
      -- block's maturity threshold; we use the current slot after time has
      -- been advanced by the caller/test.
      txBody
        { C.txValidityLowerBound = C.TxValidityLowerBound (C.allegraBasedEra @era) currentSlot
        }
    setMinAdaDepositAll params
  where
    txOutValue (C.TxOut _ val _ _) = C.txOutValueToValue val
