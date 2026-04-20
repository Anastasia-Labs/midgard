module Spec.StateQueue (tests) where

import Control.Monad (forM, unless)
import Control.Monad.Except (MonadError (throwError), withExceptT)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (foldl', for_)
import Data.Functor (void)
import Data.Maybe (isNothing)

import Cardano.Api qualified as C
import Convex.Class (MonadUtxoQuery, nextSlot, setPOSIXTime, utxosByPaymentCredential)
import Convex.CoinSelection (ChangeOutputPosition (TrailingChange))
import Convex.MockChain (MockchainT)
import Convex.Utxos (toTxOut)
import Convex.Wallet (Wallet)
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet qualified as Wallet
import PlutusLedgerApi.Common qualified as PlutusTx
import Test.Tasty

import Midgard.Contracts.ActiveOperators (activateOperator)
import Midgard.Contracts.RegisteredOperators (registerOperator)
import Midgard.Contracts.Scheduler (currentScheduleInfo, scheduleNextOperator)
import Midgard.Contracts.StateQueue (NewBlock (..), commitBlockHeader, mergeToConfirmedState)
import Midgard.Contracts.Utils (
  LinkedListInfo (..),
  findFinalUTxONode,
  findUTxOWithAsset,
  hashPlutusData,
  inlineDatumFromUTxO,
  listAssetNameFromUTxO,
 )
import Midgard.ScriptUtils (mintingPolicyId, validatorHash)
import Midgard.Scripts (
  MidgardRefScripts,
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

import Spec.Types (TestTxError (TxBuildingError))
import Spec.Utils (balanceAndSubmit', midgardTestCase)

tests :: MidgardScripts -> TestTree
tests ms =
  testGroup
    "state-queue"
    [ stateQueueTestCase ms "commit a block header" [Wallet.w1] $ \_ operatorWallets -> do
        let operatorWallet = expectSingleWallet operatorWallets

        LinkedList.Element
          { elementData = LinkedList.Root confirmedStateBefore
          } <-
          currentConfirmedStateDatum ms
        (_, currentOperator, _) <- withExceptT TxBuildingError $ currentScheduleInfo ms

        let newBlock = sampleNewBlock "1"
            expectedHeader =
              LedgerState.Header
                { prevUtxosRoot = confirmedStateBefore.confirmedUtxoRoot
                , utxosRoot = newBlock.utxosRoot
                , transactionsRoot = newBlock.transactionsRoot
                , depositsRoot = newBlock.depositsRoot
                , withdrawalsRoot = newBlock.withdrawalsRoot
                , startTime = confirmedStateBefore.confirmedEndTime
                , endTime = 0
                , prevHeaderHash = confirmedStateBefore.confirmedHeaderHash
                , operatorVkey = currentOperator
                , protocolVersion = confirmedStateBefore.confirmedProtocolVersion
                }

        (txBody, headerEndTime) <- withExceptT TxBuildingError $ commitBlockHeader ms newBlock
        void $ balanceAndSubmit' operatorWallet txBody TrailingChange []

        let expectedCommittedHeader = expectedHeader {LedgerState.endTime = headerEndTime}
            expectedHeaderHash = hashPlutusData expectedCommittedHeader
            expectedHeaderAssetName =
              C.UnsafeAssetName $ StateQueue.blockAssetNamePrefix <> expectedHeaderHash

        LinkedList.Element
          { elementData = LinkedList.Root confirmedStateAfterCommit
          , elementLink = confirmedStateLink
          } <-
          currentConfirmedStateDatum ms
        (finalNodeAssetName, finalNodeDatum) <- currentFinalStateQueueNode ms
        activeNodeData <- currentActiveOperatorNodeData ms operatorWallet

        let committedHeader = expectHeaderNode finalNodeDatum

        unless (confirmedStateAfterCommit == confirmedStateBefore) $
          throwError $
            TxBuildingError "Confirmed state should remain unchanged after committing a block header"
        unless (fmap LinkedList.getNodeKey confirmedStateLink == Just expectedHeaderHash) $
          throwError $
            TxBuildingError "Confirmed state should point at the newly queued block header"
        unless (finalNodeAssetName == expectedHeaderAssetName) $
          throwError $
            TxBuildingError "Final state queue node should carry the new block header NFT"
        unless (committedHeader == expectedCommittedHeader) $
          throwError $
            TxBuildingError "Committed block header datum does not match the expected queued header"
        unless (activeNodeData.bondUnlockTime /= Nothing) $
          throwError $
            TxBuildingError "Committing a block header should set the active operator bond unlock time"
    , stateQueueTestCase ms "merge a queued block into confirmed state" [Wallet.w1] $ \_ operatorWallets -> do
        let operatorWallet = expectSingleWallet operatorWallets

        LinkedList.Element
          { elementData = LinkedList.Root confirmedStateBefore
          } <-
          currentConfirmedStateDatum ms
        (_, currentOperator, _) <- withExceptT TxBuildingError $ currentScheduleInfo ms

        let newBlock = sampleNewBlock "2"
            expectedHeader =
              LedgerState.Header
                { prevUtxosRoot = confirmedStateBefore.confirmedUtxoRoot
                , utxosRoot = newBlock.utxosRoot
                , transactionsRoot = newBlock.transactionsRoot
                , depositsRoot = newBlock.depositsRoot
                , withdrawalsRoot = newBlock.withdrawalsRoot
                , startTime = confirmedStateBefore.confirmedEndTime
                , endTime = 0
                , prevHeaderHash = confirmedStateBefore.confirmedHeaderHash
                , operatorVkey = currentOperator
                , protocolVersion = confirmedStateBefore.confirmedProtocolVersion
                }

        (txBody, headerEndTime) <- withExceptT TxBuildingError $ commitBlockHeader ms newBlock
        void $ balanceAndSubmit' operatorWallet txBody TrailingChange []

        let expectedCommittedHeader = expectedHeader {LedgerState.endTime = headerEndTime}
            expectedHeaderHash = hashPlutusData expectedCommittedHeader
            expectedHeaderAssetName =
              C.UnsafeAssetName $ StateQueue.blockAssetNamePrefix <> expectedHeaderHash

        setPOSIXTime headerEndTime
        nextSlot

        txBody <- withExceptT TxBuildingError $ mergeToConfirmedState ms
        void $ balanceAndSubmit' operatorWallet txBody TrailingChange []

        LinkedList.Element
          { elementData = LinkedList.Root confirmedStateAfterMerge
          , elementLink = mergedLink
          } <-
          currentConfirmedStateDatum ms
        (finalNodeAssetName, finalNodeDatum) <- currentFinalStateQueueNode ms

        let expectedMergedConfirmedState =
              LedgerState.ConfirmedState
                { confirmedHeaderHash = PlutusTx.toBuiltin expectedHeaderHash
                , confirmedPrevHeaderHash = confirmedStateBefore.confirmedHeaderHash
                , confirmedUtxoRoot = newBlock.utxosRoot
                , confirmedStartTime = confirmedStateBefore.confirmedStartTime
                , confirmedEndTime = headerEndTime
                , confirmedProtocolVersion = confirmedStateBefore.confirmedProtocolVersion
                }

        unless (confirmedStateAfterMerge == expectedMergedConfirmedState) $
          throwError $
            TxBuildingError "Confirmed state was not updated with the merged block header"
        unless (isNothing mergedLink) $
          throwError $
            TxBuildingError "Confirmed state should no longer point at a queued block after merge"
        unless (finalNodeAssetName == StateQueue.confirmedStateAssetName) $
          throwError $
            TxBuildingError "Final state queue node should collapse back to the confirmed-state root after merge"
        unless (isConfirmedStateRoot finalNodeDatum) $
          throwError $
            TxBuildingError "Final state queue datum should be the confirmed-state root after merge"
        unlessM (not <$> stateQueueHasAsset ms expectedHeaderAssetName) $
          throwError $
            TxBuildingError "Merged block header NFT should be burned after the merge"
    ]

stateQueueTestCase ::
  MidgardScripts ->
  TestName ->
  [Wallet] ->
  (MidgardRefScripts -> [Wallet] -> C.ExceptT (TestTxError C.ConwayEra) (MockchainT C.ConwayEra IO) ()) ->
  TestTree
stateQueueTestCase ms msg wallets act = midgardTestCase ms msg $ \refScripts -> do
  activationTimes <- forM wallets $ \wallet -> do
    (txBody, activationTime) <-
      withExceptT TxBuildingError
        . registerOperator ms refScripts
        $ Wallet.verificationKeyHash wallet
    void $ balanceAndSubmit' wallet txBody TrailingChange []
    nextSlot
    pure activationTime

  setPOSIXTime $ foldl' max 0 activationTimes
  nextSlot

  for_ wallets $ \wallet -> do
    txBody <-
      withExceptT TxBuildingError
        . activateOperator ms refScripts
        $ Wallet.verificationKeyHash wallet
    void $ balanceAndSubmit' wallet txBody TrailingChange []

  (txBody, _) <- withExceptT TxBuildingError $ scheduleNextOperator False ms
  void $ balanceAndSubmit' (expectSingleWallet wallets) txBody TrailingChange []

  act refScripts wallets

sampleNewBlock :: String -> NewBlock
sampleNewBlock suffix =
  NewBlock
    { utxosRoot = sampleRoot $ "utxos-" <> suffix
    , transactionsRoot = sampleRoot $ "txs-" <> suffix
    , depositsRoot = sampleRoot $ "deposits-" <> suffix
    , withdrawalsRoot = sampleRoot $ "withdrawals-" <> suffix
    }

sampleRoot :: String -> LedgerState.MerkleRoot
sampleRoot = PlutusTx.toBuiltin . BS8.pack

currentConfirmedStateDatum ::
  (MonadUtxoQuery m) =>
  MidgardScripts ->
  m StateQueue.Datum
currentConfirmedStateDatum MidgardScripts {stateQueueValidator, stateQueuePolicy} = do
  stateQueueUtxos <-
    utxosByPaymentCredential $
      C.PaymentCredentialByScript $
        validatorHash stateQueueValidator
  let confirmedStateTxOut = do
        (_, (confirmedStateUtxoAnyEra, _)) <-
          findUTxOWithAsset stateQueueUtxos $
            C.AssetId (mintingPolicyId stateQueuePolicy) StateQueue.confirmedStateAssetName
        pure $ toTxOut @C.ConwayEra confirmedStateUtxoAnyEra
  case confirmedStateTxOut >>= inlineDatumFromUTxO @StateQueue.Datum of
    Just datum -> pure datum
    Nothing -> error "Confirmed state datum not found"

currentFinalStateQueueNode ::
  (MonadUtxoQuery m) =>
  MidgardScripts ->
  m (C.AssetName, StateQueue.Datum)
currentFinalStateQueueNode MidgardScripts {stateQueueValidator, stateQueuePolicy} = do
  stateQueueUtxos <-
    utxosByPaymentCredential $
      C.PaymentCredentialByScript $
        validatorHash stateQueueValidator
  let finalNodeInfo = do
        (_, (finalNodeUtxoAnyEra, _)) <-
          runReaderT
            (findFinalUTxONode stateQueueUtxos)
            LinkedListInfo
              { ownerPolicyId = mintingPolicyId stateQueuePolicy
              , rootAssetName = StateQueue.confirmedStateAssetName
              , nodeAssetNamePrefix = StateQueue.blockAssetNamePrefix
              }
        let finalNodeTxOut = toTxOut @C.ConwayEra finalNodeUtxoAnyEra
        assetName <- listAssetNameFromUTxO (mintingPolicyId stateQueuePolicy) finalNodeTxOut
        datum <- inlineDatumFromUTxO @StateQueue.Datum finalNodeTxOut
        pure (assetName, datum)
  case finalNodeInfo of
    Just info -> pure info
    Nothing -> error "Final state queue node not found"

currentActiveOperatorNodeData ::
  (MonadUtxoQuery m) =>
  MidgardScripts ->
  Wallet ->
  m ActiveOperators.NodeData
currentActiveOperatorNodeData MidgardScripts {activeOperatorsValidator, activeOperatorsPolicy} wallet = do
  activeOperatorUtxos <-
    utxosByPaymentCredential $
      C.PaymentCredentialByScript $
        validatorHash activeOperatorsValidator
  let activeOperatorAssetName =
        C.UnsafeAssetName $
          ActiveOperators.nodeAssetNamePrefix <> C.serialiseToRawBytes (Wallet.verificationKeyHash wallet)
      activeOperatorTxOut = do
        (_, (activeOperatorUtxoAnyEra, _)) <-
          findUTxOWithAsset activeOperatorUtxos $
            C.AssetId (mintingPolicyId activeOperatorsPolicy) activeOperatorAssetName
        pure $ toTxOut @C.ConwayEra activeOperatorUtxoAnyEra
      activeOperatorDatum = activeOperatorTxOut >>= inlineDatumFromUTxO @ActiveOperators.Datum
  case activeOperatorDatum of
    Just LinkedList.Element {elementData = LinkedList.Node nodeData} -> pure nodeData
    _ -> error "Active operator node datum not found"

stateQueueHasAsset ::
  (MonadUtxoQuery m) =>
  MidgardScripts ->
  C.AssetName ->
  m Bool
stateQueueHasAsset MidgardScripts {stateQueueValidator, stateQueuePolicy} assetName = do
  stateQueueUtxos <-
    utxosByPaymentCredential $
      C.PaymentCredentialByScript $
        validatorHash stateQueueValidator
  pure $
    case findUTxOWithAsset stateQueueUtxos $
      C.AssetId (mintingPolicyId stateQueuePolicy) assetName of
      Just _ -> True
      Nothing -> False

expectSingleWallet :: [Wallet] -> Wallet
expectSingleWallet = \case
  [wallet] -> wallet
  _ -> error "absurd: expected a single operator wallet"

expectHeaderNode :: StateQueue.Datum -> LedgerState.Header
expectHeaderNode LinkedList.Element {elementData = LinkedList.Node header} = header
expectHeaderNode _ = error "Expected a queued header node"

isConfirmedStateRoot :: StateQueue.Datum -> Bool
isConfirmedStateRoot LinkedList.Element {elementData = LinkedList.Root _} = True
isConfirmedStateRoot _ = False

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM predicate action = flip unless action =<< predicate
