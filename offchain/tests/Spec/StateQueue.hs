module Spec.StateQueue (tests) where

import Control.Monad (forM, unless)
import Control.Monad.Except (MonadError (throwError), withExceptT)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable (foldl', for_)
import Data.Functor (void)
import Data.Maybe (isJust)

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
import Midgard.Contracts.StateQueue (NewBlock (..), commitBlockHeader)
import Midgard.Contracts.Utils (
  LinkedListInfo (..),
  findFinalUTxONode,
  findUTxOWithAsset,
  hashPlutusData224,
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
    [ stateQueueTestCase ms "commit a block header" [Wallet.w1] $ \refScripts operatorWallets -> do
        let operatorWallet = expectSingleWallet operatorWallets

        LinkedList.Element
          { elementData = LinkedList.Root confirmedStateBefore
          } <-
          currentConfirmedStateDatum ms
        (_, Just (currentOperator, _)) <- withExceptT TxBuildingError $ currentScheduleInfo ms

        let newBlock = sampleNewBlock "1"
            expectedHeader =
              LedgerState.Header
                { prevUtxosRoot = confirmedStateBefore.confirmedUtxoRoot
                , utxosRoot = newBlock.utxosRoot
                , withdrawalsRoot = newBlock.withdrawalsRoot
                , forcedTransactionsRoot = LedgerState.genesisUtxoRoot
                , transactionsRoot = newBlock.transactionsRoot
                , depositsRoot = newBlock.depositsRoot
                , transitionTraceRoot = LedgerState.genesisUtxoRoot
                , eventToStepRoot = LedgerState.genesisUtxoRoot
                , withdrawalCount = 0
                , forcedTransactionCount = 0
                , l2TransactionCount = 0
                , depositCount = 0
                , totalEventCount = 0
                , transitionStepCount = 0
                , startTime = confirmedStateBefore.confirmedEndTime
                , endTime = 0
                , prevHeaderHash = confirmedStateBefore.confirmedHeaderHash
                , operatorVkey = currentOperator
                , protocolVersion = confirmedStateBefore.confirmedProtocolVersion
                }

        (txBody, headerEndTime) <- withExceptT TxBuildingError $ commitBlockHeader ms refScripts newBlock
        void $ balanceAndSubmit' operatorWallet txBody TrailingChange []

        let expectedCommittedHeader = expectedHeader {LedgerState.endTime = headerEndTime}
            expectedHeaderHash = hashPlutusData224 expectedCommittedHeader
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
        unless (isJust activeNodeData.bondUnlockTime) $
          throwError $
            TxBuildingError "Committing a block header should set the active operator bond unlock time"
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

  (txBody, _) <- withExceptT TxBuildingError $ scheduleNextOperator ms
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

sampleRoot :: String -> LedgerState.MidgardLedgerRoot
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

expectSingleWallet :: [Wallet] -> Wallet
expectSingleWallet = \case
  [wallet] -> wallet
  _ -> error "absurd: expected a single operator wallet"

expectHeaderNode :: StateQueue.Datum -> LedgerState.Header
expectHeaderNode LinkedList.Element {elementData = LinkedList.Node StateQueue.StateQueueNode {header}} = header
expectHeaderNode _ = error "Expected a queued header node"
