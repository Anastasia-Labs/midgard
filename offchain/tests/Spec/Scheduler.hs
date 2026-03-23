module Spec.Scheduler (tests) where

import Control.Monad (forM, unless)
import Control.Monad.Except (MonadError (throwError), withExceptT)
import Data.Foldable (Foldable (foldl'), for_, maximumBy)
import Data.Functor (void)

import Cardano.Api qualified as C
import Convex.Class (MonadUtxoQuery, nextSlot, setPOSIXTime, utxosByPaymentCredential)
import Convex.CoinSelection (ChangeOutputPosition (TrailingChange))
import Convex.MockChain (MockchainT)
import Convex.Utxos (toTxOut)
import Convex.Wallet (Wallet)
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet qualified as Wallet
import Test.Tasty

import Midgard.Contracts.ActiveOperators (activateOperator)
import Midgard.Contracts.RegisteredOperators (registerOperator)
import Midgard.Contracts.Scheduler (scheduleNextOperator)
import Midgard.Contracts.Utils (findUTxOWithAsset, inlineDatumFromUTxO)
import Midgard.ScriptUtils (mintingPolicyId, validatorHash)
import Midgard.Scripts (
  MidgardRefScripts,
  MidgardScripts (MidgardScripts, schedulerPolicy, schedulerValidator),
 )
import Midgard.Types.Scheduler qualified as Scheduler

import Data.Function (on)
import Spec.Types (TestTxError (TxBuildingError))
import Spec.Utils (balanceAndSubmit', midgardTestCase)

tests :: MidgardScripts -> TestTree
tests ms =
  testGroup
    "scheduler"
    [ schedulerTestCase ms "schedule the first operator" [Wallet.w1] $ \_ operatorWallets -> do
        (txBody, _) <- withExceptT TxBuildingError $ scheduleNextOperator False ms
        void $ balanceAndSubmit' (head operatorWallets) txBody TrailingChange []
    , schedulerTestCase ms "schedule next operator before next shift" [Wallet.w1, Wallet.w2] $ \_ operatorWallets -> do
        let (operatorWallet1, operatorWallet2) = case operatorWallets of
              [operatorWallet1, operatorWallet2] -> (operatorWallet1, operatorWallet2)
              _ -> error "absurd: operatorWallets should match structure of the wallets passed"
        -- We need to figure out who's being scheduled first: whoever's PKH is ordered higher.
        let firstOperator = maximumBy (compare `on` Wallet.verificationKeyHash) [Wallet.w1, Wallet.w2]
        (txBody, _) <- withExceptT TxBuildingError $ scheduleNextOperator False ms
        -- Note: It doesn't matter who submits the transaction as long as the shift has ended
        -- for the existing one.
        void $ balanceAndSubmit' operatorWallet1 txBody TrailingChange []
        -- Now, we're going to schedule the next operator before shift end.
        (txBody, _) <- withExceptT TxBuildingError $ scheduleNextOperator True ms
        -- Existing operator must sign off on this.
        void $
          balanceAndSubmit'
            operatorWallet2
            txBody
            TrailingChange
            [C.WitnessPaymentKey $ Wallet.getWallet firstOperator]
    , schedulerTestCase ms "rewind back to the start" [Wallet.w1, Wallet.w2] $ \_ operatorWallets -> do
        let (operatorWallet1, operatorWallet2) = case operatorWallets of
              [operatorWallet1, operatorWallet2] -> (operatorWallet1, operatorWallet2)
              _ -> error "absurd: operatorWallets should match structure of the wallets passed"

        (txBody, nextShiftStartTime) <- withExceptT TxBuildingError $ scheduleNextOperator False ms
        -- Note: It doesn't matter who submits the transaction as long as the shift has ended
        -- for the existing one.
        void $ balanceAndSubmit' operatorWallet1 txBody TrailingChange []
        Scheduler.Datum {operator = firstOperator} <- currentSchedulerDatum ms
        -- Advance to the next shift.
        setPOSIXTime nextShiftStartTime
        nextSlot

        (txBody, nextShiftStartTime) <- withExceptT TxBuildingError $ scheduleNextOperator False ms
        void $ balanceAndSubmit' operatorWallet2 txBody TrailingChange []
        Scheduler.Datum {operator = secondOperator} <- currentSchedulerDatum ms

        unless (firstOperator /= secondOperator) $
          throwError $
            TxBuildingError "Must schedule a different operators"
        -- Advance to the next shift.
        setPOSIXTime nextShiftStartTime
        nextSlot

        (txBody, _) <- withExceptT TxBuildingError $ scheduleNextOperator False ms
        void $ balanceAndSubmit' operatorWallet1 txBody TrailingChange []
        Scheduler.Datum {operator = thirdOperator} <- currentSchedulerDatum ms

        -- This time we should have rewinded back to the first operator since there's only two operators.
        unless (firstOperator == thirdOperator) $
          throwError $
            TxBuildingError "Must rewind back to the first operator"
    ]

-- | Set up a scheduler test case by registering the given wallets as operators and activating them.
schedulerTestCase ::
  MidgardScripts ->
  TestName ->
  [Wallet] ->
  (MidgardRefScripts -> [Wallet] -> C.ExceptT (TestTxError C.ConwayEra) (MockchainT C.ConwayEra IO) ()) ->
  TestTree
schedulerTestCase ms msg wallets act = midgardTestCase ms msg $ \refScripts -> do
  -- Register the operator to schedule.
  activationTimes <- forM wallets $ \wallet -> do
    (txBody, activationTime) <-
      withExceptT TxBuildingError
        . registerOperator ms refScripts
        $ Wallet.verificationKeyHash wallet
    void $ balanceAndSubmit' wallet txBody TrailingChange []
    pure activationTime

  -- Advance to a slot when we can activate all operators.
  setPOSIXTime $ foldl' max 0 activationTimes
  nextSlot

  -- Activate operators.
  for_ wallets $ \wallet -> do
    txBody <-
      withExceptT TxBuildingError
        . activateOperator ms refScripts
        $ Wallet.verificationKeyHash wallet
    balanceAndSubmit' wallet txBody TrailingChange []

  act refScripts wallets

currentSchedulerDatum ::
  (MonadUtxoQuery m) =>
  MidgardScripts ->
  m Scheduler.Datum
currentSchedulerDatum MidgardScripts {schedulerValidator, schedulerPolicy} = do
  schedulerUtxos <-
    utxosByPaymentCredential $
      C.PaymentCredentialByScript $
        validatorHash schedulerValidator
  let schedulerTxOut = do
        (_, (schedulerUtxoAnyEra, _)) <-
          findUTxOWithAsset schedulerUtxos $
            C.AssetId (mintingPolicyId schedulerPolicy) Scheduler.assetName
        pure $ toTxOut @C.ConwayEra schedulerUtxoAnyEra
  case schedulerTxOut >>= inlineDatumFromUTxO @Scheduler.Datum of
    Just datum -> pure datum
    Nothing -> error "Scheduler datum not found"
