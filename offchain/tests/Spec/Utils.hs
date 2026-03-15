module Spec.Utils (midgardTestCase, balanceAndSubmit') where

import Control.Monad.Except (MonadError (throwError), withExceptT)
import Data.Functor (void)

import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (testCase)

import Cardano.Api qualified as C
import Convex.BuildTx
import Convex.Class
import Convex.CoinSelection
import Convex.MockChain
import Convex.MockChain.CoinSelection
import Convex.MockChain.Utils (mockchainSucceeds)
import Convex.Utils (failOnError)
import Convex.Wallet (Wallet)
import Convex.Wallet.MockWallet qualified as Wallet
import Midgard.Contracts.Init

import Midgard.Scripts (MidgardRefScripts (..), MidgardScripts (..))

import Spec.Types (TestTxError (SubmissionError, TxBalancingError))

-- | Similar to 'testCase' but initializes the midgard protocol as setup.
midgardTestCase ::
  MidgardScripts ->
  TestName ->
  (MidgardRefScripts -> C.ExceptT (TestTxError C.ConwayEra) (MockchainT C.ConwayEra IO) ()) ->
  TestTree
midgardTestCase ms str act = testCase str . mockchainSucceeds . failOnError $ do
  -- Pre-publish ref scripts. Must be done one by one to prevent tx size getting too big.
  txBody <- publishMidgardMintingPolicy $ registeredOperatorsPolicy ms
  tx <- balanceAndSubmit' Wallet.w1 txBody TrailingChange []
  let registeredOperatorsPolicyRef = firstTxIn tx
  txBody <- publishMidgardMintingPolicy $ activeOperatorsPolicy ms
  tx <- balanceAndSubmit' Wallet.w1 txBody TrailingChange []
  let activeOperatorsPolicyRef = firstTxIn tx
  txBody <- publishMidgardMintingPolicy $ retiredOperatorsPolicy ms
  tx <- balanceAndSubmit' Wallet.w1 txBody TrailingChange []
  let retiredOperatorsPolicyRef = firstTxIn tx
  let refScripts = MidgardRefScripts {retiredOperatorsPolicyRef, activeOperatorsPolicyRef, registeredOperatorsPolicyRef}
  -- Initialize protocol.
  txBody <- initProtocol ms refScripts
  void $ balanceAndSubmit' Wallet.w1 txBody TrailingChange []
  -- Run the user action.
  act refScripts

{- | Saner version of balanceAndSubmit' that actually handles the SendTxError in a typed manner.
No, 'tryBalanceAndSubmit' is not the sane version because it uses 'MonadFail'.
-}
balanceAndSubmit' ::
  forall era m.
  (MonadMockchain era m, C.IsBabbageBasedEra era) =>
  Wallet ->
  TxBuilder era ->
  ChangeOutputPosition ->
  [C.ShelleyWitnessSigningKey] ->
  C.ExceptT (TestTxError era) m (C.Tx era)
balanceAndSubmit' wallet txBody changeStyle signers =
  withExceptT TxBalancingError (balanceAndSubmit mempty wallet txBody changeStyle signers)
    >>= either (throwError . SubmissionError) pure

firstTxIn :: C.Tx era -> C.TxIn
firstTxIn tx = C.TxIn (C.getTxId $ C.getTxBody tx) $ C.TxIx 0
