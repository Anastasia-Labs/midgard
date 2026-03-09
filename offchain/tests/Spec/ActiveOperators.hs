module Spec.ActiveOperators (tests) where

import Control.Monad.Except (withExceptT)
import Data.Functor (void)

import Convex.CoinSelection (ChangeOutputPosition (TrailingChange))
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet qualified as Wallet
import Test.Tasty

import Midgard.Contracts.ActiveOperators (activateOperator)
import Midgard.Contracts.RegisteredOperators (registerOperator)
import Midgard.Scripts (MidgardScripts)

import Spec.Types (TestTxError (TxBuildingError))
import Spec.Utils (balanceAndSubmit', midgardTestCase)

tests :: MidgardScripts -> TestTree
tests ms =
  testGroup
    "active-operators"
    [ midgardTestCase ms "activate a registered operator" $ \refScripts -> do
        let operatorWallet = Wallet.w1
            operatorPkh = Wallet.verificationKeyHash operatorWallet
        txBody <- withExceptT TxBuildingError $ registerOperator ms refScripts operatorPkh
        void $ balanceAndSubmit' operatorWallet txBody TrailingChange []
        txBody <- withExceptT TxBuildingError $ activateOperator ms refScripts operatorPkh
        void $ balanceAndSubmit' operatorWallet txBody TrailingChange []
    ]
