module Spec.RegisterOperator (tests) where

import Midgard.Scripts (MidgardScripts)
import Test.Tasty

import Cardano.Api qualified as C
import Control.Monad.Except (withExceptT)
import Convex.CoinSelection (ChangeOutputPosition (TrailingChange))
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet qualified as Wallet
import Data.Functor (void)
import Midgard.Contracts.RegisteredOperators
import Spec.Types (TestTxError (TxBuildingError))
import Spec.Utils (balanceAndSubmit', midgardTestCase)

tests :: MidgardScripts -> TestTree
tests ms =
  testGroup
    "register-operator"
    [ midgardTestCase ms "register-operator" $ \refScripts -> do
        pure ()
    ]
