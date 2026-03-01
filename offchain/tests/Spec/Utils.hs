module Spec.Utils (midgardTestCase) where

import Test.Tasty (TestName, TestTree, withResource)
import Test.Tasty.HUnit (testCase)

import Cardano.Api qualified as C
import Convex.BuildTx
import Convex.Class
import Convex.CoinSelection
import Convex.MockChain.CoinSelection
import Convex.MockChain.Defaults qualified as Defaults
import Convex.MockChain.Utils (mockchainSucceeds)
import Convex.Utils (failOnError)
import Convex.Wallet.MockWallet qualified as Wallet
import Midgard.Contracts.Init

import Convex.MockChain
import Midgard.Scripts (MidgardScripts)

-- | Similar to 'testCase' but initializes the midgard protocol as setup.
midgardTestCase :: (Show e) => MidgardScripts -> TestName -> C.ExceptT e (MockchainT C.ConwayEra IO) () -> TestTree
midgardTestCase ms str t =
  withResource ioProtocolInit pure
    . const
    $ testCase str
      . mockchainSucceeds
    $ failOnError t
  where
    ioProtocolInit = mockchainSucceeds . failOnError $ do
      let netId = Defaults.networkId
      params <- queryProtocolParameters
      let txBody = execBuildTx $ do
            setMinAdaDepositAll params
            initProtocol @C.ConwayEra netId ms
      balanceAndSubmit mempty Wallet.w1 txBody TrailingChange []
