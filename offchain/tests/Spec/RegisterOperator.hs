module Spec.RegisterOperator (tests) where

import Cardano.Api qualified as C
import Convex.BuildTx (execBuildTx, setMinAdaDepositAll)
import Convex.Class (MonadBlockchain (queryProtocolParameters))
import Convex.CoinSelection (ChangeOutputPosition (TrailingChange))
import Convex.MockChain.CoinSelection
import Convex.MockChain.Defaults qualified as Defaults
import Convex.MockChain.Utils (mockchainSucceeds)
import Convex.Utils (failOnError)
import Convex.Wallet.MockWallet qualified as Wallet
import Midgard.Contracts.Init (initProtocol)
import Midgard.Scripts (MidgardScripts)
import Test.Tasty
import Test.Tasty.HUnit (testCase)

tests :: MidgardScripts -> TestTree
tests ms =
  testGroup
    "register-operator"
    [ testCase "initProtocol" . mockchainSucceeds . failOnError $ do
        let netId = Defaults.networkId
        params <- queryProtocolParameters
        let txBody = execBuildTx $ do
              setMinAdaDepositAll params
              initProtocol @C.ConwayEra netId ms
        balanceAndSubmit mempty Wallet.w1 txBody TrailingChange []
    ]
