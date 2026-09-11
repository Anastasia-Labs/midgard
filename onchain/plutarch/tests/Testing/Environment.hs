module Testing.Environment (tests) where

import Midgard.Env
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Testing.Eval (passertEval)

tests :: TestTree
tests =
  testGroup
    "Compiled Aiken environment"
    [ testCase "economic constants match the selected source environment" $
        passertEval $
          let expected :: (Integer, Integer, Integer, Integer)
              expected = case environmentName of
                "default" -> (25_000_000_000, 75_000_000_000, 10_000_000_000, 30)
                "testnet" -> (500_000_000, 400_000_000, 100_000_000, 3_600_000)
                name -> error $ "Unknown compiled environment: " <> name
              (slash, reward, inactivity, shift) = expected
           in pand'List
                [ pslashingPenalty #== pconstant slash
                , pfraudProverReward #== pconstant reward
                , prequiredBond #== pconstant (slash + reward)
                , pinactivitySlashingPenalty #== pconstant inactivity
                , pshiftDuration #== pconstant shift
                , pcoinsPerUtxoByte #== 4_310
                ]
    ]
