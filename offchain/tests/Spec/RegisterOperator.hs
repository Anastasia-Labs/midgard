module Spec.RegisterOperator (tests) where

import Midgard.Scripts (MidgardScripts)
import Test.Tasty

import Spec.Utils (midgardTestCase)

tests :: MidgardScripts -> TestTree
tests ms =
  testGroup
    "register-operator"
    [ midgardTestCase @String ms "register-operator" $ do
        pure ()
    ]
