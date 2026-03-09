import Test.Tasty

import Midgard.Scripts (MidgardScripts, readAikenScripts)

import Spec.ActiveOperators qualified as ActiveOperators
import Spec.RegisteredOperators qualified as RegisteredOperators

tests :: MidgardScripts -> TestTree
tests ms = testGroup "tests" [RegisteredOperators.tests ms, ActiveOperators.tests ms]

main :: IO ()
main = do
  ms <- readAikenScripts
  defaultMain $ tests ms
