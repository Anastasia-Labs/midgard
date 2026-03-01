import Test.Tasty

import Midgard.Scripts (MidgardScripts, readAikenScripts)
import Spec.RegisterOperator qualified as RegisterOperator

tests :: MidgardScripts -> TestTree
tests ms = testGroup "tests" [RegisterOperator.tests ms]

main :: IO ()
main = do
  ms <- readAikenScripts
  defaultMain $ tests ms
