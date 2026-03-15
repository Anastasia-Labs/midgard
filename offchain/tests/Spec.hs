import Test.Tasty

import Midgard.Scripts (MidgardScripts, readAikenScripts)

import Spec.Operators qualified as Operators

tests :: MidgardScripts -> TestTree
tests ms = testGroup "tests" [Operators.tests ms]

main :: IO ()
main = do
  ms <- readAikenScripts
  defaultMain $ tests ms
