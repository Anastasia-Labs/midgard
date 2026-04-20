import Test.Tasty

import Midgard.Scripts (MidgardScripts, readAikenScripts)

import Spec.Operators qualified as Operators
import Spec.Scheduler qualified as Scheduler
import Spec.StateQueue qualified as StateQueue

tests :: MidgardScripts -> TestTree
tests ms = testGroup "tests" [Operators.tests ms, Scheduler.tests ms, StateQueue.tests ms]

main :: IO ()
main = do
  ms <- readAikenScripts
  defaultMain $ tests ms
