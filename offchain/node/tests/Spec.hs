import Test.Tasty (defaultMain, testGroup)

import Spec.Node.DB qualified as NodeDB

main :: IO ()
main = defaultMain (testGroup "midgard-node" [NodeDB.tests])
