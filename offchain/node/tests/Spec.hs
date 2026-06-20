import Test.Tasty (defaultMain, testGroup)

import Spec.Node.DB qualified as NodeDB
import Spec.Node.Server qualified as NodeServer

main :: IO ()
main = defaultMain (testGroup "midgard-node" [NodeDB.tests, NodeServer.tests])
