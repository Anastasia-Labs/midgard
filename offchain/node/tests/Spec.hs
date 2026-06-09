import Test.Tasty (defaultMain)

import Spec.Node.DB qualified as NodeDB

main :: IO ()
main = defaultMain NodeDB.tests
