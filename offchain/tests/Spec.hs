module Spec (main) where

import Test.Tasty

tests :: TestTree
tests = testGroup "tests" []

main :: IO ()
main = defaultMain tests
