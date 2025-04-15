{-# LANGUAGE ImpredicativeTypes #-}

module Main (main) where

import Control.Monad (forM_)
import Midgard.Utils (writePlutusScriptNoTrace)
import Plutarch.LedgerApi.V3 (PScriptContext)
import Plutarch.Prelude (ClosedTerm, PUnit, type (:-->))

scripts :: [(String, ClosedTerm (PScriptContext :--> PUnit))]
scripts = []

main :: IO ()
main = do
  putStrLn "Writing Plutus Scripts to files"
  forM_ scripts $ \(name, script) -> do
    writePlutusScriptNoTrace name ("./compiled/" <> name <> ".json") script
