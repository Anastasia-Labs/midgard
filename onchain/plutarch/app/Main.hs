{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE ImpredicativeTypes #-}

module Main (main) where

import Cardano.Binary qualified as CBOR
import Control.Monad (forM_)
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (
  first,
 )
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Default (def)
import Data.Text (
  Text,
  pack,
 )
import Data.Text.Encoding qualified as Text
import Plutarch (
  Config (..),
  TracingMode (..),
  LogLevel (..), 
  compile,
  printScript,
 )
import Plutarch.Evaluate (
  evalScript,
  applyArguments
 )
import Plutarch.LedgerApi.V3 (PScriptContext)
import Plutarch.Prelude
import Plutarch.Script (Script, serialiseScript)
import PlutusLedgerApi.V2 (
  Data,
  ExBudget,
 )
import System.IO

encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = Text.decodeUtf8 . Base16.encode . CBOR.serialize' . serialiseScript

evalT :: Config -> ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT cfg x = evalWithArgsT cfg x []

evalWithArgsT :: Config -> ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT cfg x args = do
  cmp <- compile cfg x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

-- writePlutusScript :: String -> FilePath -> ClosedTerm a -> IO ()
-- writePlutusScript title filepath term = do
--   case evalT term of
--     Left e -> putStrLn (show e)
--     Right (script, _, _) -> do
--       let
--         scriptType = "PlutusScriptV2" :: String
--         plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
--         content = encodePretty plutusJson
--       LBS.writeFile filepath content

writePlutusScript :: Config -> String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScript cfg title filepath term = do
  case evalT cfg term of
    Left e -> putStrLn (show e)
    Right (script, _, _) -> do
      let
        scriptType = "PlutusScriptV2" :: String
        plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
        content = encodePretty plutusJson
      LBS.writeFile filepath content

writePlutusScriptTraceBind :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptTraceBind title filepath term =
  writePlutusScript (Tracing LogInfo DoTracingAndBinds) title filepath term

writePlutusScriptTrace :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptTrace title filepath term =
  writePlutusScript (Tracing LogInfo DoTracing) title filepath term

writePlutusScriptNoTrace :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptNoTrace title filepath term =
  writePlutusScript NoTracing title filepath term

scripts :: [(String, ClosedTerm (PScriptContext :--> PUnit))]
scripts = []

main :: IO ()
main = do
  putStrLn "Writing Plutus Scripts to files"
  forM_ scripts $ \(name, script) -> do
    writePlutusScriptNoTrace name ("./compiled/" <> name <> ".json") script