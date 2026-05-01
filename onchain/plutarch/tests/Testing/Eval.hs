{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Testing.Eval (psucceeds, passertEval) where

import Cardano.Binary qualified as CBOR
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (
  first,
 )
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short (toShort)
import Data.Char (toLower)
import Data.Text (
  Text,
  pack,
  unpack,
 )
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified
import Data.Word (Word8)
import Plutarch.Evaluate (
  applyArguments,
  evalScript,
  evalScriptHuge,
 )
import Plutarch.Internal.Other (printScript)
import Plutarch.Internal.Term (
  Config (..),
  LogLevel (..),
  TracingMode (..),
  compile,
 )
import Plutarch.Prelude
import Plutarch.Pretty (prettyScript)
import Plutarch.Script (Script, deserialiseScript, serialiseScript)
import PlutusLedgerApi.V2 (BuiltinByteString, Data, ExBudget)
import PlutusTx.Prelude qualified as P
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import Test.Tasty.HUnit

-- | Serializes a compiled script and returns its CBOR as hex text.
-- | Encodes a compiled script as base16 CBOR text.
encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = Text.decodeUtf8 . Base16.encode . CBOR.serialize' . serialiseScript
-- | Evaluates a closed term with the supplied Plutarch config.

-- | Compiles and evaluates a closed term without arguments.
evalT :: Config -> ClosedTerm a -> Either Text (Script, ExBudget, [Text])
-- | Evaluates a closed term after applying the supplied data arguments.
evalT cfg x = evalWithArgsT cfg x []

-- | Compiles and evaluates a closed term with explicit arguments.
evalWithArgsT :: Config -> ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT cfg x args = do
  cmp <- compile cfg x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
-- | Writes the compiled script to disk in cardano-cli JSON format.
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

-- | Writes a compiled term to a Plutus JSON artifact.
writePlutusScript :: Config -> String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScript cfg title filepath term = do
  case evalT cfg term of
    Left e -> putStrLn (show e)
    Right (script, _, _) -> do
      let
        scriptType = "PlutusScriptV2" :: String
-- | Writes a script using tracing with let-bind capture enabled.
        plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
        content = encodePretty plutusJson
      LBS.writeFile filepath content

-- | Writes a script using standard tracing output.
-- | Writes a Plutus script with tracing and let-bind information enabled.
writePlutusScriptTraceBind :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptTraceBind title filepath term =
  writePlutusScript (Tracing LogInfo DoTracingAndBinds) title filepath term
-- | Writes a script with tracing disabled.

-- | Writes a Plutus script with tracing enabled.
writePlutusScriptTrace :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptTrace title filepath term =
-- | Compiles a closed term into a Plutus script or throws on failure.
  writePlutusScript (Tracing LogInfo DoTracing) title filepath term

-- | Writes a Plutus script without tracing.
writePlutusScriptNoTrace :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScriptNoTrace title filepath term =
  writePlutusScript NoTracing title filepath term

-- | Compiles a closed term or throws on failure.
comp :: ClosedTerm a -> Script
comp t = either (error . unpack) id $ compile (Tracing LogInfo DoTracing) t
-- | Asserts that two compiled scripts have the same printed form.

-- | Asserts the term evaluates successfully without failing
psucceeds :: ClosedTerm a -> Assertion
psucceeds p =
-- | Asserts that two closed terms compile and evaluate to the same script.
  case evalScriptHuge $ comp p of
    (Left _, _, trc) -> assertFailure ("Term failed to evaluate: " ++ show trc)
    (Right _, _, _) -> pure ()

-- | Asserts that two compiled scripts render identically.
pscriptShouldBe :: Script -> Script -> Assertion
pscriptShouldBe x y =
  assertEqual "pscriptShouldBe" (printScript x) (printScript y)

-- | Asserts that two closed terms evaluate to the same script.
pshouldBe :: ClosedTerm a -> ClosedTerm b -> Assertion
pshouldBe x y = do
  p1 <- eval $ comp x
  p2 <- eval $ comp y
  pscriptShouldBe p1 p2
  where
    -- | Evaluates a compiled script and lifts interpreter failures into the test.
    eval s = case evalScriptHuge s of
      (Left e, _, _) -> assertFailure $ "Script evaluation failed: " <> show e
      (Right x', _, _) -> pure x'

(#@?=) :: ClosedTerm a -> ClosedTerm b -> Assertion
(#@?=) = pshouldBe

-- | Asserts the term to be true
passertEval :: ClosedTerm a -> Assertion
passertEval p = p #@?= pconstant @PBool True

-- | Asserts that the term evaluates successfully with the given trace sequence
ptraces :: ClosedTerm a -> [Text] -> Assertion
ptraces p develTraces =
  case evalScript $ comp p of
    (Left _, _, _) -> assertFailure "Term failed to evaluate"
    (Right _, _, traceLog) ->
      assertEqual "ptraces: does not match expected" traceLog develTraces

-- | Converts a hex string into a Plutus builtin byte string.
toBuiltinHexString :: String -> BuiltinByteString
toBuiltinHexString = P.toBuiltin . toHexString

-- | Converts a hex string into a raw bytestring.
toHexString :: String -> BS.ByteString
toHexString =
  BS.pack . f
  where
    -- | Recursively decodes pairs of hex digits into bytes.
    f "" = []
    f [_] = error "UnevenLength"
    f (x : y : rest) = (hexDigitToWord8 x * 16 + hexDigitToWord8 y) : f rest

-- | Decodes a single hexadecimal digit into a byte nibble.
hexDigitToWord8 :: (HasCallStack) => Char -> Word8
hexDigitToWord8 = f . toLower
  where
    -- | Decodes a lowercase hexadecimal digit into its numeric value.
    f :: Char -> Word8
    f '0' = 0
    f '1' = 1
    f '2' = 2
    f '3' = 3
    f '4' = 4
    f '5' = 5
    f '6' = 6
    f '7' = 7
    f '8' = 8
    f '9' = 9
    f 'a' = 10
    f 'b' = 11
    f 'c' = 12
    f 'd' = 13
    f 'e' = 14
    f 'f' = 15
    f c = error ("InvalidHexDigit " <> [c])

-- | Writes raw script bytes to disk.
writeScriptBytesFile :: FilePath -> BS.ByteString -> IO ()
writeScriptBytesFile path script = do
  let scriptBytes = deserialiseScript (toShort script)
      renderedScript = renderString $ layoutPretty defaultLayoutOptions (prettyScript scriptBytes)
  Data.Text.IO.writeFile path (T.pack renderedScript)
