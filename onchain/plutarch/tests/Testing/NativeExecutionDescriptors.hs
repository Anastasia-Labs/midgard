-- | New binding boundaries evaluated by the exact target Aiken predicates.
module Testing.NativeExecutionDescriptors (tests) where

import Codec.Serialise (deserialiseOrFail)
import Data.Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Midgard.NativeExecutionDescriptor qualified as Descriptor
import Plutarch.Evaluate (applyArguments, evalScriptHuge)
import Plutarch.Internal.Term (Config (NoTracing), InternalConfig (..), compileWithInternalConfig)
import Plutarch.Prelude
import Plutarch.Script (Script)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore.Data qualified as D
import Test.Tasty
import Test.Tasty.HUnit

data Vector = Vector String Bool String
instance FromJSON Vector where
  parseJSON = withObject "native descriptor vector" $ \o -> Vector <$> o .: "test" <*> o .: "accepts" <*> o .: "argumentsCbor"
data Vectors = Vectors String [Vector]
instance FromJSON Vectors where
  parseJSON = withObject "native descriptor vectors" $ \o -> Vectors <$> o .: "sourceCommit" <*> o .: "vectors"

tests :: IO TestTree
tests = do
  Vectors commit vectors <- either fail pure =<< eitherDecodeFileStrict' "tests/fixtures/native-execution-descriptors.json"
  commit @?= "9797ce41ce5d436e309eca07e2020ee29c395859"
  length vectors @?= 11
  length [() | Vector _ True _ <- vectors] @?= 3
  pure $ testGroup "Native execution fixed-target descriptors" [testCase name $ run vector | vector@(Vector name _ _) <- vectors]

run :: Vector -> Assertion
run (Vector _ expected encoded) = do
  bytes <- either assertFailure pure $ Base16.decode $ BS8.pack encoded
  raw <- either (assertFailure . show) pure $ deserialiseOrFail @D.Data $ LBS.fromStrict bytes
  (label, args) <- case raw of
    D.List [D.B label, pre, witness, auxiliary] -> pure (label, [pre, witness, auxiliary, D.I $ if expected then 1 else 0])
    _ -> assertFailure "Malformed source descriptor invocation"
  kernel <- maybe (assertFailure "Unknown source descriptor") pure $ lookup label kernels
  let (result, _, traces) = evalScriptHuge $ applyArguments kernel args
  case result of
    Right _ -> pure ()
    Left err -> assertFailure $ show err <> " " <> show traces

kernels :: [(BS8.ByteString, Script)]
kernels = [("verify_native_scripts_native_descriptor", compiled True), ("verify_native_scripts_effectful_descriptor", compiled False)]

compiled :: Bool -> Script
compiled native = either (error . Text.unpack) id $ compileWithInternalConfig (InternalConfig False False) NoTracing $
  plam $ \(preRaw :: Term s PData) (witnessRaw :: Term s PData) (auxiliaryRaw :: Term s PData) expected ->
    -- Typed arguments captured from the exact target's descriptor tests.
    plet (pfromData $ punsafeCoerce preRaw) $ \pre ->
    plet (pfromData $ punsafeCoerce witnessRaw) $ \witness ->
    plet (pfromData $ punsafeCoerce auxiliaryRaw) $ \auxiliary ->
      pif (((if native then Descriptor.pnative else Descriptor.peffectful) # pre # witness # auxiliary)
        #== (pasInt # expected #== 1)) (pconstant @PUnit ()) perror
