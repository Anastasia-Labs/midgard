-- | New binding boundaries evaluated by the exact target Aiken predicates.
module Testing.PhaseANativeBounds (tests) where

import Codec.Serialise (deserialiseOrFail)
import Data.Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Midgard.FraudProofs.NativeTx.Compact qualified as Native
import Midgard.ValidationMachine qualified as VM
import Midgard.ValidationTrace qualified as Trace
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
  parseJSON = withObject "native binding vector" $ \o -> Vector <$> o .: "test" <*> o .: "accepts" <*> o .: "argumentsCbor"
data Vectors = Vectors String [Vector]
instance FromJSON Vectors where
  parseJSON = withObject "native binding vectors" $ \o -> Vectors <$> o .: "sourceCommit" <*> o .: "vectors"

tests :: IO TestTree
tests = do
  Vectors commit vectors <- either fail pure =<< eitherDecodeFileStrict' "tests/fixtures/phase-a-native-bounds.json"
  commit @?= "9797ce41ce5d436e309eca07e2020ee29c395859"
  length vectors @?= 12
  length [() | Vector _ True _ <- vectors] @?= 5
  pure $ testGroup "Phase-A fixed-target native binding boundaries" [testCase name $ run vector | vector@(Vector name _ _) <- vectors]

run :: Vector -> Assertion
run (Vector _ expected encoded) = do
  bytes <- either assertFailure pure $ Base16.decode $ BS8.pack encoded
  raw <- either (assertFailure . show) pure $ deserialiseOrFail @D.Data $ LBS.fromStrict bytes
  (label, args) <- case raw of
    D.List [D.B label, pre, witness, control] -> pure (label, [pre, witness, control, D.I $ if expected then 1 else 0])
    _ -> assertFailure "Malformed source binding invocation"
  kernel <- maybe (assertFailure "Unknown source binding") pure $ lookup label kernels
  let (result, _, traces) = evalScriptHuge $ applyArguments kernel args
  case result of
    Right _ -> pure ()
    Left err -> assertFailure $ show err <> " " <> show traces

kernels :: [(BS8.ByteString, Script)]
kernels = [("payload", compiled True), ("carried", compiled False)]

compiled :: Bool -> Script
compiled payload = either (error . Text.unpack) id $ compileWithInternalConfig (InternalConfig False False) NoTracing $
  plam $ \(preRaw :: Term s PData) (witnessRaw :: Term s PData) (controlRaw :: Term s PData) expected ->
    -- These typed records are captured from Aiken, not untrusted validator inputs.
    plet (pfromData $ punsafeCoerce preRaw) $ \pre ->
    plet (pfromData $ punsafeCoerce witnessRaw) $ \witness ->
    plet (pfromData $ punsafeCoerce controlRaw) $ \control ->
      pif ((if payload
        then VM.pphaseANativePayloadControlIsBound # pre # witness # control # 1 # 1
        else pmatch pre $ \p -> pmatch control $ \c ->
          pmatch (Native.pverifyNativeTxProofSourceV1 # pfromData (Trace.pmachineState'transactionId p)
            # pfromData (VM.pphaseANative'compactCbor c) # pfromData (VM.pphaseANative'witnessSetCompactCbor c)
            # pfromData (VM.pphaseANative'fieldPreimageLengthsCbor c)) $ \(PPair verified witnessSet) ->
              VM.pphaseANativeControlIsBoundCarried # pre # witness # control # verified # witnessSet)
        #== (pasInt # expected #== 1)) (pconstant @PUnit ()) perror
