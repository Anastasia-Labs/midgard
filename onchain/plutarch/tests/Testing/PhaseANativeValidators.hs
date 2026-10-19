-- | Exact fixed-target Aiken native-item spending and rewarding invocations.
module Testing.PhaseANativeValidators (tests) where

import Codec.Serialise (deserialiseOrFail)
import Data.Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativeScripts qualified as Native
import Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativeItemYields qualified as Yields
import Plutarch.Evaluate (applyArguments, evalScriptHuge)
import Plutarch.Internal.Term (Config (NoTracing), InternalConfig (..), compileWithInternalConfig)
import Plutarch.Prelude
import Plutarch.Script (Script)
import PlutusCore.Data qualified as D
import Test.Tasty
import Test.Tasty.HUnit

data Vector = Vector String Int Bool String
instance FromJSON Vector where
    parseJSON = withObject "native item invocation" $ \o -> Vector <$> o .: "test" <*> o .: "invocation" <*> o .: "accepts" <*> o .: "argumentsCbor"
data Vectors = Vectors String [Vector]
instance FromJSON Vectors where
    parseJSON = withObject "native item invocations" $ \o -> Vectors <$> o .: "sourceCommit" <*> o .: "vectors"

tests :: IO TestTree
tests = do
    Vectors commit vectors <- either fail pure =<< eitherDecodeFileStrict' "tests/fixtures/phase-a-native-validators.json"
    commit @?= "9797ce41ce5d436e309eca07e2020ee29c395859"
    length vectors @?= 13
    length [() | Vector _ _ True _ <- vectors] @?= 5
    pure $ testGroup "Phase-A fixed-target native item validator invocations" [testCase (name <> "/" <> show index) $ run vector | vector@(Vector name index _ _) <- vectors]

run :: Vector -> Assertion
run (Vector _ _ accepts encoded) = do
    bytes <- either assertFailure pure $ Base16.decode $ BS8.pack encoded
    raw <- either (assertFailure . show) pure $ deserialiseOrFail @D.Data $ LBS.fromStrict bytes
    (label, arguments) <- case raw of
        D.List (D.B label : fields) ->
            if label == "item_dispatcher.main.spend"
                then case reverse fields of
                    transaction : ownRef : redeemer : datum : reversedParameters ->
                        pure
                            (label, reverse reversedParameters <> [D.Constr 0 [transaction, redeemer, D.Constr 1 [ownRef, datum]]])
                    _ -> assertFailure "Incomplete source spending invocation"
                else case reverse fields of
                    transaction : credential : redeemer : reversedParameters ->
                        pure
                            (label, reverse reversedParameters <> [D.Constr 0 [transaction, redeemer, D.Constr 2 [credential]]])
                    _ -> assertFailure "Incomplete source rewarding invocation"
        _ -> assertFailure "Malformed source invocation"
    script <- maybe (assertFailure "Unknown source native item validator") pure $ lookup label scripts
    let (result, _, traces) = evalScriptHuge $ applyArguments script arguments
    case result of
        Right _ -> assertBool "Expected on-chain rejection" accepts
        Left err -> assertBool (show err <> " " <> show traces) (not accepts)

scripts :: [(BS8.ByteString, Script)]
scripts = [("item_dispatcher.main.spend", compiled Native.phaseANativeItemSemanticV1Validator), ("item_yields.native.withdraw", compiled Yields.nativeValidator), ("item_yields.foreign.withdraw", compiled Yields.foreignValidator)]
compiled :: (forall s. Term s a) -> Script
compiled term = either (error . Text.unpack) id $ compileWithInternalConfig (InternalConfig False False) NoTracing term
