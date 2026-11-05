-- | Exact fixed-target Aiken selection spending and rewarding invocations.
module Testing.CekSelectionValidators (tests) where

import Codec.Serialise (deserialiseOrFail)
import Data.Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Midgard.Validators.FraudProofs.ValidationTrace.CekSelection qualified as Selection
import Plutarch.Evaluate (applyArguments, evalScriptHuge)
import Plutarch.Internal.Term (Config (NoTracing), InternalConfig (..), compileWithInternalConfig)
import Plutarch.Prelude
import Plutarch.Script (Script)
import PlutusCore.Data qualified as D
import Test.Tasty
import Test.Tasty.HUnit

data Vector = Vector String Int Bool String
instance FromJSON Vector where
    parseJSON = withObject "core invocation" $ \o -> Vector <$> o .: "test" <*> o .: "invocation" <*> o .: "accepts" <*> o .: "argumentsCbor"
data Vectors = Vectors String [Vector]
instance FromJSON Vectors where
    parseJSON = withObject "core invocations" $ \o -> Vectors <$> o .: "sourceCommit" <*> o .: "vectors"

tests :: IO TestTree
tests = do
    Vectors commit vectors <- either fail pure =<< eitherDecodeFileStrict' "tests/fixtures/cek-selection-validators.json"
    commit @?= "9797ce41ce5d436e309eca07e2020ee29c395859"
    length vectors @?= 17
    length [() | Vector _ _ True _ <- vectors] @?= 12
    pure $ testGroup "CEK fixed-target selection validator invocations" [testCase (name <> "/" <> show index) $ run vector | vector@(Vector name index _ _) <- vectors]

run :: Vector -> Assertion
run (Vector _ _ accepts encoded) = do
    bytes <- either assertFailure pure $ Base16.decode $ BS8.pack encoded
    raw <- either (assertFailure . show) pure $ deserialiseOrFail @D.Data $ LBS.fromStrict bytes
    (label, arguments) <- case raw of
        D.List (D.B label : fields) ->
            if label == "selection"
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
    script <- maybe (assertFailure "Unknown source core validator") pure $ lookup label scripts
    let (result, _, traces) = evalScriptHuge $ applyArguments script arguments
    case result of
        Right _ -> assertBool "Expected on-chain rejection" accepts
        Left err -> assertBool (show err <> " " <> show traces) (not accepts)

scripts :: [(BS8.ByteString, Script)]
scripts = [("selection", compiled Selection.selectionValidator), ("authenticate", compiled Selection.authenticateValidator), ("successor", compiled Selection.successorValidator), ("material_program", compiled Selection.programValidator), ("material_data", compiled Selection.dataValidator)]
compiled :: (forall s. Term s a) -> Script
compiled term = either (error . Text.unpack) id $ compileWithInternalConfig (InternalConfig False False) NoTracing term
