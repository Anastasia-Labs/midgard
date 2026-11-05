-- | Exact fixed-target Aiken context-stage validator invocations.
module Testing.CekContextValidators (tests) where

import Codec.Serialise (deserialiseOrFail)
import Data.Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Midgard.Validators.FraudProofs.ValidationTrace.CekContext qualified as CekContext
import Midgard.Validators.FraudProofs.ValidationTrace.CekContextFinalization qualified as CekContextFinalization
import Midgard.Validators.FraudProofs.ValidationTrace.CekContextItem qualified as CekContextItem
import Midgard.Validators.FraudProofs.ValidationTrace.CekContextObserver qualified as CekContextObserver
import Midgard.Validators.FraudProofs.ValidationTrace.CekContextRedeemer qualified as CekContextRedeemer
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
    Vectors commit vectors <- either fail pure =<< eitherDecodeFileStrict' "tests/fixtures/cek-context-validators.json"
    commit @?= "9797ce41ce5d436e309eca07e2020ee29c395859"
    length vectors @?= 53
    length [() | Vector _ _ True _ <- vectors] @?= 38
    pure $ testGroup "CEK fixed-target context validator invocations" [testCase (name <> "/" <> show index) $ run vector | vector@(Vector name index _ _) <- vectors]

run :: Vector -> Assertion
run (Vector _ _ accepts encoded) = do
    bytes <- either assertFailure pure $ Base16.decode $ BS8.pack encoded
    raw <- either (assertFailure . show) pure $ deserialiseOrFail @D.Data $ LBS.fromStrict bytes
    (label, arguments) <- case raw of
        D.List (D.B label : fields) -> case reverse fields of
            transaction : ownRef : redeemer : datum : reversedParameters ->
                pure
                    (label, reverse reversedParameters <> [D.Constr 0 [transaction, redeemer, D.Constr 1 [ownRef, datum]]])
            _ -> assertFailure "Incomplete source invocation"
        _ -> assertFailure "Malformed source invocation"
    script <- maybe (assertFailure "Unknown source core validator") pure $ lookup label scripts
    let (result, _, traces) = evalScriptHuge $ applyArguments script arguments
    case result of
        Right _ -> assertBool "Expected on-chain rejection" accepts
        Left err -> assertBool (show err <> " " <> show traces) (not accepts)

scripts :: [(BS8.ByteString, Script)]
scripts =
    [ ("context_control", compiled CekContext.controlValidator)
    , ("context_item_bind", compiled CekContextItem.bindValidator)
    , ("context_item_hash", compiled CekContextItem.hashValidator)
    , ("context_item_return", compiled CekContextItem.returnValidator)
    , ("context_item_selection_continue", compiled CekContextItem.selectionContinueValidator)
    , ("context_item_selection_finish", compiled CekContextItem.selectionFinishValidator)
    , ("context_mint_init", compiled CekContext.mintInitValidator)
    , ("context_mint_item", compiled CekContext.mintItemValidator)
    , ("context_observer_authenticate", compiled CekContextObserver.authenticateValidator)
    , ("context_observer_fold", compiled CekContextObserver.foldValidator)
    , ("context_output", compiled CekContext.outputValidator)
    , ("context_reference", compiled CekContext.referenceValidator)
    , ("context_settle", compiled CekContext.settleValidator)
    , ("context_signer", compiled CekContext.signerValidator)
    , ("context_spend", compiled CekContext.spendValidator)
    , ("context_bind", compiled CekContext.bindValidator)
    ]
compiled :: (forall s. Term s a) -> Script
compiled term = either (error . Text.unpack) id $ compileWithInternalConfig (InternalConfig False False) NoTracing term
