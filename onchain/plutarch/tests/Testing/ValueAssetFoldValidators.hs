-- | Exact fixed-target Aiken ValueAndMint and asset-fold invocations.
module Testing.ValueAssetFoldValidators (tests) where

import Codec.Serialise (deserialiseOrFail)
import Data.Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Midgard.Validators.FraudProofs.ValidationTrace.ValueAndMint (valueAndMintV1Validator)
import Midgard.Validators.FraudProofs.ValidationTrace.ValueAndMintSemantics
import Midgard.ValueAssetFoldYield qualified as AssetFoldYield
import Plutarch.Evaluate (applyArguments, evalScriptHuge)
import Plutarch.Internal.Term (Config (NoTracing), InternalConfig (..), compileWithInternalConfig)
import Plutarch.Prelude
import Plutarch.Script (Script)
import PlutusCore.Data qualified as D
import Test.Tasty
import Test.Tasty.HUnit

data Vector = Vector String Int Bool String
instance FromJSON Vector where
  parseJSON = withObject "ValueAndMint invocation" $ \o -> Vector <$> o .: "test" <*> o .: "invocation" <*> o .: "accepts" <*> o .: "argumentsCbor"

data Vectors = Vectors String [Vector]
instance FromJSON Vectors where
  parseJSON = withObject "ValueAndMint invocations" $ \o -> Vectors <$> o .: "sourceCommit" <*> o .: "vectors"

tests :: IO TestTree
tests = do
  Vectors commit vectors <- either fail pure =<< eitherDecodeFileStrict' "tests/fixtures/value-asset-fold-validators.json"
  commit @?= "9797ce41ce5d436e309eca07e2020ee29c395859"
  length vectors @?= 68
  length [() | Vector _ _ True _ <- vectors] @?= 29
  pure $ testGroup "ValueAndMint fixed-target asset-fold validator invocations" [testCase (name <> "/" <> show index) $ run vector | vector@(Vector name index _ _) <- vectors]

run :: Vector -> Assertion
run (Vector _ _ accepts encoded) = do
  bytes <- either assertFailure pure $ Base16.decode $ BS8.pack encoded
  raw <- either (assertFailure . show) pure $ deserialiseOrFail @D.Data $ LBS.fromStrict bytes
  (label, arguments) <- case raw of
    D.List (D.B label : fields) ->
      if label == "asset_fold_yield.main.withdraw"
        then case reverse fields of
          transaction : credential : redeemer : reversedParameters ->
            pure (label, reverse reversedParameters <> [D.Constr 0 [transaction, redeemer, D.Constr 2 [credential]]])
          _ -> assertFailure "Incomplete source rewarding invocation"
        else case reverse fields of
          transaction : ownRef : redeemer : datum : reversedParameters ->
            pure (label, reverse reversedParameters <> [D.Constr 0 [transaction, redeemer, D.Constr 1 [ownRef, datum]]])
          _ -> assertFailure "Incomplete source spending invocation"
    _ -> assertFailure "Malformed source invocation"
  script <- maybe (assertFailure $ "Unknown source ValueAndMint validator: " <> BS8.unpack label) pure $ lookup label scripts
  let (result, _, traces) = evalScriptHuge $ applyArguments script arguments
  case result of
    Right _ -> assertBool "Expected on-chain rejection" accepts
    Left err -> assertBool (show err <> " " <> show traces) (not accepts)

scripts :: [(BS8.ByteString, Script)]
scripts =
  [ ("prepare_validator.main.spend", compiled valueAndMintV1Validator)
  , ("begin_validator.main.spend", compiled valueAndMintBeginSemanticV1Validator)
  , ("replay_begin_validator.main.spend", compiled valueAndMintReplayBeginSemanticV1Validator)
  , ("replay_input_validator.main.spend", compiled valueAndMintReplayInputSemanticV1Validator)
  , ("replay_asset_validator.main.spend", compiled valueAndMintReplayAssetSemanticV1Validator)
  , ("replay_finish_validator.main.spend", compiled valueAndMintReplayFinishSemanticV1Validator)
  , ("output_descriptor_validator.main.spend", compiled valueAndMintOutputDescriptorSemanticV1Validator)
  , ("output_asset_validator.main.spend", compiled valueAndMintOutputAssetSemanticV1Validator)
  , ("output_finish_validator.main.spend", compiled valueAndMintOutputFinishSemanticV1Validator)
  , ("mint_asset_validator.main.spend", compiled valueAndMintMintAssetSemanticV1Validator)
  , ("mint_finish_validator.main.spend", compiled valueAndMintMintFinishSemanticV1Validator)
  , ("finalize_validator.main.spend", compiled valueAndMintFinalizeSemanticV1Validator)
  , ("asset_fold_yield.main.withdraw", compiled AssetFoldYield.validator)
  ]

compiled :: (forall s. Term s a) -> Script
compiled term = either (error . Text.unpack) id $ compileWithInternalConfig (InternalConfig False False) NoTracing term
