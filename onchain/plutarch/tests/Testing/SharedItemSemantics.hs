-- | Exact returns and traps captured from the fixed-target Aiken tests.
module Testing.SharedItemSemantics (tests) where

import Codec.Serialise (deserialiseOrFail)
import Data.Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Midgard.ScriptSourcesItemNormalization qualified as Normalized
import Midgard.ScriptSourcesItemData qualified as Wire
import Midgard.ScriptSourcesItemSemantics qualified as Semantics
import Plutarch.Evaluate (applyArguments, evalScriptHuge)
import Plutarch.Internal.Term (Config (NoTracing), InternalConfig (..), compileWithInternalConfig)
import Plutarch.Prelude
import Plutarch.Script (Script)
import PlutusCore.Data qualified as D
import Test.Tasty
import Test.Tasty.HUnit

data Vector = Vector String Bool Bool String
instance FromJSON Vector where
  parseJSON = withObject "item semantic vector" $ \o -> Vector <$> o .: "test" <*> o .: "accepts" <*> o .: "traps" <*> o .: "argumentsCbor"
data Vectors = Vectors String [Vector]
instance FromJSON Vectors where
  parseJSON = withObject "item semantics" $ \o -> Vectors <$> o .: "sourceCommit" <*> o .: "vectors"

tests :: IO TestTree
tests = do
  Vectors commit vectors <- either fail pure =<< eitherDecodeFileStrict' "tests/fixtures/shared-item-semantics.json"
  commit @?= "9797ce41ce5d436e309eca07e2020ee29c395859"
  length vectors @?= 11
  length [() | Vector _ True _ _ <- vectors] @?= 5
  length [() | Vector _ _ True _ <- vectors] @?= 2
  pure $ testGroup "Shared item fixed-target semantics" [testCase name $ check v | v@(Vector name _ _ _) <- vectors]

check :: Vector -> Assertion
check (Vector _ accepts traps encoded) = do
  bytes <- either assertFailure pure $ Base16.decode $ BS8.pack encoded
  raw <- either (assertFailure . show) pure $ deserialiseOrFail @D.Data $ LBS.fromStrict bytes
  (label, state) <- case raw of
    D.List [D.B name, state] -> pure (name, state)
    _ -> assertFailure $ "unexpected captured shape: " <> show raw
  kernel <- maybe (assertFailure $ "unmapped semantic: " <> show label) pure $ lookup label kernels
  let (result, _, traces) = evalScriptHuge $ applyArguments kernel [state, D.I (if accepts then 1 else 0)]
  case (traps, result) of
    (True, Left _) -> pure ()
    (False, Right _) -> pure ()
    _ -> assertFailure $ show result <> " " <> show traces

kernels :: [(BS8.ByteString, Script)]
kernels =
  [ ("open_header", compile Semantics.popenHeader)
  , ("open_tail", compile Semantics.popenTail)
  , ("invalid_header", compile Semantics.pinvalidHeader)
  , ("invalid_tail", compile Semantics.pinvalidTail)
  ]
  where
    compile :: (forall s. Term s (Normalized.POuterExecution :--> PBool)) -> Script
    compile predicate = either (error . Text.unpack) id $
      compileWithInternalConfig (InternalConfig False False) NoTracing $
        plam $ \raw expected ->
          pif ((predicate # (Wire.pdecodeOuterExecution # raw)) #== (pasInt # expected #== 1)) (pconstant @PUnit ()) perror
