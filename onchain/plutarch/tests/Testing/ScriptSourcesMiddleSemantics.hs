-- | New binding boundaries evaluated by the exact target Aiken predicates.
module Testing.ScriptSourcesMiddleSemantics (tests) where

import Codec.Serialise (deserialiseOrFail)
import Data.Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Midgard.ScriptSourcesMiddleSemantics qualified as Middle
import Midgard.ValidationMachineFieldDoor (PMachineFieldDoorV1 (..))
import Plutarch.Evaluate (applyArguments, evalScriptHuge)
import Plutarch.Internal.Term (Config (NoTracing), InternalConfig (..), compileWithInternalConfig)
import Plutarch.LedgerApi.V3 (PCurrencySymbol (..))
import Plutarch.Prelude
import Plutarch.Script (Script)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore.Data qualified as D
import Test.Tasty
import Test.Tasty.HUnit

data Vector = Vector String Bool String
instance FromJSON Vector where
  parseJSON = withObject "middle-stage vector" $ \o -> Vector <$> o .: "test" <*> o .: "accepts" <*> o .: "argumentsCbor"
data Vectors = Vectors String [Vector]
instance FromJSON Vectors where
  parseJSON = withObject "middle-stage vectors" $ \o -> Vectors <$> o .: "sourceCommit" <*> o .: "vectors"

tests :: IO TestTree
tests = do
  Vectors commit vectors <- either fail pure =<< eitherDecodeFileStrict' "tests/fixtures/script-sources-middle-semantics.json"
  commit @?= "9797ce41ce5d436e309eca07e2020ee29c395859"
  length vectors @?= 22
  length [() | Vector _ True _ <- vectors] @?= 11
  pure $ testGroup "ScriptSources middle fixed-target semantics" [testCase name $ run vector | vector@(Vector name _ _) <- vectors]

run :: Vector -> Assertion
run (Vector _ expected encoded) = do
  bytes <- either assertFailure pure $ Base16.decode $ BS8.pack encoded
  raw <- either (assertFailure . show) pure $ deserialiseOrFail @D.Data $ LBS.fromStrict bytes
  (label, args) <- case raw of
    D.List (D.I label : pre : witness : extra) -> pure (label, [pre, witness, D.List extra, D.I $ if expected then 1 else 0])
    _ -> assertFailure "Malformed middle-stage invocation"
  kernel <- maybe (assertFailure "Unknown middle-stage") pure $ lookup label kernels
  let (result, _, traces) = evalScriptHuge $ applyArguments kernel args
  case result of
    Right _ -> pure ()
    Left err -> assertFailure $ show err <> " " <> show traces

kernels :: [(Integer, Script)]
kernels = [(i, compiled $ fromInteger i) | i <- [0 .. 7]]

compiled :: Int -> Script
compiled kind = either (error . Text.unpack) id $ compileWithInternalConfig (InternalConfig False False) NoTracing $
  plam $ \(preRaw :: Term s PData) (witnessRaw :: Term s PData) (extraRaw :: Term s PData) expected ->
    plet (pfromData $ punsafeCoerce preRaw) $ \pre ->
    plet (pfromData $ punsafeCoerce witnessRaw) $ \witness ->
    plet (pasList # extraRaw) $ \extra ->
      let at i = pelemAt # i # extra
          door = pcon $ PMachineFieldDoorV1 pnil (pdata $ pcon $ PCurrencySymbol $ pconstant "")
          result = case kind of
            0 -> Middle.pstageTwoAdvance # pre # witness
            1 -> Middle.pstageThreeReplay # pre # witness # (pasInt # at 0) # (pasByteStr # at 1) # (pasByteStr # at 2) # (pasByteStr # at 3)
            2 -> Middle.pstageThreeFinish # pre # witness
            3 -> Middle.pstageFourBegin # pre # witness # door # (pfromData $ punsafeCoerce $ at 0)
            4 -> Middle.pstageFourFinish # pre # witness
            5 -> Middle.pstageSixBegin # pre # witness # door # (pasInt # at 0) # (pasInt # at 1) # (pfromData $ punsafeCoerce $ at 2)
            6 -> Middle.pstageSixAsset # pre # witness # (pfromData $ punsafeCoerce $ at 0) # (pfromData $ punsafeCoerce $ at 1)
            7 -> Middle.pstageSixFinish # pre # witness
            _ -> error "Unmapped middle stage"
       in pif (result #== (pasInt # expected #== 1)) (pconstant @PUnit ()) perror
