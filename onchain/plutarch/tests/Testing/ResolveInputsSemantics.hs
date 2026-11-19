-- | New binding boundaries evaluated by the exact target Aiken predicates.
module Testing.ResolveInputsSemantics (tests) where

import Codec.Serialise (deserialiseOrFail)
import Data.Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Midgard.ResolveInputsSemantics qualified as Resolve
import Midgard.ValidationMachine (PValidationAuxiliaryWitnessV1 (..))
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
  parseJSON = withObject "resolve-input vector" $ \o -> Vector <$> o .: "test" <*> o .: "accepts" <*> o .: "argumentsCbor"
data Vectors = Vectors String [Vector]
instance FromJSON Vectors where
  parseJSON = withObject "resolve-input vectors" $ \o -> Vectors <$> o .: "sourceCommit" <*> o .: "vectors"

tests :: IO TestTree
tests = do
  Vectors commit vectors <- either fail pure =<< eitherDecodeFileStrict' "tests/fixtures/resolve-inputs-semantics.json"
  commit @?= "9797ce41ce5d436e309eca07e2020ee29c395859"
  length vectors @?= 10
  length [() | Vector _ True _ <- vectors] @?= 5
  pure $ testGroup "ResolveInputs fixed-target semantics" [testCase name $ run vector | vector@(Vector name _ _) <- vectors]

run :: Vector -> Assertion
run (Vector _ expected encoded) = do
  bytes <- either assertFailure pure $ Base16.decode $ BS8.pack encoded
  raw <- either (assertFailure . show) pure $ deserialiseOrFail @D.Data $ LBS.fromStrict bytes
  (label, args) <- case raw of
    D.List [D.B label, pre, witness, auxiliary] -> pure (label, [pre, witness, auxiliary, D.I $ if expected then 1 else 0])
    _ -> assertFailure "Malformed resolve-input invocation"
  kernel <- maybe (assertFailure "Unknown resolve-input") pure $ lookup label kernels
  let (result, _, traces) = evalScriptHuge $ applyArguments kernel args
  case result of
    Right _ -> pure ()
    Left err -> assertFailure $ show err <> " " <> show traces

kernels :: [(BS8.ByteString, Script)]
kernels = [("initial", compiled 0), ("finish", compiled 1), ("membership", compiled 2), ("nonmembership", compiled 3)]

compiled :: Int -> Script
compiled kind = either (error . Text.unpack) id $ compileWithInternalConfig (InternalConfig False False) NoTracing $
  plam $ \(preRaw :: Term s PData) (witnessRaw :: Term s PData) (auxiliaryRaw :: Term s PData) expected ->
    -- Typed arguments captured from the exact target's resolver tests.
    plet (pfromData $ punsafeCoerce preRaw) $ \pre ->
    plet (pfromData $ punsafeCoerce witnessRaw) $ \witness ->
      let result = case kind of
            0 -> Resolve.pinitial # pre # witness
            1 -> Resolve.pfinish # pre # witness
            2 -> pmatch (pfromData $ punsafeCoerce auxiliaryRaw) $ \case
              PScheduledLedgerMembershipWitness source key schedule value proof signers ->
                Resolve.pmembershipBegin # pre # witness # pfromData source # pfromData key # pfromData schedule # pfromData value # pfromData proof # pfromData signers
              _ -> perror
            3 -> pmatch (pfromData $ punsafeCoerce auxiliaryRaw) $ \case
              PScheduledLedgerNonMembershipWitness source key schedule proof ->
                Resolve.pnonMembership # pre # witness # pfromData source # pfromData key # pfromData schedule # pfromData proof
              _ -> perror
            _ -> error "Unmapped resolver"
       in pif (result #== (pasInt # expected #== 1)) (pconstant @PUnit ()) perror
