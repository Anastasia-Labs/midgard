{- | Validator-level regression vectors extracted from the exact second-wave
Aiken source. Each entry preserves the source parameters, datum, redeemer,
out-ref and complete transaction, including the original negative mutation.
The Haskell side adds the V3 ScriptContext envelope and the existing L2
proof-source field, decoded from the authenticated source leaf.
-}
module Testing.AikenSplitVectors (tests) where

import Codec.Serialise (deserialiseOrFail)
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Plutarch.Internal.Term (Config (NoTracing), InternalConfig (..), compileWithInternalConfig)
import Plutarch.Prelude
import Plutarch.Script (Script)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore.Data qualified as PD
import Test.Tasty
import Test.Tasty.HUnit

import MerkleTree.Validators.Membership (nonMembershipStakeValidator)
import Midgard.FraudProofs.TransitionTrace.Proof (pvalidateAcceptedTransactionFaultProof, pvalidateTransitionFaultProof)
import Midgard.Validators.FraudProofs.ValidationTrace.Cek (cekV1Validator)
import Midgard.Validators.FraudProofs.ValidationTrace.CekSemantics
import Testing.ExecutionFrontiers (measure)

-- The four retired core-validator vectors are replaced by the fixed-target
-- binder/compute/settle corpus in Testing.CekCoreValidators.
sourceCommit :: String
sourceCommit = "b93726aa444dd0f2d6a323b266cbf21ca7d63b5d"

data Vector = Vector String String Int Bool String
instance FromJSON Vector where
  parseJSON = withObject "Aiken validator vector" $ \o ->
    Vector <$> o .: "family" <*> o .: "test" <*> o .: "invocation" <*> o .: "accepts" <*> o .: "argumentsCbor"

data Vectors = Vectors String [Vector]
instance FromJSON Vectors where
  parseJSON = withObject "Aiken vectors" $ \o -> Vectors <$> o .: "sourceCommit" <*> o .: "vectors"

{- | Load before building the Tasty tree so each source regression keeps its
own name and can be selected independently. Missing/stale vectors fail closed.
-}
tests :: IO TestTree
tests = do
  Vectors commit vectors <- either fail pure =<< eitherDecodeFileStrict' "tests/fixtures/aiken-split-validators.json"
  if commit /= sourceCommit || null vectors then fail "Missing or wrong-source Aiken split vectors" else pure ()
  let currentVectors = [vector | vector@(Vector family _ _ _ _) <- vectors, family /= "value-and-mint"]
  mapM_
    ( \(family, count) ->
        if length [() | Vector f _ _ _ _ <- currentVectors, f == family] == count
          then pure ()
          else fail $ "Incomplete source vectors: " <> family
    )
    [("cek", 18), ("pexcludes", 7), ("transition-frontier", 13)]
  pure $
    testGroup
      "Aiken split validator source regressions"
      ( testCase "finish_witness_tail_layout_is_pinned" (finishTail currentVectors)
          : [ testCase (family <> "/" <> name <> "/" <> show invocation) $ runVector vector
            | vector@(Vector family name invocation _ _) <- currentVectors
            ]
      )

finishTail :: [Vector] -> Assertion
finishTail vectors = case [encoded | Vector "cek" "finish_validator_wins_the_hand_off" _ _ encoded <- vectors] of
  [encoded] -> do
    bytes <- either assertFailure pure $ Base16.decode $ BS8.pack encoded
    dat <- either (assertFailure . show) pure $ deserialiseOrFail @PD.Data $ LBS.fromStrict bytes
    case dat of
      PD.List fields -> case reverse fields of
        _ : _ : PD.Constr 1 [PD.Constr 0 [_, _, PD.Constr 0 [PD.B work, _]]] : _ ->
          BS.drop (BS.length work - 7) work @?= "\x00\x00\x00\x40\x40\x00\x00"
        _ -> assertFailure "Unexpected source finish action"
      _ -> assertFailure "Unexpected source finish capture"
  _ -> assertFailure "Missing unique source finish fixture"

runVector :: Vector -> Assertion
runVector (Vector family name _ accepts encoded) = do
  bytes <- either assertFailure pure $ Base16.decode $ BS8.pack encoded
  dat <- either (assertFailure . show) pure $ deserialiseOrFail @PD.Data $ LBS.fromStrict bytes
  (label, arguments) <- case dat of
    PD.List (PD.B "validate_transition_fault_proof" : fields) ->
      pure ("validate_transition_fault_proof", fields)
    PD.List (PD.B "validate_accepted_transaction_fault_proof" : fields) ->
      ("validate_accepted_transaction_fault_proof",) <$> adaptL2Proof fields
    PD.List [PD.B "pexcludes", redeemer, credential, transaction] ->
      pure ("pexcludes", [PD.Constr 0 [transaction, redeemer, PD.Constr 2 [credential]]])
    PD.List (PD.B label : fields) -> case reverse fields of
      transaction : ownOutRef : redeemer : datum : reversedParameters ->
        pure (label, reverse reversedParameters <> [PD.Constr 0 [transaction, redeemer, PD.Constr 1 [ownOutRef, datum]]])
      _ -> assertFailure "Incomplete source validator arguments"
    _ -> assertFailure "Malformed source capture"
  -- This source test negates a Bool-returning helper; an abort is not a
  -- successful False result. Keep that distinction in the translated test.
  let negatedHelper = name == "deposit_arm_rejects_arbitrary_projected_value"
      selected = if negatedHelper then "validate_transition_fault_proof_false" else label
  script <- maybe (assertFailure $ "Unknown source validator: " <> BS8.unpack selected) pure $ lookup selected compiledValidators
  measure (family <> "/" <> name) (accepts || negatedHelper) script arguments

-- The first-wave Plutarch L2 witness carries the decoded proof-source triple
-- as an extra final field. Recover it from the unchanged authenticated leaf;
-- the validator still checks its serialization against that leaf. Apply this
-- to negative cases too, so they reach the source's intended rejection.
adaptL2Proof :: [PD.Data] -> IO [PD.Data]
adaptL2Proof (PD.Constr 0 [headerHash, header, PD.Constr 4 [PD.Constr 4 witness]] : rest) =
  case witness of
    [_, _, PD.Constr 0 [_, _, _, _, _, PD.B leaf, _], _, _, _, _] -> do
      source <- either (assertFailure . show) pure $ deserialiseOrFail @PD.Data $ LBS.fromStrict leaf
      case source of
        PD.Constr 0 [PD.B _, triple@(PD.Constr 0 [_, _, _])] ->
          pure $ PD.Constr 0 [headerHash, header, PD.Constr 4 [PD.Constr 4 (witness <> [triple])]] : rest
        _ -> assertFailure "Malformed L2 source leaf in Aiken frontier fixture"
    _ -> assertFailure "Unexpected source L2 transition witness"
adaptL2Proof fields = pure fields

-- Shared compiled validators keep each test independent while avoiding a full
-- compilation of the same large resolver for every source mutation.
compiledValidators :: [(BS.ByteString, Script)]
compiledValidators =
  [ ("pexcludes", compiled nonMembershipStakeValidator)
  , ("validate_transition_fault_proof", compiled $ transitionFault True)
  , ("validate_transition_fault_proof_false", compiled $ transitionFault False)
  , ("validate_accepted_transaction_fault_proof", compiled acceptedTransactionFault)
  , ("cek_v1", compiled cekV1Validator)
  , ("cek_finish_semantic_v1", compiled cekFinishSemanticV1Validator)
  ]
 where
  compiled :: (forall s. Term s a) -> Script
  compiled term = either (error . Text.unpack) id $ compileWithInternalConfig (InternalConfig False False) NoTracing term

transitionFault :: Bool -> (forall s. Term s (PData :--> PData :--> PData :--> PData :--> PUnit))
transitionFault expected = plam $ \proof asset hub refs ->
  pif
    ( pvalidateTransitionFaultProof
        (pfromData $ punsafeCoerce proof)
        (punsafeCoerce asset)
        (pfromData $ punsafeCoerce hub)
        (pfromData $ punsafeCoerce refs)
        #== pconstant expected
    )
    (pconstant ())
    perror

acceptedTransactionFault :: forall s. Term s (PData :--> PData :--> PUnit)
acceptedTransactionFault = plam $ \proof asset ->
  pif (pvalidateAcceptedTransactionFaultProof (pfromData $ punsafeCoerce proof) (punsafeCoerce asset)) (pconstant ()) perror
