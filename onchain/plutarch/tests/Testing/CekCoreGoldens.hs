{- | Fixed-target TypeScript/Aiken core evidence, opened by the physical-hop
decoders and checked against exact state hashes and successor semantics.
-}
module Testing.CekCoreGoldens (tests) where

import Codec.Serialise (deserialiseOrFail, serialise)
import Data.Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Midgard.CekCoreWitness qualified as Core
import Midgard.CekMachine qualified as Machine
import Midgard.CekMaterialWitness qualified as Witness
import Plutarch.Evaluate (applyArguments, evalScriptHuge)
import Plutarch.Internal.Term (Config (NoTracing), InternalConfig (..), compileWithInternalConfig)
import Plutarch.Prelude
import Plutarch.Script (Script)
import PlutusCore.Data qualified as D
import Test.Tasty
import Test.Tasty.HUnit

data Vector = Vector String Bool String String String
instance FromJSON Vector where
    parseJSON = withObject "core golden" $ \o -> Vector <$> o .: "name" <*> o .: "accepts" <*> o .: "cbor" <*> o .: "pre" <*> o .: "post"
data Vectors = Vectors String [Vector]
instance FromJSON Vectors where
    parseJSON = withObject "core goldens" $ \o -> Vectors <$> o .: "sourceCommit" <*> o .: "vectors"

tests :: IO TestTree
tests = do
    Vectors commit vectors <- either fail pure =<< eitherDecodeFileStrict' "tests/fixtures/cek-core-goldens.json"
    assertEqual "fixed upstream boundary" "9797ce41ce5d436e309eca07e2020ee29c395859" commit
    length vectors @?= 453
    length (filter accepted vectors) @?= 403
    assertBool "source successes and rejections collected" (any accepted vectors && any (not . accepted) vectors)
    pure $ testGroup "CEK fixed-target core goldens" [testCase name $ run vector | vector@(Vector name _ _ _ _) <- vectors]
  where
    accepted (Vector _ ok _ _ _) = ok

run :: Vector -> Assertion
run (Vector _ accepts encoded pre post) = do
    bytes <- unhex encoded
    raw <- either (assertFailure . show) pure $ deserialiseOrFail @D.Data $ LBS.fromStrict bytes
    LBS.toStrict (serialise raw) @?= bytes
    preHash <- unhex pre
    postHash <- unhex post
    let (result, _, traces) = evalScriptHuge $ applyArguments kernel [raw, D.I (if accepts then 1 else 0), D.B preHash, D.B postHash]
    case result of Left err -> assertFailure $ show err <> " " <> show traces; Right _ -> pure ()
  where
    unhex = either assertFailure pure . Base16.decode . BS8.pack

-- Compile once for all source vectors. A rejection must be Boolean False;
-- decoding failure or abort is not a passing negative golden.
kernel :: Script
kernel = either (error . Text.unpack) id $
    compileWithInternalConfig (InternalConfig False False) NoTracing $
        plam $ \raw expected preHash postHash -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag f) ->
            pif
                (tag #== 0 #&& plength # f #== 3)
                ( plet (Core.pdecodeMachine # (pelemAt # 0 # f)) $ \pre ->
                    plet (Core.pdecodeMachine # (pelemAt # 1 # f)) $ \post ->
                        plet (Witness.pdecodeCoreStepWitness # (pelemAt # 2 # f)) $ \witness ->
                            plet (Machine.pverifyCoreStepV1 # pre # post # witness) $ \valid ->
                                pif
                                    ( pif
                                        (pasInt # expected #== 1)
                                        (valid #&& Machine.phashStateV1 # pre #== pasByteStr # preHash #&& Machine.phashStateV1 # post #== pasByteStr # postHash)
                                        (pnot # valid)
                                    )
                                    (pconstant @PUnit ())
                                    perror
                )
                perror
