{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsNativeScriptInvalid (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (pubKeyHashAddress, scriptHashAddress)
import PlutusLedgerApi.V1.Value (TokenName (..), singleton)
import PlutusLedgerApi.V3 (
  Datum (..),
  OutputDatum (..),
  PubKeyHash (..),
  ScriptContext (..),
  ScriptHash (..),
  TxId (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusTx.Builtins (dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude
import Testing.Eval (pfailsNoTraceWithoutHoistChecks, psucceedsNoTraceWithoutHoistChecks)
import Testing.FraudProofsFixture

import Midgard.Validators.FraudProofs.NativeScriptInvalid

tests :: TestTree
tests =
  testGroup
    "Native-script-invalid"
    [ testGroup
        "direct native verdict"
        [ testCase "finalizes an unsatisfied native witness" $ succeeds $ runStep03 unsatisfiedScriptItem
        , testCase "rejects a satisfied native witness" $ fails $ runStep03 satisfiedScriptItem
        , testCase "rejects malformed native bytes" $ fails $ runStep03 malformedScriptItem
        ]
    , testGroup
        "authenticated signer verdict"
        [ testCase "finalizes authenticated absence" $ succeeds $ runStep05 absentScriptItem [missingQuery]
        , testCase "rejects a satisfied script" $ fails $ runStep05 presentScriptItem [presentQuery]
        , testCase "rejects a missing signer query" $ fails $ runStep05 absentScriptItem []
        , testCase "rejects a forged absence boundary" $ fails $ runStep05 absentScriptItem [forgedMissingQuery]
        ]
    , testGroup
        "cancellation"
        [ testCase "step 03 preserves prover cancellation" $ succeeds $ step03 $ cancellationContext step03State True
        , testCase "step 04 preserves prover cancellation" $ succeeds $ step04 $ cancellationContext step04State True
        , testCase "step 05 preserves prover cancellation" $ succeeds $ step05 $ cancellationContext (step05State absentScriptItem readyPhase) True
        ]
    , testGroup
        "script cursor"
        [ testCase "starts a resumable pushdown" $ succeeds startCompoundScan
        , testCase "resumes the committed cursor" $ succeeds resumeCompoundScan
        , testCase "rejects a mutated cursor" $ fails mutatedCompoundCursor
        ]
    , testGroup
        "318-witness staged vectors"
        [ q34Case "step 03 exact 318-witness fit" $ \chunks -> succeeds $ q34Step03 chunks nextScript
        , q34Case "step 03 rejects the wrong successor" $ \chunks -> fails $ q34Step03 chunks stepScript
        , q34Case "step 04 exact 318-witness resume fit" $ \chunks -> succeeds $ q34Step04 chunks q34State32 q34Checkpoint32 32 q34State64 False
        , q34Case "step 04 rejects a mutated checkpoint" $ \chunks -> fails $ q34Step04 chunks q34State32 q34Checkpoint64 32 q34State64 False
        , q34Case "step 04 exact 318-witness penultimate fit" $ \chunks -> succeeds $ q34Step04 chunks q34State288 q34Checkpoint288 16 q34State304 False
        , q34Case "step 04 exact 318-witness terminal fit" $ \chunks -> succeeds $ q34Step04 chunks q34State304 q34Checkpoint304 14 q34State318 True
        , testCase "step 05 exact 318-signer frontier fit" $ succeeds q34Step05
        ]
    , testGroup
        "maximum native script"
        [ testCase "max 32-node native script starts" $ succeeds $ startMaxScript maxUnsatisfiedPayload
        , testCase "max 32-node native script finalizes" $ succeeds $ finalizeMaxScript maxUnsatisfiedPayload
        , testCase "rejects max 32-node satisfied native script" $ fails $ finalizeMaxScript maxSatisfiedPayload
        ]
    ]

succeeds, fails :: (forall s. Term s PUnit) -> Assertion
succeeds = psucceedsNoTraceWithoutHoistChecks
fails = pfailsNoTraceWithoutHoistChecks

step03, step04, step05 :: forall s. ScriptContext -> Term s PUnit
step03 ctx =
  nativeScriptInvalidStep03Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant certificatePolicy)
    # pconstant ctx
step04 ctx =
  nativeScriptInvalidStep04Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant ctx
step05 ctx =
  nativeScriptInvalidStep05Validator
    # pdata (pconstant ctPolicy)
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pconstant ctx

runStep03 :: forall s. BS.ByteString -> Term s PUnit
runStep03 scriptItem = step03 $ finalizeContext (stepDatum $ Just $ step03StateFor scriptItem) directRedeemer
  where
    directRedeemer =
      PD.Constr
        1
        [ PD.Constr
            0
            [ PD.I 0
            , PD.I 0
            , PD.I 0
            , PD.B scriptItem
            , witnessOpening (compactOf tx1) tx1 (addressWitnessesPreimage tx1)
            ]
        ]

runStep05 :: forall s. BS.ByteString -> [PD.Data] -> Term s PUnit
runStep05 scriptItem queries = step05 $ finalizeContext (stepDatum $ Just $ step05State scriptItem readyPhase) redeemer
  where
    redeemer =
      PD.Constr
        1
        [ PD.Constr
            2
            [ PD.I 0
            , PD.I 0
            , PD.I 0
            , PD.B scriptItem
            , PD.I 1
            , PD.List queries
            ]
        ]

finalizeContext :: PD.Data -> PD.Data -> ScriptContext
finalizeContext datum redeemer =
  spendContext
    datum
    redeemer
    [threadInput]
    [convictionOutput fraudProofAddress threadName]
    []
    [fraudProofMintEntry threadName]
    (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)

cancellationContext :: PD.Data -> Bool -> ScriptContext
cancellationContext state signedByProver =
  let context =
        spendContext
          (stepDatum $ Just state)
          cancelRedeemer
          [threadInput]
          []
          []
          [cancelMintEntry threadName]
          mempty
   in if signedByProver then context else withoutSignatories context

withoutSignatories :: ScriptContext -> ScriptContext
withoutSignatories (ScriptContext txInfo redeemer scriptInfo) =
  ScriptContext txInfo {txInfoSignatories = []} redeemer scriptInfo

step03StateFor :: BS.ByteString -> PD.Data
step03StateFor scriptItem =
  PD.Constr
    0
    [ PD.B tx1Id
    , PD.B $ witnessSetHashOf tx1
    , PD.B $ blake2b256 scriptItem
    , PD.I $ tValidityStart tx1
    , PD.I $ tValidityEnd tx1
    ]

step03State :: PD.Data
step03State = step03StateFor unsatisfiedScriptItem

step04State :: PD.Data
step04State =
  PD.Constr
    0
    [ PD.B tx1Id
    , PD.B $ witnessSetHashOf tx1
    , PD.B $ blake2b256 unsatisfiedScriptItem
    , PD.I $ tValidityStart tx1
    , PD.I $ tValidityEnd tx1
    , PD.B $ BS.replicate 32 0x33
    , PD.B ""
    , PD.I 0
    , PD.List []
    ]

step05State :: BS.ByteString -> PD.Data -> PD.Data
step05State scriptItem phase =
  PD.Constr
    0
    [ PD.B tx1Id
    , PD.B $ blake2b256 scriptItem
    , PD.I (-1)
    , PD.I (-1)
    , PD.I 1
    , PD.List oneSignerPeaks
    , phase
    ]

readyPhase :: PD.Data
readyPhase = PD.Constr 0 []

presentSigner, absentSigner :: BS.ByteString
presentSigner = BS.replicate 28 0x11
absentSigner = BS.replicate 28 0xff

signaturePayload :: BS.ByteString -> BS.ByteString
signaturePayload signer = "\x82\x00\x58\x1c" <> signer

unsatisfiedScriptItem, satisfiedScriptItem, malformedScriptItem, absentScriptItem, presentScriptItem :: BS.ByteString
unsatisfiedScriptItem = versionedScriptItem 0 nativeScriptBytes
satisfiedScriptItem = versionedScriptItem 0 $ signaturePayload $ keyHashFor 0
malformedScriptItem = versionedScriptItem 0 "\xff"
absentScriptItem = versionedScriptItem 0 $ signaturePayload absentSigner
presentScriptItem = versionedScriptItem 0 $ signaturePayload presentSigner

oneSignerPeaks :: [PD.Data]
oneSignerPeaks = [PD.Constr 0 [PD.I 0, PD.B presentSignerLeaf]]

presentSignerLeaf :: BS.ByteString
presentSignerLeaf = blake2b256 $ "MidgardSignerLeafV1" <> "\x58\x1c" <> presentSigner

missingQuery, presentQuery, forgedMissingQuery :: PD.Data
missingQuery = signerQuery absentSigner $ PD.Constr 4 [PD.List oneSignerPeaks, PD.B presentSigner, PD.List []]
presentQuery = signerQuery presentSigner $ PD.Constr 1 [PD.List oneSignerPeaks, PD.I 0, PD.List []]
forgedMissingQuery =
  signerQuery absentSigner $
    PD.Constr 4 [PD.List oneSignerPeaks, PD.B $ BS.replicate 28 0x22, PD.List []]

signerQuery :: BS.ByteString -> PD.Data -> PD.Data
signerQuery signer proof = PD.Constr 0 [PD.B signer, proof]

startCompoundScan, resumeCompoundScan, mutatedCompoundCursor :: forall s. Term s PUnit
startCompoundScan =
  step05 $
    continueContext
      (step05State compoundScriptItem readyPhase)
      (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.B compoundScriptItem, PD.I 1, PD.List []]])
      (step05State compoundScriptItem $ walkPhase compoundFirstHash)
      stepScript
      []
resumeCompoundScan =
  step05 $
    continueContext
      (step05State compoundScriptItem $ walkPhase compoundFirstHash)
      ( PD.Constr
          1
          [ PD.Constr
              1
              [ PD.I 0
              , PD.I 0
              , PD.B compoundScriptItem
              , PD.B compoundFirstCursor
              , PD.List compoundFrames
              , PD.I 1
              , PD.List [compoundMissingQuery]
              ]
          ]
      )
      (step05State compoundScriptItem $ walkPhase compoundSecondHash)
      stepScript
      []
mutatedCompoundCursor =
  step05 $
    continueContext
      (step05State compoundScriptItem $ walkPhase $ BS.replicate 32 0)
      ( PD.Constr
          1
          [ PD.Constr
              1
              [ PD.I 0
              , PD.I 0
              , PD.B compoundScriptItem
              , PD.B compoundFirstCursor
              , PD.List compoundFrames
              , PD.I 1
              , PD.List [compoundMissingQuery]
              ]
          ]
      )
      (step05State compoundScriptItem $ walkPhase compoundSecondHash)
      stepScript
      []

continueContext :: PD.Data -> PD.Data -> PD.Data -> BS.ByteString -> [TxInInfo] -> ScriptContext
continueContext inputState redeemer outputState outputScript refs =
  spendContext
    (stepDatum $ Just inputState)
    redeemer
    [threadInput]
    [stepOutput outputScript $ Just outputState]
    refs
    []
    mempty

walkPhase :: BS.ByteString -> PD.Data
walkPhase cursorHash = PD.Constr 1 [PD.B cursorHash]

compoundSigner, compoundPayload, compoundScriptItem :: BS.ByteString
compoundSigner = BS.replicate 28 0x99
compoundPayload = "\x82\x01\x82" <> signaturePayload compoundSigner <> signaturePayload compoundSigner
compoundScriptItem = versionedScriptItem 0 compoundPayload

compoundFrames :: [PD.Data]
compoundFrames = [frameData 1 2 0 2]

compoundFirstCursor, compoundSecondCursor, compoundFirstHash, compoundSecondHash :: BS.ByteString
compoundFirstCursor = cursorBytes compoundPayload (1, 2, 0, 2) 3 1 0
compoundSecondCursor = cursorBytes compoundPayload (1, 2, 0, 2) 35 2 1
compoundFirstHash = cursorHash compoundFirstCursor
compoundSecondHash = cursorHash compoundSecondCursor

compoundMissingQuery :: PD.Data
compoundMissingQuery = signerQuery compoundSigner $ PD.Constr 4 [PD.List oneSignerPeaks, PD.B presentSigner, PD.List []]

cursorBytes :: BS.ByteString -> (Integer, Integer, Integer, Integer) -> Integer -> Integer -> Integer -> BS.ByteString
cursorBytes payload frame@(kind, remaining, satisfied, required) offset visited pending =
  BS.concat
    [ "\x87\x58\x20"
    , blake2b256 payload
    , "\x58\x20"
    , blake2b256 $ frameDomain <> emptyStackRoot <> encodeFrame frame
    , "\x43"
    , be3 $ fromIntegral $ BS.length payload
    , "\x43"
    , be3 offset
    , "\x43\x00\x00\x01"
    , "\x43"
    , be3 visited
    , "\x41"
    , BS.singleton $ fromIntegral pending
    ]
  where
    _ = (kind, remaining, satisfied, required)

encodeFrame :: (Integer, Integer, Integer, Integer) -> BS.ByteString
encodeFrame (kind, remaining, satisfied, required) =
  BS.singleton (fromIntegral kind) <> be3 remaining <> be3 satisfied <> be3 required

frameData :: Integer -> Integer -> Integer -> Integer -> PD.Data
frameData kind remaining satisfied required = PD.Constr 0 [PD.I kind, PD.I remaining, PD.I satisfied, PD.I required]

be3 :: Integer -> BS.ByteString
be3 value = BS.pack [fromIntegral $ value `div` 65536, fromIntegral $ value `div` 256, fromIntegral value]

frameDomain, emptyStackRoot :: BS.ByteString
frameDomain = "MidgardNativeScriptFrameV1"
emptyStackRoot = blake2b256 frameDomain

cursorHash :: BS.ByteString -> BS.ByteString
cursorHash cursor = blake2b256 $ "MidgardNativeScriptWalkV1" <> cursor

q34Case :: String -> ([BS.ByteString] -> Assertion) -> TestTree
q34Case name assertion = testCase name $ assertion =<< mapM BS.readFile q34ChunkPaths

q34ChunkPaths :: [FilePath]
q34ChunkPaths = ["tests/fixtures/native-script-invalid-q34/chunk-0.bin", "tests/fixtures/native-script-invalid-q34/chunk-1.bin", "tests/fixtures/native-script-invalid-q34/chunk-2.bin"]

q34Step03 :: forall s. [BS.ByteString] -> BS.ByteString -> Term s PUnit
q34Step03 chunks successor =
  step03 $
    continueContext
      q34Step03State
      ( PD.Constr
          1
          [ PD.Constr
              1
              [ PD.I 0
              , PD.I 0
              , PD.B q34NativeScriptItem
              , q34Opening
              , PD.I 32
              ]
          ]
      )
      q34State32
      successor
      (q34ReferenceInputs chunks)

q34Step04 :: forall s. [BS.ByteString] -> PD.Data -> BS.ByteString -> Integer -> PD.Data -> Bool -> Term s PUnit
q34Step04 chunks inputState checkpoint budget outputState finalizing =
  step04 $
    continueContext
      inputState
      ( PD.Constr
          1
          [ PD.Constr
              (if finalizing then 1 else 0)
              [PD.I 0, PD.I 0, q34Opening, PD.B checkpoint, PD.I budget]
          ]
      )
      outputState
      (if finalizing then nextScript else stepScript)
      (q34ReferenceInputs chunks)

q34Step05 :: forall s. Term s PUnit
q34Step05 =
  step05 $
    finalizeContext
      (stepDatum $ Just q34State318)
      ( PD.Constr
          1
          [ PD.Constr
              2
              [ PD.I 0
              , PD.I 0
              , PD.I 0
              , PD.B q34NativeScriptItem
              , PD.I 1
              , PD.List [q34AbsentQuery]
              ]
          ]
      )

q34Step03State, q34State32, q34State64, q34State288, q34State304, q34State318 :: PD.Data
q34Step03State =
  PD.Constr 0 [PD.B q34TxId, PD.B q34WitnessSetHash, PD.B $ blake2b256 q34NativeScriptItem, PD.I 0, PD.I 100]
q34State32 = q34Step04State q34CheckpointHash32 q34Previous32 32 q34Peaks32
q34State64 = q34Step04State q34CheckpointHash64 q34Previous64 64 q34Peaks64
q34State288 = q34Step04State q34CheckpointHash288 q34Previous288 288 q34Peaks288
q34State304 = q34Step04State q34CheckpointHash304 q34Previous304 304 q34Peaks304
q34State318 = q34Step05State q34NativeScriptItem q34Peaks318 readyPhase

q34Step04State :: BS.ByteString -> BS.ByteString -> Integer -> [PD.Data] -> PD.Data
q34Step04State checkpointHash previous count peaks =
  PD.Constr
    0
    [ PD.B q34TxId
    , PD.B q34WitnessSetHash
    , PD.B $ blake2b256 q34NativeScriptItem
    , PD.I 0
    , PD.I 100
    , PD.B checkpointHash
    , PD.B previous
    , PD.I count
    , PD.List peaks
    ]

q34Step05State :: BS.ByteString -> [PD.Data] -> PD.Data -> PD.Data
q34Step05State scriptItem peaks phase =
  PD.Constr 0 [PD.B q34TxId, PD.B $ blake2b256 scriptItem, PD.I 0, PD.I 100, PD.I 318, PD.List peaks, phase]

q34Opening :: PD.Data
q34Opening =
  PD.Constr
    1
    [ PD.B q34CompactCbor
    , PD.Constr 0 [PD.B q34AddressHash, PD.B q34ScriptHash, PD.B q34RedeemerHash]
    , PD.Constr 2 [PD.I 0, PD.List [PD.I 1, PD.I 2, PD.I 3]]
    ]

q34ReferenceInputs :: [BS.ByteString] -> [TxInInfo]
q34ReferenceInputs chunks = q34CertificateInput : zipWith q34ChunkInput [1 ..] chunks

q34CertificateInput :: TxInInfo
q34CertificateInput =
  TxInInfo
    (TxOutRef (TxId $ toBuiltin q34TxId) 0)
    ( TxOut
        (scriptHashAddress $ ScriptHash $ toBuiltin $ unCS certificatePolicy)
        (adaValue 2_000_000 <> singleton certificatePolicy (TokenName $ toBuiltin ("MIDGARD_FIELD_PREIMAGE_CERT" :: BS.ByteString)) 1)
        (OutputDatum $ Datum $ dataToBuiltinData q34CertificateDatum)
        Nothing
    )

q34ChunkInput :: Integer -> BS.ByteString -> TxInInfo
q34ChunkInput index chunk =
  TxInInfo
    (TxOutRef (TxId $ toBuiltin q34TxId) index)
    ( TxOut
        (pubKeyHashAddress $ PubKeyHash $ toBuiltin prover)
        (adaValue 2_000_000)
        (OutputDatum $ Datum $ dataToBuiltinData $ PD.B chunk)
        Nothing
    )

q34CertificateDatum :: PD.Data
q34CertificateDatum =
  PD.Constr
    0
    [ PD.B prover
    , PD.B q34TxId
    , PD.I 7
    , PD.B q34AddressHash
    , PD.I 32757
    , PD.List $ map (PD.B . hex) ["26366e1cee678704dc218da2cf33cb754326bf8fe51b1de108b8ccea92e06032", "96084650107d9a17a7c4d9c37ae53af1023192c2236c0e2c7267c5646ff04744", "2a32995fefdf3483d00e1b3c654ede2dab8a1510b5d4e7ab6c585ea840052b3e"]
    ]

q34NativeScriptItem :: BS.ByteString
q34NativeScriptItem = versionedScriptItem 0 $ signaturePayload $ BS.replicate 28 0xff

q34AbsentQuery :: PD.Data
q34AbsentQuery =
  signerQuery (BS.replicate 28 0xff) $
    PD.Constr
      4
      [ PD.List q34Peaks318
      , PD.B q34Previous318
      , PD.List [PD.B $ hex "70a34b0a50899e8f064f44a2f85d7d1e15fdbd51434435d7df2c84d604d632c6"]
      ]

q34Peaks32, q34Peaks64, q34Peaks288, q34Peaks304, q34Peaks318 :: [PD.Data]
q34Peaks32 = [peak 5 "75f1bcc6cd0bc4f065a6f0e86f50c6906dd87835c24303964d4e56eb520ebc98"]
q34Peaks64 = [peak 6 "6b63f2529d4547c9fd332b04880026d9164db5bbea16114e4a3083801c5a8b18"]
q34Peaks288 = [peak 5 "a200e88dc07a54d5ae4d1fccfe9050273fa2414859354f9e38916fdc015938e6", peak 8 "738d1ebefdc3fc46f8ee2fc6cefa29aef784667366f75d70adfaf16612036df4"]
q34Peaks304 = peak 4 "e752280c3a26eecf94be5da5a4955c274896b9f39bc12313f9a6499a207914c8" : q34Peaks288
q34Peaks318 = [peak 1 "b5b3961284cce7d7c275bf95f6f3431884938813f9d0a83cced879e384b0e8e5", peak 2 "62eea386cfae9dd430a0213803f589211bff58b13982fdabf2bd3a3677187eb8", peak 3 "3747ab530d3dd3b00b19a55cb3e9143401be1e96a41bb216e02af737f9618840"] <> q34Peaks304

peak :: Integer -> String -> PD.Data
peak height digest = PD.Constr 0 [PD.I height, PD.B $ hex digest]

q34TxId, q34CompactCbor, q34AddressHash, q34ScriptHash, q34RedeemerHash, q34WitnessSetHash :: BS.ByteString
q34TxId = hex "d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23"
q34CompactCbor = hex "84018c5820f99641fc7bc7e291a96a6de01185240bbe7bd7b2ce2a5f6d816dc8e5c68a0c525820ccfbff4fea0f54213e078ce74e65bf844a143f9b74eece6fe005bb04742d239658206268504e96c250ed6ea83a2bc508404dd8dd3b900763c9d8f595dfc47d98d7cc1a000f42402020582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820a6f688ee8982ebdbffbfa608b43c087d3dc1c996df57e4626618da1a8e0494e3582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820101010101010101010101010101010101010101010101010101010101010101058201010101010101010101010101010101010101010101010101010101010101010005820e352221d0b5ae02610c47f76a0f7ffb63f72b72275d931d18e03ab4696754a6800"
q34AddressHash = hex "be3c48be9923f633253c5f46da8e1410bb26eaa79b617322dc29528cc44b77ef"
q34ScriptHash = hex "ad4b36af564e3d2cab118663351f7d658dad69932a9e0d5e8196783f16234a8e"
q34RedeemerHash = hex "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"
q34WitnessSetHash = hex "e352221d0b5ae02610c47f76a0f7ffb63f72b72275d931d18e03ab4696754a68"

q34Checkpoint32, q34Checkpoint64, q34Checkpoint288, q34Checkpoint304 :: BS.ByteString
q34Checkpoint32 = hex "865820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23410743007ff54300013e4300002043000ce3"
q34Checkpoint64 = hex "865820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23410743007ff54300013e43000040430019c3"
q34Checkpoint288 = hex "865820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23410743007ff54300013e43000120430073e3"
q34Checkpoint304 = hex "865820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23410743007ff54300013e4300013043007a53"

q34CheckpointHash32, q34CheckpointHash64, q34CheckpointHash288, q34CheckpointHash304 :: BS.ByteString
q34CheckpointHash32 = hex "756e2088a16c3b1fa9ac93a05559a192a7beaf48db71533524d67a2742bd524a"
q34CheckpointHash64 = hex "f8201734840cc29c2bb1e092512eda2dc0dcc36be0bc0bd71caf8dc1dbcb8c09"
q34CheckpointHash288 = hex "42435ba5c9e1b73bc0462e7f117520a4f7507aa640f394d0cffb8f24e0e5cc5d"
q34CheckpointHash304 = hex "403405148e7b6bc4cb7defdfed99127b7165176b7cda0faca64ceea31dbc3b85"

q34Previous32, q34Previous64, q34Previous288, q34Previous304, q34Previous318 :: BS.ByteString
q34Previous32 = hex "0f8101ff30ef95e19a91648426e29474d0fb04e39f9491d83cbd4411"
q34Previous64 = hex "27c648cbcfdbd9c23cc6e76e72c08ee8c421f579acc14d7a4e3edc48"
q34Previous288 = hex "ea14141719dc1075c008e3aabbb96e18b5c32d07c6ab77eabde3fea3"
q34Previous304 = hex "f2528f122d04b2588644a34f9d32a5aec5ed50b3335bd7030b74259a"
q34Previous318 = hex "ffa3561f45a11cafde2b91393f1a70fa537320fa3bc618804391f351"

startMaxScript, finalizeMaxScript :: forall s. BS.ByteString -> Term s PUnit
startMaxScript payload =
  let scriptItem = versionedScriptItem 0 payload
      first = maxFirstCursor payload
   in step05 $
        continueContext
          (q34Step05State scriptItem q34Peaks318 readyPhase)
          (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.B scriptItem, PD.I 32, PD.List []]])
          (q34Step05State scriptItem q34Peaks318 $ walkPhase $ cursorHash first)
          stepScript
          []
finalizeMaxScript payload =
  let scriptItem = versionedScriptItem 0 payload
      first = maxFirstCursor payload
      inputState = q34Step05State scriptItem q34Peaks318 $ walkPhase $ cursorHash first
      redeemer =
        PD.Constr
          1
          [ PD.Constr
              3
              [ PD.I 0
              , PD.I 0
              , PD.I 0
              , PD.B scriptItem
              , PD.B first
              , PD.List [maxFirstFrame payload]
              , PD.I 31
              , PD.List []
              ]
          ]
   in step05 $ finalizeContext (stepDatum $ Just inputState) redeemer

maxFirstCursor :: BS.ByteString -> BS.ByteString
maxFirstCursor payload =
  let (frame, pending) =
        if payload == maxSatisfiedPayload
          then ((1, 16, 15, 31), 2)
          else ((1, 16, 0, 31), 1)
   in cursorBytes payload frame 84 17 pending

maxFirstFrame :: BS.ByteString -> PD.Data
maxFirstFrame payload =
  if payload == maxSatisfiedPayload
    then frameData 1 16 15 31
    else frameData 1 16 0 31

maxUnsatisfiedPayload, maxSatisfiedPayload :: BS.ByteString
maxUnsatisfiedPayload = "\x82\x01\x98\x1f" <> BS.concat (replicate 31 "\x82\x04\x19\x03\xe8")
maxSatisfiedPayload = "\x82\x01\x98\x1f" <> BS.concat (replicate 31 "\x82\x05\x19\x03\xe8")

hex :: String -> BS.ByteString
hex = Base16.decodeLenient . BS.pack . map (fromIntegral . fromEnum)
