{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsNativeScriptDecodingStep02 (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.NativeScriptDecoding.Step02 (nativeScriptDecodingStep02Validator)
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests = testGroup "Native-script decoding step 02"
  [ testCase "opens a normal committed claim" $ psucceeds $ run (claimContext l2Phase Nothing)
  , testCase "opens a forced acceptance" $ psucceeds $ run forcedAcceptanceContext
  , testCase "copies a forced malformed accusation verbatim" $ psucceeds $ run forcedRejectionContext
  , testCase "rejects a foreign forced rejection arm" $ pfails $ run foreignRejectionContext
  , testCase "rejects a forced verdict/scalar mismatch" $ pfails $ run forcedScalarMismatchContext
  , testCase "rejects a header that is not the thread's" $ pfails $ run (claimContext l2Phase $ Just wrongAssetName)
  , testCase "rejects an event phase mismatch" $ pfails $ run (claimContext depositPhase Nothing)
  , testCase "rejects direction B on a normal source" $ pfails $ run directionBNormalContext
  , testCase "cancels under the prover signature" $ psucceeds $ run cancellationContext
  , testCase "rejects an unsigned cancellation" $ pfails $ run unsignedCancellationContext
  ]

run :: forall s. ScriptContext -> Term s PUnit
run ctx = nativeScriptDecodingStep02Validator
  # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
  # pdata (pconstant ctPolicy)
  # pconstant ctx

txId, priorRoot, postRoot, categoryId :: BS.ByteString
txId = BS.replicate 32 0x77
priorRoot = BS.replicate 32 0x55
postRoot = BS.replicate 32 0x66
categoryId = BS.replicate 4 0xa1

l2Phase, depositPhase :: PD.Data
l2Phase = PD.Constr 2 []
depositPhase = PD.Constr 3 []

eventKey :: PD.Data
eventKey = PD.Constr 2 [PD.B txId]

bindState :: Integer -> PD.Data
bindState direction = PD.Constr 0 [PD.I direction, PD.I 0, PD.B txId]

scanState :: Integer -> PD.Data
scanState direction = PD.Constr 0
  [ PD.I direction
  , PD.I 0
  , PD.B txId
  , PD.B ""
  , PD.I (-1)
  , PD.B priorRoot
  , PD.I 0
  , PD.I 0
  , PD.B ""
  , PD.I (-2)
  , PD.I (-1)
  , PD.I (-1)
  , PD.B ""
  , PD.B ""
  , PD.I (-1)
  ]

claim :: PD.Data -> (PD.Data, PD.Data, PD.Data)
claim phase = (header, eventMembership, stepMembership)
  where
    step = PD.Constr 0 [PD.I 1, PD.I 0, eventKey, phase, PD.B priorRoot, PD.B postRoot]
    eventValue = PD.Constr 0 [PD.I 0, phase]
    stepPhas = singleEntryPhasRoot (serialise $ PD.I 0) (serialise step)
    eventPhas = singleEntryPhasRoot (serialise eventKey) (serialise eventValue)
    stepRoot = commitCountedRoot 4 stepPhas 1
    eventRoot = commitCountedRoot 5 eventPhas 1
    header = headerData stepRoot eventRoot
    eventMembership = membershipProof 5 eventRoot eventPhas 1 eventKey eventValue
    stepMembership = membershipProof 4 stepRoot stepPhas 1 (PD.I 0) step

headerData :: BS.ByteString -> BS.ByteString -> PD.Data
headerData stepRoot eventRoot = PD.Constr 0
  [ PD.B $ hash32 0x01
  , PD.B $ hash32 0x02
  , PD.B $ hash32 0x03
  , PD.B $ hash32 0x04
  , PD.B $ hash32 0x05
  , PD.B $ hash32 0x06
  , PD.B stepRoot
  , PD.B eventRoot
  , PD.B $ hash32 0x09
  , PD.I 0, PD.I 0, PD.I 1, PD.I 0, PD.I 1, PD.I 1, PD.I 0
  , PD.I 100, PD.I 200, PD.I 0, PD.I 0, PD.I 0, PD.I 0
  , PD.B $ BS.replicate 28 0x02
  , PD.B prover
  , PD.I 1
  ]

assetNameOf :: PD.Data -> BS.ByteString
assetNameOf header = categoryId <> blake2b224 (serialise header)

wrongAssetName :: BS.ByteString
wrongAssetName = categoryId <> BS.replicate 28 0xdd

step02Redeemer :: PD.Data -> PD.Data -> PD.Data -> PD.Data
step02Redeemer header eventMembership stepMembership = PD.Constr 1
  [ PD.Constr 0
      [ PD.I 0
      , PD.I 0
      , header
      , eventMembership
      , stepMembership
      , PD.Constr 1 []
      , PD.I 0
      , PD.I 0
      ]
  ]

claimContext :: PD.Data -> Maybe BS.ByteString -> ScriptContext
claimContext phase assetOverride = claimContextWithDirection phase assetOverride 0

directionBNormalContext :: ScriptContext
directionBNormalContext = claimContextWithDirection l2Phase Nothing 1

claimContextWithDirection :: PD.Data -> Maybe BS.ByteString -> Integer -> ScriptContext
claimContextWithDirection phase assetOverride direction =
  spendContext
    (stepDatum $ Just $ bindState direction)
    (step02Redeemer header eventMembership stepMembership)
    [threadInputWithName assetName]
    [stepOutputWithName nextScript (Just $ scanState direction) assetName]
    []
    []
    mempty
  where
    (header, eventMembership, stepMembership) = claim phase
    assetName = maybe (assetNameOf header) id assetOverride

forcedAcceptanceContext, forcedRejectionContext, foreignRejectionContext, forcedScalarMismatchContext :: ScriptContext
forcedAcceptanceContext = forcedContext 0 0 forcedValidVerdict (-1) 1 0
forcedRejectionContext = forcedContext 1 1 forcedMalformedVerdict 0 1 3
foreignRejectionContext = forcedContext 1 1 forcedForeignVerdict 0 0 0
forcedScalarMismatchContext = forcedContext 0 1 forcedValidVerdict (-1) 0 0

forcedContext :: Integer -> Integer -> PD.Data -> Integer -> Integer -> Integer -> ScriptContext
forcedContext direction validityCode verdict scanClass accusedKind accusedCursor =
  spendContext
    (stepDatum $ Just $ PD.Constr 0 [PD.I direction, PD.I 1, PD.B ""])
    redeemer
    [threadInputWithName assetName]
    [stepOutputWithName nextScript (Just expectedState) assetName]
    []
    []
    mempty
  where
    compact = compactWithValidity txScriptSpend (witnessSetHashOf txScriptSpend) validityCode
    source = PD.Constr 0
      [ PD.B compact
      , PD.B $ witnessSetCborOf txScriptSpend
      , PD.B $ BS.pack $ 0x89 : replicate 9 0
      ]
    leaf = PD.Constr 0 [PD.B txScriptSpendId, source, verdict]
    orderKey = PD.Constr 0 [PD.B $ BS.replicate 32 0x88, PD.I 0]
    orderKeyBytes = serialise orderKey
    forcedPhas = singleEntryPhasRoot orderKeyBytes (serialise leaf)
    forcedRoot = commitCountedRoot 1 forcedPhas 1
    forcedProof = membershipProof 1 forcedRoot forcedPhas 1 orderKey leaf
    forcedEventKey = PD.Constr 1 [orderKey]
    phase = PD.Constr 1 []
    step = PD.Constr 0 [PD.I 1, PD.I 0, forcedEventKey, phase, PD.B priorRoot, PD.B postRoot]
    eventValue = PD.Constr 0 [PD.I 0, phase]
    stepPhas = singleEntryPhasRoot (serialise $ PD.I 0) (serialise step)
    eventPhas = singleEntryPhasRoot (serialise forcedEventKey) (serialise eventValue)
    stepRoot = commitCountedRoot 4 stepPhas 1
    eventRoot = commitCountedRoot 5 eventPhas 1
    header = forcedHeaderData forcedRoot stepRoot eventRoot
    assetName = assetNameOf header
    eventProof = membershipProof 5 eventRoot eventPhas 1 forcedEventKey eventValue
    stepProof = membershipProof 4 stepRoot stepPhas 1 (PD.I 0) step
    redeemer = PD.Constr 1
      [ PD.Constr 0
          [ PD.I 0, PD.I 0, header, eventProof, stepProof
          , PD.Constr 0 [forcedProof]
          , PD.I accusedKind, PD.I accusedCursor
          ]
      ]
    expectedState = PD.Constr 0
      [ PD.I direction, PD.I 1, PD.B txScriptSpendId, PD.B orderKeyBytes
      , PD.I scanClass, PD.B priorRoot, PD.I accusedKind, PD.I accusedCursor
      , PD.B "", PD.I (-2), PD.I (-1), PD.I (-1), PD.B "", PD.B "", PD.I (-1)
      ]

forcedValidVerdict, forcedMalformedVerdict, forcedForeignVerdict :: PD.Data
forcedValidVerdict = PD.Constr 0 []
forcedMalformedVerdict = PD.Constr 1 [PD.Constr 20 [PD.I 1, PD.I 3]]
forcedForeignVerdict = PD.Constr 1 [PD.Constr 6 []]

forcedHeaderData :: BS.ByteString -> BS.ByteString -> BS.ByteString -> PD.Data
forcedHeaderData forcedRoot stepRoot eventRoot = PD.Constr 0
  [ PD.B $ hash32 0x01, PD.B $ hash32 0x02, PD.B $ hash32 0x03
  , PD.B forcedRoot, PD.B $ hash32 0x05, PD.B $ hash32 0x06
  , PD.B stepRoot, PD.B eventRoot, PD.B $ hash32 0x09
  , PD.I 0, PD.I 1, PD.I 0, PD.I 0, PD.I 1, PD.I 1, PD.I 0
  , PD.I 100, PD.I 200, PD.I 0, PD.I 0, PD.I 0, PD.I 0
  , PD.B $ BS.replicate 28 0x02, PD.B prover, PD.I 1
  ]

cancellationContext, unsignedCancellationContext :: ScriptContext
cancellationContext = cancelWith threadAssetName
unsignedCancellationContext = cancelWith otherThreadName

threadAssetName :: BS.ByteString
threadAssetName = let (header, _, _) = claim l2Phase in assetNameOf header

cancelWith :: BS.ByteString -> ScriptContext
cancelWith burnedName = spendContext
  (stepDatum $ Just $ bindState 0)
  cancelRedeemer
  [threadInputWithName threadAssetName]
  []
  []
  [cancelMintEntry burnedName]
  mempty
