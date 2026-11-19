{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsNativeScriptDecodingAdvanceOrClose (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.NativeScriptDecoding.AdvanceOrClose (nativeScriptDecodingAdvanceOrCloseValidator)
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests = testGroup "Native-script decoding advance or close"
  [ testCase "commits a partial advance" $ psucceeds $ run $ advanceContext signatureItem 1 initialSignatureControl partialSignatureState stepScript
  , testCase "closes direction B at the exact terminal" $ psucceeds $ run $ advanceContext signatureItem 1 initialSignatureControl (closedState signatureItem 1 initialSignatureControl) otherScript
  , testCase "closes a direction A refusal" $ psucceeds $ run $ advanceContext malformedItem 0 initialMalformedControl (closedState malformedItem 0 initialMalformedControl) otherScript
  , testCase "rejects a replayed control" $ pfails $ run $ context signatureItem 1 terminalSignatureControl initialSignatureControl (singleChunkProof signatureItem) 10 (stateFor signatureItem 1 terminalSignatureControl) stepScript
  , testCase "rejects substituted chunk bytes" $ pfails $ run $ context signatureItem 1 initialSignatureControl initialSignatureControl (singleChunkProof malformedItem) 10 (stateFor signatureItem 1 initialSignatureControl) stepScript
  , testCase "rejects a direction B refusal" $ pfails $ run $ advanceContext malformedItem 1 initialMalformedControl (closedState malformedItem 1 initialMalformedControl) otherScript
  ]

run :: forall s. ScriptContext -> Term s PUnit
run ctx = nativeScriptDecodingAdvanceOrCloseValidator
  # pdata (pconstant $ ScriptHash $ toBuiltin otherScript)
  # pdata (pconstant ctPolicy)
  # pconstant ctx

advanceContext :: BS.ByteString -> Integer -> BS.ByteString -> PD.Data -> BS.ByteString -> ScriptContext
advanceContext item direction control outputState outputScript =
  context item direction control control (singleChunkProof item) budget outputState outputScript
  where
    budget = if item == signatureItem && outputScript == stepScript then 1 else 10

context :: BS.ByteString -> Integer -> BS.ByteString -> BS.ByteString -> PD.Data -> Integer -> PD.Data -> BS.ByteString -> ScriptContext
context item direction committedControl suppliedControl proof budget outputState outputScript = spendContext
  (stepDatum $ Just $ stateFor item direction committedControl)
  (PD.Constr 1
    [ PD.Constr 0
        [ PD.I 0, PD.I 0, PD.B suppliedControl
        , PD.Constr 0 [proof], PD.Constr 1 [], PD.List [], PD.I budget
        ]
    ])
  [threadInput]
  [stepOutput outputScript $ Just outputState]
  []
  []
  mempty

stateFor :: BS.ByteString -> Integer -> BS.ByteString -> PD.Data
stateFor item direction control = PD.Constr 0
  [ PD.I direction, PD.I $ if direction == 1 then 1 else 0, PD.B txScriptSpendId
  , PD.B $ if direction == 1 then hash32 0x66 else "", PD.I (-1)
  , PD.B $ hash32 0x44, PD.I 0, PD.I 0
  , PD.B $ hash32 0x33, PD.I 0, PD.I 0
  , PD.I $ fromIntegral $ BS.length item, PD.B $ boundedItemCommitment 0 item
  , PD.B $ machineHash control, PD.I (-1)
  ]

partialSignatureState :: PD.Data
partialSignatureState = replaceMachineHash (stateFor signatureItem 1 initialSignatureControl) partialSignatureControl

closedState :: BS.ByteString -> Integer -> BS.ByteString -> PD.Data
closedState item direction control = replaceRefusal (stateFor item direction control) 0

replaceMachineHash :: PD.Data -> BS.ByteString -> PD.Data
replaceMachineHash (PD.Constr 0 fields) control = PD.Constr 0 $ take 13 fields <> [PD.B $ machineHash control] <> drop 14 fields
replaceMachineHash _ _ = error "unexpected scan state"

replaceRefusal :: PD.Data -> Integer -> PD.Data
replaceRefusal (PD.Constr 0 fields) refusal = PD.Constr 0 $ take 14 fields <> [PD.I refusal]
replaceRefusal _ _ = error "unexpected scan state"

machineHash :: BS.ByteString -> BS.ByteString
machineHash control = blake2b256 $ "midgard/fraud-proofs/native-script-decoding/control-v1" <> control

signatureItem, malformedItem :: BS.ByteString
signatureItem = "\x82\x00\x58\x20\x82\x00\x58\x1c" <> BS.replicate 28 0x99
malformedItem = "\x82\x00\x43\x82\x07\x00"

initialSignatureControl, partialSignatureControl, terminalSignatureControl, initialMalformedControl :: BS.ByteString
initialSignatureControl = controlCbor 0 4 4 36 0
partialSignatureControl = controlCbor 2 4 36 36 1
terminalSignatureControl = controlCbor 3 4 36 36 1
initialMalformedControl = controlCbor 0 3 3 6 0

controlCbor :: Integer -> Integer -> Integer -> Integer -> Integer -> BS.ByteString
controlCbor stage start cursor end count =
  arrayHeader 8 <> cborInt 1 <> cborInt stage <> cborInt start <> cborInt cursor
    <> cborInt end <> wrapItem "" <> cborInt 0 <> cborInt count

boundedItemCommitment :: Integer -> BS.ByteString -> BS.ByteString
boundedItemCommitment outputIndex item = blake2b256 $
  "MidgardBoundedItemCommitmentV1" <> arrayHeader 5 <> cborInt 1 <> cborInt 2
    <> cborInt outputIndex <> cborInt (fromIntegral $ BS.length item) <> wrapItem frontier
  where
    leaf = chunkLeaf outputIndex item
    frontier = blake2b256 $
      "MidgardValidationMerkleFrontierV1" <> cborInt 1
        <> arrayHeader 1 <> "\x82" <> cborInt 0 <> wrapItem leaf

chunkLeaf :: Integer -> BS.ByteString -> BS.ByteString
chunkLeaf outputIndex item = blake2b256 $
  "MidgardBoundedItemChunkV1" <> arrayHeader 5 <> cborInt 1 <> cborInt 2
    <> cborInt outputIndex <> cborInt 0 <> wrapItem item

singleChunkProof :: BS.ByteString -> PD.Data
singleChunkProof item = PD.Constr 0
  [ PD.I 1, PD.I 2, PD.I 0, PD.I $ fromIntegral $ BS.length item, PD.I 0, PD.B item
  , PD.List [PD.Constr 0 [PD.I 0, PD.B $ chunkLeaf 0 item]]
  , PD.List []
  ]
