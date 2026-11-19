{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsNativeScriptDecodingBindDescriptor (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 (ScriptContext, ScriptHash (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.FraudProofs.NativeScriptDecoding.BindDescriptor (nativeScriptDecodingBindDescriptorValidator)
import Testing.Eval (pfails, psucceeds)
import Testing.FraudProofsFixture

tests :: TestTree
tests = testGroup "Native-script decoding bind descriptor"
  [ testCase "authenticates and starts the machine" $ psucceeds $ run $ context nativeSetup 0 nativeExpected nextScript nativeArgs
  , testCase "closes a non-native direction-B descriptor" $ psucceeds $ run $ context plutusSetup 1 plutusClosed otherScript plutusArgs
  , testCase "rejects a substituted outpoint key" $ pfails $ run $ context nativeSetup 0 nativeOpened nextScript (nativeArgsWithKey "\x00")
  , testCase "rejects a descriptor the ledger never held" $ pfails $ run $ context nativeSetup 0 nativeOpened nextScript substitutedDescriptorArgs
  , testCase "rejects direction A for a non-native descriptor" $ pfails $ run $ context plutusSetup 0 plutusClosed otherScript plutusArgs
  ]

run :: forall s. ScriptContext -> Term s PUnit
run ctx = nativeScriptDecodingBindDescriptorValidator
  # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
  # pdata (pconstant $ ScriptHash $ toBuiltin otherScript)
  # pdata (pconstant ctPolicy)
  # pconstant ctx

data Setup = Setup
  { setupItem :: BS.ByteString
  , setupLanguage :: Integer
  , setupDescriptor :: BS.ByteString
  , setupItemCommitment :: BS.ByteString
  , setupLedgerRoot :: BS.ByteString
  }

mkSetup :: Integer -> BS.ByteString -> Setup
mkSetup language item = Setup item language descriptor commitment ledgerRoot
  where
    commitment = boundedItemCommitment 0 item
    descriptor = descriptorCbor language item commitment
    ledgerRoot = singleEntryPhasRoot outpointCbor descriptor

nativeSetup, plutusSetup :: Setup
nativeSetup = mkSetup 0 signatureItem
plutusSetup = mkSetup 3 plutusItem

signatureItem, malformedItem, plutusItem :: BS.ByteString
signatureItem = "\x82\x00\x58\x20\x82\x00\x58\x1c" <> BS.replicate 28 0x99
malformedItem = "\x82\x00\x43\x82\x07\x00"
plutusItem = "\x82\x03\x44\x01\x02\x03\x04"

outpoint :: (BS.ByteString, Integer)
outpoint = case spendInputsOf txScriptSpend of
  first : _ -> first
  [] -> error "txScriptSpend fixture has no spend input"

outpointCbor :: BS.ByteString
outpointCbor = encodedInput outpoint

openedState :: Setup -> Integer -> PD.Data
openedState setup direction = PD.Constr 0
  [ PD.I direction, PD.I $ if direction == 1 then 1 else 0, PD.B txScriptSpendId
  , PD.B $ if direction == 1 then hash32 0x66 else "", PD.I (-1)
  , PD.B $ setupLedgerRoot setup, PD.I 0, PD.I 0
  , PD.B $ blake2b256 outpointCbor, PD.I (-2), PD.I $ snd outpoint
  , PD.I (-1), PD.B "", PD.B "", PD.I (-1)
  ]

boundState :: Setup -> Integer -> BS.ByteString -> Integer -> PD.Data
boundState setup direction machineHash refusal = PD.Constr 0
  [ PD.I direction, PD.I $ if direction == 1 then 1 else 0, PD.B txScriptSpendId
  , PD.B $ if direction == 1 then hash32 0x66 else "", PD.I (-1)
  , PD.B $ setupLedgerRoot setup, PD.I 0, PD.I 0
  , PD.B $ blake2b256 outpointCbor, PD.I $ setupLanguage setup, PD.I $ snd outpoint
  , PD.I $ fromIntegral $ BS.length $ setupItem setup, PD.B $ setupItemCommitment setup
  , PD.B machineHash, PD.I refusal
  ]

nativeOpened, nativeExpected, plutusClosed :: PD.Data
nativeOpened = openedState nativeSetup 0
nativeExpected = boundState nativeSetup 0 (blake2b256 $ "midgard/fraud-proofs/native-script-decoding/control-v1" <> initialControlCbor) (-1)
plutusClosed = boundState plutusSetup 1 "" 0

initialControlCbor :: BS.ByteString
initialControlCbor = arrayHeader 8 <> cborInt 1 <> cborInt 0 <> cborInt 4 <> cborInt 4
  <> cborInt (fromIntegral $ BS.length signatureItem) <> wrapItem "" <> cborInt 0 <> cborInt 0

data Args = Args
  { argsKey :: BS.ByteString
  , argsDescriptor :: BS.ByteString
  , argsFirstProof :: Maybe PD.Data
  }

nativeArgs, plutusArgs :: Args
nativeArgs = Args outpointCbor (setupDescriptor nativeSetup) (Just $ singleChunkProof signatureItem)
plutusArgs = Args outpointCbor (setupDescriptor plutusSetup) Nothing

nativeArgsWithKey :: BS.ByteString -> Args
nativeArgsWithKey key = nativeArgs {argsKey = key}

substitutedDescriptorArgs :: Args
substitutedDescriptorArgs = nativeArgs
  { argsDescriptor = setupDescriptor $ mkSetup 0 malformedItem
  , argsFirstProof = Just $ singleChunkProof malformedItem
  }

context :: Setup -> Integer -> PD.Data -> BS.ByteString -> Args -> ScriptContext
context setup direction outputState outputScript args = spendContext
  (stepDatum $ Just $ openedState setup direction)
  (PD.Constr 1
    [ PD.Constr 0
        [ PD.I 0, PD.I 0, PD.B $ argsKey args, PD.B $ argsDescriptor args
        , PD.List []
        , maybe (PD.Constr 1 []) (PD.Constr 0 . pure) (argsFirstProof args)
        ]
    ])
  [threadInput]
  [stepOutput outputScript $ Just outputState]
  []
  []
  mempty

descriptorCbor :: Integer -> BS.ByteString -> BS.ByteString -> BS.ByteString
descriptorCbor language item itemCommitment = BS.concat
  [ arrayHeader 16, cborInt 1, cborInt 0, cborInt 5_000, wrapItem $ hash32 0x55
  , wrapItem "\x60\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11"
  , cborInt 5_000_000, cborInt 0
  , wrapItem $ BS.pack [0xb6,0x57,0x5c,0x6c,0x81,0x26,0x4f,0xc5,0xd6,0x80,0x29,0x05,0xbc,0x4c,0xb0,0x1d,0x26,0xfc,0xca,0x7c,0x75,0x41,0x27,0x12,0xfd,0x4d,0x4b,0x7e,0x5a,0x23,0xd6,0xcd]
  , cborInt 5, cborInt language, wrapItem $ BS.replicate 28 0x99
  , cborInt $ fromIntegral $ BS.length item, wrapItem itemCommitment
  , summary 101 202, summary 103 204, summary 3 4
  ]
  where
    summary cborLength memory = arrayHeader 3 <> wrapItem (hash32 0x55) <> cborInt cborLength <> cborInt memory

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
