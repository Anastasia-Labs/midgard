{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsMintAuthorizationStep04 (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 (ScriptContext (..), ScriptHash (..), TxInfo (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.Validators.FraudProofs.MintAuthorization (mintAuthorizationStep04Validator)
import Plutarch.Prelude
import Testing.Eval (pfailsNoTraceWithoutHoistChecks, psucceedsNoTraceWithoutHoistChecks)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Mint-authorization step 04"
    [ testCase "clears a scriptless resolved output" $ succeeds $ resolveContext accusedPolicy (descriptorCbor (-1)) (ledgerRoot $ descriptorCbor (-1)) 0 honestPreimage
    , testCase "clears a foreign reference script" $ succeeds $ resolveContext accusedPolicy (descriptorCbor 0) (ledgerRoot $ descriptorCbor 0) 0 honestPreimage
    , testCase "rejects a resolved policy source" $ fails $ resolveContext referenceHash (descriptorCbor 3) (ledgerRoot $ descriptorCbor 3) 0 honestPreimage
    , testCase "rejects an out-of-range cursor" $ fails $ resolveContext accusedPolicy (descriptorCbor (-1)) (ledgerRoot $ descriptorCbor (-1)) 1 honestPreimage
    , testCase "rejects a non-member descriptor" $ fails $ resolveContext accusedPolicy (descriptorCbor (-1)) (ledgerRoot $ descriptorCbor 0) 0 honestPreimage
    , testCase "rejects a truncated field-1 preimage" $ fails $ resolveContext accusedPolicy (descriptorCbor (-1)) (ledgerRoot $ descriptorCbor (-1)) 0 "\x80"
    , testCase "completes at the item count" $ succeeds $ completeContext 1 honestPreimage
    , testCase "rejects an early completion" $ fails $ completeContext 0 honestPreimage
    , testCase "rejects completion over a truncated preimage" $ fails $ completeContext 0 "\x80"
    , testCase "cancels under the prover signature" $ succeeds $ cancellationContext True
    , testCase "rejects an unsigned cancellation" $ fails $ cancellationContext False
    ]

succeeds, fails :: ScriptContext -> Assertion
succeeds ctx = psucceedsNoTraceWithoutHoistChecks (run ctx)
fails ctx = pfailsNoTraceWithoutHoistChecks (run ctx)

run :: forall s. ScriptContext -> Term s PUnit
run ctx =
  mintAuthorizationStep04Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant ctx

accusedPolicy, referenceHash :: BS.ByteString
accusedPolicy = BS.replicate 28 0x42
referenceHash = BS.replicate 28 0x99

outpointCbor, honestPreimage, compact :: BS.ByteString
outpointCbor = encodedInput sharedInputRef
honestPreimage = referenceInputsPreimage tx3
compact = compactWithValidity tx3 (witnessSetHashOf tx3) 0

ledgerRoot :: BS.ByteString -> BS.ByteString
ledgerRoot = singleEntryPhasRoot outpointCbor

stateData :: BS.ByteString -> BS.ByteString -> Integer -> PD.Data
stateData policy root cursor =
  PD.Constr
    0
    [PD.B policy, PD.B tx3Id, PD.B root, PD.I cursor]

resolveContext :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Integer -> BS.ByteString -> ScriptContext
resolveContext policy descriptor root cursor openedPreimage =
  spendContext
    (stepDatum $ Just $ stateData policy root cursor)
    ( PD.Constr
        1
        [ PD.Constr
            0
            [ PD.I 0
            , PD.I 0
            , bodyOpening compact openedPreimage
            , PD.B descriptor
            , PD.List []
            ]
        ]
    )
    [threadInput]
    [stepOutput stepScript $ Just $ stateData policy root (cursor + 1)]
    []
    []
    mempty

completeContext :: Integer -> BS.ByteString -> ScriptContext
completeContext cursor openedPreimage =
  spendContext
    (stepDatum $ Just $ stateData accusedPolicy (hash32 0x55) cursor)
    (PD.Constr 1 [PD.Constr 1 [PD.I 0, PD.I 0, bodyOpening compact openedPreimage]])
    [threadInput]
    [stepOutput nextScript $ Just $ PD.Constr 0 [PD.B accusedPolicy, PD.I 0]]
    []
    []
    mempty

cancellationContext :: Bool -> ScriptContext
cancellationContext signedByProver =
  let context =
        spendContext
          (stepDatum $ Just $ stateData accusedPolicy (hash32 0x55) 0)
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

descriptorCbor :: Integer -> BS.ByteString
descriptorCbor language =
  BS.concat
    [ arrayHeader 16
    , cborInt 1
    , cborInt 1
    , cborInt 5_000
    , wrapItem $ hash32 0x55
    , wrapItem $ "\x60" <> BS.replicate 28 0x11
    , cborInt 5_000_000
    , cborInt 0
    , wrapItem emptyAssetRoot
    , cborInt 5
    , cborInt language
    , wrapItem scriptHash
    , cborInt scriptLength
    , wrapItem scriptCommitment
    , summary 101 202
    , summary 103 204
    , summary 3 4
    ]
  where
    scriptHash = if language == -1 then "" else referenceHash
    scriptLength = if language == -1 then 0 else 5
    scriptCommitment = if language == -1 then "" else hash32 0x44
    summary cborLength memory = arrayHeader 3 <> wrapItem (hash32 0x55) <> cborInt cborLength <> cborInt memory

emptyAssetRoot :: BS.ByteString
emptyAssetRoot =
  BS.pack
    [ 0xb6
    , 0x57
    , 0x5c
    , 0x6c
    , 0x81
    , 0x26
    , 0x4f
    , 0xc5
    , 0xd6
    , 0x80
    , 0x29
    , 0x05
    , 0xbc
    , 0x4c
    , 0xb0
    , 0x1d
    , 0x26
    , 0xfc
    , 0xca
    , 0x7c
    , 0x75
    , 0x41
    , 0x27
    , 0x12
    , 0xfd
    , 0x4d
    , 0x4b
    , 0x7e
    , 0x5a
    , 0x23
    , 0xd6
    , 0xcd
    ]
