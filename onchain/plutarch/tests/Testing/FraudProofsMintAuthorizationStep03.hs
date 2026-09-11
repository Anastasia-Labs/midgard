{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsMintAuthorizationStep03 (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 (ScriptContext (..), ScriptHash (..), TxInfo (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.Validators.FraudProofs.MintAuthorization (mintAuthorizationStep03Validator)
import Plutarch.Prelude
import Testing.Eval (pfailsNoTraceWithoutHoistChecks, psucceedsNoTraceWithoutHoistChecks)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Mint-authorization step 03"
    [ testCase "clears an absent inline witness" $ succeeds $ absenceContext txScriptSpend absentPolicy 0 (scriptWitnessesPreimage txScriptSpend)
    , testCase "rejects absence when the native policy is inline" $ fails $ absenceContext nativeWitnessTx (versionedScriptHashOf 0 nativeScriptBytes) 0 (scriptWitnessesPreimage nativeWitnessTx)
    , testCase "rejects absence when a Plutus source matches" $ fails $ absenceContext plutusWitnessTx (versionedScriptHashOf 3 "\xde\xad\xbe\xef") 0 (scriptWitnessesPreimage plutusWitnessTx)
    , testCase "rejects a truncated field-6 preimage" $ fails $ absenceContext nativeWitnessTx (versionedScriptHashOf 0 nativeScriptBytes) 0 "\x80"
    , testCase "rejects absence under direction B" $ fails $ absenceContext txScriptSpend absentPolicy 1 (scriptWitnessesPreimage txScriptSpend)
    , testCase "convicts an unsigned signature policy" $ succeeds $ evaluateContext absentSignature 1 (-1) (-1) (addressWitnessesPreimage txScriptSpend) Nothing
    , testCase "convicts a failed all policy" $ succeeds $ evaluateContext failedAll 1 (-1) (-1) (addressWitnessesPreimage txScriptSpend) Nothing
    , testCase "convicts an empty any policy" $ succeeds $ evaluateContext "\x82\x02\x80" 1 (-1) (-1) (addressWitnessesPreimage txScriptSpend) Nothing
    , testCase "convicts an unmet after policy" $ succeeds $ evaluateContext "\x82\x04\x0f" 1 10 20 (addressWitnessesPreimage txScriptSpend) Nothing
    , testCase "rejects a satisfied signature policy" $ fails $ evaluateContext presentSignature 1 (-1) (-1) (addressWitnessesPreimage txScriptSpend) Nothing
    , testCase "rejects a satisfied any policy" $ fails $ evaluateContext satisfiedAny 1 (-1) (-1) (addressWitnessesPreimage txScriptSpend) Nothing
    , testCase "rejects a satisfied at-least policy" $ fails $ evaluateContext satisfiedAtLeast 1 (-1) (-1) (addressWitnessesPreimage txScriptSpend) Nothing
    , testCase "rejects a satisfied all with before" $ fails $ evaluateContext satisfiedAllBefore 1 10 20 (addressWitnessesPreimage txScriptSpend) Nothing
    , testCase "rejects a satisfied after policy" $ fails $ evaluateContext "\x82\x04\x05" 1 10 20 (addressWitnessesPreimage txScriptSpend) Nothing
    , testCase "rejects an empty all policy" $ fails $ evaluateContext "\x82\x01\x80" 1 (-1) (-1) (addressWitnessesPreimage txScriptSpend) Nothing
    , testCase "rejects a malformed policy payload" $ fails $ evaluateContext "\x82\x07\x00" 1 (-1) (-1) (addressWitnessesPreimage txScriptSpend) Nothing
    , testCase "rejects a mismatched script hash" $ fails $ evaluateContext absentSignature 1 (-1) (-1) (addressWitnessesPreimage txScriptSpend) (Just absentPolicy)
    , testCase "rejects a truncated field-7 preimage" $ fails $ evaluateContext absentSignature 1 (-1) (-1) "\x80" Nothing
    , testCase "rejects evaluation under direction A" $ fails $ evaluateContext absentSignature 0 (-1) (-1) (addressWitnessesPreimage txScriptSpend) Nothing
    , testCase "cancels under the prover signature" $ succeeds cancellationContext
    , testCase "rejects an unsigned cancellation" $ fails unsignedCancellationContext
    ]

succeeds, fails :: ScriptContext -> Assertion
succeeds ctx = psucceedsNoTraceWithoutHoistChecks (run ctx)
fails ctx = pfailsNoTraceWithoutHoistChecks (run ctx)

run :: forall s. ScriptContext -> Term s PUnit
run ctx =
  mintAuthorizationStep03Validator
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
    # pdata (pconstant ctPolicy)
    # pdata (pconstant certificatePolicy)
    # pconstant ctx

absentPolicy, absentKey, presentKey, priorRoot :: BS.ByteString
absentPolicy = BS.replicate 28 0x42
absentKey = BS.replicate 28 0x59
presentKey = keyHashFor 0
priorRoot = hash32 0x55

nativeWitnessTx, plutusWitnessTx :: Tx
nativeWitnessTx = txScriptSpend {tScripts = [(0, nativeScriptBytes)]}
plutusWitnessTx = txScriptSpend {tScripts = [(3, "\xde\xad\xbe\xef")]}

stateData :: Tx -> BS.ByteString -> Integer -> Integer -> Integer -> PD.Data
stateData tx policy direction validityStart validityEnd =
  PD.Constr
    0
    [ PD.B policy
    , PD.I direction
    , PD.B $ txIdOf tx
    , PD.B $ witnessSetHashOf tx
    , PD.I validityStart
    , PD.I validityEnd
    , PD.B priorRoot
    ]

absenceContext :: Tx -> BS.ByteString -> Integer -> BS.ByteString -> ScriptContext
absenceContext tx policy direction openedPreimage =
  spendContext
    (stepDatum $ Just $ stateData tx policy direction (tValidityStart tx) (tValidityEnd tx))
    ( PD.Constr
        1
        [ PD.Constr
            0
            [ PD.I 0
            , PD.I 0
            , witnessOpening (compactWithValidity tx (witnessSetHashOf tx) 0) tx openedPreimage
            ]
        ]
    )
    [threadInput]
    [stepOutput nextScript $ Just $ PD.Constr 0 [PD.B policy, PD.B $ txIdOf tx, PD.B priorRoot, PD.I 0]]
    []
    []
    mempty

evaluateContext :: BS.ByteString -> Integer -> Integer -> Integer -> BS.ByteString -> Maybe BS.ByteString -> ScriptContext
evaluateContext payload direction validityStart validityEnd openedPreimage policyOverride =
  spendContext
    (stepDatum $ Just $ stateData txScriptSpend policy direction validityStart validityEnd)
    ( PD.Constr
        1
        [ PD.Constr
            1
            [ PD.I 0
            , PD.I 0
            , PD.B payload
            , witnessOpening compact txScriptSpend openedPreimage
            ]
        ]
    )
    [threadInput]
    [stepOutput nextScript $ Just $ PD.Constr 0 [PD.B policy, PD.I 1]]
    []
    []
    mempty
  where
    compact = compactWithValidity txScriptSpend (witnessSetHashOf txScriptSpend) 0
    policy = maybe (versionedScriptHashOf 0 payload) id policyOverride

signature :: BS.ByteString -> BS.ByteString
signature key = "\x82\x00\x58\x1c" <> key

absentSignature, presentSignature, failedAll, satisfiedAny, satisfiedAtLeast, satisfiedAllBefore :: BS.ByteString
absentSignature = signature absentKey
presentSignature = signature presentKey
failedAll = "\x82\x01\x82" <> presentSignature <> absentSignature
satisfiedAny = "\x82\x02\x82" <> absentSignature <> presentSignature
satisfiedAtLeast = "\x83\x03\x01\x82" <> absentSignature <> presentSignature
satisfiedAllBefore = "\x82\x01\x82" <> presentSignature <> "\x82\x05\x18\x1e"

cancellationContext, unsignedCancellationContext :: ScriptContext
cancellationContext = cancelContext True
unsignedCancellationContext = cancelContext False

cancelContext :: Bool -> ScriptContext
cancelContext signedByProver =
  let context =
        spendContext
          (stepDatum $ Just $ stateData txScriptSpend absentPolicy 0 (-1) (-1))
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
