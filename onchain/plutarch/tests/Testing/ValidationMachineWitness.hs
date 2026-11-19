{-# LANGUAGE OverloadedStrings #-}

module Testing.ValidationMachineWitness (tests) where

import Data.ByteString qualified as BS
import Plutarch.Prelude
import Midgard.ValidationMachine
import Midgard.ScriptLanguageViews qualified as LanguageViews
import Midgard.ValidationTrace
import Test.Tasty
import Test.Tasty.HUnit
import Testing.Eval (passertEvalNoTrace, pfailsNoTraceWithoutHoistChecks)

tests :: TestTree
tests = testGroup "Fixed-target machine witness"
  [ testGroup "canonical discovery bitmap"
      [ testCase label $ passertEvalNoTrace $
          pencodeScriptDiscoveryBitmap # pconstant value #== pconstant bytes
            #&& pdecodeScriptDiscoveryBitmap # pconstant bytes #== pconstant value
      | (label, value, bytes) <- [("zero", 0, ""), ("one", 1, "\x01"), ("one-byte maximum", 255, "\xff"), ("two-byte minimum", 256, "\x01\x00"), ("2048-byte maximum", 2 ^ (16384 :: Int) - 1, BS.replicate 2048 255)]
      ]
  , testGroup "native witness byte-string encoding"
      [ testCase label $ passertEvalNoTrace $
          nativeWitnessBytes (pconstant bytes) #== pconstant
            ("\x9f" <> wire <> wire <> wire <> wire <> "\x58\x20" <> BS.replicate 32 170
              <> "\x00\x20\x00\x00\x00\x40\x00\x40\x00\x00\x20\x00\x82\x80" <> wire <> "\xff")
      | (label, bytes, wire) <-
          [ ("64 bytes remain definite", BS.replicate 64 97, "\x58\x40" <> BS.replicate 64 97)
          , ("65 bytes split into 64 and 1", BS.replicate 65 97, "\x5f\x58\x40" <> BS.replicate 64 97 <> "\x41\x61\xff")
          , ("128 bytes split into two chunks", BS.replicate 128 97, "\x5f\x58\x40" <> BS.replicate 64 97 <> "\x58\x40" <> BS.replicate 64 97 <> "\xff")
          ]
      ]
  , testCase "negative bitmap refused" $ pfailsNoTraceWithoutHoistChecks $ pencodeScriptDiscoveryBitmap # (-1)
  , testCase "oversize integer refused" $ pfailsNoTraceWithoutHoistChecks $ pencodeScriptDiscoveryBitmap # pconstant (2 ^ (16384 :: Int))
  , testCase "oversize bytes refused" $ pfailsNoTraceWithoutHoistChecks $ pdecodeScriptDiscoveryBitmap # pconstant (BS.replicate 2049 1)
  , testCase "leading zero refused" $ pfailsNoTraceWithoutHoistChecks $ pdecodeScriptDiscoveryBitmap # pconstant "\x00\x01"
  , testCase "zero byte refused" $ pfailsNoTraceWithoutHoistChecks $ pdecodeScriptDiscoveryBitmap # pconstant "\x00"
  , testCase "control wire uses bytes for both bitmap fields" $ passertEvalNoTrace $
      pencodeScriptDiscoveryControl # pemptyScriptDiscoveryControl #== pconstant "\x8f\x00\x00\x00\x20\x20\x40\x40\x20\x20\x40\x40\x40\x40\x00\x80"
  , testCase "control decoder rejects former integer bitmap wire" $ pfailsNoTraceWithoutHoistChecks $
      pdecodeScriptDiscoveryControl # pconstant "\x8f\x00\x00\x00\x20\x20\x40\x40\x20\x20\x40\x00\x00\x40\x00\x80"
  , testCase "maximum bitmap control roundtrip" $ passertEvalNoTrace $
      pmatch pemptyScriptDiscoveryControl $ \empty ->
        plet (pcon empty { pscriptDiscovery'usedInlineBitmap = pdata $ pconstant (2 ^ (16384 :: Int) - 1), pscriptDiscovery'usedRedeemerBitmap = pdata 256 }) $ \control ->
          pdecodeScriptDiscoveryControl # (pencodeScriptDiscoveryControl # control) #== control
  , testGroup "script-integrity absence precondition"
      [ testCase "canonical empty hash permits native-only execution" $ passertEvalNoTrace $
          pphaseAScriptPreconditionsRejection # 0 # pconstant False # pconstant False # LanguageViews.pemptyScriptIntegrityHash # 0 #== pcon PNothing
      , testCase "canonical empty hash refuses redeemers" $ passertEvalNoTrace $
          pphaseAScriptPreconditionsRejection # 0 # pconstant True # pconstant False # LanguageViews.pemptyScriptIntegrityHash # 0 #== pcon (PJust $ pconstant "E_INVALID_FIELD_TYPE")
      , testCase "canonical empty hash refuses non-native script" $ passertEvalNoTrace $
          pphaseAScriptPreconditionsRejection # 0 # pconstant False # pconstant True # LanguageViews.pemptyScriptIntegrityHash # 0 #== pcon (PJust $ pconstant "E_INVALID_FIELD_TYPE")
      , testCase "zero hash is a supplied integrity hash" $ passertEvalNoTrace $
          pphaseAScriptPreconditionsRejection # 0 # pconstant True # pconstant True # pconstant (BS.replicate 32 0) # 0 #== pcon PNothing
      , testCase "supplied integrity hash refuses observers with absent network" $ passertEvalNoTrace $
          pphaseAScriptPreconditionsRejection # 1 # pconstant False # pconstant False # hash # 255 #== pcon (PJust $ pconstant "E_INVALID_FIELD_TYPE")
      ]
  , testGroup "phase transitions"
      [ testCase name $ passertEvalNoTrace $ transition (phase pre) (phase post) #== pconstant accepted
      | (name, pre, post, accepted) <-
          [ ("InputSets repeats", InputSets, InputSets, True)
          , ("Signatures repeats", Signatures, Signatures, True)
          , ("PhaseANativeScripts repeats", PhaseANativeScripts, PhaseANativeScripts, True)
          , ("PhaseANativeScripts enters NativeScripts", PhaseANativeScripts, NativeScripts, True)
          , ("NativeScripts returns to PhaseANativeScripts", NativeScripts, PhaseANativeScripts, True)
          , ("InputSets advances", InputSets, Signatures, True)
          , ("Signatures advances", Signatures, PhaseANativeScripts, True)
          , ("PhaseANativeScripts advances", PhaseANativeScripts, PhaseAScriptPreconditions, True)
          , ("NativeScripts advances", NativeScripts, ScriptIntegrity, True)
          , ("InputSets cannot skip signatures", InputSets, PhaseANativeScripts, False)
          , ("Signatures cannot skip Phase A", Signatures, NativeScripts, False)
          , ("PhaseANativeScripts cannot skip preconditions", PhaseANativeScripts, ResolveInputs, False)
          , ("NativeScripts cannot return to signatures", NativeScripts, Signatures, False)
          ]
      ]
  ]

transition :: forall s. Term s PValidationPhase -> Term s PValidationPhase -> Term s PBool
transition prePhase postPhase =
  plet (state prePhase 0 $ phashWorkWitness # prePhase # 0 # pconstant "") $ \pre ->
    pstructuralTransitionIsValid # pre # (pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "") (pdata $ state postPhase 1 hash))

state :: forall s. Term s PValidationPhase -> Term s PInteger -> Term s PByteString -> Term s PValidationMachineStateV1
state phase counter work = pcon $ PValidationMachineStateV1
  (pdata pmachineVersion) (pdata hash) (pdata hash) (pdata hash) (pdata hash)
  (pdata $ pcon PNormal) (pdata hash) (pdata phase) (pdata counter) (pdata work)
  (pdata 0) (pdata 0) (pdata $ pcon PPending) (pdata $ pconstant $ BS.replicate 32 0) (pdata hash)

hash :: forall s. Term s PByteString
hash = pconstant $ BS.replicate 32 1

data Phase = InputSets | Signatures | PhaseANativeScripts | NativeScripts | PhaseAScriptPreconditions | ScriptIntegrity | ResolveInputs

phase :: forall s. Phase -> Term s PValidationPhase
phase = \case
  InputSets -> pcon PInputSets
  Signatures -> pcon PSignatures
  PhaseANativeScripts -> pcon PPhaseANativeScripts
  NativeScripts -> pcon PNativeScripts
  PhaseAScriptPreconditions -> pcon PPhaseAScriptPreconditions
  ScriptIntegrity -> pcon PScriptIntegrity
  ResolveInputs -> pcon PResolveInputs

nativeWitnessBytes :: forall s. Term s PByteString -> Term s PByteString
nativeWitnessBytes bytes = pencodePhaseANativeScriptsScanWitness
  # bytes # bytes # bytes # bytes # pconstant (BS.replicate 32 170)
  # 0 # (-1) # 0 # 0 # 0 # pconstant "" # 0 # pconstant ""
  # 0 # 0 # (-1) # 0 # pnil # bytes
