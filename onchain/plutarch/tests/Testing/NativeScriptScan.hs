{-# LANGUAGE OverloadedStrings #-}

module Testing.NativeScriptScan (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.NativeScriptScan
import Testing.Eval (passertEvalNoTrace)

tests :: TestTree
tests = testGroup "Midgard.NativeScriptScan"
  [ testCase "scans_the_cross_language_tree_iteratively" $ passertEvalNoTrace scansTree
  , testCase "decodes_the_typescript_terminal_control_canonically" $ passertEvalNoTrace decodesTerminal
  , testCase "one_token_successor_is_bounded" $ passertEvalNoTrace oneTokenBounded
  , testCase "reports_authenticated_malformed_and_trailing_syntax" $ passertEvalNoTrace reportsMalformed
  , testCase "fails_closed_for_a_wrong_frame_preimage" $ passertEvalNoTrace wrongFrameFails
  ]

scansTree :: forall s. Term s PBool
scansTree =
  plet (pinitialStructureControlV1 # 0 # (plengthBS # crossLanguageScript)) $ \initial ->
  plet (advanced $ pstructureTokenStepV1 # initial # crossLanguageScript # 0) $ \root ->
  plet (advanced $ pstructureTokenStepV1 # root # crossLanguageScript # cursor root) $ \signature ->
  plet (advanced $ pstructureFrameStepV1 # signature # rootFrame 2) $ \afterSignature ->
  plet (advanced $ pstructureTokenStepV1 # afterSignature # crossLanguageScript # cursor afterSignature) $ \timelock ->
  plet (advanced $ pstructureFrameStepV1 # timelock # rootFrame 1) $ \afterTimelock ->
  plet (advanced $ pfinalizeStructureV1 # afterTimelock) $ \terminal -> pmatch terminal $ \c ->
    pfromData (pstructure'nodeCount c) #== 3
      #&& pstructureTerminalIsExactV1 # terminal
      #&& pencodeStructureControlV1 # terminal #== terminalControl

decodesTerminal :: forall s. Term s PBool
decodesTerminal = plet (pdecodeStructureControlV1 # terminalControl) $ \control ->
  pstructureTerminalIsExactV1 # control #&& pencodeStructureControlV1 # control #== terminalControl

oneTokenBounded :: forall s. Term s PBool
oneTokenBounded =
  plet (pinitialStructureControlV1 # 0 # (plengthBS # crossLanguageScript)) $ \initial ->
  plet (advanced $ pstructureTokenStepV1 # initial # crossLanguageScript # 0) $ \root -> pmatch root $ \c ->
    pfromData (pstructure'stage c) #== pstructureStageToken
      #&& pfromData (pstructure'cursor c) #== 4
      #&& pfromData (pstructure'stackDepth c) #== 1
      #&& pfromData (pstructure'nodeCount c) #== 1
      #&& plengthBS # pfromData (pstructure'stackRoot c) #== 32

reportsMalformed :: forall s. Term s PBool
reportsMalformed =
  plet (pinitialStructureControlV1 # 0 # (plengthBS # malformedScript)) $ \malformedInitial ->
  plet (pinitialStructureControlV1 # 0 # (plengthBS # validWithTrailing)) $ \trailingInitial ->
  plet (advanced $ pstructureTokenStepV1 # trailingInitial # validWithTrailing # 0) $ \trailingFinalize ->
  pmatch trailingFinalize $ \c ->
    pstructureTokenStepV1 # malformedInitial # malformedScript # 0
      #== pcon (PJust $ pcon PNativeScriptStructureInvalid)
      #&& pfromData (pstructure'stage c) #== pstructureStageFinalize
      #&& pfinalizeStructureV1 # trailingFinalize #== pcon (PJust $ pcon PNativeScriptStructureInvalid)

wrongFrameFails :: forall s. Term s PBool
wrongFrameFails =
  plet (pinitialStructureControlV1 # 0 # (plengthBS # crossLanguageScript)) $ \initial ->
  plet (advanced $ pstructureTokenStepV1 # initial # crossLanguageScript # 0) $ \root ->
  plet (advanced $ pstructureTokenStepV1 # root # crossLanguageScript # cursor root) $ \signature ->
    pstructureFrameStepV1 # signature # rootFrame 1 #== pcon PNothing

rootFrame :: forall s. Term s PInteger -> Term s PNativeScriptFrameV1
rootFrame remaining = pcon $ PNativeScriptFrameV1
  (pdata $ pconstant "") (pdata patLeastNode) (pdata 2) (pdata remaining) (pdata 0) (pdata 1)

cursor :: forall s. Term s PNativeScriptStructureControlV1 -> Term s PInteger
cursor control = pmatch control $ \c -> pfromData $ pstructure'cursor c

advanced :: forall s. Term s (PMaybe PNativeScriptStructureStepResultV1) -> Term s PNativeScriptStructureControlV1
advanced result = pmatch result $ \case
  PNothing -> perror
  PJust stepResult -> pmatch stepResult $ \case
    PNativeScriptStructureAdvanced control -> pfromData control
    _ -> perror

crossLanguageScript, terminalControl, malformedScript, validWithTrailing :: forall s. Term s PByteString
crossLanguageScript = bytes "830301828200581c444444444444444444444444444444444444444444444444444444448205182a"
terminalControl = bytes "8801030018281828400003"
malformedScript = bytes "82004144"
validWithTrailing = bytes "8200581c4444444444444444444444444444444444444444444444444444444400"

bytes :: forall s. BS.ByteString -> Term s PByteString
bytes = pconstant . Base16.decodeLenient
