{-# LANGUAGE OverloadedStrings #-}

module Testing.CanonicalDecodeItemStaging (tests) where

import Data.ByteString qualified as BS
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.CanonicalDecodeItemStaging
import Midgard.LedgerState (PItemProofV1 (..))
import Midgard.ValidationMachine hiding (pverifyCanonicalDecodeItem)
import Midgard.ValidationResolution
import Midgard.ValidationTrace
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests = testGroup "Midgard.CanonicalDecodeItemStaging"
  [ testCase "canonical_decode_item_stages_preserve_the_authenticated_base" $
      passertEvalNoTrace canonicalDecodeStagesPreserveBase
  , testCase "authentication_rejects_a_noncanonical_phase" $
      pfails $ pauthenticateCanonicalDecodeItem # base PCompactBinding # transition
  ]

canonicalDecodeStagesPreserveBase :: forall s. Term s PBool
canonicalDecodeStagesPreserveBase =
  plet (pauthenticateCanonicalDecodeItem # base PCanonicalDecode # transition) $ \authenticated ->
  plet (pprepareCanonicalDecodeItem # authenticated # source) $ \prepared ->
  plet (pobserveCanonicalDecodeItem # prepared # observation) $ \observed ->
  plet (pverifyCanonicalDecodeItem # observed # proof) $ \verified ->
    pauthenticatedCanonicalDecodeItemIsWellFormed # authenticated
      #&& ppreparedCanonicalDecodeItemIsWellFormed # prepared
      #&& pobservedCanonicalDecodeItemIsWellFormed # observed
      #&& pverifiedCanonicalDecodeItemIsWellFormed # verified

base :: forall s. PValidationPhase s -> Term s PPreparedValidationResolutionStateV1
base phase = pcon $ PPreparedValidationResolutionStateV1
  (pdata ppreparedResolutionVersion)
  (pdata $ pcon $ PValidationResolutionStateV1
    (pdata presolutionVersion) (pdata $ machineState phase) (pdata $ hash 8) (pdata $ hash 9))
  (pdata $ hash 10)

transition :: forall s. Term s PValidationOneStepWitnessV1
transition = pcon $ PValidationOneStepWitnessV1 (pdata $ pconstant "w") (pdata $ machineState PCompactBinding)

source :: forall s. Term s PCanonicalDecodeItemSourceV1
source = pcon $ PCanonicalDecodeItemSourceV1 (pdata $ hash 11) (pdata 1)

proof :: forall s. Term s PCanonicalDecodeItemProofV1
proof = pcon $ PCanonicalDecodeItemProofV1 (pdata 1) (pdata $ pconstant True) (pdata 2)

observation :: forall s. Term s PCanonicalDecodeItemObservationV1
observation = pcon $ PCanonicalDecodeItemObservationV1
  (pdata $ pcon $ PItemProofV1
    (pdata 1) (pdata 0) (pdata 1) (pdata 0) (pdata 1)
    (pdata $ hash 12) (pdata pnil) (pdata pnil))
  (pdata 1)
  (pdata $ hash 12)

machineState :: forall s. PValidationPhase s -> Term s PValidationMachineStateV1
machineState phase = pcon $ PValidationMachineStateV1
  (pdata pmachineVersion) (pdata $ hash 1) (pdata $ hash 2) (pdata $ hash 3)
  (pdata $ hash 4) (pdata $ pcon PNormal) (pdata $ hash 5) (pdata $ pcon phase)
  (pdata 0) (pdata $ hash 6) (pdata 0) (pdata 0) (pdata $ pcon PPending)
  (pdata $ hash 0) (pdata $ hash 7)

hash :: forall s. Word -> Term s PByteString
hash byte = pconstant $ BS.replicate 32 (fromIntegral byte)
