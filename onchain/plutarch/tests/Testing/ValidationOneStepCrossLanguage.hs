{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Testing.ValidationOneStepCrossLanguage (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Aiken.Cbor (pdeserialise)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.ValidationDispute (PValidationDisputeV1)
import Midgard.ValidationMachine (
  PScriptDiscoveryControlV1 (..),
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  pencodeScriptDiscoveryControl,
  pverifyCanonicalDecodeEmptySemanticsV1,
 )
import Midgard.ValidationMerkle qualified as Merkle
import Midgard.ValidationResolution (
  PValidationBoundaryEvidenceV1 (..),
  phashOneStepEvidence,
  poneStepBoundaryIsAuthenticated,
 )
import Testing.Eval (passertEvalNoTrace)

tests :: TestTree
tests =
  testGroup
    "Validation One-Step Cross-Language Tests"
    [ testCase "typescript_generated_one_step_boundary_is_authenticated" $
        passertEvalNoTrace boundaryIsAuthenticated
    , testCase "typescript_generated_canonical_decode_step_is_exact" $
        passertEvalNoTrace canonicalDecodeStepIsExact
    , testCase "typescript_generated_script_discovery_control_wire_is_exact" $
        passertEvalNoTrace scriptDiscoveryControlWireIsExact
    ]

boundaryIsAuthenticated :: forall s. Term s PBool
boundaryIsAuthenticated =
  plet (pconstant disputeCbor) $ \disputeBytes ->
  plet (pconstant boundaryEvidenceCbor) $ \boundaryBytes ->
  withDecoded disputeBytes $ \disputeData ->
  withDecoded boundaryBytes $ \boundaryData ->
  plet (asTyped @PValidationDisputeV1 disputeData) $ \dispute ->
  plet (asTyped @PValidationBoundaryEvidenceV1 boundaryData) $ \boundary ->
  pmatch boundary $ \evidence ->
    pall'
      [ plengthBS # boundaryBytes #== 851
      , plengthBS # boundaryBytes #< 16_384
      , poneStepBoundaryIsAuthenticated
          # dispute
          # pfromData (pboundary'preState evidence)
          # pfromData (pboundary'operatorPost evidence)
          # pfromData (pboundary'challengerPost evidence)
      ]

canonicalDecodeStepIsExact :: forall s. Term s PBool
canonicalDecodeStepIsExact =
  plet (pconstant boundaryEvidenceCbor) $ \boundaryBytes ->
  plet (pconstant transitionCbor) $ \transitionBytes ->
  plet (pconstant auxiliaryCbor) $ \auxiliaryBytes ->
  withDecoded boundaryBytes $ \boundaryData ->
  withDecoded transitionBytes $ \transitionData ->
  withDecoded auxiliaryBytes $ \auxiliaryData ->
  plet (asTyped @PValidationBoundaryEvidenceV1 boundaryData) $ \boundary ->
  plet (asTyped @PValidationOneStepWitnessV1 transitionData) $ \transition ->
  plet (asTyped @PValidationAuxiliaryWitnessV1 auxiliaryData) $ \auxiliary ->
  pmatch boundary $ \evidence ->
    pall'
      [ plengthBS # transitionBytes #== 794
      , plengthBS # auxiliaryBytes #== 3
      , plengthBS # transitionBytes #< 16_384
      , plengthBS # auxiliaryBytes #< 16_384
      , -- #585: TypeScript generated this compact source with the retired
        -- counted commitment; Aiken/Plutarch require the flat empty-field hash.
        pnot
          # ( pverifyCanonicalDecodeEmptySemanticsV1
                # pfromData (pboundary'preState evidence)
                # transition
            )
      , auxiliary #== pcon PNoAuxiliaryWitness
      , phashOneStepEvidence # transitionData # auxiliaryData
          #== pconstant evidenceHash
      ]

scriptDiscoveryControlWireIsExact :: forall s. Term s PBool
scriptDiscoveryControlWireIsExact =
  pencodeScriptDiscoveryControl # scriptDiscoveryControl
    #== pconstant scriptDiscoveryControlCbor

scriptDiscoveryControl :: forall s. Term s PScriptDiscoveryControlV1
scriptDiscoveryControl =
  pcon $
    PScriptDiscoveryControlV1
      (pdata 1)
      (pdata 2)
      (pdata 3)
      (pdata 0)
      (pdata 4)
      (pdata $ pconstant "\xaa")
      (pdata $ pconstant "\xbb")
      (pdata 5)
      (pdata 3)
      (pdata $ pconstant "\xcc")
      (pdata 6)
      (pdata 7)
      (pdata $ pconstant "\xdd")
      (pdata 8)
      ( pdata $
          pcons
            # pdata (pcon $ Merkle.PFrontierPeak (pdata 9) (pdata $ pconstant "\xee"))
            # pnil
      )

withDecoded :: forall s. Term s PByteString -> (Term s PData -> Term s PBool) -> Term s PBool
withDecoded source continuation =
  pmatch (pdeserialise # source) $ \case
    PNothing -> perror
    PJust value -> continuation value

asTyped :: forall a s. PIsData a => Term s PData -> Term s a
asTyped value = pfromData (punsafeCoerce @(PAsData a) value)

pall' :: forall s. [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)

disputeCbor, boundaryEvidenceCbor, transitionCbor, auxiliaryCbor, evidenceHash, scriptDiscoveryControlCbor :: BS.ByteString
disputeCbor = hex "d8799f01d8799f010158208cefaa95122fa313498c10f088249e3dbf903dfea0d9e96e548daad2e2eece1d1849582083ffe056c1ca04660a7eda92223d869f760688b60a6c04c54a97014b10d0319c5820f96cf4f288dbbde165e30ebdb246c9709185bfcc2687cf2566c25d073f327e42d87a8058200000000000000000000000000000000000000000000000000000000000000000ffd8799f010158206165886a92dce5a7c593e60e2d94463178330836bbb681747c8b11ec738b16011849582083ffe056c1ca04660a7eda92223d869f760688b60a6c04c54a97014b10d0319c58203d23c3ccef48e2a816227612f8615499aa2db27df96547ede03c01fc3db7b030d87a8058200000000000000000000000000000000000000000000000000000000000000000ff01025820f146d58f70afd5442a7db82a1c2b8e85be268abc8647721b3ccfa7dab4cf819558207c5bf7f1ee07d4091b44e34f19c9ef40410d706e32a205c1c1215e83d7fc75b558206144a6407904b04b8fda0cf814aee02087386928c1b9a3c5d5115add22014843011b000001a3185c5000d87b80ff"
boundaryEvidenceCbor = hex "d8799fd8799f015820b121ec457f8ca78cbfca9d506b34ad3bee23f3cc92de6cef531e5279441fdbde5820dacbf3f751d1d71328bb9a53d70e665e484c99499a98a312d0e5b8bb33741e655820702c0d069cbd6e7217169f4310377d52338fd99c27a1153bf497f378055ab7515820efbf53e6fdf61931d07d45ffdfe6b848d44f992f3ee0296b4f1415cc1c890c9dd87a80582088e82613515496a1d6feac6d1c0c365611a17a510965f20e861e04600008f1e3d8798001582035bb7e9a2494cddac797668468fa3b84e5fdaacd497742b13783343ed75705d10000d87980582000000000000000000000000000000000000000000000000000000000000000005820f0b18652674c5fd8b41956f645eab13ba3ac04cb9569c751e449e7800907c631ffd8799f0258207c5bf7f1ee07d4091b44e34f19c9ef40410d706e32a205c1c1215e83d7fc75b59f58203a47db87605beca1a8e4849bff9c6b4a954c8d47393c8e154e76f9c6c8b1574d582021a7dca325bf729a6ddc224713108b3cff8ad6ddd116375f0c044829d4879f695820d4a752a0336603bf524fecaa57f4e42227a6c4ed8164ddf62cc99181bd6a5e7b5820376cd2f4f675f090a0f84006e7095b39cddf359484c42dd3850cfc23f0aee1195820cebe6612c652d04d1313470f29535a6133d68c33bdcfb1f186364ca4a21e67015820c5081bbe607f3abeae2789f8632f74c9db80af86f509af339857d17da9e191aa58201c13b4fab108f5d2836445ee77e3573a7a40e3e5d3fb593f601eb80e46b54646ffffd8799f0258206144a6407904b04b8fda0cf814aee02087386928c1b9a3c5d5115add220148439f58203a47db87605beca1a8e4849bff9c6b4a954c8d47393c8e154e76f9c6c8b1574d582021a7dca325bf729a6ddc224713108b3cff8ad6ddd116375f0c044829d4879f695820d4a752a0336603bf524fecaa57f4e42227a6c4ed8164ddf62cc99181bd6a5e7b5820376cd2f4f675f090a0f84006e7095b39cddf359484c42dd3850cfc23f0aee1195820cebe6612c652d04d1313470f29535a6133d68c33bdcfb1f186364ca4a21e67015820c5081bbe607f3abeae2789f8632f74c9db80af86f509af339857d17da9e191aa58208fb5dce649543bce5669618b764760fa12255671b2946d92ce9a76d70bcfff1effffff"
transitionCbor = hex "d8799f5f58408959013b84018c58205e0d1a550b9170eb36be56c637a28ff42b7832b0f749310490995b230e329d925820971b52c16ad426099e34913c7b4adc0059f82f4b10584025d866f7abcf0df2f00b9f5820a2fa294898ac17425d8c93194f9317f1eac2957d0d018330fcb4b09365ee8cad0020205820e5ccfcd8e326be04d73634d1ef2c5840b659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf958409b528418df3b62a7182102d089fefa4323fbd18ac47d582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d58404f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820855d9637a136d7e890e2a3b4d5c543c52136ac669624b091b065db951e22890200585840678358207b8d24b2ada932d9912be6b8ef807779ddccf33eb0178ae28bfc9350caf2dd2f5820ae7b18490f716b798eb0871325c96023e7e8ba472b7aa0cedcd758405cd05f66f76c5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef34d8918270118280101010118680158269f01546d69646761582372642d636f6e73656e7375732d76311b000001977420dc000000001864ff0100002000ffd8799f015820b121ec457f8ca78cbfca9d506b34ad3bee23f3cc92de6cef531e5279441fdbde5820dacbf3f751d1d71328bb9a53d70e665e484c99499a98a312d0e5b8bb33741e655820702c0d069cbd6e7217169f4310377d52338fd99c27a1153bf497f378055ab7515820efbf53e6fdf61931d07d45ffdfe6b848d44f992f3ee0296b4f1415cc1c890c9dd87a80582088e82613515496a1d6feac6d1c0c365611a17a510965f20e861e04600008f1e3d8798002582050ee56e5508720ac9e0effc9ab394e36d780f6385f437397f1b6532e45d5bf420000d87980582000000000000000000000000000000000000000000000000000000000000000005820f0b18652674c5fd8b41956f645eab13ba3ac04cb9569c751e449e7800907c631ffff"
auxiliaryCbor = hex "d87980"
evidenceHash = hex "689686975245a3947ccc374cf3a93c14aa0c8ae37a59d8e2dfb3290740d110a1"
scriptDiscoveryControlCbor = hex "8f010203000441aa41bb050341cc060741dd0881820941ee"

hex :: BS.ByteString -> BS.ByteString
hex = Base16.decodeLenient
