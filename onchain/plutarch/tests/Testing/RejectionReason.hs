{-# LANGUAGE OverloadedStrings #-}

module Testing.RejectionReason (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.RejectionReason
import Midgard.ValidationTrace (phashRejectionCode)
import Testing.Eval (passertEval, pfails)
import PlutusCore.Data qualified as PD

tests :: TestTree
tests =
  testGroup
    "Rejection Reason V1 Aiken Parity"
    [ testCase "rejection code constants and descriptor hashes are frozen" $
        passertEval frozenCodesAndHashes
    , testGroup "rejection_code_of is exact for every arm" mappingTests
    , testGroup "integer coordinates reject malformed Data" malformedCoordinateTests
    , testCase "negative and large integer coordinates retain the same rejection code" $ passertEval $ pand'List
        [ prejectionCodeOf # pconstant @PData (PD.Constr tag $ replicate arity $ PD.I n)
            #== prejectionCodeOf # pconstant @PData (PD.Constr tag $ replicate arity $ PD.I 0)
        | (tag, arity) <- zip [0 ..] reasonArities, n <- [-1, 2 ^ (128 :: Integer)]
        ]
    ]

mappingTest ::
  String ->
  (forall s. Term s PRejectionReasonV1) ->
  (forall s. Term s PByteString) ->
  TestTree
mappingTest name reason code = testCase name $ passertEval $ maps reason code

maps :: forall s. Term s PRejectionReasonV1 -> Term s PByteString -> Term s PBool
maps reason code =
  prejectionCodeOf # pforgetData (pdata reason) #== code

mappingTests :: [TestTree]
mappingTests =
    [ mappingTest "arm 01" (pcon $ PFieldPreimageLengthMismatch $ pdata 3) prejectFieldPreimageSize
    , mappingTest "arm 02" (pcon $ PFieldItemWidthIllegal (pdata 5) (pdata 2)) prejectInvalidFieldType
    , mappingTest "arm 03" (pcon PEmptyInputs) prejectEmptyInputs
    , mappingTest "arm 04" (pcon $ PDuplicateInput (pdata 0) (pdata 1) (pdata 1) (pdata 0)) prejectDuplicateInput
    , mappingTest "arm 05" (pcon PValidityIntervalMalformed) prejectInvalidValidityIntervalFormat
    , mappingTest "arm 06" (pcon PNetworkIdMismatch) prejectNetworkIdMismatch
    , mappingTest "arm 07" (pcon PFeeBelowMinimum) prejectMinFee
    , mappingTest "arm 08" (pcon $ PAddressWitnessSignatureInvalid $ pdata 4) prejectInvalidSignature
    , mappingTest "arm 09" (pcon $ PRequiredSignerUnsigned $ pdata 1) prejectMissingRequiredWitness
    , mappingTest "arm 10" (pcon $ PWitnessScriptHeaderMalformed $ pdata 0) prejectInvalidFieldType
    , mappingTest "arm 11" (pcon $ PWitnessNativeScriptMalformed $ pdata 1) prejectInvalidFieldType
    , mappingTest "arm 12" (pcon $ PWitnessNativeScriptNodeLimit $ pdata 2) prejectNativeScriptNodeCount
    , mappingTest "arm 13" (pcon $ PWitnessNativeScriptDepthLimit $ pdata 3) prejectNativeScriptDepth
    , mappingTest "arm 14" (pcon $ PWitnessNativeScriptFalse $ pdata 4) prejectNativeScriptInvalid
    , mappingTest "arm 15" (pcon PScriptIntegrityHashMissing) prejectInvalidFieldType
    , mappingTest "arm 16" (pcon PObserversForbiddenOnUntaggedNetwork) prejectInvalidFieldType
    , mappingTest "arm 17" (pcon $ PObserverOrderInvalid $ pdata 2) prejectInvalidFieldType
    , mappingTest "arm 18" (pcon PValidityIntervalExcludesBlockSlot) prejectValidityIntervalMismatch
    , mappingTest "arm 19" (pcon $ PInputNotFound (pdata 0) (pdata 3)) prejectInputNotFound
    , mappingTest "arm 20" (pcon $ PInputSpentOutputNonCanonical (pdata 1) (pdata 0)) prejectInvalidOutput
    , mappingTest "arm 21" (pcon $ PResolvedReferenceScriptMalformed (pdata 0) (pdata 1)) prejectInvalidFieldType
    , mappingTest "arm 22" (pcon $ PResolvedReferenceScriptNodeLimit (pdata 1) (pdata 2)) prejectNativeScriptNodeCount
    , mappingTest "arm 23" (pcon $ PResolvedReferenceScriptDepthLimit (pdata 0) (pdata 0)) prejectNativeScriptDepth
    , mappingTest "arm 24" (pcon $ PSpendInputSignerMissing $ pdata 2) prejectMissingRequiredWitness
    , mappingTest "arm 25" (pcon $ PRedeemerMalformed $ pdata 0) prejectInvalidFieldType
    , mappingTest "arm 26" (pcon $ POutputNonCanonical $ pdata 1) prejectInvalidOutput
    , mappingTest "arm 27" (pcon $ POutputReferenceScriptMalformed $ pdata 2) prejectInvalidFieldType
    , mappingTest "arm 28" (pcon $ POutputReferenceScriptNodeLimit $ pdata 3) prejectNativeScriptNodeCount
    , mappingTest "arm 29" (pcon $ POutputReferenceScriptDepthLimit $ pdata 4) prejectNativeScriptDepth
    , mappingTest "arm 30" (pcon $ PProtectedOutputSignerMissing $ pdata 5) prejectMissingRequiredWitness
    , mappingTest "arm 31" (pcon $ PMintDeclaredAssetLimit $ pdata 0) prejectAssetCount
    , mappingTest "arm 32" (pcon $ PScriptSourceMissing (pdata 1) (pdata 0)) prejectMissingRequiredWitness
    , mappingTest "arm 33" (pcon $ PRedeemerMissing (pdata 3) (pdata 1)) prejectMissingRequiredWitness
    , mappingTest "arm 34" (pcon $ PUnusedScriptWitness $ pdata 5) prejectInvalidFieldType
    , mappingTest "arm 35" (pcon $ PUnusedRedeemer $ pdata 1) prejectInvalidFieldType
    , mappingTest "arm 36" (pcon $ PExecutionNativeScriptMalformed $ pdata 0) prejectInvalidFieldType
    , mappingTest "arm 37" (pcon $ PExecutionNativeScriptNodeLimit $ pdata 1) prejectNativeScriptNodeCount
    , mappingTest "arm 38" (pcon $ PExecutionNativeScriptDepthLimit $ pdata 2) prejectNativeScriptDepth
    , mappingTest "arm 39" (pcon $ PExecutionNativeScriptFalse $ pdata 3) prejectNativeScriptInvalid
    , mappingTest "arm 40" (pcon PScriptIntegrityHashMismatch) prejectInvalidFieldType
    , mappingTest "arm 41" (pcon $ PReceivePurposePlutusV3Forbidden $ pdata 4) prejectPlutusScriptInvalid
    , mappingTest "arm 42" (pcon $ PPlutusExecutionFailed $ pdata 5) prejectPlutusScriptInvalid
    , mappingTest "arm 43" (pcon $ PInputAssetAccumulationLimit (pdata 0) (pdata 7)) prejectAssetCount
    , mappingTest "arm 44" (pcon $ POutputAssetAccumulationLimit (pdata 1) (pdata 8)) prejectAssetCount
    , mappingTest "arm 45" (pcon $ PMintAssetAccumulationLimit $ pdata 2) prejectAssetCount
    , mappingTest "arm 46" (pcon $ POutputBelowMinAda $ pdata 0) prejectMinAda
    , mappingTest "arm 47" (pcon PValueNotPreserved) prejectValueNotPreserved
    ]

frozenCodesAndHashes :: forall s. Term s PBool
frozenCodesAndHashes =
  pand'List
    [ frozen prejectFieldPreimageSize "f6ecc827e4b524dd12e9ac63922f7383e015ded70a5e3fa6acae766fa4fe9b20"
    , frozen prejectAssetCount "d4994d00dbdb66963e0a7ede81198fa5426e40075df9b5b1c1c44c56e79f159a"
    , frozen prejectInvalidFieldType "75fa486cb63f3fafa27429f66f4abd86359f96fd8cc3902fd3439d26edb5a1d3"
    , frozen prejectNativeScriptDepth "ba802de85a3d98baab831ab3155478fd90711b696c3ce1479729e2271dceebdc"
    , frozen prejectNativeScriptNodeCount "9320e8acd64f25974da633a482366f8e44d727807e270102179db897d551f377"
    , frozen prejectMinAda "1e4cdaa8212d75203665a9900b617b98a563e84639a67a5d314d4eb33829c796"
    , frozen prejectEmptyInputs "fb4133ff04336d3ef299bf76f6e6b4267be9a7931285e4fbc9f848e66c3624d8"
    , frozen prejectDuplicateInput "2ab509665609f1128b035b5e1468031ccb68de72c06813e01b81c65c29225bf6"
    , frozen prejectNetworkIdMismatch "8504914f12c9176501e7227c82b24b59c1b7195c5214b361624c0d99a547da42"
    , frozen prejectMinFee "46055c0fdb633167ed024b753cfe257f68a4845f3843ffca361e865cbdfc2809"
    , frozen prejectInvalidValidityIntervalFormat "dd5343586670b495f5627816ac45da3b4ecbee9c430ec1ba123d17a1c0ae3513"
    , frozen prejectMissingRequiredWitness "fa3d8e6273d90770a92ff701d086e80a071676e27f73dc39aba35890e4dd813e"
    , frozen prejectInvalidSignature "fd5ed3d40dcfcd4b987a07c17e5ba0812d898baf6b0aab8d7f9e832532a6b63a"
    , frozen prejectNativeScriptInvalid "2774df21b393d196d960a12b5ae7efb94bf49a1e5614745f676b8658b115a0fb"
    , frozen prejectPlutusScriptInvalid "be811cb18fca768be5bc6cc1db0961972ac0b11275caae25be36feee95bdcdc8"
    , frozen prejectValidityIntervalMismatch "d6c7845e4be6c2bfb721e10ce8f6fdf1f717c62c6fa0e69396858280e6a03c2a"
    , frozen prejectInputNotFound "7010e02d40fd5e53e846ed857627684cd0eb66ea6bc07979257063505807a187"
    , frozen prejectInvalidOutput "3ba0dc74796540d3744f8aae143290b166c051584ba241bfe9a71c29c5bd049d"
    , frozen prejectValueNotPreserved "f1084e1e7329699cf7acc4a57b07331de262749506b45d023fdcc69a7c975d71"
    ]
  where
    frozen code expectedHex =
      phashRejectionCode # code #== pconstant (hex expectedHex)

hex :: String -> BS.ByteString
hex = Base16.decodeLenient . BS8.pack

-- Every coordinate of every non-nullary source constructor is an Int. Check
-- each position independently so a decoder cannot validate only the first one.
malformedCoordinateTests :: [TestTree]
malformedCoordinateTests =
  [ testCase ("tag " <> show tag <> "/field " <> show position <> "/" <> label) $
      pfails $ prejectionCodeOf # pconstant @PData
        (PD.Constr tag [if index == position then malformed else PD.I (-1) | index <- [0 .. arity - 1]])
  | (tag, arity) <- zip [0 ..] reasonArities
  , position <- [0 .. arity - 1]
  , (label, malformed) <- [("bytes", PD.B "not-an-integer"), ("constructor", PD.Constr 0 [])]
  ]

reasonArities :: [Int]
reasonArities = [1,2,0,4,0,0,0,1,1,1,1,1,1,1,0,0,1,0,2,2,2,2,2,1,1,1,1,1,1,1,1,2,2,1,1,1,1,1,1,0,1,1,2,2,1,1,0]
