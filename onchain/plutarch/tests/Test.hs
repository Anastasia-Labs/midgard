{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Data.ByteString qualified as BS
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Testing.Eval (passertEval)

-- import Midgard.Utils (pand'List)

import Data.Word (Word8)
import Plutarch.Core.Utils
import Plutarch.MerkleTree.Helpers
import Plutarch.MerkleTree.Merkling
import Plutarch.Monadic qualified as P
import Testing.ActiveOperatorsValidator qualified as ActiveOperators
import Testing.CanonicalCborScan qualified as CanonicalCborScan
import Testing.CanonicalPlutusData qualified as CanonicalPlutusData
import Testing.CanonicalDecodeItemStaging qualified as CanonicalDecodeItemStaging
import Testing.CanonicalVersionTuple qualified as CanonicalVersionTuple
import Testing.CommonValue qualified as CommonValue
import Testing.CommonUtils qualified as CommonUtils
import Testing.ComputationThreadValidator qualified as ComputationThreadValidator
import Testing.CountedMembership qualified as CountedMembership
import Testing.Crypto qualified as Crypto
import Testing.DaAttestationHandlers qualified as DaAttestationHandlers
import Testing.WitnessValidator qualified as WitnessValidator
import Testing.TxOrderFields qualified as TxOrderFields
import Testing.TxOrderV1Abi qualified as TxOrderV1Abi
import Testing.TxFieldReceipt qualified as TxFieldReceipt
import Testing.ValidationMerkle qualified as ValidationMerkle
import Testing.ValidationResolution qualified as ValidationResolution
import Testing.ValidationMachine qualified as ValidationMachine
import Testing.ValidationOneStepCrossLanguage qualified as ValidationOneStepCrossLanguage
import Testing.ValidationInstructionEvidenceBounds qualified as ValidationInstructionEvidenceBounds
import Testing.BoundedBlob qualified as BoundedBlob
import Testing.BoundedCollection qualified as BoundedCollection
import Testing.BoundedItem qualified as BoundedItem
import Testing.LedgerOutputCommitment qualified as LedgerOutputCommitment
import Testing.LedgerOutput qualified as LedgerOutput
import Testing.LedgerOutputValue qualified as LedgerOutputValue
import Testing.LedgerValueParity qualified as LedgerValueParity
import Testing.LedgerOutputScan qualified as LedgerOutputScan
import Testing.LedgerOutputProof qualified as LedgerOutputProof
import Testing.RedeemerItemProof qualified as RedeemerItemProof
import Testing.NativeScript qualified as NativeScript
import Testing.NativeScriptScan qualified as NativeScriptScan
import Testing.NativeTxScriptPushdown qualified as NativeTxScriptPushdown
import Testing.NativeTxIntraItem qualified as NativeTxIntraItem
import Testing.NativeTxFaultStatement qualified as NativeTxFaultStatement
import Testing.ScriptSourcesRedeemerNormalization qualified as ScriptSourcesRedeemerNormalization
import Testing.ScriptContext qualified as ScriptContext
import Testing.ScriptProof qualified as ScriptProof
import Testing.NativeTxCarriage qualified as NativeTxCarriage
import Testing.NativeTxCarriageWireGolden qualified as NativeTxCarriageWireGolden
import Testing.NativeTxC20Field6Maximum qualified as NativeTxC20Field6Maximum
import Testing.NativeTxC20Identity qualified as NativeTxC20Identity
import Testing.CekCost qualified as CekCost
import Testing.Blake2b256Trace qualified as Blake2b256Trace
import Testing.Blake2b224Trace qualified as Blake2b224Trace
import Testing.CekBlobFrontier qualified as CekBlobFrontier
import Testing.CekBuiltin qualified as CekBuiltin
import Testing.CekMachine qualified as CekMachine
import Testing.CekSourceBlob qualified as CekSourceBlob
import Testing.CekConstant qualified as CekConstant
import Testing.CekData qualified as CekData
import Testing.CekDataBytes qualified as CekDataBytes
import Testing.CekDataFrame qualified as CekDataFrame
import Testing.CekDataInteger qualified as CekDataInteger
import Testing.CekDataScan qualified as CekDataScan
import Testing.CekDataTraverse qualified as CekDataTraverse
import Testing.CekProof qualified as CekProof
import Testing.AikenCbor qualified as AikenCbor
import Testing.FieldPreimageCertificateValidator qualified as FieldPreimageCertificateValidator
import Testing.NativeTxCodec qualified as NativeTxCodec
import Testing.NativeTxCompact qualified as NativeTxCompact
import Testing.NativeTxFieldAccess qualified as NativeTxFieldAccess
import Testing.NativeTxMaximumProfiles qualified as NativeTxMaximumProfiles
import Testing.NativeTxMachineWalk qualified as NativeTxMachineWalk
import Testing.NativeTxComponents qualified as NativeTxComponents
import Testing.NativeTxCoreGolden qualified as NativeTxCoreGolden
import Testing.NativeTxPreimages qualified as NativeTxPreimages
import Testing.NativeTxTransaction qualified as NativeTxTransaction
import Testing.DaAttestationOperations qualified as DaAttestationOperations
import Testing.DaAttestationReaders qualified as DaAttestationReaders
import Testing.DaAttestationSignatures qualified as DaAttestationSignatures
import Testing.DaParamsGovernor qualified as DaParamsGovernor
import Testing.DepositValidator qualified as DepositValidator
import Testing.DesignPatterns qualified as DesignPatterns
import Testing.EventInclusion qualified as EventInclusion
import Testing.EventV1Abi qualified as EventV1Abi
import Testing.WithdrawalValidator qualified as WithdrawalValidator
import Testing.FraudProofCatalogueValidator qualified as FraudProofCatalogue
import Testing.FraudProofValidator qualified as FraudProof
import Testing.FieldOpening qualified as FieldOpening
import Testing.FragmentEnvelopeV1 qualified as FragmentEnvelopeV1
import Testing.FraudProofsCommon qualified as FraudProofsCommon
import Testing.FraudProofsDoubleSpend qualified as FraudProofsDoubleSpend
import Testing.FraudProofsInputNoIdx qualified as FraudProofsInputNoIdx
import Testing.FraudProofsNoInput qualified as FraudProofsNoInput
import Testing.FraudProofsNoReferenceInput qualified as FraudProofsNoReferenceInput
import Testing.FraudProofsMissingNativeScriptTx qualified as FraudProofsMissingNativeScriptTx
import Testing.FraudProofsQ1xSpendInputCardinality qualified as FraudProofsQ1xSpendInputCardinality
import Testing.FraudProofsDaHashPreimage qualified as FraudProofsDaHashPreimage
import Testing.TraceProofs qualified as TraceProofs
import Testing.TransactionRootV1Golden qualified as TransactionRootV1Golden
import Testing.FraudProofsTransitionTrace qualified as FraudProofsTransitionTrace
import Testing.TransitionTraceAiken qualified as TransitionTraceAiken
import Testing.TransitionTraceProof qualified as TransitionTraceProof
import Testing.ValidationTrace qualified as ValidationTrace
import Testing.ValidationTraceCanonicalDecode qualified as ValidationTraceCanonicalDecode
import Testing.ValidationTraceCompactBinding qualified as ValidationTraceCompactBinding
import Testing.ValidationTraceInputSets qualified as ValidationTraceInputSets
import Testing.ValidationTracePhaseANativeScripts qualified as ValidationTracePhaseANativeScripts
import Testing.ValidationTracePhaseAScriptPreconditions qualified as ValidationTracePhaseAScriptPreconditions
import Testing.ValidationTraceResolveInputs qualified as ValidationTraceResolveInputs
import Testing.ValidationTraceScriptSourcesStageZero qualified as ValidationTraceScriptSourcesStageZero
import Testing.ValidationTraceSignatures qualified as ValidationTraceSignatures
import Testing.ValidationTraceStaticLedgerRules qualified as ValidationTraceStaticLedgerRules
import Testing.ScriptLanguageViews qualified as ScriptLanguageViews
import Testing.ValidationClaim qualified as ValidationClaim
import Testing.ValidationDispute qualified as ValidationDispute
import Testing.FraudProofsInvalidRange qualified as FraudProofsInvalidRange
import Testing.FraudProofsMinFee qualified as FraudProofsMinFee
import Testing.FraudProofsSignature qualified as FraudProofsSignature
import Testing.FraudProofsWithdrawnReferenceInput qualified as FraudProofsWithdrawnReferenceInput
import Testing.FraudProofsZeroInput qualified as FraudProofsZeroInput
import Testing.HeaderValidity qualified as HeaderValidity
import Testing.HubOracleValidator qualified as HubOracleValidator
import Testing.LinkedList qualified as LinkedListTests
import Testing.MembershipValidator qualified as MembershipValidator
import Testing.OperatorDirectory qualified as OperatorDirectory
import Testing.PayoutValidator qualified as PayoutValidator
import Testing.RegisteredOperatorsValidator qualified as RegisteredOperators
import Testing.ReserveValidator qualified as ReserveValidator
import Testing.SchedulerValidator qualified as SchedulerValidator
import Testing.SettlementValidator qualified as SettlementValidator
import Testing.StateQueueLib qualified as StateQueueLib
import Testing.StateQueueValidator qualified as StateQueueValidator
import Testing.TxOrderValidator qualified as TxOrderValidator
import Testing.RetiredOperatorsValidator qualified as RetiredOperators
import Testing.MerklePatriciaForestry qualified as MPF
import Testing.MpfChunkedProof qualified as MpfChunkedProof
import Testing.MpfChunkedVerifyValidator qualified as MpfChunkedVerifyValidator
import Testing.IntraItemBytes qualified as IntraItemBytes
import Testing.MpfProof qualified as MpfProof
import Testing.MpfProofFold qualified as MpfProofFold

-- | Generates an arbitrary bytestring for property tests.
-- | Generates arbitrary byte strings for Merkle helper property tests.
genByteString :: Gen BS.ByteString
genByteString = do
  len <- choose (0, 100) -- You can choose the length range you prefer
  bytes <- vectorOf len (arbitrary :: Gen Word8)
  return $ BS.pack bytes
-- | Generates exactly four bytestring leaves for Merkle-tree tests.

-- | Generates four leaves for the fixed-width Merkle property.
-- NOTE: this used to go through an orphan @Arbitrary BS.ByteString@ instance.
-- quickcheck-instances now supplies that instance itself, so the generator is
-- named directly here to keep the 0..100 byte length range these properties
-- were written against.
genFourBytearrays :: Gen [BS.ByteString]
genFourBytearrays = vectorOf 4 genByteString

-- | Runs the Plutarch test suite.
main :: IO ()
main = defaultMain tests

-- | Checks that the four-leaf Merkle helper reconstructs the expected root.
merkle_4_test :: Property
merkle_4_test = forAll genFourBytearrays $ \nodes ->
  plift $ pmerkle_4_test # (pconstant @(PBuiltinList PByteString) nodes)

-- | Plutarch term backing the four-leaf Merkle helper property.
pmerkle_4_test :: (forall s. Term s (PBuiltinList PByteString :--> PBool))
pmerkle_4_test = plam $ \nodes -> P.do
  a <- plet $ phead # nodes
  aRest <- plet (ptail # nodes)
  b <- plet $ phead # aRest
  bRest <- plet (ptail # aRest)
  c <- plet $ phead # bRest
  d <- plet $ phead # (ptail # bRest)

  root <- plet $ pcombine # (pcombine # a # b) # (pcombine # c # d)

  pand'List
    [ pmerkle_4 # 0 # a # (pcombine # c # d) # b #== root
-- | Exercises null-hash combination behavior in the trie helpers.
    , pmerkle_4 # 1 # b # (pcombine # c # d) # a #== root
    , pmerkle_4 # 2 # c # (pcombine # a # b) # d #== root
    , pmerkle_4 # 3 # d # (pcombine # a # b) # c #== root
    ]

-- | Verifies the expected null-hash composition ladder.
combineNullHash :: Term s PBool
combineNullHash =
-- | Checks trie suffix examples against the expected outputs.
  pand'List
    [ pcombine # pnull_hash # pnull_hash #== pnull_hash_2
    , pcombine # pnull_hash_2 # pnull_hash_2 #== pnull_hash_4
    , pcombine # pnull_hash_4 # pnull_hash_4 #== pnull_hash_8
    ]

-- | Exercises suffix extraction against fixed examples.
examplesSuffix :: Term s PBool
examplesSuffix =
  pand'List
    [ (psuffix # phexByteStr "abcd456789" # 0 #== phexByteStr "ffabcd456789")
-- | Checks trie nibble-list examples against the expected outputs.
    , (psuffix # phexByteStr "abcd456789" # 1 #== phexByteStr "000bcd456789")
    , (psuffix # phexByteStr "abcd456789" # 2 #== phexByteStr "ffcd456789")
    , (psuffix # phexByteStr "abcd456789" # 4 #== phexByteStr "ff456789")
    , (psuffix # phexByteStr "abcd456789" # 5 #== phexByteStr "00056789")
    , (psuffix # phexByteStr "abcd456789" # 10 #== phexByteStr "ff")
    ]

-- | Exercises nibble slice extraction against fixed examples.
examplesNibbles :: Term s PBool
examplesNibbles =
-- | Checks single-nibble helpers against the expected outputs.
  pand'List
    [ (pnibbles # phexByteStr "0123456789" # 2 # 2 #== pconstant (BS.pack []))
    , (pnibbles # phexByteStr "0123456789" # 2 # 3 #== pconstant (BS.pack [2]))
    , (pnibbles # phexByteStr "0123456789" # 4 # 8 #== pconstant (BS.pack [4, 5, 6, 7]))
    , (pnibbles # phexByteStr "0123456789" # 3 # 6 #== pconstant (BS.pack [3, 4, 5]))
    , (pnibbles # phexByteStr "0123456789" # 1 # 7 #== pconstant (BS.pack [1, 2, 3, 4, 5, 6]))
    ]
-- | Collects the tests defined in this module.

-- | Exercises single-nibble extraction against fixed examples.
examplesNibble :: Term s PBool
examplesNibble =
  pand'List
    [ pnibble # phexByteStr "ab" # 0 #== 10
    , pnibble # phexByteStr "ab" # 1 #== 11
    ]

-- | Aggregates the helper, crypto, trie, and membership tests.
tests :: TestTree
tests =
  testGroup
    "Helper Tests"
    [ testGroup
        "combine tests"
        [ testCase "combine null hashes" $
            passertEval combineNullHash
        ]
    , testGroup
        "suffix tests"
        [ testCase "suffix examples" $ do
            passertEval examplesSuffix
        ]
    , testGroup
        "nibbles tests"
        [ testCase "nibbles examples" $ do
            passertEval examplesNibbles
        ]
    , testGroup
        "nibble tests"
        [ testCase "nibble examples" $
            passertEval examplesNibble
        ]
    , testGroup
        "Merkle tests"
        [ QC.testProperty "merkle_4 property" merkle_4_test
        ]
    , MPF.tests
    , Crypto.tests
    , MembershipValidator.tests
    , HubOracleValidator.tests
    , FraudProofCatalogue.tests
    , FraudProof.tests
    , ReserveValidator.tests
    , LinkedListTests.tests
    , DesignPatterns.tests
    , OperatorDirectory.tests
    , RetiredOperators.tests
    , RegisteredOperators.tests
    , ActiveOperators.tests
    , DepositValidator.tests
    , WithdrawalValidator.tests
    , CountedMembership.tests
    , CanonicalCborScan.tests
    , CanonicalPlutusData.tests
    , CanonicalDecodeItemStaging.tests
    , CanonicalVersionTuple.tests
    , CommonValue.tests
    , CommonUtils.tests
    , SettlementValidator.tests
    , EventInclusion.tests
    , EventV1Abi.tests
    , TxOrderValidator.tests
    , StateQueueLib.tests
    , SchedulerValidator.tests
    , HeaderValidity.tests
    , StateQueueValidator.tests
    , ComputationThreadValidator.tests
    , PayoutValidator.tests
    , DaParamsGovernor.tests
    , DaAttestationSignatures.tests
    , DaAttestationReaders.tests
    , DaAttestationOperations.tests
    , DaAttestationHandlers.tests
    , WitnessValidator.tests
    , TxOrderFields.tests
    , TxOrderV1Abi.tests
    , TxFieldReceipt.tests
    , ValidationMerkle.tests
    , ValidationResolution.tests
    , ValidationMachine.tests
    , ValidationOneStepCrossLanguage.tests
    , ValidationInstructionEvidenceBounds.tests
    , BoundedBlob.tests
    , BoundedCollection.tests
    , BoundedItem.tests
    , LedgerOutputCommitment.tests
    , LedgerOutput.tests
    , LedgerOutputValue.tests
    , LedgerValueParity.tests
    , LedgerOutputScan.tests
    , LedgerOutputProof.tests
    , RedeemerItemProof.tests
    , NativeScript.tests
    , NativeScriptScan.tests
    , NativeTxScriptPushdown.tests
    , NativeTxIntraItem.tests
    , NativeTxFaultStatement.tests
    , ScriptSourcesRedeemerNormalization.tests
    , ScriptContext.tests
    , ScriptProof.tests
    , NativeTxCarriage.tests
    , NativeTxCarriageWireGolden.tests
    , CekCost.tests
    , Blake2b256Trace.tests
    , Blake2b224Trace.tests
    , CekBlobFrontier.tests
    , CekBuiltin.tests
    , CekMachine.tests
    , CekSourceBlob.tests
    , CekConstant.tests
    , CekData.tests
    , CekDataBytes.tests
    , CekDataFrame.tests
    , CekDataInteger.tests
    , CekDataScan.tests
    , CekDataTraverse.tests
    , CekProof.tests
    , AikenCbor.tests
    , FieldPreimageCertificateValidator.tests
    , NativeTxC20Field6Maximum.tests
    , NativeTxC20Identity.tests
    , NativeTxCodec.tests
    , NativeTxCompact.tests
    , NativeTxFieldAccess.tests
    , NativeTxMaximumProfiles.tests
    , NativeTxMachineWalk.tests
    , NativeTxComponents.tests
    , NativeTxCoreGolden.tests
    , NativeTxPreimages.tests
    , NativeTxTransaction.tests
    , IntraItemBytes.tests
    , MpfProof.tests
    , MpfProofFold.tests
    , MpfChunkedVerifyValidator.tests
    , MpfChunkedProof.tests
    , FraudProofsCommon.tests
    , FieldOpening.tests
    , FraudProofsDoubleSpend.tests
    , FraudProofsZeroInput.tests
    , FraudProofsNoInput.tests
    , FraudProofsInputNoIdx.tests
    , FraudProofsNoReferenceInput.tests
    , FraudProofsSignature.tests
    , FraudProofsMissingNativeScriptTx.tests
    , FraudProofsQ1xSpendInputCardinality.tests
    , FraudProofsWithdrawnReferenceInput.tests
    , FraudProofsDaHashPreimage.tests
    , FraudProofsInvalidRange.tests
    , FraudProofsMinFee.tests
    , FragmentEnvelopeV1.tests
    , TraceProofs.tests
    , TransactionRootV1Golden.tests
    , TransitionTraceAiken.tests
    , TransitionTraceProof.tests
    , FraudProofsTransitionTrace.tests
    , ValidationTrace.tests
    , ValidationTraceCanonicalDecode.tests
    , ValidationTraceCompactBinding.tests
    , ValidationTraceInputSets.tests
    , ValidationTracePhaseANativeScripts.tests
    , ValidationTracePhaseAScriptPreconditions.tests
    , ValidationTraceResolveInputs.tests
    , ValidationTraceScriptSourcesStageZero.tests
    , ValidationTraceSignatures.tests
    , ValidationTraceStaticLedgerRules.tests
    , ScriptLanguageViews.tests
    , ValidationClaim.tests
    , ValidationDispute.tests
    ]
