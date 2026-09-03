module Testing.CanonicalVersionTuple (tests) where

import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Midgard.Blake2b224Trace qualified as Blake2b224Trace
import Midgard.Blake2b256Trace qualified as Blake2b256Trace
import Midgard.BoundedBlob qualified as BoundedBlob
import Midgard.BoundedCollection qualified as BoundedCollection
import Midgard.BoundedItem qualified as BoundedItem
import Midgard.CekBlobFrontier qualified as CekBlobFrontier
import Midgard.CekDataBytes qualified as CekDataBytes
import Midgard.CekDataInteger qualified as CekDataInteger
import Midgard.CekProof qualified as CekProof
import Midgard.CekSourceBlob qualified as CekSourceBlob
import Midgard.FraudProofs.NativeTx.Types qualified as NativeTx
import Midgard.LedgerOutputCommitment qualified as LedgerOutputCommitment
import Midgard.LedgerOutputProof qualified as LedgerOutputProof
import Midgard.LedgerOutputScan qualified as LedgerOutputScan
import Midgard.LedgerOutputValue qualified as LedgerOutputValue
import Midgard.LedgerState qualified as LedgerState
import Midgard.MpfProofFold qualified as MpfProofFold
import Midgard.NativeScriptScan qualified as NativeScriptScan
import Midgard.ValidationClaim qualified as ValidationClaim
import Midgard.ValidationDispute qualified as ValidationDispute
import Midgard.ValidationResolution qualified as ValidationResolution
import Midgard.ValidationTrace qualified as ValidationTrace
import Testing.Eval (passertEvalNoTrace)

tests :: TestTree
tests =
  testGroup
    "Canonical V1 Version Tuple Tests"
    [ testCase "canonical_v1_version_tuple_matches_typescript_profile" $
        passertEvalNoTrace canonicalV1VersionTupleMatchesTypescriptProfile
    ]

canonicalV1VersionTupleMatchesTypescriptProfile :: forall s. Term s PBool
canonicalV1VersionTupleMatchesTypescriptProfile =
  pand'List
    [ LedgerState.pprotocolVersionV1 #== 1
    , LedgerState.ptransitionStepSchemaVersionV1 #== 1
    , NativeTx.pnativeTxVersionV1 #== 1
    , ValidationTrace.pmachineVersion #== 1
    , ValidationTrace.pdescriptorVersion #== 1
    , ValidationDispute.pdisputeVersion #== 1
    , ValidationClaim.pclaimVersion #== 1
    , ValidationResolution.presolutionVersion #== 1
    , ValidationResolution.ppreparedResolutionVersion #== 1
    , ValidationResolution.pwinningResolutionVersion #== 1
    , CekProof.pprogramEnvelopeVersion #== 1
    , BoundedBlob.pversion #== 1
    , BoundedCollection.pboundedCollectionVersion #== 1
    , BoundedItem.pversion #== 1
    , CekBlobFrontier.pcekBlobFrontierVersion #== 1
    , CekDataBytes.pversion #== 1
    , CekDataInteger.pversion #== 1
    , CekSourceBlob.pcekSourceBlobVersion #== 1
    , LedgerOutputCommitment.pledgerOutputCommitmentVersion #== 1
    , LedgerOutputProof.pversion #== 1
    , LedgerOutputScan.pversion #== 1
    , LedgerOutputValue.pversion #== 1
    , NativeScriptScan.pversion #== 1
    , MpfProofFold.pproofFoldVersion #== 1
    , Blake2b224Trace.pblake2b224TraceVersion #== 1
    , Blake2b256Trace.pblake2b256TraceVersion #== 1
    ]
