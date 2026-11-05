{-# LANGUAGE OverloadedStrings #-}

-- | Exact unused-redeemer rule and nine-script Data ABI.
module Midgard.FraudProofs.UnusedRedeemer (
  PBoundRedeemerV1 (..),
  PAuthenticatedRedeemerV1 (..),
  PAuthenticatedDescriptorV1 (..),
  PAuthenticatedControlV1 (..),
  PAuthenticatedItemHeaderV1 (..),
  PReverseScanV1 (..),
  PSelectionOpeningV1 (..),
  PDecisionV1 (..),
  PStep01SourceV1 (..),
  PStep01Args (..),
  PStep02Args (..),
  PStep02aArgs (..),
  PStep02bArgs (..),
  PStep02cArgs (..),
  PLinearArgs (..),
  PStep05Args (..),
  PStep06Args (..),
  pmaximumScanBatch,
  predeemerTagForPurposeKindV1,
  pbindRedeemerV1,
  pauthenticateDescriptorV1,
  pauthenticateControlV1,
  pauthenticateItemHeaderV1,
  pauthenticateItemTailV1,
  pinitialReverseScanV1,
  pscanSelectionV1,
  preverseScanCompleteV1,
  pdecisionV1,
  pterminalContradictionV1,
  pencodeReverseScanV1,
) where

import Data.ByteString qualified as BS
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.BoundedItem (PChunkProofV1 (..))
import Midgard.BoundedItem qualified as Bounded
import Midgard.CanonicalCborScan (PCborHeadV1 (..), pheadAtV1)
import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.NativeTx.Codec (pcborInt)
import Midgard.FraudProofs.NativeTx.Compact (pnativeTxProofCommitmentV1)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PEventKey (..), PHeaderV1)
import Midgard.RedeemerItemProof (PRedeemerItemProofControlV1 (..), phashControlV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PUnusedRedeemer), prejectionCodeOf)
import Midgard.ScriptProof (pexecutionLeafHash, ppurposeLeafHash, predeemerItemLeafHash)
import Midgard.TransitionTrace (PRootDomain (PValidationTracesRootDomain), PRootMembershipProof (..), pverifyRootMembershipWithBytes)
import Midgard.ValidationMachine (PScriptDiscoveryControlV1 (..), PScriptSourcesControlV1 (..), pencodeScriptSourcesDiscoveryWitness)
import Midgard.ValidationMerkle (PFrontierPeak, pfrontierIsWellFormed, pverifyMembership)
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase (PScriptSources),
  PValidationSourceKind (PForced, PNormal),
  PValidationTraceDescriptorV1 (..),
  PValidationTraceProof (..),
  PValidationVerdict (PAccepted, PRejected),
  phashMachineState,
  phashRejectionCode,
  phashValidationContext,
  phashWorkWitness,
  pverifyTraceProof,
 )

data PBoundRedeemerV1 (s :: S) = PBoundRedeemerV1
  { pboundRedeemer'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pboundRedeemer'validationTracesRoot :: Term s (PAsData PByteString)
  , pboundRedeemer'validationTraceCount :: Term s (PAsData PInteger)
  , pboundRedeemer'redeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBoundRedeemerV1)

data PAuthenticatedRedeemerV1 (s :: S) = PAuthenticatedRedeemerV1
  { pauthenticatedRedeemer'bound :: Term s (PAsData PBoundRedeemerV1)
  , pauthenticatedRedeemer'purposeTag :: Term s (PAsData PInteger)
  , pauthenticatedRedeemer'pointerIndex :: Term s (PAsData PInteger)
  , pauthenticatedRedeemer'itemCount :: Term s (PAsData PInteger)
  , pauthenticatedRedeemer'itemLength :: Term s (PAsData PInteger)
  , pauthenticatedRedeemer'itemCommitment :: Term s (PAsData PByteString)
  , pauthenticatedRedeemer'redeemerLeaf :: Term s (PAsData PByteString)
  , pauthenticatedRedeemer'purposeCount :: Term s (PAsData PInteger)
  , pauthenticatedRedeemer'purposePeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pauthenticatedRedeemer'executionCount :: Term s (PAsData PInteger)
  , pauthenticatedRedeemer'executionPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedRedeemerV1)

data PAuthenticatedDescriptorV1 (s :: S) = PAuthenticatedDescriptorV1
  { pauthenticatedDescriptor'bound :: Term s (PAsData PBoundRedeemerV1)
  , pauthenticatedDescriptor'eventKeyHash :: Term s (PAsData PByteString)
  , pauthenticatedDescriptor'descriptor :: Term s (PAsData PValidationTraceDescriptorV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedDescriptorV1)

data PAuthenticatedControlV1 (s :: S) = PAuthenticatedControlV1
  { pauthenticatedControl'bound :: Term s (PAsData PBoundRedeemerV1)
  , pauthenticatedControl'programCounter :: Term s (PAsData PInteger)
  , pauthenticatedControl'stage :: Term s (PAsData PInteger)
  , pauthenticatedControl'expectedItemControlHash :: Term s (PAsData PByteString)
  , pauthenticatedControl'usedRedeemerBitmap :: Term s (PAsData PInteger)
  , pauthenticatedControl'currentPurposeKind :: Term s (PAsData PInteger)
  , pauthenticatedControl'currentPurposeIndex :: Term s (PAsData PInteger)
  , pauthenticatedControl'redeemerCount :: Term s (PAsData PInteger)
  , pauthenticatedControl'purposeCount :: Term s (PAsData PInteger)
  , pauthenticatedControl'purposePeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pauthenticatedControl'executionCount :: Term s (PAsData PInteger)
  , pauthenticatedControl'executionPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedControlV1)

data PAuthenticatedItemHeaderV1 (s :: S) = PAuthenticatedItemHeaderV1
  { pauthenticatedHeader'authenticated :: Term s (PAsData PAuthenticatedControlV1)
  , pauthenticatedHeader'itemIndex :: Term s (PAsData PInteger)
  , pauthenticatedHeader'itemCount :: Term s (PAsData PInteger)
  , pauthenticatedHeader'totalLength :: Term s (PAsData PInteger)
  , pauthenticatedHeader'itemCommitment :: Term s (PAsData PByteString)
  , pauthenticatedHeader'purposeTag :: Term s (PAsData PInteger)
  , pauthenticatedHeader'pointerIndex :: Term s (PAsData PInteger)
  , pauthenticatedHeader'dataOffset :: Term s (PAsData PInteger)
  , pauthenticatedHeader'dataLength :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedItemHeaderV1)

data PReverseScanV1 (s :: S) = PReverseScanV1
  { preverseScan'authenticated :: Term s (PAsData PAuthenticatedRedeemerV1)
  , preverseScan'cursor :: Term s (PAsData PInteger)
  , preverseScan'used :: Term s (PAsData PBool)
  , preverseScan'checkpointHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PReverseScanV1)

data PSelectionOpeningV1 (s :: S) = PSelectionOpeningV1
  { pselectionOpening'frontierIndex :: Term s (PAsData PInteger)
  , pselectionOpening'purposeKind :: Term s (PAsData PInteger)
  , pselectionOpening'purposeIndex :: Term s (PAsData PInteger)
  , pselectionOpening'scriptHash :: Term s (PAsData PByteString)
  , pselectionOpening'purposeSubject :: Term s (PAsData PByteString)
  , pselectionOpening'purposeSiblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , pselectionOpening'languageTag :: Term s (PAsData PInteger)
  , pselectionOpening'sourceLeaf :: Term s (PAsData PByteString)
  , pselectionOpening'redeemerLeaf :: Term s (PAsData PByteString)
  , pselectionOpening'executionSiblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSelectionOpeningV1)

data PDecisionV1 (s :: S) = PDecisionV1
  { pdecision'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pdecision'redeemerIndex :: Term s (PAsData PInteger)
  , pdecision'unused :: Term s (PAsData PBool)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDecisionV1)

data PStep01SourceV1 (s :: S)
  = PAcceptedSource (Term s (PAsData PNativeTxInclusionCarriage))
  | PForcedSource
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PHeaderV1))
      (Term s (PAsData PRootMembershipProof))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01SourceV1)

data PStep01Args (s :: S) = PStep01Args
  { pstep01Args'source :: Term s (PAsData PStep01SourceV1)
  , pstep01Args'redeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'traceMembership :: Term s (PAsData PRootMembershipProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep02aArgs (s :: S) = PStep02aArgs
  { pstep02aArgs'inputIndex :: Term s (PAsData PInteger)
  , pstep02aArgs'outputIndex :: Term s (PAsData PInteger)
  , pstep02aArgs'machineState :: Term s (PAsData PValidationMachineStateV1)
  , pstep02aArgs'traceProof :: Term s (PAsData PValidationTraceProof)
  , pstep02aArgs'control :: Term s (PAsData PScriptSourcesControlV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02aArgs)

data PStep02bArgs (s :: S) = PStep02bArgs
  { pstep02bArgs'inputIndex :: Term s (PAsData PInteger)
  , pstep02bArgs'outputIndex :: Term s (PAsData PInteger)
  , pstep02bArgs'itemControl :: Term s (PAsData PRedeemerItemProofControlV1)
  , pstep02bArgs'chunkProof :: Term s (PAsData PChunkProofV1)
  , pstep02bArgs'nextChunkProof :: Term s (PAsData (PMaybeData PChunkProofV1))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02bArgs)

data PStep02cArgs (s :: S) = PStep02cArgs
  { pstep02cArgs'inputIndex :: Term s (PAsData PInteger)
  , pstep02cArgs'outputIndex :: Term s (PAsData PInteger)
  , pstep02cArgs'chunkProof :: Term s (PAsData PChunkProofV1)
  , pstep02cArgs'nextChunkProof :: Term s (PAsData (PMaybeData PChunkProofV1))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02cArgs)

data PLinearArgs (s :: S) = PLinearArgs
  { plinearArgs'inputIndex :: Term s (PAsData PInteger)
  , plinearArgs'outputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLinearArgs)

data PStep05Args (s :: S) = PStep05Args
  { pstep05Args'inputIndex :: Term s (PAsData PInteger)
  , pstep05Args'outputIndex :: Term s (PAsData PInteger)
  , pstep05Args'openings :: Term s (PAsData (PBuiltinList (PAsData PSelectionOpeningV1)))
  , pstep05Args'itemBudget :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep05Args)

data PStep06Args (s :: S) = PStep06Args
  { pstep06Args'inputIndex :: Term s (PAsData PInteger)
  , pstep06Args'outputIndex :: Term s (PAsData PInteger)
  , pstep06Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep06Args)

pmaximumScanBatch :: forall s. Term s PInteger
pmaximumScanBatch = 16

predeemerTagForPurposeKindV1 :: forall s. Term s (PInteger :--> PMaybe PInteger)
predeemerTagForPurposeKindV1 = phoistAcyclic $ plam $ \kind ->
  pif (kind #== 0) (pcon $ PJust 0) $
    pif (kind #== 1) (pcon $ PJust 1) $
      pif (kind #== 2) (pcon $ PJust 3) $
        pif (kind #== 3) (pcon $ PJust 6) (pcon PNothing)

pbindRedeemerV1 :: forall s. Term s (Subject.PVerdictSubject :--> PByteString :--> PInteger :--> PInteger :--> PBoundRedeemerV1)
pbindRedeemerV1 = phoistAcyclic $ plam $ \subject traceRoot traceCount redeemerIndex ->
  pif
    ( Subject.psubjectIsCanonical
        # subject
        #&& plengthBS
        # traceRoot
        #== 32
        #&& traceCount
        #> 0
        #&& redeemerIndex
        #>= 0
    )
    ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
        let bound = pcon $ PBoundRedeemerV1 (pdata subject) (pdata traceRoot) (pdata traceCount) (pdata redeemerIndex)
         in pif
              (pfromData psubject'direction #== 1)
              (plet (Subject.pbindExactRejectionReason # subject # pcon (PUnusedRedeemer $ pdata redeemerIndex)) $ \_ -> bound)
              bound
    )
    perror

pauthenticateDescriptorV1 :: forall s. Term s (PBoundRedeemerV1 :--> PRootMembershipProof :--> PAuthenticatedDescriptorV1)
pauthenticateDescriptorV1 = phoistAcyclic $ plam $ \bound membership -> P.do
  PBoundRedeemerV1{..} <- pmatch bound
  subject <- plet $ pfromData pboundRedeemer'subject
  PRootMembershipProof{prootMembership'key, prootMembership'value} <- pmatch membership
  eventKey <- plet $ punsafeCoerce prootMembership'key
  descriptor <- plet $ punsafeCoerce prootMembership'value
  let expected = pcon $ PAuthenticatedDescriptorV1 (pdata bound) (pdata $ pblake2b_256 # (pserialiseData # prootMembership'key)) (pdata descriptor)
  pif
    ( peventKeyMatchesSubject eventKey subject
        #&& pdescriptorMatchesSubject descriptor subject
        #&& pverifyRootMembershipWithBytes
          membership
          (pdata $ pcon PValidationTracesRootDomain)
          (pfromData pboundRedeemer'validationTracesRoot)
          (pfromData pboundRedeemer'validationTraceCount)
          (pserialiseData # prootMembership'key)
          (pserialiseData # prootMembership'value)
    )
    expected
    perror

pauthenticateControlV1 :: forall s. Term s (PAuthenticatedDescriptorV1 :--> PValidationMachineStateV1 :--> PValidationTraceProof :--> PScriptSourcesControlV1 :--> PAuthenticatedControlV1)
pauthenticateControlV1 = phoistAcyclic $ plam $ \authenticated machineState traceProof control -> P.do
  PAuthenticatedDescriptorV1{..} <- pmatch authenticated
  bound <- plet $ pfromData pauthenticatedDescriptor'bound
  descriptor <- plet $ pfromData pauthenticatedDescriptor'descriptor
  PValidationTraceDescriptorV1{pdescriptor'stepCount} <- pmatch descriptor
  PBoundRedeemerV1{pboundRedeemer'subject, pboundRedeemer'redeemerIndex} <- pmatch bound
  subject <- plet $ pfromData pboundRedeemer'subject
  Subject.PVerdictSubject{Subject.psubject'transactionId, Subject.psubject'sourceKind} <- pmatch subject
  PValidationMachineStateV1{..} <- pmatch machineState
  PValidationTraceProof{ptraceProof'stateIndex, ptraceProof'stateHash} <- pmatch traceProof
  PScriptSourcesControlV1{..} <- pmatch control
  discovery <- plet $ pfromData pscriptSources'discovery
  PScriptDiscoveryControlV1{..} <- pmatch discovery
  let sourceKind = pif (pfromData psubject'sourceKind #== 0) (pcon PNormal) (pcon PForced)
      expected =
        pcon $
          PAuthenticatedControlV1
            (pdata bound)
            pmachineState'programCounter
            pscriptSources'stage
            pscriptDiscovery'redeemerItemControlHash
            pscriptDiscovery'usedRedeemerBitmap
            pscriptDiscovery'currentPurposeKind
            pscriptDiscovery'currentPurposeIndex
            pscriptSources'redeemerCount
            pscriptSources'purposeCount
            pscriptSources'purposePeaks
            pscriptDiscovery'executionCount
            pscriptDiscovery'executionPeaks
  pif
    ( pand'List
        [ pfromData pmachineState'eventKeyHash #== pfromData pauthenticatedDescriptor'eventKeyHash
        , pfromData pmachineState'transactionId #== pfromData psubject'transactionId
        , pfromData pmachineState'sourceKind #== sourceKind
        , pfromData pmachineState'phase #== pcon PScriptSources
        , pfromData pmachineState'programCounter #== pfromData ptraceProof'stateIndex
        , pfromData pmachineState'programCounter #< pfromData pdescriptor'stepCount
        , pfromData pmachineState'workRoot #== phashWorkWitness # pcon PScriptSources # pfromData pmachineState'programCounter # (pencodeScriptSourcesDiscoveryWitness # control # pfromData pscriptSources'stage # discovery)
        , pfromData ptraceProof'stateHash #== phashMachineState # machineState
        , pverifyTraceProof # descriptor # traceProof
        , pfromData pmachineState'transactionCommitment #== pnativeTxProofCommitmentV1 # pfromData pscriptSources'compactCbor # pfromData pscriptSources'witnessSetCompactCbor # pfromData pscriptSources'fieldPreimageLengthsCbor
        , pfromData pmachineState'validationContextHash #== phashValidationContext # pfromData pscriptSources'contextCbor
        , pfromData pboundRedeemer'redeemerIndex #< pfromData pscriptSources'redeemerCount
        , pfromData pscriptSources'purposeCount #== pfromData pscriptDiscovery'executionCount
        , pfrontierIsWellFormed # pfromData pscriptSources'purposeCount # pfromData pscriptSources'purposePeaks
        , pfrontierIsWellFormed # pfromData pscriptDiscovery'executionCount # pfromData pscriptDiscovery'executionPeaks
        , pfromData pscriptSources'stage #== 12
        , pfromData pscriptDiscovery'redeemerCursor #== pfromData pboundRedeemer'redeemerIndex
        , pfromData pscriptDiscovery'redeemerItemControlHash #/= pconstant ""
        ]
    )
    expected
    perror

pauthenticateItemHeaderV1 :: forall s. Term s (PAuthenticatedControlV1 :--> PRedeemerItemProofControlV1 :--> PChunkProofV1 :--> PMaybeData PChunkProofV1 :--> PAuthenticatedItemHeaderV1)
pauthenticateItemHeaderV1 = phoistAcyclic $ plam $ \authenticated itemControl chunkProof nextChunkProof -> P.do
  PAuthenticatedControlV1{..} <- pmatch authenticated
  PBoundRedeemerV1{pboundRedeemer'redeemerIndex} <- pmatch $ pfromData pauthenticatedControl'bound
  PRedeemerItemProofControlV1{..} <- pmatch itemControl
  let totalLength = pfromData predeemerControl'totalLength
      headerLength = pif (totalLength #< 28) totalLength 28
  bytes <- plet $ popenSpan # pfromData predeemerControl'itemIndex # totalLength # pfromData predeemerControl'itemCommitment # 0 # headerLength # chunkProof # nextChunkProof
  outer <- plet $ pexpectHead bytes 0 4
  purpose <- plet $ pexpectHead bytes (pheadOffset outer) 0
  pointer <- plet $ pexpectHead bytes (pheadOffset purpose) 0
  dat <- plet $ pexpectHead bytes (pheadOffset pointer) 2
  let purposeValue = pheadValue purpose
      pointerValue = pheadValue pointer
      dataOffset = pheadOffset dat
      dataLength = pheadValue dat
      expected =
        pcon $
          PAuthenticatedItemHeaderV1
            (pdata authenticated)
            predeemerControl'itemIndex
            predeemerControl'itemCount
            predeemerControl'totalLength
            predeemerControl'itemCommitment
            (pdata purposeValue)
            (pdata pointerValue)
            (pdata dataOffset)
            (pdata dataLength)
  pif
    ( pand'List
        [ phashControlV1 # itemControl #== pfromData pauthenticatedControl'expectedItemControlHash
        , pfromData predeemerControl'version #== 1
        , pfromData predeemerControl'mode #== 0
        , pfromData predeemerControl'stage #== 0
        , pfromData predeemerControl'itemIndex #== pfromData pboundRedeemer'redeemerIndex
        , pfromData predeemerControl'itemCount #== pfromData pauthenticatedControl'redeemerCount
        , totalLength #> 0
        , plengthBS # pfromData predeemerControl'itemCommitment #== 32
        , pfromData predeemerControl'expectedPurposeTag #== -1
        , pfromData predeemerControl'expectedPointerIndex #== -1
        , pheadValue outer #== 4
        , purposeValue #== 0 #|| purposeValue #== 1 #|| purposeValue #== 3 #|| purposeValue #== 6
        , pointerValue #>= 0
        , dataOffset #> 0
        , dataLength #> 0
        , dataOffset + dataLength #< totalLength
        , totalLength - dataOffset - dataLength #<= 19
        ]
    )
    expected
    perror

pauthenticateItemTailV1 :: forall s. Term s (PAuthenticatedItemHeaderV1 :--> PChunkProofV1 :--> PMaybeData PChunkProofV1 :--> PAuthenticatedRedeemerV1)
pauthenticateItemTailV1 = phoistAcyclic $ plam $ \header chunkProof nextChunkProof -> P.do
  PAuthenticatedItemHeaderV1{..} <- pmatch header
  authenticated <- plet $ pfromData pauthenticatedHeader'authenticated
  PAuthenticatedControlV1{pauthenticatedControl'bound, pauthenticatedControl'usedRedeemerBitmap, pauthenticatedControl'purposeCount, pauthenticatedControl'purposePeaks, pauthenticatedControl'executionCount, pauthenticatedControl'executionPeaks} <- pmatch authenticated
  bound <- plet $ pfromData pauthenticatedControl'bound
  PBoundRedeemerV1{pboundRedeemer'subject} <- pmatch bound
  subject <- plet $ pfromData pboundRedeemer'subject
  Subject.PVerdictSubject{Subject.psubject'direction} <- pmatch subject
  let tailStart = pfromData pauthenticatedHeader'dataOffset + pfromData pauthenticatedHeader'dataLength
      tailLength = pfromData pauthenticatedHeader'totalLength - tailStart
  bytes <- plet $ popenSpan # pfromData pauthenticatedHeader'itemIndex # pfromData pauthenticatedHeader'totalLength # pfromData pauthenticatedHeader'itemCommitment # tailStart # tailLength # chunkProof # nextChunkProof
  outer <- plet $ pexpectHead bytes 0 4
  memory <- plet $ pexpectHead bytes (pheadOffset outer) 0
  steps <- plet $ pexpectHead bytes (pheadOffset memory) 0
  let usedBit = pmod # (pdiv # pfromData pauthenticatedControl'usedRedeemerBitmap # (pbit # pfromData pauthenticatedHeader'itemIndex)) # 2
      expectedBit = pif (pfromData psubject'direction #== 0) (usedBit #== 0) (usedBit #== 1)
      redeemerLeaf = predeemerItemLeafHash # pfromData pauthenticatedHeader'itemIndex # pfromData pauthenticatedHeader'itemCommitment
      expected =
        pcon $
          PAuthenticatedRedeemerV1
            (pdata bound)
            pauthenticatedHeader'purposeTag
            pauthenticatedHeader'pointerIndex
            pauthenticatedHeader'itemCount
            pauthenticatedHeader'totalLength
            pauthenticatedHeader'itemCommitment
            (pdata redeemerLeaf)
            pauthenticatedControl'purposeCount
            pauthenticatedControl'purposePeaks
            pauthenticatedControl'executionCount
            pauthenticatedControl'executionPeaks
  pif
    ( pheadValue outer
        #== 2
        #&& pheadOffset steps
        #== plengthBS
        # bytes
        #&& pheadValue memory
        #>= 0
        #&& pheadValue steps
        #>= 0
        #&& expectedBit
    )
    expected
    perror

pinitialReverseScanV1 :: forall s. Term s (PAuthenticatedRedeemerV1 :--> PReverseScanV1)
pinitialReverseScanV1 = phoistAcyclic $ plam $ \authenticated ->
  pmatch authenticated $ \PAuthenticatedRedeemerV1{..} ->
    pmatch (pfromData pauthenticatedRedeemer'bound) $ \PBoundRedeemerV1{pboundRedeemer'subject, pboundRedeemer'redeemerIndex} ->
      pmatch (pfromData pboundRedeemer'subject) $ \Subject.PVerdictSubject{Subject.psubject'transactionId} ->
        pcon $
          PReverseScanV1
            (pdata authenticated)
            (pdata 0)
            (pdata $ pconstant False)
            ( pdata $
                pblake2b_256
                  # ( pconstant "MidgardUnusedRedeemerScanV1"
                        <> pfromData psubject'transactionId
                        <> pcborInt (pfromData pboundRedeemer'redeemerIndex)
                        <> pfromData pauthenticatedRedeemer'redeemerLeaf
                        <> pcborInt (pfromData pauthenticatedRedeemer'purposeCount)
                    )
            )

pscanSelectionV1 :: forall s. Term s (PReverseScanV1 :--> PSelectionOpeningV1 :--> PReverseScanV1)
pscanSelectionV1 = phoistAcyclic $ plam $ \state opening -> P.do
  PReverseScanV1{..} <- pmatch state
  authenticated <- plet $ pfromData preverseScan'authenticated
  PAuthenticatedRedeemerV1{..} <- pmatch authenticated
  PSelectionOpeningV1{..} <- pmatch opening
  purposeLeaf <- plet $ ppurposeLeafHash # pfromData pselectionOpening'purposeKind # pfromData pselectionOpening'purposeIndex # pfromData pselectionOpening'scriptHash # pfromData pselectionOpening'purposeSubject
  executionLeaf <- plet $ pexecutionLeafHash # pfromData pselectionOpening'languageTag # purposeLeaf # pfromData pselectionOpening'sourceLeaf # pfromData pselectionOpening'redeemerLeaf
  expectedTag <- plet $ pmatch (predeemerTagForPurposeKindV1 # pfromData pselectionOpening'purposeKind) $ \case
    PNothing -> perror
    PJust tag -> tag
  let used =
        expectedTag
          #== pfromData pauthenticatedRedeemer'purposeTag
          #&& pfromData pselectionOpening'purposeIndex
          #== pfromData pauthenticatedRedeemer'pointerIndex
          #&& pfromData pselectionOpening'redeemerLeaf
          #== pfromData pauthenticatedRedeemer'redeemerLeaf
      nextCursor = pfromData preverseScan'cursor + 1
      nextHash =
        pblake2b_256
          # ( pfromData preverseScan'checkpointHash
                <> pcborInt nextCursor
                <> purposeLeaf
                <> executionLeaf
                <> (pserialiseData # pforgetData (pdata used))
            )
      expected = pcon $ PReverseScanV1 preverseScan'authenticated (pdata nextCursor) (pdata used) (pdata nextHash)
  pif
    ( pnot
        # pfromData preverseScan'used
        #&& pfromData preverseScan'cursor
        #< pfromData pauthenticatedRedeemer'purposeCount
        #&& pfromData pselectionOpening'frontierIndex
        #== pfromData preverseScan'cursor
        #&& pverifyMembership
        # pfromData pauthenticatedRedeemer'purposeCount
        # pfromData pauthenticatedRedeemer'purposePeaks
        # pfromData pselectionOpening'frontierIndex
        # purposeLeaf
        # pfromData pselectionOpening'purposeSiblings
        #&& pverifyMembership
        # pfromData pauthenticatedRedeemer'executionCount
        # pfromData pauthenticatedRedeemer'executionPeaks
        # pfromData pselectionOpening'frontierIndex
        # executionLeaf
        # pfromData pselectionOpening'executionSiblings
    )
    expected
    perror

preverseScanCompleteV1 :: forall s. Term s (PReverseScanV1 :--> PBool)
preverseScanCompleteV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \PReverseScanV1{preverseScan'authenticated, preverseScan'cursor, preverseScan'used} ->
    pmatch (pfromData preverseScan'authenticated) $ \PAuthenticatedRedeemerV1{pauthenticatedRedeemer'purposeCount} ->
      pfromData preverseScan'used #|| pfromData preverseScan'cursor #== pfromData pauthenticatedRedeemer'purposeCount

pdecisionV1 :: forall s. Term s (PReverseScanV1 :--> PDecisionV1)
pdecisionV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \PReverseScanV1{preverseScan'authenticated, preverseScan'used} ->
    pmatch (pfromData preverseScan'authenticated) $ \PAuthenticatedRedeemerV1{pauthenticatedRedeemer'bound} ->
      pmatch (pfromData pauthenticatedRedeemer'bound) $ \PBoundRedeemerV1{pboundRedeemer'subject, pboundRedeemer'redeemerIndex} ->
        pif
          (preverseScanCompleteV1 # state)
          (pcon $ PDecisionV1 pboundRedeemer'subject pboundRedeemer'redeemerIndex (pdata $ pnot # pfromData preverseScan'used))
          perror

pterminalContradictionV1 :: forall s. Term s (PDecisionV1 :--> PBool)
pterminalContradictionV1 = phoistAcyclic $ plam $ \decision ->
  pmatch decision $ \PDecisionV1{pdecision'subject, pdecision'unused} ->
    Subject.pterminalContradiction # pfromData pdecision'subject # pfromData pdecision'unused

pencodeReverseScanV1 :: forall s. Term s (PReverseScanV1 :--> PByteString)
pencodeReverseScanV1 = phoistAcyclic $ plam $ \state -> pserialiseData # pforgetData (pdata state)

peventKeyMatchesSubject :: forall s. Term s PEventKey -> Term s Subject.PVerdictSubject -> Term s PBool
peventKeyMatchesSubject eventKey subject =
  pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'sourceKind, Subject.psubject'transactionId, Subject.psubject'sourceKey} ->
    pmatch eventKey $ \case
      PL2TransactionEventKey txId -> pfromData psubject'sourceKind #== 0 #&& pfromData txId #== pfromData psubject'transactionId
      PForcedTransactionEventKey txOrderId -> pfromData psubject'sourceKind #== 1 #&& pserialiseData # pforgetData txOrderId #== pfromData psubject'sourceKey
      _ -> pconstant False

pdescriptorMatchesSubject :: forall s. Term s PValidationTraceDescriptorV1 -> Term s Subject.PVerdictSubject -> Term s PBool
pdescriptorMatchesSubject descriptor subject =
  pmatch descriptor $ \PValidationTraceDescriptorV1{pdescriptor'verdict, pdescriptor'rejectionCodeHash} ->
    pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
      pif
        (pfromData psubject'direction #== 0)
        (pfromData pdescriptor'verdict #== pcon PAccepted #&& pfromData pdescriptor'rejectionCodeHash #== pconstant (BS.replicate 32 0))
        ( pfromData pdescriptor'verdict
            #== pcon PRejected
            #&& pfromData pdescriptor'rejectionCodeHash
            #== phashRejectionCode
            # (prejectionCodeOf # pforgetData (pdata $ Subject.prejectionReasonOf # subject))
        )

pchunkMatches :: forall s. Term s (PInteger :--> PInteger :--> PByteString :--> PInteger :--> PChunkProofV1 :--> PBool)
pchunkMatches = phoistAcyclic $ plam $ \itemIndex totalLength commitment chunkIndex proof ->
  pmatch proof $ \PChunkProofV1{..} ->
    pfromData pchunkProof'fieldIndex
      #== 8
      #&& pfromData pchunkProof'itemIndex
      #== itemIndex
      #&& pfromData pchunkProof'totalLength
      #== totalLength
      #&& pfromData pchunkProof'chunkIndex
      #== chunkIndex
      #&& Bounded.pverifyChunk
      # commitment
      # proof

popenSpan :: forall s. Term s (PInteger :--> PInteger :--> PByteString :--> PInteger :--> PInteger :--> PChunkProofV1 :--> PMaybeData PChunkProofV1 :--> PByteString)
popenSpan = phoistAcyclic $ plam $ \itemIndex totalLength commitment start length first second ->
  plet (pdiv # start # Bounded.pchunkBytes) $ \firstIndex ->
    plet (pdiv # (start + length - 1) # Bounded.pchunkBytes) $ \lastIndex ->
      pif
        ( length
            #> 0
            #&& start
            #>= 0
            #&& start
            + length
              #<= totalLength
              #&& lastIndex
              #<= firstIndex
            + 1
              #&& pchunkMatches
              # itemIndex
              # totalLength
              # commitment
              # firstIndex
              # first
        )
        ( pmatch first $ \PChunkProofV1{pchunkProof'chunk} ->
            let bytes =
                  pif
                    (lastIndex #== firstIndex)
                    (pmatch second $ \case PDNothing -> pfromData pchunkProof'chunk; PDJust _ -> perror)
                    ( pmatch second $ \case
                        PDNothing -> perror
                        PDJust nextData ->
                          let next = pfromData nextData
                           in pif
                                (pchunkMatches # itemIndex # totalLength # commitment # lastIndex # next)
                                (pmatch next $ \PChunkProofV1{pchunkProof'chunk = nextChunk} -> pfromData pchunkProof'chunk <> pfromData nextChunk)
                                perror
                    )
             in psliceBS # (start - firstIndex * Bounded.pchunkBytes) # length # bytes
        )
        perror

pexpectHead :: forall s. Term s PByteString -> Term s PInteger -> Term s PInteger -> Term s PCborHeadV1
pexpectHead bytes offset major = pmatch (pheadAtV1 # bytes # offset # major) $ \case
  PNothing -> perror
  PJust head' -> head'

pheadOffset :: forall s. Term s PCborHeadV1 -> Term s PInteger
pheadOffset head' = pmatch head' $ \PCborHeadV1{pcborHead'nextOffset} -> pcborHead'nextOffset

pheadValue :: forall s. Term s PCborHeadV1 -> Term s PInteger
pheadValue head' = pmatch head' $ \PCborHeadV1{pcborHead'value} -> pcborHead'value

pbit :: forall s. Term s (PInteger :--> PInteger)
pbit = phoistAcyclic $ pfix $ \self -> plam $ \index -> pif (index #== 0) 1 (2 * (self # (index - 1)))
