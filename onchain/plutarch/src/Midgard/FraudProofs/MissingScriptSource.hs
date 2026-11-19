{-# LANGUAGE OverloadedStrings #-}

module Midgard.FraudProofs.MissingScriptSource (
  PBoundPurposeV1 (..),
  PAuthenticatedPurposeV1 (..),
  PAuthenticatedTraceStateV1 (..),
  PAuthenticatedTransactionSourcesV1 (..),
  PAuthenticatedResolvedSourcesV1 (..),
  PSourceDescriptorV1 (..),
  PSourceScanStateV1 (..),
  PStep01Source (..),
  PStep01Args (..),
  PStep02Args (..),
  PStep03Args (..),
  PStep04Args (..),
  PStep05Args (..),
  PStep06Args (..),
  psourceLocationWitness,
  psourceLocationResolvedReference,
  pstagedSourceBudget,
  pbindPurposeV1,
  pauthenticateTraceStateV1,
  pauthenticatePurposeControlV1,
  pauthenticateTransactionSourcesV1,
  psourceIdentityHashV1,
  pauthenticateResolvedSourcesV1,
  pcheckpointV1,
  pinitialScanV1,
  pstateIsAuthenticV1,
  pscanSourceV1,
  padvanceScanV1,
  pscanCompleteV1,
  pterminalContradictionV1,
) where

import Data.ByteString qualified as BS
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.NativeTx.Codec (pencodeDefiniteBytes)
import Midgard.FraudProofs.NativeTx.Compact (pnativeTxProofCommitmentV1)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PEventKey (..), PHeaderV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PScriptSourceMissing), prejectionCodeOf)
import Midgard.ScriptProof (ppurposeLeafHash, psourceDescriptorLeafHash)
import Midgard.TransitionTrace (PRootDomain (PValidationTracesRootDomain), PRootMembershipProof (..), pverifyRootMembershipWithBytes)
import Midgard.ValidationMachine (PScriptDiscoveryControlV1 (..), PScriptSourcesControlV1 (..), pencodeScriptSourcesDiscoveryWitness)
import Midgard.ValidationMerkle (PFrontierPeak, pfrontierIsWellFormed, pverifyMembership, pverifyMembershipFromWellFormed)
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase (PScriptSources),
  PValidationSourceKind (PForced, PNormal),
  PValidationTraceDescriptorV1 (..),
  PValidationTraceProof (..),
  PValidationVerdict (PAccepted, PPending, PRejected),
  phashMachineState,
  phashRejectionCode,
  phashValidationContext,
  phashWorkWitness,
  pmachineStateIsWellFormed,
  pverifyTraceProof,
 )

data PBoundPurposeV1 (s :: S) = PBoundPurposeV1
  { pboundPurpose'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pboundPurpose'validationTracesRoot :: Term s (PAsData PByteString)
  , pboundPurpose'validationTraceCount :: Term s (PAsData PInteger)
  , pboundPurpose'witnessSetHash :: Term s (PAsData PByteString)
  , pboundPurpose'purposeKind :: Term s (PAsData PInteger)
  , pboundPurpose'purposeIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBoundPurposeV1)

data PAuthenticatedPurposeV1 (s :: S) = PAuthenticatedPurposeV1
  { pauthenticatedPurpose'bound :: Term s (PAsData PBoundPurposeV1)
  , pauthenticatedPurpose'priorLedgerRoot :: Term s (PAsData PByteString)
  , pauthenticatedPurpose'requiredScriptHash :: Term s (PAsData PByteString)
  , pauthenticatedPurpose'sourceCount :: Term s (PAsData PInteger)
  , pauthenticatedPurpose'scanLimit :: Term s (PAsData PInteger)
  , pauthenticatedPurpose'sourcePeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedPurposeV1)

data PAuthenticatedTraceStateV1 (s :: S) = PAuthenticatedTraceStateV1
  { pauthenticatedTrace'bound :: Term s (PAsData PBoundPurposeV1)
  , pauthenticatedTrace'machineState :: Term s (PAsData PValidationMachineStateV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedTraceStateV1)

data PAuthenticatedTransactionSourcesV1 (s :: S) = PAuthenticatedTransactionSourcesV1
  { pauthenticatedTransaction'purpose :: Term s (PAsData PAuthenticatedPurposeV1)
  , pauthenticatedTransaction'sourceCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedTransactionSourcesV1)

data PAuthenticatedResolvedSourcesV1 (s :: S) = PAuthenticatedResolvedSourcesV1
  { pauthenticatedResolved'purpose :: Term s (PAsData PAuthenticatedPurposeV1)
  , pauthenticatedResolved'transactionSourceCount :: Term s (PAsData PInteger)
  , pauthenticatedResolved'resolvedReferenceSourceCount :: Term s (PAsData PInteger)
  , pauthenticatedResolved'sourceIdentityHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedResolvedSourcesV1)

data PSourceDescriptorV1 (s :: S) = PSourceDescriptorV1
  { psourceDescriptor'sourceIndex :: Term s (PAsData PInteger)
  , psourceDescriptor'locationKind :: Term s (PAsData PInteger)
  , psourceDescriptor'sourceKey :: Term s (PAsData PByteString)
  , psourceDescriptor'languageTag :: Term s (PAsData PInteger)
  , psourceDescriptor'scriptHash :: Term s (PAsData PByteString)
  , psourceDescriptor'totalLength :: Term s (PAsData PInteger)
  , psourceDescriptor'itemCommitment :: Term s (PAsData PByteString)
  , psourceDescriptor'siblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSourceDescriptorV1)

data PSourceScanStateV1 (s :: S) = PSourceScanStateV1
  { psourceScan'authenticated :: Term s (PAsData PAuthenticatedResolvedSourcesV1)
  , psourceScan'cursor :: Term s (PAsData PInteger)
  , psourceScan'found :: Term s (PAsData PBool)
  , psourceScan'nextExpectedScriptHash :: Term s (PAsData PByteString)
  , psourceScan'checkpointHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSourceScanStateV1)

data PStep01Source (s :: S)
  = PAcceptedSource (Term s (PAsData PNativeTxInclusionCarriage))
  | PForcedSource (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PHeaderV1)) (Term s (PAsData PRootMembershipProof)) (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Source)

data PStep01Args (s :: S) = PStep01Args
  { pstep01Args'source :: Term s (PAsData PStep01Source)
  , pstep01Args'purposeKind :: Term s (PAsData PInteger)
  , pstep01Args'purposeIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'traceMembership :: Term s (PAsData PRootMembershipProof)
  , pstep02Args'machineState :: Term s (PAsData PValidationMachineStateV1)
  , pstep02Args'traceProof :: Term s (PAsData PValidationTraceProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  , pstep03Args'control :: Term s (PAsData PScriptSourcesControlV1)
  , pstep03Args'absolutePurposeIndex :: Term s (PAsData PInteger)
  , pstep03Args'requiredScriptHash :: Term s (PAsData PByteString)
  , pstep03Args'purposeSubject :: Term s (PAsData PByteString)
  , pstep03Args'purposeSiblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , pstep03Args'transactionSourceCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , pstep04Args'resolvedReferenceSourceCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

data PStep05Args (s :: S) = PStep05Args
  { pstep05Args'inputIndex :: Term s (PAsData PInteger)
  , pstep05Args'outputIndex :: Term s (PAsData PInteger)
  , pstep05Args'sources :: Term s (PAsData (PBuiltinList (PAsData PSourceDescriptorV1)))
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

psourceLocationWitness, psourceLocationResolvedReference, pstagedSourceBudget :: forall s. Term s PInteger
psourceLocationWitness = 0
psourceLocationResolvedReference = 1
pstagedSourceBudget = 24

pcheckpointDomain :: forall s. Term s PByteString
pcheckpointDomain = pconstant "midgard/fraud-proofs/missing-script-source/checkpoint-v1"

pbindPurposeV1 :: forall s. Term s (Subject.PVerdictSubject :--> PByteString :--> PInteger :--> PByteString :--> PInteger :--> PInteger :--> PBoundPurposeV1)
pbindPurposeV1 = phoistAcyclic $ plam $ \subject traceRoot traceCount witnessSetHash purposeKind purposeIndex ->
  pif
    ( Subject.psubjectIsCanonical
        # subject
        #&& plengthBS
        # traceRoot
        #== 32
        #&& traceCount
        #> 0
        #&& plengthBS
        # witnessSetHash
        #== 32
        #&& purposeKind
        #>= 0
        #&& purposeKind
        #<= 3
        #&& purposeIndex
        #>= 0
    )
    ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
        pif
          (pfromData psubject'direction #== 1)
          ( plet (Subject.prejectionReasonOf # subject) $ \reason ->
              pmatch reason $ \case
                PScriptSourceMissing kind index ->
                  pif
                    (pfromData kind #== purposeKind #&& pfromData index #== purposeIndex)
                    (pbound subject traceRoot traceCount witnessSetHash purposeKind purposeIndex)
                    perror
                _ -> perror
          )
          (pbound subject traceRoot traceCount witnessSetHash purposeKind purposeIndex)
    )
    perror

pauthenticateTraceStateV1 :: forall s. Term s (PBoundPurposeV1 :--> PRootMembershipProof :--> PValidationMachineStateV1 :--> PValidationTraceProof :--> PAuthenticatedTraceStateV1)
pauthenticateTraceStateV1 = phoistAcyclic $ plam $ \bound membership machineState traceProof -> P.do
  PBoundPurposeV1{..} <- pmatch bound
  subject <- plet $ pfromData pboundPurpose'subject
  PRootMembershipProof{prootMembership'key, prootMembership'value} <- pmatch membership
  eventKey <- plet $ pcoerceData @PEventKey prootMembership'key
  descriptor <- plet $ pcoerceData @PValidationTraceDescriptorV1 prootMembership'value
  PValidationMachineStateV1{..} <- pmatch machineState
  PValidationTraceProof{ptraceProof'stateHash} <- pmatch traceProof
  let expected = pcon $ PAuthenticatedTraceStateV1 (pdata bound) (pdata machineState)
      subjectBranch = pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'sourceKind} ->
        pif
          (pfromData psubject'sourceKind #== 0)
          (pfromData pmachineState'sourceKind #== pcon PNormal #&& pdescriptorAccepted descriptor)
          (pfromData pmachineState'sourceKind #== pcon PForced #&& pdescriptorRejectsSubject descriptor subject)
  pif
    ( pand'List
        [ peventKeyMatchesSubject eventKey subject
        , pfromData pmachineState'eventKeyHash #== pblake2b_256 # (pserialiseData # prootMembership'key)
        , pverifyRootMembershipWithBytes membership (pdata $ pcon PValidationTracesRootDomain) (pfromData pboundPurpose'validationTracesRoot) (pfromData pboundPurpose'validationTraceCount) (pserialiseData # prootMembership'key) (pserialiseData # prootMembership'value)
        , pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'transactionId} -> pfromData pmachineState'transactionId #== pfromData psubject'transactionId
        , subjectBranch
        , pmachineStateIsWellFormed # machineState
        , pfromData pmachineState'phase #== pcon PScriptSources
        , pfromData pmachineState'verdict #== pcon PPending
        , pfromData pmachineState'rejectionCodeHash #== pconstant (BS.replicate 32 0)
        , pfromData ptraceProof'stateHash #== phashMachineState # machineState
        , pverifyTraceProof # descriptor # traceProof
        ]
    )
    expected
    perror

pauthenticatePurposeControlV1 :: forall s. Term s (PAuthenticatedTraceStateV1 :--> PScriptSourcesControlV1 :--> PInteger :--> PByteString :--> PByteString :--> PBuiltinList (PAsData PByteString) :--> PAuthenticatedPurposeV1)
pauthenticatePurposeControlV1 = phoistAcyclic $ plam $ \authenticatedTrace control absolutePurposeIndex requiredScriptHash purposeSubject purposeSiblings -> P.do
  PAuthenticatedTraceStateV1{..} <- pmatch authenticatedTrace
  bound <- plet $ pfromData pauthenticatedTrace'bound
  machineState <- plet $ pfromData pauthenticatedTrace'machineState
  PBoundPurposeV1{pboundPurpose'subject, pboundPurpose'witnessSetHash, pboundPurpose'purposeKind, pboundPurpose'purposeIndex} <- pmatch bound
  subject <- plet $ pfromData pboundPurpose'subject
  PValidationMachineStateV1{pmachineState'transactionCommitment, pmachineState'validationContextHash, pmachineState'workRoot, pmachineState'programCounter, pmachineState'priorLedgerRoot} <- pmatch machineState
  PScriptSourcesControlV1{..} <- pmatch control
  discovery <- plet $ pfromData pscriptSources'discovery
  PScriptDiscoveryControlV1{..} <- pmatch discovery
  purposeLeaf <- plet $ ppurposeLeafHash # pfromData pboundPurpose'purposeKind # pfromData pboundPurpose'purposeIndex # requiredScriptHash # purposeSubject
  scanLimit <- plet $ pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} -> pif (pfromData psubject'direction #== 0) (pfromData pscriptSources'sourceCount) (pfromData pscriptDiscovery'sourceCursor + 1)
  let expected = pcon $ PAuthenticatedPurposeV1 (pdata bound) pmachineState'priorLedgerRoot (pdata requiredScriptHash) pscriptSources'sourceCount (pdata scanLimit) pscriptSources'sourcePeaks
      cursorValid = pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
        pif
          (pfromData psubject'direction #== 0)
          (pfromData pscriptDiscovery'sourceCursor #== pfromData pscriptSources'sourceCount)
          (pfromData pscriptDiscovery'sourceCursor #>= 0 #&& pfromData pscriptDiscovery'sourceCursor #< pfromData pscriptSources'sourceCount)
  pif
    ( pand'List
        [ pfromData pmachineState'transactionCommitment #== pnativeTxProofCommitmentV1 # pfromData pscriptSources'compactCbor # pfromData pscriptSources'witnessSetCompactCbor # pfromData pscriptSources'fieldPreimageLengthsCbor
        , pblake2b_256 # pfromData pscriptSources'witnessSetCompactCbor #== pfromData pboundPurpose'witnessSetHash
        , pfromData pmachineState'validationContextHash #== phashValidationContext # pfromData pscriptSources'contextCbor
        , pfromData pmachineState'workRoot #== phashWorkWitness # pcon PScriptSources # pfromData pmachineState'programCounter # (pencodeScriptSourcesDiscoveryWitness # control # 9 # discovery)
        , absolutePurposeIndex #>= 0
        , absolutePurposeIndex #== pfromData pscriptDiscovery'purposeCursor
        , absolutePurposeIndex #< pfromData pscriptSources'purposeCount
        , pverifyMembership # pfromData pscriptSources'purposeCount # pfromData pscriptSources'purposePeaks # absolutePurposeIndex # purposeLeaf # purposeSiblings
        , plengthBS # requiredScriptHash #== 28
        , pfromData pscriptSources'stage #== 9
        , pfromData pscriptSources'sourceTotalCount #== pfromData pscriptSources'sourceCount
        , pfrontierIsWellFormed # pfromData pscriptSources'sourceCount # pfromData pscriptSources'sourcePeaks
        , pfrontierIsWellFormed # pfromData pscriptSources'purposeCount # pfromData pscriptSources'purposePeaks
        , pfromData pscriptDiscovery'currentPurposeKind #== pfromData pboundPurpose'purposeKind
        , pfromData pscriptDiscovery'currentPurposeIndex #== pfromData pboundPurpose'purposeIndex
        , pfromData pscriptDiscovery'currentScriptHash #== requiredScriptHash
        , pfromData pscriptDiscovery'currentSubject #== purposeSubject
        , cursorValid
        , pfromData pscriptDiscovery'matchedSourceIndex #== (-1)
        , pfromData pscriptSources'pendingSourceCbor #== pconstant ""
        , pfromData pscriptSources'outputProof #== pcon PDNothing
        ]
    )
    expected
    perror

pauthenticateTransactionSourcesV1 :: forall s. Term s (PAuthenticatedPurposeV1 :--> PInteger :--> PAuthenticatedTransactionSourcesV1)
pauthenticateTransactionSourcesV1 = phoistAcyclic $ plam $ \purpose transactionSourceCount ->
  pmatch purpose $ \PAuthenticatedPurposeV1{pauthenticatedPurpose'scanLimit} ->
    pif
      (transactionSourceCount #>= 0 #&& transactionSourceCount #<= pfromData pauthenticatedPurpose'scanLimit)
      (pcon $ PAuthenticatedTransactionSourcesV1 (pdata purpose) (pdata transactionSourceCount))
      perror

psourceIdentityHashV1 :: forall s. Term s (PAuthenticatedPurposeV1 :--> PInteger :--> PInteger :--> PByteString)
psourceIdentityHashV1 = phoistAcyclic $ plam $ \purpose transactionCount resolvedCount ->
  pmatch purpose $ \PAuthenticatedPurposeV1{..} ->
    pblake2b_256
      # ( pcheckpointDomain
            <> pfromData pauthenticatedPurpose'priorLedgerRoot
            <> (pserialiseData # pforgetData pauthenticatedPurpose'sourceCount)
            <> (pserialiseData # pforgetData pauthenticatedPurpose'scanLimit)
            <> (pserialiseData # pforgetData pauthenticatedPurpose'sourcePeaks)
            <> (pserialiseData # pforgetData (pdata transactionCount))
            <> (pserialiseData # pforgetData (pdata resolvedCount))
        )

pauthenticateResolvedSourcesV1 :: forall s. Term s (PAuthenticatedTransactionSourcesV1 :--> PInteger :--> PAuthenticatedResolvedSourcesV1)
pauthenticateResolvedSourcesV1 = phoistAcyclic $ plam $ \staged resolvedCount ->
  pmatch staged $ \PAuthenticatedTransactionSourcesV1{..} ->
    plet (pfromData pauthenticatedTransaction'purpose) $ \purpose ->
      pmatch purpose $ \PAuthenticatedPurposeV1{pauthenticatedPurpose'scanLimit} ->
        pif
          (resolvedCount #>= 0 #&& pfromData pauthenticatedTransaction'sourceCount + resolvedCount #== pfromData pauthenticatedPurpose'scanLimit)
          (pcon $ PAuthenticatedResolvedSourcesV1 (pdata purpose) pauthenticatedTransaction'sourceCount (pdata resolvedCount) (pdata $ psourceIdentityHashV1 # purpose # pfromData pauthenticatedTransaction'sourceCount # resolvedCount))
          perror

pcheckpointV1 :: forall s. Term s (PAuthenticatedResolvedSourcesV1 :--> PInteger :--> PBool :--> PByteString :--> PByteString)
pcheckpointV1 = phoistAcyclic $ plam $ \authenticated cursor found nextHash ->
  pmatch authenticated $ \PAuthenticatedResolvedSourcesV1{pauthenticatedResolved'sourceIdentityHash} ->
    pblake2b_256
      # ( pcheckpointDomain
            <> pfromData pauthenticatedResolved'sourceIdentityHash
            <> (pserialiseData # pforgetData (pdata cursor))
            <> (pserialiseData # pforgetData (pdata found))
            <> (pencodeDefiniteBytes # nextHash)
        )

pinitialScanV1 :: forall s. Term s (PAuthenticatedResolvedSourcesV1 :--> PByteString :--> PSourceScanStateV1)
pinitialScanV1 = phoistAcyclic $ plam $ \authenticated scanHash ->
  pcon $ PSourceScanStateV1 (pdata authenticated) (pdata 0) (pdata $ pconstant False) (pdata scanHash) (pdata $ pcheckpointV1 # authenticated # 0 # pconstant False # scanHash)

pstateIsAuthenticV1 :: forall s. Term s (PSourceScanStateV1 :--> PBool)
pstateIsAuthenticV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \PSourceScanStateV1{..} ->
    plet (pfromData psourceScan'authenticated) $ \authenticated ->
      pmatch authenticated $ \PAuthenticatedResolvedSourcesV1{pauthenticatedResolved'purpose, pauthenticatedResolved'sourceIdentityHash} ->
        pmatch (pfromData pauthenticatedResolved'purpose) $ \PAuthenticatedPurposeV1{pauthenticatedPurpose'scanLimit} ->
          pand'List
            [ plengthBS # pfromData pauthenticatedResolved'sourceIdentityHash #== 32
            , plengthBS # pfromData psourceScan'nextExpectedScriptHash #== 28
            , pfromData psourceScan'cursor #>= 0
            , pfromData psourceScan'cursor #<= pfromData pauthenticatedPurpose'scanLimit
            , pfromData psourceScan'checkpointHash #== pcheckpointV1 # authenticated # pfromData psourceScan'cursor # pfromData psourceScan'found # pfromData psourceScan'nextExpectedScriptHash
            ]

pscanSourceV1 :: forall s. Term s (PSourceScanStateV1 :--> PSourceDescriptorV1 :--> PByteString :--> PSourceScanStateV1)
pscanSourceV1 = phoistAcyclic $ plam $ \state descriptor nextHash ->
  pif
    (pstateIsAuthenticV1 # state)
    (pscanSourceUncheckedV1 # pconstant False # state # descriptor # nextHash)
    perror

pscanSourceUncheckedV1 :: forall s. Term s (PBool :--> PSourceScanStateV1 :--> PSourceDescriptorV1 :--> PByteString :--> PSourceScanStateV1)
pscanSourceUncheckedV1 = phoistAcyclic $ plam $ \frontierAuthenticated state descriptor nextHash -> P.do
  PSourceScanStateV1{..} <- pmatch state
  authenticated <- plet $ pfromData psourceScan'authenticated
  PAuthenticatedResolvedSourcesV1{pauthenticatedResolved'purpose, pauthenticatedResolved'transactionSourceCount} <- pmatch authenticated
  purpose <- plet $ pfromData pauthenticatedResolved'purpose
  PAuthenticatedPurposeV1{..} <- pmatch purpose
  PSourceDescriptorV1{..} <- pmatch descriptor
  let cursor = pfromData psourceScan'cursor
      expectedLocation = pif (cursor #< pfromData pauthenticatedResolved'transactionSourceCount) psourceLocationWitness psourceLocationResolvedReference
      locationOrigin = pif (pfromData psourceDescriptor'locationKind #== psourceLocationWitness) 0 1
      leaf = psourceDescriptorLeafHash # locationOrigin # pfromData psourceDescriptor'sourceKey # pfromData psourceDescriptor'languageTag # pfromData psourceDescriptor'scriptHash # pfromData psourceDescriptor'totalLength # pfromData psourceDescriptor'itemCommitment
      nextCursor = cursor + 1
      found = pfromData psourceScan'found #|| pfromData psourceDescriptor'scriptHash #== pfromData pauthenticatedPurpose'requiredScriptHash
      expected = pcon $ PSourceScanStateV1 psourceScan'authenticated (pdata nextCursor) (pdata found) (pdata nextHash) (pdata $ pcheckpointV1 # authenticated # nextCursor # found # nextHash)
      membership =
        pif
          frontierAuthenticated
          (pverifyMembershipFromWellFormed # pfromData pauthenticatedPurpose'sourceCount # pfromData pauthenticatedPurpose'sourcePeaks # pfromData psourceDescriptor'sourceIndex # leaf # pfromData psourceDescriptor'siblings)
          (pverifyMembership # pfromData pauthenticatedPurpose'sourceCount # pfromData pauthenticatedPurpose'sourcePeaks # pfromData psourceDescriptor'sourceIndex # leaf # pfromData psourceDescriptor'siblings)
  pif
    ( pand'List
        [ cursor #< pfromData pauthenticatedPurpose'scanLimit
        , pfromData psourceDescriptor'sourceIndex #== cursor
        , pfromData psourceDescriptor'locationKind #== expectedLocation
        , pfromData psourceDescriptor'totalLength #> 0
        , plengthBS # pfromData psourceDescriptor'scriptHash #== 28
        , plengthBS # pfromData psourceDescriptor'itemCommitment #== 32
        , membership
        ]
    )
    expected
    perror

padvanceScanUncheckedV1 :: forall s. Term s (PSourceScanStateV1 :--> PBuiltinList (PAsData PSourceDescriptorV1) :--> PByteString :--> PByteString :--> PSourceScanStateV1)
padvanceScanUncheckedV1 = phoistAcyclic $ pfix $ \self -> plam $ \state sources scanHash finalHash ->
  pelimList
    ( \source rest ->
        pmatch state $ \PSourceScanStateV1{psourceScan'authenticated, psourceScan'cursor} ->
          pmatch (pfromData psourceScan'authenticated) $ \PAuthenticatedResolvedSourcesV1{pauthenticatedResolved'purpose} ->
            pmatch (pfromData pauthenticatedResolved'purpose) $ \PAuthenticatedPurposeV1{pauthenticatedPurpose'scanLimit} ->
              let nextCursor = pfromData psourceScan'cursor + 1
                  nextHash = pif (nextCursor #== pfromData pauthenticatedPurpose'scanLimit) finalHash scanHash
               in self # (pscanSourceUncheckedV1 # pconstant True # state # pfromData source # nextHash) # rest # scanHash # finalHash
    )
    state
    sources

psourceFrontierIsWellFormedV1 :: forall s. Term s (PSourceScanStateV1 :--> PBool)
psourceFrontierIsWellFormedV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \PSourceScanStateV1{psourceScan'authenticated} ->
    pmatch (pfromData psourceScan'authenticated) $ \PAuthenticatedResolvedSourcesV1{pauthenticatedResolved'purpose} ->
      pmatch (pfromData pauthenticatedResolved'purpose) $ \PAuthenticatedPurposeV1{pauthenticatedPurpose'sourceCount, pauthenticatedPurpose'sourcePeaks} ->
        pfrontierIsWellFormed # pfromData pauthenticatedPurpose'sourceCount # pfromData pauthenticatedPurpose'sourcePeaks

padvanceScanV1 :: forall s. Term s (PSourceScanStateV1 :--> PBuiltinList (PAsData PSourceDescriptorV1) :--> PByteString :--> PByteString :--> PSourceScanStateV1)
padvanceScanV1 = phoistAcyclic $ plam $ \state sources scanHash finalHash ->
  pelimList
    ( \_ _ ->
        pif
          (pstateIsAuthenticV1 # state #&& psourceFrontierIsWellFormedV1 # state)
          (padvanceScanUncheckedV1 # state # sources # scanHash # finalHash)
          perror
    )
    state
    sources

pscanCompleteV1 :: forall s. Term s (PSourceScanStateV1 :--> PBool)
pscanCompleteV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \PSourceScanStateV1{psourceScan'authenticated, psourceScan'cursor} ->
    pmatch (pfromData psourceScan'authenticated) $ \PAuthenticatedResolvedSourcesV1{pauthenticatedResolved'purpose} ->
      pmatch (pfromData pauthenticatedResolved'purpose) $ \PAuthenticatedPurposeV1{pauthenticatedPurpose'scanLimit} ->
        pstateIsAuthenticV1 # state #&& pfromData psourceScan'cursor #== pfromData pauthenticatedPurpose'scanLimit

pterminalContradictionV1 :: forall s. Term s (PSourceScanStateV1 :--> PBool)
pterminalContradictionV1 = phoistAcyclic $ plam $ \state ->
  pif
    (pscanCompleteV1 # state)
    ( pmatch state $ \PSourceScanStateV1{psourceScan'authenticated, psourceScan'found} ->
        pmatch (pfromData psourceScan'authenticated) $ \PAuthenticatedResolvedSourcesV1{pauthenticatedResolved'purpose} ->
          pmatch (pfromData pauthenticatedResolved'purpose) $ \PAuthenticatedPurposeV1{pauthenticatedPurpose'bound} ->
            pmatch (pfromData pauthenticatedPurpose'bound) $ \PBoundPurposeV1{pboundPurpose'subject} ->
              Subject.pterminalContradiction # pfromData pboundPurpose'subject # (pnot # pfromData psourceScan'found)
    )
    perror

pbound :: forall s. Term s Subject.PVerdictSubject -> Term s PByteString -> Term s PInteger -> Term s PByteString -> Term s PInteger -> Term s PInteger -> Term s PBoundPurposeV1
pbound subject traceRoot traceCount witnessSetHash purposeKind purposeIndex = pcon $ PBoundPurposeV1 (pdata subject) (pdata traceRoot) (pdata traceCount) (pdata witnessSetHash) (pdata purposeKind) (pdata purposeIndex)

peventKeyMatchesSubject :: forall s. Term s PEventKey -> Term s Subject.PVerdictSubject -> Term s PBool
peventKeyMatchesSubject eventKey subject =
  pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'sourceKind, Subject.psubject'transactionId, Subject.psubject'sourceKey} ->
    pmatch eventKey $ \case
      PL2TransactionEventKey txId -> pfromData psubject'sourceKind #== 0 #&& pfromData txId #== pfromData psubject'transactionId
      PForcedTransactionEventKey txOrderId -> pfromData psubject'sourceKind #== 1 #&& pserialiseData # pforgetData txOrderId #== pfromData psubject'sourceKey
      _ -> pconstant False

pdescriptorAccepted :: forall s. Term s PValidationTraceDescriptorV1 -> Term s PBool
pdescriptorAccepted descriptor = pmatch descriptor $ \PValidationTraceDescriptorV1{pdescriptor'verdict} -> pfromData pdescriptor'verdict #== pcon PAccepted

pdescriptorRejectsSubject :: forall s. Term s PValidationTraceDescriptorV1 -> Term s Subject.PVerdictSubject -> Term s PBool
pdescriptorRejectsSubject descriptor subject =
  pmatch descriptor $ \PValidationTraceDescriptorV1{pdescriptor'verdict, pdescriptor'rejectionCodeHash} ->
    pfromData pdescriptor'verdict
      #== pcon PRejected
      #&& pfromData
        pdescriptor'rejectionCodeHash
      #== phashRejectionCode
      # (prejectionCodeOf # pforgetData (pdata $ Subject.prejectionReasonOf # subject))

pcoerceData :: forall a s. (PIsData a) => Term s PData -> Term s a
pcoerceData = pfromData . punsafeCoerce
