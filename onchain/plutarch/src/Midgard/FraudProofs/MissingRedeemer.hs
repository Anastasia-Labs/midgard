{-# LANGUAGE OverloadedStrings #-}

-- | Exact missing-redeemer rule and seven-step ABI.
module Midgard.FraudProofs.MissingRedeemer (
  PBoundPurposeV1 (..),
  PAuthenticatedPurposeV1 (..),
  PAuthenticatedStageTenV1 (..),
  PAuthenticatedDescriptorV1 (..),
  PScanStateV1 (..),
  PDecisionStateV1 (..),
  PStep01Source (..),
  PStep01Args (..),
  PStep02Args (..),
  PStep02aArgs (..),
  PStep02bArgs (..),
  PAuthenticationStateV1 (..),
  PStep03ActionV1 (..),
  PStep04Args (..),
  PStep05Args (..),
  predeemerFieldIndex,
  pscanBatch,
  predeemerTagForPurposeKindV1,
  pisRedeemerBearingLanguageV1,
  pbindPurposeV1,
  pauthenticateDescriptorV1,
  pauthenticateStageTenTraceV1,
  pauthenticateStageTenSelectionV1,
  pinitialScanV1,
  ppointerFromItemV1,
  pscanItemV1,
  pdecisionV1,
  pterminalContradictionV1,
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

import Midgard.CanonicalCborScan (PCborHeadV1 (..), pheadAtV1)
import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.NativeTx.Compact (pnativeTxProofCommitmentV1)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PEventKey (..), PHeaderV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PRedeemerMissing), prejectionCodeOf)
import Midgard.ScriptProof (ppurposeLeafHash, psourceDescriptorLeafHash)
import Midgard.TransitionTrace (PRootDomain (PValidationTracesRootDomain), PRootMembershipProof (..), pverifyRootMembershipWithBytes)
import Midgard.ValidationMachine (PScriptDiscoveryControlV1 (..), PScriptSourcesControlV1 (..), pencodeScriptSourcesDiscoveryWitness)
import Midgard.ValidationMerkle (PFrontierPeak, pfrontierIsWellFormed, pverifyMembership)
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
  , pboundPurpose'witnessSetHash :: Term s (PAsData PByteString)
  , pboundPurpose'validationTracesRoot :: Term s (PAsData PByteString)
  , pboundPurpose'validationTraceCount :: Term s (PAsData PInteger)
  , pboundPurpose'purposeKind :: Term s (PAsData PInteger)
  , pboundPurpose'purposeIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBoundPurposeV1)

data PAuthenticatedPurposeV1 (s :: S) = PAuthenticatedPurposeV1
  { pauthenticatedPurpose'bound :: Term s (PAsData PBoundPurposeV1)
  , pauthenticatedPurpose'purposeCount :: Term s (PAsData PInteger)
  , pauthenticatedPurpose'redeemerTag :: Term s (PAsData PInteger)
  , pauthenticatedPurpose'requiredScriptHash :: Term s (PAsData PByteString)
  , pauthenticatedPurpose'sourceIndex :: Term s (PAsData PInteger)
  , pauthenticatedPurpose'sourceLanguageTag :: Term s (PAsData PInteger)
  , pauthenticatedPurpose'sourceLeaf :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedPurposeV1)

data PAuthenticatedStageTenV1 (s :: S) = PAuthenticatedStageTenV1
  { pauthenticatedStageTen'bound :: Term s (PAsData PBoundPurposeV1)
  , pauthenticatedStageTen'sourceCount :: Term s (PAsData PInteger)
  , pauthenticatedStageTen'sourcePeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pauthenticatedStageTen'purposeCount :: Term s (PAsData PInteger)
  , pauthenticatedStageTen'purposePeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pauthenticatedStageTen'discovery :: Term s (PAsData PScriptDiscoveryControlV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedStageTenV1)

data PAuthenticatedDescriptorV1 (s :: S) = PAuthenticatedDescriptorV1
  { pauthenticatedDescriptor'bound :: Term s (PAsData PBoundPurposeV1)
  , pauthenticatedDescriptor'eventKeyHash :: Term s (PAsData PByteString)
  , pauthenticatedDescriptor'descriptor :: Term s (PAsData PValidationTraceDescriptorV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedDescriptorV1)

data PScanStateV1 (s :: S) = PScanStateV1
  { pscanState'authenticated :: Term s (PAsData PAuthenticatedPurposeV1)
  , pscanState'checkpointHash :: Term s (PAsData PByteString)
  , pscanState'cursor :: Term s (PAsData PInteger)
  , pscanState'itemCount :: Term s (PAsData PInteger)
  , pscanState'found :: Term s (PAsData PBool)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScanStateV1)

data PDecisionStateV1 (s :: S) = PDecisionStateV1
  { pdecisionState'bound :: Term s (PAsData PBoundPurposeV1)
  , pdecisionState'redeemerMissing :: Term s (PAsData PBool)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDecisionStateV1)

data PStep01Source (s :: S)
  = PAcceptedSource (Term s (PAsData PNativeTxInclusionCarriage))
  | PForcedSource
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PHeaderV1))
      (Term s (PAsData PRootMembershipProof))
      (Term s (PAsData PInteger))
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
  , pstep02bArgs'absolutePurposeIndex :: Term s (PAsData PInteger)
  , pstep02bArgs'purposeSiblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , pstep02bArgs'sourceOriginKind :: Term s (PAsData PInteger)
  , pstep02bArgs'sourceKey :: Term s (PAsData PByteString)
  , pstep02bArgs'sourceLanguageTag :: Term s (PAsData PInteger)
  , pstep02bArgs'sourceScriptHash :: Term s (PAsData PByteString)
  , pstep02bArgs'sourceTotalLength :: Term s (PAsData PInteger)
  , pstep02bArgs'sourceItemCommitment :: Term s (PAsData PByteString)
  , pstep02bArgs'sourceSiblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02bArgs)

data PAuthenticationStateV1 (s :: S)
  = PReady (Term s (PAsData PAuthenticatedPurposeV1))
  | PGrammar (Term s (PAsData PAuthenticatedPurposeV1)) (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticationStateV1)

data PStep03ActionV1 (s :: S)
  = PAuthenticateDirect (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PFieldOpeningV1))
  | PStartGrammar (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PFieldOpeningV1)) (Term s (PAsData PInteger))
  | PResumeGrammar (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PFieldOpeningV1)) (Term s (PAsData PByteString)) (Term s (PAsData PInteger))
  | PFinishGrammar (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PFieldOpeningV1)) (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03ActionV1)

data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , pstep04Args'opening :: Term s (PAsData PFieldOpeningV1)
  , pstep04Args'checkpointBytes :: Term s (PAsData PByteString)
  , pstep04Args'itemBudget :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

data PStep05Args (s :: S) = PStep05Args
  { pstep05Args'inputIndex :: Term s (PAsData PInteger)
  , pstep05Args'outputIndex :: Term s (PAsData PInteger)
  , pstep05Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep05Args)

predeemerFieldIndex, pscanBatch :: forall s. Term s PInteger
predeemerFieldIndex = 8
pscanBatch = 16

predeemerTagForPurposeKindV1 :: forall s. Term s (PInteger :--> PMaybe PInteger)
predeemerTagForPurposeKindV1 = phoistAcyclic $ plam $ \kind ->
  pif (kind #== 0) (pcon $ PJust 0) $
    pif (kind #== 1) (pcon $ PJust 1) $
      pif (kind #== 2) (pcon $ PJust 3) $
        pif (kind #== 3) (pcon $ PJust 6) (pcon PNothing)

pisRedeemerBearingLanguageV1 :: forall s. Term s (PInteger :--> PBool)
pisRedeemerBearingLanguageV1 = phoistAcyclic $ plam $ \tag -> tag #== 3 #|| tag #== 128

pbindPurposeV1 :: forall s. Term s (Subject.PVerdictSubject :--> PByteString :--> PByteString :--> PInteger :--> PInteger :--> PInteger :--> PBoundPurposeV1)
pbindPurposeV1 = phoistAcyclic $ plam $ \subject witnessSetHash traceRoot traceCount purposeKind purposeIndex ->
  pif
    ( Subject.psubjectIsCanonical
        # subject
        #&& plengthBS
        # witnessSetHash
        #== 32
        #&& plengthBS
        # traceRoot
        #== 32
        #&& traceCount
        #> 0
        #&& purposeIndex
        #>= 0
    )
    ( pmatch (predeemerTagForPurposeKindV1 # purposeKind) $ \case
        PNothing -> perror
        PJust _ ->
          pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
            let bound = pcon $ PBoundPurposeV1 (pdata subject) (pdata witnessSetHash) (pdata traceRoot) (pdata traceCount) (pdata purposeKind) (pdata purposeIndex)
             in pif
                  (pfromData psubject'direction #== 1)
                  ( plet
                      (Subject.pbindExactRejectionReason # subject # pcon (PRedeemerMissing (pdata purposeKind) (pdata purposeIndex)))
                      $ \_ -> bound
                  )
                  bound
    )
    perror

pauthenticateDescriptorV1 :: forall s. Term s (PBoundPurposeV1 :--> PRootMembershipProof :--> PAuthenticatedDescriptorV1)
pauthenticateDescriptorV1 = phoistAcyclic $ plam $ \bound membership -> P.do
  PBoundPurposeV1{..} <- pmatch bound
  subject <- plet $ pfromData pboundPurpose'subject
  PRootMembershipProof{prootMembership'key, prootMembership'value} <- pmatch membership
  eventKey <- plet $ pcoerceData @PEventKey prootMembership'key
  descriptor <- plet $ pcoerceData @PValidationTraceDescriptorV1 prootMembership'value
  let expected = pcon $ PAuthenticatedDescriptorV1 (pdata bound) (pdata $ pblake2b_256 # (pserialiseData # prootMembership'key)) (pdata descriptor)
  pif
    ( peventKeyMatchesSubject eventKey subject
        #&& pdescriptorVerdictMatchesSubject descriptor subject
        #&& pverifyRootMembershipWithBytes
          membership
          (pdata $ pcon PValidationTracesRootDomain)
          (pfromData pboundPurpose'validationTracesRoot)
          (pfromData pboundPurpose'validationTraceCount)
          (pserialiseData # prootMembership'key)
          (pserialiseData # prootMembership'value)
    )
    expected
    perror

pauthenticateStageTenTraceV1 :: forall s. Term s (PAuthenticatedDescriptorV1 :--> PValidationMachineStateV1 :--> PValidationTraceProof :--> PScriptSourcesControlV1 :--> PAuthenticatedStageTenV1)
pauthenticateStageTenTraceV1 = phoistAcyclic $ plam $ \authenticatedDescriptor machineState traceProof control -> P.do
  PAuthenticatedDescriptorV1{..} <- pmatch authenticatedDescriptor
  bound <- plet $ pfromData pauthenticatedDescriptor'bound
  descriptor <- plet $ pfromData pauthenticatedDescriptor'descriptor
  PBoundPurposeV1{pboundPurpose'subject, pboundPurpose'witnessSetHash} <- pmatch bound
  subject <- plet $ pfromData pboundPurpose'subject
  Subject.PVerdictSubject{Subject.psubject'transactionId, Subject.psubject'sourceKind} <- pmatch subject
  PValidationMachineStateV1{..} <- pmatch machineState
  PValidationTraceProof{ptraceProof'stateHash} <- pmatch traceProof
  PScriptSourcesControlV1{..} <- pmatch control
  discovery <- plet $ pfromData pscriptSources'discovery
  PScriptDiscoveryControlV1{..} <- pmatch discovery
  let expectedSourceKind = pif (pfromData psubject'sourceKind #== 0) (pcon PNormal) (pcon PForced)
      expected = pcon $ PAuthenticatedStageTenV1 (pdata bound) pscriptSources'sourceCount pscriptSources'sourcePeaks pscriptSources'purposeCount pscriptSources'purposePeaks (pdata discovery)
  pif
    ( pand'List
        [ pmachineStateIsWellFormed # machineState
        , pfromData pmachineState'eventKeyHash #== pfromData pauthenticatedDescriptor'eventKeyHash
        , pfromData pmachineState'transactionId #== pfromData psubject'transactionId
        , pfromData pmachineState'sourceKind #== expectedSourceKind
        , pfromData pmachineState'phase #== pcon PScriptSources
        , pfromData pmachineState'verdict #== pcon PPending
        , pfromData pmachineState'rejectionCodeHash #== pconstant (BS.replicate 32 0)
        , pfromData pmachineState'transactionCommitment #== pnativeTxProofCommitmentV1 # pfromData pscriptSources'compactCbor # pfromData pscriptSources'witnessSetCompactCbor # pfromData pscriptSources'fieldPreimageLengthsCbor
        , pblake2b_256 # pfromData pscriptSources'witnessSetCompactCbor #== pfromData pboundPurpose'witnessSetHash
        , pfromData pmachineState'validationContextHash #== phashValidationContext # pfromData pscriptSources'contextCbor
        , pfromData pmachineState'workRoot #== phashWorkWitness # pcon PScriptSources # pfromData pmachineState'programCounter # (pencodeScriptSourcesDiscoveryWitness # control # 10 # discovery)
        , pfromData ptraceProof'stateHash #== phashMachineState # machineState
        , pverifyTraceProof # descriptor # traceProof
        , pfromData pscriptSources'stage #== 10
        , pfromData pscriptSources'sourceTotalCount #== pfromData pscriptSources'sourceCount
        , pfromData pscriptSources'redeemerTotalCount #== pfromData pscriptSources'redeemerCount
        , pfrontierIsWellFormed # pfromData pscriptSources'sourceCount # pfromData pscriptSources'sourcePeaks
        , pfrontierIsWellFormed # pfromData pscriptSources'redeemerCount # pfromData pscriptSources'redeemerPeaks
        , pfrontierIsWellFormed # pfromData pscriptSources'purposeCount # pfromData pscriptSources'purposePeaks
        , pfromData pscriptDiscovery'purposeCursor #< pfromData pscriptSources'purposeCount
        , pfromData pscriptDiscovery'sourceCursor #<= pfromData pscriptSources'sourceCount
        , pfromData pscriptDiscovery'redeemerCursor #>= 0
        , pfromData pscriptDiscovery'redeemerCursor #<= pfromData pscriptSources'redeemerCount
        , pfromData pscriptDiscovery'executionCount #== pfromData pscriptDiscovery'purposeCursor
        , pfromData pscriptSources'pendingSourceCbor #== pconstant ""
        , pfromData pscriptSources'outputProof #== pcon PDNothing
        ]
    )
    expected
    perror

pauthenticateStageTenSelectionV1 :: forall s. Term s (PAuthenticatedStageTenV1 :--> PInteger :--> PBuiltinList (PAsData PByteString) :--> PInteger :--> PByteString :--> PInteger :--> PByteString :--> PInteger :--> PByteString :--> PBuiltinList (PAsData PByteString) :--> PAuthenticatedPurposeV1)
pauthenticateStageTenSelectionV1 = phoistAcyclic $ plam $ \stageTen absolutePurposeIndex purposeSiblings originKind sourceKey languageTag scriptHash totalLength itemCommitment sourceSiblings -> P.do
  PAuthenticatedStageTenV1{..} <- pmatch stageTen
  bound <- plet $ pfromData pauthenticatedStageTen'bound
  PBoundPurposeV1{pboundPurpose'purposeKind, pboundPurpose'purposeIndex} <- pmatch bound
  discovery <- plet $ pfromData pauthenticatedStageTen'discovery
  PScriptDiscoveryControlV1{..} <- pmatch discovery
  purposeLeaf <- plet $ ppurposeLeafHash # pfromData pboundPurpose'purposeKind # pfromData pboundPurpose'purposeIndex # pfromData pscriptDiscovery'currentScriptHash # pfromData pscriptDiscovery'currentSubject
  sourceLeaf <- plet $ psourceDescriptorLeafHash # originKind # sourceKey # languageTag # scriptHash # totalLength # itemCommitment
  let expected tag = pcon $ PAuthenticatedPurposeV1 (pdata bound) pauthenticatedStageTen'purposeCount (pdata tag) pscriptDiscovery'currentScriptHash pscriptDiscovery'matchedSourceIndex (pdata languageTag) (pdata sourceLeaf)
      valid =
        pand'List
          [ absolutePurposeIndex #== pfromData pscriptDiscovery'purposeCursor
          , pverifyMembership # pfromData pauthenticatedStageTen'purposeCount # pfromData pauthenticatedStageTen'purposePeaks # absolutePurposeIndex # purposeLeaf # purposeSiblings
          , pfromData pscriptDiscovery'currentPurposeKind #== pfromData pboundPurpose'purposeKind
          , pfromData pscriptDiscovery'currentPurposeIndex #== pfromData pboundPurpose'purposeIndex
          , plengthBS # pfromData pscriptDiscovery'currentScriptHash #== 28
          , pfromData pscriptDiscovery'currentSubject #/= pconstant ""
          , pfromData pscriptDiscovery'matchedSourceIndex #>= 0
          , pfromData pscriptDiscovery'matchedSourceIndex #< pfromData pauthenticatedStageTen'sourceCount
          , scriptHash #== pfromData pscriptDiscovery'currentScriptHash
          , languageTag #== pfromData pscriptDiscovery'matchedLanguageTag
          , sourceLeaf #== pfromData pscriptDiscovery'matchedSourceLeaf
          , pverifyMembership # pfromData pauthenticatedStageTen'sourceCount # pfromData pauthenticatedStageTen'sourcePeaks # pfromData pscriptDiscovery'matchedSourceIndex # sourceLeaf # sourceSiblings
          , pisRedeemerBearingLanguageV1 # pfromData pscriptDiscovery'matchedLanguageTag
          , plengthBS # pfromData pscriptDiscovery'matchedSourceLeaf #== 32
          ]
  pif valid (pmatch (predeemerTagForPurposeKindV1 # pfromData pboundPurpose'purposeKind) $ \case PNothing -> perror; PJust tag -> expected tag) perror

pinitialScanV1 :: forall s. Term s (PAuthenticatedPurposeV1 :--> PByteString :--> PInteger :--> PScanStateV1)
pinitialScanV1 = phoistAcyclic $ plam $ \authenticated checkpoint itemCount ->
  pif
    (plengthBS # checkpoint #== 32 #&& itemCount #>= 0)
    (pcon $ PScanStateV1 (pdata authenticated) (pdata checkpoint) (pdata 0) (pdata itemCount) (pdata $ pconstant False))
    perror

ppointerFromItemV1 :: forall s. Term s (PByteString :--> PMaybe (PPair PInteger PInteger))
ppointerFromItemV1 = phoistAcyclic $ plam $ \item ->
  pmatch (pheadAtV1 # item # 0 # 4) $ \case
    PNothing -> pcon PNothing
    PJust outer ->
      pmatch outer $ \(PCborHeadV1 next value) ->
        pif
          (value #== 4)
          ( pmatch (pheadAtV1 # item # next # 0) $ \case
              PNothing -> pcon PNothing
              PJust tag -> pmatch tag $ \(PCborHeadV1 afterTag tagValue) ->
                pmatch (pheadAtV1 # item # afterTag # 0) $ \case
                  PNothing -> pcon PNothing
                  PJust index -> pmatch index $ \(PCborHeadV1 _ indexValue) -> pcon $ PJust $ pcon $ PPair tagValue indexValue
          )
          (pcon PNothing)

pscanItemV1 :: forall s. Term s (PScanStateV1 :--> PInteger :--> PByteString :--> PScanStateV1)
pscanItemV1 = phoistAcyclic $ plam $ \state itemIndex item ->
  pmatch state $ \PScanStateV1{..} ->
    pif
      (pnot # pfromData pscanState'found #&& itemIndex #== pfromData pscanState'cursor #&& itemIndex #< pfromData pscanState'itemCount)
      ( pmatch (ppointerFromItemV1 # item) $ \case
          PNothing -> perror
          PJust pointer -> pmatch pointer $ \(PPair tag index) ->
            pmatch (pfromData pscanState'authenticated) $ \PAuthenticatedPurposeV1{pauthenticatedPurpose'bound, pauthenticatedPurpose'redeemerTag} ->
              pmatch (pfromData pauthenticatedPurpose'bound) $ \PBoundPurposeV1{pboundPurpose'purposeIndex} ->
                pcon $ PScanStateV1 pscanState'authenticated pscanState'checkpointHash (pdata $ itemIndex + 1) pscanState'itemCount (pdata $ tag #== pfromData pauthenticatedPurpose'redeemerTag #&& index #== pfromData pboundPurpose'purposeIndex)
      )
      perror

pdecisionV1 :: forall s. Term s (PScanStateV1 :--> PDecisionStateV1)
pdecisionV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \PScanStateV1{..} ->
    pif
      (pfromData pscanState'found #|| pfromData pscanState'cursor #== pfromData pscanState'itemCount)
      ( pmatch (pfromData pscanState'authenticated) $ \PAuthenticatedPurposeV1{pauthenticatedPurpose'bound} ->
          pcon $ PDecisionStateV1 pauthenticatedPurpose'bound (pdata $ pnot # pfromData pscanState'found)
      )
      perror

pterminalContradictionV1 :: forall s. Term s (PDecisionStateV1 :--> PBool)
pterminalContradictionV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \PDecisionStateV1{pdecisionState'bound, pdecisionState'redeemerMissing} ->
    pmatch (pfromData pdecisionState'bound) $ \PBoundPurposeV1{pboundPurpose'subject} ->
      Subject.pterminalContradiction # pfromData pboundPurpose'subject # pfromData pdecisionState'redeemerMissing

peventKeyMatchesSubject :: forall s. Term s PEventKey -> Term s Subject.PVerdictSubject -> Term s PBool
peventKeyMatchesSubject eventKey subject =
  pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'sourceKind, Subject.psubject'transactionId, Subject.psubject'sourceKey} ->
    pmatch eventKey $ \case
      PL2TransactionEventKey txId -> pfromData psubject'sourceKind #== 0 #&& pfromData txId #== pfromData psubject'transactionId
      PForcedTransactionEventKey txOrderId -> pfromData psubject'sourceKind #== 1 #&& pserialiseData # pforgetData txOrderId #== pfromData psubject'sourceKey
      _ -> pconstant False

pdescriptorVerdictMatchesSubject :: forall s. Term s PValidationTraceDescriptorV1 -> Term s Subject.PVerdictSubject -> Term s PBool
pdescriptorVerdictMatchesSubject descriptor subject =
  pmatch descriptor $ \PValidationTraceDescriptorV1{pdescriptor'verdict, pdescriptor'rejectionCodeHash} ->
    pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
      pif
        (pfromData psubject'direction #== 0)
        (pfromData pdescriptor'verdict #== pcon PAccepted #&& pfromData pdescriptor'rejectionCodeHash #== pconstant (BS.replicate 32 0))
        (pfromData pdescriptor'verdict #== pcon PRejected #&& pfromData pdescriptor'rejectionCodeHash #== phashRejectionCode # (prejectionCodeOf # pforgetData (pdata $ Subject.prejectionReasonOf # subject)))

pcoerceData :: forall a s. (PIsData a) => Term s PData -> Term s a
pcoerceData = pfromData . punsafeCoerce
