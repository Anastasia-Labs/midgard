{-# LANGUAGE OverloadedStrings #-}

-- | Exact unused-script-witness rule and six-script Data ABI.
module Midgard.FraudProofs.UnusedScriptWitness (
  PBoundWitnessV1 (..),
  PAuthenticatedWitnessV1 (..),
  PReverseScanV1 (..),
  PDecisionV1 (..),
  PSourceOpeningV1 (..),
  PPurposeOpeningV1 (..),
  PTerminalScriptSourcesWitnessV1 (..),
  PStep01SourceV1 (..),
  PStep01Args (..),
  PStep02Args (..),
  PLinearArgs (..),
  PStep04Args (..),
  PStep05Args (..),
  PStep06Args (..),
  pmaximumScanBatch,
  pbindWitnessV1,
  pauthenticateWitnessV1,
  pinitialReverseScanV1,
  pauthenticateEarlierSourceV1,
  palternateSourcesCompleteV1,
  pscanPurposeV1,
  preverseScanCompleteV1,
  pdecisionV1,
  pterminalContradictionV1,
  pencodeReverseScanV1,
) where

import Aiken.Cbor (pdeserialise)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.NativeTx.Codec (pcborInt)
import Midgard.FraudProofs.NativeTx.Compact (pnativeTxProofCommitmentV1)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PEventKey (..), PHeaderV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PUnusedScriptWitness), prejectionCodeOf)
import Midgard.ScriptProof (pinlineSourceLeafHash, ppurposeLeafHash)
import Midgard.TransitionTrace (PRootDomain (PValidationTracesRootDomain), PRootMembershipProof (..), pverifyRootMembershipWithBytes)
import Midgard.ValidationMerkle (PFrontierPeak (..), pfrontierIsWellFormed, pverifyMembership)
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

data PBoundWitnessV1 (s :: S) = PBoundWitnessV1
  { pboundWitness'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pboundWitness'validationTracesRoot :: Term s (PAsData PByteString)
  , pboundWitness'validationTraceCount :: Term s (PAsData PInteger)
  , pboundWitness'scriptIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBoundWitnessV1)

data PAuthenticatedWitnessV1 (s :: S) = PAuthenticatedWitnessV1
  { pauthenticatedWitness'bound :: Term s (PAsData PBoundWitnessV1)
  , pauthenticatedWitness'priorLedgerRoot :: Term s (PAsData PByteString)
  , pauthenticatedWitness'languageTag :: Term s (PAsData PInteger)
  , pauthenticatedWitness'scriptHash :: Term s (PAsData PByteString)
  , pauthenticatedWitness'scriptTotalLength :: Term s (PAsData PInteger)
  , pauthenticatedWitness'itemCommitment :: Term s (PAsData PByteString)
  , pauthenticatedWitness'sourceCount :: Term s (PAsData PInteger)
  , pauthenticatedWitness'sourcePeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pauthenticatedWitness'purposeCount :: Term s (PAsData PInteger)
  , pauthenticatedWitness'purposePeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAuthenticatedWitnessV1)

data PReverseScanV1 (s :: S) = PReverseScanV1
  { preverseScan'witness :: Term s (PAsData PAuthenticatedWitnessV1)
  , preverseScan'alternateCursor :: Term s (PAsData PInteger)
  , preverseScan'purposeCursor :: Term s (PAsData PInteger)
  , preverseScan'shadowed :: Term s (PAsData PBool)
  , preverseScan'used :: Term s (PAsData PBool)
  , preverseScan'checkpointHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PReverseScanV1)

data PDecisionV1 (s :: S) = PDecisionV1
  { pdecision'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pdecision'scriptIndex :: Term s (PAsData PInteger)
  , pdecision'unused :: Term s (PAsData PBool)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDecisionV1)

data PSourceOpeningV1 (s :: S) = PSourceOpeningV1
  { psourceOpening'sourceIndex :: Term s (PAsData PInteger)
  , psourceOpening'languageTag :: Term s (PAsData PInteger)
  , psourceOpening'scriptHash :: Term s (PAsData PByteString)
  , psourceOpening'totalLength :: Term s (PAsData PInteger)
  , psourceOpening'itemCommitment :: Term s (PAsData PByteString)
  , psourceOpening'siblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSourceOpeningV1)

data PPurposeOpeningV1 (s :: S) = PPurposeOpeningV1
  { ppurposeOpening'frontierIndex :: Term s (PAsData PInteger)
  , ppurposeOpening'purposeKind :: Term s (PAsData PInteger)
  , ppurposeOpening'purposeIndex :: Term s (PAsData PInteger)
  , ppurposeOpening'scriptHash :: Term s (PAsData PByteString)
  , ppurposeOpening'purposeSubject :: Term s (PAsData PByteString)
  , ppurposeOpening'siblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPurposeOpeningV1)

newtype PTerminalScriptSourcesWitnessV1 (s :: S) = PTerminalScriptSourcesWitnessV1
  { pterminalWitness'witnessCbor :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTerminalScriptSourcesWitnessV1)

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
  , pstep01Args'scriptIndex :: Term s (PAsData PInteger)
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
  , pstep02Args'control :: Term s (PAsData PTerminalScriptSourcesWitnessV1)
  , pstep02Args'languageTag :: Term s (PAsData PInteger)
  , pstep02Args'scriptHash :: Term s (PAsData PByteString)
  , pstep02Args'totalLength :: Term s (PAsData PInteger)
  , pstep02Args'itemCommitment :: Term s (PAsData PByteString)
  , pstep02Args'sourceSiblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PLinearArgs (s :: S) = PLinearArgs
  { plinearArgs'inputIndex :: Term s (PAsData PInteger)
  , plinearArgs'outputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLinearArgs)

data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , pstep04Args'openings :: Term s (PAsData (PBuiltinList (PAsData PSourceOpeningV1)))
  , pstep04Args'itemBudget :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

data PStep05Args (s :: S) = PStep05Args
  { pstep05Args'inputIndex :: Term s (PAsData PInteger)
  , pstep05Args'outputIndex :: Term s (PAsData PInteger)
  , pstep05Args'openings :: Term s (PAsData (PBuiltinList (PAsData PPurposeOpeningV1)))
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

data PTerminalControl s = PTerminalControl
  { pcontrol'compactCbor :: Term s PByteString
  , pcontrol'witnessSetCompactCbor :: Term s PByteString
  , pcontrol'fieldPreimageLengthsCbor :: Term s PByteString
  , pcontrol'contextCbor :: Term s PByteString
  , pcontrol'stage :: Term s PInteger
  , pcontrol'sourceCount :: Term s PInteger
  , pcontrol'sourcePeaks :: Term s (PBuiltinList (PAsData PFrontierPeak))
  , pcontrol'redeemerCount :: Term s PInteger
  , pcontrol'redeemerPeaks :: Term s (PBuiltinList (PAsData PFrontierPeak))
  , pcontrol'purposeCount :: Term s PInteger
  , pcontrol'purposePeaks :: Term s (PBuiltinList (PAsData PFrontierPeak))
  , pcontrol'sourceTotalCount :: Term s PInteger
  , pcontrol'redeemerTotalCount :: Term s PInteger
  , pcontrol'purposeCursor :: Term s PInteger
  , pcontrol'sourceCursor :: Term s PInteger
  , pcontrol'redeemerCursor :: Term s PInteger
  , pcontrol'executionCount :: Term s PInteger
  , pcontrol'executionPeaks :: Term s (PBuiltinList (PAsData PFrontierPeak))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsSOPStruct PTerminalControl)

pmaximumScanBatch :: forall s. Term s PInteger
pmaximumScanBatch = 24

pat :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s PData
pat fields index = pelemAt # index # fields

pdecodeFrontier :: forall s. Term s PData -> Term s (PBuiltinList (PAsData PFrontierPeak))
pdecodeFrontier dat =
  pmap
    # plam
      ( \peak ->
          plet (pasList # peak) $ \fields ->
            pif
              (plength # fields #== 2)
              (pdata $ pcon $ PFrontierPeak (pdata $ pasInt # pat fields 0) (pdata $ pasByteStr # pat fields 1))
              perror
      )
    # (pasList # dat)

pdecodeTerminalControl :: forall s. Term s (PByteString :--> PTerminalControl)
pdecodeTerminalControl = phoistAcyclic $ plam $ \witnessCbor ->
  pmatch (pdeserialise # witnessCbor) $ \case
    PNothing -> perror
    PJust dat ->
      plet (pasList # dat) $ \items ->
        pif
          (plength # items #== 31)
          ( pmatch (pdeserialise # (pasByteStr # pat items 30)) $ \case
              PNothing -> perror
              PJust discoveryDat ->
                plet (pasList # discoveryDat) $ \discovery ->
                  pif
                    ( plength
                        # discovery
                        #== 15
                        #&& pasInt
                        # pat discovery 3
                        #== (-1)
                        #&& pasInt
                        # pat discovery 4
                        #== (-1)
                        #&& pasByteStr
                        # pat discovery 5
                        #== pconstant ""
                        #&& pasByteStr
                        # pat discovery 6
                        #== pconstant ""
                        #&& pasInt
                        # pat discovery 7
                        #== (-1)
                        #&& pasInt
                        # pat discovery 8
                        #== (-1)
                        #&& pasByteStr
                        # pat discovery 9
                        #== pconstant ""
                    )
                    ( pcon $
                        PTerminalControl
                          (pasByteStr # pat items 0)
                          (pasByteStr # pat items 1)
                          (pasByteStr # pat items 2)
                          (pasByteStr # pat items 3)
                          (pasInt # pat items 9)
                          (pasInt # pat items 10)
                          (pdecodeFrontier $ pat items 11)
                          (pasInt # pat items 12)
                          (pdecodeFrontier $ pat items 13)
                          (pasInt # pat items 18)
                          (pdecodeFrontier $ pat items 19)
                          (pasInt # pat items 25)
                          (pasInt # pat items 26)
                          (pasInt # pat discovery 0)
                          (pasInt # pat discovery 1)
                          (pasInt # pat discovery 2)
                          (pasInt # pat discovery 13)
                          (pdecodeFrontier $ pat discovery 14)
                    )
                    perror
          )
          perror

pbindWitnessV1 :: forall s. Term s (Subject.PVerdictSubject :--> PByteString :--> PInteger :--> PInteger :--> PBoundWitnessV1)
pbindWitnessV1 = phoistAcyclic $ plam $ \subject traceRoot traceCount scriptIndex ->
  pif
    ( Subject.psubjectIsCanonical
        # subject
        #&& plengthBS
        # traceRoot
        #== 32
        #&& traceCount
        #> 0
        #&& scriptIndex
        #>= 0
    )
    ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
        let bound = pcon $ PBoundWitnessV1 (pdata subject) (pdata traceRoot) (pdata traceCount) (pdata scriptIndex)
         in pif
              (pfromData psubject'direction #== 1)
              (plet (Subject.pbindExactRejectionReason # subject # pcon (PUnusedScriptWitness $ pdata scriptIndex)) $ \_ -> bound)
              bound
    )
    perror

pauthenticateWitnessV1 ::
  forall s.
  Term
    s
    ( PBoundWitnessV1
        :--> PRootMembershipProof
        :--> PValidationMachineStateV1
        :--> PValidationTraceProof
        :--> PTerminalScriptSourcesWitnessV1
        :--> PInteger
        :--> PByteString
        :--> PInteger
        :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PAuthenticatedWitnessV1
    )
pauthenticateWitnessV1 = phoistAcyclic $ plam $ \bound membership machineState traceProof retained languageTag scriptHash totalLength itemCommitment sourceSiblings -> P.do
  PBoundWitnessV1{..} <- pmatch bound
  subject <- plet $ pfromData pboundWitness'subject
  Subject.PVerdictSubject{Subject.psubject'direction, Subject.psubject'sourceKind, Subject.psubject'transactionId} <- pmatch subject
  PRootMembershipProof{prootMembership'key, prootMembership'value} <- pmatch membership
  eventKey <- plet $ punsafeCoerce prootMembership'key
  descriptor <- plet $ punsafeCoerce prootMembership'value
  PValidationMachineStateV1{..} <- pmatch machineState
  PValidationTraceProof{ptraceProof'stateHash} <- pmatch traceProof
  PTerminalScriptSourcesWitnessV1{pterminalWitness'witnessCbor} <- pmatch retained
  control <- plet $ pdecodeTerminalControl # pfromData pterminalWitness'witnessCbor
  PTerminalControl{..} <- pmatch control
  sourceLeaf <- plet $ pinlineSourceLeafHash # pfromData pboundWitness'scriptIndex # languageTag # scriptHash # totalLength # itemCommitment
  let expected =
        pcon $
          PAuthenticatedWitnessV1
            (pdata bound)
            pmachineState'priorLedgerRoot
            (pdata languageTag)
            (pdata scriptHash)
            (pdata totalLength)
            (pdata itemCommitment)
            (pdata pcontrol'sourceCount)
            (pdata pcontrol'sourcePeaks)
            (pdata pcontrol'purposeCount)
            (pdata pcontrol'purposePeaks)
      subjectBranch =
        pif
          (pfromData psubject'sourceKind #== 0)
          (pfromData pmachineState'sourceKind #== pcon PNormal #&& pdescriptorAccepted descriptor)
          (pfromData pmachineState'sourceKind #== pcon PForced #&& pdescriptorRejectsSubject descriptor subject)
      stageBranch =
        pif
          (pfromData psubject'direction #== 0)
          (pcontrol'stage #== 11 #&& pcontrol'sourceCursor #== pfromData pboundWitness'scriptIndex)
          (pcontrol'stage #== 12 #&& pcontrol'sourceCursor #== pcontrol'sourceCount)
  pif
    ( pand'List
        [ peventKeyMatchesSubject eventKey subject
        , pfromData pmachineState'eventKeyHash #== pblake2b_256 # (pserialiseData # prootMembership'key)
        , pverifyRootMembershipWithBytes membership (pdata $ pcon PValidationTracesRootDomain) (pfromData pboundWitness'validationTracesRoot) (pfromData pboundWitness'validationTraceCount) (pserialiseData # prootMembership'key) (pserialiseData # prootMembership'value)
        , pfromData pmachineState'transactionId #== pfromData psubject'transactionId
        , subjectBranch
        , pfromData pmachineState'phase #== pcon PScriptSources
        , pfromData pmachineState'workRoot #== phashWorkWitness # pcon PScriptSources # pfromData pmachineState'programCounter # pfromData pterminalWitness'witnessCbor
        , pfromData ptraceProof'stateHash #== phashMachineState # machineState
        , pverifyTraceProof # descriptor # traceProof
        , pfromData pmachineState'transactionCommitment #== pnativeTxProofCommitmentV1 # pcontrol'compactCbor # pcontrol'witnessSetCompactCbor # pcontrol'fieldPreimageLengthsCbor
        , pfromData pmachineState'validationContextHash #== phashValidationContext # pcontrol'contextCbor
        , stageBranch
        , pcontrol'sourceTotalCount #== pcontrol'sourceCount
        , pcontrol'redeemerTotalCount #== pcontrol'redeemerCount
        , pfrontierIsWellFormed # pcontrol'sourceCount # pcontrol'sourcePeaks
        , pfrontierIsWellFormed # pcontrol'redeemerCount # pcontrol'redeemerPeaks
        , pcontrol'purposeCount #== pcontrol'purposeCursor
        , pfrontierIsWellFormed # pcontrol'purposeCount # pcontrol'purposePeaks
        , pcontrol'redeemerCursor #>= 0
        , pcontrol'redeemerCursor #<= pcontrol'redeemerCount
        , pcontrol'executionCount #== pcontrol'purposeCursor
        , pfrontierIsWellFormed # pcontrol'executionCount # pcontrol'executionPeaks
        , pfromData pboundWitness'scriptIndex #< pcontrol'sourceCount
        , pverifyMembership # pcontrol'sourceCount # pcontrol'sourcePeaks # pfromData pboundWitness'scriptIndex # sourceLeaf # sourceSiblings
        , totalLength #> 0
        , plengthBS # scriptHash #== 28
        , plengthBS # itemCommitment #== 32
        ]
    )
    expected
    perror

pinitialReverseScanV1 :: forall s. Term s (PAuthenticatedWitnessV1 :--> PReverseScanV1)
pinitialReverseScanV1 = phoistAcyclic $ plam $ \witness ->
  pmatch witness $ \PAuthenticatedWitnessV1{..} ->
    pmatch (pfromData pauthenticatedWitness'bound) $ \PBoundWitnessV1{pboundWitness'subject, pboundWitness'scriptIndex} ->
      pmatch (pfromData pboundWitness'subject) $ \Subject.PVerdictSubject{Subject.psubject'transactionId} ->
        pcon $
          PReverseScanV1
            (pdata witness)
            (pdata 0)
            (pdata 0)
            (pdata $ pconstant False)
            (pdata $ pconstant False)
            ( pdata $
                pblake2b_256
                  # ( pconstant "MidgardUnusedScriptWitnessScanV1"
                        <> pfromData psubject'transactionId
                        <> pcborInt (pfromData pboundWitness'scriptIndex)
                        <> pfromData pauthenticatedWitness'scriptHash
                        <> pcborInt (pfromData pauthenticatedWitness'purposeCount)
                    )
            )

pauthenticateEarlierSourceV1 :: forall s. Term s (PReverseScanV1 :--> PSourceOpeningV1 :--> PReverseScanV1)
pauthenticateEarlierSourceV1 = phoistAcyclic $ plam $ \state opening -> P.do
  PReverseScanV1{..} <- pmatch state
  witness <- plet $ pfromData preverseScan'witness
  PAuthenticatedWitnessV1{..} <- pmatch witness
  PBoundWitnessV1{pboundWitness'scriptIndex} <- pmatch $ pfromData pauthenticatedWitness'bound
  PSourceOpeningV1{..} <- pmatch opening
  leaf <- plet $ pinlineSourceLeafHash # pfromData psourceOpening'sourceIndex # pfromData psourceOpening'languageTag # pfromData psourceOpening'scriptHash # pfromData psourceOpening'totalLength # pfromData psourceOpening'itemCommitment
  let nextCursor = pfromData preverseScan'alternateCursor + 1
      nextShadowed = pfromData preverseScan'shadowed #|| pfromData psourceOpening'scriptHash #== pfromData pauthenticatedWitness'scriptHash
      expected =
        pcon $
          PReverseScanV1
            preverseScan'witness
            (pdata nextCursor)
            preverseScan'purposeCursor
            (pdata nextShadowed)
            preverseScan'used
            (pdata $ pblake2b_256 # (pfromData preverseScan'checkpointHash <> pcborInt nextCursor <> pfromData psourceOpening'scriptHash))
  pif
    ( pfromData preverseScan'alternateCursor
        #< pfromData pboundWitness'scriptIndex
        #&& pfromData psourceOpening'sourceIndex
        #== pfromData preverseScan'alternateCursor
        #&& pverifyMembership
        # pfromData pauthenticatedWitness'sourceCount
        # pfromData pauthenticatedWitness'sourcePeaks
        # pfromData psourceOpening'sourceIndex
        # leaf
        # pfromData psourceOpening'siblings
    )
    expected
    perror

palternateSourcesCompleteV1 :: forall s. Term s (PReverseScanV1 :--> PBool)
palternateSourcesCompleteV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \PReverseScanV1{preverseScan'witness, preverseScan'alternateCursor} ->
    pmatch (pfromData preverseScan'witness) $ \PAuthenticatedWitnessV1{pauthenticatedWitness'bound} ->
      pmatch (pfromData pauthenticatedWitness'bound) $ \PBoundWitnessV1{pboundWitness'scriptIndex} ->
        pfromData preverseScan'alternateCursor #== pfromData pboundWitness'scriptIndex

pscanPurposeV1 :: forall s. Term s (PReverseScanV1 :--> PPurposeOpeningV1 :--> PReverseScanV1)
pscanPurposeV1 = phoistAcyclic $ plam $ \state opening -> P.do
  PReverseScanV1{..} <- pmatch state
  witness <- plet $ pfromData preverseScan'witness
  PAuthenticatedWitnessV1{..} <- pmatch witness
  PPurposeOpeningV1{..} <- pmatch opening
  leaf <- plet $ ppurposeLeafHash # pfromData ppurposeOpening'purposeKind # pfromData ppurposeOpening'purposeIndex # pfromData ppurposeOpening'scriptHash # pfromData ppurposeOpening'purposeSubject
  let nextCursor = pfromData preverseScan'purposeCursor + 1
      matched = pnot # pfromData preverseScan'shadowed #&& pfromData ppurposeOpening'scriptHash #== pfromData pauthenticatedWitness'scriptHash
      expected =
        pcon $
          PReverseScanV1
            preverseScan'witness
            preverseScan'alternateCursor
            (pdata nextCursor)
            preverseScan'shadowed
            (pdata matched)
            ( pdata $
                pblake2b_256
                  # ( pfromData preverseScan'checkpointHash
                        <> pcborInt (pfromData ppurposeOpening'purposeKind)
                        <> pcborInt (pfromData ppurposeOpening'purposeIndex)
                        <> pfromData ppurposeOpening'scriptHash
                    )
            )
  pif
    ( palternateSourcesCompleteV1
        # state
        #&& pnot
        # pfromData preverseScan'used
        #&& pfromData preverseScan'purposeCursor
        #< pfromData pauthenticatedWitness'purposeCount
        #&& pfromData ppurposeOpening'frontierIndex
        #== pfromData preverseScan'purposeCursor
        #&& pfromData ppurposeOpening'purposeKind
        #>= 0
        #&& pfromData ppurposeOpening'purposeKind
        #<= 3
        #&& pverifyMembership
        # pfromData pauthenticatedWitness'purposeCount
        # pfromData pauthenticatedWitness'purposePeaks
        # pfromData ppurposeOpening'frontierIndex
        # leaf
        # pfromData ppurposeOpening'siblings
    )
    expected
    perror

preverseScanCompleteV1 :: forall s. Term s (PReverseScanV1 :--> PBool)
preverseScanCompleteV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \PReverseScanV1{preverseScan'witness, preverseScan'purposeCursor, preverseScan'used} ->
    pmatch (pfromData preverseScan'witness) $ \PAuthenticatedWitnessV1{pauthenticatedWitness'purposeCount} ->
      pfromData preverseScan'used #|| pfromData preverseScan'purposeCursor #== pfromData pauthenticatedWitness'purposeCount

pdecisionV1 :: forall s. Term s (PReverseScanV1 :--> PDecisionV1)
pdecisionV1 = phoistAcyclic $ plam $ \state ->
  pmatch state $ \PReverseScanV1{preverseScan'witness, preverseScan'shadowed, preverseScan'used} ->
    pmatch (pfromData preverseScan'witness) $ \PAuthenticatedWitnessV1{pauthenticatedWitness'bound} ->
      pmatch (pfromData pauthenticatedWitness'bound) $ \PBoundWitnessV1{pboundWitness'subject, pboundWitness'scriptIndex} ->
        pif
          (palternateSourcesCompleteV1 # state #&& preverseScanCompleteV1 # state)
          (pcon $ PDecisionV1 pboundWitness'subject pboundWitness'scriptIndex (pdata $ pfromData preverseScan'shadowed #|| pnot # pfromData preverseScan'used))
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

pdescriptorAccepted :: forall s. Term s PValidationTraceDescriptorV1 -> Term s PBool
pdescriptorAccepted descriptor =
  pmatch descriptor $ \PValidationTraceDescriptorV1{pdescriptor'verdict} -> pfromData pdescriptor'verdict #== pcon PAccepted

pdescriptorRejectsSubject :: forall s. Term s PValidationTraceDescriptorV1 -> Term s Subject.PVerdictSubject -> Term s PBool
pdescriptorRejectsSubject descriptor subject =
  pmatch descriptor $ \PValidationTraceDescriptorV1{pdescriptor'verdict, pdescriptor'rejectionCodeHash} ->
    pfromData pdescriptor'verdict
      #== pcon PRejected
      #&& pfromData pdescriptor'rejectionCodeHash
      #== phashRejectionCode
      # (prejectionCodeOf # pforgetData (pdata $ Subject.prejectionReasonOf # subject))
