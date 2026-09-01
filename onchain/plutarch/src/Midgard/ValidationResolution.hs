{-# LANGUAGE OverloadedStrings #-}

module Midgard.ValidationResolution (
  PValidationResolutionStateV1 (..),
  PValidationBoundaryEvidenceV1 (..),
  PPreparedValidationResolutionStateV1 (..),
  PWinningValidationResolutionStateV1 (..),
  presolutionVersion,
  ppreparedResolutionVersion,
  pwinningResolutionVersion,
  phashOneStepEvidence,
  presolverIndex,
  presolverCount,
  poneStepBoundaryIsAuthenticated,
  pprepareValidationResolution,
  pvalidationResolutionStateIsWellFormed,
  pchallengerUniquelyWins,
  pchallengerWinsWithValidSuccessor,
  pprepareSemanticResolution,
  ppreparedResolutionIsWellFormed,
  pwinningResolution,
  pwinningResolutionIsWellFormed,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Builtin.Data (plistData, pserialiseData)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.Prelude

import Midgard.ValidationDispute (PDisputeTurn (..), PValidationDisputeV1 (..), pdisputeVersion)
import Midgard.ValidationMachine (PValidationOneStepWitnessV1 (..), pstructuralTransitionIsValid)
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase (..),
  PValidationTraceProof (..),
  phashMachineState,
  pmachineStateIsWellFormed,
  pverifyTraceProof,
 )

data PValidationResolutionStateV1 (s :: S) = PValidationResolutionStateV1
  { presolution'version :: Term s (PAsData PInteger)
  , presolution'preState :: Term s (PAsData PValidationMachineStateV1)
  , presolution'operatorSuccessorHash :: Term s (PAsData PByteString)
  , presolution'challengerSuccessorHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationResolutionStateV1)

data PValidationBoundaryEvidenceV1 (s :: S) = PValidationBoundaryEvidenceV1
  { pboundary'preState :: Term s (PAsData PValidationMachineStateV1)
  , pboundary'operatorPost :: Term s (PAsData PValidationTraceProof)
  , pboundary'challengerPost :: Term s (PAsData PValidationTraceProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationBoundaryEvidenceV1)

data PPreparedValidationResolutionStateV1 (s :: S) = PPreparedValidationResolutionStateV1
  { pprepared'version :: Term s (PAsData PInteger)
  , pprepared'resolution :: Term s (PAsData PValidationResolutionStateV1)
  , pprepared'evidenceHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPreparedValidationResolutionStateV1)

data PWinningValidationResolutionStateV1 (s :: S) = PWinningValidationResolutionStateV1
  { pwinning'version :: Term s (PAsData PInteger) }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PWinningValidationResolutionStateV1)

presolutionVersion, ppreparedResolutionVersion, pwinningResolutionVersion :: forall (s :: S). Term s PInteger
presolutionVersion = 1
ppreparedResolutionVersion = 1
pwinningResolutionVersion = 1

poneStepEvidenceDomain :: forall (s :: S). Term s PByteString
poneStepEvidenceDomain = phexByteStr "4d69646761726456616c69646174696f6e4f6e655374657045766964656e63655631"

phashOneStepEvidence :: forall (s :: S). Term s (PData :--> PData :--> PByteString)
phashOneStepEvidence = phoistAcyclic $ plam $ \transition auxiliary ->
  pblake2b_256 # (poneStepEvidenceDomain <> (pserialiseData # (plistData # (pcons # transition #$ pcons # auxiliary # pnil))))

presolverIndex :: forall (s :: S). Term s (PValidationPhase :--> PInteger)
presolverIndex = phoistAcyclic $ plam $ \phase -> pmatch phase $ \case
  PCanonicalDecode -> 0
  PCompactBinding -> 1
  PStaticLedgerRules -> 2
  PInputSets -> 3
  PSignatures -> 4
  PPhaseANativeScripts -> 5
  PPhaseAScriptPreconditions -> 6
  PResolveInputs -> 7
  PScriptSources -> 8
  PNativeScripts -> 9
  PScriptIntegrity -> 10
  PCek -> 11
  PValueAndMint -> 12
  PLedgerDelta -> 13
  PTerminal -> -1

presolverCount :: forall (s :: S). Term s PInteger
presolverCount = 14

poneStepBoundaryIsAuthenticated ::
  forall (s :: S).
  Term s (PValidationDisputeV1 :--> PValidationMachineStateV1 :--> PValidationTraceProof :--> PValidationTraceProof :--> PBool)
poneStepBoundaryIsAuthenticated = phoistAcyclic $ plam $ \dispute pre operatorPost challengerPost ->
  pmatch dispute $ \d -> pmatch pre $ \preState -> pmatch operatorPost $ \operator -> pmatch challengerPost $ \challenger -> pand'List
    [ pfromData (pdispute'version d) #== pdisputeVersion
    , pfromData (pdispute'turn d) #== pcon PReadyForOneStep
    , pfromData (pdispute'highIndex d) #== pfromData (pdispute'lowIndex d) + 1
    , presolverIndex # pfromData (pmachineState'phase preState) #>= 0
    , pmachineStateIsWellFormed # pre
    , phashMachineState # pre #== pfromData (pdispute'agreedLowHash d)
    , pfromData (ptraceProof'stateIndex operator) #== pfromData (pdispute'highIndex d)
    , pfromData (ptraceProof'stateIndex challenger) #== pfromData (pdispute'highIndex d)
    , pverifyTraceProof # pfromData (pdispute'operatorDescriptor d) # operatorPost
    , pverifyTraceProof # pfromData (pdispute'challengerDescriptor d) # challengerPost
    , pfromData (ptraceProof'stateHash operator) #== pfromData (pdispute'operatorHighHash d)
    , pfromData (ptraceProof'stateHash challenger) #== pfromData (pdispute'challengerHighHash d)
    ]

pprepareValidationResolution ::
  forall (s :: S).
  Term s (PValidationDisputeV1 :--> PValidationMachineStateV1 :--> PValidationTraceProof :--> PValidationTraceProof :--> PValidationResolutionStateV1)
pprepareValidationResolution = phoistAcyclic $ plam $ \dispute pre operatorPost challengerPost ->
  pif (poneStepBoundaryIsAuthenticated # dispute # pre # operatorPost # challengerPost)
    (pmatch operatorPost $ \operator -> pmatch challengerPost $ \challenger -> pcon $ PValidationResolutionStateV1
      (pdata presolutionVersion) (pdata pre) (ptraceProof'stateHash operator) (ptraceProof'stateHash challenger))
    perror

pvalidationResolutionStateIsWellFormed :: forall (s :: S). Term s (PValidationResolutionStateV1 :--> PBool)
pvalidationResolutionStateIsWellFormed = phoistAcyclic $ plam $ \state -> pmatch state $ \s ->
  plet (pfromData $ presolution'preState s) $ \pre -> pmatch pre $ \preState -> pand'List
    [ pfromData (presolution'version s) #== presolutionVersion
    , pmachineStateIsWellFormed # pre
    , presolverIndex # pfromData (pmachineState'phase preState) #>= 0
    , plengthBS # pfromData (presolution'operatorSuccessorHash s) #== 32
    , plengthBS # pfromData (presolution'challengerSuccessorHash s) #== 32
    , presolution'operatorSuccessorHash s #/= presolution'challengerSuccessorHash s
    ]

pchallengerUniquelyWins ::
  forall (s :: S).
  Term s (PValidationResolutionStateV1 :--> PValidationMachineStateV1 :--> PValidationMachineStateV1 :--> PBool :--> PBool :--> PBool)
pchallengerUniquelyWins = phoistAcyclic $ plam $ \state operatorSuccessor challengerSuccessor operatorValid challengerValid ->
  pmatch state $ \s -> pand'List
    [ pvalidationResolutionStateIsWellFormed # state
    , phashMachineState # operatorSuccessor #== pfromData (presolution'operatorSuccessorHash s)
    , phashMachineState # challengerSuccessor #== pfromData (presolution'challengerSuccessorHash s)
    , pnot # operatorValid
    , challengerValid
    ]

pchallengerWinsWithValidSuccessor ::
  forall (s :: S).
  Term s (PValidationResolutionStateV1 :--> PValidationMachineStateV1 :--> PBool :--> PBool)
pchallengerWinsWithValidSuccessor = phoistAcyclic $ plam $ \state challengerSuccessor challengerValid ->
  pmatch state $ \s -> pand'List
    [ pvalidationResolutionStateIsWellFormed # state
    , phashMachineState # challengerSuccessor #== pfromData (presolution'challengerSuccessorHash s)
    , challengerValid
    ]

pprepareSemanticResolution ::
  forall (s :: S).
  Term s (PValidationResolutionStateV1 :--> PValidationOneStepWitnessV1 :--> PData :--> PPreparedValidationResolutionStateV1)
pprepareSemanticResolution = phoistAcyclic $ plam $ \state transition auxiliary ->
  pmatch state $ \s -> pmatch transition $ \t ->
    pif (pvalidationResolutionStateIsWellFormed # state
      #&& phashMachineState # pfromData (poneStep'claimedSuccessor t) #== pfromData (presolution'challengerSuccessorHash s)
      #&& pstructuralTransitionIsValid # pfromData (presolution'preState s) # transition)
      (pcon $ PPreparedValidationResolutionStateV1
        (pdata ppreparedResolutionVersion)
        (pdata state)
        (pdata $ phashOneStepEvidence # pforgetData (pdata transition) # auxiliary))
      perror

ppreparedResolutionIsWellFormed :: forall (s :: S). Term s (PPreparedValidationResolutionStateV1 :--> PBool)
ppreparedResolutionIsWellFormed = phoistAcyclic $ plam $ \state -> pmatch state $ \s -> pand'List
  [ pfromData (pprepared'version s) #== ppreparedResolutionVersion
  , pvalidationResolutionStateIsWellFormed # pfromData (pprepared'resolution s)
  , plengthBS # pfromData (pprepared'evidenceHash s) #== 32
  ]

pwinningResolution :: forall (s :: S). Term s PWinningValidationResolutionStateV1
pwinningResolution = pcon $ PWinningValidationResolutionStateV1 (pdata pwinningResolutionVersion)

pwinningResolutionIsWellFormed :: forall (s :: S). Term s (PWinningValidationResolutionStateV1 :--> PBool)
pwinningResolutionIsWellFormed = phoistAcyclic $ plam $ \state -> pmatch state $ \s ->
  pfromData (pwinning'version s) #== pwinningResolutionVersion
