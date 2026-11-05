{-# LANGUAGE OverloadedStrings #-}

-- | Direct distinct-asset accumulation-limit rule (categories 43-45) and staged Data ABI.
module Midgard.FraudProofs.DistinctAssetAccumulationLimit (
  PCoordinateV1 (..),
  PBoundV1 (..),
  PFoldStateV1 (..),
  PStep01Source (..),
  PStep01Args (..),
  PStep02Args (..),
  PInputEvidenceV1 (..),
  PInputActionV1 (..),
  POutputEvidenceV1 (..),
  POutputActionV1 (..),
  PMintEvidenceV1 (..),
  PMintActionV1 (..),
  PStep06Args (..),
  pcoordinateIsWellFormedV1,
  pbindCoordinateV1,
  pinitializeAccumulatorV1,
  pskipFoldV1,
  pauthenticateInputFoldV1,
  pauthenticateOutputFoldV1,
  pauthenticateMintFoldV1,
  pterminalContradictionV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerOutput (pmaxDistinctAssetCount)
import Midgard.LedgerOutputCommitment qualified as Descriptor
import Midgard.LedgerState (PEventKey (..), PHeaderV1)
import Midgard.MpfProof qualified as Mpf
import Midgard.MpfProof.Types (PProof (..))
import Midgard.RejectionReason (
  PRejectionReasonV1 (PInputAssetAccumulationLimit, PMintAssetAccumulationLimit, POutputAssetAccumulationLimit),
  prejectAssetCount,
 )
import Midgard.TransitionTrace (PRootDomain (PValidationTracesRootDomain), PRootMembershipProof (..), pverifyRootMembershipWithBytes)
import Midgard.ValidationMachine (
  PNativeScriptsControlV1 (..),
  PValueAccumulatorV1 (..),
  PValueAndMintControlV1 (..),
  PValueAssetMutationWitnessV1 (..),
  pencodeValueAndMintControlV1,
  pmintAssetLeafHash,
  presolutionScheduleNodeHash,
 )
import Midgard.ValidationMerkle (PFrontierPeak, pverifyMembership)
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase (PValueAndMint),
  PValidationSourceKind (PForced, PNormal),
  PValidationTraceDescriptorV1 (..),
  PValidationTraceProof (..),
  PValidationVerdict (PAccepted, PRejected),
  pcborInt,
  phashMachineState,
  phashRejectionCode,
  phashWorkWitness,
  pverifyTraceProof,
 )

data PCoordinateV1 (s :: S) = PCoordinateV1
  { pcoordinate'fold :: Term s (PAsData PInteger)
  , pcoordinate'primaryIndex :: Term s (PAsData PInteger)
  , pcoordinate'assetIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCoordinateV1)

data PBoundV1 (s :: S) = PBoundV1
  { pbound'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pbound'validationTracesRoot :: Term s (PAsData PByteString)
  , pbound'validationTraceCount :: Term s (PAsData PInteger)
  , pbound'coordinate :: Term s (PAsData PCoordinateV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBoundV1)

data PFoldStateV1 (s :: S) = PFoldStateV1
  { pfoldState'bound :: Term s (PAsData PBoundV1)
  , pfoldState'control :: Term s (PAsData (PMaybeData PValueAndMintControlV1))
  , pfoldState'stage :: Term s (PAsData PInteger)
  , pfoldState'decisiveFaultHolds :: Term s (PAsData (PMaybeData PBool))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFoldStateV1)

data PStep01Source (s :: S)
  = PAcceptedSource (Term s (PAsData PNativeTxInclusionCarriage))
  | PForcedSource (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PHeaderV1)) (Term s (PAsData PRootMembershipProof)) (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Source)

data PStep01Args (s :: S) = PStep01Args
  { pstep01Args'source :: Term s (PAsData PStep01Source)
  , pstep01Args'coordinate :: Term s (PAsData PCoordinateV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'traceMembership :: Term s (PAsData PRootMembershipProof)
  , pstep02Args'pre :: Term s (PAsData PValidationMachineStateV1)
  , pstep02Args'traceProof :: Term s (PAsData PValidationTraceProof)
  , pstep02Args'control :: Term s (PAsData PValueAndMintControlV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PInputEvidenceV1 (s :: S) = PInputEvidenceV1
  { pinputEvidence'sourceKind :: Term s (PAsData PInteger)
  , pinputEvidence'key :: Term s (PAsData PByteString)
  , pinputEvidence'nextScheduleHash :: Term s (PAsData PByteString)
  , pinputEvidence'descriptorCbor :: Term s (PAsData PByteString)
  , pinputEvidence'assetIndex :: Term s (PAsData PInteger)
  , pinputEvidence'policyId :: Term s (PAsData PByteString)
  , pinputEvidence'assetName :: Term s (PAsData PByteString)
  , pinputEvidence'quantity :: Term s (PAsData PInteger)
  , pinputEvidence'assetPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pinputEvidence'assetSiblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , pinputEvidence'mutation :: Term s (PAsData PValueAssetMutationWitnessV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PInputEvidenceV1)

data PInputActionV1 (s :: S)
  = PInputSkip (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  | PInputAuthenticate (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PInputEvidenceV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PInputActionV1)

data POutputEvidenceV1 (s :: S) = POutputEvidenceV1
  { poutputEvidence'outputIndex :: Term s (PAsData PInteger)
  , poutputEvidence'descriptorCbor :: Term s (PAsData PByteString)
  , poutputEvidence'assetIndex :: Term s (PAsData PInteger)
  , poutputEvidence'policyId :: Term s (PAsData PByteString)
  , poutputEvidence'assetName :: Term s (PAsData PByteString)
  , poutputEvidence'quantity :: Term s (PAsData PInteger)
  , poutputEvidence'assetPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , poutputEvidence'assetSiblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , poutputEvidence'mutation :: Term s (PAsData PValueAssetMutationWitnessV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POutputEvidenceV1)

data POutputActionV1 (s :: S)
  = POutputSkip (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  | POutputAuthenticate (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData POutputEvidenceV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POutputActionV1)

data PMintEvidenceV1 (s :: S) = PMintEvidenceV1
  { pmintEvidence'mintIndex :: Term s (PAsData PInteger)
  , pmintEvidence'policyId :: Term s (PAsData PByteString)
  , pmintEvidence'assetName :: Term s (PAsData PByteString)
  , pmintEvidence'quantity :: Term s (PAsData PInteger)
  , pmintEvidence'siblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , pmintEvidence'mutation :: Term s (PAsData PValueAssetMutationWitnessV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintEvidenceV1)

data PMintActionV1 (s :: S)
  = PMintSkip (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  | PMintAuthenticate (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PMintEvidenceV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintActionV1)

data PStep06Args (s :: S) = PStep06Args
  { pstep06Args'inputIndex :: Term s (PAsData PInteger)
  , pstep06Args'outputIndex :: Term s (PAsData PInteger)
  , pstep06Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep06Args)

pcoordinateIsWellFormedV1 :: forall s. Term s (PCoordinateV1 :--> PBool)
pcoordinateIsWellFormedV1 = phoistAcyclic $ plam $ \coordinate -> pmatch coordinate $ \PCoordinateV1{..} ->
  let fold = pfromData pcoordinate'fold
   in pand'List
        [ fold #>= 0
        , fold #<= 2
        , pfromData pcoordinate'primaryIndex #>= 0
        , pfromData pcoordinate'assetIndex #>= 0
        , pif (fold #== 2) (pfromData pcoordinate'assetIndex #== 0) (pconstant True)
        ]

pbindCoordinateV1 :: forall s. Term s (Subject.PVerdictSubject :--> PByteString :--> PInteger :--> PCoordinateV1 :--> PBoundV1)
pbindCoordinateV1 = phoistAcyclic $ plam $ \subject root count coordinate ->
  pif
    (Subject.psubjectIsCanonical # subject #&& plengthBS # root #== 32 #&& count #> 0 #&& pcoordinateIsWellFormedV1 # coordinate)
    ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
        let bound = pcon $ PBoundV1 (pdata subject) (pdata root) (pdata count) (pdata coordinate)
         in pif
              (pfromData psubject'direction #== 1)
              ( pmatch coordinate $ \PCoordinateV1{..} ->
                  let expected =
                        pif
                          (pfromData pcoordinate'fold #== 0)
                          (pcon $ PInputAssetAccumulationLimit pcoordinate'primaryIndex pcoordinate'assetIndex)
                          ( pif
                              (pfromData pcoordinate'fold #== 1)
                              (pcon $ POutputAssetAccumulationLimit pcoordinate'primaryIndex pcoordinate'assetIndex)
                              (pcon $ PMintAssetAccumulationLimit pcoordinate'primaryIndex)
                          )
                   in plet (Subject.pbindExactRejectionReason # subject # expected) $ \_ -> bound
              )
              bound
    )
    perror

pinitializeAccumulatorV1 :: forall s. Term s (PBoundV1 :--> PRootMembershipProof :--> PValidationMachineStateV1 :--> PValidationTraceProof :--> PValueAndMintControlV1 :--> PFoldStateV1)
pinitializeAccumulatorV1 = phoistAcyclic $ plam $ \bound membership pre proof control -> P.do
  PBoundV1{..} <- pmatch bound
  coordinate <- plet $ pfromData pbound'coordinate
  PCoordinateV1{pcoordinate'fold} <- pmatch coordinate
  PValueAndMintControlV1{..} <- pmatch control
  native <- plet $ pfromData pvalueAndMint'nativeControl
  PNativeScriptsControlV1{..} <- pmatch native
  accumulator <- plet $ pfromData pvalueAndMint'valueAccumulator
  PValueAccumulatorV1{..} <- pmatch accumulator
  let fold = pfromData pcoordinate'fold
      cursorValid =
        pif
          (fold #== 0)
          ( pand'List
              [ pfromData pvalueAndMint'replayCursor #>= 0
              , pfromData pvalueAndMint'replayCursor #< pfromData pnativeControl'resolvedInputCount
              , pfromData pvalueAndMint'replayAssetCursor #> 0
              , pfromData pvalueAndMint'replayAssetCursor #<= pmaxDistinctAssetCount
              ]
          )
          ( pif
              (fold #== 1)
              ( pand'List
                  [ pfromData pvalueAndMint'outputCursor #>= 0
                  , pfromData pvalueAndMint'outputCursor #< pfromData pnativeControl'outputCount
                  , pfromData pvalueAndMint'outputAssetCursor #> 0
                  , pfromData pvalueAndMint'outputAssetCursor #<= pmaxDistinctAssetCount
                  ]
              )
              (pfromData pvalueAndMint'mintCursor #>= 0 #&& pfromData pvalueAndMint'mintCursor #< pfromData pnativeControl'mintCount)
          )
      expected = pcon $ PFoldStateV1 (pdata bound) (pdata $ pcon $ PDJust $ pdata control) (pdata 0) (pdata $ pcon PDNothing)
  pif
    ( pand'List
        [ pcoordinateIsWellFormedV1 # coordinate
        , ptracePreIsAuthenticated bound membership pre proof control
        , pfromData pvalueAndMint'stage #== fold + 2
        , plengthBS # pfromData pvalueAccumulator'assetRoot #== 32
        , pfromData pvalueAccumulator'seenAssetCount #>= 0
        , pfromData pvalueAccumulator'seenAssetCount #<= pmaxDistinctAssetCount
        , pfromData pvalueAccumulator'nonzeroAssetCount #>= 0
        , pfromData pvalueAccumulator'nonzeroAssetCount #<= pfromData pvalueAccumulator'seenAssetCount
        , cursorValid
        ]
    )
    expected
    perror

pskipFoldV1 :: forall s. Term s (PFoldStateV1 :--> PInteger :--> PFoldStateV1)
pskipFoldV1 = phoistAcyclic $ plam $ \state fold -> pmatch state $ \s@PFoldStateV1{..} ->
  pmatch (pfromData pfoldState'bound) $ \PBoundV1{pbound'coordinate} ->
    pmatch (pfromData pbound'coordinate) $ \PCoordinateV1{pcoordinate'fold} ->
      let target = pfromData pcoordinate'fold
          hasDecision = pmatch (pfromData pfoldState'decisiveFaultHolds) $ \case PDNothing -> pconstant False; PDJust _ -> pconstant True
       in pif
            (pfromData pfoldState'stage #== fold #&& target #/= fold #&& pif (target #< fold) hasDecision (pnot # hasDecision))
            (pcon s{pfoldState'stage = pdata $ fold + 1})
            perror

pauthenticateInputFoldV1 :: forall s. Term s (PFoldStateV1 :--> PInteger :--> PByteString :--> PByteString :--> PByteString :--> PInteger :--> PByteString :--> PByteString :--> PInteger :--> PBuiltinList (PAsData PFrontierPeak) :--> PBuiltinList (PAsData PByteString) :--> PValueAssetMutationWitnessV1 :--> PFoldStateV1)
pauthenticateInputFoldV1 = phoistAcyclic $ plam $ \state sourceKind key nextSchedule descriptorCbor assetIndex policy asset quantity peaks siblings mutation ->
  pwithControl state $ \control -> pmatch control $ \c -> pmatch state $ \PFoldStateV1{pfoldState'bound} -> pmatch (pfromData pfoldState'bound) $ \PBoundV1{pbound'coordinate} -> pmatch (pfromData pbound'coordinate) $ \PCoordinateV1{..} ->
    pif
      ( pand'List
          [ sourceKind #== 0
          , pfromData (pvalueAndMint'stage c) #== 2
          , pfromData pcoordinate'primaryIndex #== pfromData (pvalueAndMint'replayCursor c)
          , pfromData pcoordinate'assetIndex #== assetIndex
          , pfromData (pvalueAndMint'replayAssetCursor c) #== assetIndex + 1
          , presolutionScheduleNodeHash # sourceKind # key # nextSchedule #== pfromData (pvalueAndMint'replayRemainingScheduleHash c)
          , pblake2b_256 # descriptorCbor #== pfromData (pvalueAndMint'replayValueHash c)
          , Descriptor.pverifyOutputAssetMembership # (Descriptor.pdecodeLedgerOutputCommitment # descriptorCbor) # assetIndex # policy # asset # quantity # peaks # siblings
          ]
      )
      (pfinish state 0 $ pmutationFault control (policy <> asset) quantity mutation)
      perror

pauthenticateOutputFoldV1 :: forall s. Term s (PFoldStateV1 :--> PInteger :--> PByteString :--> PInteger :--> PByteString :--> PByteString :--> PInteger :--> PBuiltinList (PAsData PFrontierPeak) :--> PBuiltinList (PAsData PByteString) :--> PValueAssetMutationWitnessV1 :--> PFoldStateV1)
pauthenticateOutputFoldV1 = phoistAcyclic $ plam $ \state outputIndex descriptorCbor assetIndex policy asset quantity peaks siblings mutation ->
  pwithControl state $ \control -> pmatch control $ \c -> pmatch state $ \PFoldStateV1{pfoldState'bound} -> pmatch (pfromData pfoldState'bound) $ \PBoundV1{pbound'coordinate} -> pmatch (pfromData pbound'coordinate) $ \PCoordinateV1{..} ->
    pif
      ( pand'List
          [ pfromData (pvalueAndMint'stage c) #== 3
          , pfromData pcoordinate'primaryIndex #== outputIndex
          , pfromData pcoordinate'assetIndex #== assetIndex
          , pfromData (pvalueAndMint'outputCursor c) #== outputIndex
          , pfromData (pvalueAndMint'outputAssetCursor c) #== assetIndex + 1
          , pblake2b_256 # descriptorCbor #== pfromData (pvalueAndMint'replayValueHash c)
          , Descriptor.pverifyOutputAssetMembership # (Descriptor.pdecodeLedgerOutputCommitment # descriptorCbor) # assetIndex # policy # asset # quantity # peaks # siblings
          ]
      )
      (pfinish state 1 $ pmutationFault control (policy <> asset) (0 - quantity) mutation)
      perror

pauthenticateMintFoldV1 :: forall s. Term s (PFoldStateV1 :--> PInteger :--> PByteString :--> PByteString :--> PInteger :--> PBuiltinList (PAsData PByteString) :--> PValueAssetMutationWitnessV1 :--> PFoldStateV1)
pauthenticateMintFoldV1 = phoistAcyclic $ plam $ \state mintIndex policy asset quantity siblings mutation ->
  pwithControl state $ \control -> pmatch control $ \c -> pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native -> pmatch state $ \PFoldStateV1{pfoldState'bound} -> pmatch (pfromData pfoldState'bound) $ \PBoundV1{pbound'coordinate} -> pmatch (pfromData pbound'coordinate) $ \PCoordinateV1{pcoordinate'primaryIndex} ->
    pif
      ( pand'List
          [ pfromData (pvalueAndMint'stage c) #== 4
          , pfromData pcoordinate'primaryIndex #== mintIndex
          , pfromData (pvalueAndMint'mintCursor c) #== mintIndex
          , pverifyMembership # pfromData (pnativeControl'mintCount native) # pfromData (pnativeControl'mintPeaks native) # mintIndex # (pmintAssetLeafHash # policy # asset # quantity) # siblings
          ]
      )
      (pfinish state 2 $ pmutationFault control (policy <> asset) quantity mutation)
      perror

pterminalContradictionV1 :: forall s. Term s (PFoldStateV1 :--> PBool)
pterminalContradictionV1 = phoistAcyclic $ plam $ \state -> pmatch state $ \PFoldStateV1{..} ->
  pif
    (pfromData pfoldState'stage #== 3)
    ( pmatch (pfromData pfoldState'decisiveFaultHolds) $ \case
        PDNothing -> perror
        PDJust fault -> pmatch (pfromData pfoldState'bound) $ \PBoundV1{pbound'subject} -> Subject.pterminalContradiction # pfromData pbound'subject # pfromData fault
    )
    perror

pfinish :: forall s. Term s PFoldStateV1 -> Term s PInteger -> Term s PBool -> Term s PFoldStateV1
pfinish state fold fault = pmatch state $ \s@PFoldStateV1{..} -> pmatch (pfromData pfoldState'bound) $ \PBoundV1{pbound'coordinate} -> pmatch (pfromData pbound'coordinate) $ \PCoordinateV1{pcoordinate'fold} ->
  pif
    (pfromData pfoldState'stage #== fold #&& pfromData pcoordinate'fold #== fold #&& pmatch (pfromData pfoldState'decisiveFaultHolds) (\case PDNothing -> pconstant True; PDJust _ -> pconstant False))
    (pcon s{pfoldState'stage = pdata $ fold + 1, pfoldState'decisiveFaultHolds = pdata $ pcon $ PDJust $ pdata fault})
    perror

pwithControl :: forall s. Term s PFoldStateV1 -> (Term s PValueAndMintControlV1 -> Term s PFoldStateV1) -> Term s PFoldStateV1
pwithControl state continuation = pmatch state $ \PFoldStateV1{pfoldState'control} -> pmatch (pfromData pfoldState'control) $ \case
  PDNothing -> perror
  PDJust control -> continuation $ pfromData control

pmutationFault :: forall s. Term s PValueAndMintControlV1 -> Term s PByteString -> Term s PInteger -> Term s PValueAssetMutationWitnessV1 -> Term s PBool
pmutationFault control unit quantity mutation = pmatch control $ \PValueAndMintControlV1{pvalueAndMint'valueAccumulator} -> pmatch (pfromData pvalueAndMint'valueAccumulator) $ \PValueAccumulatorV1{..} -> pmatch mutation $ \PValueAssetMutationWitnessV1{..} ->
  let proof = pcon $ PProof $ pfromData pvalueMutation'deltaProof
      valid = plengthBS # unit #>= 28 #&& plengthBS # unit #<= 60 #&& quantity #/= 0 #&& Mpf.pproofHasAtMostSteps # proof # 16
      root = pfromData pvalueAccumulator'assetRoot
      old = pfromData pvalueMutation'oldDelta
   in pif
        valid
        ( pif
            (pfromData pvalueMutation'deltaWasPresent)
            (pmatch (Mpf.pupdateRoot # root # unit # pcborInt old # pcborInt (old + quantity) # proof) $ \case PNothing -> perror; PJust _ -> pconstant False)
            ( pif
                (old #== 0)
                (pmatch (Mpf.pinsertRoot # root # unit # pcborInt quantity # proof) $ \case PNothing -> perror; PJust _ -> pfromData pvalueAccumulator'seenAssetCount #>= pmaxDistinctAssetCount)
                perror
            )
        )
        perror

ptracePreIsAuthenticated :: forall s. Term s PBoundV1 -> Term s PRootMembershipProof -> Term s PValidationMachineStateV1 -> Term s PValidationTraceProof -> Term s PValueAndMintControlV1 -> Term s PBool
ptracePreIsAuthenticated bound membership pre proof control = P.do
  PBoundV1{..} <- pmatch bound
  subject <- plet $ pfromData pbound'subject
  Subject.PVerdictSubject{Subject.psubject'sourceKind, Subject.psubject'transactionId} <- pmatch subject
  PRootMembershipProof{prootMembership'key, prootMembership'value} <- pmatch membership
  eventKey <- plet $ pcoerceData @PEventKey prootMembership'key
  descriptor <- plet $ pcoerceData @PValidationTraceDescriptorV1 prootMembership'value
  PValidationTraceDescriptorV1{pdescriptor'verdict, pdescriptor'rejectionCodeHash} <- pmatch descriptor
  PValidationMachineStateV1{..} <- pmatch pre
  PValidationTraceProof{ptraceProof'stateHash} <- pmatch proof
  let sourceMatches =
        pif
          (pfromData psubject'sourceKind #== 0)
          (pfromData pmachineState'sourceKind #== pcon PNormal #&& pfromData pdescriptor'verdict #== pcon PAccepted)
          (pfromData pmachineState'sourceKind #== pcon PForced #&& pfromData pdescriptor'verdict #== pcon PRejected #&& pfromData pdescriptor'rejectionCodeHash #== phashRejectionCode # prejectAssetCount)
  pand'List
    [ peventKeyMatchesSubject eventKey subject
    , pfromData pmachineState'eventKeyHash #== pblake2b_256 # (pserialiseData # prootMembership'key)
    , pfromData pmachineState'transactionId #== pfromData psubject'transactionId
    , pfromData pmachineState'phase #== pcon PValueAndMint
    , pfromData pmachineState'workRoot #== phashWorkWitness # pcon PValueAndMint # pfromData pmachineState'programCounter # (pencodeValueAndMintControlV1 # control)
    , pverifyRootMembershipWithBytes membership (pdata $ pcon PValidationTracesRootDomain) (pfromData pbound'validationTracesRoot) (pfromData pbound'validationTraceCount) (pserialiseData # prootMembership'key) (pserialiseData # prootMembership'value)
    , pfromData ptraceProof'stateHash #== phashMachineState # pre
    , pverifyTraceProof # descriptor # proof
    , sourceMatches
    ]

peventKeyMatchesSubject :: forall s. Term s PEventKey -> Term s Subject.PVerdictSubject -> Term s PBool
peventKeyMatchesSubject eventKey subject = pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'sourceKind, Subject.psubject'transactionId, Subject.psubject'sourceKey} -> pmatch eventKey $ \case
  PL2TransactionEventKey txId -> pfromData psubject'sourceKind #== 0 #&& pfromData txId #== pfromData psubject'transactionId
  PForcedTransactionEventKey txOrderId -> pfromData psubject'sourceKind #== 1 #&& pserialiseData # pforgetData txOrderId #== pfromData psubject'sourceKey
  _ -> pconstant False

pcoerceData :: forall a s. (PIsData a) => Term s PData -> Term s a
pcoerceData = pfromData . punsafeCoerce
