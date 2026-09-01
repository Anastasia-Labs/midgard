{- |
Module      : Midgard.CekMachine
Description : Plutarch port of @lib/midgard/cek-machine-v1.ak@.

The authenticated CEK transition verifier. Its data constructors deliberately
follow the Aiken source order because they are an off-chain ABI as well as an
on-chain witness format.
-}
module Midgard.CekMachine (
  PMachineStateV1 (..),
  PEnvironmentSummaryV1 (..),
  PMapConversionControlV1 (..),
  PMapConversionStartWitnessV1 (..),
  PMachineValueWitnessV1 (..),
  PCoreStepWitnessV1 (..),
  PCoreStepEvidenceV1 (..),
  pmodeCompute,
  pmodeReturn,
  pmodeLookup,
  pmodeBuiltin,
  pmodeHaltSuccess,
  pmodeHaltError,
  pmodeCaseSelect,
  pmodeCaseApply,
  pmodeSemanticBuiltin,
  perrorExplicit,
  perrorUnboundVariable,
  perrorInvalidApplication,
  perrorInvalidForce,
  perrorNonconstantHalt,
  perrorInvalidCaseScrutinee,
  perrorCaseBranchMissing,
  perrorBuiltinFailure,
  pstateIsWellFormed,
  phashStateV1,
  pbuiltinForceCount,
  pbuiltinArgumentCount,
  pencodeMapConversionControlV1,
  phashMapConversionControlV1,
  pverifyComputeStep,
  pverifyLookupStep,
  pverifyReturnStep,
  pverifyCaseSelectStep,
  pverifyCaseApplyStep,
  pverifyBuiltinStep,
  pverifyBuiltinDirectStep,
  pverifyBuiltinSemanticStep,
  pverifyBuiltinFailureStep,
  pverifyBuiltinSemanticFailureStep,
  pverifyBuiltinTypeFailureStep,
  pverifyBuiltinBlsFinalStep,
  pverifySemanticBuiltinControlStep,
  pverifyCoreStepV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Midgard.CekBuiltin qualified as Builtin
import Midgard.CekConstant (PConstantWitnessV1)
import Midgard.CekConstant qualified as Constant
import Midgard.CekCost (PBuiltinBudgetV1 (..))
import Midgard.CekData qualified as Data
import Midgard.CekProof qualified as Proof
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteArrayHeader, pencodeDefiniteBytes)

pmodeCompute, pmodeReturn, pmodeLookup, pmodeBuiltin, pmodeHaltSuccess, pmodeHaltError, pmodeCaseSelect, pmodeCaseApply, pmodeSemanticBuiltin :: forall s. Term s PInteger
pmodeCompute = 0
pmodeReturn = 1
pmodeLookup = 2
pmodeBuiltin = 3
pmodeHaltSuccess = 4
pmodeHaltError = 5
pmodeCaseSelect = 6
pmodeCaseApply = 7
pmodeSemanticBuiltin = 8

perrorExplicit, perrorUnboundVariable, perrorInvalidApplication, perrorInvalidForce, perrorNonconstantHalt, perrorInvalidCaseScrutinee, perrorCaseBranchMissing, perrorBuiltinFailure :: forall s. Term s PInteger
perrorExplicit = 0
perrorUnboundVariable = 1
perrorInvalidApplication = 2
perrorInvalidForce = 3
perrorNonconstantHalt = 4
perrorInvalidCaseScrutinee = 5
perrorCaseBranchMissing = 6
perrorBuiltinFailure = 7

pmachineStepCpu, pmachineStepMemory :: forall s. Term s PInteger
pmachineStepCpu = 16000
pmachineStepMemory = 100

data PMachineStateV1 (s :: S) = PMachineStateV1
  { pstate'mode :: Term s (PAsData PInteger)
  , pstate'executionIndex :: Term s (PAsData PInteger)
  , pstate'focusRoot :: Term s (PAsData PByteString)
  , pstate'environmentRoot :: Term s (PAsData PByteString)
  , pstate'continuationRoot :: Term s (PAsData PByteString)
  , pstate'auxiliary :: Term s (PAsData PInteger)
  , pstate'cpu :: Term s (PAsData PInteger)
  , pstate'memory :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMachineStateV1)

data PEnvironmentSummaryV1 (s :: S)
  = PEmptyEnvironmentSummary
  | PNonEmptyEnvironmentSummary
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PEnvironmentSummaryV1)

data PMapConversionControlV1 (s :: S) = PMapConversionControlV1
  { pmap'tag :: Term s (PAsData PInteger)
  , pmap'resultRoot :: Term s (PAsData PByteString)
  , pmap'sourceRoot :: Term s (PAsData PByteString)
  , pmap'sourceRemaining :: Term s (PAsData PInteger)
  , pmap'sourcePayloadCborLength :: Term s (PAsData PInteger)
  , pmap'sourceMemory :: Term s (PAsData PInteger)
  , pmap'destinationRoot :: Term s (PAsData PByteString)
  , pmap'destinationRemaining :: Term s (PAsData PInteger)
  , pmap'destinationPayloadCborLength :: Term s (PAsData PInteger)
  , pmap'destinationMemory :: Term s (PAsData PInteger)
  , pmap'budgetCpu :: Term s (PAsData PInteger)
  , pmap'budgetMemory :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMapConversionControlV1)

data PMapConversionStartWitnessV1 (s :: S) = PMapConversionStartWitnessV1
  { pmapStart'sourceNode :: Term s (PAsData Data.PDataNodeV1)
  , pmapStart'sourceList :: Term s (PAsData (PMaybeData Data.PDataListNodeV1))
  , pmapStart'sourcePairs :: Term s (PAsData (PMaybeData Data.PDataPairNodeV1))
  , pmapStart'resultNode :: Term s (PAsData Data.PDataNodeV1)
  , pmapStart'resultList :: Term s (PAsData (PMaybeData Data.PDataListNodeV1))
  , pmapStart'resultPairs :: Term s (PAsData (PMaybeData Data.PDataPairNodeV1))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMapConversionStartWitnessV1)

data PMachineValueWitnessV1 (s :: S)
  = PMachineConstantValue
      (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
      (Term s (PAsData PInteger)) (Term s (PAsData PByteString)) (Term s (PAsData PInteger))
  | PMachineLambdaValue (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
  | PMachineDelayValue (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
  | PMachineConstrValue (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PByteString))
  | PMachineBuiltinValue
      (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
      (Term s (PAsData PInteger)) (Term s (PAsData PByteString))
  | PMachineBlsMillerLoopValue (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMachineValueWitnessV1)

data PCoreStepWitnessV1 (s :: S)
  = PComputeVariable (Term s (PAsData PInteger))
  | PComputeConstant (Term s (PAsData PConstantWitnessV1))
  | PComputeLambda (Term s (PAsData PByteString))
  | PComputeDelay (Term s (PAsData PByteString))
  | PComputeApplication (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
  | PComputeForce (Term s (PAsData PByteString))
  | PComputeError
  | PComputeBuiltin (Term s (PAsData PInteger))
  | PComputeConstrEmpty (Term s (PAsData PInteger))
  | PComputeConstrNonEmpty
      (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
      (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
  | PComputeCase
      (Term s (PAsData PByteString)) (Term s (PAsData PInteger)) (Term s (PAsData PByteString))
  | PLookupEnvironment
      (Term s (PAsData PByteString)) (Term s (PAsData PByteString)) (Term s (PAsData PInteger))
  | PLookupEmptyEnvironment
  | PReturnEmptyContinuation (Term s (PAsData PMachineValueWitnessV1))
  | PReturnApplyArgument
      (Term s (PAsData PByteString)) (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
  | PReturnApplyLambda
      (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
      (Term s (PAsData PEnvironmentSummaryV1)) (Term s (PAsData PByteString))
  | PReturnApplyBuiltin
      (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
      (Term s (PAsData PInteger)) (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
  | PReturnApplyInvalid (Term s (PAsData PMachineValueWitnessV1)) (Term s (PAsData PByteString))
  | PReturnApplyValueLambda
      (Term s (PAsData PByteString)) (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
      (Term s (PAsData PEnvironmentSummaryV1)) (Term s (PAsData PByteString))
  | PReturnApplyValueBuiltin
      (Term s (PAsData PByteString)) (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
      (Term s (PAsData PInteger)) (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
  | PReturnApplyValueInvalid
      (Term s (PAsData PByteString)) (Term s (PAsData PMachineValueWitnessV1)) (Term s (PAsData PByteString))
  | PReturnForceDelay
      (Term s (PAsData PByteString)) (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
  | PReturnForceBuiltin
      (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
      (Term s (PAsData PInteger)) (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
  | PReturnForceInvalid (Term s (PAsData PMachineValueWitnessV1)) (Term s (PAsData PByteString))
  | PReturnConstrNext
      (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PByteString))
      (Term s (PAsData PByteString)) (Term s (PAsData PInteger)) (Term s (PAsData PByteString))
      (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
  | PReturnConstrDone
      (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PByteString))
      (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
  | PReturnCaseConstr
      (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PByteString))
      (Term s (PAsData PInteger)) (Term s (PAsData PByteString))
      (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
  | PReturnCaseInvalid
      (Term s (PAsData PMachineValueWitnessV1)) (Term s (PAsData PInteger))
      (Term s (PAsData PByteString)) (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
  | PSelectCaseBranch
      (Term s (PAsData PByteString)) (Term s (PAsData PByteString)) (Term s (PAsData PInteger))
      (Term s (PAsData PByteString)) (Term s (PAsData PByteString)) (Term s (PAsData PInteger))
  | PApplyCaseValue
      (Term s (PAsData PByteString)) (Term s (PAsData PByteString)) (Term s (PAsData PInteger))
      (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
  | PExecuteBuiltinDirect
      (Term s (PAsData PInteger)) (Term s (PAsData (PBuiltinList (PAsData Builtin.PValueWitnessV1))))
      (Term s (PAsData Builtin.PValueWitnessV1))
  | PExecuteBuiltinSemantic
      (Term s (PAsData PInteger)) (Term s (PAsData (PBuiltinList (PAsData Builtin.PValueWitnessV1))))
      (Term s (PAsData Builtin.PValueWitnessV1)) (Term s (PAsData Builtin.PSemanticBuiltinWitnessV1))
  | PStartBuiltinMapConversion
      (Term s (PAsData PInteger)) (Term s (PAsData (PBuiltinList (PAsData Builtin.PValueWitnessV1))))
      (Term s (PAsData Builtin.PValueWitnessV1)) (Term s (PAsData PMapConversionStartWitnessV1))
  | PStepBuiltinListToMap
      (Term s (PAsData PMapConversionControlV1)) (Term s (PAsData Data.PDataListNodeV1))
      (Term s (PAsData Data.PDataNodeV1)) (Term s (PAsData Data.PDataListNodeV1))
      (Term s (PAsData Data.PDataListNodeV1)) (Term s (PAsData Data.PDataNodeV1))
      (Term s (PAsData Data.PDataNodeV1)) (Term s (PAsData Data.PDataPairNodeV1))
  | PStepBuiltinMapToList
      (Term s (PAsData PMapConversionControlV1)) (Term s (PAsData Data.PDataPairNodeV1))
      (Term s (PAsData Data.PDataListNodeV1)) (Term s (PAsData Data.PDataNodeV1))
      (Term s (PAsData Data.PDataListNodeV1)) (Term s (PAsData Data.PDataListNodeV1))
      (Term s (PAsData Data.PDataNodeV1)) (Term s (PAsData Data.PDataNodeV1))
  | PFinishBuiltinMapConversion (Term s (PAsData PMapConversionControlV1))
  | PExecuteBuiltinSemanticFailure
      (Term s (PAsData PInteger)) (Term s (PAsData (PBuiltinList (PAsData Builtin.PValueWitnessV1))))
      (Term s (PAsData Builtin.PSemanticBuiltinWitnessV1))
  | PExecuteBuiltinBlsFinal
      (Term s (PAsData PByteString)) (Term s (PAsData PByteString))
      (Term s (PAsData Builtin.PBlsExpressionWitnessV1)) (Term s (PAsData Builtin.PBlsExpressionWitnessV1))
      (Term s (PAsData Builtin.PValueWitnessV1))
  | PExecuteBuiltinFailure
      (Term s (PAsData PInteger)) (Term s (PAsData (PBuiltinList (PAsData Builtin.PValueWitnessV1))))
  | PExecuteBuiltinTypeFailure
      (Term s (PAsData PInteger)) (Term s (PAsData (PBuiltinList (PAsData Builtin.PRuntimeValueWitnessV1))))
  | PComputeContextConstant (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCoreStepWitnessV1)

data PCoreStepEvidenceV1 (s :: S) = PCoreStepEvidenceV1
  (Term s (PAsData PMachineStateV1))
  (Term s (PAsData PMachineStateV1))
  (Term s (PAsData PCoreStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCoreStepEvidenceV1)

prootIsHash :: forall s. Term s PByteString -> Term s PBool
prootIsHash root = plengthBS # root #== 32

pstateIsWellFormed :: forall s. Term s (PMachineStateV1 :--> PBool)
pstateIsWellFormed = phoistAcyclic $ plam $ \state -> pmatch state $ \s ->
  pand'List
    [ pmodeCompute #<= pfromData (pstate'mode s)
    , pfromData (pstate'mode s) #<= pmodeSemanticBuiltin
    , 0 #<= pfromData (pstate'executionIndex s)
    , pfromData (pstate'executionIndex s) #<= 4294967295
    , prootIsHash $ pfromData (pstate'focusRoot s)
    , prootIsHash $ pfromData (pstate'environmentRoot s)
    , prootIsHash $ pfromData (pstate'continuationRoot s)
    , 0 #<= pfromData (pstate'auxiliary s)
    , pfromData (pstate'auxiliary s) #<= 18446744073709551615
    , 0 #<= pfromData (pstate'cpu s)
    , pfromData (pstate'cpu s) #<= 18446744073709551615
    , 0 #<= pfromData (pstate'memory s)
    , pfromData (pstate'memory s) #<= 18446744073709551615
    ]

phashStateV1 :: forall s. Term s (PMachineStateV1 :--> PByteString)
phashStateV1 = phoistAcyclic $ plam $ \state ->
  pif (pstateIsWellFormed # state)
    (pmatch state $ \s -> Proof.phashMachineStateV1
      # pfromData (pstate'mode s) # pfromData (pstate'executionIndex s)
      # pfromData (pstate'focusRoot s) # pfromData (pstate'environmentRoot s)
      # pfromData (pstate'continuationRoot s) # pfromData (pstate'auxiliary s)
      # pfromData (pstate'cpu s) # pfromData (pstate'memory s))
    perror

pbuiltinForceCount :: forall s. Term s (PInteger :--> PInteger)
pbuiltinForceCount = phoistAcyclic $ plam $ \tag ->
  pif (0 #<= tag #&& tag #<= Proof.pmaxBuiltinTag)
    (pif (tag #== 29 #|| tag #== 30 #|| tag #== 31) 2 $
      pif (tag #== 26 #|| tag #== 27 #|| tag #== 28 #|| tag #== 32
        #|| tag #== 33 #|| tag #== 34 #|| tag #== 35 #|| tag #== 36) 1 0)
    perror

pbuiltinArgumentCount :: forall s. Term s (PInteger :--> PInteger)
pbuiltinArgumentCount = phoistAcyclic $ plam $ \tag ->
  pif (0 #<= tag #&& tag #<= Proof.pmaxBuiltinTag)
    (pif (tag #== 36) 6 $
      pif (tag #== 12 #|| tag #== 21 #|| tag #== 26 #|| tag #== 31 #|| tag #== 52
        #|| tag #== 53 #|| tag #== 73 #|| tag #== 75 #|| tag #== 76 #|| tag #== 77 #|| tag #== 80) 3 $
      pif (tag #<= 11 #|| tag #== 14 #|| tag #== 15 #|| tag #== 16 #|| tag #== 17
        #|| tag #== 22 #|| tag #== 23 #|| tag #== 27 #|| tag #== 28 #|| tag #== 32
        #|| tag #== 37 #|| tag #== 47 #|| tag #== 48 #|| tag #== 54 #|| tag #== 56
        #|| tag #== 57 #|| tag #== 58 #|| tag #== 61 #|| tag #== 63 #|| tag #== 64
        #|| tag #== 65 #|| tag #== 68 #|| tag #== 69 #|| tag #== 70 #|| tag #== 74
        #|| tag #== 79 #|| tag #== 81 #|| tag #== 82 #|| tag #== 83) 2 1)
    perror

pencodeMapConversionControlV1 :: forall s. Term s (PMapConversionControlV1 :--> PByteString)
pencodeMapConversionControlV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pencodeDefiniteArrayHeader # 12
    <> pcborInt (pfromData $ pmap'tag c)
    <> (pencodeDefiniteBytes # pfromData (pmap'resultRoot c))
    <> (pencodeDefiniteBytes # pfromData (pmap'sourceRoot c))
    <> pcborInt (pfromData $ pmap'sourceRemaining c)
    <> pcborInt (pfromData $ pmap'sourcePayloadCborLength c)
    <> pcborInt (pfromData $ pmap'sourceMemory c)
    <> (pencodeDefiniteBytes # pfromData (pmap'destinationRoot c))
    <> pcborInt (pfromData $ pmap'destinationRemaining c)
    <> pcborInt (pfromData $ pmap'destinationPayloadCborLength c)
    <> pcborInt (pfromData $ pmap'destinationMemory c)
    <> pcborInt (pfromData $ pmap'budgetCpu c)
    <> pcborInt (pfromData $ pmap'budgetMemory c)

pmapConversionControlIsWellFormedV1 :: forall s. Term s (PMapConversionControlV1 :--> PBool)
pmapConversionControlIsWellFormedV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  plet (pfromData $ pmap'sourceRemaining c) $ \remaining ->
  pand'List
    [ pfromData (pmap'tag c) #== 38 #|| pfromData (pmap'tag c) #== 43
    , prootIsHash $ pfromData (pmap'resultRoot c)
    , prootIsHash $ pfromData (pmap'sourceRoot c)
    , prootIsHash $ pfromData (pmap'destinationRoot c)
    , 0 #<= remaining
    , remaining #== pfromData (pmap'destinationRemaining c)
    , 0 #<= pfromData (pmap'sourcePayloadCborLength c)
    , 0 #<= pfromData (pmap'sourceMemory c)
    , 0 #<= pfromData (pmap'destinationPayloadCborLength c)
    , 0 #<= pfromData (pmap'destinationMemory c)
    , 0 #<= pfromData (pmap'budgetCpu c)
    , 0 #<= pfromData (pmap'budgetMemory c)
    , pif (remaining #== 0)
        (pfromData (pmap'sourcePayloadCborLength c) #== 0
          #&& pfromData (pmap'sourceMemory c) #== 0
          #&& pfromData (pmap'destinationPayloadCborLength c) #== 0
          #&& pfromData (pmap'destinationMemory c) #== 0
          #&& pif (pfromData (pmap'tag c) #== 38)
            (pfromData (pmap'sourceRoot c) #== Data.pemptyDataListRootV1
              #&& pfromData (pmap'destinationRoot c) #== Data.pemptyDataPairRootV1)
            (pfromData (pmap'sourceRoot c) #== Data.pemptyDataPairRootV1
              #&& pfromData (pmap'destinationRoot c) #== Data.pemptyDataListRootV1))
        (pconstant True)
    ]

phashMapConversionControlV1 :: forall s. Term s (PMapConversionControlV1 :--> PByteString)
phashMapConversionControlV1 = phoistAcyclic $ plam $ \control ->
  pif (pmapConversionControlIsWellFormedV1 # control)
    (pblake2b_256 # (pconstant "MidgardCekMapConversionControlV1" <> (pencodeMapConversionControlV1 # control)))
    perror

pdataSummaryFromNode :: forall s. Term s Data.PDataNodeV1 -> Term s Data.PDataSummaryV1
pdataSummaryFromNode node =
  pcon $ Data.PDataSummaryV1
    (pdata $ Data.phashDataNodeV1 # node)
    (pdata $ Data.pdataNodeCborLengthV1 # node)
    (pdata $ Data.pdataNodeMemoryV1 # node)

plistSequenceFromNode :: forall s. Term s Data.PDataNodeV1 -> Term s Data.PDataSequenceSummaryV1
plistSequenceFromNode node = pmatch node $ \case
  Data.PListDataNode {Data.pnode'itemsCount, Data.pnode'itemsRoot, Data.pnode'memory} ->
    pcon $ Data.PDataSequenceSummaryV1
      pnode'itemsRoot pnode'itemsCount
      (pdata $ Data.pdataNodeCborLengthV1 # node - pif (pfromData pnode'itemsCount #== 0) 1 2)
      (pdata $ pfromData pnode'memory - 4)
  _ -> perror

pmapSequenceFromNode :: forall s. Term s Data.PDataNodeV1 -> Term s Data.PDataSequenceSummaryV1
pmapSequenceFromNode node = pmatch node $ \case
  Data.PMapDataNode {Data.pnode'entriesCount, Data.pnode'entriesRoot, Data.pnode'memory} ->
    plet (pfromData pnode'entriesCount) $ \count ->
    plet (pif (count #< 24) 1 $ pif (count #<= 255) 2 $ pif (count #<= 65535) 3 5) $ \headerLength ->
      pcon $ Data.PDataSequenceSummaryV1
        pnode'entriesRoot pnode'entriesCount
        (pdata $ Data.pdataNodeCborLengthV1 # node - headerLength)
        (pdata $ pfromData pnode'memory - 4)
  _ -> perror

ppairWrapperIsExact ::
  forall s.
  Term s Data.PDataNodeV1 -> Term s Data.PDataListNodeV1 -> Term s Data.PDataListNodeV1 ->
  Term s Data.PDataNodeV1 -> Term s Data.PDataNodeV1 -> Term s PBool
ppairWrapperIsExact pair first second key value =
  Data.pverifyDataNodeV1 # pair # (pcon $ PDJust $ pdata first) # (pcon PDNothing)
    #&& pmatch pair (\case
      Data.PConstrSmallData {Data.pnode'constructor, Data.pnode'fieldsCount} ->
        pfromData pnode'constructor #== 0 #&& pfromData pnode'fieldsCount #== 2
      _ -> pconstant False)
    #&& Data.pverifyDataListLinkV1 # first # key # (pcon $ PDJust $ pdata second)
    #&& Data.pverifyDataListLinkV1 # second # value # (pcon PDNothing)

pnextListToMapControl ::
  forall s. PMapConversionControlV1 s -> Data.PDataListNodeV1 s -> Data.PDataPairNodeV1 s -> Term s PMapConversionControlV1
pnextListToMapControl control source destination =
  pcon $ PMapConversionControlV1
    (pmap'tag control)
    (pmap'resultRoot control)
    (Data.plistNode'tail source)
    (pdata $ pfromData (pmap'sourceRemaining control) - 1)
    (pdata $ pfromData (pmap'sourcePayloadCborLength control) - pfromData (Data.plistNode'headCborLength source))
    (pdata $ pfromData (pmap'sourceMemory control) - pfromData (Data.plistNode'headMemory source))
    (Data.ppairNode'tail destination)
    (pdata $ pfromData (pmap'destinationRemaining control) - 1)
    (pdata $ pfromData (pmap'destinationPayloadCborLength control)
      - pfromData (Data.ppairNode'keyCborLength destination) - pfromData (Data.ppairNode'valueCborLength destination))
    (pdata $ pfromData (pmap'destinationMemory control)
      - pfromData (Data.ppairNode'keyMemory destination) - pfromData (Data.ppairNode'valueMemory destination))
    (pmap'budgetCpu control)
    (pmap'budgetMemory control)

pverifyBuiltinListToMapStep ::
  forall s.
  Term s PMachineStateV1 -> Term s PMachineStateV1 -> Term s PMapConversionControlV1 ->
  Term s Data.PDataListNodeV1 -> Term s Data.PDataNodeV1 -> Term s Data.PDataListNodeV1 ->
  Term s Data.PDataListNodeV1 -> Term s Data.PDataNodeV1 -> Term s Data.PDataNodeV1 ->
  Term s Data.PDataPairNodeV1 -> Term s PBool
pverifyBuiltinListToMapStep pre post control source pair first second key value destination =
  pmatch pre $ \p -> pmatch control $ \c -> pmatch source $ \sourceNode -> pmatch destination $ \destinationNode ->
  pmatch (pdataSummaryFromNode key) $ \keySummary -> pmatch (pdataSummaryFromNode value) $ \valueSummary ->
  pmatch (pdataSummaryFromNode pair) $ \pairSummary ->
  pif (pfromData (pmap'tag c) #== 38 #&& 0 #< pfromData (pmap'sourceRemaining c))
    (pfromData (pstate'focusRoot p) #== phashMapConversionControlV1 # control
      #&& Data.phashDataListNodeV1 # source #== pfromData (pmap'sourceRoot c)
      #&& pfromData (Data.plistNode'length sourceNode) #== pfromData (pmap'sourceRemaining c)
      #&& pfromData (Data.plistNode'payloadCborLength sourceNode) #== pfromData (pmap'sourcePayloadCborLength c)
      #&& pfromData (Data.plistNode'memory sourceNode) #== pfromData (pmap'sourceMemory c)
      #&& pfromData (Data.plistNode'head sourceNode) #== pfromData (Data.psummary'root pairSummary)
      #&& pfromData (Data.plistNode'headCborLength sourceNode) #== pfromData (Data.psummary'cborLength pairSummary)
      #&& pfromData (Data.plistNode'headMemory sourceNode) #== pfromData (Data.psummary'memory pairSummary)
      #&& ppairWrapperIsExact pair first second key value
      #&& Data.phashDataPairNodeV1 # destination #== pfromData (pmap'destinationRoot c)
      #&& pfromData (Data.ppairNode'length destinationNode) #== pfromData (pmap'destinationRemaining c)
      #&& pfromData (Data.ppairNode'payloadCborLength destinationNode) #== pfromData (pmap'destinationPayloadCborLength c)
      #&& pfromData (Data.ppairNode'memory destinationNode) #== pfromData (pmap'destinationMemory c)
      #&& pfromData (Data.ppairNode'key destinationNode) #== pfromData (Data.psummary'root keySummary)
      #&& pfromData (Data.ppairNode'keyCborLength destinationNode) #== pfromData (Data.psummary'cborLength keySummary)
      #&& pfromData (Data.ppairNode'keyMemory destinationNode) #== pfromData (Data.psummary'memory keySummary)
      #&& pfromData (Data.ppairNode'value destinationNode) #== pfromData (Data.psummary'root valueSummary)
      #&& pfromData (Data.ppairNode'valueCborLength destinationNode) #== pfromData (Data.psummary'cborLength valueSummary)
      #&& pfromData (Data.ppairNode'valueMemory destinationNode) #== pfromData (Data.psummary'memory valueSummary)
      #&& post #== pexactState pre pmodeSemanticBuiltin
        (phashMapConversionControlV1 # pnextListToMapControl c sourceNode destinationNode)
        (pfromData $ pstate'environmentRoot p) (pfromData $ pstate'continuationRoot p) 0 0 0)
    perror

pnextMapToListControl ::
  forall s. PMapConversionControlV1 s -> Data.PDataPairNodeV1 s -> Data.PDataListNodeV1 s -> Term s PMapConversionControlV1
pnextMapToListControl control source destination =
  pcon $ PMapConversionControlV1
    (pmap'tag control)
    (pmap'resultRoot control)
    (Data.ppairNode'tail source)
    (pdata $ pfromData (pmap'sourceRemaining control) - 1)
    (pdata $ pfromData (pmap'sourcePayloadCborLength control)
      - pfromData (Data.ppairNode'keyCborLength source) - pfromData (Data.ppairNode'valueCborLength source))
    (pdata $ pfromData (pmap'sourceMemory control)
      - pfromData (Data.ppairNode'keyMemory source) - pfromData (Data.ppairNode'valueMemory source))
    (Data.plistNode'tail destination)
    (pdata $ pfromData (pmap'destinationRemaining control) - 1)
    (pdata $ pfromData (pmap'destinationPayloadCborLength control) - pfromData (Data.plistNode'headCborLength destination))
    (pdata $ pfromData (pmap'destinationMemory control) - pfromData (Data.plistNode'headMemory destination))
    (pmap'budgetCpu control)
    (pmap'budgetMemory control)

pverifyBuiltinMapToListStep ::
  forall s.
  Term s PMachineStateV1 -> Term s PMachineStateV1 -> Term s PMapConversionControlV1 ->
  Term s Data.PDataPairNodeV1 -> Term s Data.PDataListNodeV1 -> Term s Data.PDataNodeV1 ->
  Term s Data.PDataListNodeV1 -> Term s Data.PDataListNodeV1 -> Term s Data.PDataNodeV1 ->
  Term s Data.PDataNodeV1 -> Term s PBool
pverifyBuiltinMapToListStep pre post control source destination pair first second key value =
  pmatch pre $ \p -> pmatch control $ \c -> pmatch source $ \sourceNode -> pmatch destination $ \destinationNode ->
  pmatch (pdataSummaryFromNode key) $ \keySummary -> pmatch (pdataSummaryFromNode value) $ \valueSummary ->
  pmatch (pdataSummaryFromNode pair) $ \pairSummary ->
  pif (pfromData (pmap'tag c) #== 43 #&& 0 #< pfromData (pmap'sourceRemaining c))
    (pfromData (pstate'focusRoot p) #== phashMapConversionControlV1 # control
      #&& Data.phashDataPairNodeV1 # source #== pfromData (pmap'sourceRoot c)
      #&& pfromData (Data.ppairNode'length sourceNode) #== pfromData (pmap'sourceRemaining c)
      #&& pfromData (Data.ppairNode'payloadCborLength sourceNode) #== pfromData (pmap'sourcePayloadCborLength c)
      #&& pfromData (Data.ppairNode'memory sourceNode) #== pfromData (pmap'sourceMemory c)
      #&& pfromData (Data.ppairNode'key sourceNode) #== pfromData (Data.psummary'root keySummary)
      #&& pfromData (Data.ppairNode'keyCborLength sourceNode) #== pfromData (Data.psummary'cborLength keySummary)
      #&& pfromData (Data.ppairNode'keyMemory sourceNode) #== pfromData (Data.psummary'memory keySummary)
      #&& pfromData (Data.ppairNode'value sourceNode) #== pfromData (Data.psummary'root valueSummary)
      #&& pfromData (Data.ppairNode'valueCborLength sourceNode) #== pfromData (Data.psummary'cborLength valueSummary)
      #&& pfromData (Data.ppairNode'valueMemory sourceNode) #== pfromData (Data.psummary'memory valueSummary)
      #&& ppairWrapperIsExact pair first second key value
      #&& Data.phashDataListNodeV1 # destination #== pfromData (pmap'destinationRoot c)
      #&& pfromData (Data.plistNode'length destinationNode) #== pfromData (pmap'destinationRemaining c)
      #&& pfromData (Data.plistNode'payloadCborLength destinationNode) #== pfromData (pmap'destinationPayloadCborLength c)
      #&& pfromData (Data.plistNode'memory destinationNode) #== pfromData (pmap'destinationMemory c)
      #&& pfromData (Data.plistNode'head destinationNode) #== pfromData (Data.psummary'root pairSummary)
      #&& pfromData (Data.plistNode'headCborLength destinationNode) #== pfromData (Data.psummary'cborLength pairSummary)
      #&& pfromData (Data.plistNode'headMemory destinationNode) #== pfromData (Data.psummary'memory pairSummary)
      #&& post #== pexactState pre pmodeSemanticBuiltin
        (phashMapConversionControlV1 # pnextMapToListControl c sourceNode destinationNode)
        (pfromData $ pstate'environmentRoot p) (pfromData $ pstate'continuationRoot p) 0 0 0)
    perror

pverifyBuiltinMapConversionFinish ::
  forall s. Term s PMachineStateV1 -> Term s PMachineStateV1 -> Term s PMapConversionControlV1 -> Term s PBool
pverifyBuiltinMapConversionFinish pre post control = pmatch pre $ \p -> pmatch control $ \c ->
  pmapConversionControlIsWellFormedV1 # control
    #&& pfromData (pmap'sourceRemaining c) #== 0
    #&& pfromData (pstate'focusRoot p) #== phashMapConversionControlV1 # control
    #&& post #== pexactState pre pmodeReturn (pfromData $ pmap'resultRoot c) Proof.pemptyEnvironmentRootV1
      (pfromData $ pstate'continuationRoot p) 0
      (pfromData $ pmap'budgetCpu c) (pfromData $ pmap'budgetMemory c)

pvalueWitnessHash :: forall s. Term s PMachineValueWitnessV1 -> Term s PByteString
pvalueWitnessHash value = pmatch value $ \case
  PMachineConstantValue typeRoot payloadRoot payloadLength semanticRoot memory ->
    Proof.phashConstantValueV1 # pfromData typeRoot # pfromData payloadRoot # pfromData payloadLength
      # pfromData semanticRoot # pfromData memory
  PMachineLambdaValue body environment -> Proof.phashLambdaValueV1 # pfromData body # pfromData environment
  PMachineDelayValue body environment -> Proof.phashDelayValueV1 # pfromData body # pfromData environment
  PMachineConstrValue tag count root -> Proof.phashConstrValueV1 # pfromData tag # pfromData count # pfromData root
  PMachineBuiltinValue tag forces count root ->
    Proof.phashBuiltinValueV1 # pfromData tag # pfromData forces # pfromData count # pfromData root
  PMachineBlsMillerLoopValue root -> Proof.phashBlsMillerLoopValueV1 # pfromData root

pvalueIsConstant :: forall s. Term s PMachineValueWitnessV1 -> Term s PBool
pvalueIsConstant value = pmatch value $ \case PMachineConstantValue {} -> pconstant True; _ -> pconstant False

pvalueIsLambdaOrBuiltin :: forall s. Term s PMachineValueWitnessV1 -> Term s PBool
pvalueIsLambdaOrBuiltin value = pmatch value $ \case
  PMachineLambdaValue {} -> pconstant True
  PMachineBuiltinValue {} -> pconstant True
  _ -> pconstant False

pvalueIsDelayOrForceableBuiltin :: forall s. Term s PMachineValueWitnessV1 -> Term s PBool
pvalueIsDelayOrForceableBuiltin value = pmatch value $ \case
  PMachineDelayValue {} -> pconstant True
  PMachineBuiltinValue _ forces _ _ -> 0 #< pfromData forces
  _ -> pconstant False

pvalueIsConstr :: forall s. Term s PMachineValueWitnessV1 -> Term s PBool
pvalueIsConstr value = pmatch value $ \case PMachineConstrValue {} -> pconstant True; _ -> pconstant False

plinkedSequenceTailIsWellFormed :: forall s. Term s PByteString -> Term s PInteger -> Term s PBool
plinkedSequenceTailIsWellFormed tailRoot len =
  pif (len #== 1) (tailRoot #== Proof.pemptySequenceRootV1) (tailRoot #/= Proof.pemptySequenceRootV1)

plinkedSequenceRootIsWellFormed :: forall s. Term s PByteString -> Term s PInteger -> Term s PBool
plinkedSequenceRootIsWellFormed root count =
  pif (count #== 0) (root #== Proof.pemptySequenceRootV1) (root #/= Proof.pemptySequenceRootV1)

pexactState ::
  forall s.
  Term s PMachineStateV1 -> Term s PInteger -> Term s PByteString -> Term s PByteString ->
  Term s PByteString -> Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PMachineStateV1
pexactState pre mode focus environment continuation auxiliary cpuDelta memoryDelta = pmatch pre $ \p ->
  pcon $ PMachineStateV1
    (pdata mode)
    (pstate'executionIndex p)
    (pdata focus)
    (pdata environment)
    (pdata continuation)
    (pdata auxiliary)
    (pdata $ pfromData (pstate'cpu p) + cpuDelta)
    (pdata $ pfromData (pstate'memory p) + memoryDelta)

pexactComputeSuccessor ::
  forall s.
  Term s PMachineStateV1 -> Term s PInteger -> Term s PByteString -> Term s PByteString ->
  Term s PByteString -> Term s PInteger -> Term s PMachineStateV1
pexactComputeSuccessor pre mode focus environment continuation auxiliary =
  pexactState pre mode focus environment continuation auxiliary pmachineStepCpu pmachineStepMemory

perrorSuccessor :: forall s. Term s PMachineStateV1 -> Term s PInteger -> Term s PMachineStateV1
perrorSuccessor pre reason =
  pexactState pre pmodeHaltError Proof.phashErrorTermV1 Proof.pemptyEnvironmentRootV1
    Proof.pemptyContinuationRootV1 reason 0 0

pverifyComputeStep ::
  forall s. Term s (PMachineStateV1 :--> PMachineStateV1 :--> PCoreStepWitnessV1 :--> PBool)
pverifyComputeStep = phoistAcyclic $ plam $ \pre post witness -> pmatch pre $ \p -> pmatch witness $ \case
  PComputeVariable index ->
    pfromData (pstate'focusRoot p) #== Proof.phashVariableTermV1 # pfromData index
      #&& post #== pexactComputeSuccessor pre pmodeLookup
        (pfromData $ pstate'environmentRoot p) (pfromData $ pstate'environmentRoot p)
        (pfromData $ pstate'continuationRoot p) (pfromData index)
  PComputeConstant value ->
    plet (Builtin.presultRootV1 # pcon (Builtin.PConstantValue value)) $ \valueRoot ->
      pfromData (pstate'focusRoot p) #== Proof.phashConstantTermV1 # valueRoot
        #&& post #== pexactComputeSuccessor pre pmodeReturn valueRoot Proof.pemptyEnvironmentRootV1
          (pfromData $ pstate'continuationRoot p) 0
  PComputeContextConstant valueRoot ->
    pfromData (pstate'focusRoot p) #== Proof.phashContextConstantTermV1 # pfromData valueRoot
      #&& post #== pexactComputeSuccessor pre pmodeReturn (pfromData valueRoot) Proof.pemptyEnvironmentRootV1
        (pfromData $ pstate'continuationRoot p) 0
  PComputeLambda body ->
    plet (Proof.phashLambdaValueV1 # pfromData body # pfromData (pstate'environmentRoot p)) $ \valueRoot ->
      pfromData (pstate'focusRoot p) #== Proof.phashLambdaTermV1 # pfromData body
        #&& post #== pexactComputeSuccessor pre pmodeReturn valueRoot Proof.pemptyEnvironmentRootV1
          (pfromData $ pstate'continuationRoot p) 0
  PComputeDelay body ->
    plet (Proof.phashDelayValueV1 # pfromData body # pfromData (pstate'environmentRoot p)) $ \valueRoot ->
      pfromData (pstate'focusRoot p) #== Proof.phashDelayTermV1 # pfromData body
        #&& post #== pexactComputeSuccessor pre pmodeReturn valueRoot Proof.pemptyEnvironmentRootV1
          (pfromData $ pstate'continuationRoot p) 0
  PComputeApplication function argument ->
    plet (Proof.phashApplyArgumentContinuationV1 # pfromData argument
      # pfromData (pstate'environmentRoot p) # pfromData (pstate'continuationRoot p)) $ \continuation ->
      pfromData (pstate'focusRoot p) #== Proof.phashApplicationTermV1 # pfromData function # pfromData argument
        #&& post #== pexactComputeSuccessor pre pmodeCompute (pfromData function)
          (pfromData $ pstate'environmentRoot p) continuation 0
  PComputeForce term ->
    plet (Proof.phashForceContinuationV1 # pfromData (pstate'continuationRoot p)) $ \continuation ->
      pfromData (pstate'focusRoot p) #== Proof.phashForceTermV1 # pfromData term
        #&& post #== pexactComputeSuccessor pre pmodeCompute (pfromData term)
          (pfromData $ pstate'environmentRoot p) continuation 0
  PComputeError ->
    pfromData (pstate'focusRoot p) #== Proof.phashErrorTermV1
      #&& post #== perrorSuccessor pre perrorExplicit
  PComputeBuiltin tag ->
    plet (Proof.phashBuiltinValueV1 # pfromData tag # (pbuiltinForceCount # pfromData tag)
      # 0 # Proof.pemptySequenceRootV1) $ \valueRoot ->
      pfromData (pstate'focusRoot p) #== Proof.phashBuiltinTermV1 # pfromData tag
        #&& post #== pexactComputeSuccessor pre pmodeReturn valueRoot Proof.pemptyEnvironmentRootV1
          (pfromData $ pstate'continuationRoot p) 0
  PComputeConstrEmpty tag ->
    plet (Proof.phashConstrValueV1 # pfromData tag # 0 # Proof.pemptySequenceRootV1) $ \valueRoot ->
      pfromData (pstate'focusRoot p) #== Proof.phashConstrTermV1 # pfromData tag # 0 # Proof.pemptySequenceRootV1
        #&& post #== pexactComputeSuccessor pre pmodeReturn valueRoot Proof.pemptyEnvironmentRootV1
          (pfromData $ pstate'continuationRoot p) 0
  PComputeConstrNonEmpty tag termsCount firstTerm remainingTermsRoot ->
    plet (Proof.phashSequenceNodeV1 # pfromData firstTerm # pfromData remainingTermsRoot # pfromData termsCount) $ \termsRoot ->
    plet (Proof.phashConstrContinuationV1 # pfromData tag # (pfromData termsCount - 1)
      # pfromData remainingTermsRoot # 0 # Proof.pemptySequenceRootV1
      # pfromData (pstate'environmentRoot p) # pfromData (pstate'continuationRoot p)) $ \continuation ->
      0 #< pfromData termsCount
        #&& plinkedSequenceTailIsWellFormed (pfromData remainingTermsRoot) (pfromData termsCount)
        #&& pfromData (pstate'focusRoot p) #== Proof.phashConstrTermV1 # pfromData tag # pfromData termsCount # termsRoot
        #&& post #== pexactComputeSuccessor pre pmodeCompute (pfromData firstTerm)
          (pfromData $ pstate'environmentRoot p) continuation 0
  PComputeCase scrutinee branchesCount branchesRoot ->
    plet (Proof.phashCaseContinuationV1 # pfromData branchesCount # pfromData branchesRoot
      # pfromData (pstate'environmentRoot p) # pfromData (pstate'continuationRoot p)) $ \continuation ->
      0 #<= pfromData branchesCount
        #&& plinkedSequenceRootIsWellFormed (pfromData branchesRoot) (pfromData branchesCount)
        #&& pfromData (pstate'focusRoot p) #== Proof.phashCaseTermV1
          # pfromData scrutinee # pfromData branchesCount # pfromData branchesRoot
        #&& post #== pexactComputeSuccessor pre pmodeCompute (pfromData scrutinee)
          (pfromData $ pstate'environmentRoot p) continuation 0
  _ -> pconstant False

pverifyLookupStep ::
  forall s. Term s (PMachineStateV1 :--> PMachineStateV1 :--> PCoreStepWitnessV1 :--> PBool)
pverifyLookupStep = phoistAcyclic $ plam $ \pre post witness -> pmatch pre $ \p -> pmatch witness $ \case
  PLookupEnvironment value tailRoot len ->
    pif (0 #< pfromData len
      #&& pfromData (pstate'focusRoot p) #== Proof.phashEnvironmentNodeV1 # pfromData value # pfromData tailRoot # pfromData len
      #&& pfromData (pstate'environmentRoot p) #== pfromData (pstate'focusRoot p)
      #&& pif (pfromData len #== 1)
        (pfromData tailRoot #== Proof.pemptyEnvironmentRootV1)
        (pfromData tailRoot #/= Proof.pemptyEnvironmentRootV1))
      (pif (pfromData (pstate'auxiliary p) #== 0)
        (post #== pexactState pre pmodeReturn (pfromData value) Proof.pemptyEnvironmentRootV1
          (pfromData $ pstate'continuationRoot p) 0 0 0)
        (post #== pexactState pre pmodeLookup (pfromData tailRoot) (pfromData tailRoot)
          (pfromData $ pstate'continuationRoot p) (pfromData (pstate'auxiliary p) - 1) 0 0))
      (pconstant False)
  PLookupEmptyEnvironment ->
    pfromData (pstate'focusRoot p) #== Proof.pemptyEnvironmentRootV1
      #&& pfromData (pstate'environmentRoot p) #== Proof.pemptyEnvironmentRootV1
      #&& post #== perrorSuccessor pre perrorUnboundVariable
  _ -> pconstant False

penvironmentSummaryMatches :: forall s. Term s PByteString -> Term s PEnvironmentSummaryV1 -> Term s PBool
penvironmentSummaryMatches root summary = pmatch summary $ \case
  PEmptyEnvironmentSummary -> root #== Proof.pemptyEnvironmentRootV1
  PNonEmptyEnvironmentSummary value tailRoot len ->
    0 #< pfromData len
      #&& root #== Proof.phashEnvironmentNodeV1 # pfromData value # pfromData tailRoot # pfromData len
      #&& pif (pfromData len #== 1)
        (pfromData tailRoot #== Proof.pemptyEnvironmentRootV1)
        (pfromData tailRoot #/= Proof.pemptyEnvironmentRootV1)

penvironmentSummaryLength :: forall s. Term s PEnvironmentSummaryV1 -> Term s PInteger
penvironmentSummaryLength summary = pmatch summary $ \case
  PEmptyEnvironmentSummary -> 0
  PNonEmptyEnvironmentSummary _ _ len -> pfromData len

pverifyReturnStep ::
  forall s. Term s (PMachineStateV1 :--> PMachineStateV1 :--> PCoreStepWitnessV1 :--> PBool)
pverifyReturnStep = phoistAcyclic $ plam $ \pre post witness -> pmatch pre $ \p -> pmatch witness $ \case
  PReturnEmptyContinuation value ->
    pfromData (pstate'continuationRoot p) #== Proof.pemptyContinuationRootV1
      #&& pfromData (pstate'focusRoot p) #== pvalueWitnessHash (pfromData value)
      #&& post #== pif (pvalueIsConstant $ pfromData value)
        (pexactState pre pmodeHaltSuccess (pfromData $ pstate'focusRoot p) Proof.pemptyEnvironmentRootV1
          Proof.pemptyContinuationRootV1 0 0 0)
        (perrorSuccessor pre perrorNonconstantHalt)
  PReturnApplyArgument argument capturedEnvironment tailRoot ->
    pfromData (pstate'continuationRoot p) #== Proof.phashApplyArgumentContinuationV1
      # pfromData argument # pfromData capturedEnvironment # pfromData tailRoot
      #&& post #== pexactState pre pmodeCompute (pfromData argument) (pfromData capturedEnvironment)
        (Proof.phashApplyFunctionContinuationV1 # pfromData (pstate'focusRoot p) # pfromData tailRoot) 0 0 0
  PReturnApplyLambda body closureEnvironment closureSummary tailRoot ->
    plet (Proof.phashLambdaValueV1 # pfromData body # pfromData closureEnvironment) $ \functionValue ->
    plet (penvironmentSummaryLength $ pfromData closureSummary) $ \environmentLength ->
    plet (Proof.phashEnvironmentNodeV1 # pfromData (pstate'focusRoot p)
      # pfromData closureEnvironment # (environmentLength + 1)) $ \nextEnvironment ->
      penvironmentSummaryMatches (pfromData closureEnvironment) (pfromData closureSummary)
        #&& pfromData (pstate'continuationRoot p) #== Proof.phashApplyFunctionContinuationV1 # functionValue # pfromData tailRoot
        #&& post #== pexactState pre pmodeCompute (pfromData body) nextEnvironment (pfromData tailRoot) 0 0 0
  PReturnApplyBuiltin tag forcesRemaining argumentsCount argumentsRoot tailRoot ->
    plet (Proof.phashBuiltinValueV1 # pfromData tag # pfromData forcesRemaining
      # pfromData argumentsCount # pfromData argumentsRoot) $ \functionValue ->
    plet (pfromData argumentsCount + 1) $ \nextCount ->
    plet (pbuiltinArgumentCount # pfromData tag) $ \required ->
    plet (Proof.phashSequenceNodeV1 # pfromData (pstate'focusRoot p) # pfromData argumentsRoot # nextCount) $ \nextRoot ->
    plet (Proof.phashBuiltinValueV1 # pfromData tag # pfromData forcesRemaining # nextCount # nextRoot) $ \nextValue ->
      pfromData forcesRemaining #== 0
        #&& 0 #<= pfromData argumentsCount
        #&& pfromData argumentsCount #< required
        #&& pfromData (pstate'continuationRoot p) #== Proof.phashApplyFunctionContinuationV1 # functionValue # pfromData tailRoot
        #&& post #== pexactState pre (pif (nextCount #== required) pmodeBuiltin pmodeReturn)
          nextValue Proof.pemptyEnvironmentRootV1 (pfromData tailRoot) 0 0 0
  PReturnApplyInvalid function tailRoot ->
    pnot # pvalueIsLambdaOrBuiltin (pfromData function)
      #&& pfromData (pstate'continuationRoot p) #== Proof.phashApplyFunctionContinuationV1
        # pvalueWitnessHash (pfromData function) # pfromData tailRoot
      #&& post #== perrorSuccessor pre perrorInvalidApplication
  PReturnApplyValueLambda argument body closureEnvironment closureSummary tailRoot ->
    plet (Proof.phashLambdaValueV1 # pfromData body # pfromData closureEnvironment) $ \functionValue ->
    plet (Proof.phashEnvironmentNodeV1 # pfromData argument # pfromData closureEnvironment
      # (penvironmentSummaryLength (pfromData closureSummary) + 1)) $ \nextEnvironment ->
      penvironmentSummaryMatches (pfromData closureEnvironment) (pfromData closureSummary)
        #&& pfromData (pstate'focusRoot p) #== functionValue
        #&& pfromData (pstate'continuationRoot p) #== Proof.phashApplyValueContinuationV1 # pfromData argument # pfromData tailRoot
        #&& post #== pexactState pre pmodeCompute (pfromData body) nextEnvironment (pfromData tailRoot) 0 0 0
  PReturnApplyValueBuiltin argument tag forcesRemaining argumentsCount argumentsRoot tailRoot ->
    plet (Proof.phashBuiltinValueV1 # pfromData tag # pfromData forcesRemaining
      # pfromData argumentsCount # pfromData argumentsRoot) $ \functionValue ->
    plet (pfromData argumentsCount + 1) $ \nextCount ->
    plet (pbuiltinArgumentCount # pfromData tag) $ \required ->
    plet (Proof.phashSequenceNodeV1 # pfromData argument # pfromData argumentsRoot # nextCount) $ \nextRoot ->
    plet (Proof.phashBuiltinValueV1 # pfromData tag # pfromData forcesRemaining # nextCount # nextRoot) $ \nextValue ->
      pfromData forcesRemaining #== 0
        #&& 0 #<= pfromData argumentsCount
        #&& pfromData argumentsCount #< required
        #&& pfromData (pstate'focusRoot p) #== functionValue
        #&& pfromData (pstate'continuationRoot p) #== Proof.phashApplyValueContinuationV1 # pfromData argument # pfromData tailRoot
        #&& post #== pexactState pre (pif (nextCount #== required) pmodeBuiltin pmodeReturn)
          nextValue Proof.pemptyEnvironmentRootV1 (pfromData tailRoot) 0 0 0
  PReturnApplyValueInvalid argument function tailRoot ->
    pnot # pvalueIsLambdaOrBuiltin (pfromData function)
      #&& pfromData (pstate'focusRoot p) #== pvalueWitnessHash (pfromData function)
      #&& pfromData (pstate'continuationRoot p) #== Proof.phashApplyValueContinuationV1 # pfromData argument # pfromData tailRoot
      #&& post #== perrorSuccessor pre perrorInvalidApplication
  PReturnForceDelay body closureEnvironment tailRoot ->
    pfromData (pstate'focusRoot p) #== Proof.phashDelayValueV1 # pfromData body # pfromData closureEnvironment
      #&& pfromData (pstate'continuationRoot p) #== Proof.phashForceContinuationV1 # pfromData tailRoot
      #&& post #== pexactState pre pmodeCompute (pfromData body) (pfromData closureEnvironment) (pfromData tailRoot) 0 0 0
  PReturnForceBuiltin tag forcesRemaining argumentsCount argumentsRoot tailRoot ->
    plet (Proof.phashBuiltinValueV1 # pfromData tag # pfromData forcesRemaining
      # pfromData argumentsCount # pfromData argumentsRoot) $ \valueRoot ->
    plet (Proof.phashBuiltinValueV1 # pfromData tag # (pfromData forcesRemaining - 1)
      # pfromData argumentsCount # pfromData argumentsRoot) $ \nextValue ->
      0 #< pfromData forcesRemaining
        #&& pfromData forcesRemaining #<= pbuiltinForceCount # pfromData tag
        #&& pfromData (pstate'focusRoot p) #== valueRoot
        #&& pfromData (pstate'continuationRoot p) #== Proof.phashForceContinuationV1 # pfromData tailRoot
        #&& post #== pexactState pre pmodeReturn nextValue Proof.pemptyEnvironmentRootV1 (pfromData tailRoot) 0 0 0
  PReturnForceInvalid value tailRoot ->
    pnot # pvalueIsDelayOrForceableBuiltin (pfromData value)
      #&& pfromData (pstate'focusRoot p) #== pvalueWitnessHash (pfromData value)
      #&& pfromData (pstate'continuationRoot p) #== Proof.phashForceContinuationV1 # pfromData tailRoot
      #&& post #== perrorSuccessor pre perrorInvalidForce
  PReturnConstrNext tag remainingTermsCount nextTerm remainingTermsTail valuesCount valuesRoot capturedEnvironment tailRoot ->
    plet (Proof.phashSequenceNodeV1 # pfromData nextTerm # pfromData remainingTermsTail # pfromData remainingTermsCount) $ \remainingTermsRoot ->
    plet (pfromData valuesCount + 1) $ \nextValuesCount ->
    plet (Proof.phashSequenceNodeV1 # pfromData (pstate'focusRoot p) # pfromData valuesRoot # nextValuesCount) $ \nextValuesRoot ->
    plet (Proof.phashConstrContinuationV1 # pfromData tag # (pfromData remainingTermsCount - 1)
      # pfromData remainingTermsTail # nextValuesCount # nextValuesRoot
      # pfromData capturedEnvironment # pfromData tailRoot) $ \nextContinuation ->
      0 #< pfromData remainingTermsCount
        #&& 0 #<= pfromData valuesCount
        #&& plinkedSequenceRootIsWellFormed (pfromData valuesRoot) (pfromData valuesCount)
        #&& plinkedSequenceTailIsWellFormed (pfromData remainingTermsTail) (pfromData remainingTermsCount)
        #&& pfromData (pstate'continuationRoot p) #== Proof.phashConstrContinuationV1
          # pfromData tag # pfromData remainingTermsCount # remainingTermsRoot
          # pfromData valuesCount # pfromData valuesRoot # pfromData capturedEnvironment # pfromData tailRoot
        #&& post #== pexactState pre pmodeCompute (pfromData nextTerm) (pfromData capturedEnvironment) nextContinuation 0 0 0
  PReturnConstrDone tag valuesCount valuesRoot capturedEnvironment tailRoot ->
    plet (pfromData valuesCount + 1) $ \nextValuesCount ->
    plet (Proof.phashSequenceNodeV1 # pfromData (pstate'focusRoot p) # pfromData valuesRoot # nextValuesCount) $ \nextValuesRoot ->
      0 #<= pfromData valuesCount
        #&& plinkedSequenceRootIsWellFormed (pfromData valuesRoot) (pfromData valuesCount)
        #&& pfromData (pstate'continuationRoot p) #== Proof.phashConstrContinuationV1
          # pfromData tag # 0 # Proof.pemptySequenceRootV1 # pfromData valuesCount # pfromData valuesRoot
          # pfromData capturedEnvironment # pfromData tailRoot
        #&& post #== pexactState pre pmodeReturn
          (Proof.phashConstrValueV1 # pfromData tag # nextValuesCount # nextValuesRoot)
          Proof.pemptyEnvironmentRootV1 (pfromData tailRoot) 0 0 0
  PReturnCaseConstr tag valuesCount valuesRoot branchesCount branchesRoot capturedEnvironment tailRoot ->
    0 #<= pfromData valuesCount
      #&& 0 #<= pfromData branchesCount
      #&& plinkedSequenceRootIsWellFormed (pfromData valuesRoot) (pfromData valuesCount)
      #&& plinkedSequenceRootIsWellFormed (pfromData branchesRoot) (pfromData branchesCount)
      #&& pfromData (pstate'focusRoot p) #== Proof.phashConstrValueV1 # pfromData tag # pfromData valuesCount # pfromData valuesRoot
      #&& pfromData (pstate'continuationRoot p) #== Proof.phashCaseContinuationV1
        # pfromData branchesCount # pfromData branchesRoot # pfromData capturedEnvironment # pfromData tailRoot
      #&& post #== pif (0 #<= pfromData tag #&& pfromData tag #< pfromData branchesCount)
        (pexactState pre pmodeCaseSelect (pfromData branchesRoot) (pfromData valuesRoot)
          (Proof.phashCaseSelectContinuationV1 # pfromData capturedEnvironment # pfromData tailRoot # pfromData valuesCount)
          (pfromData tag) 0 0)
        (perrorSuccessor pre perrorCaseBranchMissing)
  PReturnCaseInvalid value branchesCount branchesRoot capturedEnvironment tailRoot ->
    pnot # pvalueIsConstr (pfromData value)
      #&& 0 #<= pfromData branchesCount
      #&& plinkedSequenceRootIsWellFormed (pfromData branchesRoot) (pfromData branchesCount)
      #&& pfromData (pstate'focusRoot p) #== pvalueWitnessHash (pfromData value)
      #&& pfromData (pstate'continuationRoot p) #== Proof.phashCaseContinuationV1
        # pfromData branchesCount # pfromData branchesRoot # pfromData capturedEnvironment # pfromData tailRoot
      #&& post #== perrorSuccessor pre perrorInvalidCaseScrutinee
  _ -> pconstant False

pverifyCaseSelectStep ::
  forall s. Term s (PMachineStateV1 :--> PMachineStateV1 :--> PCoreStepWitnessV1 :--> PBool)
pverifyCaseSelectStep = phoistAcyclic $ plam $ \pre post witness -> pmatch pre $ \p -> pmatch witness $ \case
  PSelectCaseBranch branch remainingBranchesRoot len capturedEnvironment tailRoot valuesCount ->
    plet (Proof.phashCaseSelectContinuationV1 # pfromData capturedEnvironment # pfromData tailRoot # pfromData valuesCount) $ \workRoot ->
      0 #< pfromData len
        #&& 0 #<= pfromData valuesCount
        #&& 0 #<= pfromData (pstate'auxiliary p)
        #&& pfromData (pstate'auxiliary p) #< pfromData len
        #&& plinkedSequenceTailIsWellFormed (pfromData remainingBranchesRoot) (pfromData len)
        #&& plinkedSequenceRootIsWellFormed (pfromData $ pstate'environmentRoot p) (pfromData valuesCount)
        #&& pfromData (pstate'focusRoot p) #== Proof.phashSequenceNodeV1
          # pfromData branch # pfromData remainingBranchesRoot # pfromData len
        #&& pfromData (pstate'continuationRoot p) #== workRoot
        #&& post #== pif (0 #< pfromData (pstate'auxiliary p))
          (pexactState pre pmodeCaseSelect (pfromData remainingBranchesRoot)
            (pfromData $ pstate'environmentRoot p) workRoot (pfromData (pstate'auxiliary p) - 1) 0 0)
          (pif (pfromData valuesCount #== 0)
            (pexactState pre pmodeCompute (pfromData branch) (pfromData capturedEnvironment) (pfromData tailRoot) 0 0 0)
            (pexactState pre pmodeCaseApply (pfromData $ pstate'environmentRoot p) (pfromData branch)
              (Proof.phashCaseApplyContinuationV1 # pfromData capturedEnvironment # pfromData tailRoot)
              (pfromData valuesCount) 0 0))
  _ -> pconstant False

pverifyCaseApplyStep ::
  forall s. Term s (PMachineStateV1 :--> PMachineStateV1 :--> PCoreStepWitnessV1 :--> PBool)
pverifyCaseApplyStep = phoistAcyclic $ plam $ \pre post witness -> pmatch pre $ \p -> pmatch witness $ \case
  PApplyCaseValue value remainingValuesRoot len capturedEnvironment builtContinuation ->
    plet (Proof.phashApplyValueContinuationV1 # pfromData value # pfromData builtContinuation) $ \nextContinuation ->
      0 #< pfromData len
        #&& pfromData (pstate'auxiliary p) #== pfromData len
        #&& plinkedSequenceTailIsWellFormed (pfromData remainingValuesRoot) (pfromData len)
        #&& pfromData (pstate'focusRoot p) #== Proof.phashSequenceNodeV1
          # pfromData value # pfromData remainingValuesRoot # pfromData len
        #&& pfromData (pstate'continuationRoot p) #== Proof.phashCaseApplyContinuationV1
          # pfromData capturedEnvironment # pfromData builtContinuation
        #&& post #== pif (pfromData len #== 1)
          (pexactState pre pmodeCompute (pfromData $ pstate'environmentRoot p) (pfromData capturedEnvironment)
            nextContinuation 0 0 0)
          (pexactState pre pmodeCaseApply (pfromData remainingValuesRoot) (pfromData $ pstate'environmentRoot p)
            (Proof.phashCaseApplyContinuationV1 # pfromData capturedEnvironment # nextContinuation)
            (pfromData len - 1) 0 0)
  _ -> pconstant False

pverifyBuiltinDirectStep ::
  forall s.
  Term s PMachineStateV1 -> Term s PMachineStateV1 -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Builtin.PValueWitnessV1)) -> Term s Builtin.PValueWitnessV1 -> Term s PBool
pverifyBuiltinDirectStep pre post tag arguments result = pmatch pre $ \p ->
  pmatch (Builtin.pargumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
  plet (Proof.phashBuiltinValueV1 # tag # 0 # argumentsCount # argumentsRoot) $ \builtinRoot ->
  pmatch (Builtin.pdirectBuiltinBudgetV1 # tag # arguments) $ \budget ->
    argumentsCount #== pbuiltinArgumentCount # tag
      #&& pfromData (pstate'focusRoot p) #== builtinRoot
      #&& Builtin.pverifyDirectBuiltinV1 # tag # builtinRoot # arguments # result
      #&& post #== pexactState pre pmodeReturn (Builtin.presultRootV1 # result)
        Proof.pemptyEnvironmentRootV1 (pfromData $ pstate'continuationRoot p) 0
        (pfromData $ pbudget'cpu budget) (pfromData $ pbudget'memory budget)

pverifyBuiltinMapConversionStart ::
  forall s.
  Term s PMachineStateV1 -> Term s PMachineStateV1 -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Builtin.PValueWitnessV1)) -> Term s Builtin.PValueWitnessV1 ->
  Term s PMapConversionStartWitnessV1 -> Term s PBool
pverifyBuiltinMapConversionStart pre post tag arguments result material = pmatch pre $ \p ->
  pif (tag #== 38 #|| tag #== 43)
    (pelimList
      (\source rest -> pif (pnull # rest)
        (pmatch material $ \m ->
          plet (pfromData $ pmapStart'sourceNode m) $ \sourceNode ->
          plet (pfromData $ pmapStart'resultNode m) $ \resultNode ->
          plet (Builtin.psemanticConstantPayloadV1 # pfromData source) $ \sourcePayload ->
          plet (Builtin.psemanticConstantPayloadV1 # result) $ \resultPayload ->
          pif (sourcePayload #== pdataSummaryFromNode sourceNode
            #&& resultPayload #== pdataSummaryFromNode resultNode
            #&& Data.pverifyDataNodeV1 # sourceNode # pfromData (pmapStart'sourceList m) # pfromData (pmapStart'sourcePairs m)
            #&& Data.pverifyDataNodeV1 # resultNode # pfromData (pmapStart'resultList m) # pfromData (pmapStart'resultPairs m))
            (plet
              (pif (tag #== 38)
                (pif
                  ( Builtin.psemanticConstantTypeV1 # pfromData source
                      #== pcon (Constant.PListConstant $ pdata $ pcon $ Constant.PPairConstant
                        (pdata $ pcon Constant.PDataConstant) (pdata $ pcon Constant.PDataConstant))
                    #&& Builtin.psemanticConstantTypeV1 # result #== pcon Constant.PDataConstant
                  )
                  (pcon $ PPair (plistSequenceFromNode sourceNode) (pmapSequenceFromNode resultNode))
                  perror)
                (pif
                  ( Builtin.psemanticConstantTypeV1 # pfromData source #== pcon Constant.PDataConstant
                    #&& Builtin.psemanticConstantTypeV1 # result
                      #== pcon (Constant.PListConstant $ pdata $ pcon $ Constant.PPairConstant
                        (pdata $ pcon Constant.PDataConstant) (pdata $ pcon Constant.PDataConstant))
                  )
                  (pcon $ PPair (pmapSequenceFromNode sourceNode) (plistSequenceFromNode resultNode))
                  perror))
              $ \sequences -> pmatch sequences $ \(PPair sourceSequence destinationSequence) ->
                pmatch sourceSequence $ \sourceParts -> pmatch destinationSequence $ \destinationParts ->
                pif (pfromData (Data.pseq'length sourceParts) #== pfromData (Data.pseq'length destinationParts)
                  #&& pif (tag #== 38)
                    (Builtin.psemanticConstantMemoryV1 # pfromData source
                      #== pfromData (Data.pseq'memory sourceParts) - pfromData (Data.pseq'length sourceParts) * 4
                      #&& Builtin.psemanticConstantMemoryV1 # result #== pfromData (Data.pseq'memory destinationParts) + 4)
                    (Builtin.psemanticConstantMemoryV1 # pfromData source #== pfromData (Data.pseq'memory sourceParts) + 4
                      #&& Builtin.psemanticConstantMemoryV1 # result
                        #== pfromData (Data.pseq'memory destinationParts) - pfromData (Data.pseq'length destinationParts) * 4))
                  (pmatch (Builtin.pargumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
                    plet (Proof.phashBuiltinValueV1 # tag # 0 # argumentsCount # argumentsRoot) $ \builtinRoot ->
                    pmatch (Builtin.pdirectBuiltinBudgetV1 # tag # arguments) $ \budget ->
                    plet
                      (pcon $ PMapConversionControlV1
                        (pdata tag) (pdata $ Builtin.presultRootV1 # result)
                        (Data.pseq'root sourceParts) (Data.pseq'length sourceParts)
                        (Data.pseq'payloadCborLength sourceParts) (Data.pseq'memory sourceParts)
                        (Data.pseq'root destinationParts) (Data.pseq'length destinationParts)
                        (Data.pseq'payloadCborLength destinationParts) (Data.pseq'memory destinationParts)
                        (pbudget'cpu budget) (pbudget'memory budget))
                      $ \control ->
                        argumentsCount #== pbuiltinArgumentCount # tag
                          #&& pfromData (pstate'focusRoot p) #== builtinRoot
                          #&& post #== pexactState pre pmodeSemanticBuiltin (phashMapConversionControlV1 # control)
                            Proof.pemptyEnvironmentRootV1 (pfromData $ pstate'continuationRoot p) 0 0 0)
                  perror)
            perror)
        perror)
      perror
      arguments)
    perror

pverifyBuiltinSemanticStep ::
  forall s.
  Term s PMachineStateV1 -> Term s PMachineStateV1 -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Builtin.PValueWitnessV1)) -> Term s Builtin.PValueWitnessV1 ->
  Term s Builtin.PSemanticBuiltinWitnessV1 -> Term s PBool
pverifyBuiltinSemanticStep pre post tag arguments result material = pmatch pre $ \p ->
  pmatch (Builtin.pargumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
  plet (Proof.phashBuiltinValueV1 # tag # 0 # argumentsCount # argumentsRoot) $ \builtinRoot ->
  pmatch (Builtin.pdirectBuiltinBudgetV1 # tag # arguments) $ \budget ->
    argumentsCount #== pbuiltinArgumentCount # tag
      #&& pfromData (pstate'focusRoot p) #== builtinRoot
      #&& Builtin.pverifySemanticBuiltinV1 # tag # builtinRoot # arguments # result # material
      #&& post #== pexactState pre pmodeReturn (Builtin.presultRootV1 # result)
        Proof.pemptyEnvironmentRootV1 (pfromData $ pstate'continuationRoot p) 0
        (pfromData $ pbudget'cpu budget) (pfromData $ pbudget'memory budget)

pverifyBuiltinFailureStep ::
  forall s.
  Term s PMachineStateV1 -> Term s PMachineStateV1 -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Builtin.PValueWitnessV1)) -> Term s PBool
pverifyBuiltinFailureStep pre post tag arguments = pmatch pre $ \p ->
  pmatch (Builtin.pargumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
  plet (Proof.phashBuiltinValueV1 # tag # 0 # argumentsCount # argumentsRoot) $ \builtinRoot ->
  pmatch (Builtin.pdirectBuiltinFailureBudgetV1 # tag # arguments) $ \budget ->
    pfromData (pstate'focusRoot p) #== builtinRoot
      #&& Builtin.pverifyDirectBuiltinFailureV1 # tag # builtinRoot # arguments
      #&& post #== pexactState pre pmodeHaltError Proof.phashErrorTermV1 Proof.pemptyEnvironmentRootV1
        Proof.pemptyContinuationRootV1 perrorBuiltinFailure
        (pfromData $ pbudget'cpu budget) (pfromData $ pbudget'memory budget)

pverifyBuiltinSemanticFailureStep ::
  forall s.
  Term s PMachineStateV1 -> Term s PMachineStateV1 -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Builtin.PValueWitnessV1)) -> Term s Builtin.PSemanticBuiltinWitnessV1 -> Term s PBool
pverifyBuiltinSemanticFailureStep pre post tag arguments material = pmatch pre $ \p ->
  pmatch (Builtin.pargumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
  plet (Proof.phashBuiltinValueV1 # tag # 0 # argumentsCount # argumentsRoot) $ \builtinRoot ->
    argumentsCount #== pbuiltinArgumentCount # tag
      #&& pfromData (pstate'focusRoot p) #== builtinRoot
      #&& Builtin.pverifySemanticBuiltinFailureV1 # tag # builtinRoot # arguments # material
      #&& post #== pexactState pre pmodeHaltError Proof.phashErrorTermV1 Proof.pemptyEnvironmentRootV1
        Proof.pemptyContinuationRootV1 perrorBuiltinFailure 0 0

pverifyBuiltinTypeFailureStep ::
  forall s.
  Term s PMachineStateV1 -> Term s PMachineStateV1 -> Term s PInteger ->
  Term s (PBuiltinList (PAsData Builtin.PRuntimeValueWitnessV1)) -> Term s PBool
pverifyBuiltinTypeFailureStep pre post tag arguments = pmatch pre $ \p ->
  pmatch (Builtin.pruntimeArgumentsRootV1 # arguments) $ \(PPair argumentsRoot argumentsCount) ->
  plet (Proof.phashBuiltinValueV1 # tag # 0 # argumentsCount # argumentsRoot) $ \builtinRoot ->
    pfromData (pstate'focusRoot p) #== builtinRoot
      #&& Builtin.pverifyBuiltinTypeFailureV1 # tag # builtinRoot # arguments
      #&& post #== pexactState pre pmodeHaltError Proof.phashErrorTermV1 Proof.pemptyEnvironmentRootV1
        Proof.pemptyContinuationRootV1 perrorBuiltinFailure 0 0

pverifyBuiltinBlsFinalStep ::
  forall s.
  Term s PMachineStateV1 -> Term s PMachineStateV1 -> Term s PByteString -> Term s PByteString ->
  Term s Builtin.PBlsExpressionWitnessV1 -> Term s Builtin.PBlsExpressionWitnessV1 ->
  Term s Builtin.PValueWitnessV1 -> Term s PBool
pverifyBuiltinBlsFinalStep pre post leftRoot rightRoot left right result = pmatch pre $ \p ->
  plet
    (pcons # pdata (pcon $ Builtin.PBlsMillerLoopValue $ pdata leftRoot)
      # (pcons # pdata (pcon $ Builtin.PBlsMillerLoopValue $ pdata rightRoot) # pnil))
    $ \arguments -> pmatch (Builtin.pdirectBuiltinBudgetV1 # 70 # arguments) $ \budget ->
      Builtin.pverifyDirectBlsFinalRootsV1
        # pfromData (pstate'focusRoot p) # leftRoot # rightRoot # left # right # result
        #&& post #== pexactState pre pmodeReturn (Builtin.presultRootV1 # result)
          Proof.pemptyEnvironmentRootV1 (pfromData $ pstate'continuationRoot p) 0
          (pfromData $ pbudget'cpu budget) (pfromData $ pbudget'memory budget)

pverifyBuiltinStep ::
  forall s. Term s (PMachineStateV1 :--> PMachineStateV1 :--> PCoreStepWitnessV1 :--> PBool)
pverifyBuiltinStep = phoistAcyclic $ plam $ \pre post witness -> pmatch witness $ \case
  PExecuteBuiltinDirect tag arguments result ->
    pverifyBuiltinDirectStep pre post (pfromData tag) (pfromData arguments) (pfromData result)
  PExecuteBuiltinSemantic tag arguments result material ->
    pverifyBuiltinSemanticStep pre post (pfromData tag) (pfromData arguments) (pfromData result) (pfromData material)
  PStartBuiltinMapConversion tag arguments result material ->
    pverifyBuiltinMapConversionStart pre post (pfromData tag) (pfromData arguments) (pfromData result) (pfromData material)
  PExecuteBuiltinSemanticFailure tag arguments material ->
    pverifyBuiltinSemanticFailureStep pre post (pfromData tag) (pfromData arguments) (pfromData material)
  PExecuteBuiltinBlsFinal leftRoot rightRoot left right result ->
    pverifyBuiltinBlsFinalStep pre post (pfromData leftRoot) (pfromData rightRoot)
      (pfromData left) (pfromData right) (pfromData result)
  PExecuteBuiltinFailure tag arguments ->
    pverifyBuiltinFailureStep pre post (pfromData tag) (pfromData arguments)
  PExecuteBuiltinTypeFailure tag arguments ->
    pverifyBuiltinTypeFailureStep pre post (pfromData tag) (pfromData arguments)
  _ -> pconstant False

pverifySemanticBuiltinControlStep ::
  forall s. Term s (PMachineStateV1 :--> PMachineStateV1 :--> PCoreStepWitnessV1 :--> PBool)
pverifySemanticBuiltinControlStep = phoistAcyclic $ plam $ \pre post witness -> pmatch witness $ \case
  PStepBuiltinListToMap control source pair first second key value destination ->
    pverifyBuiltinListToMapStep pre post (pfromData control) (pfromData source) (pfromData pair)
      (pfromData first) (pfromData second) (pfromData key) (pfromData value) (pfromData destination)
  PStepBuiltinMapToList control source destination pair first second key value ->
    pverifyBuiltinMapToListStep pre post (pfromData control) (pfromData source) (pfromData destination)
      (pfromData pair) (pfromData first) (pfromData second) (pfromData key) (pfromData value)
  PFinishBuiltinMapConversion control -> pverifyBuiltinMapConversionFinish pre post (pfromData control)
  _ -> pconstant False

pverifyCoreStepV1 :: forall s. Term s (PMachineStateV1 :--> PMachineStateV1 :--> PCoreStepWitnessV1 :--> PBool)
pverifyCoreStepV1 = phoistAcyclic $ plam $ \pre post witness ->
  pif (pstateIsWellFormed # pre #&& pstateIsWellFormed # post)
    (pmatch pre $ \p -> pmatch post $ \q ->
      pfromData (pstate'executionIndex p) #== pfromData (pstate'executionIndex q)
        #&& pif (pfromData (pstate'mode p) #== pmodeCompute) (pverifyComputeStep # pre # post # witness)
          (pif (pfromData (pstate'mode p) #== pmodeLookup) (pverifyLookupStep # pre # post # witness) $
            pif (pfromData (pstate'mode p) #== pmodeReturn) (pverifyReturnStep # pre # post # witness) $
            pif (pfromData (pstate'mode p) #== pmodeCaseSelect) (pverifyCaseSelectStep # pre # post # witness) $
            pif (pfromData (pstate'mode p) #== pmodeCaseApply) (pverifyCaseApplyStep # pre # post # witness)
              (pif (pfromData (pstate'mode p) #== pmodeBuiltin) (pverifyBuiltinStep # pre # post # witness)
                (pif (pfromData (pstate'mode p) #== pmodeSemanticBuiltin)
                  (pverifySemanticBuiltinControlStep # pre # post # witness) (pconstant False)))))
    (pconstant False)
