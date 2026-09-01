{-# LANGUAGE OverloadedStrings #-}

module Midgard.RedeemerItemProof (
  PRedeemerItemDescriptorV1 (..), PRedeemerItemSourceSpanV1 (..), PRedeemerItemProofControlV1 (..),
  PRedeemerItemProofActionV1 (..), PRedeemerItemProofWitnessV1 (..), PRedeemerItemProofStepResultV1 (..),
  pversion, predeemerFieldIndex, pmodeDescriptor, pmodeData, pstageHeader, pstageTail, pstageData,
  pstageTerminal, pmaxHeaderSpan, pmaxTailSpan, pstageDataOuterFieldsAreWellFormedV1,
  pcontrolIsWellFormed, pinitialControlV1, pencodeControlV1, pdecodeControlV1, phashControlV1,
  pstageDataOuterControlPrefixV1, pstageDataSomeTraversalHashPrefixV1,
  phashStageDataFromAuthenticatedPrefixV1, phashStageDataOuterWithCheckedTraversalV1,
  pdescriptorV1, pfinalizeV1, pnextSourceSpanV1, pheaderProofStepV1, ptailProofStepV1, pstepV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils ((#/=), pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.BoundedItem (PChunkProofV1 (..))
import Midgard.BoundedItem qualified as Bounded
import Midgard.CanonicalCborScan (PCborHeadV1 (..), pheadAtV1)
import Midgard.CekData (PDataSummaryV1)
import Midgard.CekDataTraverse (PDataTraverseActionV1, PDataTraverseControlV1)
import Midgard.CekDataTraverse qualified as Traverse
import Midgard.CekSourceBlob qualified as Blob
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteArrayHeader, pencodeDefiniteBytes)

pversion, predeemerFieldIndex, pmodeDescriptor, pmodeData, pstageHeader, pstageTail, pstageData,
  pstageTerminal, pmaxHeaderSpan, pmaxTailSpan :: forall s. Term s PInteger
pversion = 1
predeemerFieldIndex = 8
pmodeDescriptor = 0
pmodeData = 1
pstageHeader = 0
pstageTail = 1
pstageData = 2
pstageTerminal = 3
pmaxHeaderSpan = 28
pmaxTailSpan = 19

pcontrolDomain :: forall s. Term s PByteString
pcontrolDomain = pconstant "MidgardRedeemerItemProofControlV1"

data PRedeemerItemDescriptorV1 s = PRedeemerItemDescriptorV1
  { predeemerDescriptor'itemIndex :: Term s (PAsData PInteger)
  , predeemerDescriptor'itemCount :: Term s (PAsData PInteger)
  , predeemerDescriptor'totalLength :: Term s (PAsData PInteger)
  , predeemerDescriptor'itemCommitment :: Term s (PAsData PByteString)
  , predeemerDescriptor'purposeTag :: Term s (PAsData PInteger)
  , predeemerDescriptor'pointerIndex :: Term s (PAsData PInteger)
  , predeemerDescriptor'dataOffset :: Term s (PAsData PInteger)
  , predeemerDescriptor'dataLength :: Term s (PAsData PInteger)
  , predeemerDescriptor'executionMemory :: Term s (PAsData PInteger)
  , predeemerDescriptor'executionSteps :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PRedeemerItemDescriptorV1)

data PRedeemerItemSourceSpanV1 s = PRedeemerItemSourceSpanV1
  { predeemerSpan'absoluteStart :: Term s (PAsData PInteger)
  , predeemerSpan'length :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PRedeemerItemSourceSpanV1)

data PRedeemerItemProofControlV1 s = PRedeemerItemProofControlV1
  { predeemerControl'version :: Term s (PAsData PInteger)
  , predeemerControl'mode :: Term s (PAsData PInteger)
  , predeemerControl'stage :: Term s (PAsData PInteger)
  , predeemerControl'itemIndex :: Term s (PAsData PInteger)
  , predeemerControl'itemCount :: Term s (PAsData PInteger)
  , predeemerControl'totalLength :: Term s (PAsData PInteger)
  , predeemerControl'itemCommitment :: Term s (PAsData PByteString)
  , predeemerControl'expectedPurposeTag :: Term s (PAsData PInteger)
  , predeemerControl'expectedPointerIndex :: Term s (PAsData PInteger)
  , predeemerControl'purposeTag :: Term s (PAsData PInteger)
  , predeemerControl'pointerIndex :: Term s (PAsData PInteger)
  , predeemerControl'dataOffset :: Term s (PAsData PInteger)
  , predeemerControl'dataLength :: Term s (PAsData PInteger)
  , predeemerControl'executionMemory :: Term s (PAsData PInteger)
  , predeemerControl'executionSteps :: Term s (PAsData PInteger)
  , predeemerControl'traversal :: Term s (PAsData (PMaybeData PDataTraverseControlV1))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PRedeemerItemProofControlV1)

data PRedeemerItemProofActionV1 s
  = PRedeemerItemOpenHeader
  | PRedeemerItemOpenTail
  | PRedeemerItemTraverseData (Term s (PAsData PDataTraverseActionV1))
  | PRedeemerItemFinishData
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PRedeemerItemProofActionV1)

data PRedeemerItemProofWitnessV1 s = PRedeemerItemProofWitnessV1
  { predeemerWitness'action :: Term s (PAsData PRedeemerItemProofActionV1)
  , predeemerWitness'chunkProof :: Term s (PAsData (PMaybeData PChunkProofV1))
  , predeemerWitness'nextChunkProof :: Term s (PAsData (PMaybeData PChunkProofV1))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PRedeemerItemProofWitnessV1)

data PRedeemerItemProofStepResultV1 s
  = PRedeemerItemProofAdvanced (Term s (PAsData PRedeemerItemProofControlV1))
  | PRedeemerItemProofInvalid
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PRedeemerItemProofStepResultV1)

psupportedPurposeTag :: forall s. Term s PInteger -> Term s PBool
psupportedPurposeTag tag = tag #== 0 #|| tag #== 1 #|| tag #== 3 #|| tag #== 6

popenedDescriptorIsWellFormed :: forall s. Term s PRedeemerItemProofControlV1 -> Term s PBool
popenedDescriptorIsWellFormed control = pmatch control $ \c ->
  plet (pfromData $ predeemerControl'totalLength c) $ \total ->
  plet (pfromData $ predeemerControl'dataOffset c) $ \offset ->
  plet (pfromData $ predeemerControl'dataLength c) $ \length ->
  plet (total - offset - length) $ \tailLength ->
  plet (pfromData $ predeemerControl'expectedPurposeTag c) $ \expected ->
    pand'List
      [ psupportedPurposeTag (pfromData $ predeemerControl'purposeTag c)
      , pfromData (predeemerControl'pointerIndex c) #>= 0
      , offset #> 0, length #> 0, offset + length #< total
      , tailLength #> 0, tailLength #<= pmaxTailSpan
      , pif (expected #== -1) (pconstant True)
          (pfromData (predeemerControl'purposeTag c) #== expected
            #&& pfromData (predeemerControl'pointerIndex c) #== pfromData (predeemerControl'expectedPointerIndex c))
      ]

pexpectedFieldsAreWellFormed :: forall s. Term s PRedeemerItemProofControlV1 -> Term s PBool
pexpectedFieldsAreWellFormed control = pmatch control $ \c ->
  plet (pfromData $ predeemerControl'expectedPurposeTag c) $ \purpose ->
  plet (pfromData $ predeemerControl'expectedPointerIndex c) $ \pointer ->
    (purpose #== -1 #&& pointer #== -1) #|| (psupportedPurposeTag purpose #&& pointer #>= 0)

ptraversalOuterMatches :: forall s. Term s PRedeemerItemProofControlV1 -> Term s PBool
ptraversalOuterMatches control = pmatch control $ \c -> pmatch (pfromData $ predeemerControl'traversal c) $ \case
  PDNothing -> pconstant False
  PDJust traversal -> pmatch (pfromData traversal) $ \t ->
    pfromData (Traverse.ptraverse'sourceStart t) #== pfromData (predeemerControl'dataOffset c)
      #&& pfromData (Traverse.ptraverse'sourceLength t) #== pfromData (predeemerControl'dataLength c)

pstageDataOuterFieldsAreWellFormedV1 :: forall s. Term s (PRedeemerItemProofControlV1 :--> PBool)
pstageDataOuterFieldsAreWellFormedV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c -> pand'List
  [ pfromData (predeemerControl'version c) #== pversion
  , pfromData (predeemerControl'mode c) #== pmodeData
  , pfromData (predeemerControl'stage c) #== pstageData
  , pfromData (predeemerControl'itemIndex c) #>= 0
  , pfromData (predeemerControl'itemCount c) #> pfromData (predeemerControl'itemIndex c)
  , pfromData (predeemerControl'totalLength c) #> 0
  , plengthBS # pfromData (predeemerControl'itemCommitment c) #== 32
  , pexpectedFieldsAreWellFormed control
  , popenedDescriptorIsWellFormed control
  , pfromData (predeemerControl'executionMemory c) #>= 0
  , pfromData (predeemerControl'executionSteps c) #>= 0
  , ptraversalOuterMatches control
  ]

pcontrolIsWellFormed :: forall s. Term s (PRedeemerItemProofControlV1 :--> PBool)
pcontrolIsWellFormed = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  plet (pfromData $ predeemerControl'stage c) $ \stage ->
  plet (pfromData $ predeemerControl'mode c) $ \mode ->
  plet (pfromData $ predeemerControl'traversal c) $ \traversal ->
  plet (popenedDescriptorIsWellFormed control) $ \descriptorOpen ->
  plet (pfromData (predeemerControl'executionMemory c) #>= 0
      #&& pfromData (predeemerControl'executionSteps c) #>= 0) $ \exUnitsOpen ->
  pand'List
    [ pfromData (predeemerControl'version c) #== pversion
    , mode #== pmodeDescriptor #|| mode #== pmodeData
    , stage #>= pstageHeader, stage #<= pstageTerminal
    , pfromData (predeemerControl'itemIndex c) #>= 0
    , pfromData (predeemerControl'itemCount c) #> pfromData (predeemerControl'itemIndex c)
    , pfromData (predeemerControl'totalLength c) #> 0
    , plengthBS # pfromData (predeemerControl'itemCommitment c) #== 32
    , pexpectedFieldsAreWellFormed control
    , pif (stage #== pstageHeader)
        (pfromData (predeemerControl'purposeTag c) #== -1
          #&& pfromData (predeemerControl'pointerIndex c) #== -1
          #&& pfromData (predeemerControl'dataOffset c) #== 0
          #&& pfromData (predeemerControl'dataLength c) #== 0
          #&& pfromData (predeemerControl'executionMemory c) #== -1
          #&& pfromData (predeemerControl'executionSteps c) #== -1
          #&& traversal #== pcon PDNothing)
        (pif (stage #== pstageTail)
          (descriptorOpen #&& pfromData (predeemerControl'executionMemory c) #== -1
            #&& pfromData (predeemerControl'executionSteps c) #== -1 #&& traversal #== pcon PDNothing)
          (pif (stage #== pstageData)
            (pstageDataOuterFieldsAreWellFormedV1 # control
              #&& pmatch traversal (\case PDNothing -> pconstant False; PDJust t -> Traverse.pcontrolIsWellFormed # pfromData t))
            (pif (mode #== pmodeDescriptor)
              (descriptorOpen #&& exUnitsOpen #&& traversal #== pcon PDNothing)
              (descriptorOpen #&& exUnitsOpen #&& ptraversalOuterMatches control
                #&& pmatch traversal (\case
                  PDNothing -> pconstant False
                  PDJust t -> Traverse.pcontrolIsWellFormed # pfromData t
                    #&& pmatch (pfromData t) (\tf -> pfromData (Traverse.ptraverse'stage tf) #== Traverse.pstageTerminal)
                    #&& pnot # (Traverse.pfinalizeV1 # pfromData t #== pcon PNothing))))))
    ]

pinitialControlV1 :: forall s. Term s (PInteger :--> PInteger :--> PInteger :--> PInteger :--> PByteString :--> PInteger :--> PInteger :--> PRedeemerItemProofControlV1)
pinitialControlV1 = phoistAcyclic $ plam $ \mode itemIndex itemCount total commitment expectedPurpose expectedPointer ->
  plet (pcon $ PRedeemerItemProofControlV1
    (pdata pversion) (pdata mode) (pdata pstageHeader) (pdata itemIndex) (pdata itemCount) (pdata total)
    (pdata commitment) (pdata expectedPurpose) (pdata expectedPointer) (pdata $ -1) (pdata $ -1)
    (pdata 0) (pdata 0) (pdata $ -1) (pdata $ -1) (pdata $ pcon PDNothing)) $ \control ->
      pif (pcontrolIsWellFormed # control) control perror

pencodeControlPrefix :: forall s. Term s PRedeemerItemProofControlV1 -> Term s PByteString
pencodeControlPrefix control = pmatch control $ \c ->
  (pencodeDefiniteArrayHeader # 16)
    <> pcborInt (pfromData $ predeemerControl'version c) <> pcborInt (pfromData $ predeemerControl'mode c)
    <> pcborInt (pfromData $ predeemerControl'stage c) <> pcborInt (pfromData $ predeemerControl'itemIndex c)
    <> pcborInt (pfromData $ predeemerControl'itemCount c) <> pcborInt (pfromData $ predeemerControl'totalLength c)
    <> (pencodeDefiniteBytes # pfromData (predeemerControl'itemCommitment c))
    <> pcborInt (pfromData $ predeemerControl'expectedPurposeTag c) <> pcborInt (pfromData $ predeemerControl'expectedPointerIndex c)
    <> pcborInt (pfromData $ predeemerControl'purposeTag c) <> pcborInt (pfromData $ predeemerControl'pointerIndex c)
    <> pcborInt (pfromData $ predeemerControl'dataOffset c) <> pcborInt (pfromData $ predeemerControl'dataLength c)
    <> pcborInt (pfromData $ predeemerControl'executionMemory c) <> pcborInt (pfromData $ predeemerControl'executionSteps c)

pencodeOptionalTraversal :: forall s. Term s (PMaybeData PDataTraverseControlV1 :--> PByteString)
pencodeOptionalTraversal = phoistAcyclic $ plam $ \value -> pmatch value $ \case
  PDNothing -> pconstant "\xd8\x7a\x80"
  PDJust traversal -> pconstant "\xd8\x79\x9f" <> (Traverse.pencodeControlV1 # pfromData traversal) <> pconstant "\xff"

pencodeControlV1 :: forall s. Term s (PRedeemerItemProofControlV1 :--> PByteString)
pencodeControlV1 = phoistAcyclic $ plam $ \control -> pif (pcontrolIsWellFormed # control)
  (pmatch control $ \c -> pencodeControlPrefix control <> (pencodeOptionalTraversal # pfromData (predeemerControl'traversal c))) perror

poptionalTraversalFromData :: forall s. Term s PData -> Term s (PMaybeData PDataTraverseControlV1)
poptionalTraversalFromData dat = pmatch (pasConstr # dat) $ \(PBuiltinPair index fields) ->
  pif (index #== 1 #&& pnull # fields) (pcon PDNothing) $
    pif (index #== 0 #&& plength # fields #== 1)
      (pcon $ PDJust $ pdata $ Traverse.pcontrolFromDataV1 # (pelemAt # 0 # fields)) perror

pdecodeControlV1 :: forall s. Term s (PByteString :--> PRedeemerItemProofControlV1)
pdecodeControlV1 = phoistAcyclic $ plam $ \cbor -> pmatch (pdeserialise # cbor) $ \case
  PNothing -> perror
  PJust dat -> plet (pasList # dat) $ \xs -> pif (plength # xs #== 16)
    (plet (pcon $ PRedeemerItemProofControlV1
      (pdata $ pasInt # (pelemAt # 0 # xs)) (pdata $ pasInt # (pelemAt # 1 # xs))
      (pdata $ pasInt # (pelemAt # 2 # xs)) (pdata $ pasInt # (pelemAt # 3 # xs))
      (pdata $ pasInt # (pelemAt # 4 # xs)) (pdata $ pasInt # (pelemAt # 5 # xs))
      (pdata $ pasByteStr # (pelemAt # 6 # xs)) (pdata $ pasInt # (pelemAt # 7 # xs))
      (pdata $ pasInt # (pelemAt # 8 # xs)) (pdata $ pasInt # (pelemAt # 9 # xs))
      (pdata $ pasInt # (pelemAt # 10 # xs)) (pdata $ pasInt # (pelemAt # 11 # xs))
      (pdata $ pasInt # (pelemAt # 12 # xs)) (pdata $ pasInt # (pelemAt # 13 # xs))
      (pdata $ pasInt # (pelemAt # 14 # xs)) (pdata $ poptionalTraversalFromData $ pelemAt # 15 # xs)) $ \control ->
        pif (pcontrolIsWellFormed # control #&& pencodeControlV1 # control #== cbor) control perror) perror

pcontrolHashPrefix :: forall s. Term s PRedeemerItemProofControlV1 -> Term s PByteString
pcontrolHashPrefix control = pcontrolDomain <> pencodeControlPrefix control

pstageDataOuterControlPrefixV1 :: forall s. Term s (PRedeemerItemProofControlV1 :--> PByteString)
pstageDataOuterControlPrefixV1 = phoistAcyclic $ plam $ \control ->
  pif (pstageDataOuterFieldsAreWellFormedV1 # control) (pencodeControlPrefix control) perror

pstageDataSomeTraversalHashPrefixV1 :: forall s. Term s (PRedeemerItemProofControlV1 :--> PByteString)
pstageDataSomeTraversalHashPrefixV1 = phoistAcyclic $ plam $ \control ->
  pif (pstageDataOuterFieldsAreWellFormedV1 # control) (pcontrolHashPrefix control <> pconstant "\xd8\x79\x9f") perror

phashStageDataFromAuthenticatedPrefixV1 :: forall s. Term s (PByteString :--> PByteString :--> PByteString)
phashStageDataFromAuthenticatedPrefixV1 = phoistAcyclic $ plam $ \prefix traversal -> pblake2b_256 # (prefix <> traversal <> pconstant "\xff")

phashStageDataOuterWithCheckedTraversalV1 :: forall s. Term s (PRedeemerItemProofControlV1 :--> PByteString :--> PByteString)
phashStageDataOuterWithCheckedTraversalV1 = phoistAcyclic $ plam $ \control traversal ->
  phashStageDataFromAuthenticatedPrefixV1 # (pstageDataSomeTraversalHashPrefixV1 # control) # traversal

phashControlV1 :: forall s. Term s (PRedeemerItemProofControlV1 :--> PByteString)
phashControlV1 = phoistAcyclic $ plam $ \control -> pif (pcontrolIsWellFormed # control)
  (pblake2b_256 # (pcontrolHashPrefix control <> pmatch control (\c -> pencodeOptionalTraversal # pfromData (predeemerControl'traversal c)))) perror

pdescriptorV1 :: forall s. Term s (PRedeemerItemProofControlV1 :--> PMaybe PRedeemerItemDescriptorV1)
pdescriptorV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pif (pcontrolIsWellFormed # control #&& (pfromData (predeemerControl'stage c) #>= pstageData
      #|| (pfromData (predeemerControl'stage c) #== pstageTerminal #&& pfromData (predeemerControl'mode c) #== pmodeDescriptor)))
    (pcon $ PJust $ pcon $ PRedeemerItemDescriptorV1
      (predeemerControl'itemIndex c) (predeemerControl'itemCount c) (predeemerControl'totalLength c)
      (predeemerControl'itemCommitment c) (predeemerControl'purposeTag c) (predeemerControl'pointerIndex c)
      (predeemerControl'dataOffset c) (predeemerControl'dataLength c)
      (predeemerControl'executionMemory c) (predeemerControl'executionSteps c)) (pcon PNothing)

pfinalizeV1 :: forall s. Term s (PRedeemerItemProofControlV1 :--> PMaybe PDataSummaryV1)
pfinalizeV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pif (pcontrolIsWellFormed # control #&& pfromData (predeemerControl'mode c) #== pmodeData
      #&& pfromData (predeemerControl'stage c) #== pstageTerminal)
    (pmatch (pfromData $ predeemerControl'traversal c) $ \case
      PDNothing -> pcon PNothing
      PDJust traversal -> Traverse.pfinalizeV1 # pfromData traversal) (pcon PNothing)

pnextSourceSpanV1 :: forall s. Term s (PRedeemerItemProofControlV1 :--> PMaybe PRedeemerItemSourceSpanV1)
pnextSourceSpanV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pif (pnot # (pcontrolIsWellFormed # control)) (pcon PNothing) $
    pif (pfromData (predeemerControl'stage c) #== pstageHeader)
      (pcon $ PJust $ pcon $ PRedeemerItemSourceSpanV1 (pdata 0)
        (pdata $ pif (pfromData (predeemerControl'totalLength c) #< pmaxHeaderSpan)
          (pfromData $ predeemerControl'totalLength c) pmaxHeaderSpan)) $
    pif (pfromData (predeemerControl'stage c) #== pstageTail)
      (pcon $ PJust $ pcon $ PRedeemerItemSourceSpanV1
        (pdata $ pfromData (predeemerControl'dataOffset c) + pfromData (predeemerControl'dataLength c))
        (pdata $ pfromData (predeemerControl'totalLength c) - pfromData (predeemerControl'dataOffset c) - pfromData (predeemerControl'dataLength c))) $
    pif (pfromData (predeemerControl'stage c) #== pstageData)
      (pmatch (pfromData $ predeemerControl'traversal c) $ \case
        PDNothing -> pcon PNothing
        PDJust traversal -> pmatch (Traverse.pnextSourceSpanV1 # pfromData traversal) $ \case
          PNothing -> pcon PNothing
          PJust spanValue -> pmatch spanValue $ \s -> pcon $ PJust $ pcon $ PRedeemerItemSourceSpanV1
            (Blob.pspan'absoluteStart s) (Blob.pspan'length s)) (pcon PNothing)

pchunkProofMatches :: forall s. Term s PRedeemerItemProofControlV1 -> Term s PChunkProofV1 -> Term s PInteger -> Term s PBool
pchunkProofMatches control proof index = pmatch control $ \c -> pmatch proof $ \p -> pand'List
  [ pfromData (pchunkProof'fieldIndex p) #== predeemerFieldIndex
  , pfromData (pchunkProof'itemIndex p) #== pfromData (predeemerControl'itemIndex c)
  , pfromData (pchunkProof'totalLength p) #== pfromData (predeemerControl'totalLength c)
  , pfromData (pchunkProof'chunkIndex p) #== index
  , Bounded.pverifyChunk # pfromData (predeemerControl'itemCommitment c) # proof
  ]

pauthenticatedSpan :: forall s. Term s PRedeemerItemProofControlV1 -> Term s PRedeemerItemSourceSpanV1 ->
  Term s PChunkProofV1 -> Term s (PMaybeData PChunkProofV1) -> Term s (PMaybe PByteString)
pauthenticatedSpan control spanValue proof nextProof = pmatch control $ \c -> pmatch spanValue $ \spanFields ->
  plet (pfromData $ predeemerSpan'absoluteStart spanFields) $ \start ->
  plet (pfromData $ predeemerSpan'length spanFields) $ \length ->
  pif (length #<= 0 #|| length #> Traverse.pmaxSourceSpan #|| start #< 0
      #|| start + length #> pfromData (predeemerControl'totalLength c)) (pcon PNothing) $
    plet (pdiv # start # Bounded.pchunkBytes) $ \firstIndex ->
    plet (pdiv # (start + length - 1) # Bounded.pchunkBytes) $ \lastIndex ->
    plet (start - firstIndex * Bounded.pchunkBytes) $ \localStart ->
    pif (lastIndex #> firstIndex + 1 #|| pnot # (pchunkProofMatches control proof firstIndex)) (pcon PNothing) $
      pif (lastIndex #== firstIndex)
        (pmatch nextProof $ \case
          PDNothing -> pmatch proof $ \p -> pcon $ PJust $ psliceBS # localStart # length # pfromData (pchunkProof'chunk p)
          PDJust _ -> pcon PNothing)
        (pmatch nextProof $ \case
          PDNothing -> pcon PNothing
          PDJust nextData -> plet (pfromData nextData) $ \next ->
            pif (pchunkProofMatches control next lastIndex)
              (pmatch proof $ \p -> pmatch next $ \n -> pcon $ PJust $ psliceBS # localStart # length
                # (pfromData (pchunkProof'chunk p) <> pfromData (pchunkProof'chunk n))) (pcon PNothing))

pwithHeaderFields :: forall s. Term s PRedeemerItemProofControlV1 -> Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PRedeemerItemProofControlV1
pwithHeaderFields control purpose pointer offset length = pmatch control $ \c -> pcon $ PRedeemerItemProofControlV1
  (predeemerControl'version c) (predeemerControl'mode c) (pdata pstageTail)
  (predeemerControl'itemIndex c) (predeemerControl'itemCount c) (predeemerControl'totalLength c)
  (predeemerControl'itemCommitment c) (predeemerControl'expectedPurposeTag c) (predeemerControl'expectedPointerIndex c)
  (pdata purpose) (pdata pointer) (pdata offset) (pdata length)
  (predeemerControl'executionMemory c) (predeemerControl'executionSteps c) (predeemerControl'traversal c)

pheaderStep :: forall s. Term s PRedeemerItemProofControlV1 -> Term s PByteString -> Term s PRedeemerItemProofStepResultV1
pheaderStep control source = pmatch (pheadAtV1 # source # 0 # 4) $ \case
  PNothing -> pcon PRedeemerItemProofInvalid
  PJust outer -> pmatch outer $ \o -> pif (pcborHead'value o #/= 4) (pcon PRedeemerItemProofInvalid) $
    pmatch (pheadAtV1 # source # pcborHead'nextOffset o # 0) $ \case
      PNothing -> pcon PRedeemerItemProofInvalid
      PJust purpose -> pmatch purpose $ \p -> pmatch (pheadAtV1 # source # pcborHead'nextOffset p # 0) $ \case
        PNothing -> pcon PRedeemerItemProofInvalid
        PJust pointer -> pmatch pointer $ \i -> pmatch (pheadAtV1 # source # pcborHead'nextOffset i # 2) $ \case
          PNothing -> pcon PRedeemerItemProofInvalid
          PJust dat -> pmatch dat $ \d -> plet
            (pwithHeaderFields control (pcborHead'value p) (pcborHead'value i) (pcborHead'nextOffset d) (pcborHead'value d)) $ \next ->
              pif (pcontrolIsWellFormed # next) (pcon $ PRedeemerItemProofAdvanced $ pdata next) (pcon PRedeemerItemProofInvalid)

pwithTailFields :: forall s. Term s PRedeemerItemProofControlV1 -> Term s PInteger -> Term s PInteger -> Term s PRedeemerItemProofControlV1
pwithTailFields control memory steps = pmatch control $ \c ->
  plet (pfromData (predeemerControl'mode c) #== pmodeData) $ \dataMode -> pcon $ PRedeemerItemProofControlV1
    (predeemerControl'version c) (predeemerControl'mode c) (pdata $ pif dataMode pstageData pstageTerminal)
    (predeemerControl'itemIndex c) (predeemerControl'itemCount c) (predeemerControl'totalLength c)
    (predeemerControl'itemCommitment c) (predeemerControl'expectedPurposeTag c) (predeemerControl'expectedPointerIndex c)
    (predeemerControl'purposeTag c) (predeemerControl'pointerIndex c) (predeemerControl'dataOffset c)
    (predeemerControl'dataLength c) (pdata memory) (pdata steps)
    (pdata $ pif dataMode
      (pcon $ PDJust $ pdata $ Traverse.pinitialControlV1 # pfromData (predeemerControl'dataOffset c)
        # pfromData (predeemerControl'dataLength c)) (pcon PDNothing))

ptailStep :: forall s. Term s PRedeemerItemProofControlV1 -> Term s PByteString -> Term s PRedeemerItemProofStepResultV1
ptailStep control source = pmatch (pheadAtV1 # source # 0 # 4) $ \case
  PNothing -> pcon PRedeemerItemProofInvalid
  PJust outer -> pmatch outer $ \o -> pif (pcborHead'value o #/= 2) (pcon PRedeemerItemProofInvalid) $
    pmatch (pheadAtV1 # source # pcborHead'nextOffset o # 0) $ \case
      PNothing -> pcon PRedeemerItemProofInvalid
      PJust memory -> pmatch memory $ \m -> pmatch (pheadAtV1 # source # pcborHead'nextOffset m # 0) $ \case
        PNothing -> pcon PRedeemerItemProofInvalid
        PJust steps -> pmatch steps $ \st ->
          pif (pcborHead'nextOffset st #/= plengthBS # source) (pcon PRedeemerItemProofInvalid) $
            plet (pwithTailFields control (pcborHead'value m) (pcborHead'value st)) $ \next ->
              pif (pcontrolIsWellFormed # next) (pcon $ PRedeemerItemProofAdvanced $ pdata next) (pcon PRedeemerItemProofInvalid)

padvanced :: forall s. Term s PRedeemerItemProofControlV1 -> Term s (PMaybe PRedeemerItemProofStepResultV1)
padvanced control = pif (pcontrolIsWellFormed # control)
  (pcon $ PJust $ pcon $ PRedeemerItemProofAdvanced $ pdata control) (pcon PNothing)

pwithTraversal :: forall s. Term s PRedeemerItemProofControlV1 -> Term s PDataTraverseControlV1 -> Term s PRedeemerItemProofControlV1
pwithTraversal control traversal = pmatch control $ \c -> pcon $ PRedeemerItemProofControlV1
  (predeemerControl'version c) (predeemerControl'mode c) (predeemerControl'stage c)
  (predeemerControl'itemIndex c) (predeemerControl'itemCount c) (predeemerControl'totalLength c)
  (predeemerControl'itemCommitment c) (predeemerControl'expectedPurposeTag c) (predeemerControl'expectedPointerIndex c)
  (predeemerControl'purposeTag c) (predeemerControl'pointerIndex c) (predeemerControl'dataOffset c)
  (predeemerControl'dataLength c) (predeemerControl'executionMemory c) (predeemerControl'executionSteps c)
  (pdata $ pcon $ PDJust $ pdata traversal)

pfinishData :: forall s. Term s PRedeemerItemProofControlV1 -> Term s PRedeemerItemProofControlV1
pfinishData control = pmatch control $ \c -> pcon $ PRedeemerItemProofControlV1
  (predeemerControl'version c) (predeemerControl'mode c) (pdata pstageTerminal)
  (predeemerControl'itemIndex c) (predeemerControl'itemCount c) (predeemerControl'totalLength c)
  (predeemerControl'itemCommitment c) (predeemerControl'expectedPurposeTag c) (predeemerControl'expectedPointerIndex c)
  (predeemerControl'purposeTag c) (predeemerControl'pointerIndex c) (predeemerControl'dataOffset c)
  (predeemerControl'dataLength c) (predeemerControl'executionMemory c) (predeemerControl'executionSteps c)
  (predeemerControl'traversal c)

papplyStep :: forall s. Term s PRedeemerItemProofControlV1 -> Term s (PMaybe PByteString) -> Term s PRedeemerItemProofActionV1 -> Term s (PMaybe PRedeemerItemProofStepResultV1)
papplyStep control source action = pmatch control $ \c ->
  pif (pfromData (predeemerControl'stage c) #== pstageHeader)
    (pmatch action $ \case
      PRedeemerItemOpenHeader -> pmatch source $ \case PNothing -> pcon PNothing; PJust bytes -> pcon $ PJust $ pheaderStep control bytes
      _ -> pcon PNothing) $
  pif (pfromData (predeemerControl'stage c) #== pstageTail)
    (pmatch action $ \case
      PRedeemerItemOpenTail -> pmatch source $ \case PNothing -> pcon PNothing; PJust bytes -> pcon $ PJust $ ptailStep control bytes
      _ -> pcon PNothing) $
  pif (pfromData (predeemerControl'stage c) #== pstageData)
    (pmatch (pfromData $ predeemerControl'traversal c) $ \case
      PDNothing -> pcon PNothing
      PDJust traversalData -> plet (pfromData traversalData) $ \traversal -> pmatch traversal $ \t ->
        pif (pfromData (Traverse.ptraverse'stage t) #== Traverse.pstageTerminal)
          (pmatch action $ \case
            PRedeemerItemFinishData -> pmatch source $ \case
              PNothing -> padvanced $ pfinishData control
              PJust _ -> pcon PNothing
            _ -> pcon PNothing)
          (pmatch action $ \case
            PRedeemerItemTraverseData traversalAction -> pmatch (Traverse.pstepV1 # traversal # source # pfromData traversalAction) $ \case
              PNothing -> pcon PNothing
              PJust next -> padvanced $ pwithTraversal control next
            _ -> pcon PNothing)) (pcon PNothing)

pauthenticateWitness :: forall s. Term s PRedeemerItemProofControlV1 -> Term s PRedeemerItemProofWitnessV1 -> Term s (PMaybe (PMaybe PByteString))
pauthenticateWitness control witness = pmatch witness $ \w ->
  pmatch (pnextSourceSpanV1 # control) $ \case
    PNothing -> pif (pfromData (predeemerWitness'chunkProof w) #== pcon PDNothing
        #&& pfromData (predeemerWitness'nextChunkProof w) #== pcon PDNothing)
      (pcon $ PJust $ pcon PNothing) (pcon PNothing)
    PJust spanValue -> pmatch (pfromData $ predeemerWitness'chunkProof w) $ \case
      PDNothing -> pcon PNothing
      PDJust proofData -> pmatch (pauthenticatedSpan control spanValue (pfromData proofData)
          (pfromData $ predeemerWitness'nextChunkProof w)) $ \case
        PNothing -> pcon PNothing
        PJust bytes -> pcon $ PJust $ pcon $ PJust bytes

pheaderProofStepV1 :: forall s. Term s (PRedeemerItemProofControlV1 :--> PRedeemerItemProofWitnessV1 :--> PMaybe PRedeemerItemProofStepResultV1)
pheaderProofStepV1 = phoistAcyclic $ plam $ \control witness ->
  pif (pnot # (pcontrolIsWellFormed # control)) (pcon PNothing) $ pmatch control $ \c ->
  pif (pfromData (predeemerControl'stage c) #/= pstageHeader) (pcon PNothing) $ pmatch witness $ \w ->
  pmatch (pfromData $ predeemerWitness'action w) $ \case
    PRedeemerItemOpenHeader -> pmatch (pauthenticateWitness control witness) $ \case
      PNothing -> pcon PNothing
      PJust source -> pmatch source $ \case
        PNothing -> pcon PNothing
        PJust bytes -> pcon $ PJust $ pheaderStep control bytes
    _ -> pcon PNothing

ptailProofStepV1 :: forall s. Term s (PRedeemerItemProofControlV1 :--> PRedeemerItemProofWitnessV1 :--> PMaybe PRedeemerItemProofStepResultV1)
ptailProofStepV1 = phoistAcyclic $ plam $ \control witness ->
  pif (pnot # (pcontrolIsWellFormed # control)) (pcon PNothing) $ pmatch control $ \c ->
  pif (pfromData (predeemerControl'stage c) #/= pstageTail) (pcon PNothing) $ pmatch witness $ \w ->
  pmatch (pfromData $ predeemerWitness'action w) $ \case
    PRedeemerItemOpenTail -> pmatch (pauthenticateWitness control witness) $ \case
      PNothing -> pcon PNothing
      PJust source -> pmatch source $ \case
        PNothing -> pcon PNothing
        PJust bytes -> pcon $ PJust $ ptailStep control bytes
    _ -> pcon PNothing

pstepV1 :: forall s. Term s (PRedeemerItemProofControlV1 :--> PRedeemerItemProofWitnessV1 :--> PMaybe PRedeemerItemProofStepResultV1)
pstepV1 = phoistAcyclic $ plam $ \control witness ->
  pif (pnot # (pcontrolIsWellFormed # control)) (pcon PNothing) $ pmatch witness $ \w ->
  pmatch (pauthenticateWitness control witness) $ \case
    PNothing -> pcon PNothing
    PJust source -> papplyStep control source (pfromData $ predeemerWitness'action w)
