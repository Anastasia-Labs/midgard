{-# LANGUAGE OverloadedStrings #-}

-- | Exact field-6 witness-script decoding rule and step ABI.
module Midgard.FraudProofs.WitnessScriptDecoding (
  PBoundWitnessScriptV1 (..),
  PWitnessScriptScanStateV1 (..),
  PStep01Source (..),
  PStep01Args (..),
  PStep02Args (..),
  PStep03Args (..),
  PStep04Args (..),
  pfieldIndex,
  presultPending,
  presultNoFault,
  presultHeaderMalformed,
  presultNativeMalformed,
  presultNodeLimit,
  presultDepthLimit,
  pbindSubjectV1,
  pcheckpointV1,
  pauthenticateItemV1,
  pstateIsAuthenticV1,
  pauthenticatedWindowV1,
  padvancedStateV1,
  pclosedStateV1,
  pmappedRefusalClassV1,
  pterminalContradictionV1,
  pencodeBoundV1,
  pencodeScanStateV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Midgard.BoundedItem qualified as Bounded
import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.NativeScriptDecoding.Engine qualified as Engine
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteBytes)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PHeaderV1)
import Midgard.NativeScriptScan qualified as Scan
import Midgard.RejectionReason (PRejectionReasonV1 (..))
import Midgard.TransitionTrace (PRootMembershipProof)

data PBoundWitnessScriptV1 (s :: S) = PBoundWitnessScriptV1
  { pboundWitness'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pboundWitness'witnessSetHash :: Term s (PAsData PByteString)
  , pboundWitness'scriptIndex :: Term s (PAsData PInteger)
  , pboundWitness'accusedClass :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBoundWitnessScriptV1)

data PWitnessScriptScanStateV1 (s :: S) = PWitnessScriptScanStateV1
  { pwitnessScan'bound :: Term s (PAsData PBoundWitnessScriptV1)
  , pwitnessScan'totalLength :: Term s (PAsData PInteger)
  , pwitnessScan'itemCommitment :: Term s (PAsData PByteString)
  , pwitnessScan'controlCbor :: Term s (PAsData PByteString)
  , pwitnessScan'nextExpectedScriptHash :: Term s (PAsData PByteString)
  , pwitnessScan'checkpointHash :: Term s (PAsData PByteString)
  , pwitnessScan'resultClass :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PWitnessScriptScanStateV1)

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
  , pstep01Args'scriptIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'opening :: Term s (PAsData PFieldOpeningV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  , pstep03Args'controlCbor :: Term s (PAsData PByteString)
  , pstep03Args'chunkProof :: Term s (PAsData (PMaybeData Bounded.PChunkProofV1))
  , pstep03Args'nextChunkProof :: Term s (PAsData (PMaybeData Bounded.PChunkProofV1))
  , pstep03Args'frames :: Term s (PAsData (PBuiltinList (PAsData Scan.PNativeScriptFrameV1)))
  , pstep03Args'stepBudget :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , pstep04Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

pfieldIndex, presultPending, presultNoFault, presultHeaderMalformed, presultNativeMalformed, presultNodeLimit, presultDepthLimit :: forall s. Term s PInteger
pfieldIndex = 6
presultPending = -1
presultNoFault = -2
presultHeaderMalformed = 0
presultNativeMalformed = 1
presultNodeLimit = 2
presultDepthLimit = 3

pcheckpointDomain :: forall s. Term s PByteString
pcheckpointDomain = pconstant "midgard/fraud-proofs/witness-script-decoding/checkpoint-v1"

pclassIsFault :: forall s. Term s (PInteger :--> PBool)
pclassIsFault = phoistAcyclic $ plam $ \value -> value #>= presultHeaderMalformed #&& value #<= presultDepthLimit

pbindSubjectV1 :: forall s. Term s (Subject.PVerdictSubject :--> PByteString :--> PInteger :--> PBoundWitnessScriptV1)
pbindSubjectV1 = phoistAcyclic $ plam $ \subject witnessSetHash scriptIndex ->
  pif
    (Subject.psubjectIsCanonical # subject #&& plengthBS # witnessSetHash #== 32 #&& scriptIndex #>= 0)
    ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
        let accusedClass =
              pif
                (pfromData psubject'direction #== 1)
                ( pmatch (Subject.prejectionReasonOf # subject) $ \case
                    PWitnessScriptHeaderMalformed actual -> pif (pfromData actual #== scriptIndex) presultHeaderMalformed perror
                    PWitnessNativeScriptMalformed actual -> pif (pfromData actual #== scriptIndex) presultNativeMalformed perror
                    PWitnessNativeScriptNodeLimit actual -> pif (pfromData actual #== scriptIndex) presultNodeLimit perror
                    PWitnessNativeScriptDepthLimit actual -> pif (pfromData actual #== scriptIndex) presultDepthLimit perror
                    _ -> perror
                )
                presultPending
         in pcon $ PBoundWitnessScriptV1 (pdata subject) (pdata witnessSetHash) (pdata scriptIndex) (pdata accusedClass)
    )
    perror

pcheckpointV1 :: forall s. Term s (PBoundWitnessScriptV1 :--> PInteger :--> PByteString :--> PByteString :--> PByteString :--> PByteString)
pcheckpointV1 = phoistAcyclic $ plam $ \bound totalLength itemCommitment controlCbor nextExpectedScriptHash ->
  pmatch bound $ \PBoundWitnessScriptV1{pboundWitness'subject, pboundWitness'scriptIndex} ->
    pblake2b_256
      #$ pcheckpointDomain
      <> (Subject.pencodeVerdictSubject # pfromData pboundWitness'subject)
      <> pcborInt (pfromData pboundWitness'scriptIndex)
      <> pcborInt totalLength
      <> (pencodeDefiniteBytes # itemCommitment)
      <> (pencodeDefiniteBytes # controlCbor)
      <> (pencodeDefiniteBytes # nextExpectedScriptHash)

pstateWith :: forall s. Term s PBoundWitnessScriptV1 -> Term s PInteger -> Term s PByteString -> Term s PByteString -> Term s PByteString -> Term s PInteger -> Term s PWitnessScriptScanStateV1
pstateWith bound totalLength itemCommitment controlCbor nextExpectedScriptHash resultClass =
  pcon $
    PWitnessScriptScanStateV1
      (pdata bound)
      (pdata totalLength)
      (pdata itemCommitment)
      (pdata controlCbor)
      (pdata nextExpectedScriptHash)
      (pdata $ pcheckpointV1 # bound # totalLength # itemCommitment # controlCbor # nextExpectedScriptHash)
      (pdata resultClass)

pauthenticateItemV1 :: forall s. Term s (PBoundWitnessScriptV1 :--> PByteString :--> PByteString :--> PWitnessScriptScanStateV1)
pauthenticateItemV1 = phoistAcyclic $ plam $ \bound item scanScriptHash ->
  pmatch bound $ \PBoundWitnessScriptV1{pboundWitness'scriptIndex} ->
    let totalLength = plengthBS # item
        itemCommitment = Bounded.pfromBytes # pfieldIndex # pfromData pboundWitness'scriptIndex # item
     in pmatch (Engine.pbindMachineV1 # item # totalLength) $ \case
          Engine.PMachineBindMalformedV1 ->
            pmatch (Scan.pversionedScriptHeaderV1 # item # totalLength) $ \case
              PNothing -> pstateWith bound totalLength itemCommitment (pconstant "") scanScriptHash presultHeaderMalformed
              PJust _ -> pstateWith bound totalLength itemCommitment (pconstant "") scanScriptHash presultNativeMalformed
          Engine.PMachineBindNonNativeV1 _ -> pstateWith bound totalLength itemCommitment (pconstant "") scanScriptHash presultNoFault
          Engine.PMachineBoundV1 control -> pstateWith bound totalLength itemCommitment (Scan.pencodeStructureControlV1 # pfromData control) scanScriptHash presultPending

pstateIsAuthenticV1 :: forall s. Term s (PWitnessScriptScanStateV1 :--> PBool)
pstateIsAuthenticV1 = phoistAcyclic $ plam $ \state -> pmatch state $ \PWitnessScriptScanStateV1{..} ->
  pand'List
    [ pfromData pwitnessScan'totalLength #>= 0
    , plengthBS # pfromData pwitnessScan'itemCommitment #== 32
    , plengthBS # pfromData pwitnessScan'nextExpectedScriptHash #== 28
    , pfromData pwitnessScan'checkpointHash
        #== pcheckpointV1
        # pfromData pwitnessScan'bound
        # pfromData pwitnessScan'totalLength
        # pfromData pwitnessScan'itemCommitment
        # pfromData pwitnessScan'controlCbor
        # pfromData pwitnessScan'nextExpectedScriptHash
    ]

pchunkMatches :: forall s. Term s PWitnessScriptScanStateV1 -> Term s Bounded.PChunkProofV1 -> Term s PInteger -> Term s PBool
pchunkMatches state proof expectedIndex = pmatch state $ \PWitnessScriptScanStateV1{pwitnessScan'bound, pwitnessScan'totalLength, pwitnessScan'itemCommitment} ->
  pmatch (pfromData pwitnessScan'bound) $ \PBoundWitnessScriptV1{pboundWitness'scriptIndex} ->
    pmatch proof $ \Bounded.PChunkProofV1{..} ->
      pand'List
        [ pfromData pchunkProof'fieldIndex #== pfieldIndex
        , pfromData pchunkProof'itemIndex #== pfromData pboundWitness'scriptIndex
        , pfromData pchunkProof'totalLength #== pfromData pwitnessScan'totalLength
        , pfromData pchunkProof'chunkIndex #== expectedIndex
        , Bounded.pverifyChunk # pfromData pwitnessScan'itemCommitment # proof
        ]

pauthenticatedWindowV1 :: forall s. Term s (PWitnessScriptScanStateV1 :--> Scan.PNativeScriptStructureControlV1 :--> Bounded.PChunkProofV1 :--> PMaybeData Bounded.PChunkProofV1 :--> Engine.PScanWindowV1)
pauthenticatedWindowV1 = phoistAcyclic $ plam $ \state control proof nextProof ->
  pmatch state $ \PWitnessScriptScanStateV1{pwitnessScan'totalLength} ->
    pmatch control $ \Scan.PNativeScriptStructureControlV1{Scan.pstructure'cursor} ->
      let index = pdiv # pfromData pstructure'cursor # Bounded.pchunkBytes
       in pif
            (pchunkMatches state proof index)
            ( pmatch proof $ \Bounded.PChunkProofV1{pchunkProof'chunk} ->
                pif
                  (index + 1 #< Bounded.pchunkCount # pfromData pwitnessScan'totalLength)
                  ( pmatch nextProof $ \case
                      PDNothing -> perror
                      PDJust nextData ->
                        let next = pfromData nextData
                         in pif
                              (pchunkMatches state next (index + 1))
                              ( pmatch next $ \Bounded.PChunkProofV1{pchunkProof'chunk = nextChunk} ->
                                  pcon $ Engine.PScanWindowV1 (pdata $ pfromData pchunkProof'chunk <> pfromData nextChunk) (pdata $ index * Bounded.pchunkBytes)
                              )
                              perror
                  )
                  ( pmatch nextProof $ \case
                      PDNothing -> pcon $ Engine.PScanWindowV1 pchunkProof'chunk (pdata $ index * Bounded.pchunkBytes)
                      PDJust _ -> perror
                  )
            )
            perror

padvancedStateV1 :: forall s. Term s (PWitnessScriptScanStateV1 :--> Scan.PNativeScriptStructureControlV1 :--> PByteString :--> PWitnessScriptScanStateV1)
padvancedStateV1 = phoistAcyclic $ plam $ \state control nextExpectedScriptHash -> pmatch state $ \PWitnessScriptScanStateV1{..} ->
  pstateWith
    (pfromData pwitnessScan'bound)
    (pfromData pwitnessScan'totalLength)
    (pfromData pwitnessScan'itemCommitment)
    (Scan.pencodeStructureControlV1 # control)
    nextExpectedScriptHash
    presultPending

pclosedStateV1 :: forall s. Term s (PWitnessScriptScanStateV1 :--> PInteger :--> PByteString :--> PWitnessScriptScanStateV1)
pclosedStateV1 = phoistAcyclic $ plam $ \state resultClass finalScriptHash -> pmatch state $ \PWitnessScriptScanStateV1{..} ->
  pif
    (resultClass #== presultNoFault #|| pclassIsFault # resultClass)
    ( pstateWith
        (pfromData pwitnessScan'bound)
        (pfromData pwitnessScan'totalLength)
        (pfromData pwitnessScan'itemCommitment)
        (pfromData pwitnessScan'controlCbor)
        finalScriptHash
        resultClass
    )
    perror

pmappedRefusalClassV1 :: forall s. Term s (PInteger :--> PInteger)
pmappedRefusalClassV1 = phoistAcyclic $ plam $ \scanClass ->
  pif
    (scanClass #== Engine.prefusalClassMalformed)
    presultNativeMalformed
    ( pif
        (scanClass #== Engine.prefusalClassNodeLimit)
        presultNodeLimit
        (pif (scanClass #== Engine.prefusalClassDepthLimit) presultDepthLimit perror)
    )

pterminalContradictionV1 :: forall s. Term s (PWitnessScriptScanStateV1 :--> PBool)
pterminalContradictionV1 = phoistAcyclic $ plam $ \state -> pmatch state $ \PWitnessScriptScanStateV1{pwitnessScan'bound, pwitnessScan'resultClass} ->
  pif
    (pstateIsAuthenticV1 # state #&& pfromData pwitnessScan'resultClass #/= presultPending)
    ( pmatch (pfromData pwitnessScan'bound) $ \PBoundWitnessScriptV1{pboundWitness'subject, pboundWitness'accusedClass} ->
        pmatch (pfromData pboundWitness'subject) $ \subject@Subject.PVerdictSubject{Subject.psubject'direction} ->
          pif
            (pfromData psubject'direction #== 1)
            (pif (pfromData pboundWitness'accusedClass #>= presultHeaderMalformed) (pfromData pwitnessScan'resultClass #/= pfromData pboundWitness'accusedClass) perror)
            (Subject.pterminalContradiction # pcon subject # (pclassIsFault # pfromData pwitnessScan'resultClass))
    )
    perror

pencodeBoundV1 :: forall s. Term s (PBoundWitnessScriptV1 :--> PByteString)
pencodeBoundV1 = phoistAcyclic $ plam $ \bound -> pserialiseData # pforgetData (pdata bound)

pencodeScanStateV1 :: forall s. Term s (PWitnessScriptScanStateV1 :--> PByteString)
pencodeScanStateV1 = phoistAcyclic $ plam $ \state -> pserialiseData # pforgetData (pdata state)
