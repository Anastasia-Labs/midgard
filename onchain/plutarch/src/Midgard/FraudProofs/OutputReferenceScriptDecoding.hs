{-# LANGUAGE OverloadedStrings #-}

-- | Exact output reference-script decoding rule and six-step ABI.
module Midgard.FraudProofs.OutputReferenceScriptDecoding (
    PBoundOutputV1 (..),
    POutputDescriptorStateV1 (..),
    PReferenceScriptScanStateV1 (..),
    PStep01Source (..),
    PStep01Args (..),
    PStep02Args (..),
    PStep03Args (..),
    PStep04Args (..),
    PStep05Args (..),
    PStep06Args (..),
    poutputsFieldIndex,
    pmaxOutputBytes,
    poutputScanning,
    poutputCanonical,
    poutputNonCanonical,
    presultPending,
    presultNoFault,
    presultMalformed,
    presultNodeLimit,
    presultDepthLimit,
    pbindOutputV1,
    pinitialOutputScanV1,
    poutputStateIsAuthenticV1,
    padvanceOutputScanV1,
    pcheckpointV1,
    pbindReferenceScriptV1,
    pscanStateIsAuthenticV1,
    pauthenticatedWindowV1,
    padvancedScanStateV1,
    pclosedScanStateV1,
    pmappedRefusalClassV1,
    pterminalContradictionV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.BoundedItem qualified as Bounded
import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.NativeScriptDecoding.Engine qualified as Engine
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteBytes)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerOutputScan qualified as OutputScan
import Midgard.LedgerState (PHeaderV1)
import Midgard.NativeScriptScan qualified as Scan
import Midgard.RejectionReason (PRejectionReasonV1 (..))
import Midgard.TransitionTrace (PRootMembershipProof)

data PBoundOutputV1 (s :: S) = PBoundOutputV1
    { pboundOutput'subject :: Term s (PAsData Subject.PVerdictSubject)
    , pboundOutput'outputIndex :: Term s (PAsData PInteger)
    , pboundOutput'accusedClass :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PBoundOutputV1)

data POutputDescriptorStateV1 (s :: S) = POutputDescriptorStateV1
    { poutputDescriptor'bound :: Term s (PAsData PBoundOutputV1)
    , poutputDescriptor'itemLength :: Term s (PAsData PInteger)
    , poutputDescriptor'itemHash :: Term s (PAsData PByteString)
    , poutputDescriptor'chunkHashes :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
    , poutputDescriptor'control :: Term s (PAsData OutputScan.PLedgerOutputScanControlV1)
    , poutputDescriptor'outcome :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct POutputDescriptorStateV1)

data PReferenceScriptScanStateV1 (s :: S) = PReferenceScriptScanStateV1
    { preferenceScan'bound :: Term s (PAsData PBoundOutputV1)
    , preferenceScan'totalLength :: Term s (PAsData PInteger)
    , preferenceScan'itemCommitment :: Term s (PAsData PByteString)
    , preferenceScan'controlCbor :: Term s (PAsData PByteString)
    , preferenceScan'nextExpectedScriptHash :: Term s (PAsData PByteString)
    , preferenceScan'checkpointHash :: Term s (PAsData PByteString)
    , preferenceScan'resultClass :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PReferenceScriptScanStateV1)

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
    , pstep01Args'outputIndex :: Term s (PAsData PInteger)
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
    , pstep03Args'window :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PStep04Args (s :: S) = PStep04Args
    { pstep04Args'inputIndex :: Term s (PAsData PInteger)
    , pstep04Args'outputIndex :: Term s (PAsData PInteger)
    , pstep04Args'opening :: Term s (PAsData PFieldOpeningV1)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

data PStep05Args (s :: S) = PStep05Args
    { pstep05Args'inputIndex :: Term s (PAsData PInteger)
    , pstep05Args'outputIndex :: Term s (PAsData PInteger)
    , pstep05Args'controlCbor :: Term s (PAsData PByteString)
    , pstep05Args'chunkProof :: Term s (PAsData (PMaybeData Bounded.PChunkProofV1))
    , pstep05Args'nextChunkProof :: Term s (PAsData (PMaybeData Bounded.PChunkProofV1))
    , pstep05Args'frames :: Term s (PAsData (PBuiltinList (PAsData Scan.PNativeScriptFrameV1)))
    , pstep05Args'stepBudget :: Term s (PAsData PInteger)
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

poutputsFieldIndex
    , pmaxOutputBytes
    , poutputScanning
    , poutputCanonical
    , poutputNonCanonical
    , presultPending
    , presultNoFault
    , presultMalformed
    , presultNodeLimit
    , presultDepthLimit
    , poutputChunkBytes ::
        forall s. Term s PInteger
poutputsFieldIndex = 2
pmaxOutputBytes = 16384
poutputScanning = 0
poutputCanonical = 1
poutputNonCanonical = 2
presultPending = -1
presultNoFault = -2
presultMalformed = 0
presultNodeLimit = 1
presultDepthLimit = 2
poutputChunkBytes = 4095

pcheckpointDomain :: forall s. Term s PByteString
pcheckpointDomain = pconstant "midgard/fraud-proofs/output-reference-script-decoding/checkpoint-v1"

pclassIsFault :: forall s. Term s (PInteger :--> PBool)
pclassIsFault = phoistAcyclic $ plam $ \value -> value #>= presultMalformed #&& value #<= presultDepthLimit

pbindOutputV1 :: forall s. Term s (Subject.PVerdictSubject :--> PInteger :--> PBoundOutputV1)
pbindOutputV1 = phoistAcyclic $ plam $ \subject outputIndex ->
    pif
        (Subject.psubjectIsCanonical # subject #&& outputIndex #>= 0)
        ( pmatch subject $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
            let accusedClass =
                    pif
                        (pfromData psubject'direction #== 1)
                        ( pmatch (Subject.prejectionReasonOf # subject) $ \case
                            POutputReferenceScriptMalformed actual -> pif (pfromData actual #== outputIndex) presultMalformed perror
                            POutputReferenceScriptNodeLimit actual -> pif (pfromData actual #== outputIndex) presultNodeLimit perror
                            POutputReferenceScriptDepthLimit actual -> pif (pfromData actual #== outputIndex) presultDepthLimit perror
                            _ -> perror
                        )
                        presultPending
             in pcon $ PBoundOutputV1 (pdata subject) (pdata outputIndex) (pdata accusedClass)
        )
        perror

pminimumInteger :: forall s. Term s (PInteger :--> PInteger :--> PInteger)
pminimumInteger = phoistAcyclic $ plam $ \left right -> pif (left #< right) left right

poutputChunkHashes :: forall s. Term s (PByteString :--> PInteger :--> PInteger :--> PBuiltinList (PAsData PByteString))
poutputChunkHashes = phoistAcyclic $ pfix $ \self -> plam $ \item offset total ->
    pif (offset #>= total) pnil $ P.do
        len <- plet $ pminimumInteger # poutputChunkBytes # (total - offset)
        pcons # pdata (pblake2b_256 # (psliceBS # offset # len # item)) # (self # item # (offset + len) # total)

pinitialOutputScanV1 :: forall s. Term s (PBoundOutputV1 :--> PByteString :--> POutputDescriptorStateV1)
pinitialOutputScanV1 = phoistAcyclic $ plam $ \bound item ->
    plet (plengthBS # item) $ \itemLength ->
        pif
            (itemLength #> 0 #&& itemLength #<= pmaxOutputBytes)
            ( pcon $
                POutputDescriptorStateV1
                    (pdata bound)
                    (pdata itemLength)
                    (pdata $ pblake2b_256 # item)
                    (pdata $ poutputChunkHashes # item # 0 # itemLength)
                    (pdata OutputScan.pinitialControlV1)
                    (pdata poutputScanning)
            )
            perror

poutputStateIsAuthenticV1 :: forall s. Term s (POutputDescriptorStateV1 :--> PBool)
poutputStateIsAuthenticV1 = phoistAcyclic $ plam $ \state -> pmatch state $ \POutputDescriptorStateV1{..} ->
    let itemLength = pfromData poutputDescriptor'itemLength
        hashes = pfromData poutputDescriptor'chunkHashes
        outcome = pfromData poutputDescriptor'outcome
     in pand'List
            [ itemLength #> 0
            , itemLength #<= pmaxOutputBytes
            , plengthBS # pfromData poutputDescriptor'itemHash #== 32
            , plength # hashes #== pdiv # (itemLength + poutputChunkBytes - 1) # poutputChunkBytes
            , pall # (plam $ \hash -> plengthBS # pfromData hash #== 32) # hashes
            , OutputScan.pcontrolIsWellFormed # pfromData poutputDescriptor'control
            , outcome #>= poutputScanning
            , outcome #<= poutputNonCanonical
            ]

padvanceOutputScanV1 :: forall s. Term s (POutputDescriptorStateV1 :--> PByteString :--> POutputDescriptorStateV1)
padvanceOutputScanV1 = phoistAcyclic $ plam $ \state window ->
    pif
        (poutputStateIsAuthenticV1 # state)
        ( pmatch state $ \st@POutputDescriptorStateV1{..} ->
            pif
                (pfromData poutputDescriptor'outcome #== poutputScanning)
                ( let control = pfromData poutputDescriptor'control
                      itemLength = pfromData poutputDescriptor'itemLength
                      hashes = pfromData poutputDescriptor'chunkHashes
                   in pmatch control $ \OutputScan.PLedgerOutputScanControlV1{OutputScan.pscan'stage, OutputScan.pscan'cursor} ->
                        let stage = pfromData pscan'stage
                            cursor = pfromData pscan'cursor
                            next =
                                pif
                                    (stage #== OutputScan.pstageOptionalField #&& cursor #== itemLength)
                                    (OutputScan.pfinishV1 # control # itemLength)
                                    ( pif
                                        (cursor #< itemLength)
                                        ( let chunkStart = cursor - pmod # cursor # poutputChunkBytes
                                              chunkIndex = pdiv # cursor # poutputChunkBytes
                                              currentLength = pminimumInteger # poutputChunkBytes # (itemLength - chunkStart)
                                              currentHash = pfromData $ pelemAt # chunkIndex # hashes
                                              nextLength = pminimumInteger # poutputChunkBytes # (itemLength - chunkStart - currentLength)
                                              windowLength = currentLength + pif (stage #<= OutputScan.pstageOptionalField #&& nextLength #> 0) nextLength 0
                                           in pif
                                                ( pblake2b_256
                                                    # (psliceBS # 0 # currentLength # window)
                                                    #== currentHash
                                                    #&& plengthBS
                                                    # window
                                                    #== windowLength
                                                    #&& pif
                                                        (windowLength #> currentLength)
                                                        (pblake2b_256 # (psliceBS # currentLength # (windowLength - currentLength) # window) #== pfromData (pelemAt # (chunkIndex + 1) # hashes))
                                                        (pconstant True)
                                                )
                                                (OutputScan.pstepV1 # control # itemLength # window # (cursor - chunkStart))
                                                perror
                                        )
                                        (pcon PNothing)
                                    )
                         in pmatch next $ \case
                                PNothing -> pcon st{poutputDescriptor'outcome = pdata poutputNonCanonical}
                                PJust nextControl ->
                                    pcon
                                        st
                                            { poutputDescriptor'control = pdata nextControl
                                            , poutputDescriptor'outcome = pdata $ pif (OutputScan.pterminalIsExactV1 # nextControl # itemLength) poutputCanonical poutputScanning
                                            }
                )
                perror
        )
        perror

pcheckpointV1 :: forall s. Term s (PBoundOutputV1 :--> PInteger :--> PByteString :--> PByteString :--> PByteString :--> PByteString)
pcheckpointV1 = phoistAcyclic $ plam $ \bound totalLength itemCommitment controlCbor nextExpectedScriptHash ->
    pmatch bound $ \PBoundOutputV1{..} ->
        pblake2b_256
            #$ pcheckpointDomain
            <> (Subject.pencodeVerdictSubject # pfromData pboundOutput'subject)
            <> pcborInt (pfromData pboundOutput'outputIndex)
            <> pcborInt (pfromData pboundOutput'accusedClass)
            <> pcborInt totalLength
            <> (pencodeDefiniteBytes # itemCommitment)
            <> (pencodeDefiniteBytes # controlCbor)
            <> (pencodeDefiniteBytes # nextExpectedScriptHash)

pscanStateWith :: forall s. Term s PBoundOutputV1 -> Term s PInteger -> Term s PByteString -> Term s PByteString -> Term s PByteString -> Term s PInteger -> Term s PReferenceScriptScanStateV1
pscanStateWith bound totalLength itemCommitment controlCbor nextExpectedScriptHash resultClass =
    pcon $
        PReferenceScriptScanStateV1
            (pdata bound)
            (pdata totalLength)
            (pdata itemCommitment)
            (pdata controlCbor)
            (pdata nextExpectedScriptHash)
            (pdata $ pcheckpointV1 # bound # totalLength # itemCommitment # controlCbor # nextExpectedScriptHash)
            (pdata resultClass)

pbindReferenceScriptV1 :: forall s. Term s (POutputDescriptorStateV1 :--> PByteString :--> PByteString :--> PReferenceScriptScanStateV1)
pbindReferenceScriptV1 = phoistAcyclic $ plam $ \outputState outputItem scanScriptHash ->
    pif
        (poutputStateIsAuthenticV1 # outputState)
        ( pmatch outputState $ \POutputDescriptorStateV1{..} ->
            let itemLength = pfromData poutputDescriptor'itemLength
                control = pfromData poutputDescriptor'control
                bound = pfromData poutputDescriptor'bound
             in pif
                    ( pfromData poutputDescriptor'outcome
                        #== poutputCanonical
                        #&& plengthBS
                        # outputItem
                        #== itemLength
                        #&& pblake2b_256
                        # outputItem
                        #== pfromData poutputDescriptor'itemHash
                    )
                    ( pmatch control $ \OutputScan.PLedgerOutputScanControlV1{OutputScan.pscan'referenceScriptLanguage, OutputScan.pscan'referenceScriptItemOffset} ->
                        pif
                            (pfromData pscan'referenceScriptLanguage #== -1)
                            ( pmatch bound $ \PBoundOutputV1{pboundOutput'outputIndex} ->
                                pscanStateWith bound 0 (Bounded.pfromBytes # poutputsFieldIndex # pfromData pboundOutput'outputIndex # pconstant "") (pconstant "") scanScriptHash presultNoFault
                            )
                            ( let offset = pfromData pscan'referenceScriptItemOffset
                               in pif
                                    (offset #>= 0 #&& offset #< itemLength)
                                    ( let item = psliceBS # offset # (itemLength - offset) # outputItem
                                          totalLength = plengthBS # item
                                          commitment = pmatch bound $ \PBoundOutputV1{pboundOutput'outputIndex} -> Bounded.pfromBytes # poutputsFieldIndex # pfromData pboundOutput'outputIndex # item
                                       in pmatch (Engine.pbindMachineV1 # item # totalLength) $ \case
                                            Engine.PMachineBindMalformedV1 -> pscanStateWith bound totalLength commitment (pconstant "") scanScriptHash presultMalformed
                                            Engine.PMachineBindNonNativeV1 _ -> pscanStateWith bound totalLength commitment (pconstant "") scanScriptHash presultNoFault
                                            Engine.PMachineBoundV1 machineControl -> pscanStateWith bound totalLength commitment (Scan.pencodeStructureControlV1 # pfromData machineControl) scanScriptHash presultPending
                                    )
                                    perror
                            )
                    )
                    perror
        )
        perror

pscanStateIsAuthenticV1 :: forall s. Term s (PReferenceScriptScanStateV1 :--> PBool)
pscanStateIsAuthenticV1 = phoistAcyclic $ plam $ \state -> pmatch state $ \PReferenceScriptScanStateV1{..} ->
    pand'List
        [ pfromData preferenceScan'totalLength #>= 0
        , plengthBS # pfromData preferenceScan'itemCommitment #== 32
        , plengthBS # pfromData preferenceScan'nextExpectedScriptHash #== 28
        , pfromData preferenceScan'checkpointHash
            #== pcheckpointV1
            # pfromData preferenceScan'bound
            # pfromData preferenceScan'totalLength
            # pfromData preferenceScan'itemCommitment
            # pfromData preferenceScan'controlCbor
            # pfromData preferenceScan'nextExpectedScriptHash
        ]

pchunkMatches :: forall s. Term s PReferenceScriptScanStateV1 -> Term s Bounded.PChunkProofV1 -> Term s PInteger -> Term s PBool
pchunkMatches state proof expectedIndex = pmatch state $ \PReferenceScriptScanStateV1{preferenceScan'bound, preferenceScan'totalLength, preferenceScan'itemCommitment} ->
    pmatch (pfromData preferenceScan'bound) $ \PBoundOutputV1{pboundOutput'outputIndex} ->
        pmatch proof $ \Bounded.PChunkProofV1{..} ->
            pand'List
                [ pfromData pchunkProof'fieldIndex #== poutputsFieldIndex
                , pfromData pchunkProof'itemIndex #== pfromData pboundOutput'outputIndex
                , pfromData pchunkProof'totalLength #== pfromData preferenceScan'totalLength
                , pfromData pchunkProof'chunkIndex #== expectedIndex
                , Bounded.pverifyChunk # pfromData preferenceScan'itemCommitment # proof
                ]

pauthenticatedWindowV1 :: forall s. Term s (PReferenceScriptScanStateV1 :--> Scan.PNativeScriptStructureControlV1 :--> Bounded.PChunkProofV1 :--> PMaybeData Bounded.PChunkProofV1 :--> Engine.PScanWindowV1)
pauthenticatedWindowV1 = phoistAcyclic $ plam $ \state control proof nextProof ->
    pmatch state $ \PReferenceScriptScanStateV1{preferenceScan'totalLength} ->
        pmatch control $ \Scan.PNativeScriptStructureControlV1{Scan.pstructure'cursor} ->
            let index = pdiv # pfromData pstructure'cursor # Bounded.pchunkBytes
             in pif
                    (pchunkMatches state proof index)
                    ( pmatch proof $ \Bounded.PChunkProofV1{pchunkProof'chunk} ->
                        pif
                            (index + 1 #< Bounded.pchunkCount # pfromData preferenceScan'totalLength)
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

padvancedScanStateV1 :: forall s. Term s (PReferenceScriptScanStateV1 :--> Scan.PNativeScriptStructureControlV1 :--> PByteString :--> PReferenceScriptScanStateV1)
padvancedScanStateV1 = phoistAcyclic $ plam $ \state control nextHash -> pmatch state $ \PReferenceScriptScanStateV1{..} ->
    pscanStateWith
        (pfromData preferenceScan'bound)
        (pfromData preferenceScan'totalLength)
        (pfromData preferenceScan'itemCommitment)
        (Scan.pencodeStructureControlV1 # control)
        nextHash
        presultPending

pclosedScanStateV1 :: forall s. Term s (PReferenceScriptScanStateV1 :--> PInteger :--> PByteString :--> PReferenceScriptScanStateV1)
pclosedScanStateV1 = phoistAcyclic $ plam $ \state resultClass finalHash -> pmatch state $ \PReferenceScriptScanStateV1{..} ->
    pif
        (resultClass #== presultNoFault #|| pclassIsFault # resultClass)
        ( pscanStateWith
            (pfromData preferenceScan'bound)
            (pfromData preferenceScan'totalLength)
            (pfromData preferenceScan'itemCommitment)
            (pfromData preferenceScan'controlCbor)
            finalHash
            resultClass
        )
        perror

pmappedRefusalClassV1 :: forall s. Term s (PInteger :--> PInteger)
pmappedRefusalClassV1 = phoistAcyclic $ plam $ \scanClass ->
    pif
        (scanClass #== Engine.prefusalClassMalformed)
        presultMalformed
        (pif (scanClass #== Engine.prefusalClassNodeLimit) presultNodeLimit (pif (scanClass #== Engine.prefusalClassDepthLimit) presultDepthLimit perror))

pterminalContradictionV1 :: forall s. Term s (PReferenceScriptScanStateV1 :--> PBool)
pterminalContradictionV1 = phoistAcyclic $ plam $ \state -> pmatch state $ \PReferenceScriptScanStateV1{preferenceScan'bound, preferenceScan'resultClass} ->
    pif
        (pscanStateIsAuthenticV1 # state #&& pfromData preferenceScan'resultClass #/= presultPending)
        ( pmatch (pfromData preferenceScan'bound) $ \PBoundOutputV1{pboundOutput'subject, pboundOutput'accusedClass} ->
            pmatch (pfromData pboundOutput'subject) $ \subject@Subject.PVerdictSubject{Subject.psubject'direction} ->
                pif
                    (pfromData psubject'direction #== 1)
                    (pfromData preferenceScan'resultClass #/= pfromData pboundOutput'accusedClass)
                    (Subject.pterminalContradiction # pcon subject # (pclassIsFault # pfromData preferenceScan'resultClass))
        )
        perror
