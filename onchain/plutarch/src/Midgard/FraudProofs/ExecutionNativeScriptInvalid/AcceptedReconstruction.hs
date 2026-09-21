{-# LANGUAGE OverloadedStrings #-}

-- | Accepted-direction canonical purpose and source reconstruction.
module Midgard.FraudProofs.ExecutionNativeScriptInvalid.AcceptedReconstruction (
    PSelectedPurposeV1 (..),
    PSelectedSourceV1 (..),
    PAcceptedReconstructionStateV1 (..),
    pphaseSpend,
    pphaseMint,
    pphaseObserve,
    pphaseReceive,
    pphaseInlineSource,
    pphaseReferenceSource,
    pphaseComplete,
    pcheckpointV1,
    pinitialV1,
    pstateIsAuthenticV1,
    pscanReceiveOutputV1,
    pfinishReceivePassV1,
    padvanceNonScriptItemV1,
    pappendPurposeV1,
    pfinishPurposePhaseV1,
    pappendSourceV1,
    padvanceReferenceWithoutSourceV1,
    pauthenticatedSourceV1,
    pfinishInlineSourcesV1,
    pfinishReferenceSourcesV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Midgard.FraudProofs.ExecutionNativeScriptInvalid (
    PAuthenticatedExecutionSourceV1 (..),
    PBoundExecutionV1 (..),
 )
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteBytes)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject

data PSelectedPurposeV1 (s :: S) = PSelectedPurposeV1
    { pselectedPurpose'purposeKind :: Term s (PAsData PInteger)
    , pselectedPurpose'purposeIndex :: Term s (PAsData PInteger)
    , pselectedPurpose'scriptHash :: Term s (PAsData PByteString)
    , pselectedPurpose'subject :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PSelectedPurposeV1)

data PSelectedSourceV1 (s :: S) = PSelectedSourceV1
    { pselectedSource'sourceIndex :: Term s (PAsData PInteger)
    , pselectedSource'originKind :: Term s (PAsData PInteger)
    , pselectedSource'sourceKey :: Term s (PAsData PByteString)
    , pselectedSource'languageTag :: Term s (PAsData PInteger)
    , pselectedSource'scriptHash :: Term s (PAsData PByteString)
    , pselectedSource'totalLength :: Term s (PAsData PInteger)
    , pselectedSource'itemCommitment :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PSelectedSourceV1)

data PAcceptedReconstructionStateV1 (s :: S) = PAcceptedReconstructionStateV1
    { preconstruction'bound :: Term s (PAsData PBoundExecutionV1)
    , preconstruction'phase :: Term s (PAsData PInteger)
    , preconstruction'fieldCursor :: Term s (PAsData PInteger)
    , preconstruction'executionCursor :: Term s (PAsData PInteger)
    , preconstruction'previousKey :: Term s (PAsData PByteString)
    , preconstruction'receiveCandidate :: Term s (PAsData PByteString)
    , preconstruction'sourceBaseIndex :: Term s (PAsData PInteger)
    , preconstruction'sourceCursor :: Term s (PAsData PInteger)
    , preconstruction'selectedPurpose :: Term s (PAsData (PMaybeData PSelectedPurposeV1))
    , preconstruction'selectedSource :: Term s (PAsData (PMaybeData PSelectedSourceV1))
    , preconstruction'nextExpectedScriptHash :: Term s (PAsData PByteString)
    , preconstruction'checkpointHash :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PAcceptedReconstructionStateV1)

pphaseSpend, pphaseMint, pphaseObserve, pphaseReceive, pphaseInlineSource, pphaseReferenceSource, pphaseComplete :: forall s. Term s PInteger
pphaseSpend = 0
pphaseMint = 1
pphaseObserve = 2
pphaseReceive = 3
pphaseInlineSource = 4
pphaseReferenceSource = 5
pphaseComplete = 6

pcheckpointDomain :: forall s. Term s PByteString
pcheckpointDomain = pconstant "midgard/fraud-proofs/execution-native-script-invalid/accepted-reconstruction-v1"

pcheckpointV1 :: forall s. Term s (PAcceptedReconstructionStateV1 :--> PByteString :--> PByteString)
pcheckpointV1 = phoistAcyclic $ plam $ \state nextHash -> pmatch state $ \PAcceptedReconstructionStateV1{..} ->
    pmatch (pfromData preconstruction'bound) $ \PBoundExecutionV1{..} ->
        pblake2b_256
            #$ pcheckpointDomain
            <> (Subject.pencodeVerdictSubject # pfromData pboundExecution'subject)
            <> (pencodeDefiniteBytes # pfromData pboundExecution'compactCbor)
            <> pfromData pboundExecution'priorLedgerRoot
            <> pcborInt (pfromData pboundExecution'executionIndex)
            <> pcborInt (pfromData preconstruction'phase)
            <> pcborInt (pfromData preconstruction'fieldCursor)
            <> pcborInt (pfromData preconstruction'executionCursor)
            <> (pencodeDefiniteBytes # pfromData preconstruction'previousKey)
            <> (pencodeDefiniteBytes # pfromData preconstruction'receiveCandidate)
            <> pcborInt (pfromData preconstruction'sourceBaseIndex)
            <> pcborInt (pfromData preconstruction'sourceCursor)
            <> (pserialiseData # pforgetData preconstruction'selectedPurpose)
            <> (pserialiseData # pforgetData preconstruction'selectedSource)
            <> (pencodeDefiniteBytes # nextHash)

pwithCheckpoint :: forall s. Term s PAcceptedReconstructionStateV1 -> Term s PByteString -> Term s PAcceptedReconstructionStateV1
pwithCheckpoint state nextHash =
    let next = pmatch state $ \PAcceptedReconstructionStateV1{..} ->
            pcon $
                PAcceptedReconstructionStateV1
                    preconstruction'bound
                    preconstruction'phase
                    preconstruction'fieldCursor
                    preconstruction'executionCursor
                    preconstruction'previousKey
                    preconstruction'receiveCandidate
                    preconstruction'sourceBaseIndex
                    preconstruction'sourceCursor
                    preconstruction'selectedPurpose
                    preconstruction'selectedSource
                    (pdata nextHash)
                    preconstruction'checkpointHash
     in pmatch next $ \PAcceptedReconstructionStateV1{..} ->
            pcon $
                PAcceptedReconstructionStateV1
                    preconstruction'bound
                    preconstruction'phase
                    preconstruction'fieldCursor
                    preconstruction'executionCursor
                    preconstruction'previousKey
                    preconstruction'receiveCandidate
                    preconstruction'sourceBaseIndex
                    preconstruction'sourceCursor
                    preconstruction'selectedPurpose
                    preconstruction'selectedSource
                    preconstruction'nextExpectedScriptHash
                    (pdata $ pcheckpointV1 # next # nextHash)

pinitialV1 :: forall s. Term s (PBoundExecutionV1 :--> PByteString :--> PAcceptedReconstructionStateV1)
pinitialV1 = phoistAcyclic $ plam $ \bound nextHash ->
    pmatch bound $ \PBoundExecutionV1{pboundExecution'subject} ->
        pmatch (pfromData pboundExecution'subject) $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
            pif
                (pfromData psubject'direction #== 0)
                ( pwithCheckpoint
                    ( pcon $
                        PAcceptedReconstructionStateV1
                            (pdata bound)
                            (pdata pphaseSpend)
                            (pdata 0)
                            (pdata 0)
                            (pdata $ pconstant "")
                            (pdata $ pconstant "")
                            (pdata 0)
                            (pdata 0)
                            (pdata $ pcon PDNothing)
                            (pdata $ pcon PDNothing)
                            (pdata nextHash)
                            (pdata $ pconstant "")
                    )
                    nextHash
                )
                perror

pstateIsAuthenticV1 :: forall s. Term s (PAcceptedReconstructionStateV1 :--> PBool)
pstateIsAuthenticV1 = phoistAcyclic $ plam $ \state -> pmatch state $ \PAcceptedReconstructionStateV1{..} ->
    pmatch (pfromData preconstruction'bound) $ \PBoundExecutionV1{pboundExecution'subject} ->
        pmatch (pfromData pboundExecution'subject) $ \Subject.PVerdictSubject{Subject.psubject'direction} ->
            pand'List
                [ pfromData psubject'direction #== 0
                , pfromData preconstruction'phase #>= pphaseSpend
                , pfromData preconstruction'phase #<= pphaseComplete
                , pfromData preconstruction'fieldCursor #>= 0
                , pfromData preconstruction'executionCursor #>= 0
                , pfromData preconstruction'sourceBaseIndex #>= 0
                , pfromData preconstruction'sourceCursor #>= pfromData preconstruction'sourceBaseIndex
                , pfromData preconstruction'receiveCandidate #== pconstant "" #|| plengthBS # pfromData preconstruction'receiveCandidate #== 28
                , plengthBS # pfromData preconstruction'nextExpectedScriptHash #== 28
                , pfromData preconstruction'checkpointHash #== pcheckpointV1 # state # pfromData preconstruction'nextExpectedScriptHash
                ]

pscanReceiveOutputV1 :: forall s. Term s (PAcceptedReconstructionStateV1 :--> PMaybeData PByteString :--> PByteString :--> PAcceptedReconstructionStateV1)
pscanReceiveOutputV1 = phoistAcyclic $ plam $ \state candidate nextHash -> pmatch state $ \PAcceptedReconstructionStateV1{..} ->
    let valid = pstateIsAuthenticV1 # state #&& pfromData preconstruction'phase #== pphaseReceive #&& pfromData preconstruction'selectedPurpose #== pcon PDNothing
        receiveCandidate = pmatch candidate $ \case
            PDNothing -> pfromData preconstruction'receiveCandidate
            PDJust scriptHashData ->
                let scriptHash = pfromData scriptHashData
                 in pif
                        ( plengthBS
                            # scriptHash
                            #== 28
                            #&& (pfromData preconstruction'previousKey #== pconstant "" #|| pfromData preconstruction'previousKey #< scriptHash)
                            #&& (pfromData preconstruction'receiveCandidate #== pconstant "" #|| scriptHash #< pfromData preconstruction'receiveCandidate)
                        )
                        scriptHash
                        (pif (plengthBS # scriptHash #== 28) (pfromData preconstruction'receiveCandidate) perror)
        next =
            pcon $
                PAcceptedReconstructionStateV1
                    preconstruction'bound
                    preconstruction'phase
                    (pdata $ pfromData preconstruction'fieldCursor + 1)
                    preconstruction'executionCursor
                    preconstruction'previousKey
                    (pdata receiveCandidate)
                    preconstruction'sourceBaseIndex
                    preconstruction'sourceCursor
                    preconstruction'selectedPurpose
                    preconstruction'selectedSource
                    preconstruction'nextExpectedScriptHash
                    preconstruction'checkpointHash
     in pif valid (pwithCheckpoint next nextHash) perror

pappendPurposeV1 :: forall s. Term s (PAcceptedReconstructionStateV1 :--> PInteger :--> PInteger :--> PByteString :--> PByteString :--> PByteString :--> PByteString :--> PAcceptedReconstructionStateV1)
pappendPurposeV1 = phoistAcyclic $ plam $ \state purposeKind purposeIndex scriptHash subject canonicalKey nextHash -> pmatch state $ \PAcceptedReconstructionStateV1{..} ->
    let valid =
            pand'List
                [ pstateIsAuthenticV1 # state
                , pfromData preconstruction'selectedPurpose #== pcon PDNothing
                , purposeKind #== pfromData preconstruction'phase
                , purposeKind #>= pphaseSpend
                , purposeKind #<= pphaseReceive
                , purposeIndex #>= 0
                , plengthBS # scriptHash #== 28
                , pfromData preconstruction'previousKey #== pconstant "" #|| pfromData preconstruction'previousKey #< canonicalKey
                ]
        selected =
            pif
                ( pmatch (pfromData preconstruction'bound) $ \PBoundExecutionV1{pboundExecution'executionIndex} ->
                    pfromData preconstruction'executionCursor #== pfromData pboundExecution'executionIndex
                )
                (pcon $ PDJust $ pdata $ pcon $ PSelectedPurposeV1 (pdata purposeKind) (pdata purposeIndex) (pdata scriptHash) (pdata subject))
                (pcon PDNothing)
        isSelected = selected #/= pcon PDNothing
        next =
            pcon $
                PAcceptedReconstructionStateV1
                    preconstruction'bound
                    (pdata $ pif isSelected pphaseInlineSource (pfromData preconstruction'phase))
                    (pdata $ pif isSelected 0 (pfromData preconstruction'fieldCursor + 1))
                    (pdata $ pfromData preconstruction'executionCursor + 1)
                    (pdata $ pif isSelected (pconstant "") canonicalKey)
                    preconstruction'receiveCandidate
                    preconstruction'sourceBaseIndex
                    preconstruction'sourceCursor
                    (pdata selected)
                    preconstruction'selectedSource
                    preconstruction'nextExpectedScriptHash
                    preconstruction'checkpointHash
     in pif valid (pwithCheckpoint next nextHash) perror

pfinishReceivePassV1 :: forall s. Term s (PAcceptedReconstructionStateV1 :--> PByteString :--> PByteString :--> PAcceptedReconstructionStateV1)
pfinishReceivePassV1 = phoistAcyclic $ plam $ \state nextScanHash nextSourceHash -> pmatch state $ \PAcceptedReconstructionStateV1{..} ->
    let valid = pstateIsAuthenticV1 # state #&& pfromData preconstruction'phase #== pphaseReceive #&& pfromData preconstruction'selectedPurpose #== pcon PDNothing #&& pfromData preconstruction'receiveCandidate #/= pconstant ""
        nextHash = pmatch (pfromData preconstruction'bound) $ \PBoundExecutionV1{pboundExecution'executionIndex} -> pif (pfromData preconstruction'executionCursor #== pfromData pboundExecution'executionIndex) nextSourceHash nextScanHash
        emitted = pappendPurposeV1 # state # pphaseReceive # pfromData preconstruction'executionCursor # pfromData preconstruction'receiveCandidate # pfromData preconstruction'receiveCandidate # pfromData preconstruction'receiveCandidate # nextHash
     in pif
            valid
            ( pmatch emitted $ \PAcceptedReconstructionStateV1{..} ->
                pwithCheckpoint
                    ( pcon $
                        PAcceptedReconstructionStateV1
                            preconstruction'bound
                            preconstruction'phase
                            (pdata 0)
                            preconstruction'executionCursor
                            preconstruction'previousKey
                            (pdata $ pconstant "")
                            preconstruction'sourceBaseIndex
                            preconstruction'sourceCursor
                            preconstruction'selectedPurpose
                            preconstruction'selectedSource
                            preconstruction'nextExpectedScriptHash
                            preconstruction'checkpointHash
                    )
                    (pfromData preconstruction'nextExpectedScriptHash)
            )
            perror

padvanceNonScriptItemV1 :: forall s. Term s (PAcceptedReconstructionStateV1 :--> PByteString :--> PByteString :--> PAcceptedReconstructionStateV1)
padvanceNonScriptItemV1 = phoistAcyclic $ plam $ \state canonicalKey nextHash -> pmatch state $ \PAcceptedReconstructionStateV1{..} ->
    let valid = pstateIsAuthenticV1 # state #&& pfromData preconstruction'selectedPurpose #== pcon PDNothing #&& (pfromData preconstruction'previousKey #== pconstant "" #|| pfromData preconstruction'previousKey #< canonicalKey)
        next = pcon $ PAcceptedReconstructionStateV1 preconstruction'bound preconstruction'phase (pdata $ pfromData preconstruction'fieldCursor + 1) preconstruction'executionCursor (pdata canonicalKey) preconstruction'receiveCandidate preconstruction'sourceBaseIndex preconstruction'sourceCursor preconstruction'selectedPurpose preconstruction'selectedSource preconstruction'nextExpectedScriptHash preconstruction'checkpointHash
     in pif valid (pwithCheckpoint next nextHash) perror

pfinishPurposePhaseV1 :: forall s. Term s (PAcceptedReconstructionStateV1 :--> PByteString :--> PAcceptedReconstructionStateV1)
pfinishPurposePhaseV1 = phoistAcyclic $ plam $ \state nextHash -> pmatch state $ \PAcceptedReconstructionStateV1{..} ->
    let valid = pstateIsAuthenticV1 # state #&& pfromData preconstruction'phase #>= pphaseSpend #&& pfromData preconstruction'phase #<= pphaseReceive
        nextPhase = pif (pfromData preconstruction'selectedPurpose #/= pcon PDNothing) pphaseInlineSource (pfromData preconstruction'phase + 1)
        next = pcon $ PAcceptedReconstructionStateV1 preconstruction'bound (pdata nextPhase) (pdata 0) preconstruction'executionCursor (pdata $ pconstant "") preconstruction'receiveCandidate preconstruction'sourceBaseIndex preconstruction'sourceCursor preconstruction'selectedPurpose preconstruction'selectedSource preconstruction'nextExpectedScriptHash preconstruction'checkpointHash
     in pif (valid #&& nextPhase #<= pphaseInlineSource) (pwithCheckpoint next nextHash) perror

pappendSourceV1 :: forall s. Term s (PAcceptedReconstructionStateV1 :--> PSelectedSourceV1 :--> PByteString :--> PAcceptedReconstructionStateV1)
pappendSourceV1 = phoistAcyclic $ plam $ \state source nextHash -> pmatch state $ \PAcceptedReconstructionStateV1{..} ->
    pmatch source $ \PSelectedSourceV1{..} ->
        let valid =
                pand'List
                    [ pstateIsAuthenticV1 # state
                    , pfromData preconstruction'selectedSource #== pcon PDNothing
                    , pfromData preconstruction'phase #>= pphaseInlineSource
                    , pfromData preconstruction'phase #<= pphaseReferenceSource
                    , pfromData pselectedSource'originKind #== pfromData preconstruction'phase - pphaseInlineSource
                    , pfromData pselectedSource'sourceIndex #== pfromData preconstruction'sourceCursor
                    , pfromData pselectedSource'totalLength #> 0
                    , plengthBS # pfromData pselectedSource'scriptHash #== 28
                    , plengthBS # pfromData pselectedSource'itemCommitment #== 32
                    ]
            selected = pmatch (pfromData preconstruction'selectedPurpose) $ \case
                PDNothing -> perror
                PDJust purposeData -> pmatch (pfromData purposeData) $ \PSelectedPurposeV1{pselectedPurpose'scriptHash} ->
                    pif (pfromData pselectedSource'scriptHash #== pfromData pselectedPurpose'scriptHash) (pcon $ PDJust $ pdata source) (pcon PDNothing)
            next = pcon $ PAcceptedReconstructionStateV1 preconstruction'bound preconstruction'phase (pdata $ pfromData preconstruction'fieldCursor + 1) preconstruction'executionCursor preconstruction'previousKey preconstruction'receiveCandidate preconstruction'sourceBaseIndex (pdata $ pfromData preconstruction'sourceCursor + 1) preconstruction'selectedPurpose (pdata selected) preconstruction'nextExpectedScriptHash preconstruction'checkpointHash
         in pif valid (pwithCheckpoint next nextHash) perror

padvanceReferenceWithoutSourceV1 :: forall s. Term s (PAcceptedReconstructionStateV1 :--> PByteString :--> PAcceptedReconstructionStateV1)
padvanceReferenceWithoutSourceV1 = phoistAcyclic $ plam $ \state nextHash -> pmatch state $ \PAcceptedReconstructionStateV1{..} ->
    let valid = pstateIsAuthenticV1 # state #&& pfromData preconstruction'phase #== pphaseReferenceSource #&& pfromData preconstruction'selectedSource #== pcon PDNothing
        next = pcon $ PAcceptedReconstructionStateV1 preconstruction'bound preconstruction'phase (pdata $ pfromData preconstruction'fieldCursor + 1) preconstruction'executionCursor preconstruction'previousKey preconstruction'receiveCandidate preconstruction'sourceBaseIndex preconstruction'sourceCursor preconstruction'selectedPurpose preconstruction'selectedSource preconstruction'nextExpectedScriptHash preconstruction'checkpointHash
     in pif valid (pwithCheckpoint next nextHash) perror

pauthenticatedSourceV1 :: forall s. Term s (PAcceptedReconstructionStateV1 :--> PAuthenticatedExecutionSourceV1)
pauthenticatedSourceV1 = phoistAcyclic $ plam $ \state -> pmatch state $ \PAcceptedReconstructionStateV1{..} ->
    pmatch (pfromData preconstruction'bound) $ \bound@PBoundExecutionV1{pboundExecution'priorLedgerRoot, pboundExecution'compactCbor} ->
        pmatch (pfromData preconstruction'selectedPurpose) $ \case
            PDNothing -> perror
            PDJust purposeData -> pmatch (pfromData purposeData) $ \PSelectedPurposeV1{pselectedPurpose'purposeKind, pselectedPurpose'scriptHash} ->
                pmatch (pfromData preconstruction'selectedSource) $ \case
                    PDNothing -> perror
                    PDJust sourceData -> pmatch (pfromData sourceData) $ \PSelectedSourceV1{..} ->
                        pif
                            ( pand'List
                                [ pstateIsAuthenticV1 # state
                                , pfromData pselectedPurpose'scriptHash #== pfromData pselectedSource'scriptHash
                                , pfromData pselectedPurpose'purposeKind #>= pphaseSpend
                                , pfromData pselectedPurpose'purposeKind #<= pphaseReceive
                                ]
                            )
                            ( pcon $
                                PAuthenticatedExecutionSourceV1
                                    (pdata $ pcon bound)
                                    pboundExecution'priorLedgerRoot
                                    pselectedSource'sourceIndex
                                    pselectedSource'originKind
                                    pselectedSource'sourceKey
                                    pselectedSource'languageTag
                                    pselectedSource'scriptHash
                                    pselectedSource'totalLength
                                    pselectedSource'itemCommitment
                                    pboundExecution'compactCbor
                            )
                            perror

pfinishInlineSourcesV1 :: forall s. Term s (PAcceptedReconstructionStateV1 :--> PByteString :--> PAcceptedReconstructionStateV1)
pfinishInlineSourcesV1 = phoistAcyclic $ plam $ \state nextHash -> pmatch state $ \PAcceptedReconstructionStateV1{..} ->
    let valid = pstateIsAuthenticV1 # state #&& pfromData preconstruction'phase #== pphaseInlineSource
        nextPhase = pif (pfromData preconstruction'selectedSource #== pcon PDNothing) pphaseReferenceSource pphaseComplete
        next = pcon $ PAcceptedReconstructionStateV1 preconstruction'bound (pdata nextPhase) (pdata 0) preconstruction'executionCursor preconstruction'previousKey preconstruction'receiveCandidate preconstruction'sourceCursor preconstruction'sourceCursor preconstruction'selectedPurpose preconstruction'selectedSource preconstruction'nextExpectedScriptHash preconstruction'checkpointHash
     in pif valid (pwithCheckpoint next nextHash) perror

pfinishReferenceSourcesV1 :: forall s. Term s (PAcceptedReconstructionStateV1 :--> PByteString :--> PAcceptedReconstructionStateV1)
pfinishReferenceSourcesV1 = phoistAcyclic $ plam $ \state nextHash -> pmatch state $ \PAcceptedReconstructionStateV1{..} ->
    let valid = pstateIsAuthenticV1 # state #&& pfromData preconstruction'phase #== pphaseReferenceSource #&& pfromData preconstruction'selectedSource #/= pcon PDNothing
        next = pcon $ PAcceptedReconstructionStateV1 preconstruction'bound (pdata pphaseComplete) preconstruction'fieldCursor preconstruction'executionCursor preconstruction'previousKey preconstruction'receiveCandidate preconstruction'sourceBaseIndex preconstruction'sourceCursor preconstruction'selectedPurpose preconstruction'selectedSource preconstruction'nextExpectedScriptHash preconstruction'checkpointHash
     in pif valid (pwithCheckpoint next nextHash) perror
