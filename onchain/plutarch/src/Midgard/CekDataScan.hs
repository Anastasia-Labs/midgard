{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.CekDataScan
Description : Plutarch port of @lib/midgard/cek-data-scan-v1.ak@.
-}
module Midgard.CekDataScan (
  PDataScanFrameV1 (..),
  PRawDataLeafV1 (..),
  POpenedDataFrameV1 (..),
  PDataScanControlV1 (..),
  prootFrame,
  pconstructorFrame,
  plistFrame,
  pmapFrame,
  pmaxRevealedDataLeafBytes,
  pemptyResultV1,
  pcontrolIsWellFormed,
  pencodeControlV1,
  pcontrolFromCborV1,
  phashControlV1,
  pinitialControlV1,
  pframeIsWellFormed,
  phashFrameV1,
  pchildLeafHashV1,
  pappendChildV1,
  pfoldListChildV1,
  pfoldMapPairV1,
  pfinalizedSummaryV1,
  pboundedBlobRootV1,
  psmallLeafAtV1,
  prevealedLeafAtV1,
  popenSmallConstructorAtV1,
  popenConstructorAtV1,
  popenListAtV1,
  popenMapAtV1,
  pcloseSequenceAtV1,
  pappendCompletedChildV1,
  popenConstructorControlStepV1,
  popenListControlStepV1,
  popenMapControlStepV1,
  prevealLeafControlStepV1,
  pcloseSequenceControlStepV1,
  pfoldListControlStepV1,
  pfoldMapControlStepV1,
  pfinalizeFrameControlStepV1,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.ByteString (pintegerToByteString, pmostSignificantFirst)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.CekData qualified as Data
import Midgard.CekProof qualified as Proof
import Midgard.FraudProofs.NativeTx.Codec (
  pcborInt,
  pencodeDefiniteArrayHeader,
  pencodeDefiniteBytes,
  psliceLen,
 )
import Midgard.FraudProofs.NativeTx.Preimages (pdecodeCanonicalMapHeaderAt)
import Midgard.ValidationMerkle qualified as Merkle

pframeDomain, pchildDomain :: forall (s :: S). Term s PByteString
pframeDomain = pconstant "MidgardCekDataScanFrameV1"
pchildDomain = pconstant "MidgardCekDataScanChildV1"

prootFrame, pconstructorFrame, plistFrame, pmapFrame :: forall (s :: S). Term s PInteger
prootFrame = 0
pconstructorFrame = 1
plistFrame = 2
pmapFrame = 3

pmaxRevealedDataLeafBytes :: forall (s :: S). Term s PInteger
pmaxRevealedDataLeafBytes = Proof.pmaxBoundedBlobBytesV1

data PDataScanFrameV1 (s :: S) = PDataScanFrameV1
  { pframe'kind :: Term s (PAsData PInteger)
  , pframe'constructor :: Term s (PAsData PInteger)
  , pframe'tail :: Term s (PAsData PByteString)
  , pframe'expectedChildren :: Term s (PAsData PInteger)
  , pframe'childCount :: Term s (PAsData PInteger)
  , pframe'childPeaks :: Term s (PAsData (PBuiltinList (PAsData Merkle.PFrontierPeak)))
  , pframe'foldCursor :: Term s (PAsData PInteger)
  , pframe'sequence :: Term s (PAsData Data.PDataSequenceSummaryV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDataScanFrameV1)

data PRawDataLeafV1 (s :: S) = PRawDataLeafV1
  { pleaf'nextOffset :: Term s PInteger
  , pleaf'summary :: Term s Data.PDataSummaryV1
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsSOPStruct PRawDataLeafV1)

data POpenedDataFrameV1 (s :: S) = POpenedDataFrameV1
  { popened'nextOffset :: Term s PInteger
  , popened'frame :: Term s PDataScanFrameV1
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsSOPStruct POpenedDataFrameV1)

data PDataScanControlV1 (s :: S) = PDataScanControlV1
  { pscan'rawHash :: Term s (PAsData PByteString)
  , pscan'rawLength :: Term s (PAsData PInteger)
  , pscan'offset :: Term s (PAsData PInteger)
  , pscan'frameRoot :: Term s (PAsData PByteString)
  , pscan'frameClosed :: Term s (PAsData PBool)
  , pscan'result :: Term s (PAsData Data.PDataSummaryV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDataScanControlV1)

pencodeSequence :: forall (s :: S). Term s (Data.PDataSequenceSummaryV1 :--> PByteString)
pencodeSequence = phoistAcyclic $
  plam $ \summary ->
    pmatch summary $ \s ->
      pconstant "\x84"
        <> (pencodeDefiniteBytes # pfromData (Data.pseq'root s))
        <> pcborInt (pfromData (Data.pseq'length s))
        <> pcborInt (pfromData (Data.pseq'payloadCborLength s))
        <> pcborInt (pfromData (Data.pseq'memory s))

pencodeSummary :: forall (s :: S). Term s (Data.PDataSummaryV1 :--> PByteString)
pencodeSummary = phoistAcyclic $
  plam $ \summary ->
    pmatch summary $ \s ->
      pconstant "\x83"
        <> (pencodeDefiniteBytes # pfromData (Data.psummary'root s))
        <> pcborInt (pfromData (Data.psummary'cborLength s))
        <> pcborInt (pfromData (Data.psummary'memory s))

pemptyResultV1 :: forall (s :: S). Term s Data.PDataSummaryV1
pemptyResultV1 =
  pcon $
    Data.PDataSummaryV1
      { Data.psummary'root = pdata (pconstant "")
      , Data.psummary'cborLength = pdata 0
      , Data.psummary'memory = pdata 0
      }

poptionalSummaryIsWellFormed :: forall (s :: S). Term s (Data.PDataSummaryV1 :--> PBool)
poptionalSummaryIsWellFormed = phoistAcyclic $
  plam $ \summary ->
    pmatch summary $ \s ->
      pif
        (pfromData (Data.psummary'root s) #== pconstant "")
        ( pfromData (Data.psummary'cborLength s) #== 0
            #&& pfromData (Data.psummary'memory s) #== 0
        )
        ( plengthBS # pfromData (Data.psummary'root s) #== 32
            #&& 0 #<= pfromData (Data.psummary'cborLength s)
            #&& 0 #<= pfromData (Data.psummary'memory s)
        )

pcontrolIsWellFormed :: forall (s :: S). Term s (PDataScanControlV1 :--> PBool)
pcontrolIsWellFormed = phoistAcyclic $
  plam $ \control ->
    pmatch control $ \c ->
      pand'List
        [ plengthBS # pfromData (pscan'rawHash c) #== 32
        , 0 #< pfromData (pscan'rawLength c)
        , pfromData (pscan'rawLength c) #<= pmaxRevealedDataLeafBytes
        , 0 #<= pfromData (pscan'offset c)
        , pfromData (pscan'offset c) #<= pfromData (pscan'rawLength c)
        , pfromData (pscan'frameRoot c) #== pconstant ""
            #|| plengthBS # pfromData (pscan'frameRoot c) #== 32
        , poptionalSummaryIsWellFormed # pfromData (pscan'result c)
        ]
        #&& pcontrolPhaseIsWellFormed c

pcontrolPhaseIsWellFormed :: forall (s :: S). PDataScanControlV1 s -> Term s PBool
pcontrolPhaseIsWellFormed c =
  pmatch (pfromData (pscan'result c)) $ \result ->
    pif
      (pnot # (pfromData (Data.psummary'root result) #== pconstant ""))
      ( pfromData (pscan'frameRoot c) #== pconstant ""
          #&& pnot # pfromData (pscan'frameClosed c)
          #&& pfromData (pscan'offset c) #== pfromData (pscan'rawLength c)
      )
      $ pif
        (pfromData (pscan'frameRoot c) #== pconstant "")
        ( pnot # pfromData (pscan'frameClosed c)
            #&& pfromData (pscan'offset c) #== 0
        )
        (pconstant True)

pencodeControlV1 :: forall (s :: S). Term s (PDataScanControlV1 :--> PByteString)
pencodeControlV1 = phoistAcyclic $
  plam $ \control ->
    pif (pnot # (pcontrolIsWellFormed # control)) perror $
      pmatch control $ \c ->
        (pencodeDefiniteArrayHeader # 6)
          <> (pencodeDefiniteBytes # pfromData (pscan'rawHash c))
          <> pcborInt (pfromData (pscan'rawLength c))
          <> pcborInt (pfromData (pscan'offset c))
          <> (pencodeDefiniteBytes # pfromData (pscan'frameRoot c))
          <> (pserialiseData # pforgetData (pscan'frameClosed c))
          <> (pencodeSummary # pfromData (pscan'result c))

psummaryFromData :: forall (s :: S). Term s (PData :--> Data.PDataSummaryV1)
psummaryFromData = phoistAcyclic $
  plam $ \d ->
    plet (pasList # d) $ \items ->
      pif (pnot # (plength # items #== 3)) perror $
        pcon $
          Data.PDataSummaryV1
            { Data.psummary'root = pdata (pasByteStr # (pelemAt # 0 # items))
            , Data.psummary'cborLength = pdata (pasInt # (pelemAt # 1 # items))
            , Data.psummary'memory = pdata (pasInt # (pelemAt # 2 # items))
            }

pboolFromData :: forall (s :: S). Term s (PData :--> PBool)
pboolFromData = phoistAcyclic $
  plam $ \d ->
    pmatch (pasConstr # d) $ \(PBuiltinPair index fields) ->
      pif (pnot # (pnull # fields)) perror $
        pif (index #== 0) (pconstant False) $
          pif (index #== 1) (pconstant True) perror

pcontrolFromCborV1 :: forall (s :: S). Term s (PByteString :--> PDataScanControlV1)
pcontrolFromCborV1 = phoistAcyclic $
  plam $ \controlCbor ->
    pmatch (pdeserialise # controlCbor) $ \case
      PNothing -> perror
      PJust d ->
        plet (pasList # d) $ \items ->
          pif (pnot # (plength # items #== 6)) perror $
            plet
              ( pcon $
                  PDataScanControlV1
                    { pscan'rawHash = pdata (pasByteStr # (pelemAt # 0 # items))
                    , pscan'rawLength = pdata (pasInt # (pelemAt # 1 # items))
                    , pscan'offset = pdata (pasInt # (pelemAt # 2 # items))
                    , pscan'frameRoot = pdata (pasByteStr # (pelemAt # 3 # items))
                    , pscan'frameClosed = pdata (pboolFromData # (pelemAt # 4 # items))
                    , pscan'result = pdata (psummaryFromData # (pelemAt # 5 # items))
                    }
              )
              $ \control ->
                pif
                  ( pcontrolIsWellFormed # control
                      #&& pencodeControlV1 # control #== controlCbor
                  )
                  control
                  perror

phashControlV1 :: forall (s :: S). Term s (PDataScanControlV1 :--> PByteString)
phashControlV1 = phoistAcyclic $ plam $ \control -> pblake2b_256 # (pencodeControlV1 # control)

pinitialControlV1 :: forall (s :: S). Term s (PByteString :--> PInteger :--> PDataScanControlV1)
pinitialControlV1 = phoistAcyclic $
  plam $ \rawHash rawLength ->
    plet
      ( pcon $
          PDataScanControlV1
            { pscan'rawHash = pdata rawHash
            , pscan'rawLength = pdata rawLength
            , pscan'offset = pdata 0
            , pscan'frameRoot = pdata (pconstant "")
            , pscan'frameClosed = pdata (pconstant False)
            , pscan'result = pdata pemptyResultV1
            }
      )
      $ \control -> pif (pcontrolIsWellFormed # control) control perror

prawMatchesControl :: forall (s :: S). Term s (PDataScanControlV1 :--> PByteString :--> PBool)
prawMatchesControl = phoistAcyclic $
  plam $ \control rawCbor ->
    pmatch control $ \c ->
      pcontrolIsWellFormed # control
        #&& plengthBS # rawCbor #== pfromData (pscan'rawLength c)
        #&& pblake2b_256 # rawCbor #== pfromData (pscan'rawHash c)

pframeIsWellFormed :: forall (s :: S). Term s (PDataScanFrameV1 :--> PBool)
pframeIsWellFormed = phoistAcyclic $
  plam $ \frame ->
    pmatch frame $ \f ->
      pand'List
        [ prootFrame #<= pfromData (pframe'kind f)
        , pfromData (pframe'kind f) #<= pmapFrame
        , pif
            (pfromData (pframe'kind f) #== pconstructorFrame)
            (0 #<= pfromData (pframe'constructor f))
            (pfromData (pframe'constructor f) #== 0)
        , pfromData (pframe'tail f) #== pconstant ""
            #|| plengthBS # pfromData (pframe'tail f) #== 32
        , 0 #<= pfromData (pframe'expectedChildren f)
        , pif
            (pfromData (pframe'kind f) #== prootFrame)
            (pfromData (pframe'expectedChildren f) #== 1)
            $ pif
              (pfromData (pframe'kind f) #== pmapFrame)
              (prem # pfromData (pframe'expectedChildren f) # 2 #== 0)
              (pconstant True)
        , 0 #<= pfromData (pframe'childCount f)
        , pfromData (pframe'childCount f) #<= pfromData (pframe'expectedChildren f)
        , Merkle.pfrontierIsWellFormed
            # pfromData (pframe'childCount f)
            # pfromData (pframe'childPeaks f)
        , 0 #<= pfromData (pframe'foldCursor f)
        , pif
            (pfromData (pframe'kind f) #== pmapFrame)
            (pfromData (pframe'foldCursor f) #<= pquot # pfromData (pframe'expectedChildren f) # 2)
            (pfromData (pframe'foldCursor f) #<= pfromData (pframe'expectedChildren f))
        , psequenceIsWellFormed f
        , pif
            (0 #< pfromData (pframe'foldCursor f))
            (pfromData (pframe'childCount f) #== pfromData (pframe'expectedChildren f))
            (pconstant True)
        ]

psequenceIsWellFormed :: forall (s :: S). PDataScanFrameV1 s -> Term s PBool
psequenceIsWellFormed f =
  pmatch (pfromData (pframe'sequence f)) $ \sequence ->
    pand'List
      [ plengthBS # pfromData (Data.pseq'root sequence) #== 32
      , pfromData (Data.pseq'length sequence) #== pfromData (pframe'foldCursor f)
      , 0 #<= pfromData (Data.pseq'payloadCborLength sequence)
      , 0 #<= pfromData (Data.pseq'memory sequence)
      ]

phashFrameV1 :: forall (s :: S). Term s (PDataScanFrameV1 :--> PByteString)
phashFrameV1 = phoistAcyclic $
  plam $ \frame ->
    pif (pnot # (pframeIsWellFormed # frame)) perror $
      pmatch frame $ \f ->
        pblake2b_256
          #$ pframeDomain
          <> (pencodeDefiniteArrayHeader # 8)
          <> pcborInt (pfromData (pframe'kind f))
          <> pcborInt (pfromData (pframe'constructor f))
          <> (pencodeDefiniteBytes # pfromData (pframe'tail f))
          <> pcborInt (pfromData (pframe'expectedChildren f))
          <> pcborInt (pfromData (pframe'childCount f))
          <> (Merkle.pencodeFrontier # pfromData (pframe'childPeaks f))
          <> pcborInt (pfromData (pframe'foldCursor f))
          <> (pencodeSequence # pfromData (pframe'sequence f))

pchildLeafHashV1 ::
  forall (s :: S). Term s (PInteger :--> Data.PDataSummaryV1 :--> PByteString)
pchildLeafHashV1 = phoistAcyclic $
  plam $ \childIndex child ->
    pmatch child $ \summary ->
      pif
        ( 0 #<= childIndex
            #&& plengthBS # pfromData (Data.psummary'root summary) #== 32
            #&& 0 #<= pfromData (Data.psummary'cborLength summary)
            #&& 0 #<= pfromData (Data.psummary'memory summary)
        )
        ( pblake2b_256
            #$ pchildDomain
            <> pcborInt childIndex
            <> (pencodeDefiniteBytes # pfromData (Data.psummary'root summary))
            <> pcborInt (pfromData (Data.psummary'cborLength summary))
            <> pcborInt (pfromData (Data.psummary'memory summary))
        )
        perror

pappendChildV1 ::
  forall (s :: S). Term s (PDataScanFrameV1 :--> Data.PDataSummaryV1 :--> PMaybe PDataScanFrameV1)
pappendChildV1 = phoistAcyclic $
  plam $ \frame child ->
    pmatch frame $ \f ->
      pif
        ( pframeIsWellFormed # frame
            #&& pfromData (pframe'foldCursor f) #== 0
            #&& pfromData (pframe'childCount f) #< pfromData (pframe'expectedChildren f)
        )
        ( pcon $
            PJust $
              pcon
                f
                  { pframe'childCount = pdata (pfromData (pframe'childCount f) + 1)
                  , pframe'childPeaks =
                      pdata $
                        Merkle.pappendLeaf
                          # pfromData (pframe'childCount f)
                          # pfromData (pframe'childPeaks f)
                          # (pchildLeafHashV1 # pfromData (pframe'childCount f) # child)
                  }
        )
        (pcon PNothing)

pfoldListChildV1 ::
  forall (s :: S).
  Term
    s
    ( PDataScanFrameV1
        :--> PInteger
        :--> Data.PDataSummaryV1
        :--> PBuiltinList (PAsData PByteString)
        :--> PMaybe PDataScanFrameV1
    )
pfoldListChildV1 = phoistAcyclic $
  plam $ \frame childIndex child siblings ->
    pmatch frame $ \f ->
      plet
        (pfromData (pframe'expectedChildren f) - pfromData (pframe'foldCursor f) - 1)
        $ \expectedIndex ->
          pif
            ( pframeIsWellFormed # frame
                #&& ( pfromData (pframe'kind f) #== pconstructorFrame
                        #|| pfromData (pframe'kind f) #== plistFrame
                    )
                #&& pfromData (pframe'childCount f) #== pfromData (pframe'expectedChildren f)
                #&& pfromData (pframe'foldCursor f) #< pfromData (pframe'expectedChildren f)
                #&& childIndex #== expectedIndex
                #&& Merkle.pverifyMembership
                  # pfromData (pframe'childCount f)
                  # pfromData (pframe'childPeaks f)
                  # childIndex
                  # (pchildLeafHashV1 # childIndex # child)
                  # siblings
            )
            ( pcon $
                PJust $
                  pcon
                    f
                      { pframe'foldCursor = pdata (pfromData (pframe'foldCursor f) + 1)
                      , pframe'sequence =
                          pdata $
                            Data.pprependDataListSummaryV1
                              # child
                              # pfromData (pframe'sequence f)
                      }
            )
            (pcon PNothing)

pfoldMapPairV1 ::
  forall (s :: S).
  Term
    s
    ( PDataScanFrameV1
        :--> PInteger
        :--> Data.PDataSummaryV1
        :--> Data.PDataSummaryV1
        :--> PBuiltinList (PAsData PByteString)
        :--> PBuiltinList (PAsData PByteString)
        :--> PMaybe PDataScanFrameV1
    )
pfoldMapPairV1 = phoistAcyclic $
  plam $ \frame pairIndex key value keySiblings valueSiblings ->
    pmatch frame $ \f ->
      plet (pquot # pfromData (pframe'expectedChildren f) # 2) $ \pairCount ->
        plet (pairCount - pfromData (pframe'foldCursor f) - 1) $ \expectedPairIndex ->
          plet (pairIndex * 2) $ \keyIndex ->
            plet (keyIndex + 1) $ \valueIndex ->
              pif
                ( pframeIsWellFormed # frame
                    #&& pfromData (pframe'kind f) #== pmapFrame
                    #&& pfromData (pframe'childCount f) #== pfromData (pframe'expectedChildren f)
                    #&& pfromData (pframe'foldCursor f) #< pairCount
                    #&& pairIndex #== expectedPairIndex
                    #&& Merkle.pverifyMembership
                      # pfromData (pframe'childCount f)
                      # pfromData (pframe'childPeaks f)
                      # keyIndex
                      # (pchildLeafHashV1 # keyIndex # key)
                      # keySiblings
                    #&& Merkle.pverifyMembership
                      # pfromData (pframe'childCount f)
                      # pfromData (pframe'childPeaks f)
                      # valueIndex
                      # (pchildLeafHashV1 # valueIndex # value)
                      # valueSiblings
                )
                ( pcon $
                    PJust $
                      pcon
                        f
                          { pframe'foldCursor = pdata (pfromData (pframe'foldCursor f) + 1)
                          , pframe'sequence =
                              pdata $
                                Data.pprependDataPairSummaryV1
                                  # key
                                  # value
                                  # pfromData (pframe'sequence f)
                          }
                )
                (pcon PNothing)

pfinalizedSummaryV1 ::
  forall (s :: S).
  Term s (PDataScanFrameV1 :--> PMaybe Data.PDataSummaryV1 :--> PMaybe Data.PDataSummaryV1)
pfinalizedSummaryV1 = phoistAcyclic $
  plam $ \frame rootChild ->
    pif (pnot # (pframeIsWellFormed # frame)) (pcon PNothing) $
      pmatch frame $ \f ->
        pif
          (pfromData (pframe'kind f) #== prootFrame)
          ( pmatch rootChild $ \case
              PNothing -> perror
              PJust child ->
                pif
                  ( pfromData (pframe'childCount f) #== 1
                      #&& pfromData (pframe'foldCursor f) #== 0
                      #&& Merkle.pverifyMembership
                        # 1
                        # pfromData (pframe'childPeaks f)
                        # 0
                        # (pchildLeafHashV1 # 0 # child)
                        # pcon PNil
                  )
                  (pcon (PJust child))
                  (pcon PNothing)
          )
          $ pif
            ( pfromData (pframe'kind f) #== pconstructorFrame
                #&& pfromData (pframe'foldCursor f) #== pfromData (pframe'expectedChildren f)
                #&& pisNothing rootChild
            )
            ( pcon $
                PJust $
                  pif
                    (pfromData (pframe'constructor f) #<= 127)
                    ( Data.psmallConstrDataSummaryV1
                        # pfromData (pframe'constructor f)
                        # pfromData (pframe'sequence f)
                    )
                    ( Data.plargeConstrDataSummaryV1
                        # pfromData (pframe'constructor f)
                        # ( pboundedBlobRootV1
                              #$ pserialiseData
                              #$ pforgetData (pframe'constructor f)
                          )
                        # pfromData (pframe'sequence f)
                    )
            )
            $ pif
              ( pfromData (pframe'kind f) #== plistFrame
                  #&& pfromData (pframe'foldCursor f) #== pfromData (pframe'expectedChildren f)
                  #&& pisNothing rootChild
              )
              (pcon (PJust (Data.plistDataSummaryV1 # pfromData (pframe'sequence f))))
              $ pif
                ( pfromData (pframe'kind f) #== pmapFrame
                    #&& pfromData (pframe'foldCursor f)
                    #== pquot # pfromData (pframe'expectedChildren f) # 2
                    #&& pisNothing rootChild
                )
                (pcon (PJust (Data.pmapDataSummaryV1 # pfromData (pframe'sequence f))))
                (pcon PNothing)

pisNothing :: forall (s :: S) (a :: S -> Type). Term s (PMaybe a) -> Term s PBool
pisNothing value = pmatch value $ \case PNothing -> pconstant True; PJust _ -> pconstant False

prawPreimageMatches :: forall (s :: S). Term s (PByteString :--> PByteString :--> PBool)
prawPreimageMatches = phoistAcyclic $
  plam $ \rawCbor expectedHash ->
    plengthBS # expectedHash #== 32 #&& pblake2b_256 # rawCbor #== expectedHash

pboundedBlobRootV1 :: forall (s :: S). Term s (PByteString :--> PByteString)
pboundedBlobRootV1 = Proof.pboundedBlobRootV1

pdataIsSmallLeaf :: forall (s :: S). Term s (PData :--> PBool)
pdataIsSmallLeaf = phoistAcyclic $
  plam $ \d -> pchooseData # d # pconstant False # pconstant False # pconstant False # pconstant True # pconstant True

psmallLeafAtV1 ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PInteger :--> PByteString :--> PMaybe PRawDataLeafV1)
psmallLeafAtV1 = phoistAcyclic $
  plam $ \rawCbor expectedHash offset itemCbor ->
    plet (plengthBS # itemCbor) $ \itemLength ->
      plet (offset + itemLength) $ \nextOffset ->
        pif
          ( prawPreimageMatches # rawCbor # expectedHash
              #&& 0 #<= offset
              #&& 0 #< itemLength
              #&& itemLength #<= Proof.pmaxBlobChunkBytesV1
              #&& nextOffset #<= plengthBS # rawCbor
              #&& psliceLen # rawCbor # offset # itemLength #== itemCbor
          )
          ( pmatch (pdeserialise # itemCbor) $ \case
              PNothing -> pcon PNothing
              PJust d ->
                pif
                  ( pdataIsSmallLeaf # d
                      #&& pserialiseData # d #== itemCbor
                      #&& psmallLeafPayloadFits d
                  )
                  (pcon (PJust (pcon (PRawDataLeafV1 nextOffset (Data.psemanticDataSummaryV1 # d)))))
                  (pcon PNothing)
          )
          (pcon PNothing)

psmallLeafPayloadFits :: forall (s :: S). Term s PData -> Term s PBool
psmallLeafPayloadFits d =
  pforce $
    pchooseData
      # d
      # pdelay (pconstant True)
      # pdelay (pconstant True)
      # pdelay (pconstant True)
      # pdelay (pconstant True)
      # pdelay (plengthBS # (pasByteStr # d) #<= Proof.pmaxBlobChunkBytesV1)

prevealedLeafAtV1 ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PInteger :--> PInteger :--> PMaybe PRawDataLeafV1)
prevealedLeafAtV1 = phoistAcyclic $
  plam $ \rawCbor expectedHash offset itemLength ->
    plet (offset + itemLength) $ \nextOffset ->
      pif
        ( prawPreimageMatches # rawCbor # expectedHash
            #&& 0 #<= offset
            #&& 0 #< itemLength
            #&& itemLength #<= pmaxRevealedDataLeafBytes
            #&& nextOffset #<= plengthBS # rawCbor
        )
        ( plet (psliceLen # rawCbor # offset # itemLength) $ \itemCbor ->
            pmatch (pdeserialise # itemCbor) $ \case
              PNothing -> pcon PNothing
              PJust d ->
                pif (pnot # (pserialiseData # d #== itemCbor)) (pcon PNothing) $
                  pmatch (prevealedSummary d itemCbor) $ \case
                    PNothing -> pcon PNothing
                    PJust summary -> pcon (PJust (pcon (PRawDataLeafV1 nextOffset summary)))
        )
        (pcon PNothing)

prevealedSummary :: forall (s :: S). Term s PData -> Term s PByteString -> Term s (PMaybe Data.PDataSummaryV1)
prevealedSummary d itemCbor =
  pforce $
    pchooseData
      # d
      # pdelay (pcon PNothing)
      # pdelay (pcon PNothing)
      # pdelay (pcon PNothing)
      # pdelay (pcon (PJust (Data.pintegerDataSummaryV1 # (pasInt # d) # (pboundedBlobRootV1 # itemCbor))))
      # pdelay
        ( plet (pasByteStr # d) $ \bytes ->
            pcon (PJust (Data.pbytesDataSummaryV1 # (plengthBS # bytes) # (pboundedBlobRootV1 # bytes)))
        )

pcanonicalSmallConstructorPrefix :: forall (s :: S). Term s (PInteger :--> PByteString)
pcanonicalSmallConstructorPrefix = phoistAcyclic $
  plam $ \constructor ->
    pif (0 #<= constructor #&& constructor #<= 127) `flip` perror $
      pif
        (constructor #<= 6)
        (pconstant "\xd8" <> pintegerToByteString # pmostSignificantFirst # 1 # (121 + constructor))
        (pconstant "\xd9" <> pintegerToByteString # pmostSignificantFirst # 2 # (1280 + constructor - 7))

psequenceHeaderAt ::
  forall (s :: S). Term s (PByteString :--> PInteger :--> PInteger :--> PMaybe PInteger)
psequenceHeaderAt = phoistAcyclic $
  plam $ \rawCbor offset expectedChildren ->
    pif
      (expectedChildren #== 0)
      ( pif
          (psliceLen # rawCbor # offset # 1 #== pconstant "\x80")
          (pcon (PJust (offset + 1)))
          (pcon PNothing)
      )
      ( pif
          (psliceLen # rawCbor # offset # 1 #== pconstant "\x9f")
          (pcon (PJust (offset + 1)))
          (pcon PNothing)
      )

pnewFrame ::
  forall (s :: S).
  Term s PInteger -> Term s PInteger -> Term s PByteString -> Term s PInteger -> Term s Data.PDataSequenceSummaryV1 -> Term s PDataScanFrameV1
pnewFrame kind constructor tailRoot expectedChildren sequence =
  pcon $
    PDataScanFrameV1
      { pframe'kind = pdata kind
      , pframe'constructor = pdata constructor
      , pframe'tail = pdata tailRoot
      , pframe'expectedChildren = pdata expectedChildren
      , pframe'childCount = pdata 0
      , pframe'childPeaks = pdata Merkle.pemptyFrontier
      , pframe'foldCursor = pdata 0
      , pframe'sequence = pdata sequence
      }

popenSmallConstructorAtV1 ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PInteger :--> PInteger :--> PInteger :--> PByteString :--> PMaybe POpenedDataFrameV1)
popenSmallConstructorAtV1 = phoistAcyclic $
  plam $ \rawCbor expectedHash offset constructor expectedChildren tailRoot ->
    plet (pcanonicalSmallConstructorPrefix # constructor) $ \prefix ->
      plet (plengthBS # prefix) $ \prefixLength ->
        pif
          ( prawPreimageMatches # rawCbor # expectedHash
              #&& 0 #<= offset
              #&& 0 #<= expectedChildren
              #&& offset + prefixLength #< plengthBS # rawCbor
              #&& psliceLen # rawCbor # offset # prefixLength #== prefix
          )
          ( pmatch (psequenceHeaderAt # rawCbor # (offset + prefixLength) # expectedChildren) $ \case
              PNothing -> pcon PNothing
              PJust nextOffset ->
                pcon (PJust (pcon (POpenedDataFrameV1 nextOffset (pnewFrame pconstructorFrame constructor tailRoot expectedChildren Data.pemptyDataListSummaryV1))))
          )
          (pcon PNothing)

popenConstructorAtV1 ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PInteger :--> PInteger :--> PInteger :--> PByteString :--> PMaybe POpenedDataFrameV1)
popenConstructorAtV1 = phoistAcyclic $
  plam $ \rawCbor expectedHash offset constructor expectedChildren tailRoot ->
    pif
      (constructor #<= 127)
      (popenSmallConstructorAtV1 # rawCbor # expectedHash # offset # constructor # expectedChildren # tailRoot)
      $ plet (pserialiseData #$ pforgetData (pdata constructor))
      $ \constructorCbor ->
        plet (plengthBS # constructorCbor) $ \constructorCborLength ->
          plet (offset + 3 + constructorCborLength) $ \fieldsOffset ->
            pif
              ( 127 #< constructor
                  #&& prawPreimageMatches # rawCbor # expectedHash
                  #&& 0 #<= offset
                  #&& 0 #<= expectedChildren
                  #&& constructorCborLength #<= pmaxRevealedDataLeafBytes
                  #&& fieldsOffset #< plengthBS # rawCbor
                  #&& psliceLen # rawCbor # offset # 3 #== pconstant "\xd8\x66\x82"
                  #&& psliceLen # rawCbor # (offset + 3) # constructorCborLength #== constructorCbor
              )
              ( pmatch (psequenceHeaderAt # rawCbor # fieldsOffset # expectedChildren) $ \case
                  PNothing -> pcon PNothing
                  PJust nextOffset ->
                    pcon (PJust (pcon (POpenedDataFrameV1 nextOffset (pnewFrame pconstructorFrame constructor tailRoot expectedChildren Data.pemptyDataListSummaryV1))))
              )
              (pcon PNothing)

popenListAtV1 ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PInteger :--> PInteger :--> PByteString :--> PMaybe POpenedDataFrameV1)
popenListAtV1 = phoistAcyclic $
  plam $ \rawCbor expectedHash offset expectedChildren tailRoot ->
    pif
      ( prawPreimageMatches # rawCbor # expectedHash
          #&& 0 #<= offset
          #&& 0 #<= expectedChildren
          #&& offset #< plengthBS # rawCbor
      )
      ( pmatch (psequenceHeaderAt # rawCbor # offset # expectedChildren) $ \case
          PNothing -> pcon PNothing
          PJust nextOffset ->
            pcon (PJust (pcon (POpenedDataFrameV1 nextOffset (pnewFrame plistFrame 0 tailRoot expectedChildren Data.pemptyDataListSummaryV1))))
      )
      (pcon PNothing)

popenMapAtV1 ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PInteger :--> PByteString :--> PMaybe POpenedDataFrameV1)
popenMapAtV1 = phoistAcyclic $
  plam $ \rawCbor expectedHash offset tailRoot ->
    pif
      (prawPreimageMatches # rawCbor # expectedHash #&& 0 #<= offset #&& offset #< plengthBS # rawCbor)
      ( pmatch (pdecodeCanonicalMapHeaderAt # rawCbor # offset) $ \(PPair nextOffset pairCount) ->
          pcon (PJust (pcon (POpenedDataFrameV1 nextOffset (pnewFrame pmapFrame 0 tailRoot (pairCount * 2) Data.pemptyDataPairSummaryV1))))
      )
      (pcon PNothing)

pcloseSequenceAtV1 ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PInteger :--> PDataScanFrameV1 :--> PMaybe PInteger)
pcloseSequenceAtV1 = phoistAcyclic $
  plam $ \rawCbor expectedHash offset frame ->
    pmatch frame $ \f ->
      pif
        ( prawPreimageMatches # rawCbor # expectedHash
            #&& pframeIsWellFormed # frame
            #&& pfromData (pframe'childCount f) #== pfromData (pframe'expectedChildren f)
            #&& ( pfromData (pframe'kind f) #== pconstructorFrame
                    #|| pfromData (pframe'kind f) #== plistFrame
                )
        )
        ( pif
            (pfromData (pframe'expectedChildren f) #== 0)
            (pcon (PJust offset))
            $ pif
              (offset #< plengthBS # rawCbor #&& psliceLen # rawCbor # offset # 1 #== pconstant "\xff")
              (pcon (PJust (offset + 1)))
              (pcon PNothing)
        )
        (pcon PNothing)

pappendCompletedChildV1 ::
  forall (s :: S).
  Term s (PDataScanFrameV1 :--> Data.PDataSummaryV1 :--> PDataScanFrameV1 :--> PMaybe PDataScanFrameV1)
pappendCompletedChildV1 = phoistAcyclic $
  plam $ \child childSummary parent ->
    pmatch child $ \c ->
      pif
        ( pfromData (pframe'tail c) #== phashFrameV1 # parent
            #&& pmatch (pfinalizedSummaryV1 # child # pcon PNothing) (\case PNothing -> pconstant False; PJust exact -> exact #== childSummary)
        )
        (pappendChildV1 # parent # childSummary)
        (pcon PNothing)

popenParentTailV1 ::
  forall (s :: S). Term s (PDataScanControlV1 :--> PMaybe PDataScanFrameV1 :--> PMaybe PByteString)
popenParentTailV1 = phoistAcyclic $
  plam $ \control parent ->
    pmatch control $ \c ->
      pif
        ( pcontrolIsWellFormed # control
            #&& psummaryRoot (pfromData (pscan'result c)) #== pconstant ""
            #&& pnot # pfromData (pscan'frameClosed c)
        )
        ( pmatch parent $ \case
            PNothing ->
              pif (pfromData (pscan'frameRoot c) #== pconstant "") (pcon (PJust (pconstant ""))) (pcon PNothing)
            PJust frame ->
              pmatch frame $ \f ->
                pif
                  ( pframeIsWellFormed # frame
                      #&& pfromData (pscan'frameRoot c) #== phashFrameV1 # frame
                      #&& pfromData (pframe'foldCursor f) #== 0
                      #&& pfromData (pframe'childCount f) #< pfromData (pframe'expectedChildren f)
                  )
                  (pcon (PJust (pfromData (pscan'frameRoot c))))
                  (pcon PNothing)
        )
        (pcon PNothing)

psummaryRoot :: forall (s :: S). Term s Data.PDataSummaryV1 -> Term s PByteString
psummaryRoot summary = pmatch summary $ \s -> pfromData (Data.psummary'root s)

pcontrolWithOpenedFrameV1 ::
  forall (s :: S). Term s (PDataScanControlV1 :--> POpenedDataFrameV1 :--> PMaybe PDataScanControlV1)
pcontrolWithOpenedFrameV1 = phoistAcyclic $
  plam $ \control opened ->
    pmatch control $ \c ->
      pmatch opened $ \(POpenedDataFrameV1 nextOffset frame) ->
        pmatch frame $ \f ->
          pcheckedControl $
            pcon
              c
                { pscan'offset = pdata nextOffset
                , pscan'frameRoot = pdata (phashFrameV1 # frame)
                , pscan'frameClosed = pdata (pfromData (pframe'expectedChildren f) #== 0)
                }

pcheckedControl :: forall (s :: S). Term s PDataScanControlV1 -> Term s (PMaybe PDataScanControlV1)
pcheckedControl next = pif (pcontrolIsWellFormed # next) (pcon (PJust next)) (pcon PNothing)

popenConstructorControlStepV1 ::
  forall (s :: S).
  Term s (PDataScanControlV1 :--> PByteString :--> PMaybe PDataScanFrameV1 :--> PInteger :--> PInteger :--> PMaybe PDataScanControlV1)
popenConstructorControlStepV1 = phoistAcyclic $
  plam $ \control rawCbor parent constructor expectedChildren ->
    pif (prawMatchesControl # control # rawCbor)
      ( pmatch (popenParentTailV1 # control # parent) $ \case
          PNothing -> pcon PNothing
          PJust tailRoot ->
            pmatch control $ \c ->
              pmatch (popenConstructorAtV1 # rawCbor # pfromData (pscan'rawHash c) # pfromData (pscan'offset c) # constructor # expectedChildren # tailRoot) $ \case
                PNothing -> pcon PNothing
                PJust opened -> pcontrolWithOpenedFrameV1 # control # opened
      )
      (pcon PNothing)

popenListControlStepV1 ::
  forall (s :: S).
  Term s (PDataScanControlV1 :--> PByteString :--> PMaybe PDataScanFrameV1 :--> PInteger :--> PMaybe PDataScanControlV1)
popenListControlStepV1 = phoistAcyclic $
  plam $ \control rawCbor parent expectedChildren ->
    pif (prawMatchesControl # control # rawCbor)
      ( pmatch (popenParentTailV1 # control # parent) $ \case
          PNothing -> pcon PNothing
          PJust tailRoot ->
            pmatch control $ \c ->
              pmatch (popenListAtV1 # rawCbor # pfromData (pscan'rawHash c) # pfromData (pscan'offset c) # expectedChildren # tailRoot) $ \case
                PNothing -> pcon PNothing
                PJust opened -> pcontrolWithOpenedFrameV1 # control # opened
      )
      (pcon PNothing)

popenMapControlStepV1 ::
  forall (s :: S).
  Term s (PDataScanControlV1 :--> PByteString :--> PMaybe PDataScanFrameV1 :--> PMaybe PDataScanControlV1)
popenMapControlStepV1 = phoistAcyclic $
  plam $ \control rawCbor parent ->
    pif (prawMatchesControl # control # rawCbor)
      ( pmatch (popenParentTailV1 # control # parent) $ \case
          PNothing -> pcon PNothing
          PJust tailRoot ->
            pmatch control $ \c ->
              pmatch (popenMapAtV1 # rawCbor # pfromData (pscan'rawHash c) # pfromData (pscan'offset c) # tailRoot) $ \case
                PNothing -> pcon PNothing
                PJust opened -> pcontrolWithOpenedFrameV1 # control # opened
      )
      (pcon PNothing)

prevealLeafControlStepV1 ::
  forall (s :: S).
  Term s (PDataScanControlV1 :--> PByteString :--> PMaybe PDataScanFrameV1 :--> PInteger :--> PMaybe PDataScanControlV1)
prevealLeafControlStepV1 = phoistAcyclic $
  plam $ \control rawCbor parent itemLength ->
    pmatch control $ \c ->
      pif
        ( prawMatchesControl # control # rawCbor
            #&& psummaryRoot (pfromData (pscan'result c)) #== pconstant ""
            #&& pnot # pfromData (pscan'frameClosed c)
        )
        ( pmatch (prevealedLeafAtV1 # rawCbor # pfromData (pscan'rawHash c) # pfromData (pscan'offset c) # itemLength) $ \case
            PNothing -> pcon PNothing
            PJust leaf ->
              pmatch leaf $ \(PRawDataLeafV1 nextOffset summary) ->
                pmatch parent $ \case
                  PNothing ->
                    pif
                      ( pfromData (pscan'frameRoot c) #== pconstant ""
                          #&& nextOffset #== pfromData (pscan'rawLength c)
                      )
                      (pcheckedControl (pcon c {pscan'offset = pdata nextOffset, pscan'result = pdata summary}))
                      (pcon PNothing)
                  PJust frame ->
                    pif
                      (pframeIsWellFormed # frame #&& pfromData (pscan'frameRoot c) #== phashFrameV1 # frame)
                      ( pmatch (pappendChildV1 # frame # summary) $ \case
                          PNothing -> pcon PNothing
                          PJust nextFrame ->
                            pmatch nextFrame $ \nf ->
                              pcheckedControl $
                                pcon
                                  c
                                    { pscan'offset = pdata nextOffset
                                    , pscan'frameRoot = pdata (phashFrameV1 # nextFrame)
                                    , pscan'frameClosed =
                                        pdata $
                                          pfromData (pframe'kind nf) #== pmapFrame
                                            #&& pfromData (pframe'childCount nf) #== pfromData (pframe'expectedChildren nf)
                                    }
                      )
                      (pcon PNothing)
        )
        (pcon PNothing)

pcloseSequenceControlStepV1 ::
  forall (s :: S).
  Term s (PDataScanControlV1 :--> PByteString :--> PDataScanFrameV1 :--> PMaybe PDataScanControlV1)
pcloseSequenceControlStepV1 = phoistAcyclic $
  plam $ \control rawCbor frame ->
    pmatch control $ \c ->
      pmatch frame $ \f ->
        pif
          ( prawMatchesControl # control # rawCbor
              #&& psummaryRoot (pfromData (pscan'result c)) #== pconstant ""
              #&& pnot # pfromData (pscan'frameClosed c)
              #&& pframeIsWellFormed # frame
              #&& pfromData (pscan'frameRoot c) #== phashFrameV1 # frame
              #&& pfromData (pframe'foldCursor f) #== 0
          )
          ( pmatch (pcloseSequenceAtV1 # rawCbor # pfromData (pscan'rawHash c) # pfromData (pscan'offset c) # frame) $ \case
              PNothing -> pcon PNothing
              PJust nextOffset -> pcheckedControl (pcon c {pscan'offset = pdata nextOffset, pscan'frameClosed = pdata (pconstant True)})
          )
          (pcon PNothing)

pfoldListControlStepV1 ::
  forall (s :: S).
  Term s (PDataScanControlV1 :--> PDataScanFrameV1 :--> PInteger :--> Data.PDataSummaryV1 :--> PBuiltinList (PAsData PByteString) :--> PMaybe PDataScanControlV1)
pfoldListControlStepV1 = phoistAcyclic $
  plam $ \control frame childIndex child siblings ->
    pif (pcontrolCanFold control frame)
      ( pmatch (pfoldListChildV1 # frame # childIndex # child # siblings) $ \case
          PNothing -> pcon PNothing
          PJust nextFrame -> pmatch control $ \c -> pcon (PJust (pcon c {pscan'frameRoot = pdata (phashFrameV1 # nextFrame)}))
      )
      (pcon PNothing)

pcontrolCanFold :: forall (s :: S). Term s PDataScanControlV1 -> Term s PDataScanFrameV1 -> Term s PBool
pcontrolCanFold control frame =
  pmatch control $ \c ->
    pcontrolIsWellFormed # control
      #&& psummaryRoot (pfromData (pscan'result c)) #== pconstant ""
      #&& pfromData (pscan'frameClosed c)
      #&& pframeIsWellFormed # frame
      #&& pfromData (pscan'frameRoot c) #== phashFrameV1 # frame

pfoldMapControlStepV1 ::
  forall (s :: S).
  Term s (PDataScanControlV1 :--> PDataScanFrameV1 :--> PInteger :--> Data.PDataSummaryV1 :--> Data.PDataSummaryV1 :--> PBuiltinList (PAsData PByteString) :--> PBuiltinList (PAsData PByteString) :--> PMaybe PDataScanControlV1)
pfoldMapControlStepV1 = phoistAcyclic $
  plam $ \control frame pairIndex key value keySiblings valueSiblings ->
    pif (pcontrolCanFold control frame)
      ( pmatch (pfoldMapPairV1 # frame # pairIndex # key # value # keySiblings # valueSiblings) $ \case
          PNothing -> pcon PNothing
          PJust nextFrame -> pmatch control $ \c -> pcon (PJust (pcon c {pscan'frameRoot = pdata (phashFrameV1 # nextFrame)}))
      )
      (pcon PNothing)

pfinalizeFrameControlStepV1 ::
  forall (s :: S).
  Term s (PDataScanControlV1 :--> PDataScanFrameV1 :--> PMaybe PDataScanFrameV1 :--> PMaybe PDataScanControlV1)
pfinalizeFrameControlStepV1 = phoistAcyclic $
  plam $ \control frame parent ->
    pmatch control $ \c ->
      pif
        (pcontrolCanFold control frame)
        ( pmatch (pfinalizedSummaryV1 # frame # pcon PNothing) $ \case
            PNothing -> pcon PNothing
            PJust summary ->
              pmatch parent $ \case
                PNothing ->
                  pmatch frame $ \f ->
                    pif
                      ( pfromData (pframe'tail f) #== pconstant ""
                          #&& pfromData (pscan'offset c) #== pfromData (pscan'rawLength c)
                      )
                      ( pcheckedControl $
                          pcon
                            c
                              { pscan'frameRoot = pdata (pconstant "")
                              , pscan'frameClosed = pdata (pconstant False)
                              , pscan'result = pdata summary
                              }
                      )
                      (pcon PNothing)
                PJust parentFrame ->
                  pmatch (pappendCompletedChildV1 # frame # summary # parentFrame) $ \case
                    PNothing -> pcon PNothing
                    PJust nextParent ->
                      pmatch nextParent $ \np ->
                        pcheckedControl $
                          pcon
                            c
                              { pscan'frameRoot = pdata (phashFrameV1 # nextParent)
                              , pscan'frameClosed =
                                  pdata $
                                    pfromData (pframe'kind np) #== pmapFrame
                                      #&& pfromData (pframe'childCount np) #== pfromData (pframe'expectedChildren np)
                              }
        )
        (pcon PNothing)
