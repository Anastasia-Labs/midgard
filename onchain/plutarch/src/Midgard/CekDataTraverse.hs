{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.CekDataTraverse
Description : Plutarch port of @lib/midgard/cek-data-traverse-v1.ak@.
-}
module Midgard.CekDataTraverse (
  PDataTraverseControlV1 (..),
  PDataTraverseActionV1 (..),
  PFinalizeFrameTransitionV1 (..),
  pversion,
  pheadBytes,
  pmaxSourceSpan,
  pstageHead,
  pstageInteger,
  pstageBytes,
  pstageLargeConstructor,
  pstageLargeFields,
  pstageClose,
  pstageFold,
  pstageTerminal,
  pcontrolIsWellFormed,
  pinitialControlV1,
  pencodeOptionalSummaryV1,
  pencodeControlV1,
  pcontrolFromDataV1,
  pdecodeControlV1,
  phashControlV1,
  pnextSourceSpanV1,
  pprevalidatedFinalizeFrameTransitionV1,
  pprevalidatedFoldMapNextFrameRootV1,
  pstepHead,
  pstepFold,
  pstepV1,
  pfinalizeV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.ByteString (pbyteStringToInteger, pmostSignificantFirst)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.CekData (PDataSummaryV1 (..))
import Midgard.CekDataBytes qualified as Bytes
import Midgard.CekDataFrame qualified as Frame
import Midgard.CekDataInteger qualified as Integer
import Midgard.CekSourceBlob qualified as Blob
import Midgard.FraudProofs.NativeTx.Codec (
  pbyteAt,
  pcborInt,
  pencodeDefiniteArrayHeader,
  pencodeDefiniteBytes,
  psliceLen,
 )

pversion, pheadBytes, pmaxSourceSpan :: forall (s :: S). Term s PInteger
pversion = 1
pheadBytes = 14
pmaxSourceSpan = 132

pstageHead, pstageInteger, pstageBytes, pstageLargeConstructor, pstageLargeFields, pstageClose, pstageFold, pstageTerminal ::
  forall (s :: S). Term s PInteger
pstageHead = 0
pstageInteger = 1
pstageBytes = 2
pstageLargeConstructor = 3
pstageLargeFields = 4
pstageClose = 5
pstageFold = 6
pstageTerminal = 7

puint32Max, puint64Max :: forall (s :: S). Term s PInteger
puint32Max = 4_294_967_295
puint64Max = 18_446_744_073_709_551_615

pcontrolDomain :: forall (s :: S). Term s PByteString
pcontrolDomain = pconstant "MidgardCekDataTraverseControlV1"

data PDataTraverseControlV1 (s :: S) = PDataTraverseControlV1
  { ptraverse'version :: Term s (PAsData PInteger)
  , ptraverse'stage :: Term s (PAsData PInteger)
  , ptraverse'sourceStart :: Term s (PAsData PInteger)
  , ptraverse'sourceLength :: Term s (PAsData PInteger)
  , ptraverse'offset :: Term s (PAsData PInteger)
  , ptraverse'frameRoot :: Term s (PAsData PByteString)
  , ptraverse'pendingLargeExpectedChildren :: Term s (PAsData (PMaybeData PInteger))
  , ptraverse'integer :: Term s (PAsData (PMaybeData Integer.PCekDataIntegerControlV1))
  , ptraverse'bytes :: Term s (PAsData (PMaybeData Bytes.PCekDataBytesControlV1))
  , ptraverse'result :: Term s (PAsData (PMaybeData PDataSummaryV1))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDataTraverseControlV1)

data PDataTraverseActionV1 (s :: S)
  = PNoAction
  | PHeadScalar (Term s (PAsData PInteger))
  | PHeadSequence (Term s (PAsData PInteger))
  | PHeadMap
  | PHeadLargeConstructor (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  | PAttachScalar (Term s (PAsData (PMaybeData Frame.PDataFrameV1)))
  | PFoldList
      (Term s (PAsData Frame.PDataFrameV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PDataSummaryV1))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PFoldMap
      (Term s (PAsData Frame.PDataFrameV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PDataSummaryV1))
      (Term s (PAsData PDataSummaryV1))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PFinalizeFrame
      (Term s (PAsData Frame.PDataFrameV1))
      (Term s (PAsData (PMaybeData Frame.PDataFrameV1)))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDataTraverseActionV1)

data PFinalizeFrameTransitionV1 (s :: S) = PFinalizeFrameTransitionV1
  { ptransition'nextStage :: Term s PInteger
  , ptransition'nextFrameRoot :: Term s PByteString
  , ptransition'nextResult :: Term s (PMaybeData PDataSummaryV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsSOPStruct PFinalizeFrameTransitionV1)

data PCborArgumentV1 (s :: S) = PCborArgumentV1
  (Term s PInteger)
  (Term s PInteger)
  (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsSOPStruct PCborArgumentV1)

data PSmallConstructorHeadV1 (s :: S) = PSmallConstructorHeadV1
  (Term s PInteger)
  (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsSOPStruct PSmallConstructorHeadV1)

pminimum :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pminimum = phoistAcyclic $ plam $ \a b -> pif (a #< b) a b

puint32IsWellFormed, puint64IsWellFormed :: forall (s :: S). Term s (PInteger :--> PBool)
puint32IsWellFormed = phoistAcyclic $ plam $ \v -> 0 #<= v #&& v #<= puint32Max
puint64IsWellFormed = phoistAcyclic $ plam $ \v -> 0 #<= v #&& v #<= puint64Max

poptionalHashIsWellFormed :: forall (s :: S). Term s (PByteString :--> PBool)
poptionalHashIsWellFormed = phoistAcyclic $ plam $ \v -> plengthBS # v #== 0 #|| plengthBS # v #== 32

psummaryIsWellFormed :: forall (s :: S). Term s (PDataSummaryV1 :--> PBool)
psummaryIsWellFormed = phoistAcyclic $
  plam $ \summary -> pmatch summary $ \s ->
    pand'List
      [ plengthBS # pfromData (psummary'root s) #== 32
      , 0 #< pfromData (psummary'cborLength s)
      , puint64IsWellFormed # pfromData (psummary'cborLength s)
      , 4 #<= pfromData (psummary'memory s)
      , puint64IsWellFormed # pfromData (psummary'memory s)
      ]

poptionalSummaryIsWellFormed :: forall (s :: S). Term s (PMaybeData PDataSummaryV1 :--> PBool)
poptionalSummaryIsWellFormed = phoistAcyclic $ plam $ \value -> pmatch value $ \case
  PDNothing -> pconstant True
  PDJust exact -> psummaryIsWellFormed # pfromData exact

poptionalUint32IsWellFormed :: forall (s :: S). Term s (PMaybeData PInteger :--> PBool)
poptionalUint32IsWellFormed = phoistAcyclic $ plam $ \value -> pmatch value $ \case
  PDNothing -> pconstant True
  PDJust exact -> puint32IsWellFormed # pfromData exact

pnestedIntegerFits ::
  forall (s :: S).
  Term s PDataTraverseControlV1 -> Term s Integer.PCekDataIntegerControlV1 -> Term s PBool -> Term s PBool
pnestedIntegerFits control integer startsAtCursor =
  pmatch control $ \c -> pmatch integer $ \i ->
    plet (pfromData (ptraverse'sourceStart c) + pfromData (ptraverse'offset c)) $ \cursor ->
      pand'List
        [ Integer.pcontrolIsWellFormed # integer
        , pif startsAtCursor (pfromData (Integer.pint'sourceStart i) #== cursor)
            (pfromData (Integer.pint'sourceStart i) + pfromData (Integer.pint'sourceLength i) #== cursor)
        , pfromData (ptraverse'sourceStart c) #<= pfromData (Integer.pint'sourceStart i)
        , pfromData (Integer.pint'sourceStart i) + pfromData (Integer.pint'sourceLength i)
            #<= pfromData (ptraverse'sourceStart c) + pfromData (ptraverse'sourceLength c)
        ]

pstageIsWellFormed :: forall (s :: S). Term s PDataTraverseControlV1 -> Term s PBool
pstageIsWellFormed control = pmatch control $ \c ->
  plet (pfromData (ptraverse'stage c)) $ \stage ->
  plet (pfromData (ptraverse'offset c)) $ \offset ->
  plet (pfromData (ptraverse'sourceLength c)) $ \sourceLength ->
  plet (pfromData (ptraverse'frameRoot c)) $ \frameRoot ->
  plet (pfromData (ptraverse'pendingLargeExpectedChildren c)) $ \pending ->
  plet (pfromData (ptraverse'integer c)) $ \integer ->
  plet (pfromData (ptraverse'bytes c)) $ \bytes ->
  plet (pfromData (ptraverse'result c)) $ \result ->
    pif (stage #== pstageHead)
      ( pand'List
          [ offset #< sourceLength
          , plengthBS # frameRoot #== 32 #|| offset #== 0
          , pending #== pcon PDNothing
          , integer #== pcon PDNothing
          , bytes #== pcon PDNothing
          , result #== pcon PDNothing
          ]
      ) $
    pif (stage #== pstageInteger)
      (pmatch integer $ \case
        PDNothing -> pconstant False
        PDJust exact -> pand'List
          [ pending #== pcon PDNothing, bytes #== pcon PDNothing, result #== pcon PDNothing
          , pnestedIntegerFits control (pfromData exact) (pconstant True)
          ]) $
    pif (stage #== pstageBytes)
      (pmatch bytes $ \case
        PDNothing -> pconstant False
        PDJust exact -> pmatch (pfromData exact) $ \b -> pand'List
          [ pending #== pcon PDNothing, integer #== pcon PDNothing, result #== pcon PDNothing
          , Bytes.pcontrolIsWellFormed # pfromData exact
          , pfromData (Bytes.pbytes'sourceStart b) #== pfromData (ptraverse'sourceStart c) + offset
          , offset + pfromData (Bytes.pbytes'sourceLength b) #<= sourceLength
          ]) $
    pif (stage #== pstageLargeConstructor)
      (pmatch integer $ \case
        PDNothing -> pconstant False
        PDJust exact -> pmatch exact $ \_ -> pand'List
          [ pnot # (pending #== pcon PDNothing), bytes #== pcon PDNothing, result #== pcon PDNothing
          , pnestedIntegerFits control (pfromData exact) (pconstant True)
          , pmatch (pfromData exact) $ \i -> offset + pfromData (Integer.pint'sourceLength i) #< sourceLength
          ]) $
    pif (stage #== pstageLargeFields)
      (pmatch integer $ \case
        PDNothing -> pconstant False
        PDJust exact -> pmatch (pfromData exact) $ \i -> pand'List
          [ pnot # (pending #== pcon PDNothing)
          , pfromData (Integer.pint'stage i) #== Integer.pstageTerminal
          , bytes #== pcon PDNothing, result #== pcon PDNothing
          , pnestedIntegerFits control (pfromData exact) (pconstant False)
          , offset #< sourceLength
          ]) $
    pif (stage #== pstageClose #|| stage #== pstageFold)
      ( pand'List
          [ plengthBS # frameRoot #== 32
          , pending #== pcon PDNothing, integer #== pcon PDNothing
          , bytes #== pcon PDNothing, result #== pcon PDNothing
          , pif (stage #== pstageClose) (offset #< sourceLength) (pconstant True)
          ]
      )
      ( pand'List
          [ offset #== sourceLength, plengthBS # frameRoot #== 0
          , pending #== pcon PDNothing, integer #== pcon PDNothing
          , bytes #== pcon PDNothing, pnot # (result #== pcon PDNothing)
          ]
      )

pcontrolIsWellFormed :: forall (s :: S). Term s (PDataTraverseControlV1 :--> PBool)
pcontrolIsWellFormed = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pand'List
    [ pfromData (ptraverse'version c) #== pversion
    , pstageHead #<= pfromData (ptraverse'stage c)
    , pfromData (ptraverse'stage c) #<= pstageTerminal
    , puint32IsWellFormed # pfromData (ptraverse'sourceStart c)
    , 0 #< pfromData (ptraverse'sourceLength c)
    , puint32IsWellFormed # pfromData (ptraverse'sourceLength c)
    , puint32IsWellFormed # pfromData (ptraverse'offset c)
    , pfromData (ptraverse'offset c) #<= pfromData (ptraverse'sourceLength c)
    , poptionalHashIsWellFormed # pfromData (ptraverse'frameRoot c)
    , poptionalUint32IsWellFormed # pfromData (ptraverse'pendingLargeExpectedChildren c)
    , poptionalSummaryIsWellFormed # pfromData (ptraverse'result c)
    , pstageIsWellFormed control
    ]

pinitialControlV1 :: forall (s :: S). Term s (PInteger :--> PInteger :--> PDataTraverseControlV1)
pinitialControlV1 = phoistAcyclic $ plam $ \sourceStart sourceLength ->
  plet (pcon PDataTraverseControlV1
    { ptraverse'version = pdata pversion
    , ptraverse'stage = pdata pstageHead
    , ptraverse'sourceStart = pdata sourceStart
    , ptraverse'sourceLength = pdata sourceLength
    , ptraverse'offset = pdata 0
    , ptraverse'frameRoot = pdata (pconstant "")
    , ptraverse'pendingLargeExpectedChildren = pdata (pcon PDNothing)
    , ptraverse'integer = pdata (pcon PDNothing)
    , ptraverse'bytes = pdata (pcon PDNothing)
    , ptraverse'result = pdata (pcon PDNothing)
    }) $ \control -> pif (pcontrolIsWellFormed # control) control perror

pencodeOptionalInt :: forall (s :: S). Term s (PMaybeData PInteger :--> PByteString)
pencodeOptionalInt = phoistAcyclic $ plam $ \value -> pmatch value $ \case
  PDNothing -> pconstant "\xd8\x7a\x80"
  PDJust exact -> pconstant "\xd8\x79\x9f" <> pcborInt (pfromData exact) <> pconstant "\xff"

pencodeOptionalInteger :: forall (s :: S). Term s (PMaybeData Integer.PCekDataIntegerControlV1 :--> PByteString)
pencodeOptionalInteger = phoistAcyclic $ plam $ \value -> pmatch value $ \case
  PDNothing -> pconstant "\xd8\x7a\x80"
  PDJust exact -> pconstant "\xd8\x79\x9f" <> (Integer.pencodeControlV1 # pfromData exact) <> pconstant "\xff"

pencodeOptionalBytes :: forall (s :: S). Term s (PMaybeData Bytes.PCekDataBytesControlV1 :--> PByteString)
pencodeOptionalBytes = phoistAcyclic $ plam $ \value -> pmatch value $ \case
  PDNothing -> pconstant "\xd8\x7a\x80"
  PDJust exact -> pconstant "\xd8\x79\x9f" <> (Bytes.pencodeControlV1 # pfromData exact) <> pconstant "\xff"

pencodeSummary :: forall (s :: S). Term s (PDataSummaryV1 :--> PByteString)
pencodeSummary = phoistAcyclic $ plam $ \summary -> pmatch summary $ \s ->
  (pencodeDefiniteArrayHeader # 3)
    <> (pencodeDefiniteBytes # pfromData (psummary'root s))
    <> pcborInt (pfromData (psummary'cborLength s))
    <> pcborInt (pfromData (psummary'memory s))

pencodeOptionalSummaryV1 :: forall (s :: S). Term s (PMaybeData PDataSummaryV1 :--> PByteString)
pencodeOptionalSummaryV1 = phoistAcyclic $ plam $ \value ->
  pif (pnot # (poptionalSummaryIsWellFormed # value)) perror $ pmatch value $ \case
    PDNothing -> pconstant "\xd8\x7a\x80"
    PDJust exact -> pconstant "\xd8\x79\x9f" <> (pencodeSummary # pfromData exact) <> pconstant "\xff"

pencodeControlV1 :: forall (s :: S). Term s (PDataTraverseControlV1 :--> PByteString)
pencodeControlV1 = phoistAcyclic $ plam $ \control ->
  pif (pnot # (pcontrolIsWellFormed # control)) perror $ pmatch control $ \c ->
    (pencodeDefiniteArrayHeader # 10)
      <> pcborInt (pfromData (ptraverse'version c))
      <> pcborInt (pfromData (ptraverse'stage c))
      <> pcborInt (pfromData (ptraverse'sourceStart c))
      <> pcborInt (pfromData (ptraverse'sourceLength c))
      <> pcborInt (pfromData (ptraverse'offset c))
      <> (pencodeDefiniteBytes # pfromData (ptraverse'frameRoot c))
      <> (pencodeOptionalInt # pfromData (ptraverse'pendingLargeExpectedChildren c))
      <> (pencodeOptionalInteger # pfromData (ptraverse'integer c))
      <> (pencodeOptionalBytes # pfromData (ptraverse'bytes c))
      <> (pencodeOptionalSummaryV1 # pfromData (ptraverse'result c))

poptionalIntFromData :: forall (s :: S). Term s (PData :--> PMaybeData PInteger)
poptionalIntFromData = phoistAcyclic $ plam $ \d -> pmatch (pasConstr # d) $ \(PBuiltinPair index fields) ->
  pif (index #== 0)
    (pif (plength # fields #== 1) (pcon (PDJust (pdata (pasInt # (pelemAt # 0 # fields))))) perror)
    (pif (index #== 1 #&& pnull # fields) (pcon PDNothing) perror)

poptionalIntegerFromData :: forall (s :: S). Term s (PData :--> PMaybeData Integer.PCekDataIntegerControlV1)
poptionalIntegerFromData = phoistAcyclic $ plam $ \d -> pmatch (pasConstr # d) $ \(PBuiltinPair index fields) ->
  pif (index #== 0)
    (pif (plength # fields #== 1) (pcon (PDJust (pdata (Integer.pcontrolFromDataV1 # (pelemAt # 0 # fields))))) perror)
    (pif (index #== 1 #&& pnull # fields) (pcon PDNothing) perror)

poptionalBytesFromData :: forall (s :: S). Term s (PData :--> PMaybeData Bytes.PCekDataBytesControlV1)
poptionalBytesFromData = phoistAcyclic $ plam $ \d -> pmatch (pasConstr # d) $ \(PBuiltinPair index fields) ->
  pif (index #== 0)
    (pif (plength # fields #== 1) (pcon (PDJust (pdata (Bytes.pcontrolFromDataV1 # (pelemAt # 0 # fields))))) perror)
    (pif (index #== 1 #&& pnull # fields) (pcon PDNothing) perror)

poptionalSummaryFromData :: forall (s :: S). Term s (PData :--> PMaybeData PDataSummaryV1)
poptionalSummaryFromData = phoistAcyclic $ plam $ \d -> pmatch (pasConstr # d) $ \(PBuiltinPair index fields) ->
  pif (index #== 0)
    ( pif (plength # fields #== 1)
        ( plet (pasList # (pelemAt # 0 # fields)) $ \items ->
            pif (plength # items #== 3)
              ( pcon $ PDJust $ pdata $ pcon $ PDataSummaryV1
                  { psummary'root = pdata (pasByteStr # (pelemAt # 0 # items))
                  , psummary'cborLength = pdata (pasInt # (pelemAt # 1 # items))
                  , psummary'memory = pdata (pasInt # (pelemAt # 2 # items))
                  }
              )
              perror
        )
        perror
    )
    (pif (index #== 1 #&& pnull # fields) (pcon PDNothing) perror)

pcontrolFromDataV1 :: forall (s :: S). Term s (PData :--> PDataTraverseControlV1)
pcontrolFromDataV1 = phoistAcyclic $ plam $ \d -> plet (pasList # d) $ \items ->
  pif (pnot # (plength # items #== 10)) perror $
    plet (pcon PDataTraverseControlV1
      { ptraverse'version = pdata (pasInt # (pelemAt # 0 # items))
      , ptraverse'stage = pdata (pasInt # (pelemAt # 1 # items))
      , ptraverse'sourceStart = pdata (pasInt # (pelemAt # 2 # items))
      , ptraverse'sourceLength = pdata (pasInt # (pelemAt # 3 # items))
      , ptraverse'offset = pdata (pasInt # (pelemAt # 4 # items))
      , ptraverse'frameRoot = pdata (pasByteStr # (pelemAt # 5 # items))
      , ptraverse'pendingLargeExpectedChildren = pdata (poptionalIntFromData # (pelemAt # 6 # items))
      , ptraverse'integer = pdata (poptionalIntegerFromData # (pelemAt # 7 # items))
      , ptraverse'bytes = pdata (poptionalBytesFromData # (pelemAt # 8 # items))
      , ptraverse'result = pdata (poptionalSummaryFromData # (pelemAt # 9 # items))
      }) $ \control -> pif (pcontrolIsWellFormed # control) control perror

pdecodeControlV1 :: forall (s :: S). Term s (PByteString :--> PDataTraverseControlV1)
pdecodeControlV1 = phoistAcyclic $ plam $ \cbor -> pmatch (pdeserialise # cbor) $ \case
  PNothing -> perror
  PJust d -> plet (pcontrolFromDataV1 # d) $ \control -> pif (pencodeControlV1 # control #== cbor) control perror

phashControlV1 :: forall (s :: S). Term s (PDataTraverseControlV1 :--> PByteString)
phashControlV1 = phoistAcyclic $ plam $ \control -> pblake2b_256 # (pcontrolDomain <> (pencodeControlV1 # control))

pnextSourceSpanV1 :: forall (s :: S). Term s (PDataTraverseControlV1 :--> PMaybe Blob.PCekSourceBlobSpanV1)
pnextSourceSpanV1 = phoistAcyclic $ plam $ \control ->
  pif (pnot # (pcontrolIsWellFormed # control)) (pcon PNothing) $ pmatch control $ \c ->
    plet (pfromData (ptraverse'stage c)) $ \stage ->
      pif (stage #== pstageHead)
        (pcon (PJust (pcon (Blob.PCekSourceBlobSpanV1
          (pdata (pfromData (ptraverse'sourceStart c) + pfromData (ptraverse'offset c)))
          (pdata (pminimum # pheadBytes # (pfromData (ptraverse'sourceLength c) - pfromData (ptraverse'offset c)))))))) $
      pif (stage #== pstageInteger #|| stage #== pstageLargeConstructor)
        (pmatch (pfromData (ptraverse'integer c)) $ \case
          PDNothing -> perror
          PDJust exact -> Integer.pnextSourceSpanV1 # pfromData exact) $
      pif (stage #== pstageBytes)
        (pmatch (pfromData (ptraverse'bytes c)) $ \case
          PDNothing -> perror
          PDJust exact -> Bytes.pnextSourceSpanV1 # pfromData exact) $
      pif (stage #== pstageLargeFields #|| stage #== pstageClose)
        (pcon (PJust (pcon (Blob.PCekSourceBlobSpanV1
          (pdata (pfromData (ptraverse'sourceStart c) + pfromData (ptraverse'offset c)))
          (pdata 1)))))
        (pcon PNothing)

pargumentByteCount :: forall (s :: S). Term s (PInteger :--> PInteger)
pargumentByteCount = phoistAcyclic $ plam $ \additional ->
  pif (additional #== 24) 1 $ pif (additional #== 25) 2 $ pif (additional #== 26) 4 (-1)

pargumentIsMinimal :: forall (s :: S). Term s (PInteger :--> PInteger :--> PBool)
pargumentIsMinimal = phoistAcyclic $ plam $ \additional value ->
  pif (additional #== 24) (24 #<= value) $
  pif (additional #== 25) (255 #< value) $
  pif (additional #== 26) (65_535 #< value) (pconstant False)

preadCanonicalArgument :: forall (s :: S). Term s (PByteString :--> PInteger :--> PMaybe PCborArgumentV1)
preadCanonicalArgument = phoistAcyclic $ plam $ \bytes offset -> plet (plengthBS # bytes) $ \len ->
  pif (offset #< 0 #|| len #<= offset) (pcon PNothing) $
    plet (pbyteAt # bytes # offset) $ \initial ->
    plet (pquot # initial # 32) $ \major ->
    plet (prem # initial # 32) $ \additional ->
      pif (additional #< 24)
        (pcon (PJust (pcon (PCborArgumentV1 major additional (offset + 1))))) $
      plet (pargumentByteCount # additional) $ \byteCount ->
        pif (byteCount #< 0 #|| len #< offset + 1 + byteCount) (pcon PNothing) $
          plet (pbyteStringToInteger # pmostSignificantFirst # (psliceLen # bytes # (offset + 1) # byteCount)) $ \value ->
            pif (pargumentIsMinimal # additional # value)
              (pcon (PJust (pcon (PCborArgumentV1 major value (offset + 1 + byteCount)))))
              (pcon PNothing)

pparseSmallConstructorHead :: forall (s :: S). Term s (PByteString :--> PMaybe PSmallConstructorHeadV1)
pparseSmallConstructorHead = phoistAcyclic $ plam $ \bytes -> plet (plengthBS # bytes) $ \len ->
  pif (2 #<= len #&& pbyteAt # bytes # 0 #== 216 #&& 121 #<= pbyteAt # bytes # 1 #&& pbyteAt # bytes # 1 #<= 127)
    (pcon (PJust (pcon (PSmallConstructorHeadV1 (pbyteAt # bytes # 1 - 121) 2)))) $
  pif (3 #<= len #&& pbyteAt # bytes # 0 #== 217)
    (plet (pbyteAt # bytes # 1 * 256 + pbyteAt # bytes # 2) $ \tag ->
      plet (tag - 1280 + 7) $ \constructor ->
        pif (1280 #<= tag #&& tag #<= 1400 #&& constructor #<= 127)
          (pcon (PJust (pcon (PSmallConstructorHeadV1 constructor 3))))
          (pcon PNothing))
    (pcon PNothing)

pexactSourceBytes ::
  forall (s :: S). Term s PDataTraverseControlV1 -> Term s (PMaybe PByteString) -> Term s (PMaybe PByteString)
pexactSourceBytes control sourceBytes = pmatch sourceBytes $ \case
  PNothing -> pcon PNothing
  PJust bytes -> pmatch (pnextSourceSpanV1 # control) $ \case
    PNothing -> pcon PNothing
    PJust span -> pmatch span $ \(Blob.PCekSourceBlobSpanV1 _ len) ->
      pif (plengthBS # bytes #== pfromData len) (pcon (PJust bytes)) (pcon PNothing)

pcheckedControl :: forall (s :: S). Term s PDataTraverseControlV1 -> Term s (PMaybe PDataTraverseControlV1)
pcheckedControl control = pif (pcontrolIsWellFormed # control) (pcon (PJust control)) (pcon PNothing)

pnextParentStage :: forall (s :: S). Term s Frame.PDataFrameV1 -> Term s PInteger
pnextParentStage frame = pmatch frame $ \f ->
  pif (pfromData (Frame.pframe'childCount f) #< pfromData (Frame.pframe'expectedChildren f)) pstageHead $
  pif (pfromData (Frame.pframe'kind f) #== Frame.pmapFrame) pstageFold pstageClose

pprevalidatedAttachSummaryTransitionV1 ::
  forall (s :: S).
  Term s PByteString -> Term s PDataSummaryV1 -> Term s (PMaybeData Frame.PDataFrameV1) ->
  Term s PInteger -> Term s PInteger -> Term s (PMaybe PFinalizeFrameTransitionV1)
pprevalidatedAttachSummaryTransitionV1 currentFrameRoot summary parent offset sourceLength =
  pif (pnot # (psummaryIsWellFormed # summary)) (pcon PNothing) $
  pif (plengthBS # currentFrameRoot #== 0)
    (pif (parent #== pcon PDNothing #&& offset #== sourceLength)
      (pcon (PJust (pcon (PFinalizeFrameTransitionV1 pstageTerminal (pconstant "") (pcon (PDJust (pdata summary)))))))
      (pcon PNothing)) $
  pmatch parent $ \case
    PDNothing -> pcon PNothing
    PDJust parentData -> plet (pfromData parentData) $ \parentFrame ->
      pif (Frame.pframeIsWellFormedV1 # parentFrame #&& Frame.phashFrameV1 # parentFrame #== currentFrameRoot)
        (pmatch (Frame.pappendChildV1 # parentFrame # summary) $ \case
          PNothing -> pcon PNothing
          PJust nextParent -> plet (pnextParentStage nextParent) $ \nextStage ->
            pif
              ( pif (nextStage #== pstageHead #|| nextStage #== pstageClose) (offset #< sourceLength) (pconstant True)
                  #&& (nextStage #== pstageHead #|| nextStage #== pstageClose #|| nextStage #== pstageFold)
              )
              (pcon (PJust (pcon (PFinalizeFrameTransitionV1 nextStage (Frame.phashFrameV1 # nextParent) (pcon PDNothing)))))
              (pcon PNothing))
        (pcon PNothing)

pattachSummary ::
  forall (s :: S).
  Term s PDataTraverseControlV1 -> Term s PDataSummaryV1 -> Term s (PMaybeData Frame.PDataFrameV1) -> Term s PInteger ->
  Term s (PMaybe PDataTraverseControlV1)
pattachSummary control summary parent offset = pmatch control $ \c ->
  pmatch (pprevalidatedAttachSummaryTransitionV1
    (pfromData (ptraverse'frameRoot c)) summary parent offset (pfromData (ptraverse'sourceLength c))) $ \case
      PNothing -> pcon PNothing
      PJust transition -> pmatch transition $ \(PFinalizeFrameTransitionV1 nextStage nextRoot nextResult) ->
        pcheckedControl (pcon c
          { ptraverse'stage = pdata nextStage
          , ptraverse'offset = pdata offset
          , ptraverse'frameRoot = pdata nextRoot
          , ptraverse'pendingLargeExpectedChildren = pdata (pcon PDNothing)
          , ptraverse'integer = pdata (pcon PDNothing)
          , ptraverse'bytes = pdata (pcon PDNothing)
          , ptraverse'result = pdata nextResult
          })

pstepHeadScalar :: forall (s :: S). Term s (PDataTraverseControlV1 :--> PByteString :--> PInteger :--> PMaybe PDataTraverseControlV1)
pstepHeadScalar = phoistAcyclic $ plam $ \control bytes itemLength -> pmatch control $ \c ->
  pif (itemLength #<= 0 #|| pnot # (puint32IsWellFormed # itemLength) #||
       pfromData (ptraverse'offset c) + itemLength #> pfromData (ptraverse'sourceLength c)) (pcon PNothing) $
    plet (pbyteAt # bytes # 0) $ \first ->
      pif (pquot # first # 32 #<= 1 #|| first #== 194 #|| first #== 195)
        (pcheckedControl (pcon c
          { ptraverse'stage = pdata pstageInteger
          , ptraverse'integer = pdata (pcon (PDJust (pdata (Integer.pinitialControlV1 #
              (pfromData (ptraverse'sourceStart c) + pfromData (ptraverse'offset c)) # itemLength))))
          })) $
      pif (pquot # first # 32 #== 2)
        (pcheckedControl (pcon c
          { ptraverse'stage = pdata pstageBytes
          , ptraverse'bytes = pdata (pcon (PDJust (pdata (Bytes.pinitialControlV1 #
              (pfromData (ptraverse'sourceStart c) + pfromData (ptraverse'offset c)) # itemLength))))
          }))
        (pcon PNothing)

popenedFrameControl :: forall (s :: S). Term s (PDataTraverseControlV1 :--> Frame.PDataFrameV1 :--> PInteger :--> PMaybe PDataTraverseControlV1)
popenedFrameControl = phoistAcyclic $ plam $ \control frame headLength -> pmatch control $ \c -> pmatch frame $ \f ->
  pcheckedControl (pcon c
    { ptraverse'stage = pdata (pif (pfromData (Frame.pframe'expectedChildren f) #== 0) pstageFold pstageHead)
    , ptraverse'offset = pdata (pfromData (ptraverse'offset c) + headLength)
    , ptraverse'frameRoot = pdata (Frame.phashFrameV1 # frame)
    })

pstepHeadSequence :: forall (s :: S). Term s (PDataTraverseControlV1 :--> PByteString :--> PInteger :--> PMaybe PDataTraverseControlV1)
pstepHeadSequence = phoistAcyclic $ plam $ \control bytes expectedChildren ->
  pif (pnot # (puint32IsWellFormed # expectedChildren)) (pcon PNothing) $
    plet (pif (expectedChildren #== 0) 128 159) $ \header ->
      pmatch (pparseSmallConstructorHead # bytes) $ \case
        PJust head -> pmatch head $ \(PSmallConstructorHeadV1 constructor prefixLength) ->
          pif (prefixLength #< plengthBS # bytes #&& pbyteAt # bytes # prefixLength #== header)
            (popenedFrameControl # control #
              (Frame.pinitialSmallConstrFrameV1 # constructor # (pframeRoot control) # expectedChildren)
              # (prefixLength + 1))
            (pcon PNothing)
        PNothing -> pif (pbyteAt # bytes # 0 #== header)
          (popenedFrameControl # control # (Frame.pinitialListFrameV1 # (pframeRoot control) # expectedChildren) # 1)
          (pcon PNothing)

pstepHeadMap :: forall (s :: S). Term s (PDataTraverseControlV1 :--> PByteString :--> PMaybe PDataTraverseControlV1)
pstepHeadMap = phoistAcyclic $ plam $ \control bytes -> pmatch (preadCanonicalArgument # bytes # 0) $ \case
  PNothing -> pcon PNothing
  PJust argument -> pmatch argument $ \(PCborArgumentV1 major value nextOffset) ->
    pif (major #== 5 #&& value #<= pquot # puint32Max # 2)
      (popenedFrameControl # control # (Frame.pinitialMapFrameV1 # (pframeRoot control) # (value * 2)) # nextOffset)
      (pcon PNothing)

pstepHeadLargeConstructor :: forall (s :: S). Term s (PDataTraverseControlV1 :--> PByteString :--> PInteger :--> PInteger :--> PMaybe PDataTraverseControlV1)
pstepHeadLargeConstructor = phoistAcyclic $ plam $ \control bytes constructorCborLength expectedChildren -> pmatch control $ \c ->
  pif
    ( constructorCborLength #> 0
        #&& puint32IsWellFormed # constructorCborLength
        #&& puint32IsWellFormed # expectedChildren
        #&& 3 #<= plengthBS # bytes
        #&& psliceLen # bytes # 0 # 3 #== pconstant "\xd8\x66\x82"
        #&& pfromData (ptraverse'offset c) + 3 + constructorCborLength #< pfromData (ptraverse'sourceLength c)
    )
    (plet (pfromData (ptraverse'offset c) + 3) $ \offset -> pcheckedControl (pcon c
      { ptraverse'stage = pdata pstageLargeConstructor
      , ptraverse'offset = pdata offset
      , ptraverse'pendingLargeExpectedChildren = pdata (pcon (PDJust (pdata expectedChildren)))
      , ptraverse'integer = pdata (pcon (PDJust (pdata (Integer.pinitialControlV1 #
          (pfromData (ptraverse'sourceStart c) + offset) # constructorCborLength))))
      }))
    (pcon PNothing)

pactionIsNoAction :: forall (s :: S). Term s PDataTraverseActionV1 -> Term s PBool
pactionIsNoAction action = pmatch action $ \case
  PNoAction -> pconstant True
  _ -> pconstant False

pframeRoot :: forall (s :: S). Term s PDataTraverseControlV1 -> Term s PByteString
pframeRoot control = pmatch control $ \c -> pfromData (ptraverse'frameRoot c)

pstepHead :: forall (s :: S). Term s (PDataTraverseControlV1 :--> PMaybe PByteString :--> PDataTraverseActionV1 :--> PMaybe PDataTraverseControlV1)
pstepHead = phoistAcyclic $ plam $ \control sourceBytes action -> pmatch (pexactSourceBytes control sourceBytes) $ \case
  PNothing -> pcon PNothing
  PJust bytes -> pmatch action $ \case
    PHeadScalar itemLength -> pstepHeadScalar # control # bytes # pfromData itemLength
    PHeadSequence expectedChildren -> pstepHeadSequence # control # bytes # pfromData expectedChildren
    PHeadMap -> pstepHeadMap # control # bytes
    PHeadLargeConstructor constructorLength expectedChildren ->
      pstepHeadLargeConstructor # control # bytes # pfromData constructorLength # pfromData expectedChildren
    _ -> pcon PNothing

pstepInteger :: forall (s :: S). Term s (PDataTraverseControlV1 :--> PMaybe PByteString :--> PDataTraverseActionV1 :--> PMaybe PDataTraverseControlV1)
pstepInteger = phoistAcyclic $ plam $ \control sourceBytes action -> pmatch control $ \c ->
  pmatch (pfromData (ptraverse'integer c)) $ \case
    PDNothing -> perror
    PDJust integerData -> plet (pfromData integerData) $ \integer -> pmatch integer $ \i ->
      pif (pfromData (Integer.pint'stage i) #== Integer.pstageTerminal)
        (pmatch sourceBytes $ \case
          PJust _ -> pcon PNothing
          PNothing -> pmatch action $ \case
            PAttachScalar parent -> pmatch (Integer.pfinalizeV1 # integer) $ \case
              PNothing -> pcon PNothing
              PJust summary -> pattachSummary control summary (pfromData parent)
                (pfromData (ptraverse'offset c) + pfromData (Integer.pint'sourceLength i))
            _ -> pcon PNothing) $
      pif (pnot # (pactionIsNoAction action)) (pcon PNothing) $
        pmatch (Integer.pstepV1 # integer # sourceBytes) $ \case
          PNothing -> pcon PNothing
          PJust next -> pcheckedControl (pcon c {ptraverse'integer = pdata (pcon (PDJust (pdata next)))})

pstepBytes :: forall (s :: S). Term s (PDataTraverseControlV1 :--> PMaybe PByteString :--> PDataTraverseActionV1 :--> PMaybe PDataTraverseControlV1)
pstepBytes = phoistAcyclic $ plam $ \control sourceBytes action -> pmatch control $ \c ->
  pmatch (pfromData (ptraverse'bytes c)) $ \case
    PDNothing -> perror
    PDJust bytesData -> plet (pfromData bytesData) $ \bytesControl -> pmatch bytesControl $ \b ->
      pif (pfromData (Bytes.pbytes'stage b) #== Bytes.pstageTerminal)
        (pmatch sourceBytes $ \case
          PJust _ -> pcon PNothing
          PNothing -> pmatch action $ \case
            PAttachScalar parent -> pmatch (Bytes.pfinalizeV1 # bytesControl) $ \case
              PNothing -> pcon PNothing
              PJust summary -> pattachSummary control summary (pfromData parent)
                (pfromData (ptraverse'offset c) + pfromData (Bytes.pbytes'sourceLength b))
            _ -> pcon PNothing) $
      pif (pnot # (pactionIsNoAction action)) (pcon PNothing) $
        pmatch (Bytes.pstepV1 # bytesControl # sourceBytes) $ \case
          PNothing -> pcon PNothing
          PJust next -> pcheckedControl (pcon c {ptraverse'bytes = pdata (pcon (PDJust (pdata next)))})

pstepLargeConstructor :: forall (s :: S). Term s (PDataTraverseControlV1 :--> PMaybe PByteString :--> PDataTraverseActionV1 :--> PMaybe PDataTraverseControlV1)
pstepLargeConstructor = phoistAcyclic $ plam $ \control sourceBytes action -> pmatch control $ \c ->
  pmatch (pfromData (ptraverse'integer c)) $ \case
    PDNothing -> perror
    PDJust integerData -> plet (pfromData integerData) $ \integer -> pmatch integer $ \i ->
      pif (pfromData (Integer.pint'stage i) #== Integer.pstageTerminal)
        (pif (sourceBytes #== pcon PNothing #&& pactionIsNoAction action)
          (pcheckedControl (pcon c
            { ptraverse'stage = pdata pstageLargeFields
            , ptraverse'offset = pdata (pfromData (ptraverse'offset c) + pfromData (Integer.pint'sourceLength i))
            }))
          (pcon PNothing)) $
      pif (pnot # (pactionIsNoAction action)) (pcon PNothing) $
        plet
          (pif (pfromData (Integer.pint'stage i) #== Integer.pstageSyntax)
            (pmatch sourceBytes $ \case
              PNothing -> pconstant False
              PJust bytes -> pmatch (Integer.pparseLargeConstructorSyntaxV1 # bytes # pfromData (Integer.pint'sourceLength i)) $ \case
                PNothing -> pconstant False
                PJust _ -> pconstant True)
            (pconstant True)) $ \syntaxValid ->
          pif (pnot # syntaxValid) (pcon PNothing) $
            pmatch (Integer.pstepV1 # integer # sourceBytes) $ \case
              PNothing -> pcon PNothing
              PJust next -> pcheckedControl (pcon c {ptraverse'integer = pdata (pcon (PDJust (pdata next)))})

pstepLargeFields :: forall (s :: S). Term s (PDataTraverseControlV1 :--> PMaybe PByteString :--> PDataTraverseActionV1 :--> PMaybe PDataTraverseControlV1)
pstepLargeFields = phoistAcyclic $ plam $ \control sourceBytes action ->
  pif (pnot # (pactionIsNoAction action)) (pcon PNothing) $ pmatch control $ \c ->
    pmatch (pfromData (ptraverse'pendingLargeExpectedChildren c)) $ \case
      PDNothing -> perror
      PDJust expectedData -> pmatch (pfromData (ptraverse'integer c)) $ \case
        PDNothing -> perror
        PDJust integerData -> plet (pfromData integerData) $ \integer -> pmatch integer $ \i ->
          pmatch (pfromData (Integer.pint'blob i)) $ \case
            PDNothing -> perror
            PDJust blobData ->
              pmatch (pexactSourceBytes control sourceBytes) $ \case
                PNothing -> pcon PNothing
                PJust bytes -> pmatch (Blob.pfinalizeV1 # pfromData blobData) $ \case
                  PNothing -> pcon PNothing
                  PJust constructorRoot -> plet (pfromData expectedData) $ \expectedChildren ->
                    plet (pif (expectedChildren #== 0) 128 159) $ \header ->
                    pif (pbyteAt # bytes # 0 #== header)
                      (plet (Frame.pinitialLargeConstrFrameV1 # constructorRoot #
                              pfromData (Integer.pint'sourceLength i) # pfromData (Integer.pint'memory i) #
                              pfromData (ptraverse'frameRoot c) # expectedChildren) $ \frame ->
                        pcheckedControl (pcon c
                          { ptraverse'stage = pdata (pif (expectedChildren #== 0) pstageFold pstageHead)
                          , ptraverse'offset = pdata (pfromData (ptraverse'offset c) + 1)
                          , ptraverse'frameRoot = pdata (Frame.phashFrameV1 # frame)
                          , ptraverse'pendingLargeExpectedChildren = pdata (pcon PDNothing)
                          , ptraverse'integer = pdata (pcon PDNothing)
                          }))
                      (pcon PNothing)

pstepClose :: forall (s :: S). Term s (PDataTraverseControlV1 :--> PMaybe PByteString :--> PDataTraverseActionV1 :--> PMaybe PDataTraverseControlV1)
pstepClose = phoistAcyclic $ plam $ \control sourceBytes action ->
  pif (pnot # (pactionIsNoAction action)) (pcon PNothing) $
    pmatch (pexactSourceBytes control sourceBytes) $ \case
      PNothing -> pcon PNothing
      PJust bytes -> pif (pbyteAt # bytes # 0 #== 255)
        (pmatch control $ \c -> pcheckedControl (pcon c
          { ptraverse'stage = pdata pstageFold
          , ptraverse'offset = pdata (pfromData (ptraverse'offset c) + 1)
          }))
        (pcon PNothing)

pprevalidatedFinalizeFrameTransitionV1 ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PInteger :--> Frame.PDataFrameV1 :-->
    PMaybeData Frame.PDataFrameV1 :--> PMaybe PFinalizeFrameTransitionV1)
pprevalidatedFinalizeFrameTransitionV1 = phoistAcyclic $ plam $ \currentRoot currentOffset sourceLength frame parent ->
  pif (Frame.pframeIsWellFormedV1 # frame #&& Frame.phashFrameV1 # frame #== currentRoot)
    (pmatch (Frame.pfinalizedSummaryV1 # frame) $ \case
      PNothing -> pcon PNothing
      PJust summary -> pmatch frame $ \f ->
        plet (pfromData (Frame.pframe'tail f)) $ \tailRoot ->
          pprevalidatedAttachSummaryTransitionV1 tailRoot summary parent currentOffset sourceLength)
    (pcon PNothing)

pstepFinalizeFrame ::
  forall (s :: S). Term s PDataTraverseControlV1 -> Term s Frame.PDataFrameV1 -> Term s (PMaybeData Frame.PDataFrameV1) -> Term s (PMaybe PDataTraverseControlV1)
pstepFinalizeFrame control frame parent = pmatch control $ \c ->
  pmatch (pprevalidatedFinalizeFrameTransitionV1 # pfromData (ptraverse'frameRoot c) #
    pfromData (ptraverse'offset c) # pfromData (ptraverse'sourceLength c) # frame # parent) $ \case
      PNothing -> pcon PNothing
      PJust transition -> pmatch transition $ \(PFinalizeFrameTransitionV1 nextStage nextRoot nextResult) ->
        pcheckedControl (pcon c
          { ptraverse'stage = pdata nextStage
          , ptraverse'frameRoot = pdata nextRoot
          , ptraverse'pendingLargeExpectedChildren = pdata (pcon PDNothing)
          , ptraverse'integer = pdata (pcon PDNothing)
          , ptraverse'bytes = pdata (pcon PDNothing)
          , ptraverse'result = pdata nextResult
          })

pprevalidatedFoldMapNextFrameRootV1 ::
  forall (s :: S).
  Term s (PByteString :--> Frame.PDataFrameV1 :--> PInteger :--> PDataSummaryV1 :--> PDataSummaryV1 :-->
    PBuiltinList (PAsData PByteString) :--> PBuiltinList (PAsData PByteString) :--> PMaybe PByteString)
pprevalidatedFoldMapNextFrameRootV1 = phoistAcyclic $ plam $ \currentRoot frame pairIndex key value keySiblings valueSiblings ->
  pif (Frame.pframeIsWellFormedV1 # frame #&& Frame.phashFrameV1 # frame #== currentRoot)
    (pmatch (Frame.pfoldMapPairV1 # frame # pairIndex # key # value # keySiblings # valueSiblings) $ \case
      PNothing -> pcon PNothing
      PJust next -> pcon (PJust (Frame.phashFrameV1 # next)))
    (pcon PNothing)

pstepFold :: forall (s :: S). Term s (PDataTraverseControlV1 :--> PMaybe PByteString :--> PDataTraverseActionV1 :--> PMaybe PDataTraverseControlV1)
pstepFold = phoistAcyclic $ plam $ \control sourceBytes action -> pmatch sourceBytes $ \case
  PJust _ -> pcon PNothing
  PNothing -> pmatch action $ \case
    PFoldList frameData childIndex childData siblingsData ->
      plet (pfromData frameData) $ \frame ->
      pif (Frame.pframeIsWellFormedV1 # frame #&& Frame.phashFrameV1 # frame #== pframeRoot control)
        (pmatch (Frame.pfoldListChildV1 # frame # pfromData childIndex # pfromData childData # pfromData siblingsData) $ \case
          PNothing -> pcon PNothing
          PJust next -> pmatch control $ \c -> pcheckedControl (pcon c
            {ptraverse'frameRoot = pdata (Frame.phashFrameV1 # next)}))
        (pcon PNothing)
    PFoldMap frameData pairIndex keyData valueData keySiblingsData valueSiblingsData ->
      pmatch (pprevalidatedFoldMapNextFrameRootV1 # (pframeRoot control) # pfromData frameData #
        pfromData pairIndex # pfromData keyData # pfromData valueData #
        pfromData keySiblingsData # pfromData valueSiblingsData) $ \case
          PNothing -> pcon PNothing
          PJust nextRoot -> pmatch control $ \c -> pcheckedControl (pcon c {ptraverse'frameRoot = pdata nextRoot})
    PFinalizeFrame frameData parentData -> pstepFinalizeFrame control (pfromData frameData) (pfromData parentData)
    _ -> pcon PNothing

pstepV1 ::
  forall (s :: S). Term s (PDataTraverseControlV1 :--> PMaybe PByteString :--> PDataTraverseActionV1 :--> PMaybe PDataTraverseControlV1)
pstepV1 = phoistAcyclic $ plam $ \control sourceBytes action ->
  pif (pnot # (pcontrolIsWellFormed # control)) (pcon PNothing) $ pmatch control $ \c ->
    plet (pfromData (ptraverse'stage c)) $ \stage ->
      pif (stage #== pstageHead) (pstepHead # control # sourceBytes # action) $
      pif (stage #== pstageInteger) (pstepInteger # control # sourceBytes # action) $
      pif (stage #== pstageBytes) (pstepBytes # control # sourceBytes # action) $
      pif (stage #== pstageLargeConstructor) (pstepLargeConstructor # control # sourceBytes # action) $
      pif (stage #== pstageLargeFields) (pstepLargeFields # control # sourceBytes # action) $
      pif (stage #== pstageClose) (pstepClose # control # sourceBytes # action) $
      pif (stage #== pstageFold) (pstepFold # control # sourceBytes # action) (pcon PNothing)

pfinalizeV1 :: forall (s :: S). Term s (PDataTraverseControlV1 :--> PMaybe PDataSummaryV1)
pfinalizeV1 = phoistAcyclic $ plam $ \control ->
  pif (pcontrolIsWellFormed # control #&& pstage control #== pstageTerminal)
    (pmatch control $ \c -> pmatch (pfromData (ptraverse'result c)) $ \case
      PDNothing -> pcon PNothing
      PDJust result -> pcon (PJust (pfromData result)))
    (pcon PNothing)

pstage :: forall (s :: S). Term s PDataTraverseControlV1 -> Term s PInteger
pstage control = pmatch control $ \c -> pfromData (ptraverse'stage c)
