{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.CekDataInteger
Description : Plutarch port of @lib/midgard/cek-data-integer-v1.ak@.
-}
module Midgard.CekDataInteger (
  PCekDataIntegerControlV1 (..),
  PCborArgumentV1 (..),
  pversion,
  psyntaxBytes,
  pstageSyntax,
  pstageBlob,
  pstageTerminal,
  pparseSyntaxV1,
  pparseLargeConstructorSyntaxV1,
  pcontrolIsWellFormed,
  pinitialControlV1,
  pencodeControlV1,
  pcontrolFromDataV1,
  pdecodeControlV1,
  pnextSourceSpanV1,
  pstepV1,
  pfinalizeV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.ByteString (pbyteStringToInteger, pmostSignificantFirst)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.CekData (PDataNodeV1 (..), PDataSummaryV1 (..), phashDataNodeV1)
import Midgard.CekSourceBlob qualified as Blob
import Midgard.FraudProofs.NativeTx.Codec (
  pbyteAt,
  pcborInt,
  pencodeDefiniteArrayHeader,
  psliceLen,
 )

pversion, psyntaxBytes, pstageSyntax, pstageBlob, pstageTerminal :: forall (s :: S). Term s PInteger
pversion = 1
psyntaxBytes = 14
pstageSyntax = 0
pstageBlob = 1
pstageTerminal = 2

puint32Max, puint64Max :: forall (s :: S). Term s PInteger
puint32Max = 4_294_967_295
puint64Max = 18_446_744_073_709_551_615

data PCekDataIntegerControlV1 (s :: S) = PCekDataIntegerControlV1
  { pint'version :: Term s (PAsData PInteger)
  , pint'stage :: Term s (PAsData PInteger)
  , pint'sourceStart :: Term s (PAsData PInteger)
  , pint'sourceLength :: Term s (PAsData PInteger)
  , pint'memory :: Term s (PAsData PInteger)
  , pint'blob :: Term s (PAsData (PMaybeData Blob.PCekSourceBlobControlV1))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekDataIntegerControlV1)

data PCborArgumentV1 (s :: S) = PCborArgumentV1
  { parg'major :: Term s PInteger
  , parg'value :: Term s PInteger
  , parg'nextOffset :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsSOPStruct PCborArgumentV1)

pminimum :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pminimum = phoistAcyclic $ plam $ \first second -> pif (first #< second) first second

pargumentByteCount :: forall (s :: S). Term s (PInteger :--> PInteger)
pargumentByteCount = phoistAcyclic $
  plam $ \additional ->
    pif (additional #== 24) 1 $
      pif (additional #== 25) 2 $
        pif (additional #== 26) 4 $
          pif (additional #== 27) 8 (-1)

pargumentIsMinimal :: forall (s :: S). Term s (PInteger :--> PInteger :--> PBool)
pargumentIsMinimal = phoistAcyclic $
  plam $ \additional value ->
    pif (additional #== 24) (24 #<= value) $
      pif (additional #== 25) (255 #< value) $
        pif (additional #== 26) (65_535 #< value) $
          pif (additional #== 27) (4_294_967_295 #< value) (pconstant False)

preadCanonicalArgument ::
  forall (s :: S). Term s (PByteString :--> PInteger :--> PMaybe PCborArgumentV1)
preadCanonicalArgument = phoistAcyclic $
  plam $ \bytes offset ->
    plet (plengthBS # bytes) $ \len ->
      pif (offset #< 0 #|| len #<= offset) (pcon PNothing) $
        plet (pbyteAt # bytes # offset) $ \initial ->
          plet (pquot # initial # 32) $ \major ->
            plet (prem # initial # 32) $ \additional ->
              pif
                (additional #< 24)
                (pcon (PJust (pcon (PCborArgumentV1 major additional (offset + 1)))))
                $ plet (pargumentByteCount # additional)
                $ \byteCount ->
                  pif
                    (byteCount #< 0 #|| len #< offset + 1 + byteCount)
                    (pcon PNothing)
                    $ plet
                      ( pbyteStringToInteger
                          # pmostSignificantFirst
                          # (psliceLen # bytes # (offset + 1) # byteCount)
                      )
                    $ \value ->
                      pif
                        (pargumentIsMinimal # additional # value)
                        ( pcon $
                            PJust $
                              pcon $
                                PCborArgumentV1 major value (offset + 1 + byteCount)
                        )
                        (pcon PNothing)

punsignedByteSize :: forall (s :: S). Term s (PInteger :--> PInteger)
punsignedByteSize = phoistAcyclic $
  pfix $ \self -> plam $ \value ->
    pif (value #< 256) 1 (1 + self # (pquot # value # 256))

pparseSyntaxV1 :: forall (s :: S). Term s (PByteString :--> PInteger :--> PMaybe PInteger)
pparseSyntaxV1 = phoistAcyclic $
  plam $ \bytes sourceLength ->
    pif
      ( sourceLength #< 1
          #|| puint32Max #< sourceLength
          #|| pnot # (plengthBS # bytes #== pminimum # sourceLength # psyntaxBytes)
      )
      (pcon PNothing)
      $ plet (pbyteAt # bytes # 0)
      $ \first ->
        pif
          (pquot # first # 32 #<= 1)
          ( pmatch (preadCanonicalArgument # bytes # 0) $ \case
              PNothing -> pcon PNothing
              PJust argument ->
                pmatch argument $ \(PCborArgumentV1 major value nextOffset) ->
                  pif
                    (major #<= 1 #&& value #<= puint64Max #&& nextOffset #== sourceLength)
                    (pcon (PJust (4 + punsignedByteSize # (value * 2))))
                    (pcon PNothing)
          )
          $ pif
            (first #== 194 #|| first #== 195)
            ( pmatch (preadCanonicalArgument # bytes # 1) $ \case
                PNothing -> pcon PNothing
                PJust magnitude ->
                  pmatch magnitude $ \(PCborArgumentV1 major value nextOffset) ->
                    pif
                      ( major #== 2
                          #&& 9 #<= value
                          #&& value #<= puint32Max
                          #&& nextOffset + value #== sourceLength
                          #&& nextOffset #< plengthBS # bytes
                      )
                      ( plet (pbyteAt # bytes # nextOffset) $ \firstMagnitudeByte ->
                          pif
                            (firstMagnitudeByte #== 0)
                            (pcon PNothing)
                            (pcon (PJust (4 + value + pif (128 #<= firstMagnitudeByte) 1 0)))
                      )
                      (pcon PNothing)
            )
            (pcon PNothing)

pparseLargeConstructorSyntaxV1 ::
  forall (s :: S). Term s (PByteString :--> PInteger :--> PMaybe PInteger)
pparseLargeConstructorSyntaxV1 = phoistAcyclic $
  plam $ \bytes sourceLength ->
    pmatch (pparseSyntaxV1 # bytes # sourceLength) $ \case
      PNothing -> pcon PNothing
      PJust memory ->
        plet (pbyteAt # bytes # 0) $ \first ->
          pif
            (first #== 194)
            (pcon (PJust memory))
            $ pmatch (preadCanonicalArgument # bytes # 0)
            $ \case
              PNothing -> pcon PNothing
              PJust argument ->
                pmatch argument $ \(PCborArgumentV1 major value nextOffset) ->
                  pif
                    (major #== 0 #&& 127 #< value #&& nextOffset #== sourceLength)
                    (pcon (PJust memory))
                    (pcon PNothing)

pcontrolIsWellFormed :: forall (s :: S). Term s (PCekDataIntegerControlV1 :--> PBool)
pcontrolIsWellFormed = phoistAcyclic $
  plam $ \control ->
    pmatch control $ \c ->
      pand'List
        [ pfromData (pint'version c) #== pversion
        , pstageSyntax #<= pfromData (pint'stage c)
        , pfromData (pint'stage c) #<= pstageTerminal
        , 0 #<= pfromData (pint'sourceStart c)
        , 1 #<= pfromData (pint'sourceLength c)
        , pfromData (pint'sourceLength c) #<= puint32Max
        , 0 #<= pfromData (pint'memory c)
        , pfromData (pint'memory c) #<= puint64Max
        ]
        #&& pstageIsWellFormed control

pstageIsWellFormed :: forall (s :: S). Term s PCekDataIntegerControlV1 -> Term s PBool
pstageIsWellFormed control =
  pmatch control $ \c ->
    pif
      (pfromData (pint'stage c) #== pstageSyntax)
      ( pfromData (pint'memory c) #== 0
          #&& pmatch (pfromData (pint'blob c)) (\case PDNothing -> pconstant True; PDJust _ -> pconstant False)
      )
      $ pmatch (pfromData (pint'blob c))
      $ \case
        PDNothing -> pconstant False
        PDJust blobData ->
          plet (pfromData blobData) $ \blob ->
            pand'List
              [ 5 #<= pfromData (pint'memory c)
              , Blob.pcontrolIsWellFormed # blob
              , pblobSourceStart blob #== pfromData (pint'sourceStart c)
              , pblobSourceLength blob #== pfromData (pint'sourceLength c)
              , pif
                  (pfromData (pint'stage c) #== pstageTerminal)
                  (pblobStage blob #== Blob.pstageTerminal)
                  (pconstant True)
              ]

pblobSourceStart :: forall (s :: S). Term s Blob.PCekSourceBlobControlV1 -> Term s PInteger
pblobSourceStart blob = pmatch blob $ \b -> pfromData (Blob.pblob'sourceStart b)

pblobSourceLength :: forall (s :: S). Term s Blob.PCekSourceBlobControlV1 -> Term s PInteger
pblobSourceLength blob = pmatch blob $ \b -> pfromData (Blob.pblob'sourceLength b)

pblobStage :: forall (s :: S). Term s Blob.PCekSourceBlobControlV1 -> Term s PInteger
pblobStage blob = pmatch blob $ \b -> pfromData (Blob.pblob'stage b)

pinitialControlV1 ::
  forall (s :: S). Term s (PInteger :--> PInteger :--> PCekDataIntegerControlV1)
pinitialControlV1 = phoistAcyclic $
  plam $ \sourceStart sourceLength ->
    plet
      ( pcon $
          PCekDataIntegerControlV1
            { pint'version = pdata pversion
            , pint'stage = pdata pstageSyntax
            , pint'sourceStart = pdata sourceStart
            , pint'sourceLength = pdata sourceLength
            , pint'memory = pdata 0
            , pint'blob = pdata (pcon PDNothing)
            }
      )
      $ \control -> pif (pcontrolIsWellFormed # control) control perror

pencodeOptionalBlob ::
  forall (s :: S). Term s (PMaybeData Blob.PCekSourceBlobControlV1 :--> PByteString)
pencodeOptionalBlob = phoistAcyclic $
  plam $ \blob ->
    pmatch blob $ \case
      PDNothing -> pconstant "\xd8\x7a\x80"
      PDJust value ->
        pconstant "\xd8\x79\x9f" <> (Blob.pencodeControlV1 # pfromData value) <> pconstant "\xff"

pencodeControlV1 :: forall (s :: S). Term s (PCekDataIntegerControlV1 :--> PByteString)
pencodeControlV1 = phoistAcyclic $
  plam $ \control ->
    pif (pnot # (pcontrolIsWellFormed # control)) perror $
      pmatch control $ \c ->
        (pencodeDefiniteArrayHeader # 6)
          <> pcborInt pversion
          <> pcborInt (pfromData (pint'stage c))
          <> pcborInt (pfromData (pint'sourceStart c))
          <> pcborInt (pfromData (pint'sourceLength c))
          <> pcborInt (pfromData (pint'memory c))
          <> (pencodeOptionalBlob # pfromData (pint'blob c))

poptionalBlobFromData ::
  forall (s :: S). Term s (PData :--> PMaybeData Blob.PCekSourceBlobControlV1)
poptionalBlobFromData = phoistAcyclic $
  plam $ \d ->
    pmatch (pasConstr # d) $ \(PBuiltinPair index fields) ->
      pif
        (index #== 0)
        ( pif (pnot # (plength # fields #== 1)) perror $
            pcon (PDJust (pdata (Blob.pcontrolFromDataV1 # (pelemAt # 0 # fields))))
        )
        $ pif (index #== 1 #&& pnull # fields) (pcon PDNothing) perror

pcontrolFromDataV1 :: forall (s :: S). Term s (PData :--> PCekDataIntegerControlV1)
pcontrolFromDataV1 = phoistAcyclic $
  plam $ \d ->
    plet (pasList # d) $ \items ->
      pif (pnot # (plength # items #== 6)) perror $
        plet
          ( pcon $
              PCekDataIntegerControlV1
                { pint'version = pdata (pasInt # (pelemAt # 0 # items))
                , pint'stage = pdata (pasInt # (pelemAt # 1 # items))
                , pint'sourceStart = pdata (pasInt # (pelemAt # 2 # items))
                , pint'sourceLength = pdata (pasInt # (pelemAt # 3 # items))
                , pint'memory = pdata (pasInt # (pelemAt # 4 # items))
                , pint'blob = pdata (poptionalBlobFromData # (pelemAt # 5 # items))
                }
          )
          $ \control -> pif (pcontrolIsWellFormed # control) control perror

pdecodeControlV1 :: forall (s :: S). Term s (PByteString :--> PCekDataIntegerControlV1)
pdecodeControlV1 = phoistAcyclic $
  plam $ \controlCbor ->
    pmatch (pdeserialise # controlCbor) $ \case
      PNothing -> perror
      PJust d ->
        plet (pcontrolFromDataV1 # d) $ \control ->
          pif (pencodeControlV1 # control #== controlCbor) control perror

pnextSourceSpanV1 ::
  forall (s :: S). Term s (PCekDataIntegerControlV1 :--> PMaybe Blob.PCekSourceBlobSpanV1)
pnextSourceSpanV1 = phoistAcyclic $
  plam $ \control ->
    pif (pnot # (pcontrolIsWellFormed # control)) (pcon PNothing) $
      pmatch control $ \c ->
        pif
          (pfromData (pint'stage c) #== pstageSyntax)
          ( pcon $
              PJust $
                pcon $
                  Blob.PCekSourceBlobSpanV1
                    (pint'sourceStart c)
                    (pdata (pminimum # pfromData (pint'sourceLength c) # psyntaxBytes))
          )
          $ pif
            (pfromData (pint'stage c) #== pstageBlob)
            ( pmatch (pfromData (pint'blob c)) $ \case
                PDNothing -> perror
                PDJust blob -> Blob.pnextSourceSpanV1 # pfromData blob
            )
            (pcon PNothing)

pstepSyntax ::
  forall (s :: S).
  Term s PCekDataIntegerControlV1 ->
  Term s (PMaybe PByteString) ->
  Term s (PMaybe PCekDataIntegerControlV1)
pstepSyntax control sourceBytes =
  pmatch sourceBytes $ \case
    PNothing -> pcon PNothing
    PJust bytes ->
      pmatch (pnextSourceSpanV1 # control) $ \case
        PNothing -> perror
        PJust span ->
          pmatch span $ \(Blob.PCekSourceBlobSpanV1 _ spanLength) ->
            pif (pnot # (plengthBS # bytes #== pfromData spanLength)) (pcon PNothing) $
              pmatch control $ \c ->
                pmatch (pparseSyntaxV1 # bytes # pfromData (pint'sourceLength c)) $ \case
                  PNothing -> pcon PNothing
                  PJust memory ->
                    plet
                      ( pcon
                          c
                            { pint'stage = pdata pstageBlob
                            , pint'memory = pdata memory
                            , pint'blob =
                                pdata $
                                  pcon $
                                    PDJust $
                                      pdata $
                                        Blob.pinitialControlV1
                                          # pfromData (pint'sourceStart c)
                                          # pfromData (pint'sourceLength c)
                            }
                      )
                      $ \next ->
                        pif (pcontrolIsWellFormed # next) (pcon (PJust next)) (pcon PNothing)

pstepBlob ::
  forall (s :: S).
  Term s PCekDataIntegerControlV1 ->
  Term s (PMaybe PByteString) ->
  Term s (PMaybe PCekDataIntegerControlV1)
pstepBlob control sourceBytes =
  pmatch control $ \c ->
    pmatch (pfromData (pint'blob c)) $ \case
      PDNothing -> perror
      PDJust blobData ->
        plet (pfromData blobData) $ \blob ->
          pif
            (pblobStage blob #== Blob.pstageTerminal)
            ( pmatch sourceBytes $ \case
                PJust _ -> pcon PNothing
                PNothing ->
                  plet (pcon c {pint'stage = pdata pstageTerminal}) $ \next ->
                    pif (pcontrolIsWellFormed # next) (pcon (PJust next)) (pcon PNothing)
            )
            $ pmatch (Blob.pstepV1 # blob # sourceBytes)
            $ \case
              PNothing -> pcon PNothing
              PJust nextBlob ->
                plet (pcon c {pint'blob = pdata (pcon (PDJust (pdata nextBlob)))}) $ \next ->
                  pif (pcontrolIsWellFormed # next) (pcon (PJust next)) (pcon PNothing)

pstepV1 ::
  forall (s :: S).
  Term
    s
    ( PCekDataIntegerControlV1
        :--> PMaybe PByteString
        :--> PMaybe PCekDataIntegerControlV1
    )
pstepV1 = phoistAcyclic $
  plam $ \control sourceBytes ->
    pif (pnot # (pcontrolIsWellFormed # control)) (pcon PNothing) $
      pmatch control $ \c ->
        pif
          (pfromData (pint'stage c) #== pstageSyntax)
          (pstepSyntax control sourceBytes)
          $ pif
            (pfromData (pint'stage c) #== pstageBlob)
            (pstepBlob control sourceBytes)
            (pcon PNothing)

pfinalizeV1 ::
  forall (s :: S). Term s (PCekDataIntegerControlV1 :--> PMaybe PDataSummaryV1)
pfinalizeV1 = phoistAcyclic $
  plam $ \control ->
    pif
      ( pnot
          #$ pcontrolIsWellFormed
          # control
          #&& pcontrolStage control
          #== pstageTerminal
      )
      (pcon PNothing)
      $ pmatch control
      $ \c ->
        pmatch (pfromData (pint'blob c)) $ \case
          PDNothing -> perror
          PDJust blobData ->
            pmatch (Blob.pfinalizeV1 # pfromData blobData) $ \case
              PNothing -> pcon PNothing
              PJust cborRoot ->
                plet
                  ( pcon $
                      PIntegerDataNode
                        { pnode'cborRoot = pdata cborRoot
                        , pnode'cborLength = pint'sourceLength c
                        , pnode'memory = pint'memory c
                        }
                  )
                  $ \node ->
                    pcon $
                      PJust $
                        pcon $
                          PDataSummaryV1
                            { psummary'root = pdata (phashDataNodeV1 # node)
                            , psummary'cborLength = pint'sourceLength c
                            , psummary'memory = pint'memory c
                            }

pcontrolStage :: forall (s :: S). Term s PCekDataIntegerControlV1 -> Term s PInteger
pcontrolStage control = pmatch control $ \c -> pfromData (pint'stage c)
