{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.CekDataBytes
Description : Plutarch port of @lib/midgard/cek-data-bytes-v1.ak@.
-}
module Midgard.CekDataBytes (
  PCekDataBytesControlV1 (..),
  PContentPlanV1 (..),
  pversion,
  psyntaxBytes,
  pmaximumSourceSpan,
  pstageSyntax,
  pstageBlob,
  pstageBreak,
  pstageTerminal,
  pparseSyntaxV1,
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

import Plutarch.Builtin.ByteString (pintegerToByteString, pmostSignificantFirst)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.CekData (
  PDataNodeV1 (..),
  PDataSummaryV1 (..),
  pbytesDataCborLengthV1,
  phashDataNodeV1,
 )
import Midgard.CekSourceBlob qualified as Blob
import Midgard.FraudProofs.NativeTx.Codec (
  pbyteAt,
  pcborInt,
  pencodeDefiniteArrayHeader,
  psliceLen,
 )

pversion, psyntaxBytes, pmaximumSourceSpan :: forall (s :: S). Term s PInteger
pversion = 1
psyntaxBytes = 2
pmaximumSourceSpan = 132

pstageSyntax, pstageBlob, pstageBreak, pstageTerminal :: forall (s :: S). Term s PInteger
pstageSyntax = 0
pstageBlob = 1
pstageBreak = 2
pstageTerminal = 3

puint32Max, pcardanoDataBytesChunk :: forall (s :: S). Term s PInteger
puint32Max = 4_294_967_295
pcardanoDataBytesChunk = 64

data PCekDataBytesControlV1 (s :: S) = PCekDataBytesControlV1
  { pbytes'version :: Term s (PAsData PInteger)
  , pbytes'stage :: Term s (PAsData PInteger)
  , pbytes'sourceStart :: Term s (PAsData PInteger)
  , pbytes'sourceLength :: Term s (PAsData PInteger)
  , pbytes'bytesLength :: Term s (PAsData PInteger)
  , pbytes'blob :: Term s (PAsData (PMaybeData Blob.PCekSourceBlobControlV1))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekDataBytesControlV1)

data PContentPlanV1 (s :: S) = PContentPlanV1
  { pplan'span :: Term s Blob.PCekSourceBlobSpanV1
  , pplan'contentStart :: Term s PInteger
  , pplan'contentLength :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsSOPStruct PContentPlanV1)

pminimum :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pminimum = phoistAcyclic $ plam $ \first second -> pif (first #< second) first second

pdefiniteBytesHeader :: forall (s :: S). Term s (PInteger :--> PByteString)
pdefiniteBytesHeader = phoistAcyclic $
  plam $ \len ->
    pif (0 #<= len #&& len #<= pcardanoDataBytesChunk) `flip` perror $
      pif
        (len #< 24)
        (pintegerToByteString # pmostSignificantFirst # 1 # (64 + len))
        (pconstant "\x58" <> (pintegerToByteString # pmostSignificantFirst # 1 # len))

pdefiniteHeaderLength :: forall (s :: S). Term s (PInteger :--> PInteger)
pdefiniteHeaderLength = phoistAcyclic $ plam $ \len -> plengthBS # (pdefiniteBytesHeader # len)

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
          (64 #<= first #&& first #<= 87)
          ( plet (first - 64) $ \bytesLength ->
              pif
                (sourceLength #== 1 + bytesLength)
                (pcon (PJust bytesLength))
                (pcon PNothing)
          )
          $ pif
            (first #== 88)
            ( pif (plengthBS # bytes #< 2) (pcon PNothing) $
                plet (pbyteAt # bytes # 1) $ \bytesLength ->
                  pif
                    ( 24 #<= bytesLength
                        #&& bytesLength #<= pcardanoDataBytesChunk
                        #&& sourceLength #== 2 + bytesLength
                    )
                    (pcon (PJust bytesLength))
                    (pcon PNothing)
            )
            $ pif
              (first #== 95 #&& 2 #<= sourceLength)
              ( plet (sourceLength - 2) $ \framedPayloadLength ->
                  plet (pquot # framedPayloadLength # 66) $ \fullChunks ->
                    plet (prem # framedPayloadLength # 66) $ \encodedRemainder ->
                      plet
                        ( pif
                            (encodedRemainder #== 0)
                            0
                            $ pif
                              (2 #<= encodedRemainder #&& encodedRemainder #<= 24)
                              (encodedRemainder - 1)
                              $ pif
                                (26 #<= encodedRemainder #&& encodedRemainder #<= 65)
                                (encodedRemainder - 2)
                                (-1)
                        )
                        $ \remainder ->
                          plet (fullChunks * pcardanoDataBytesChunk + remainder) $ \bytesLength ->
                            pif
                              ( 0 #<= remainder
                                  #&& pcardanoDataBytesChunk #< bytesLength
                                  #&& bytesLength #<= puint32Max
                                  #&& pbytesDataCborLengthV1 # bytesLength #== sourceLength
                              )
                              (pcon (PJust bytesLength))
                              (pcon PNothing)
              )
              (pcon PNothing)

pcontrolIsWellFormed :: forall (s :: S). Term s (PCekDataBytesControlV1 :--> PBool)
pcontrolIsWellFormed = phoistAcyclic $
  plam $ \control ->
    pmatch control $ \c ->
      pand'List
        [ pfromData (pbytes'version c) #== pversion
        , pstageSyntax #<= pfromData (pbytes'stage c)
        , pfromData (pbytes'stage c) #<= pstageTerminal
        , 0 #<= pfromData (pbytes'sourceStart c)
        , 1 #<= pfromData (pbytes'sourceLength c)
        , pfromData (pbytes'sourceLength c) #<= puint32Max
        , 0 #<= pfromData (pbytes'bytesLength c)
        , pfromData (pbytes'bytesLength c) #<= puint32Max
        ]
        #&& pstageIsWellFormed control

pstageIsWellFormed :: forall (s :: S). Term s PCekDataBytesControlV1 -> Term s PBool
pstageIsWellFormed control =
  pmatch control $ \c ->
    pif
      (pfromData (pbytes'stage c) #== pstageSyntax)
      ( pfromData (pbytes'bytesLength c) #== 0
          #&& pmatch (pfromData (pbytes'blob c)) (\case PDNothing -> pconstant True; PDJust _ -> pconstant False)
      )
      $ pmatch (pfromData (pbytes'blob c))
      $ \case
        PDNothing -> pconstant False
        PDJust blobData ->
          plet (pfromData blobData) $ \blob ->
            pand'List
              [ pbytesDataCborLengthV1 # pfromData (pbytes'bytesLength c)
                  #== pfromData (pbytes'sourceLength c)
              , Blob.pcontrolIsWellFormed # blob
              , pblobSourceStart blob #== 0
              , pblobSourceLength blob #== pfromData (pbytes'bytesLength c)
              , pif
                  (pfromData (pbytes'stage c) #== pstageBreak)
                  ( pcardanoDataBytesChunk #< pfromData (pbytes'bytesLength c)
                      #&& pblobStage blob #== Blob.pstageTerminal
                  )
                  $ pif
                    (pfromData (pbytes'stage c) #== pstageTerminal)
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
  forall (s :: S). Term s (PInteger :--> PInteger :--> PCekDataBytesControlV1)
pinitialControlV1 = phoistAcyclic $
  plam $ \sourceStart sourceLength ->
    plet
      ( pcon $
          PCekDataBytesControlV1
            { pbytes'version = pdata pversion
            , pbytes'stage = pdata pstageSyntax
            , pbytes'sourceStart = pdata sourceStart
            , pbytes'sourceLength = pdata sourceLength
            , pbytes'bytesLength = pdata 0
            , pbytes'blob = pdata (pcon PDNothing)
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

pencodeControlV1 :: forall (s :: S). Term s (PCekDataBytesControlV1 :--> PByteString)
pencodeControlV1 = phoistAcyclic $
  plam $ \control ->
    pif (pnot # (pcontrolIsWellFormed # control)) perror $
      pmatch control $ \c ->
        (pencodeDefiniteArrayHeader # 6)
          <> pcborInt pversion
          <> pcborInt (pfromData (pbytes'stage c))
          <> pcborInt (pfromData (pbytes'sourceStart c))
          <> pcborInt (pfromData (pbytes'sourceLength c))
          <> pcborInt (pfromData (pbytes'bytesLength c))
          <> (pencodeOptionalBlob # pfromData (pbytes'blob c))

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

pcontrolFromDataV1 :: forall (s :: S). Term s (PData :--> PCekDataBytesControlV1)
pcontrolFromDataV1 = phoistAcyclic $
  plam $ \d ->
    plet (pasList # d) $ \items ->
      pif (pnot # (plength # items #== 6)) perror $
        plet
          ( pcon $
              PCekDataBytesControlV1
                { pbytes'version = pdata (pasInt # (pelemAt # 0 # items))
                , pbytes'stage = pdata (pasInt # (pelemAt # 1 # items))
                , pbytes'sourceStart = pdata (pasInt # (pelemAt # 2 # items))
                , pbytes'sourceLength = pdata (pasInt # (pelemAt # 3 # items))
                , pbytes'bytesLength = pdata (pasInt # (pelemAt # 4 # items))
                , pbytes'blob = pdata (poptionalBlobFromData # (pelemAt # 5 # items))
                }
          )
          $ \control -> pif (pcontrolIsWellFormed # control) control perror

pdecodeControlV1 :: forall (s :: S). Term s (PByteString :--> PCekDataBytesControlV1)
pdecodeControlV1 = phoistAcyclic $
  plam $ \controlCbor ->
    pmatch (pdeserialise # controlCbor) $ \case
      PNothing -> perror
      PJust d ->
        plet (pcontrolFromDataV1 # d) $ \control ->
          pif (pencodeControlV1 # control #== controlCbor) control perror

pindefiniteRawStart ::
  forall (s :: S). Term s (PCekDataBytesControlV1 :--> PInteger :--> PInteger)
pindefiniteRawStart = phoistAcyclic $
  plam $ \control contentStart ->
    pmatch control $ \c ->
      pif
        (contentStart #== pfromData (pbytes'bytesLength c))
        (pfromData (pbytes'sourceLength c) - 1)
        $ plet (pquot # contentStart # pcardanoDataBytesChunk)
        $ \chunkIndex ->
          plet (chunkIndex * pcardanoDataBytesChunk) $ \chunkStart ->
            plet (contentStart - chunkStart) $ \withinChunk ->
              plet
                ( pminimum
                    # pcardanoDataBytesChunk
                    # (pfromData (pbytes'bytesLength c) - chunkStart)
                )
                $ \chunkLength ->
                  plet (1 + chunkIndex * 66) $ \headerStart ->
                    pif
                      (withinChunk #== 0)
                      headerStart
                      (headerStart + pdefiniteHeaderLength # chunkLength + withinChunk)

pmappedRawLength ::
  forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
pmappedRawLength = phoistAcyclic $
  pfix $ \self -> plam $ \bytesLength contentCursor remaining ->
    pif (remaining #== 0) 0 $
      plet
        (pquot # contentCursor # pcardanoDataBytesChunk * pcardanoDataBytesChunk)
        $ \chunkStart ->
          plet (contentCursor - chunkStart) $ \withinChunk ->
            plet
              (pminimum # pcardanoDataBytesChunk # (bytesLength - chunkStart))
              $ \chunkLength ->
                plet (pminimum # remaining # (chunkLength - withinChunk)) $ \takeBytes ->
                  plet
                    (pif (withinChunk #== 0) (pdefiniteHeaderLength # chunkLength) 0)
                    $ \headerLength ->
                      headerLength
                        + takeBytes
                        + self # bytesLength # (contentCursor + takeBytes) # (remaining - takeBytes)

pcontentPlan ::
  forall (s :: S). Term s (PCekDataBytesControlV1 :--> PMaybe PContentPlanV1)
pcontentPlan = phoistAcyclic $
  plam $ \control ->
    pmatch control $ \c ->
      pif (pnot # (pfromData (pbytes'stage c) #== pstageBlob)) (pcon PNothing) $
        pmatch (pfromData (pbytes'blob c)) $ \case
          PDNothing -> perror
          PDJust blobData ->
            pmatch (Blob.pnextSourceSpanV1 # pfromData blobData) $ \case
              PNothing -> pcon PNothing
              PJust virtualSpan ->
                pmatch virtualSpan $ \(Blob.PCekSourceBlobSpanV1 absoluteStart spanLength) ->
                  plet (pfromData absoluteStart) $ \contentStart ->
                    plet (pfromData spanLength) $ \contentLength ->
                      plet (contentStart + contentLength) $ \contentEnd ->
                        pif
                          (contentStart #< 0 #|| pfromData (pbytes'bytesLength c) #< contentEnd)
                          (pcon PNothing)
                          $ pif
                            (pfromData (pbytes'bytesLength c) #<= pcardanoDataBytesChunk)
                            ( pcon $
                                PJust $
                                  pcon $
                                    PContentPlanV1
                                      { pplan'span =
                                          pcon $
                                            Blob.PCekSourceBlobSpanV1
                                              { Blob.pspan'absoluteStart =
                                                  pdata $
                                                    pfromData (pbytes'sourceStart c)
                                                      + pdefiniteHeaderLength
                                                      # pfromData (pbytes'bytesLength c)
                                                      + contentStart
                                              , Blob.pspan'length = pdata contentLength
                                              }
                                      , pplan'contentStart = contentStart
                                      , pplan'contentLength = contentLength
                                      }
                            )
                            $ plet
                              ( pmappedRawLength
                                  # pfromData (pbytes'bytesLength c)
                                  # contentStart
                                  # contentLength
                              )
                            $ \rawLength ->
                              pif
                                (pmaximumSourceSpan #< rawLength)
                                (pcon PNothing)
                                ( pcon $
                                    PJust $
                                      pcon $
                                        PContentPlanV1
                                          { pplan'span =
                                              pcon $
                                                Blob.PCekSourceBlobSpanV1
                                                  { Blob.pspan'absoluteStart =
                                                      pdata $
                                                        pfromData (pbytes'sourceStart c)
                                                          + pindefiniteRawStart
                                                          # control
                                                          # contentStart
                                                  , Blob.pspan'length = pdata rawLength
                                                  }
                                          , pplan'contentStart = contentStart
                                          , pplan'contentLength = contentLength
                                          }
                                )

pextractIndefiniteContent ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PMaybe PByteString
    )
pextractIndefiniteContent = phoistAcyclic $
  pfix $ \self -> plam $ \sourceBytes rawCursor bytesLength contentCursor remaining ->
    pif
      (remaining #== 0)
      ( pif
          (rawCursor #== plengthBS # sourceBytes)
          (pcon (PJust (pconstant "")))
          (pcon PNothing)
      )
      $ plet
        (pquot # contentCursor # pcardanoDataBytesChunk * pcardanoDataBytesChunk)
      $ \chunkStart ->
        plet (contentCursor - chunkStart) $ \withinChunk ->
          plet (pminimum # pcardanoDataBytesChunk # (bytesLength - chunkStart)) $ \chunkLength ->
            plet (pminimum # remaining # (chunkLength - withinChunk)) $ \takeBytes ->
              plet
                (pif (withinChunk #== 0) (pdefiniteBytesHeader # chunkLength) (pconstant ""))
                $ \header ->
                  plet (plengthBS # header) $ \headerLength ->
                    pif
                      ( takeBytes #<= 0
                          #|| plengthBS # sourceBytes #< rawCursor + headerLength + takeBytes
                          #|| pnot # (psliceLen # sourceBytes # rawCursor # headerLength #== header)
                      )
                      (pcon PNothing)
                      $ plet
                        (psliceLen # sourceBytes # (rawCursor + headerLength) # takeBytes)
                      $ \content ->
                        pmatch
                          ( self
                              # sourceBytes
                              # (rawCursor + headerLength + takeBytes)
                              # bytesLength
                              # (contentCursor + takeBytes)
                              # (remaining - takeBytes)
                          )
                          $ \case
                            PNothing -> pcon PNothing
                            PJust rest -> pcon (PJust (content <> rest))

pextractContent ::
  forall (s :: S).
  Term s (PCekDataBytesControlV1 :--> PContentPlanV1 :--> PByteString :--> PMaybe PByteString)
pextractContent = phoistAcyclic $
  plam $ \control plan sourceBytes ->
    pmatch plan $ \(PContentPlanV1 span contentStart contentLength) ->
      pmatch span $ \(Blob.PCekSourceBlobSpanV1 _ spanLength) ->
        pif
          (pnot # (plengthBS # sourceBytes #== pfromData spanLength))
          (pcon PNothing)
          $ pmatch control
          $ \c ->
            pif
              (pfromData (pbytes'bytesLength c) #<= pcardanoDataBytesChunk)
              (pcon (PJust sourceBytes))
              ( pextractIndefiniteContent
                  # sourceBytes
                  # 0
                  # pfromData (pbytes'bytesLength c)
                  # contentStart
                  # contentLength
              )

pnextSourceSpanV1 ::
  forall (s :: S). Term s (PCekDataBytesControlV1 :--> PMaybe Blob.PCekSourceBlobSpanV1)
pnextSourceSpanV1 = phoistAcyclic $
  plam $ \control ->
    pif (pnot # (pcontrolIsWellFormed # control)) (pcon PNothing) $
      pmatch control $ \c ->
        pif
          (pfromData (pbytes'stage c) #== pstageSyntax)
          ( pcon $
              PJust $
                pcon $
                  Blob.PCekSourceBlobSpanV1
                    (pbytes'sourceStart c)
                    (pdata (pminimum # pfromData (pbytes'sourceLength c) # psyntaxBytes))
          )
          $ pif
            (pfromData (pbytes'stage c) #== pstageBreak)
            ( pcon $
                PJust $
                  pcon $
                    Blob.PCekSourceBlobSpanV1
                      (pdata (pfromData (pbytes'sourceStart c) + pfromData (pbytes'sourceLength c) - 1))
                      (pdata 1)
            )
            $ pmatch (pcontentPlan # control)
            $ \case
              PNothing -> pcon PNothing
              PJust plan -> pmatch plan $ \(PContentPlanV1 span _ _) -> pcon (PJust span)

pstepSyntax ::
  forall (s :: S).
  Term s PCekDataBytesControlV1 ->
  Term s (PMaybe PByteString) ->
  Term s (PMaybe PCekDataBytesControlV1)
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
                pmatch (pparseSyntaxV1 # bytes # pfromData (pbytes'sourceLength c)) $ \case
                  PNothing -> pcon PNothing
                  PJust bytesLength ->
                    plet
                      ( pcon
                          c
                            { pbytes'stage = pdata pstageBlob
                            , pbytes'bytesLength = pdata bytesLength
                            , pbytes'blob =
                                pdata $
                                  pcon $
                                    PDJust $
                                      pdata (Blob.pinitialControlV1 # 0 # bytesLength)
                            }
                      )
                      $ \next ->
                        pif (pcontrolIsWellFormed # next) (pcon (PJust next)) (pcon PNothing)

pstepBreak ::
  forall (s :: S).
  Term s PCekDataBytesControlV1 ->
  Term s (PMaybe PByteString) ->
  Term s (PMaybe PCekDataBytesControlV1)
pstepBreak control sourceBytes =
  pmatch sourceBytes $ \case
    PNothing -> pcon PNothing
    PJust bytes ->
      pif (bytes #== pconstant "\xff")
        ( pmatch control $ \c ->
            plet (pcon c {pbytes'stage = pdata pstageTerminal}) $ \next ->
              pif (pcontrolIsWellFormed # next) (pcon (PJust next)) (pcon PNothing)
        )
        (pcon PNothing)

pstepBlob ::
  forall (s :: S).
  Term s PCekDataBytesControlV1 ->
  Term s (PMaybe PByteString) ->
  Term s (PMaybe PCekDataBytesControlV1)
pstepBlob control sourceBytes =
  pmatch control $ \c ->
    pmatch (pfromData (pbytes'blob c)) $ \case
      PDNothing -> perror
      PDJust blobData ->
        plet (pfromData blobData) $ \blob ->
          pif
            (pblobStage blob #== Blob.pstageTerminal)
            ( pmatch sourceBytes $ \case
                PJust _ -> pcon PNothing
                PNothing ->
                  plet
                    ( pcon
                        c
                          { pbytes'stage =
                              pdata $
                                pif
                                  (pcardanoDataBytesChunk #< pfromData (pbytes'bytesLength c))
                                  pstageBreak
                                  pstageTerminal
                          }
                    )
                    $ \next ->
                      pif (pcontrolIsWellFormed # next) (pcon (PJust next)) (pcon PNothing)
            )
            $ pmatch (pcontentPlan # control)
            $ \case
              PNothing ->
                pmatch sourceBytes $ \case
                  PJust _ -> pcon PNothing
                  PNothing -> pupdateBlob c (Blob.pstepV1 # blob # pcon PNothing)
              PJust plan ->
                pmatch sourceBytes $ \case
                  PNothing -> pcon PNothing
                  PJust bytes ->
                    pmatch (pextractContent # control # plan # bytes) $ \case
                      PNothing -> pcon PNothing
                      PJust content -> pupdateBlob c (Blob.pstepV1 # blob # pcon (PJust content))

pupdateBlob ::
  forall (s :: S).
  PCekDataBytesControlV1 s ->
  Term s (PMaybe Blob.PCekSourceBlobControlV1) ->
  Term s (PMaybe PCekDataBytesControlV1)
pupdateBlob c result =
  pmatch result $ \case
    PNothing -> pcon PNothing
    PJust blob ->
      plet (pcon c {pbytes'blob = pdata (pcon (PDJust (pdata blob)))}) $ \next ->
        pif (pcontrolIsWellFormed # next) (pcon (PJust next)) (pcon PNothing)

pstepV1 ::
  forall (s :: S).
  Term
    s
    ( PCekDataBytesControlV1
        :--> PMaybe PByteString
        :--> PMaybe PCekDataBytesControlV1
    )
pstepV1 = phoistAcyclic $
  plam $ \control sourceBytes ->
    pif (pnot # (pcontrolIsWellFormed # control)) (pcon PNothing) $
      pmatch control $ \c ->
        pif
          (pfromData (pbytes'stage c) #== pstageSyntax)
          (pstepSyntax control sourceBytes)
          $ pif
            (pfromData (pbytes'stage c) #== pstageBlob)
            (pstepBlob control sourceBytes)
            $ pif
              (pfromData (pbytes'stage c) #== pstageBreak)
              (pstepBreak control sourceBytes)
              (pcon PNothing)

pfinalizeV1 :: forall (s :: S). Term s (PCekDataBytesControlV1 :--> PMaybe PDataSummaryV1)
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
        pmatch (pfromData (pbytes'blob c)) $ \case
          PDNothing -> perror
          PDJust blobData ->
            pmatch (Blob.pfinalizeV1 # pfromData blobData) $ \case
              PNothing -> pcon PNothing
              PJust bytesRoot ->
                plet
                  ( 4
                      + pif
                        (pfromData (pbytes'bytesLength c) #== 0)
                        1
                        (pfromData (pbytes'bytesLength c))
                  )
                  $ \memory ->
                    plet
                      ( pcon $
                          PBytesDataNode
                            { pnode'bytesRoot = pdata bytesRoot
                            , pnode'bytesLength = pbytes'bytesLength c
                            , pnode'cborLength = pbytes'sourceLength c
                            , pnode'memory = pdata memory
                            }
                      )
                      $ \node ->
                        pcon $
                          PJust $
                            pcon $
                              PDataSummaryV1
                                { psummary'root = pdata (phashDataNodeV1 # node)
                                , psummary'cborLength = pbytes'sourceLength c
                                , psummary'memory = pdata memory
                                }

pcontrolStage :: forall (s :: S). Term s PCekDataBytesControlV1 -> Term s PInteger
pcontrolStage control = pmatch control $ \c -> pfromData (pbytes'stage c)
