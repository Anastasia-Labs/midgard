{- |
Module      : Midgard.CanonicalPlutusData
Description : Plutarch port of @lib/midgard/canonical-plutus-data-v1.ak@.

The datum/redeemer canonicity predicate — @docs/spec/midgard-tx.md@ §6.2.

Canonicity here is __membership in the image of the Plutus @serialiseData@
builtin__: exactly the byte forms that builtin emits and cardano-ledger's
@decodeData@ accepts. The module decides that membership by walking the bytes,
rather than by round-tripping through a decoder — which is what the Aiken module
replaced, and for a reason worth keeping in view. Aiken stdlib v3.1.0's decoder
is /narrower/ than @serialiseData@'s image in two places, and a round-trip pin
inherited both gaps: canonical tag-2/3 bignums, and tag-102 constructors for
alternatives at or above 128. Both are forms L1 accepts, so rejecting them cost
Midgard L1 parity — a datum Cardano would take could not be spent on Midgard.

Everything the retired predicate enforced still holds: minimal integer heads
below @2^64@; definite byte strings of at most 64 bytes, with indefinite 64-byte
chunking above; empty lists exactly @80@ and non-empty lists indefinite
(@9f … ff@); definite-length maps; constructor tags @d8 79+alt@ (0–6),
@d9 0500+alt-7@ (7–127) and tag 102 (at or above 128); exactly one item consuming
exactly the declared bytes; and no text, simple, float or other tags anywhere.

=== Every function here is total

Malformed input yields 'PNothing' rather than an abort, so the predicate can gate
untrusted bytes. That is why this module does not reach for
"Midgard.FraudProofs.NativeTx.Codec"'s readers, which are the same arithmetic
with the opposite failure mode: those abort, and a predicate that aborted on the
input it exists to reject would be useless.

=== Two predicates, two questions

'pisCanonicalPlutusDataV1' asks whether the bytes are in @serialiseData@'s image.
'pisMaterialisablePlutusDataV1' asks the narrower §11.2 question: whether every
construct inside them is one the Aiken-stdlib @deserialise@ path can turn back
into @Data@. Declining to materialise is not declaring non-canonical, and the two
must keep answering differently — the flag is threaded through the whole scan
rather than read off the head byte, so a bignum nested at any depth clears it.
-}
module Midgard.CanonicalPlutusData (
  -- * The grammar's constants
  pdataBytesChunk,
  psmallConstrTagBase,
  pmaxSmallConstrAlternative,
  plargeConstrTagBase,
  pmaxLargeConstrAlternative,
  pgenericConstrTag,
  pminGenericConstrAlternative,
  ppositiveBignumTag,
  pnegativeBignumTag,
  pminBignumMagnitudeBytes,

  -- * The predicates
  pisCanonicalPlutusDataV1,
  pisMaterialisablePlutusDataV1,
  pcanonicalDataEndAtV1,

  -- * The scan
  PScanData,
  pscanData,
  pscanHead,
  pbyteAtM,
  pscanByteString,
  pscanDefiniteBytes,
  pscanByteChunks,
  pscanBignumMagnitude,
) where

import Data.Kind (Type)

import Plutarch.Builtin.ByteString (pbyteStringToInteger, pmostSignificantFirst)
import Plutarch.Core.Internal.Builtins (pindexBS')
import Plutarch.Prelude

--------------------------------------------------------------------------------
-- The grammar's constants
--------------------------------------------------------------------------------

{- | @serialiseData@ emits byte strings definite up to this length and switches
to indefinite chunking of exactly this size above it.
-}
pdataBytesChunk :: forall (s :: S). Term s PInteger
pdataBytesChunk = 64

-- | Constructor alternatives 0–6 use tags 121–127.
psmallConstrTagBase :: forall (s :: S). Term s PInteger
psmallConstrTagBase = 121

pmaxSmallConstrAlternative :: forall (s :: S). Term s PInteger
pmaxSmallConstrAlternative = 6

-- | Alternatives 7–127 use tags 1280–1400.
plargeConstrTagBase :: forall (s :: S). Term s PInteger
plargeConstrTagBase = 1280

pmaxLargeConstrAlternative :: forall (s :: S). Term s PInteger
pmaxLargeConstrAlternative = 127

-- | Alternatives at or above 128 use tag 102 with an explicit alternative field.
pgenericConstrTag :: forall (s :: S). Term s PInteger
pgenericConstrTag = 102

pminGenericConstrAlternative :: forall (s :: S). Term s PInteger
pminGenericConstrAlternative = 128

-- | Tag 2 is a positive bignum, tag 3 a negative one.
ppositiveBignumTag :: forall (s :: S). Term s PInteger
ppositiveBignumTag = 2

pnegativeBignumTag :: forall (s :: S). Term s PInteger
pnegativeBignumTag = 3

{- | @|i| >= 2^64@ needs at least nine magnitude bytes; anything smaller has a
shorter spelling as a plain major-0/1 integer, so a shorter magnitude is
non-minimal.
-}
pminBignumMagnitudeBytes :: forall (s :: S). Term s PInteger
pminBignumMagnitudeBytes = 9

pbreakByte :: forall (s :: S). Term s PInteger
pbreakByte = 0xff

pindefiniteAdditionalInfo :: forall (s :: S). Term s PInteger
pindefiniteAdditionalInfo = 31

--------------------------------------------------------------------------------
-- The predicates
--------------------------------------------------------------------------------

{- | Aiken @is_canonical_plutus_data_v1@.

True when the bytes are exactly one canonical Plutus @Data@ item — one
@serialiseData@ could have produced — with nothing trailing it.
-}
pisCanonicalPlutusDataV1 :: forall (s :: S). Term s (PByteString :--> PBool)
pisCanonicalPlutusDataV1 = phoistAcyclic $
  plam $ \cborBytes ->
    pmatch (pscanData # cborBytes # 0) $ \case
      PNothing -> pconstant False
      PJust scanned -> pmatch scanned $ \(PPair offset _materialisable) ->
        offset #== plengthBS # cborBytes

{- | Aiken @is_materialisable_plutus_data_v1@ — canonical /and/ §11.2 decodable.

Two canonical forms are not, and neither is reachable through the stdlib
@deserialise@: tag-2/3 bignums, whose major-6 arm computes a negative
constructor alternative and so /aborts/ the machine on real L1, and tag-102
constructors, which that decoder declines outright. A caller has to screen with
this before asking the decoder, because the first of the two is an abort rather
than a decline.
-}
pisMaterialisablePlutusDataV1 :: forall (s :: S). Term s (PByteString :--> PBool)
pisMaterialisablePlutusDataV1 = phoistAcyclic $
  plam $ \cborBytes ->
    pmatch (pscanData # cborBytes # 0) $ \case
      PNothing -> pconstant False
      PJust scanned -> pmatch scanned $ \(PPair offset materialisable) ->
        offset #== plengthBS # cborBytes #&& materialisable

{- | Aiken @canonical_data_end_at_v1@ — the offset just past one canonical item.

The interior-access primitive of §11.2: navigating to a datum's @k@-th child
means skipping its earlier siblings, and skipping a sibling is exactly this.
Exposed rather than re-derived so that the acceptor and the predicate keep one
grammar and one verdict.
-}
pcanonicalDataEndAtV1 ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PMaybe PInteger)
pcanonicalDataEndAtV1 = phoistAcyclic $
  plam $ \bytes offset ->
    pmatch (pscanData # bytes # offset) $ \case
      PNothing -> pcon PNothing
      PJust scanned -> pmatch scanned $ \(PPair next _materialisable) ->
        pcon (PJust next)

--------------------------------------------------------------------------------
-- Byte-level primitives
--------------------------------------------------------------------------------

-- | Aiken @byte_at@ — the byte at an offset, or nothing if it is out of range.
pbyteAtM :: forall (s :: S). Term s (PByteString :--> PInteger :--> PMaybe PInteger)
pbyteAtM = phoistAcyclic $
  plam $ \bytes offset ->
    pif
      (offset #< 0 #|| offset #>= plengthBS # bytes)
      (pcon PNothing)
      (pcon (PJust (pindexBS' # bytes # offset)))

-- | Aiken @major_of@.
pmajorOf :: forall (s :: S). Term s PInteger -> Term s PInteger
pmajorOf first = pdiv # first # 32

-- | The additional-information nibble: Aiken's @first % 32@.
padditionalOf :: forall (s :: S). Term s PInteger -> Term s PInteger
padditionalOf first = pmod # first # 32

{- | Aiken @read_big_endian@.

A big-endian unsigned integer of @width@ bytes, or nothing if the bytes run out.
Aiken spells this as nested @* 256 +@ recursion; the slice-and-convert below is
the same value for every width the grammar uses, and the explicit bounds check is
what keeps it total — 'psliceBS' clamps a short read rather than failing, so
without it a truncated head would decode to a shorter number instead of nothing.
-}
preadBigEndian ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PInteger :--> PMaybe PInteger)
preadBigEndian = phoistAcyclic $
  plam $ \bytes offset width ->
    pif
      (offset #< 0 #|| offset + width #> plengthBS # bytes)
      (pcon PNothing)
      ( pcon
          ( PJust
              (pbyteStringToInteger # pmostSignificantFirst # (psliceBS # offset # width # bytes))
          )
      )

{- | Aiken @scan_head@ — one CBOR head, as @(next offset, argument)@.

Only minimal widths are accepted, so each wider form must carry a value the
narrower one could not have held. Additional-info values 28–31 always fail here;
the indefinite forms are handled by their own callers, which look for the
@5f@/@9f@ prefix byte before reaching this.
-}
pscanHead ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PMaybe (PPair PInteger PInteger))
pscanHead = phoistAcyclic $
  plam $ \bytes offset ->
    pmatch (pbyteAtM # bytes # offset) $ \case
      PNothing -> pcon PNothing
      PJust first ->
        plet (padditionalOf first) $ \additional ->
          pif (additional #<= 23) (pjustPair (offset + 1) additional) $
            pif
              (additional #== 24)
              ( pmatch (pbyteAtM # bytes # (offset + 1)) $ \case
                  PNothing -> pcon PNothing
                  PJust value ->
                    pif (value #>= 24) (pjustPair (offset + 2) value) (pcon PNothing)
              )
              $ pif
                (additional #== 25)
                (pwider bytes (offset + 1) 2 0xff (offset + 3))
                $ pif
                  (additional #== 26)
                  (pwider bytes (offset + 1) 4 0xffff (offset + 5))
                  $ pif
                    (additional #== 27)
                    (pwider bytes (offset + 1) 8 0xffffffff (offset + 9))
                    (pcon PNothing)
  where
    -- A wider head is minimal only if its value exceeds the narrower form's ceiling.
    pwider bytes at width ceiling' next =
      pmatch (preadBigEndian # bytes # at # width) $ \case
        PNothing -> pcon PNothing
        PJust value -> pif (value #> ceiling') (pjustPair next value) (pcon PNothing)

pjustPair ::
  forall (s :: S) (a :: S -> Type) (b :: S -> Type).
  Term s a ->
  Term s b ->
  Term s (PMaybe (PPair a b))
pjustPair x y = pcon (PJust (pcon (PPair x y)))

--------------------------------------------------------------------------------
-- The scan
--------------------------------------------------------------------------------

{- | The recursive scan's own type.

@(offset just past the item, materialisable)@. The second component is the §11.2
flag, cleared as soon as any construct the stdlib @deserialise@ path cannot take
is passed, at any depth. Structure and materialisability are decided in one pass
because they read the same tags, and a caller that needs both should not pay for
two walks.
-}
type PScanData = PByteString :--> PInteger :--> PMaybe (PPair PInteger PBool)

{- | Aiken @scan_data@.

The five majors Plutus @Data@ can wear. Major 3 (text) and major 7
(simple/float) are outside it, and integers at or above @2^64@ arrive as major-6
tags rather than here.
-}
pscanData :: forall (s :: S). Term s PScanData
pscanData = phoistAcyclic $
  pfix $ \self -> plam $ \bytes offset ->
    pmatch (pbyteAtM # bytes # offset) $ \case
      PNothing -> pcon PNothing
      PJust first ->
        plet (pmajorOf first) $ \major ->
          pif
            (major #== 0 #|| major #== 1)
            ( pmatch (pscanHead # bytes # offset) $ \case
                PNothing -> pcon PNothing
                PJust scanned -> pmatch scanned $ \(PPair next _value) ->
                  pjustPair next (pconstant True)
            )
            $ pif (major #== 2) (pliftLeaf (pscanByteString # bytes # offset) (pconstant True))
              $ pif (major #== 4) (pscanList # self # bytes # offset)
                $ pif (major #== 5) (pscanMap # self # bytes # offset)
                  $ pif (major #== 6) (pscanTagged # self # bytes # offset) (pcon PNothing)

{- | Aiken @lift@.

Lifts a leaf scan — one with no @Data@ inside it, so nothing that could carry the
§11.2 flag down — into the flagged form, with the flag the leaf deserves stated
rather than set and then corrected.
-}
pliftLeaf ::
  forall (s :: S).
  Term s (PMaybe PInteger) ->
  Term s PBool ->
  Term s (PMaybe (PPair PInteger PBool))
pliftLeaf scanned materialisable =
  pmatch scanned $ \case
    PNothing -> pcon PNothing
    PJust next -> pjustPair next materialisable

{- | Aiken @unmaterialisable@.

Clears the §11.2 flag while preserving the structural verdict: these bytes are
canonical, and they are not materialisable.
-}
punmaterialisable ::
  forall (s :: S).
  Term s (PMaybe (PPair PInteger PBool)) ->
  Term s (PMaybe (PPair PInteger PBool))
punmaterialisable scanned =
  pmatch scanned $ \case
    PNothing -> pcon PNothing
    PJust p -> pmatch p $ \(PPair next _) -> pjustPair next (pconstant False)

--------------------------------------------------------------------------------
-- Byte strings
--------------------------------------------------------------------------------

-- | Aiken @scan_byte_string@.
pscanByteString ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PMaybe PInteger)
pscanByteString = phoistAcyclic $
  plam $ \bytes offset ->
    pmatch (pbyteAtM # bytes # offset) $ \case
      PNothing -> pcon PNothing
      PJust first ->
        pif
          (padditionalOf first #== pindefiniteAdditionalInfo)
          (pscanByteChunks # bytes # (offset + 1) # 0)
          ( pmatch (pscanDefiniteBytes # bytes # offset) $ \case
              PNothing -> pcon PNothing
              PJust scanned -> pmatch scanned $ \(PPair next len) ->
                -- Above the chunk size `serialiseData` always chunks, so a long
                -- definite byte string is not in its image.
                pif (len #<= pdataBytesChunk) (pcon (PJust next)) (pcon PNothing)
          )

{- | Aiken @scan_definite_bytes@ — a definite head plus payload, as
@(offset past the payload, payload length)@.
-}
pscanDefiniteBytes ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PMaybe (PPair PInteger PInteger))
pscanDefiniteBytes = phoistAcyclic $
  plam $ \bytes offset ->
    pmatch (pbyteAtM # bytes # offset) $ \case
      PNothing -> pcon PNothing
      PJust first ->
        pif (pnot #$ pmajorOf first #== 2) (pcon PNothing) $
          pmatch (pscanHead # bytes # offset) $ \case
            PNothing -> pcon PNothing
            PJust scanned -> pmatch scanned $ \(PPair next len) ->
              pif
                (next + len #<= plengthBS # bytes)
                (pjustPair (next + len) len)
                (pcon PNothing)

{- | Aiken @scan_byte_chunks@.

The indefinite byte-string form: every chunk is exactly 'pdataBytesChunk' bytes
except the final one, which is 1–64, and there are at least two chunks — a single
chunk would have been spelled definite.
-}
pscanByteChunks ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PInteger :--> PMaybe PInteger)
pscanByteChunks = phoistAcyclic $
  pfix $ \self -> plam $ \bytes offset seen ->
    pmatch (pbyteAtM # bytes # offset) $ \case
      PNothing -> pcon PNothing
      PJust first ->
        pif (first #== pbreakByte) (pif (seen #>= 2) (pcon (PJust (offset + 1))) (pcon PNothing)) $
          pmatch (pscanDefiniteBytes # bytes # offset) $ \case
            PNothing -> pcon PNothing
            PJust scanned -> pmatch scanned $ \(PPair next len) ->
              pif (len #== pdataBytesChunk) (self # bytes # next # (seen + 1)) $
                pif
                  (len #> 0 #&& len #< pdataBytesChunk)
                  -- A short chunk can only be the last one.
                  ( pmatch (pbyteAtM # bytes # next) $ \case
                      PNothing -> pcon PNothing
                      PJust terminator ->
                        pif
                          (terminator #== pbreakByte #&& seen #>= 1)
                          (pcon (PJust (next + 1)))
                          (pcon PNothing)
                  )
                  (pcon PNothing)

--------------------------------------------------------------------------------
-- Lists and maps
--------------------------------------------------------------------------------

{- | Aiken @scan_list@.

Lists and constructor argument lists share one rule: empty is exactly @80@,
non-empty is indefinite @9f … ff@.
-}
pscanList :: forall (s :: S). Term s (PScanData :--> PScanData)
pscanList = phoistAcyclic $
  plam $ \scanData bytes offset ->
    pmatch (pbyteAtM # bytes # offset) $ \case
      PNothing -> pcon PNothing
      PJust first ->
        pif (first #== 0x80) (pjustPair (offset + 1) (pconstant True)) $
          pif
            (first #== 0x9f)
            (pscanListItems # scanData # bytes # (offset + 1) # 0 # pconstant True)
            (pcon PNothing)

pscanListItems ::
  forall (s :: S).
  Term
    s
    ( PScanData
        :--> PByteString
        :--> PInteger
        :--> PInteger
        :--> PBool
        :--> PMaybe (PPair PInteger PBool)
    )
pscanListItems = phoistAcyclic $
  pfix $ \self -> plam $ \scanData bytes offset seen ok ->
    pmatch (pbyteAtM # bytes # offset) $ \case
      PNothing -> pcon PNothing
      PJust first ->
        pif
          (first #== pbreakByte)
          -- An empty indefinite list would be `9f ff`; `80` is the only spelling.
          (pif (seen #>= 1) (pjustPair (offset + 1) ok) (pcon PNothing))
          ( pmatch (scanData # bytes # offset) $ \case
              PNothing -> pcon PNothing
              PJust scanned -> pmatch scanned $ \(PPair next itemOk) ->
                self # scanData # bytes # next # (seen + 1) # (ok #&& itemOk)
          )

-- | Aiken @scan_map@ — definite-length, with entry order preserved as authored.
pscanMap :: forall (s :: S). Term s (PScanData :--> PScanData)
pscanMap = phoistAcyclic $
  plam $ \scanData bytes offset ->
    pmatch (pscanHead # bytes # offset) $ \case
      PNothing -> pcon PNothing
      PJust scanned -> pmatch scanned $ \(PPair next entryCount) ->
        pscanMapEntries # scanData # bytes # next # entryCount # pconstant True

pscanMapEntries ::
  forall (s :: S).
  Term
    s
    ( PScanData
        :--> PByteString
        :--> PInteger
        :--> PInteger
        :--> PBool
        :--> PMaybe (PPair PInteger PBool)
    )
pscanMapEntries = phoistAcyclic $
  pfix $ \self -> plam $ \scanData bytes offset remaining ok ->
    pif (remaining #<= 0) (pjustPair offset ok) $
      pmatch (scanData # bytes # offset) $ \case
        PNothing -> pcon PNothing
        PJust key -> pmatch key $ \(PPair afterKey keyOk) ->
          pmatch (scanData # bytes # afterKey) $ \case
            PNothing -> pcon PNothing
            PJust value -> pmatch value $ \(PPair afterValue valueOk) ->
              self # scanData # bytes # afterValue # (remaining - 1) # (ok #&& keyOk #&& valueOk)

--------------------------------------------------------------------------------
-- Tags
--------------------------------------------------------------------------------

{- | Aiken @scan_tagged@ — constructors and bignums, the only tags §6.2 admits.

The two that clear the §11.2 flag are here, and they clear it for different
reasons: tag 102 is /declined/ by the stdlib decoder, tag 2 and tag 3 make it
/abort/. Both are canonical.
-}
pscanTagged :: forall (s :: S). Term s (PScanData :--> PScanData)
pscanTagged = phoistAcyclic $
  plam $ \scanData bytes offset ->
    pmatch (pbyteAtM # bytes # offset) $ \case
      PNothing -> pcon PNothing
      PJust first ->
        pif (padditionalOf first #== pindefiniteAdditionalInfo) (pcon PNothing) $
          pmatch (pscanHead # bytes # offset) $ \case
            PNothing -> pcon PNothing
            PJust scanned -> pmatch scanned $ \(PPair next tag) ->
              pif
                ( tag
                    #>= psmallConstrTagBase
                    #&& tag
                    #<= psmallConstrTagBase + pmaxSmallConstrAlternative
                )
                (pscanList # scanData # bytes # next)
                $ pif
                  ( tag
                      #>= plargeConstrTagBase
                      #&& tag
                      #<= plargeConstrTagBase + pmaxLargeConstrAlternative - 7
                  )
                  (pscanList # scanData # bytes # next)
                  $ pif
                    (tag #== pgenericConstrTag)
                    (punmaterialisable (pscanGenericConstr # scanData # bytes # next))
                    $ pif
                      (tag #== ppositiveBignumTag #|| tag #== pnegativeBignumTag)
                      (pliftLeaf (pscanBignumMagnitude # bytes # next) (pconstant False))
                      (pcon PNothing)

{- | Aiken @scan_generic_constr@.

§6.2: @d8 66 82 ‖ uint(alternative) ‖ args-list@, with a minimal alternative that
must be at least 128 — below that the tag-121 and tag-1280 spellings are the
canonical ones.
-}
pscanGenericConstr :: forall (s :: S). Term s (PScanData :--> PScanData)
pscanGenericConstr = phoistAcyclic $
  plam $ \scanData bytes offset ->
    pmatch (pbyteAtM # bytes # offset) $ \case
      PNothing -> pcon PNothing
      PJust first ->
        pif (pnot #$ first #== 0x82) (pcon PNothing) $
          pmatch (pbyteAtM # bytes # (offset + 1)) $ \case
            PNothing -> pcon PNothing
            PJust alternativeHead ->
              pif (pnot #$ pmajorOf alternativeHead #== 0) (pcon PNothing) $
                pmatch (pscanHead # bytes # (offset + 1)) $ \case
                  PNothing -> pcon PNothing
                  PJust scanned -> pmatch scanned $ \(PPair next alternative) ->
                    pif
                      (alternative #>= pminGenericConstrAlternative)
                      (pscanList # scanData # bytes # next)
                      (pcon PNothing)

{- | Aiken @scan_bignum_magnitude@.

§6.2: a canonical magnitude is a byte string of at least nine bytes with no
leading zero, chunked at 64 bytes like any other byte string.
-}
pscanBignumMagnitude ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PMaybe PInteger)
pscanBignumMagnitude = phoistAcyclic $
  plam $ \bytes offset ->
    pmatch (pbyteAtM # bytes # offset) $ \case
      PNothing -> pcon PNothing
      PJust first ->
        pif (pnot #$ pmajorOf first #== 2) (pcon PNothing) $
          pif
            (padditionalOf first #== pindefiniteAdditionalInfo)
            -- Chunked: the magnitude is longer than 64 bytes, so the length
            -- floor is met by construction; only the leading byte still needs
            -- checking.
            ( pmatch (pscanDefiniteBytes # bytes # (offset + 1)) $ \case
                PNothing -> pcon PNothing
                PJust _firstChunk ->
                  pmatch (pfirstChunkLeadingByte # bytes # (offset + 1)) $ \case
                    PNothing -> pcon PNothing
                    PJust leading ->
                      pif
                        (leading #== 0)
                        (pcon PNothing)
                        (pscanByteChunks # bytes # (offset + 1) # 0)
            )
            ( pmatch (pscanHead # bytes # offset) $ \case
                PNothing -> pcon PNothing
                PJust scanned -> pmatch scanned $ \(PPair next len) ->
                  pif
                    ( len
                        #< pminBignumMagnitudeBytes
                        #|| len
                        #> pdataBytesChunk
                        #|| next + len
                        #> plengthBS # bytes
                    )
                    (pcon PNothing)
                    $ pmatch (pbyteAtM # bytes # next) $ \case
                      PNothing -> pcon PNothing
                      PJust leading ->
                        pif (leading #== 0) (pcon PNothing) (pcon (PJust (next + len)))
            )

-- | Aiken @first_chunk_leading_byte@.
pfirstChunkLeadingByte ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PMaybe PInteger)
pfirstChunkLeadingByte = phoistAcyclic $
  plam $ \bytes offset ->
    pmatch (pscanHead # bytes # offset) $ \case
      PNothing -> pcon PNothing
      PJust scanned -> pmatch scanned $ \(PPair next _len) -> pbyteAtM # bytes # next
