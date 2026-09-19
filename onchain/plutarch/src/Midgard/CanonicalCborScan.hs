{- |
Module      : Midgard.CanonicalCborScan
Description : Plutarch port of @lib/midgard/canonical-cbor-scan-v1.ak@.

Two total readers over canonical CBOR: one head, and one definite byte string.

This is the smallest of the port's three copies of the same head arithmetic, and
the duplication is the Aiken tree's rather than the port's. The other two are
'Midgard.FraudProofs.NativeTx.Codec', which /aborts/ on malformed input because
its callers have already established the bytes are well formed, and
'Midgard.CanonicalPlutusData', which walks a whole @Data@ grammar. This one sits
between them: total like the second, but reading one head at a time for a caller
that knows which major type it expects next.

=== Fail closed

Indefinite forms, reserved additional-info values, and non-minimal length
encodings all return 'PNothing'. Minimality is the property that matters, and for
the usual reason: Midgard hashes these bytes, so a length written in a wider form
than it needs would give one logical value several commitments.

=== The major type is an argument, not a discovery

'pheadAtV1' takes the major type its caller expects and refuses anything else.
That is what makes it usable as a scanner: a caller walking a known structure
states what should come next and gets nothing back if the bytes disagree, rather
than having to inspect a returned tag.
-}
module Midgard.CanonicalCborScan (
  PCborHeadV1 (..),
  PCborBytesV1 (..),
  pheadAtV1,
  pbytesAtV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.ByteString (pbyteStringToInteger, pmostSignificantFirst)
import Plutarch.Core.Internal.Builtins (pindexBS')
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottRec (..))

import Midgard.FraudProofs.NativeTx.Codec (psliceLen)

{- | Aiken @CborHeadV1@ — where the head ended, and what it said.

Scott-encoded rather than @Constr@, which is the exception the port allows
itself: these two records are function results that never reach a datum or a
redeemer, in Aiken or here, so nothing observes their encoding. Should a consumer
ever put one on the wire, this has to become 'DeriveAsDataRec' — Aiken would have
been emitting a @Constr@ all along.
-}
data PCborHeadV1 (s :: S) = PCborHeadV1
  { pcborHead'nextOffset :: Term s PInteger
  , pcborHead'value :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PCborHeadV1)

-- | Aiken @CborBytesV1@ — where the payload ended, and the payload.
data PCborBytesV1 (s :: S) = PCborBytesV1
  { pcborBytes'nextOffset :: Term s PInteger
  , pcborBytes'value :: Term s PByteString
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PCborBytesV1)

-- | Aiken @byte_at_if_present@.
pbyteAtIfPresent ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PMaybe PInteger)
pbyteAtIfPresent = phoistAcyclic $
  plam $ \bytes offset ->
    pif
      (offset #>= 0 #&& offset #< plengthBS # bytes)
      (pcon (PJust (pindexBS' # bytes # offset)))
      (pcon PNothing)

{- | Aiken @read_big_endian@.

Aiken accumulates one byte at a time; this reads the slice and converts it, which
is the same value for every width the grammar uses. The bounds check is what
keeps the two agreeing on failure: 'psliceBS' clamps a short read rather than
failing, so a truncated head would otherwise decode to a shorter number instead
of to nothing.
-}
preadBigEndian ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PInteger :--> PMaybe PInteger)
preadBigEndian = phoistAcyclic $
  plam $ \bytes offset width ->
    pif
      (offset #>= 0 #&& offset + width #<= plengthBS # bytes)
      ( pcon
          ( PJust
              (pbyteStringToInteger # pmostSignificantFirst # (psliceBS # offset # width # bytes))
          )
      )
      (pcon PNothing)

{- | Aiken @head_at_v1@.

One definite, shortest-form CBOR argument for the requested major type. Each
wider form must carry a value the narrower one could not have held, which is what
makes the encoding injective.
-}
pheadAtV1 ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PInteger :--> PMaybe PCborHeadV1)
pheadAtV1 = phoistAcyclic $
  plam $ \bytes offset expectedMajor ->
    pmatch (pbyteAtIfPresent # bytes # offset) $ \case
      PNothing -> pcon PNothing
      PJust first ->
        pif (pnot #$ pdiv # first # 32 #== expectedMajor) (pcon PNothing) $
          plet (pmod # first # 32) $ \additional ->
            pif (additional #< 24) (phead' (offset + 1) additional) $
              pif (additional #== 24) (pminimal bytes (offset + 1) 1 24) $
                pif (additional #== 25) (pminimal bytes (offset + 1) 2 256) $
                  pif (additional #== 26) (pminimal bytes (offset + 1) 4 65536) $
                    pif
                      (additional #== 27)
                      (pminimal bytes (offset + 1) 8 4294967296)
                      (pcon PNothing)
  where
    phead' next value = pcon (PJust (pcon (PCborHeadV1 next value)))
    -- A wider form is minimal only if its value could not have been written narrower.
    pminimal bytes at width floor' =
      pmatch (preadBigEndian # bytes # at # width) $ \case
        PNothing -> pcon PNothing
        PJust value ->
          pif (value #>= floor') (phead' (at + width) value) (pcon PNothing)

{- | Aiken @bytes_at_v1@ — a definite byte string, head and payload.

The payload has to be inside the input; a length that runs past the end is a
malformed encoding rather than a short read.
-}
pbytesAtV1 ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PMaybe PCborBytesV1)
pbytesAtV1 = phoistAcyclic $
  plam $ \bytes offset ->
    pmatch (pheadAtV1 # bytes # offset # 2) $ \case
      PNothing -> pcon PNothing
      PJust head' -> pmatch head' $ \(PCborHeadV1 nextOffset len) ->
        pif
          (nextOffset + len #<= plengthBS # bytes)
          ( pcon
              ( PJust
                  ( pcon
                      ( PCborBytesV1
                          (nextOffset + len)
                          (psliceLen # bytes # nextOffset # len)
                      )
                  )
              )
          )
          (pcon PNothing)
