{- |
Module      : Midgard.IntraItemBytes
Description : Plutarch port of @lib/midgard/intra-item-bytes-v1.ak@.

The byte primitives §11's intra-item mechanisms share.

§5.5's value map, §11.2's @Data@ and §11.3's native script all sit /inside/ an
item, below the §5.1 envelope the §8.8 door reads. They are three grammars, but
they are spelled in one CBOR: a definite head of a known major type, minimal
width, followed by bytes at an offset. §6.1 asks that one logical number have one
spelling, and two readers of the same head are two chances for that to stop being
true — so there is one reader here, and the mechanisms differ only in what they
read /with/ it.

=== Abort, never clamp

Every function fails closed on an out-of-range argument rather than returning a
clamped or truncated answer (§7.3). That matters more than it looks: two clamped
reads of different ranges can be byte-equal, and equality evidence fabricated
that way is indistinguishable from the real thing. 'psliceBS' clamps, so the
bound is checked before the read rather than inferred from it.

=== The @Option@-returning twin

"Midgard.CanonicalCborScan" answers the same question for a caller that still has
to /decide/ whether bytes are canonical. These are for a caller that has already
decided and is now reading, where a violation is not a verdict but a fault. That
is the whole difference between the two modules, and it is why the port carries
both rather than collapsing them.
-}
module Midgard.IntraItemBytes (
  pheadAt,
  pbyteIn,
  psliceExact,
) where

import Plutarch.Builtin.ByteString (pbyteStringToInteger, pmostSignificantFirst)
import Plutarch.Core.Internal.Builtins (pindexBS')
import Plutarch.Prelude

{- | Aiken @intra_item_bytes_v1.head_at@.

The one minimal-width definite-head reader for §11's interior grammars, as
@(offset just past the head, argument)@.

@expected_major@ is pinned by the call site, so a map head cannot be read where a
byte string belongs, and every width carries its own minimality bound, so one
logical number has one spelling (§6.1). All five definite widths are admitted
because the numbers these grammars carry genuinely reach the eight-byte form — a
lovelace amount, an asset quantity, a timelock slot. The narrow things — asset
name lengths, group counts, arities, child counts — are bounded by their own call
sites rather than here.
-}
pheadAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PInteger :--> PPair PInteger PInteger)
pheadAt = phoistAcyclic $
  plam $ \bytes offset expectedMajor ->
    plet (pbyteIn # bytes # offset) $ \first ->
      pif (pdiv # first # 32 #== expectedMajor) `flip` perror $
        plet (pmod # first # 32) $ \additional ->
          pif (additional #<= 23) (pcon (PPair (offset + 1) additional)) $
            pif
              (additional #== 24)
              ( plet (pbyteIn # bytes # (offset + 1)) $ \value ->
                  pif (value #>= 24) (pcon (PPair (offset + 2) value)) perror
              )
              $ pif (additional #== 25) (pminimal bytes (offset + 1) 2 0xff (offset + 3))
                $ pif (additional #== 26) (pminimal bytes (offset + 1) 4 0xffff (offset + 5))
                  $ pif
                    (additional #== 27)
                    (pminimal bytes (offset + 1) 8 0xffffffff (offset + 9))
                    perror
  where
    -- A wider form is minimal only if its value could not have been written
    -- narrower. `psliceExact` is what makes the read itself fail closed.
    pminimal bytes at width ceiling' next =
      plet
        ( pbyteStringToInteger
            # pmostSignificantFirst
            #$ psliceExact
            # bytes
            # at
            # width
        )
        $ \value -> pif (value #> ceiling') (pcon (PPair next value)) perror

{- | Aiken @intra_item_bytes_v1.byte_in@.

@bytearray.at@ is fail-closed on an out-of-range index in Aiken, but the failure
is a machine error rather than a stated bound. Stating it keeps the refusal
attributable — and in Plutarch it is not optional, because the underlying
builtin's behaviour outside the string is not something to lean on.
-}
pbyteIn :: forall (s :: S). Term s (PByteString :--> PInteger :--> PInteger)
pbyteIn = phoistAcyclic $
  plam $ \bytes offset ->
    pif
      (0 #<= offset #&& offset #< plengthBS # bytes)
      (pindexBS' # bytes # offset)
      perror

{- | Aiken @intra_item_bytes_v1.slice_exact@.

The only slice §11 takes. §7.3 abort-never-clamp: 'psliceBS' clamps, and two
clamped reads of different ranges can be byte-equal, so the range is checked
against the input's length before anything is read.
-}
psliceExact ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PInteger :--> PByteString)
psliceExact = phoistAcyclic $
  plam $ \bytes offset len ->
    pif
      (0 #<= offset #&& 0 #<= len #&& offset + len #<= plengthBS # bytes)
      (pif (len #== 0) (pconstant "") (psliceBS # offset # len # bytes))
      perror
