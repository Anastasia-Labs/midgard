{- |
Module      : Aiken.Cbor
Description : Plutarch port of @aiken/cbor.deserialise@ from @aiken-lang/stdlib@.

The CBOR decoder, and the single largest thing standing between this port and the
rest of the Aiken tree: __twenty-nine__ live library modules call
@cbor.deserialise@, and none of them could be ported without it.

=== Why it has to be written at all

Plutus has @serialiseData@ as a builtin and no inverse. Aiken's @deserialise@ is
not a builtin either — it is an interpreter written in Aiken, which is why the
stdlib's own documentation warns that it is "an order of magnitude more
expensive" than serialising. So there is nothing to delegate to here: the decoder
is transcribed, structure for structure, from
@build/packages/aiken-lang-stdlib/lib/aiken/cbor.ak@.

=== The cursor runs backwards

The original counts __remaining__ bytes rather than consumed ones: @cursor@ starts
at the input's length, every read subtracts, and the byte a read looks at is at
offset @length - cursor@. A decode succeeded only if the cursor is exactly zero
at the end, which is what rejects trailing bytes. That convention is kept here
verbatim — it is not the convention this port would have chosen, but it is the
one every failure mode below is stated in, and re-deriving them under a forward
offset would be a rewrite rather than a port.

=== Failure is a cursor, not an exception

The original has no @Option@ inside the decoder. It signals failure by returning
a cursor of @-1@ and a placeholder value, and every subsequent read sees a
non-positive cursor and returns the same failure, so a failure deep inside a
nested structure propagates outward by arithmetic. The port uses 'PMaybe' and
short-circuits instead. The two agree on every input: a @-1@ cursor can never
reach the final @consumed == 0@ test as a success, and no successful decode ever
produces one.

The one place that equivalence is worth stating precisely is 'ptakeBytes'.
@slice_bytearray@ /clamps/ — a read past the end returns a short slice rather
than failing — and the original does not check for it. It does not need to: a
clamped read drives the cursor negative, and negative is not zero, so the final
test rejects. The port reproduces the clamp deliberately (§7.3's abort-never-clamp
rule elsewhere in this tree is the opposite rule for a different reason) because
adding a bounds check here would accept nothing extra and would diverge on the
cursor arithmetic.

=== The knot has to be tied in Plutarch, not in Haskell

The original is a set of mutually recursive Aiken functions. Writing the port
that way — top-level Plutarch terms referring to each other — type-checks and
compiles, and then hangs: a Plutarch term is a value, so a cycle among top-level
term definitions is an /infinite value/, and forcing it exhausts memory before
anything is evaluated. The recursion is therefore tied once, with 'pfix' inside
'pdecodeData', and every helper that needs to decode an item takes that decoder
as an argument. The sequence combinators already took an element decoder as a
parameter in the original, so they needed no change; the four structural cases
did.

=== What it refuses

Major types 3 (text) and 7 (simple values, floats) have no @Data@ counterpart and
are refused. Tag 102 is refused explicitly. Everything else in the @Data@ grammar
decodes, including the indefinite-length forms, which
"Midgard.CanonicalPlutusData" would reject as non-canonical: canonicity is a
/separate/ question, asked by re-encoding and comparing, and this module answers
only "is this CBOR at all".
-}
module Aiken.Cbor (
  pdeserialise,
) where

import Data.Kind (Type)
import PlutusCore qualified as PLC

import Plutarch.Builtin.ByteString (pbyteStringToInteger, pmostSignificantFirst)
import Plutarch.Core.Internal.Builtins (pindexBS')
import Plutarch.Internal.Term (punsafeBuiltin)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

--------------------------------------------------------------------------------
-- The entry point
--------------------------------------------------------------------------------

{- | Aiken @cbor.deserialise@.

@Nothing@ for empty input, for anything the @Data@ grammar does not cover, and —
the case worth naming — for input that decodes but leaves bytes over. A decoder
that accepted a prefix would let two different byte strings claim the same datum,
which is exactly what every re-encoding check in this tree exists to prevent.
-}
pdeserialise :: forall (s :: S). Term s (PByteString :--> PMaybe PData)
pdeserialise = phoistAcyclic $
  plam $ \bytes ->
    plet (plengthBS # bytes) $ \len ->
      pif (len #== 0) (pcon PNothing) $
        pmatch (pdecodeData # bytes # len # len) $ \case
          PNothing -> pcon PNothing
          PJust result ->
            pmatch result $ \(PPair value remaining) ->
              -- Aiken calls this `consumed`; it is the cursor, so zero means
              -- every byte was used and nothing was left over.
              pif (remaining #== 0) (pcon (PJust value)) (pcon PNothing)

--------------------------------------------------------------------------------
-- Reading bytes
--------------------------------------------------------------------------------

{- | The decoder's shape: bytes, the input's length, the cursor in; the value and
the new cursor out.

@length@ is passed rather than recomputed because the byte a read looks at is at
@length - cursor@ and @plengthBS@ is not free.
-}
type PDecoder (a :: S -> Type) =
  PByteString :--> PInteger :--> PInteger :--> PMaybe (PPair a PInteger)

{- | Aiken @peek@, at the only offset it is ever called with.

Every call site in the original is @peek(1)@, so the offset is fixed here rather
than carried; a peek that did not consume its byte would be a different function
and the original does not have one.
-}
ppeekByte :: forall (s :: S). Term s (PDecoder PInteger)
ppeekByte = phoistAcyclic $
  plam $ \bytes len cursor ->
    pif (cursor #<= 0) (pcon PNothing) $
      pcon (PJust (pcon (PPair (pindexBS' # bytes # (len - cursor)) (cursor - 1))))

{- | Aiken @take@.

Clamps rather than checking, deliberately — see the module header.
-}
ptakeBytes ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PMaybe (PPair PByteString PInteger)
    )
ptakeBytes = phoistAcyclic $
  plam $ \bytes len n cursor ->
    pif (cursor #<= 0) (pcon PNothing) $
      pcon (PJust (pcon (PPair (psliceBS # (len - cursor) # n # bytes) (cursor - n))))

--------------------------------------------------------------------------------
-- Headers
--------------------------------------------------------------------------------

{- | Aiken @decode_uint@.

The five minor-argument forms: immediate below 24, one following byte at 24, and
two, four or eight following bytes at 25, 26 and 27. Anything above — 28, 29, 30
and the indefinite marker 31 — has no length to read and is refused here rather
than by its caller.
-}
pdecodeUint ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PMaybe (PPair PInteger PInteger)
    )
pdecodeUint = phoistAcyclic $
  plam $ \bytes len header cursor ->
    pif (header #< 24) (pjustPair header cursor) $
      pif (header #== 24) (ppeekByte # bytes # len # cursor) $
        pif (header #< 28) `flip` pcon PNothing $
          plet (pwidthOf # (header - 25)) $ \width ->
            pmatch (ptakeBytes # bytes # len # width # cursor) $ \case
              PNothing -> pcon PNothing
              PJust taken ->
                pmatch taken $ \(PPair payload nextCursor) ->
                  pjustPair
                    (pbyteStringToInteger # pmostSignificantFirst # payload)
                    nextCursor

{- | Aiken @bytearray.at(#[2, 4, 8], header - 25)@.

Written as a comparison chain rather than as an index into a three-byte constant:
the original's index is already in range because its caller has bounded @header@
to 25, 26 or 27, and a chain says so where a lookup only implies it.
-}
pwidthOf :: forall (s :: S). Term s (PInteger :--> PInteger)
pwidthOf = phoistAcyclic $
  plam $ \index -> pif (index #== 0) 2 $ pif (index #== 1) 4 8

--------------------------------------------------------------------------------
-- Byte strings
--------------------------------------------------------------------------------

-- | Aiken @decode_bytes@: a length header, then that many bytes.
pdecodeBytes ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PMaybe (PPair PByteString PInteger)
    )
pdecodeBytes = phoistAcyclic $
  plam $ \bytes len header cursor ->
    pmatch (pdecodeUint # bytes # len # header # cursor) $ \case
      PNothing -> pcon PNothing
      PJust widthAndCursor ->
        pmatch widthAndCursor $ \(PPair width nextCursor) ->
          ptakeBytes # bytes # len # width # nextCursor

{- | Aiken @decode_chunks@.

The indefinite byte-string form: definite chunks until a break, concatenated.
Nothing checks that the chunks are non-empty or that there is more than one,
because the encoder that produced them is not this decoder's problem — a
re-encoding check upstream is what makes an indefinite string non-canonical.
-}
pdecodeChunks ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PInteger :--> PMaybe (PPair PByteString PInteger))
pdecodeChunks = phoistAcyclic $
  pfix $ \self -> plam $ \bytes len cursor ->
    pmatch (ppeekByte # bytes # len # cursor) $ \case
      PNothing -> pcon PNothing
      PJust peeked ->
        pmatch peeked $ \(PPair next afterNext) ->
          pif (next #== ptokenBreak) (pjustPair (pconstant "") afterNext) $
            pmatch (pdecodeBytes # bytes # len # (next - 64) # afterNext) $ \case
              PNothing -> pcon PNothing
              PJust chunkAndCursor ->
                pmatch chunkAndCursor $ \(PPair chunk afterChunk) ->
                  pmatch (self # bytes # len # afterChunk) $ \case
                    PNothing -> pcon PNothing
                    PJust restAndCursor ->
                      pmatch restAndCursor $ \(PPair rest afterRest) ->
                        pjustPair (chunk <> rest) afterRest

--------------------------------------------------------------------------------
-- Sequences
--------------------------------------------------------------------------------

{- | Aiken @decode_definite@, at whichever element decoder it is given.

The original is polymorphic in the element; Plutarch is not, so this is a Haskell
function instantiated twice — once at 'PData' for arrays and constructor fields,
once at a key/value pair for maps. One implementation, two instantiations, which
is what monomorphising the original would have produced.
-}
pdecodeDefinite ::
  forall (a :: S -> Type) (s :: S).
  PElemConstraint PBuiltinList a =>
  Term s (PDecoder a) ->
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PMaybe (PPair (PBuiltinList a) PInteger)
    )
pdecodeDefinite decodeOne =
  pfix $ \self -> plam $ \bytes len size cursor ->
    pif (size #<= 0) (pjustPair pnil cursor) $
      pmatch (decodeOne # bytes # len # cursor) $ \case
        PNothing -> pcon PNothing
        PJust elemAndCursor ->
          pmatch elemAndCursor $ \(PPair element afterElement) ->
            pmatch (self # bytes # len # (size - 1) # afterElement) $ \case
              PNothing -> pcon PNothing
              PJust restAndCursor ->
                pmatch restAndCursor $ \(PPair rest afterRest) ->
                  pjustPair (pcons # element # rest) afterRest

{- | Aiken @decode_indefinite@.

The @+ 1@ is the original's and is load-bearing: the peek that tests for the
break byte consumes it, but when it is /not/ the break byte it is the first byte
of the next element, so the cursor is handed back before the element is decoded.
-}
pdecodeIndefinite ::
  forall (a :: S -> Type) (s :: S).
  PElemConstraint PBuiltinList a =>
  Term s (PDecoder a) ->
  Term s (PByteString :--> PInteger :--> PInteger :--> PMaybe (PPair (PBuiltinList a) PInteger))
pdecodeIndefinite decodeOne =
  pfix $ \self -> plam $ \bytes len cursor ->
    pmatch (ppeekByte # bytes # len # cursor) $ \case
      PNothing -> pcon PNothing
      PJust peeked ->
        pmatch peeked $ \(PPair next afterNext) ->
          pif (next #== ptokenBreak) (pjustPair pnil afterNext) $
            pmatch (decodeOne # bytes # len # (afterNext + 1)) $ \case
              PNothing -> pcon PNothing
              PJust elemAndCursor ->
                pmatch elemAndCursor $ \(PPair element afterElement) ->
                  pmatch (self # bytes # len # afterElement) $ \case
                    PNothing -> pcon PNothing
                    PJust restAndCursor ->
                      pmatch restAndCursor $ \(PPair rest afterRest) ->
                        pjustPair (pcons # element # rest) afterRest

-- | Aiken @decode_pair@: two data items, no header of their own.
pdecodePair ::
  forall (s :: S).
  Term s (PDecoder PData) -> Term s (PDecoder (PBuiltinPair PData PData))
pdecodePair decodeItem =
  plam $ \bytes len cursor ->
    pmatch (decodeItem # bytes # len # cursor) $ \case
      PNothing -> pcon PNothing
      PJust keyAndCursor ->
        pmatch keyAndCursor $ \(PPair key afterKey) ->
          pmatch (decodeItem # bytes # len # afterKey) $ \case
            PNothing -> pcon PNothing
            PJust valueAndCursor ->
              pmatch valueAndCursor $ \(PPair value afterValue) ->
                pjustPair
                  (punsafeCoerce (ppairDataBuiltin # pdataCoerce key # pdataCoerce value))
                  afterValue

--------------------------------------------------------------------------------
-- The item decoder
--------------------------------------------------------------------------------

{- | Aiken @decode_data@ — one CBOR item, dispatched on its major type.

The order of the tests is the original's, which is not the numeric order of the
major types: 0–2 first, then 6, then 4, then 5. Nothing depends on the order, but
a reader comparing the two files should not have to check that.
-}
pdecodeData :: forall (s :: S). Term s (PDecoder PData)
pdecodeData = phoistAcyclic $
  pfix $ \self -> plam $ \bytes len cursor ->
    pmatch (ppeekByte # bytes # len # cursor) $ \case
      PNothing -> pcon PNothing
      PJust peeked ->
        pmatch peeked $ \(PPair next afterNext) ->
          plet (pquot # next # 32) $ \majorType ->
            pif (majorType #<= 2) (pdecodeAtomic bytes len next afterNext majorType) $
              pif (majorType #== 6) (pdecodeTagged self bytes len next afterNext) $
                pif (majorType #== 4) (pdecodeArray self bytes len next afterNext) $
                  pif (majorType #== 5) (pdecodeMap self bytes len next afterNext) $
                    pcon PNothing

-- | Major types 0, 1 and 2: unsigned, negative, and byte strings.
pdecodeAtomic ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PMaybe (PPair PData PInteger))
pdecodeAtomic bytes len next cursor majorType =
  pif
    (majorType #== 0)
    (pmapPair (pdecodeUint # bytes # len # next # cursor) (\i -> pforgetData (pdata i)))
    $ pif
      (majorType #== 1)
      ( pmapPair
          (pdecodeUint # bytes # len # (next - 32) # cursor)
          (\i -> pforgetData (pdata (negate i - 1)))
      )
    $ pif
      (next #== ptokenBeginBytes)
      (pmapPair (pdecodeChunks # bytes # len # cursor) (\b -> pforgetData (pdata b)))
      ( pmapPair
          (pdecodeBytes # bytes # len # (next - 64) # cursor)
          (\b -> pforgetData (pdata b))
      )

{- | Major type 6: a tag, then a constructor's fields.

The tag arithmetic is Plutus's: 121–127 are constructors 0–6 and 1280 upwards are
7 onwards. Tag 102 is refused; nothing else is, so a tag outside those ranges
produces a constructor index the original would also produce.
-}
pdecodeTagged ::
  forall (s :: S).
  Term s (PDecoder PData) ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PMaybe (PPair PData PInteger))
pdecodeTagged decodeItem bytes len next cursor =
  pmatch (pdecodeUint # bytes # len # (next - 192) # cursor) $ \case
    PNothing -> pcon PNothing
    PJust tagAndCursor ->
      pmatch tagAndCursor $ \(PPair tag afterTag) ->
        pmatch (ppeekByte # bytes # len # afterTag) $ \case
          PNothing -> pcon PNothing
          PJust peeked ->
            pmatch peeked $ \(PPair header afterHeader) ->
              pif (tag #== 102) (pcon PNothing) $
                plet (pif (1280 #<= tag) (tag - 1280 + 7) (tag - 121)) $ \index ->
                  pmapPair
                    ( pif
                        (header #== ptokenBeginList)
                        (pdecodeIndefinite decodeItem # bytes # len # afterHeader)
                        (pdefiniteItems decodeItem bytes len (header - 128) afterHeader)
                    )
                    (\fields -> pforgetData (pconstrBuiltin # index # fields))

-- | Major type 4: an array, definite or indefinite.
pdecodeArray ::
  forall (s :: S).
  Term s (PDecoder PData) ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PMaybe (PPair PData PInteger))
pdecodeArray decodeItem bytes len next cursor =
  pmapPair
    ( pif
        (next #== ptokenBeginList)
        (pdecodeIndefinite decodeItem # bytes # len # cursor)
        (pdefiniteItems decodeItem bytes len (next - 128) cursor)
    )
    (plistData #)

-- | Major type 5: a map, definite or indefinite.
pdecodeMap ::
  forall (s :: S).
  Term s (PDecoder PData) ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PMaybe (PPair PData PInteger))
pdecodeMap decodeItem bytes len next cursor =
  pmapPair
    ( pif
        (next #== ptokenBeginMap)
        (pdecodeIndefinite (pdecodePair decodeItem) # bytes # len # cursor)
        ( pmatch (pdecodeUint # bytes # len # (next - 160) # cursor) $ \case
            PNothing -> pcon PNothing
            PJust sizeAndCursor ->
              pmatch sizeAndCursor $ \(PPair size afterSize) ->
                pdecodeDefinite (pdecodePair decodeItem) # bytes # len # size # afterSize
        )
    )
    (pmapData #)

-- | A definite-length run of data items, behind its own size header.
pdefiniteItems ::
  forall (s :: S).
  Term s (PDecoder PData) ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PMaybe (PPair (PBuiltinList PData) PInteger))
pdefiniteItems decodeItem bytes len header cursor =
  pmatch (pdecodeUint # bytes # len # header # cursor) $ \case
    PNothing -> pcon PNothing
    PJust sizeAndCursor ->
      pmatch sizeAndCursor $ \(PPair size afterSize) ->
        pdecodeDefinite decodeItem # bytes # len # size # afterSize

--------------------------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------------------------

-- | The CBOR tokens the decoder branches on by value.
ptokenBeginBytes, ptokenBeginList, ptokenBeginMap, ptokenBreak :: forall (s :: S). Term s PInteger
ptokenBeginBytes = 0x5f
ptokenBeginList = 0x9f
ptokenBeginMap = 0xbf
ptokenBreak = 0xff

pjustPair ::
  forall (a :: S -> Type) (s :: S).
  Term s a ->
  Term s PInteger ->
  Term s (PMaybe (PPair a PInteger))
pjustPair value cursor = pcon (PJust (pcon (PPair value cursor)))

-- | Apply a function to the value half of a decoder's result.
pmapPair ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PMaybe (PPair a PInteger)) ->
  (Term s a -> Term s b) ->
  Term s (PMaybe (PPair b PInteger))
pmapPair result f =
  pmatch result $ \case
    PNothing -> pcon PNothing
    PJust valueAndCursor ->
      pmatch valueAndCursor $ \(PPair value cursor) -> pjustPair (f value) cursor

{- | @MapData@, which Plutarch does not name.

Every other @Data@ constructor has a Plutarch spelling — 'plistData',
'pconstrBuiltin', and 'pdata' at 'PInteger' and 'PByteString' for @I@ and @B@ —
but the builtin that wraps a key/value list is not exported, so it is named here
by its Plutus Core tag.
-}
pmapData :: forall (s :: S). Term s (PBuiltinList (PBuiltinPair PData PData) :--> PData)
pmapData = punsafeBuiltin PLC.MapData

-- | A decoded item, seen as the @PAsData@ a builtin pair wants.
pdataCoerce :: forall (a :: S -> Type) (s :: S). Term s PData -> Term s (PAsData a)
pdataCoerce = punsafeCoerce
