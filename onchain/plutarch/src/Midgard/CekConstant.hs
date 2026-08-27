{- |
Module      : Midgard.CekConstant
Description : Plutarch port of @lib/midgard/cek-constant-v1.ak@.

What a UPLC constant is, on-chain: a type, a payload, and the one root both a
whole-payload reveal and a node-by-node semantic proof must agree on.

=== One root, two ways of reaching it

'pconstantRootV1' takes a witness that reveals the entire payload;
'psemanticConstantRootV1' takes only a "Midgard.CekData" summary of it. Both
build the same 'Midgard.CekProof.phashConstantValueV1' preimage, with the
semantic root standing in for /both/ the payload root and the semantic root.
That is the rule that lets a builtin proof touch three nodes of a large list
without the value's identity changing.

=== The type is a prefix encoding, and the payload must fit it

A constant type is a flat CBOR array read as a prefix expression — @[5, 0]@ is a
list of integers, @[6, 0, 1]@ a pair of an integer and a byte string — decoded by
'pdecodeConstantTypeV1', which insists the array is exactly consumed and
re-serialises to the bytes it was given. 'ppayloadMatchesTypeV1' then walks the
payload against that type. Neither is a formality: the memory a constant is
charged depends on its type, so a payload admitted under the wrong type is a
mispriced step.

=== Two sizes, deliberately different

'pdataMemorySizeV1' is what Plutus charges a @Data@ value — four words a node
plus the leaf. 'psemanticMemorySizeV1' is what the /machine/ charges the
constant, which for a typed constant is the payload alone: an integer costs its
digits, a unit one word, a pair the sum of its halves. They coincide only for
@DataConstant@, and that is written into the port as the one arm that calls the
other.
-}
module Midgard.CekConstant (
  -- * Limits
  pmaxDirectPayloadBytes,

  -- * Types
  PConstantTypeV1 (..),
  PConstantWitnessV1 (..),

  -- * Decoding
  pdecodeConstantTypeV1,
  pconstantTypeIsKnownV1,
  pdecodeConstantPayloadV1,
  ppayloadMatchesTypeV1,

  -- * Roots
  pconstantRootV1,
  pverifyConstantWitnessV1,
  psemanticConstantRootV1,
  psemanticDataConstantRootV1,

  -- * Reading a witness
  pconstantTypeV1,
  pconstantPayloadV1,

  -- * Sizes
  pintegerMemorySizeV1,
  pbytearrayMemorySizeV1,
  pdataMemorySizeV1,
  pconstantPayloadMemorySizeV1,
  pconstantMemorySizeV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Aiken.Cbor (pdeserialise)
import Midgard.CekData (PDataSummaryV1 (..), psemanticDataSummaryV1)
import Midgard.CekProof (phashBlobChunkV1, phashConstantValueV1)
import Midgard.Common.Utils (pconstrOf)

--------------------------------------------------------------------------------
-- Limits
--------------------------------------------------------------------------------

{- | Aiken @max_direct_payload_bytes@.

The largest canonical payload one bounded builtin proof can reveal. Anything
larger is committed as a semantic tree instead, which is what
'psemanticConstantRootV1' exists for.
-}
pmaxDirectPayloadBytes :: forall (s :: S). Term s PInteger
pmaxDirectPayloadBytes = 9215

pmaxTypeCborBytes :: forall (s :: S). Term s PInteger
pmaxTypeCborBytes = 64

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

{- | Aiken @ConstantTypeV1@ — a UPLC type, as a tree.

Two of the eleven are recursive, which is the whole reason the wire form is a
prefix encoding rather than a tag: a list of pairs of lists has no bounded tag.

The constructor indices here are 0–10 in declaration order. They are __not__ the
wire tags: the wire skips 7, so @DataConstant@ is constructor 7 and wire tag 8,
and everything after it is offset by one. Both numbers appear in
'pdecodeConstantTypeV1', where the mapping is made.
-}
data PConstantTypeV1 (s :: S)
  = PIntegerConstant
  | PByteStringConstant
  | PStringConstant
  | PUnitConstant
  | PBooleanConstant
  | PListConstant {ptype'element :: Term s (PAsData PConstantTypeV1)}
  | PPairConstant
      { ptype'first :: Term s (PAsData PConstantTypeV1)
      , ptype'second :: Term s (PAsData PConstantTypeV1)
      }
  | PDataConstant
  | PBlsG1Constant
  | PBlsG2Constant
  | PBlsMillerLoopResultConstant
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PConstantTypeV1)

-- | Aiken @ConstantWitnessV1@ — a whole constant, revealed.
data PConstantWitnessV1 (s :: S) = PConstantWitnessV1
  { pwitness'typeCbor :: Term s (PAsData PByteString)
  , pwitness'payloadCbor :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PConstantWitnessV1)

--------------------------------------------------------------------------------
-- Decoding the type
--------------------------------------------------------------------------------

{- | Aiken @decode_type_items@.

One step of the prefix decoder: read a tag, and for the two recursive tags read
as many further types as the tag demands, returning what is left over. The
leftover is what 'pdecodeConstantTypeV1' checks is empty — a type that decodes
but leaves items behind is not a type.

Tied with 'pfix': a cycle among top-level Plutarch term definitions is an
infinite value rather than a recursive function.
-}
pdecodeTypeItems ::
  forall (s :: S).
  Term s (PBuiltinList PData :--> PPair PConstantTypeV1 (PBuiltinList PData))
pdecodeTypeItems = phoistAcyclic $
  pfix $ \self -> plam $ \items ->
    pelimList
      ( \tagData rest ->
          plet (pasInt # tagData) $ \tag ->
            pif (tag #== 0) (pleaf PIntegerConstant rest) $
              pif (tag #== 1) (pleaf PByteStringConstant rest) $
                pif (tag #== 2) (pleaf PStringConstant rest) $
                  pif (tag #== 3) (pleaf PUnitConstant rest) $
                    pif (tag #== 4) (pleaf PBooleanConstant rest) $
                      pif (tag #== 5) (plistType self rest) $
                        pif (tag #== 6) (ppairType self rest) $
                          -- The wire skips 7; 8 is Data.
                          pif (tag #== 8) (pleaf PDataConstant rest) $
                            pif (tag #== 9) (pleaf PBlsG1Constant rest) $
                              pif (tag #== 10) (pleaf PBlsG2Constant rest) $
                                pif
                                  (tag #== 11)
                                  (pleaf PBlsMillerLoopResultConstant rest)
                                  perror
      )
      perror
      items

pleaf ::
  forall (s :: S).
  (forall (s' :: S). PConstantTypeV1 s') ->
  Term s (PBuiltinList PData) ->
  Term s (PPair PConstantTypeV1 (PBuiltinList PData))
pleaf constructor rest = pcon (PPair (pcon constructor) rest)

plistType ::
  forall (s :: S).
  Term s (PBuiltinList PData :--> PPair PConstantTypeV1 (PBuiltinList PData)) ->
  Term s (PBuiltinList PData) ->
  Term s (PPair PConstantTypeV1 (PBuiltinList PData))
plistType self rest =
  pmatch (self # rest) $ \(PPair element remaining) ->
    pcon (PPair (pcon (PListConstant (pdata element))) remaining)

ppairType ::
  forall (s :: S).
  Term s (PBuiltinList PData :--> PPair PConstantTypeV1 (PBuiltinList PData)) ->
  Term s (PBuiltinList PData) ->
  Term s (PPair PConstantTypeV1 (PBuiltinList PData))
ppairType self rest =
  pmatch (self # rest) $ \(PPair first afterFirst) ->
    pmatch (self # afterFirst) $ \(PPair second remaining) ->
      pcon (PPair (pcon (PPairConstant (pdata first) (pdata second))) remaining)

{- | Aiken @decode_constant_type_v1@.

Bounded at 64 bytes, decoded, re-serialised and compared — so a non-canonical
encoding of a legal type is not a legal type — then read as a prefix expression
that must consume every item. Aborts rather than declining at every step: a
constant whose type does not parse has no root to compare against.
-}
pdecodeConstantTypeV1 :: forall (s :: S). Term s (PByteString :--> PConstantTypeV1)
pdecodeConstantTypeV1 = phoistAcyclic $
  plam $ \typeCbor ->
    pif (pnot # (plengthBS # typeCbor #<= pmaxTypeCborBytes)) perror $
      pmatch (pdeserialise # typeCbor) $ \case
        PNothing -> perror
        PJust typeData ->
          pif (pnot # (pserialiseData # typeData #== typeCbor)) perror $
            pmatch (pdecodeTypeItems #$ pasList # typeData) $ \(PPair constantType remaining) ->
              pif (pnull # remaining) constantType perror

{- | Aiken @constant_type_is_known_v1@.

Every arm of the Aiken's @when@ is @True@; the work is done by the decode above,
which aborts on anything it does not recognise. Written as a tag read rather
than an eleven-armed @pmatch@ with identical bodies, which Plutarch
mis-compiles.
-}
pconstantTypeIsKnownV1 :: forall (s :: S). Term s (PByteString :--> PBool)
pconstantTypeIsKnownV1 = phoistAcyclic $
  plam $ \typeCbor ->
    let (tag, _) = pconstrOf (pdata (pdecodeConstantTypeV1 # typeCbor))
     in 0 #<= tag

--------------------------------------------------------------------------------
-- Decoding the payload
--------------------------------------------------------------------------------

{- | Aiken @decode_constant_payload_v1@.

The @0x40@ special case is not an optimisation. @cbor.deserialise@ declines a
top-level empty byte string, because its cursor convention leaves nothing after
the header byte — so without this branch the empty byte-string constant, which
is perfectly canonical on the ledger, would have no payload. Everything else
goes through the decoder and its re-serialisation check.
-}
pdecodeConstantPayloadV1 :: forall (s :: S). Term s (PByteString :--> PData)
pdecodeConstantPayloadV1 = phoistAcyclic $
  plam $ \payloadCbor ->
    pif (pnot # (plengthBS # payloadCbor #<= pmaxDirectPayloadBytes)) perror $
      pif (payloadCbor #== pconstant "\x40") (pforgetData (pdata (pconstant @PByteString ""))) $
        pmatch (pdeserialise # payloadCbor) $ \case
          PNothing -> perror
          PJust payload ->
            pif (pserialiseData # payload #== payloadCbor) payload perror

pisDataConstr, pisDataList, pisDataInteger, pisDataBytes ::
  forall (s :: S). Term s (PData :--> PBool)
pisDataConstr = phoistAcyclic $ plam $ \d -> pchoose5 d ptrue pfalse pfalse pfalse pfalse
pisDataList = phoistAcyclic $ plam $ \d -> pchoose5 d pfalse pfalse ptrue pfalse pfalse
pisDataInteger = phoistAcyclic $ plam $ \d -> pchoose5 d pfalse pfalse pfalse ptrue pfalse
pisDataBytes = phoistAcyclic $ plam $ \d -> pchoose5 d pfalse pfalse pfalse pfalse ptrue

pchoose5 ::
  forall (s :: S).
  Term s PData ->
  Term s PBool ->
  Term s PBool ->
  Term s PBool ->
  Term s PBool ->
  Term s PBool ->
  Term s PBool
pchoose5 d a b c e f = pchooseData # d # a # b # c # e # f

ptrue, pfalse :: forall (s :: S). Term s PBool
ptrue = pconstant @PBool True
pfalse = pconstant @PBool False

--------------------------------------------------------------------------------
-- Type checking the payload
--------------------------------------------------------------------------------

{- | Aiken @payload_matches_type@.

The one arm that does not merely inspect: a @String@ constant round-trips its
bytes through @decodeUtf8@ and @encodeUtf8@, and invalid UTF-8 /aborts/ there
rather than returning @False@. That is the Aiken's behaviour and it is kept,
because the alternative — catching it — is not something a Plutus script can do.

@BlsMillerLoopResultConstant@ is @False@ unconditionally: a Miller-loop result is
an opaque UPLC value with no @Data@ representation, so no payload can match it.
-}
ppayloadMatchesTypeV1 ::
  forall (s :: S). Term s (PConstantTypeV1 :--> PData :--> PBool)
ppayloadMatchesTypeV1 = phoistAcyclic $
  pfix $ \self -> plam $ \constantType payload ->
    pmatch constantType $ \case
      PIntegerConstant -> pisDataInteger # payload
      PByteStringConstant -> pisDataBytes # payload
      PStringConstant ->
        pif (pnot # (pisDataBytes # payload)) pfalse $
          plet (pasByteStr # payload) $ \bytes ->
            (pencodeUtf8 #$ pdecodeUtf8 # bytes) #== bytes
      PUnitConstant -> pnullaryConstr payload 0 0
      PBooleanConstant -> pnullaryConstr payload 0 1
      PListConstant {ptype'element} ->
        pif (pnot # (pisDataList # payload)) pfalse $
          pall
            # plam (\item -> self # pfromData ptype'element # item)
            # (pasList # payload)
      PPairConstant {ptype'first, ptype'second} ->
        pif (pnot # (pisDataConstr # payload)) pfalse $
          pmatch (pasConstr # payload) $ \(PBuiltinPair tag fields) ->
            pif (pnot # (plength # fields #== 2)) pfalse $
              pand'List
                [ tag #== 0
                , self # pfromData ptype'first # (pelemAt # 0 # fields)
                , self # pfromData ptype'second # (pelemAt # 1 # fields)
                ]
      PDataConstant -> ptrue
      PBlsG1Constant -> pbytesOfLength payload 48
      PBlsG2Constant -> pbytesOfLength payload 96
      PBlsMillerLoopResultConstant -> pfalse

-- | A no-field constructor whose tag lies in @[low, high]@ — unit and boolean.
pnullaryConstr ::
  forall (s :: S).
  Term s PData -> Term s PInteger -> Term s PInteger -> Term s PBool
pnullaryConstr payload low high =
  pif (pnot # (pisDataConstr # payload)) pfalse $
    pmatch (pasConstr # payload) $ \(PBuiltinPair tag fields) ->
      low #<= tag #&& tag #<= high #&& (pnull # fields)

pbytesOfLength ::
  forall (s :: S). Term s PData -> Term s PInteger -> Term s PBool
pbytesOfLength payload len =
  pif
    (pisDataBytes # payload)
    (plengthBS # (pasByteStr # payload) #== len)
    pfalse

--------------------------------------------------------------------------------
-- Roots
--------------------------------------------------------------------------------

{- | Aiken @constant_root_v1@.

The whole-payload form. The semantic root stands in for the payload root as
well, which is what makes this and 'psemanticConstantRootV1' agree.
-}
pconstantRootV1 :: forall (s :: S). Term s (PConstantWitnessV1 :--> PByteString)
pconstantRootV1 = phoistAcyclic $
  plam $ \witness ->
    pmatch witness $ \(PConstantWitnessV1 typeCbor payloadCbor) ->
      plet (pdecodeConstantTypeV1 # pfromData typeCbor) $ \constantType ->
        plet (pdecodeConstantPayloadV1 # pfromData payloadCbor) $ \payload ->
          pif (pnot # (ppayloadMatchesTypeV1 # constantType # payload)) perror $
            pmatch (psemanticDataSummaryV1 # payload) $ \(PDataSummaryV1 root cborLength _) ->
              phashConstantValueV1
                # (phashBlobChunkV1 # pfromData typeCbor)
                # pfromData root
                # pfromData cborLength
                # pfromData root
                # (psemanticMemorySizeV1 # constantType # payload)

-- | Aiken @verify_constant_witness_v1@.
pverifyConstantWitnessV1 ::
  forall (s :: S). Term s (PByteString :--> PConstantWitnessV1 :--> PBool)
pverifyConstantWitnessV1 = phoistAcyclic $
  plam $ \root witness -> root #== (pconstantRootV1 # witness)

{- | Aiken @semantic_constant_root_v1@.

The content-addressed form: a type, a "Midgard.CekData" summary and the memory
the machine charges. It builds the same preimage as 'pconstantRootV1', so a
value has one consensus root whether a proof reveals its whole payload or only
the nodes a builtin touched.
-}
psemanticConstantRootV1 ::
  forall (s :: S).
  Term s (PByteString :--> PDataSummaryV1 :--> PInteger :--> PByteString)
psemanticConstantRootV1 = phoistAcyclic $
  plam $ \typeCbor summary memory ->
    pmatch summary $ \(PDataSummaryV1 root cborLength summaryMemory) ->
      pif
        ( pnot
            #$ pand'List
              [ plengthBS # typeCbor #<= pmaxTypeCborBytes
              , pconstantTypeIsKnownV1 # typeCbor
              , plengthBS # pfromData root #== 32
              , 0 #<= pfromData cborLength
              , 0 #<= pfromData summaryMemory
              , 0 #<= memory
              ]
        )
        perror
        $ phashConstantValueV1
          # (phashBlobChunkV1 # typeCbor)
          # pfromData root
          # pfromData cborLength
          # pfromData root
          # memory

{- | Aiken @semantic_data_constant_root_v1@.

@0x9f08ff@ is the indefinite array @[8]@ — the one-item type expression naming
@Data@. A @Data@ constant's machine memory is exactly its semantic tree memory,
which is what lets the validation machine seed its script context without
revealing the context's CBOR.
-}
psemanticDataConstantRootV1 ::
  forall (s :: S). Term s (PDataSummaryV1 :--> PByteString)
psemanticDataConstantRootV1 = phoistAcyclic $
  plam $ \summary ->
    pmatch summary $ \(PDataSummaryV1 _ _ memory) ->
      psemanticConstantRootV1
        # pconstant "\x9f\x08\xff"
        # summary
        # pfromData memory

--------------------------------------------------------------------------------
-- Reading a witness
--------------------------------------------------------------------------------

-- | Aiken @constant_type_v1@ — the type, once the payload has been checked against it.
pconstantTypeV1 :: forall (s :: S). Term s (PConstantWitnessV1 :--> PConstantTypeV1)
pconstantTypeV1 = phoistAcyclic $
  plam $ \witness ->
    pmatch witness $ \(PConstantWitnessV1 typeCbor payloadCbor) ->
      plet (pdecodeConstantTypeV1 # pfromData typeCbor) $ \constantType ->
        pif
          ( ppayloadMatchesTypeV1
              # constantType
              # (pdecodeConstantPayloadV1 # pfromData payloadCbor)
          )
          constantType
          perror

-- | Aiken @constant_payload_v1@.
pconstantPayloadV1 :: forall (s :: S). Term s (PConstantWitnessV1 :--> PData)
pconstantPayloadV1 = phoistAcyclic $
  plam $ \witness ->
    pmatch witness $ \(PConstantWitnessV1 typeCbor payloadCbor) ->
      plet (pdecodeConstantPayloadV1 # pfromData payloadCbor) $ \payload ->
        pif
          ( ppayloadMatchesTypeV1
              # (pdecodeConstantTypeV1 # pfromData typeCbor)
              # payload
          )
          payload
          perror

--------------------------------------------------------------------------------
-- Sizes
--------------------------------------------------------------------------------

punsignedByteSizeV1 :: forall (s :: S). Term s (PInteger :--> PInteger)
punsignedByteSizeV1 = phoistAcyclic $
  pfix $ \self -> plam $ \value ->
    pif (value #< 256) 1 (1 + (self #$ pquot # value # 256))

-- | Aiken @integer_memory_size_v1@ — bytes of the doubled magnitude.
pintegerMemorySizeV1 :: forall (s :: S). Term s (PInteger :--> PInteger)
pintegerMemorySizeV1 = phoistAcyclic $
  plam $ \value ->
    punsignedByteSizeV1
      #$ pif (value #< 0) ((negate value - 1) * 2) (value * 2)

-- | Aiken @bytearray_memory_size_v1@ — the empty string still costs one word.
pbytearrayMemorySizeV1 :: forall (s :: S). Term s (PByteString :--> PInteger)
pbytearrayMemorySizeV1 = phoistAcyclic $
  plam $ \value ->
    plet (plengthBS # value) $ \len -> pif (len #== 0) 1 len

{- | Aiken @data_memory_size_v1@ — what Plutus charges a @Data@ value.

Four words a node plus the leaf's own size. The five branches are whole
recursive walks, so they are delayed: 'pchooseData' evaluates all six of its
arguments, and an eager constructor branch would run @unConstrData@ on an
integer.
-}
pdataMemorySizeV1 :: forall (s :: S). Term s (PData :--> PInteger)
pdataMemorySizeV1 = phoistAcyclic $
  pfix $ \self -> plam $ \value ->
    pforce $
      pchooseData
        # value
        # pdelay
          ( 4
              + ( pdataItemsMemorySizeV1
                    # self
                    #$ psndBuiltinPair (pasConstr # value)
                )
          )
        # pdelay (4 + (pdataPairsMemorySizeV1 # self # (pasMap # value)))
        # pdelay (4 + (pdataItemsMemorySizeV1 # self # (pasList # value)))
        # pdelay (4 + (pintegerMemorySizeV1 #$ pasInt # value))
        # pdelay (4 + (pbytearrayMemorySizeV1 #$ pasByteStr # value))

psndBuiltinPair ::
  forall (s :: S).
  Term s (PBuiltinPair PInteger (PBuiltinList PData)) -> Term s (PBuiltinList PData)
psndBuiltinPair pair = pmatch pair $ \(PBuiltinPair _ fields) -> fields

-- | Aiken @data_items_memory_size_v1@.
pdataItemsMemorySizeV1 ::
  forall (s :: S).
  Term s ((PData :--> PInteger) :--> PBuiltinList PData :--> PInteger)
pdataItemsMemorySizeV1 = phoistAcyclic $
  pfix $ \self -> plam $ \size items ->
    pelimList (\item rest -> (size # item) + (self # size # rest)) 0 items

-- | Aiken @data_pairs_memory_size_v1@.
pdataPairsMemorySizeV1 ::
  forall (s :: S).
  Term
    s
    ( (PData :--> PInteger)
        :--> PBuiltinList (PBuiltinPair PData PData)
        :--> PInteger
    )
pdataPairsMemorySizeV1 = phoistAcyclic $
  pfix $ \self -> plam $ \size entries ->
    pelimList
      ( \entry rest ->
          pmatch entry $ \(PBuiltinPair key value) ->
            (size # key) + (size # value) + (self # size # rest)
      )
      0
      entries

{- | Aiken @semantic_memory_size_v1@ — what the /machine/ charges the constant.

Read by constructor tag rather than by @pmatch@: byte string and string are the
same expression, and so are unit and boolean, and a @pmatch@ whose arms have
identical bodies mis-compiles in Plutarch. Declaration order is integer, bytes,
string, unit, boolean, list, pair, data, G1, G2, Miller-loop.
-}
psemanticMemorySizeV1 ::
  forall (s :: S). Term s (PConstantTypeV1 :--> PData :--> PInteger)
psemanticMemorySizeV1 = phoistAcyclic $
  pfix $ \self -> plam $ \constantType payload ->
    let (tag, fields) = pconstrOf (pdata constantType)
     in plet tag $ \kind ->
          pif (kind #== 0) (pintegerMemorySizeV1 #$ pasInt # payload) $
            -- 1 bytes and 2 string: the same size, from the same bytes.
            pif (kind #== 1 #|| kind #== 2) (pbytearrayMemorySizeV1 #$ pasByteStr # payload) $
              -- 3 unit and 4 boolean: one word each.
              pif (kind #== 3 #|| kind #== 4) 1 $
                pif (kind #== 5) (plistMemory self fields payload) $
                  pif (kind #== 6) (ppairMemory self fields payload) $
                    pif (kind #== 7) (pdataMemorySizeV1 # payload) $
                      pif (kind #== 8) 48 $
                        pif (kind #== 9) 96 192

plistMemory ::
  forall (s :: S).
  Term s (PConstantTypeV1 :--> PData :--> PInteger) ->
  Term s (PBuiltinList PData) ->
  Term s PData ->
  Term s PInteger
plistMemory self fields payload =
  plet (pfromData (punsafeTypeAt fields 0)) $ \element ->
    pdataItemsMemorySizeV1
      # plam (\item -> self # element # item)
      # (pasList # payload)

ppairMemory ::
  forall (s :: S).
  Term s (PConstantTypeV1 :--> PData :--> PInteger) ->
  Term s (PBuiltinList PData) ->
  Term s PData ->
  Term s PInteger
ppairMemory self fields payload =
  pmatch (pasConstr # payload) $ \(PBuiltinPair tag values) ->
    pif (pnot # (tag #== 0)) perror $
      pif (pnot # (plength # values #== 2)) perror $
        (self # pfromData (punsafeTypeAt fields 0) # (pelemAt # 0 # values))
          + (self # pfromData (punsafeTypeAt fields 1) # (pelemAt # 1 # values))

-- | A nested type read out of a constructor's field vector.
punsafeTypeAt ::
  forall (s :: S).
  Term s (PBuiltinList PData) -> Term s PInteger -> Term s (PAsData PConstantTypeV1)
punsafeTypeAt fields index = punsafeCoerce (pelemAt # index # fields)

-- | Aiken @constant_payload_memory_size_v1@.
pconstantPayloadMemorySizeV1 ::
  forall (s :: S). Term s (PConstantTypeV1 :--> PData :--> PInteger)
pconstantPayloadMemorySizeV1 = phoistAcyclic $
  plam $ \constantType payload ->
    pif
      (ppayloadMatchesTypeV1 # constantType # payload)
      (psemanticMemorySizeV1 # constantType # payload)
      perror

-- | Aiken @constant_memory_size_v1@.
pconstantMemorySizeV1 :: forall (s :: S). Term s (PConstantWitnessV1 :--> PInteger)
pconstantMemorySizeV1 = phoistAcyclic $
  plam $ \witness ->
    psemanticMemorySizeV1
      # (pconstantTypeV1 # witness)
      # (pconstantPayloadV1 # witness)
