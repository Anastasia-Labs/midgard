{- |
Module      : Midgard.FraudProofs.NativeTx.Preimages
Description : Plutarch port of
              @lib/midgard/fraud-proofs/native-tx/preimages.ak@.

Producers and decoders for the nine field preimages of §5.

Every field — all nine, mint and both witness fields included — is the same §5.1
envelope: a definite array header followed by one definite byte-string-wrapped
item per element, and exactly @80@ when empty. The uniformity is the point: one
head decode plus a byte jump per item gives an O(1) top-level skip, and a single
walk implementation serves every field dispute.

The commitment over these bytes is a plain @blake2b_256@
('Midgard.NativeTxFieldAccess.pfieldCommitment'). This module owns the bytes,
not the hashing.

=== This module is not an access idiom

Nothing here verifies a field hash on its own. The decoders take an
/already-authenticated/ preimage and materialise it as typed items, which is what
@transaction.verify_midgard_transaction_field_preimage_v1@ needs in order to
re-encode and so pin the canonical spelling. The Aiken tree removed the two
helpers that used to verify a hash here — a free-standing field hash is the shape
§4's positional-identity invariant prohibits at a dispute entry point.
'pverifyCanonicalMintPreimageCbor' is not an exception: it is called with a hash
that came out of an authenticated view.

=== Mint is the field that never re-encodes

Every other field is checked by decoding and re-encoding, so a lenient decoder is
caught by the byte comparison. Field 5 is not: it is walked in place, precisely
so a large mint does not have to be materialised as @Data@ inside the L1
execution-memory envelope. That makes the canonical rules here — minimal-width
byte strings and map headers, ascending key order, no duplicates, no empty
policy, no zero quantity — the only thing standing between one mint and two
encodings. They are enforced on the /producer/ side as well, so an encoder
cannot hand back a preimage that never decodes.
-}
module Midgard.FraudProofs.NativeTx.Preimages (
  -- * Producers
  pencodeInputPreimage,
  pencodeOutputPreimage,
  pencodeHash28ListPreimage,
  pencodeAddressWitnessPreimage,
  pencodeScriptWitnessPreimage,
  pencodeRedeemerWitnessPreimage,

  -- * Decoders
  pdecodeMidgardTxByteListPreimageCbor,
  pdecodeMidgardTxHash28ListPreimageCbor,
  pdecodeMidgardTxInputsPreimageCbor,
  pdecodeMidgardTxOutputsPreimageCbor,
  pdecodeMidgardTxAddressWitnessesPreimageCbor,
  pdecodeMidgardTxScriptWitnessesPreimageCbor,
  pdecodeMidgardTxRedeemerWitnessesPreimageCbor,

  -- * Field 5 — mint
  pencodeMintPolicyItem,
  pmintPolicyItems,
  pencodeMintPreimage,
  pdecodeMidgardTxMintPreimageCbor,
  pverifyCanonicalMintPreimageCbor,

  -- * Canonical CBOR primitives
  pdecodeCanonicalBytesAt,
  pdecodeCanonicalMapHeaderAt,
  pcanonicalBytesKeyPrecedes,
) where

import Data.Kind (Type)
import Plutarch.LedgerApi.AssocMap (PAssocMap (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Codec (
  pbyteAt,
  pcborInt,
  pdecodeCanonicalIntAt,
  pdecodeDefiniteBytesAt,
  pencodeDefiniteBytes,
  pencodeDefiniteMapHeader,
  pexpectByte,
  psliceLen,
 )
import Midgard.FraudProofs.NativeTx.Components (
  pdecodeMidgardAddressWitnessCbor,
  pdecodeMidgardRedeemerWitnessAt,
  pdecodeMidgardTxInputCbor,
  pdecodeMidgardTxOutputCbor,
  pdecodeMidgardVersionedScriptAt,
  pencodeMidgardAddressWitness,
  pencodeMidgardRedeemerWitness,
  pencodeMidgardTxInput,
  pencodeMidgardTxOutput,
  pencodeMidgardVersionedScript,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddressWitness,
  PMidgardRedeemerWitness,
  PMidgardTxInput,
  PMidgardTxOutput,
  PMidgardVersionedScript,
 )
import Midgard.NativeTxFieldAccess (
  pdecodeFieldArrayHeaderAt,
  pencodeFieldPreimage,
  pfieldCommitment,
  phash28ItemBytes,
 )

--------------------------------------------------------------------------------
-- Shared shapes
--------------------------------------------------------------------------------

{- | The item list an envelope is built from.

Aiken's @List<ByteArray>@ is a @list(data)@ of @B@-wrapped items; the port uses
the unwrapped builtin list, since this list is an intermediate that never
crosses a @Data@ boundary — it is built, enveloped and discarded inside one
script.
-}
type PItems = PBuiltinList PByteString

-- | A mint's asset entries: name to quantity, quantity signed.
type PMintAssets = PAssocMap PByteString PInteger

-- | Aiken's @Pair<Data, Data>@ list, as @un_map_data@ hands it back.
type PDataPairs = PBuiltinList (PBuiltinPair PData PData)

-- | Inequality; Plutarch has @#==@ but no negated form.
(#!=) :: forall (s :: S) (a :: S -> Type). PEq a => Term s a -> Term s a -> Term s PBool
x #!= y = pnot # (x #== y)

infix 4 #!=

-- | The first component of a builtin pair.
pfst :: forall (s :: S) (a :: S -> Type) (b :: S -> Type). Term s (PBuiltinPair a b) -> Term s a
pfst p = pmatch p $ \(PBuiltinPair a _) -> a

-- | The second component of a builtin pair.
psnd :: forall (s :: S) (a :: S -> Type) (b :: S -> Type). Term s (PBuiltinPair a b) -> Term s b
psnd p = pmatch p $ \(PBuiltinPair _ b) -> b

{- | Maps a Data-encoded item list through an encoder and envelopes the result.

The shape every producer below shares: @list.map(encode) |> encode_field_preimage@.
-}
penvelopeMapped ::
  forall (s :: S) (a :: S -> Type).
  PIsData a =>
  Term s (a :--> PByteString) ->
  Term s (PBuiltinList (PAsData a) :--> PByteString)
penvelopeMapped encode =
  plam $ \items -> pencodeFieldPreimage #$ pmapItems # encode # items

-- | The mapping run behind 'penvelopeMapped'.
pmapItems ::
  forall (s :: S) (a :: S -> Type).
  PIsData a =>
  Term s ((a :--> PByteString) :--> PBuiltinList (PAsData a) :--> PItems)
pmapItems = phoistAcyclic $
  pfix $ \self -> plam $ \encode items ->
    pelimList
      (\item rest -> pcons # (encode #$ pfromData item) # (self # encode # rest))
      (pcon PNil)
      items

--------------------------------------------------------------------------------
-- Producers
--------------------------------------------------------------------------------

-- | Aiken @preimages.encode_input_preimage@ — field 0 or 1.
pencodeInputPreimage ::
  forall (s :: S). Term s (PBuiltinList (PAsData PMidgardTxInput) :--> PByteString)
pencodeInputPreimage = phoistAcyclic $ penvelopeMapped pencodeMidgardTxInput

-- | Aiken @preimages.encode_output_preimage@ — field 2.
pencodeOutputPreimage ::
  forall (s :: S). Term s (PBuiltinList (PAsData PMidgardTxOutput) :--> PByteString)
pencodeOutputPreimage = phoistAcyclic $ penvelopeMapped pencodeMidgardTxOutput

{- | Aiken @preimages.encode_hash28_list_preimage@ — fields 3 and 4.

Every item is asserted to be exactly 28 bytes, which is what fixes the stride at
30 and makes item access arithmetic rather than a walk.
-}
pencodeHash28ListPreimage :: forall (s :: S). Term s (PItems :--> PByteString)
pencodeHash28ListPreimage = phoistAcyclic $
  plam $ \items -> pencodeFieldPreimage #$ pmapBytes # pexpectHash28 # items

-- | 'pmapItems' for a list that is already unwrapped bytes.
pmapBytes ::
  forall (s :: S).
  Term s ((PByteString :--> PByteString) :--> PItems :--> PItems)
pmapBytes = phoistAcyclic $
  pfix $ \self -> plam $ \f items ->
    pelimList
      (\item rest -> pcons # (f # item) # (self # f # rest))
      (pcon PNil)
      items

-- | Aiken @preimages.expect_hash28@.
pexpectHash28 :: forall (s :: S). Term s (PByteString :--> PByteString)
pexpectHash28 = phoistAcyclic $
  plam $ \item -> pif (plengthBS # item #== phash28ItemBytes) item perror

-- | Aiken @preimages.encode_address_witness_preimage@ — field 7.
pencodeAddressWitnessPreimage ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PMidgardAddressWitness) :--> PByteString)
pencodeAddressWitnessPreimage =
  phoistAcyclic $ penvelopeMapped pencodeMidgardAddressWitness

{- | Aiken @preimages.encode_script_witness_preimage@ — field 6.

Under the retired counted scheme this concatenated raw item CBOR with no
per-item wrapper. The §5.1 envelope now applies here like everywhere else.
-}
pencodeScriptWitnessPreimage ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PMidgardVersionedScript) :--> PByteString)
pencodeScriptWitnessPreimage =
  phoistAcyclic $ penvelopeMapped pencodeMidgardVersionedScript

-- | Aiken @preimages.encode_redeemer_witness_preimage@ — field 8, same reversion.
pencodeRedeemerWitnessPreimage ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PMidgardRedeemerWitness) :--> PByteString)
pencodeRedeemerWitnessPreimage =
  phoistAcyclic $ penvelopeMapped pencodeMidgardRedeemerWitness

--------------------------------------------------------------------------------
-- Decoders
--------------------------------------------------------------------------------

{- | The shape every decoder below shares: read the §5.1 header, walk exactly
that many items, and demand the walk landed on the end of the input.

The trailing length check is what stops bytes riding along behind the last item
inside something whose hash was checked as a whole.
-}
pdecodeEnvelope ::
  forall (s :: S) (a :: S -> Type).
  PIsListLike PBuiltinList a =>
  Term s (PByteString :--> PInteger :--> PInteger :--> PPair PInteger (PBuiltinList a)) ->
  Term s (PByteString :--> PBuiltinList a)
pdecodeEnvelope walk =
  plam $ \preimageCbor -> P.do
    PPair o1 itemCount <- pmatch (pdecodeFieldArrayHeaderAt # preimageCbor # 0)
    PPair o2 items <- pmatch (walk # preimageCbor # o1 # itemCount)
    pif (o2 #== plengthBS # preimageCbor) items perror

{- | The item walk every decoder below shares: unwrap one @58 LL@ item, hand its
bytes to @readItem@, recurse.
-}
pwalkItems ::
  forall (s :: S) (a :: S -> Type).
  PIsListLike PBuiltinList a =>
  Term s (PByteString :--> a) ->
  Term s (PByteString :--> PInteger :--> PInteger :--> PPair PInteger (PBuiltinList a))
pwalkItems readItem =
  pfix $ \self -> plam $ \preimageCbor offset remaining ->
    pif (remaining #<= 0) (pcon (PPair offset pnil)) $ P.do
      PPair o1 itemCbor <- pmatch (pdecodeDefiniteBytesAt # preimageCbor # offset)
      item <- plet (readItem # itemCbor)
      PPair o2 rest <- pmatch (self # preimageCbor # o1 # (remaining - 1))
      pcon (PPair o2 (pcons # item # rest))

-- | Aiken @preimages.decode_midgard_tx_byte_list_preimage_cbor@.
pdecodeMidgardTxByteListPreimageCbor ::
  forall (s :: S). Term s (PByteString :--> PItems)
pdecodeMidgardTxByteListPreimageCbor =
  phoistAcyclic $ pdecodeEnvelope (pwalkItems (plam (\itemCbor -> itemCbor)))

{- | Aiken @preimages.decode_midgard_tx_hash28_list_preimage_cbor@ — fields 3, 4.

The §5.3 width assertion is enforced on the way in, so the decoder can never
admit an item the encoder would refuse.
-}
pdecodeMidgardTxHash28ListPreimageCbor ::
  forall (s :: S). Term s (PByteString :--> PItems)
pdecodeMidgardTxHash28ListPreimageCbor =
  phoistAcyclic $ pdecodeEnvelope (pwalkItems pexpectHash28)

-- | Aiken @preimages.decode_midgard_tx_inputs_preimage_cbor@.
pdecodeMidgardTxInputsPreimageCbor ::
  forall (s :: S). Term s (PByteString :--> PBuiltinList (PAsData PMidgardTxInput))
pdecodeMidgardTxInputsPreimageCbor =
  phoistAcyclic $
    pdecodeEnvelope (pwalkItems (plam (\itemCbor -> pdata (pdecodeMidgardTxInputCbor # itemCbor))))

-- | Aiken @preimages.decode_midgard_tx_outputs_preimage_cbor@.
pdecodeMidgardTxOutputsPreimageCbor ::
  forall (s :: S). Term s (PByteString :--> PBuiltinList (PAsData PMidgardTxOutput))
pdecodeMidgardTxOutputsPreimageCbor =
  phoistAcyclic $
    pdecodeEnvelope (pwalkItems (plam (\itemCbor -> pdata (pdecodeMidgardTxOutputCbor # itemCbor))))

-- | Aiken @preimages.decode_midgard_tx_address_witnesses_preimage_cbor@.
pdecodeMidgardTxAddressWitnessesPreimageCbor ::
  forall (s :: S). Term s (PByteString :--> PBuiltinList (PAsData PMidgardAddressWitness))
pdecodeMidgardTxAddressWitnessesPreimageCbor =
  phoistAcyclic $
    pdecodeEnvelope (pwalkItems (plam (\itemCbor -> pdata (pdecodeMidgardAddressWitnessCbor # itemCbor))))

{- | Aiken @preimages.decode_midgard_tx_script_witnesses_preimage_cbor@.

The item readers for fields 6 and 8 take an /offset/ decoder rather than a
whole-bytes one, so each carries its own @expect item_offset == length@. The
input and output item decoders check that internally; these two cannot, because
the same functions are used to read out of a longer buffer.
-}
pdecodeMidgardTxScriptWitnessesPreimageCbor ::
  forall (s :: S). Term s (PByteString :--> PBuiltinList (PAsData PMidgardVersionedScript))
pdecodeMidgardTxScriptWitnessesPreimageCbor =
  phoistAcyclic $
    pdecodeEnvelope
      (pwalkItems (plam (\itemCbor -> pdata (pexactly pdecodeMidgardVersionedScriptAt # itemCbor))))

-- | Aiken @preimages.decode_midgard_tx_redeemer_witnesses_preimage_cbor@.
pdecodeMidgardTxRedeemerWitnessesPreimageCbor ::
  forall (s :: S). Term s (PByteString :--> PBuiltinList (PAsData PMidgardRedeemerWitness))
pdecodeMidgardTxRedeemerWitnessesPreimageCbor =
  phoistAcyclic $
    pdecodeEnvelope
      (pwalkItems (plam (\itemCbor -> pdata (pexactly pdecodeMidgardRedeemerWitnessAt # itemCbor))))

-- | Runs an offset decoder over a whole byte string and demands it consumed it all.
pexactly ::
  forall (s :: S) (a :: S -> Type).
  Term s (PByteString :--> PInteger :--> PPair PInteger a) ->
  Term s (PByteString :--> a)
pexactly decode =
  plam $ \itemCbor -> P.do
    PPair itemOffset value <- pmatch (decode # itemCbor # 0)
    pif (itemOffset #== plengthBS # itemCbor) value perror

--------------------------------------------------------------------------------
-- Field 5 — mint
--------------------------------------------------------------------------------

{- | Aiken @preimages.encode_mint_asset_quantities@.

§5.6: asset names within a policy appear in canonical key order and duplicates
reject. The order is carried through the walk as @previous_asset_name@ rather
than checked over a materialised key list — the same shape
'pdecodeMintAssetsAt' uses on the way back in.

Enforced on the producer side and not merely assumed of the caller: the decoder
checks it too, so an encoder that let a descending or duplicated run past would
hand back a preimage that never decodes, which is a producer emitting
uncommittable bytes with no error.
-}
pencodeMintAssetQuantities ::
  forall (s :: S).
  Term s (PDataPairs :--> PMaybe PByteString :--> PByteString)
pencodeMintAssetQuantities = phoistAcyclic $
  pfix $ \self -> plam $ \pairs previousAssetName ->
    pelimList
      ( \pair rest -> P.do
          assetName <- plet (pasByteStr # pfst pair)
          quantity <- plet (pasInt # psnd pair)
          pif
            ( 32
                #< plengthBS
                # assetName
                #|| quantity
                #== 0
                #|| pnot
                # (pfollows # previousAssetName # assetName)
            )
            perror
            ( (pencodeDefiniteBytes # assetName)
                <> pcborInt quantity
                <> (self # rest #$ pcon (PJust assetName))
            )
      )
      (pconstant "")
      pairs

{- | The ordering guard both mint walks share: the first key is always fine, and
every later one must strictly follow its predecessor.
-}
pfollows ::
  forall (s :: S). Term s (PMaybe PByteString :--> PByteString :--> PBool)
pfollows = phoistAcyclic $
  plam $ \previous key -> pmatch previous $ \case
    PNothing -> pconstant True
    PJust p -> pcanonicalBytesKeyPrecedes # p # key

-- | Aiken @preimages.encode_mint_policy_item@ — @[policy_id, asset_map]@.
pencodeMintPolicyItem ::
  forall (s :: S). Term s (PData :--> PData :--> PByteString)
pencodeMintPolicyItem = phoistAcyclic $
  plam $ \policyIdData assetMapData -> P.do
    policyId <- plet (pasByteStr # policyIdData)
    assetPairs <- plet (pasMap # assetMapData)
    pif (plengthBS # policyId #!= 28 #|| pnull # assetPairs) perror $
      pconstant "\x82"
        <> (pencodeDefiniteBytes # policyId)
        <> (pencodeDefiniteMapHeader #$ plength # assetPairs)
        <> (pencodeMintAssetQuantities # assetPairs #$ pcon PNothing)

{- | Aiken @preimages.canonical_mint_policy_pairs@.

An empty mint is the @Data@ /list/ @[]@, not an empty map, so the comparison is
against @List []@ and not against @Map []@. Anything else must be a non-empty
map.
-}
pcanonicalMintPolicyPairs :: forall (s :: S). Term s (PData :--> PDataPairs)
pcanonicalMintPolicyPairs = phoistAcyclic $
  plam $ \mint ->
    pif (mint #== pemptyMintData) (pcon PNil) $
      plet (pasMap # mint) $ \policyPairs ->
        pif (pnull # policyPairs) perror policyPairs

-- | The @Data@ an empty mint is: an empty list, not an empty map.
pemptyMintData :: forall (s :: S). Term s PData
pemptyMintData = pforgetData (pdata (pcon PNil :: Term s (PBuiltinList PData)))

{- | Aiken @preimages.encode_mint_policy_items@.

§5.6's field-level ordering rule, the twin of the asset-name one a level down.
It lives on the run rather than inside 'pencodeMintPolicyItem' because ordering
is a property of the run and no single item can see it — the same split the
decoder makes between 'pdecodeMintPolicyItemsAt' and 'pdecodeMintPolicyItemCbor'.
-}
pencodeMintPolicyItems ::
  forall (s :: S). Term s (PDataPairs :--> PMaybe PByteString :--> PItems)
pencodeMintPolicyItems = phoistAcyclic $
  pfix $ \self -> plam $ \pairs previousPolicyId ->
    pelimList
      ( \pair rest -> P.do
          policyId <- plet (pasByteStr # pfst pair)
          pif (pnot # (pfollows # previousPolicyId # policyId)) perror $
            pcons
              # (pencodeMintPolicyItem # pfst pair # psnd pair)
              # (self # rest #$ pcon (PJust policyId))
      )
      (pcon PNil)
      pairs

-- | Aiken @preimages.mint_policy_items@.
pmintPolicyItems :: forall (s :: S). Term s (PData :--> PItems)
pmintPolicyItems = phoistAcyclic $
  plam $ \mint ->
    pencodeMintPolicyItems # (pcanonicalMintPolicyPairs # mint) #$ pcon PNothing

{- | Aiken @preimages.encode_mint_preimage@.

The field-5 preimage is the enveloped list of per-policy items, and an empty
mint encodes as @80@ like every other field. The retired raw-map form — @a0@
when empty, @a1 …@ otherwise — is prohibited.
-}
pencodeMintPreimage :: forall (s :: S). Term s (PData :--> PByteString)
pencodeMintPreimage = phoistAcyclic $
  plam $ \mint -> pencodeFieldPreimage #$ pmintPolicyItems # mint

{- | Aiken @preimages.decode_midgard_tx_mint_preimage_cbor@.

Rebuilds the mint @Data@ map from the enveloped preimage. Ordering, widths,
non-empty policies and non-zero quantities are all checked on the way through,
so a decoded mint is a canonical mint.
-}
pdecodeMidgardTxMintPreimageCbor :: forall (s :: S). Term s (PByteString :--> PData)
pdecodeMidgardTxMintPreimageCbor = phoistAcyclic $
  plam $ \preimageCbor -> P.do
    PPair o1 itemCount <- pmatch (pdecodeFieldArrayHeaderAt # preimageCbor # 0)
    PPair o2 policyPairs <-
      pmatch (pdecodeMintPolicyItemsAt # preimageCbor # o1 # itemCount #$ pcon PNothing)
    pif (o2 #!= plengthBS # preimageCbor) perror $
      pif
        (itemCount #== 0)
        pemptyMintData
        (pforgetData (pdata (pcon (PAssocMap policyPairs))))

-- | Aiken @preimages.decode_mint_policy_items_at@.
pdecodeMintPolicyItemsAt ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PInteger
        :--> PMaybe PByteString
        :--> PPair PInteger (PBuiltinList (PBuiltinPair (PAsData PByteString) (PAsData PMintAssets)))
    )
pdecodeMintPolicyItemsAt = phoistAcyclic $
  pfix $ \self -> plam $ \preimageCbor offset remaining previousPolicyId ->
    pif (remaining #<= 0) (pcon (PPair offset (pcon PNil))) $ P.do
      PPair o1 itemCbor <- pmatch (pdecodeDefiniteBytesAt # preimageCbor # offset)
      PPair policyId assetPairs <- pmatch (pdecodeMintPolicyItemCbor # itemCbor)
      pif (pnot # (pfollows # previousPolicyId # policyId)) perror $ P.do
        PPair o2 rest <-
          pmatch (self # preimageCbor # o1 # (remaining - 1) #$ pcon (PJust policyId))
        pcon
          ( PPair
              o2
              ( pcons
                  # ( ppairDataBuiltin
                        # pdata policyId
                        # pdata (pcon (PAssocMap assetPairs))
                    )
                  # rest
              )
          )

-- | Aiken @preimages.decode_mint_policy_item_cbor@.
pdecodeMintPolicyItemCbor ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PPair PByteString (PBuiltinList (PBuiltinPair (PAsData PByteString) (PAsData PInteger)))
    )
pdecodeMintPolicyItemCbor = phoistAcyclic $
  plam $ \itemCbor -> P.do
    PPair o1 policyId <-
      pmatch (pdecodeCanonicalBytesAt # itemCbor # (pexpectByte # itemCbor # 0 # 130))
    PPair o2 assetCount <- pmatch (pdecodeCanonicalMapHeaderAt # itemCbor # o1)
    pif (plengthBS # policyId #!= 28 #|| assetCount #<= 0) perror $ P.do
      PPair o3 assetPairs <-
        pmatch (pdecodeMintAssetsAt # itemCbor # o2 # assetCount #$ pcon PNothing)
      pif (o3 #== plengthBS # itemCbor) (pcon (PPair policyId assetPairs)) perror

-- | Aiken @preimages.decode_mint_assets_at@.
pdecodeMintAssetsAt ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PInteger
        :--> PMaybe PByteString
        :--> PPair PInteger (PBuiltinList (PBuiltinPair (PAsData PByteString) (PAsData PInteger)))
    )
pdecodeMintAssetsAt = phoistAcyclic $
  pfix $ \self -> plam $ \itemCbor offset remaining previousAssetName ->
    pif (remaining #<= 0) (pcon (PPair offset (pcon PNil))) $ P.do
      PPair o1 assetName <- pmatch (pdecodeCanonicalBytesAt # itemCbor # offset)
      pif
        ( 32
            #< plengthBS
            # assetName
            #|| pnot
            # (pfollows # previousAssetName # assetName)
        )
        perror
        $ P.do
          PPair o2 quantity <- pmatch (pdecodeCanonicalIntAt # itemCbor # o1)
          pif (quantity #== 0) perror $ P.do
            PPair o3 rest <-
              pmatch (self # itemCbor # o2 # (remaining - 1) #$ pcon (PJust assetName))
            pcon
              ( PPair
                  o3
                  (pcons # (ppairDataBuiltin # pdata assetName # pdata quantity) # rest)
              )

{- | Aiken @preimages.decode_canonical_bytes_at@.

The minimal-width byte-string reader. Each wide form carries a lower bound on
the length it may encode — 24 for the one-byte form, 256 for the two-byte,
65536 for the four-byte — and those bounds are the whole of what "canonical"
means here. 'Midgard.FraudProofs.NativeTx.Codec.pdecodeDefiniteBytesAt' reads
the same grammar without them.
-}
pdecodeCanonicalBytesAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PByteString)
pdecodeCanonicalBytesAt = phoistAcyclic $
  plam $ \bytes offset ->
    plet (pbyteAt # bytes # offset) $ \tag ->
      pif
        (64 #<= tag #&& tag #<= 87)
        (ptake bytes (offset + 1) (tag - 64))
        ( pif
            (tag #== 88)
            ( plet (pbyteAt # bytes # (offset + 1)) $ \len ->
                pif (len #< 24) perror (ptake bytes (offset + 2) len)
            )
            ( pif
                (tag #== 89)
                ( plet (pbeUint bytes (offset + 1) 2) $ \len ->
                    pif (len #<= 0xff) perror (ptake bytes (offset + 3) len)
                )
                ( pif
                    (tag #== 90)
                    ( plet (pbeUint bytes (offset + 1) 4) $ \len ->
                        pif (len #<= 0xffff) perror (ptake bytes (offset + 5) len)
                    )
                    perror
                )
            )
        )
  where
    ptake bytes start len =
      pcon (PPair (start + len) (psliceLen # bytes # start # len))

-- | Aiken @preimages.decode_canonical_map_header_at@ — the map-header twin.
pdecodeCanonicalMapHeaderAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PInteger)
pdecodeCanonicalMapHeaderAt = phoistAcyclic $
  plam $ \bytes offset ->
    plet (pbyteAt # bytes # offset) $ \tag ->
      pif
        (160 #<= tag #&& tag #<= 183)
        (pcon (PPair (offset + 1) (tag - 160)))
        ( pif
            (tag #== 184)
            ( plet (pbyteAt # bytes # (offset + 1)) $ \count ->
                pif (count #< 24) perror (pcon (PPair (offset + 2) count))
            )
            ( pif
                (tag #== 185)
                ( plet (pbeUint bytes (offset + 1) 2) $ \count ->
                    pif (count #<= 0xff) perror (pcon (PPair (offset + 3) count))
                )
                ( pif
                    (tag #== 186)
                    ( plet (pbeUint bytes (offset + 1) 4) $ \count ->
                        pif (count #<= 0xffff) perror (pcon (PPair (offset + 5) count))
                    )
                    perror
                )
            )
        )

-- | A big-endian unsigned integer of the given width, read byte by byte.
pbeUint ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Int ->
  Term s PInteger
pbeUint bytes offset width =
  foldl
    (\acc i -> acc * 256 + (pbyteAt # bytes # (offset + fromIntegral i)))
    0
    [0 .. width - 1]

-- | Aiken @preimages.canonical_bytes_encoded_length@.
pcanonicalBytesEncodedLength ::
  forall (s :: S). Term s (PByteString :--> PInteger)
pcanonicalBytesEncodedLength = phoistAcyclic $
  plam $ \bytes ->
    plet (plengthBS # bytes) $ \len ->
      pif (len #< 24) (1 + len) $
        pif (len #<= 0xff) (2 + len) $
          pif (len #<= 0xffff) (3 + len) (5 + len)

{- | Aiken @preimages.canonical_bytes_key_precedes@.

Canonical CBOR map order: shorter /encoded/ key first, then lexicographic. The
encoded length rather than the payload length is what matters, though for byte
strings the two orderings agree — the header width is a monotone function of the
payload width.
-}
pcanonicalBytesKeyPrecedes ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PBool)
pcanonicalBytesKeyPrecedes = phoistAcyclic $
  plam $ \left right ->
    plet (pcanonicalBytesEncodedLength # left) $ \leftLength ->
      plet (pcanonicalBytesEncodedLength # right) $ \rightLength ->
        pif (leftLength #< rightLength) (pconstant True) $
          pif (rightLength #< leftLength) (pconstant False) (left #< right)

{- | Aiken @preimages.verify_canonical_mint_preimage_cbor@.

Checks the mint preimage against its flat field commitment /without/ first
materialising the whole map as @Data@: the hash is taken over the raw bytes, and
the canonical-CBOR, ordering, non-empty-policy, non-zero-quantity and width rules
are then walked in place. That is what keeps an independently revealed mint
inside the L1 execution-memory envelope.

Note the hash check is an @expect@, not a returned @False@ — a wrong hash fails
the script rather than producing a negative verdict.
-}
pverifyCanonicalMintPreimageCbor ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PBool)
pverifyCanonicalMintPreimageCbor = phoistAcyclic $
  plam $ \preimageCbor expectedHash ->
    pif (pfieldCommitment # preimageCbor #!= expectedHash) perror $ P.do
      PPair o1 itemCount <- pmatch (pdecodeFieldArrayHeaderAt # preimageCbor # 0)
      (pverifyCanonicalMintItemsAt # preimageCbor # o1 # itemCount #$ pcon PNothing)
        #== plengthBS
        # preimageCbor

-- | Aiken @preimages.verify_canonical_mint_items_at@ — the in-place walk.
pverifyCanonicalMintItemsAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PInteger :--> PMaybe PByteString :--> PInteger)
pverifyCanonicalMintItemsAt = phoistAcyclic $
  pfix $ \self -> plam $ \preimageCbor offset remaining previousPolicyId ->
    pif (remaining #<= 0) offset $ P.do
      PPair o1 itemCbor <- pmatch (pdecodeDefiniteBytesAt # preimageCbor # offset)
      PPair policyId _assets <- pmatch (pdecodeMintPolicyItemCbor # itemCbor)
      pif (pnot # (pfollows # previousPolicyId # policyId)) perror $
        self # preimageCbor # o1 # (remaining - 1) #$ pcon (PJust policyId)
