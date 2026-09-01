{- |
Module      : Midgard.FraudProofs.NativeTx.Components
Description : Plutarch port of
              @lib/midgard/fraud-proofs/native-tx/components.ak@.

The per-item codecs: one input, one output, one address, one value, one witness.
Everything above this — @preimages.ak@ and @transaction.ak@ — is these functions
run over a list and hashed.

Two design decisions here are load-bearing and neither is obvious from the code.

=== The output index is deliberately non-minimal

'pencodeFixedOutputIndex' always writes the three-byte @19 XXXX@ form, even for
@0@, which canonical CBOR would spell in a single byte. That is the sole
deliberate departure from minimal encoding in the whole format (spec §5.3), and
it buys /arithmetic item access/: every input item is exactly 38 bytes, so with
the @58 26@ wrapper the stride is 40 and the nth input is at a computed offset
rather than at the end of a walk.

Uniqueness is not waived by picking a different canon. The one-byte forms, the
@18 XX@ form, and every wider form all reject, so each output index still has
exactly one admissible encoding — just not the minimal one.

=== Assets are keyed by a flat unit, not nested by policy

A 'Midgard.FraudProofs.NativeTx.Types.PMidgardAssets' entry is keyed by the
28-byte policy id with the asset name concatenated onto it. The wire format,
however, /is/ nested — a map of policy to a map of asset name to quantity — so
the encoder has to rediscover the policy grouping by scanning for a change of
prefix ('pcountPolicyGroups' and 'pcountPolicyPrefix'), and the decoder has to
flatten it back.

The grouping is by /adjacency/, not by identity. Assets @[A, B, A]@ produce three
groups, not two, and the policy @A@ appears twice in the resulting CBOR map. That
is admitted: encoder and decoder agree on it, and the value's commitment is over
the ordered list rather than over the multiset. The consequence worth knowing is
that two orderings of the same assets are two different values with two different
hashes — the list order is part of what a transaction commits to.
-}
module Midgard.FraudProofs.NativeTx.Components (
  -- * Inputs
  pencodeFixedOutputIndex,
  pencodeMidgardTxInput,
  pdecodeMidgardTxInputCbor,

  -- * Addresses
  pencodeMidgardAddress,
  pdecodeMidgardAddressBytes,

  -- * Values
  passetUnitFromPolicyAsset,
  pencodeMidgardValue,

  -- * Scripts
  pmidgardScriptLanguageToTag,
  pencodeMidgardVersionedScript,
  pdecodeMidgardVersionedScriptAt,
  pdecodeMidgardVersionedScriptData,

  -- * Outputs
  pencodeMidgardTxOutput,
  pdecodeMidgardTxOutputData,
  pdecodeMidgardTxOutputCbor,

  -- * Witnesses
  pencodeMidgardAddressWitness,
  pdecodeMidgardAddressWitnessCbor,
  pencodeMidgardRedeemerWitnessData,
  pencodeMidgardRedeemerWitness,
  pdecodeMidgardRedeemerWitnessAt,
  pdecodeMidgardRedeemerWitnessData,
) where

import Data.Kind (Type)
import Plutarch.Builtin.ByteString (
  pintegerToByteString,
  pmostSignificantFirst,
 )
import Plutarch.Core.Internal.Builtins (pconsBS')
import Plutarch.LedgerApi.AssocMap (PAssocMap (..))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Codec (
  pbyteAt,
  pcborInt,
  pdecodeDefiniteBytesAt,
  pdecodeDefiniteMapHeaderAt,
  pdecodeUintAt,
  pencodeDefiniteBytes,
  pencodeDefiniteMapHeader,
  pexpectByte,
  pexpectH28,
  pexpectH32,
  psliceLen,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddress (..),
  PMidgardAddressWitness (..),
  PMidgardCredential (..),
  PMidgardExecutionUnits (..),
  PMidgardRedeemerPurpose (..),
  PMidgardRedeemerWitness (..),
  PMidgardScriptLanguage (..),
  PMidgardTxInput (..),
  PMidgardTxOutput (..),
  PMidgardValue (..),
  PMidgardVersionedScript (..),
 )

--------------------------------------------------------------------------------
-- Small shared shapes
--------------------------------------------------------------------------------

{- | The flat asset list underneath
'Midgard.FraudProofs.NativeTx.Types.PMidgardAssets', which is what every helper
in the value section actually walks.
-}
type PAssetList = PBuiltinList (PBuiltinPair (PAsData PByteString) (PAsData PInteger))

{- | Inequality. Plutarch has @#==@ but no negated form, and this module reads
much better with one — several of the shape checks below are "reject unless the
byte is exactly this".
-}
(#!=) :: forall (s :: S) (a :: S -> Type). PEq a => Term s a -> Term s a -> Term s PBool
x #!= y = pnot # (x #== y)

infix 4 #!=

-- | The first component of a builtin pair.
pfst :: forall (s :: S) (a :: S -> Type) (b :: S -> Type). Term s (PBuiltinPair a b) -> Term s a
pfst p = pmatch p $ \(PBuiltinPair a _) -> a

-- | The second component of a builtin pair.
psnd :: forall (s :: S) (a :: S -> Type) (b :: S -> Type). Term s (PBuiltinPair a b) -> Term s b
psnd p = pmatch p $ \(PBuiltinPair _ b) -> b

{- | Aiken's @bytearray.from_int_big_endian@, with the width fixed at the call
site. The private twin of the one in "Midgard.FraudProofs.NativeTx.Codec".
-}
pbigEndian ::
  forall (s :: S). Term s PInteger -> Term s PInteger -> Term s PByteString
pbigEndian width n = pintegerToByteString # pmostSignificantFirst # width # n

{- | Aiken's @list.drop@, over the asset list.

The same function as @Midgard.Validators.Witness.pdropList@, repeated rather
than shared because that module is a validator and this one is a codec —
importing it here would point the dependency graph the wrong way. Aiken's
semantics: a non-positive count returns the list unchanged, and dropping past
the end gives the empty list rather than failing.
-}
pdropAssets :: forall (s :: S). Term s (PInteger :--> PAssetList :--> PAssetList)
pdropAssets = phoistAcyclic $
  pfix $ \self -> plam $ \count assets ->
    pif
      (count #<= 0)
      assets
      (pelimList (\_ rest -> self # (count - 1) # rest) assets assets)

--------------------------------------------------------------------------------
-- Inputs
--------------------------------------------------------------------------------

-- | Aiken @components.expect_output_index@ — a @uint16@.
pexpectOutputIndex :: forall (s :: S). Term s (PInteger :--> PInteger)
pexpectOutputIndex = phoistAcyclic $
  plam $ \index -> pif (0 #<= index #&& index #<= 65535) index perror

-- | Aiken @components.expect_asset_name@ — at most 32 bytes.
pexpectAssetName :: forall (s :: S). Term s (PByteString :--> PByteString)
pexpectAssetName = phoistAcyclic $
  plam $ \assetName -> pif (plengthBS # assetName #<= 32) assetName perror

{- | Aiken @components.encode_fixed_output_index@.

The fixed three-byte form. See the module header for why this one encoding is
deliberately not minimal.
-}
pencodeFixedOutputIndex :: forall (s :: S). Term s (PInteger :--> PByteString)
pencodeFixedOutputIndex = phoistAcyclic $
  plam $ \outputIndex ->
    pconstant "\x19" <> pbigEndian 2 (pexpectOutputIndex # outputIndex)

{- | Aiken @components.decode_fixed_output_index_at@.

Pins the @0x19@ head, so the minimal forms and the wider ones all reject.
-}
pdecodeFixedOutputIndexAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PInteger)
pdecodeFixedOutputIndexAt = phoistAcyclic $
  plam $ \bytes offset ->
    pif
      (pbyteAt # bytes # offset #== 25)
      ( pcon
          ( PPair
              (offset + 3)
              ( (pbyteAt # bytes # (offset + 1)) * 256
                  + (pbyteAt # bytes # (offset + 2))
              )
          )
      )
      perror

-- | Aiken @components.encode_midgard_tx_input@ — @[tx_id, output_index]@.
pencodeMidgardTxInput ::
  forall (s :: S). Term s (PMidgardTxInput :--> PByteString)
pencodeMidgardTxInput = phoistAcyclic $
  plam $ \input -> P.do
    PMidgardTxInput {ptxInput'txId, ptxInput'outputIndex} <- pmatch input
    pconstant "\x82"
      <> (pencodeDefiniteBytes #$ pexpectH32 # pfromData ptxInput'txId)
      <> (pencodeFixedOutputIndex # pfromData ptxInput'outputIndex)

{- | Aiken @components.decode_midgard_tx_input_cbor@.

The trailing @expect offset == length@ is what makes an input item exactly its
own bytes — nothing may ride along behind it.
-}
pdecodeMidgardTxInputCbor ::
  forall (s :: S). Term s (PByteString :--> PMidgardTxInput)
pdecodeMidgardTxInputCbor = phoistAcyclic $
  plam $ \inputCbor -> P.do
    PPair o1 txId <-
      pmatch (pdecodeDefiniteBytesAt # inputCbor # (pexpectByte # inputCbor # 0 # 130))
    PPair o2 outputIndex <- pmatch (pdecodeFixedOutputIndexAt # inputCbor # o1)
    pif
      (plengthBS # txId #!= 32 #|| o2 #!= plengthBS # inputCbor)
      perror
      ( pcon
          ( PMidgardTxInput
              { ptxInput'txId = pdata txId
              , ptxInput'outputIndex = pdata outputIndex
              }
          )
      )

--------------------------------------------------------------------------------
-- Addresses
--------------------------------------------------------------------------------

-- | Aiken @components.midgard_credential_hash@ — the hash, checked to be 28 bytes.
pmidgardCredentialHash ::
  forall (s :: S). Term s (PMidgardCredential :--> PByteString)
pmidgardCredentialHash = phoistAcyclic $
  plam $ \credential -> pmatch credential $ \case
    PMidgardPubKeyCredential hash -> pexpectH28 # pfromData hash
    PMidgardScriptCredential hash -> pexpectH28 # pfromData hash

-- | Aiken @components.midgard_credential_is_script@.
pmidgardCredentialIsScript ::
  forall (s :: S). Term s (PMidgardCredential :--> PBool)
pmidgardCredentialIsScript = phoistAcyclic $
  plam $ \credential -> pmatch credential $ \case
    PMidgardPubKeyCredential _ -> pconstant False
    PMidgardScriptCredential _ -> pconstant True

-- | Aiken @components.midgard_credential_from_hash@.
pmidgardCredentialFromHash ::
  forall (s :: S). Term s (PBool :--> PByteString :--> PMidgardCredential)
pmidgardCredentialFromHash = phoistAcyclic $
  plam $ \isScript hash ->
    pif
      isScript
      (pcon (PMidgardScriptCredential (pdata hash)))
      (pcon (PMidgardPubKeyCredential (pdata hash)))

{- | Aiken @components.encode_midgard_address@.

The header nibbles: the high one is the address type (payment and stake
script-ness, or 6\/7 for the no-stake forms), the low one the network id with
Midgard's @protected@ bit added on top. The payload is the header byte followed
by the payment hash and, when present, the stake hash — 29 or 57 bytes.
-}
pencodeMidgardAddress ::
  forall (s :: S). Term s (PMidgardAddress :--> PByteString)
pencodeMidgardAddress = phoistAcyclic $
  plam $ \address -> P.do
    PMidgardAddress
      { paddress'protected
      , paddress'networkId
      , paddress'paymentCredential
      , paddress'stakeCredential
      } <-
      pmatch address
    networkId <- plet (pfromData paddress'networkId)
    pif (networkId #!= 0 #&& networkId #!= 1) perror $ P.do
      paymentCredential <- plet (pfromData paddress'paymentCredential)
      stakeCredential <- plet (pfromData paddress'stakeCredential)
      paymentIsScript <- plet (pmidgardCredentialIsScript # paymentCredential)
      addressType <-
        plet
          ( pmatch stakeCredential $ \case
              PDJust stake ->
                pif paymentIsScript 1 0
                  + pif (pmidgardCredentialIsScript # pfromData stake) 2 0
              PDNothing -> pif paymentIsScript 7 6
          )
      header <-
        plet
          ( addressType * 16
              + networkId
              + pif (pfromData paddress'protected) 8 0
          )
      paymentAddress <-
        plet (pconsBS' # header #$ pmidgardCredentialHash # paymentCredential)
      pmatch stakeCredential $ \case
        PDJust stake ->
          paymentAddress <> (pmidgardCredentialHash # pfromData stake)
        PDNothing -> paymentAddress

{- | Aiken @components.decode_midgard_address_payload@.

Note the cross-checks between length and address type: a 57-byte payload must
carry a type of 3 or less, and a 29-byte one a type of 6 or 7. Without them the
same bytes would decode under more than one reading of the header.
-}
pdecodeMidgardAddressPayload ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PInteger :--> PMidgardAddress)
pdecodeMidgardAddressPayload = phoistAcyclic $
  plam $ \bytes offset len ->
    pif (len #!= 29 #&& len #!= 57) perror $ P.do
      header <- plet (pbyteAt # bytes # offset)
      addressType <- plet (pdiv # header # 16)
      networkNibble <- plet (header - addressType * 16)
      protected <- plet (8 #<= networkNibble)
      networkId <- plet (pif protected (networkNibble - 8) networkNibble)
      pif (networkId #!= 0 #&& networkId #!= 1) perror $ P.do
        paymentIsScript <-
          plet (addressType #== 1 #|| addressType #== 3 #|| addressType #== 7)
        paymentCredential <-
          plet
            ( pmidgardCredentialFromHash
                # paymentIsScript
                #$ psliceLen
                # bytes
                # (offset + 1)
                # 28
            )
        stakeCredential <-
          plet
            ( pif
                (len #== 57)
                ( pif
                    (addressType #<= 3)
                    ( pcon . PDJust . pdata $
                        pmidgardCredentialFromHash
                          # (addressType #== 2 #|| addressType #== 3)
                          #$ psliceLen
                          # bytes
                          # (offset + 29)
                          # 28
                    )
                    perror
                )
                ( pif
                    (addressType #== 6 #|| addressType #== 7)
                    (pcon PDNothing)
                    perror
                )
            )
        pcon
          ( PMidgardAddress
              { paddress'protected = pdata protected
              , paddress'networkId = pdata networkId
              , paddress'paymentCredential = pdata paymentCredential
              , paddress'stakeCredential = pdata stakeCredential
              }
          )

-- | Aiken @components.decode_midgard_address_bytes@ — the whole string is the payload.
pdecodeMidgardAddressBytes ::
  forall (s :: S). Term s (PByteString :--> PMidgardAddress)
pdecodeMidgardAddressBytes = phoistAcyclic $
  plam $ \addressBytes ->
    pdecodeMidgardAddressPayload
      # addressBytes
      # 0
      # (plengthBS # addressBytes)

{- | Aiken @components.decode_midgard_address_at@.

Pins the @0x58@ one-byte-length head. Both admissible payload lengths, 29 and
57, need that head and no other, so accepting the wider forms would give a second
encoding of the same address.
-}
pdecodeMidgardAddressAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PMidgardAddress)
pdecodeMidgardAddressAt = phoistAcyclic $
  plam $ \bytes offset ->
    pif (pbyteAt # bytes # offset #!= 88) perror $
      plet (pbyteAt # bytes # (offset + 1)) $ \len ->
        pcon
          ( PPair
              (offset + 2 + len)
              (pdecodeMidgardAddressPayload # bytes # (offset + 2) # len)
          )

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

-- | Aiken @components.expect_asset_unit@ — a policy id plus at most 32 name bytes.
pexpectAssetUnit :: forall (s :: S). Term s (PByteString :--> PByteString)
pexpectAssetUnit = phoistAcyclic $
  plam $ \unit ->
    plet (plengthBS # unit) $ \len ->
      pif (28 #<= len #&& len #<= 60) unit perror

-- | Aiken @components.asset_unit_from_policy_asset@.
passetUnitFromPolicyAsset ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PByteString)
passetUnitFromPolicyAsset = phoistAcyclic $
  plam $ \policyId assetName ->
    (pexpectH28 # policyId) <> (pexpectAssetName # assetName)

{- | Aiken @components.asset_unit_from_decoded_policy_asset@.

The policy id is unchecked here because it came out of a decoder that already
pinned its width — checking it again would be dead weight on every asset.
-}
passetUnitFromDecodedPolicyAsset ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PByteString)
passetUnitFromDecodedPolicyAsset = phoistAcyclic $
  plam $ \policyId assetName -> policyId <> (pexpectAssetName # assetName)

-- | Aiken @components.asset_unit_policy_id@.
passetUnitPolicyId :: forall (s :: S). Term s (PByteString :--> PByteString)
passetUnitPolicyId = phoistAcyclic $
  plam $ \unit -> psliceLen # (pexpectAssetUnit # unit) # 0 # 28

-- | Aiken @components.asset_unit_name@.
passetUnitName :: forall (s :: S). Term s (PByteString :--> PByteString)
passetUnitName = phoistAcyclic $
  plam $ \unit ->
    plet (pexpectAssetUnit # unit) $ \checked ->
      psliceLen # checked # 28 # (plengthBS # checked - 28)

-- | Aiken @components.decode_h28_at@ — the pinned @58 1c@ head and its 28 bytes.
pdecodeH28At ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PByteString)
pdecodeH28At = phoistAcyclic $
  plam $ \bytes offset ->
    pif
      (pbyteAt # bytes # offset #!= 88 #|| pbyteAt # bytes # (offset + 1) #!= 28)
      perror
      (pcon (PPair (offset + 30) (psliceLen # bytes # (offset + 2) # 28)))

{- | Aiken @components.decode_midgard_asset_quantities@.

Reads one policy's tokens out of a @Data@ map and appends @tail@ behind them, so
the flattened list comes out in wire order without a reversal.
-}
pdecodeMidgardAssetQuantities ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PBuiltinList (PBuiltinPair PData PData)
        :--> PAssetList
        :--> PAssetList
    )
pdecodeMidgardAssetQuantities = phoistAcyclic $
  pfix $ \self -> plam $ \policyId pairs tl ->
    pelimList
      ( \pair rest -> P.do
          assetName <- plet (pasByteStr # pfst pair)
          quantity <- plet (pasInt # psnd pair)
          pif (quantity #<= 0) perror $
            pcons
              # ( ppairDataBuiltin
                    # pdata (passetUnitFromDecodedPolicyAsset # policyId # assetName)
                    # pdata quantity
                )
              # (self # policyId # rest # tl)
      )
      tl
      pairs

{- | Aiken @components.decode_midgard_policy_assets@.

@expect [_, ..] = asset_pairs@ is the one that matters: an empty token map for a
policy would be a second encoding of a value that omits the policy entirely.
-}
pdecodeMidgardPolicyAssets ::
  forall (s :: S).
  Term s (PBuiltinList (PBuiltinPair PData PData) :--> PAssetList)
pdecodeMidgardPolicyAssets = phoistAcyclic $
  pfix $ \self -> plam $ \pairs ->
    pelimList
      ( \pair rest -> P.do
          policyId <- plet (pexpectH28 #$ pasByteStr # pfst pair)
          assetPairs <- plet (pasMap # psnd pair)
          pif (pnull # assetPairs) perror $
            pdecodeMidgardAssetQuantities
              # policyId
              # assetPairs
              # (self # rest)
      )
      (pcon PNil)
      pairs

{- | Aiken @components.encode_midgard_asset_quantities@.

Writes exactly @remaining@ entries and demands every one of them still belong to
@policy_id@. That demand is defensive rather than reachable: 'pcountPolicyPrefix'
computed @remaining@ from the same prefix, so the entries always match. It is
kept because it is what makes the count and the bytes provably agree.
-}
pencodeMidgardAssetQuantities ::
  forall (s :: S).
  Term s (PByteString :--> PAssetList :--> PInteger :--> PByteString)
pencodeMidgardAssetQuantities = phoistAcyclic $
  pfix $ \self -> plam $ \policyId assets remaining ->
    pif (remaining #<= 0) (pconstant "") $
      pelimList
        ( \asset rest -> P.do
            unit <- plet (pfromData (pfst asset))
            quantity <- plet (pfromData (psnd asset))
            pif
              (passetUnitPolicyId # unit #!= policyId #|| quantity #<= 0)
              perror
              ( (pencodeDefiniteBytes #$ passetUnitName # unit)
                  <> pcborInt quantity
                  <> (self # policyId # rest # (remaining - 1))
              )
        )
        perror
        assets

-- | Aiken @components.count_policy_prefix@ — how many leading entries share a policy.
pcountPolicyPrefix ::
  forall (s :: S). Term s (PByteString :--> PAssetList :--> PInteger)
pcountPolicyPrefix = phoistAcyclic $
  pfix $ \self -> plam $ \policyId assets ->
    pelimList
      ( \asset rest ->
          pif
            (passetUnitPolicyId # pfromData (pfst asset) #== policyId)
            (1 + self # policyId # rest)
            0
      )
      0
      assets

{- | Aiken @components.count_policy_groups@.

Counts /runs/, not distinct policies: a policy that reappears after another one
counts twice, and the CBOR map it produces then has a repeated key. Nothing
rejects that, and nothing needs to — the header count and the body agree, so it
round-trips. See the module header.
-}
pcountPolicyGroups ::
  forall (s :: S).
  Term s (PAssetList :--> PMaybe PByteString :--> PInteger)
pcountPolicyGroups = phoistAcyclic $
  pfix $ \self -> plam $ \assets previousPolicyId ->
    pelimList
      ( \asset rest -> P.do
          policyId <- plet (passetUnitPolicyId # pfromData (pfst asset))
          increment <-
            plet
              ( pmatch previousPolicyId $ \case
                  PJust previous -> pif (policyId #== previous) 0 1
                  PNothing -> 1
              )
          increment + (self # rest #$ pcon (PJust policyId))
      )
      0
      assets

-- | Aiken @components.encode_midgard_policy_assets_body@ — one group per pass.
pencodeMidgardPolicyAssetsBody ::
  forall (s :: S). Term s (PAssetList :--> PByteString)
pencodeMidgardPolicyAssetsBody = phoistAcyclic $
  pfix $ \self -> plam $ \assets ->
    pelimList
      ( \asset _ -> P.do
          policyId <- plet (passetUnitPolicyId # pfromData (pfst asset))
          assetCount <- plet (pcountPolicyPrefix # policyId # assets)
          (pencodeDefiniteBytes # policyId)
            <> (pencodeDefiniteMapHeader # assetCount)
            <> (pencodeMidgardAssetQuantities # policyId # assets # assetCount)
            <> (self #$ pdropAssets # assetCount # assets)
      )
      (pconstant "")
      assets

-- | Aiken @components.encode_midgard_policy_assets@.
pencodeMidgardPolicyAssets ::
  forall (s :: S). Term s (PAssetList :--> PByteString)
pencodeMidgardPolicyAssets = phoistAcyclic $
  plam $ \assets ->
    (pencodeDefiniteMapHeader #$ pcountPolicyGroups # assets # pcon PNothing)
      <> (pencodeMidgardPolicyAssetsBody # assets)

-- | Aiken @components.encode_midgard_value@ — @[lovelace, assets]@.
pencodeMidgardValue :: forall (s :: S). Term s (PMidgardValue :--> PByteString)
pencodeMidgardValue = phoistAcyclic $
  plam $ \value -> P.do
    PMidgardValue {pvalue'lovelace, pvalue'assets} <- pmatch value
    lovelace <- plet (pfromData pvalue'lovelace)
    pif (lovelace #< 0) perror $
      pconstant "\x82"
        <> pcborInt lovelace
        <> (pencodeMidgardPolicyAssets #$ pto (pfromData pvalue'assets))

-- | Aiken @components.decode_midgard_value_data@.
pdecodeMidgardValueData :: forall (s :: S). Term s (PData :--> PMidgardValue)
pdecodeMidgardValueData = phoistAcyclic $
  plam $ \dat ->
    pelimList
      ( \lovelaceData rest ->
          pelimList
            ( \assetsData rest' ->
                pif (pnot # (pnull # rest')) perror $
                  plet (pasInt # lovelaceData) $ \lovelace ->
                    pif (lovelace #< 0) perror $
                      pcon
                        ( PMidgardValue
                            { pvalue'lovelace = pdata lovelace
                            , pvalue'assets =
                                pdata . pcon . PAssocMap $
                                  pdecodeMidgardPolicyAssets #$ pasMap # assetsData
                            }
                        )
            )
            perror
            rest
      )
      perror
      (pasList # dat)

{- | Aiken @components.append_assets@.

Plain concatenation, written out because the recursion below builds each policy's
tokens before it knows the rest.
-}
pappendAssets ::
  forall (s :: S). Term s (PAssetList :--> PAssetList :--> PAssetList)
pappendAssets = phoistAcyclic $
  pfix $ \self -> plam $ \left right ->
    pelimList
      (\asset rest -> pcons # asset # (self # rest # right))
      right
      left

-- | Aiken @components.decode_midgard_asset_quantities_at@.
pdecodeMidgardAssetQuantitiesAt ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PInteger
        :--> PByteString
        :--> PPair PInteger PAssetList
    )
pdecodeMidgardAssetQuantitiesAt = phoistAcyclic $
  pfix $ \self -> plam $ \bytes offset remaining policyId ->
    pif (remaining #<= 0) (pcon (PPair offset (pcon PNil))) $ P.do
      PPair o1 assetName <- pmatch (pdecodeDefiniteBytesAt # bytes # offset)
      PPair o2 quantity <- pmatch (pdecodeUintAt # bytes # o1)
      pif (32 #< plengthBS # assetName #|| quantity #<= 0) perror $ P.do
        PPair o3 rest <- pmatch (self # bytes # o2 # (remaining - 1) # policyId)
        pcon
          ( PPair
              o3
              ( pcons
                  # (ppairDataBuiltin # pdata (policyId <> assetName) # pdata quantity)
                  # rest
              )
          )

-- | Aiken @components.decode_midgard_policy_assets_at@.
pdecodeMidgardPolicyAssetsAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PInteger :--> PPair PInteger PAssetList)
pdecodeMidgardPolicyAssetsAt = phoistAcyclic $
  pfix $ \self -> plam $ \bytes offset remaining ->
    pif (remaining #<= 0) (pcon (PPair offset (pcon PNil))) $ P.do
      PPair o1 policyId <- pmatch (pdecodeH28At # bytes # offset)
      PPair o2 assetCount <- pmatch (pdecodeDefiniteMapHeaderAt # bytes # o1)
      pif (assetCount #<= 0) perror $ P.do
        PPair o3 policyAssets <-
          pmatch (pdecodeMidgardAssetQuantitiesAt # bytes # o2 # assetCount # policyId)
        PPair o4 restAssets <- pmatch (self # bytes # o3 # (remaining - 1))
        pcon (PPair o4 (pappendAssets # policyAssets # restAssets))

-- | Aiken @components.decode_midgard_value_at@.
pdecodeMidgardValueAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PMidgardValue)
pdecodeMidgardValueAt = phoistAcyclic $
  plam $ \bytes offset -> P.do
    PPair o1 lovelace <-
      pmatch (pdecodeUintAt # bytes # (pexpectByte # bytes # offset # 130))
    PPair o2 policyCount <- pmatch (pdecodeDefiniteMapHeaderAt # bytes # o1)
    PPair o3 assets <- pmatch (pdecodeMidgardPolicyAssetsAt # bytes # o2 # policyCount)
    pcon
      ( PPair
          o3
          ( pcon
              ( PMidgardValue
                  { pvalue'lovelace = pdata lovelace
                  , pvalue'assets = pdata (pcon (PAssocMap assets))
                  }
              )
          )
      )

--------------------------------------------------------------------------------
-- Scripts
--------------------------------------------------------------------------------

{- | Aiken @components.midgard_script_language_to_tag@.

@0@, @3@ and @128@ — not the constructor indices. The first two match the
ledger's own language numbering, and @128@ sits above it so a Midgard-only
language can never be mistaken for a future L1 one.
-}
pmidgardScriptLanguageToTag ::
  forall (s :: S). Term s (PMidgardScriptLanguage :--> PInteger)
pmidgardScriptLanguageToTag = phoistAcyclic $
  plam $ \language -> pmatch language $ \case
    PNativeCardanoScript -> 0
    PPlutusV3Script -> 3
    PMidgardV1Script -> 128

-- | Aiken @components.midgard_script_language_from_tag@.
pmidgardScriptLanguageFromTag ::
  forall (s :: S). Term s (PInteger :--> PMidgardScriptLanguage)
pmidgardScriptLanguageFromTag = phoistAcyclic $
  plam $ \tag ->
    pif (tag #== 0) (pcon PNativeCardanoScript) $
      pif (tag #== 3) (pcon PPlutusV3Script) $
        pif (tag #== 128) (pcon PMidgardV1Script) perror

{- | Aiken @components.decode_midgard_script_language_tag_at@.

Two widths, because the tags straddle the CBOR boundary: @0@ and @3@ are single
bytes, while @128@ needs the @18 80@ form. The reader accepts the wide form for
/any/ value, so 'pmidgardScriptLanguageFromTag' is what rejects the rest.
-}
pdecodeMidgardScriptLanguageTagAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PInteger)
pdecodeMidgardScriptLanguageTagAt = phoistAcyclic $
  plam $ \bytes offset ->
    plet (pbyteAt # bytes # offset) $ \tag ->
      pif
        (tag #== 0 #|| tag #== 3)
        (pcon (PPair (offset + 1) tag))
        ( pif
            (tag #== 24)
            (pcon (PPair (offset + 2) (pbyteAt # bytes # (offset + 1))))
            perror
        )

-- | Aiken @components.decode_midgard_versioned_script_at@.
pdecodeMidgardVersionedScriptAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PMidgardVersionedScript)
pdecodeMidgardVersionedScriptAt = phoistAcyclic $
  plam $ \bytes offset -> P.do
    PPair o1 tag <-
      pmatch (pdecodeMidgardScriptLanguageTagAt # bytes # (pexpectByte # bytes # offset # 130))
    PPair o2 scriptBytes <- pmatch (pdecodeDefiniteBytesAt # bytes # o1)
    pcon
      ( PPair
          o2
          ( pcon
              ( PMidgardVersionedScript
                  { pversionedScript'language =
                      pdata (pmidgardScriptLanguageFromTag # tag)
                  , pversionedScript'scriptBytes = pdata scriptBytes
                  }
              )
          )
      )

-- | Aiken @components.encode_midgard_versioned_script@ — @[tag, script_bytes]@.
pencodeMidgardVersionedScript ::
  forall (s :: S). Term s (PMidgardVersionedScript :--> PByteString)
pencodeMidgardVersionedScript = phoistAcyclic $
  plam $ \script -> P.do
    PMidgardVersionedScript
      {pversionedScript'language, pversionedScript'scriptBytes} <-
      pmatch script
    pconstant "\x82"
      <> pcborInt (pmidgardScriptLanguageToTag # pfromData pversionedScript'language)
      <> (pencodeDefiniteBytes # pfromData pversionedScript'scriptBytes)

-- | Aiken @components.decode_midgard_versioned_script_data@.
pdecodeMidgardVersionedScriptData ::
  forall (s :: S). Term s (PData :--> PMidgardVersionedScript)
pdecodeMidgardVersionedScriptData = phoistAcyclic $
  plam $ \dat ->
    pelimList
      ( \tagData rest ->
          pelimList
            ( \scriptBytesData rest' ->
                pif (pnot # (pnull # rest')) perror $
                  pcon
                    ( PMidgardVersionedScript
                        { pversionedScript'language =
                            pdata (pmidgardScriptLanguageFromTag #$ pasInt # tagData)
                        , pversionedScript'scriptBytes =
                            pdata (pasByteStr # scriptBytesData)
                        }
                    )
            )
            perror
            rest
      )
      perror
      (pasList # dat)

--------------------------------------------------------------------------------
-- Outputs
--------------------------------------------------------------------------------

{- | Aiken @components.encode_midgard_tx_output@.

A CBOR map keyed @0@ address, @1@ value, @2@ datum, @3@ script reference, with
the optional entries simply absent when they are @None@ — so the map header is
@a2@, @a3@ or @a4@ and the keys stay ascending. There is no @null@ placeholder,
which is what keeps one output from having two encodings.
-}
pencodeMidgardTxOutput ::
  forall (s :: S). Term s (PMidgardTxOutput :--> PByteString)
pencodeMidgardTxOutput = phoistAcyclic $
  plam $ \output -> P.do
    PMidgardTxOutput
      {ptxOutput'address, ptxOutput'value, ptxOutput'datumCbor, ptxOutput'scriptRef} <-
      pmatch output
    required <-
      plet
        ( pkey 0
            <> (pencodeDefiniteBytes #$ pencodeMidgardAddress # pfromData ptxOutput'address)
            <> pkey 1
            <> (pencodeMidgardValue # pfromData ptxOutput'value)
        )
    pmatch (pfromData ptxOutput'datumCbor) $ \case
      PDJust datumCbor ->
        plet (pkey 2 <> (pencodeDefiniteBytes # pfromData datumCbor)) $ \datumEntry ->
          pmatch (pfromData ptxOutput'scriptRef) $ \case
            PDJust scriptRef ->
              pconstant "\xa4" <> required <> datumEntry <> pscriptEntry scriptRef
            PDNothing -> pconstant "\xa3" <> required <> datumEntry
      PDNothing ->
        pmatch (pfromData ptxOutput'scriptRef) $ \case
          PDJust scriptRef ->
            pconstant "\xa3" <> required <> pscriptEntry scriptRef
          PDNothing -> pconstant "\xa2" <> required
  where
    pscriptEntry scriptRef =
      pkey 3 <> (pencodeMidgardVersionedScript # pfromData scriptRef)

{- | Aiken's @cbor.serialise(n)@ for the four small map keys.

Written as a literal because the keys are fixed: @0@ through @3@ each serialise
to their own byte.
-}
pkey :: forall (s :: S). Integer -> Term s PByteString
pkey n = pconsBS' # pconstant n # pconstant ""

{- | Aiken @components.decode_midgard_tx_output_data@.

The arity of the map decides the shape, and the keys are pinned in every arm, so
a three-entry map can be @0,1,2@ or @0,1,3@ and nothing else.
-}
pdecodeMidgardTxOutputData ::
  forall (s :: S). Term s (PData :--> PMidgardTxOutput)
pdecodeMidgardTxOutputData = phoistAcyclic $
  plam $ \dat ->
    pelimList
      ( \e0 r0 ->
          pelimList
            ( \e1 r1 ->
                pif (pasInt # pfst e0 #!= 0 #|| pasInt # pfst e1 #!= 1) perror $ P.do
                  address <- plet (pdecodeMidgardAddressBytes #$ pasByteStr # psnd e0)
                  value <- plet (pdecodeMidgardValueData # psnd e1)
                  pelimList
                    ( \e2 r2 ->
                        plet (pasInt # pfst e2) $ \extraKey ->
                          pelimList
                            ( \e3 r3 ->
                                pif
                                  ( pnot
                                      # (pnull # r3)
                                      #|| extraKey
                                      #!= 2
                                      #|| pasInt
                                      # pfst e3
                                      #!= 3
                                  )
                                  perror
                                  ( pmkOutput
                                      address
                                      value
                                      (pcon (PDJust (pdata (pasByteStr # psnd e2))))
                                      ( pcon . PDJust . pdata $
                                          pdecodeMidgardVersionedScriptData # psnd e3
                                      )
                                  )
                            )
                            ( pif
                                (extraKey #== 2)
                                ( pmkOutput
                                    address
                                    value
                                    (pcon (PDJust (pdata (pasByteStr # psnd e2))))
                                    (pcon PDNothing)
                                )
                                ( pif
                                    (extraKey #== 3)
                                    ( pmkOutput
                                        address
                                        value
                                        (pcon PDNothing)
                                        ( pcon . PDJust . pdata $
                                            pdecodeMidgardVersionedScriptData # psnd e2
                                        )
                                    )
                                    perror
                                )
                            )
                            r2
                    )
                    (pmkOutput address value (pcon PDNothing) (pcon PDNothing))
                    r1
            )
            perror
            r0
      )
      perror
      (pasMap # dat)

-- | The four-field constructor, shared by every arm of the two output decoders.
pmkOutput ::
  forall (s :: S).
  Term s PMidgardAddress ->
  Term s PMidgardValue ->
  Term s (PMaybeData PByteString) ->
  Term s (PMaybeData PMidgardVersionedScript) ->
  Term s PMidgardTxOutput
pmkOutput address value datumCbor scriptRef =
  pcon
    ( PMidgardTxOutput
        { ptxOutput'address = pdata address
        , ptxOutput'value = pdata value
        , ptxOutput'datumCbor = pdata datumCbor
        , ptxOutput'scriptRef = pdata scriptRef
        }
    )

{- | Aiken @components.decode_midgard_tx_output_cbor@.

The byte-level twin of 'pdecodeMidgardTxOutputData'. It reads the entry count
straight out of the map header — @a2@, @a3@ or @a4@ and nothing else, since
anything wider would be a second encoding of a map this small — and then checks
the offset landed exactly on the end in every arm.
-}
pdecodeMidgardTxOutputCbor ::
  forall (s :: S). Term s (PByteString :--> PMidgardTxOutput)
pdecodeMidgardTxOutputCbor = phoistAcyclic $
  plam $ \outputCbor -> P.do
    entryCountTag <- plet (pbyteAt # outputCbor # 0)
    pif (entryCountTag #< 162 #|| 164 #< entryCountTag) perror $ P.do
      entryCount <- plet (entryCountTag - 160)
      end <- plet (plengthBS # outputCbor)
      PPair o1 address <-
        pmatch (pdecodeMidgardAddressAt # outputCbor # (pexpectByte # outputCbor # 1 # 0))
      PPair o2 value <-
        pmatch (pdecodeMidgardValueAt # outputCbor # (pexpectByte # outputCbor # o1 # 1))
      let twoEntries =
            pexactly (o2 #== end) $
              pmkOutput address value (pcon PDNothing) (pcon PDNothing)
          withDatum extraOffset = P.do
            PPair o4 datumCbor <- pmatch (pdecodeDefiniteBytesAt # outputCbor # extraOffset)
            datum <- plet (pcon (PDJust (pdata datumCbor)))
            pif
              (entryCount #== 3)
              (pexactly (o4 #== end) $ pmkOutput address value datum (pcon PDNothing))
              ( P.do
                  PPair o5 scriptRef <-
                    pmatch
                      ( pdecodeMidgardVersionedScriptAt
                          # outputCbor
                          # (pexpectByte # outputCbor # o4 # 3)
                      )
                  pexactly (o5 #== end) $
                    pmkOutput address value datum (pcon (PDJust (pdata scriptRef)))
              )
          withScriptOnly extraOffset = P.do
            PPair o4 scriptRef <-
              pmatch (pdecodeMidgardVersionedScriptAt # outputCbor # extraOffset)
            pexactly (o4 #== end) $
              pmkOutput address value (pcon PDNothing) (pcon (PDJust (pdata scriptRef)))
      pif (entryCount #== 2) twoEntries $ P.do
        extraKey <- plet (pbyteAt # outputCbor # o2)
        o3 <- plet (o2 + 1)
        pif (extraKey #== 2) (withDatum o3) $
          pif (extraKey #!= 3 #|| entryCount #!= 3) perror (withScriptOnly o3)
  where
    pexactly cond result = pif cond result perror

--------------------------------------------------------------------------------
-- Witnesses
--------------------------------------------------------------------------------

-- | Aiken @components.encode_midgard_address_witness@ — a 32-byte key, a 64-byte signature.
pencodeMidgardAddressWitness ::
  forall (s :: S). Term s (PMidgardAddressWitness :--> PByteString)
pencodeMidgardAddressWitness = phoistAcyclic $
  plam $ \witness -> P.do
    PMidgardAddressWitness
      {paddressWitness'verificationKey, paddressWitness'signature} <-
      pmatch witness
    verificationKey <- plet (pfromData paddressWitness'verificationKey)
    signature <- plet (pfromData paddressWitness'signature)
    pif
      (plengthBS # verificationKey #!= 32 #|| plengthBS # signature #!= 64)
      perror
      ( pconstant "\x82"
          <> (pencodeDefiniteBytes # verificationKey)
          <> (pencodeDefiniteBytes # signature)
      )

-- | Aiken @components.decode_midgard_address_witness_cbor@.
pdecodeMidgardAddressWitnessCbor ::
  forall (s :: S). Term s (PByteString :--> PMidgardAddressWitness)
pdecodeMidgardAddressWitnessCbor = phoistAcyclic $
  plam $ \witnessCbor -> P.do
    PPair o1 verificationKey <-
      pmatch (pdecodeDefiniteBytesAt # witnessCbor # (pexpectByte # witnessCbor # 0 # 130))
    PPair o2 signature <- pmatch (pdecodeDefiniteBytesAt # witnessCbor # o1)
    pif
      ( plengthBS
          # verificationKey
          #!= 32
          #|| plengthBS
          # signature
          #!= 64
          #|| o2
          #!= plengthBS
          # witnessCbor
      )
      perror
      ( pcon
          ( PMidgardAddressWitness
              { paddressWitness'verificationKey = pdata verificationKey
              , paddressWitness'signature = pdata signature
              }
          )
      )

-- | Aiken @components.midgard_redeemer_purpose_to_tag@ — 0 through 6, in declared order.
pmidgardRedeemerPurposeToTag ::
  forall (s :: S). Term s (PMidgardRedeemerPurpose :--> PInteger)
pmidgardRedeemerPurposeToTag = phoistAcyclic $
  plam $ \purpose -> pmatch purpose $ \case
    PSpendRedeemer -> 0
    PMintRedeemer -> 1
    PCertRedeemer -> 2
    PRewardRedeemer -> 3
    PVoteRedeemer -> 4
    PProposeRedeemer -> 5
    PReceiveRedeemer -> 6

-- | Aiken @components.midgard_redeemer_purpose_from_tag@.
pmidgardRedeemerPurposeFromTag ::
  forall (s :: S). Term s (PInteger :--> PMidgardRedeemerPurpose)
pmidgardRedeemerPurposeFromTag = phoistAcyclic $
  plam $ \tag ->
    pif (tag #== 0) (pcon PSpendRedeemer) $
      pif (tag #== 1) (pcon PMintRedeemer) $
        pif (tag #== 2) (pcon PCertRedeemer) $
          pif (tag #== 3) (pcon PRewardRedeemer) $
            pif (tag #== 4) (pcon PVoteRedeemer) $
              pif (tag #== 5) (pcon PProposeRedeemer) $
                pif (tag #== 6) (pcon PReceiveRedeemer) perror

{- | Aiken @components.decode_midgard_redeemer_purpose_tag_at@.

One raw byte, with no width ladder at all — every admissible tag is @0..6@, which
is its own CBOR encoding, so 'pmidgardRedeemerPurposeFromTag' rejects everything
else.
-}
pdecodeMidgardRedeemerPurposeTagAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PInteger)
pdecodeMidgardRedeemerPurposeTagAt = phoistAcyclic $
  plam $ \bytes offset ->
    pcon (PPair (offset + 1) (pbyteAt # bytes # offset))

-- | Aiken @components.encode_midgard_redeemer_witness_data@.
pencodeMidgardRedeemerWitnessData ::
  forall (s :: S). Term s (PMidgardRedeemerWitness :--> PData)
pencodeMidgardRedeemerWitnessData = phoistAcyclic $
  plam $ \witness -> P.do
    PMidgardRedeemerWitness
      { predeemerWitness'purpose
      , predeemerWitness'index
      , predeemerWitness'redeemerCbor
      , predeemerWitness'executionUnits
      } <-
      pmatch witness
    PMidgardExecutionUnits {pexecutionUnits'memory, pexecutionUnits'steps} <-
      pmatch (pfromData predeemerWitness'executionUnits)
    index <- plet (pfromData predeemerWitness'index)
    memory <- plet (pfromData pexecutionUnits'memory)
    steps <- plet (pfromData pexecutionUnits'steps)
    pif (index #< 0 #|| memory #< 0 #|| steps #< 0) perror $
      plistData
        #$ pcons
        # pforgetData
          (pdata (pmidgardRedeemerPurposeToTag # pfromData predeemerWitness'purpose))
        #$ pcons
        # pforgetData (pdata index)
        #$ pcons
        # pforgetData predeemerWitness'redeemerCbor
        #$ pcons
        # ( plistData
              #$ pcons
              # pforgetData (pdata memory)
              #$ pcons
              # pforgetData (pdata steps)
              # pcon PNil
          )
        # pcon PNil

-- | Aiken @components.encode_midgard_redeemer_witness@.
pencodeMidgardRedeemerWitness ::
  forall (s :: S). Term s (PMidgardRedeemerWitness :--> PByteString)
pencodeMidgardRedeemerWitness = phoistAcyclic $
  plam $ \witness -> P.do
    PMidgardRedeemerWitness
      { predeemerWitness'purpose
      , predeemerWitness'index
      , predeemerWitness'redeemerCbor
      , predeemerWitness'executionUnits
      } <-
      pmatch witness
    PMidgardExecutionUnits {pexecutionUnits'memory, pexecutionUnits'steps} <-
      pmatch (pfromData predeemerWitness'executionUnits)
    index <- plet (pfromData predeemerWitness'index)
    memory <- plet (pfromData pexecutionUnits'memory)
    steps <- plet (pfromData pexecutionUnits'steps)
    pif (index #< 0 #|| memory #< 0 #|| steps #< 0) perror $
      pconstant "\x84"
        <> pcborInt (pmidgardRedeemerPurposeToTag # pfromData predeemerWitness'purpose)
        <> pcborInt index
        <> (pencodeDefiniteBytes # pfromData predeemerWitness'redeemerCbor)
        <> pconstant "\x82"
        <> pcborInt memory
        <> pcborInt steps

{- | Aiken @components.decode_midgard_redeemer_witness_at@.

Unlike the input and witness decoders this one takes an offset and leaves the
caller to check where it ended — redeemers are read out of a longer preimage
rather than from bytes of their own.
-}
pdecodeMidgardRedeemerWitnessAt ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PPair PInteger PMidgardRedeemerWitness)
pdecodeMidgardRedeemerWitnessAt = phoistAcyclic $
  plam $ \bytes offset -> P.do
    PPair o1 purposeTag <-
      pmatch (pdecodeMidgardRedeemerPurposeTagAt # bytes # (pexpectByte # bytes # offset # 132))
    PPair o2 index <- pmatch (pdecodeUintAt # bytes # o1)
    PPair o3 redeemerCbor <- pmatch (pdecodeDefiniteBytesAt # bytes # o2)
    PPair o4 memory <- pmatch (pdecodeUintAt # bytes # (pexpectByte # bytes # o3 # 130))
    PPair o5 steps <- pmatch (pdecodeUintAt # bytes # o4)
    pcon
      ( PPair
          o5
          ( pmkRedeemerWitness
              (pmidgardRedeemerPurposeFromTag # purposeTag)
              index
              redeemerCbor
              memory
              steps
          )
      )

-- | Aiken @components.decode_midgard_redeemer_witness_data@.
pdecodeMidgardRedeemerWitnessData ::
  forall (s :: S). Term s (PData :--> PMidgardRedeemerWitness)
pdecodeMidgardRedeemerWitnessData = phoistAcyclic $
  plam $ \dat ->
    pelimList
      ( \purposeData r0 ->
          pelimList
            ( \indexData r1 ->
                pelimList
                  ( \redeemerCborData r2 ->
                      pelimList
                        ( \executionUnitsData r3 ->
                            pif (pnot # (pnull # r3)) perror $
                              pelimList
                                ( \memoryData r4 ->
                                    pelimList
                                      ( \stepsData r5 ->
                                          pif (pnot # (pnull # r5)) perror $
                                            pmkRedeemerWitness
                                              (pmidgardRedeemerPurposeFromTag #$ pasInt # purposeData)
                                              (pasInt # indexData)
                                              (pasByteStr # redeemerCborData)
                                              (pasInt # memoryData)
                                              (pasInt # stepsData)
                                      )
                                      perror
                                      r4
                                )
                                perror
                                (pasList # executionUnitsData)
                        )
                        perror
                        r2
                  )
                  perror
                  r1
            )
            perror
            r0
      )
      perror
      (pasList # dat)

-- | The five-field constructor shared by the two redeemer decoders.
pmkRedeemerWitness ::
  forall (s :: S).
  Term s PMidgardRedeemerPurpose ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PMidgardRedeemerWitness
pmkRedeemerWitness purpose index redeemerCbor memory steps =
  pcon
    ( PMidgardRedeemerWitness
        { predeemerWitness'purpose = pdata purpose
        , predeemerWitness'index = pdata index
        , predeemerWitness'redeemerCbor = pdata redeemerCbor
        , predeemerWitness'executionUnits =
            pdata
              ( pcon
                  ( PMidgardExecutionUnits
                      { pexecutionUnits'memory = pdata memory
                      , pexecutionUnits'steps = pdata steps
                      }
                  )
              )
        }
    )
