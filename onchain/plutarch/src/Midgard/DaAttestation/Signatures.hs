{- |
Module      : Midgard.DaAttestation.Signatures
Description : Plutarch port of the signature and bitmap layer of
              @validators/da-attestation.ak@.

A data-availability attestation is a claim, signed by @da_threshold@ members of
the governed committee, that a block's data was published. This module is how
those signatures are counted; the validator's branches are a separate slice.

Two things carry the weight here.

/Signers are counted in a bitmap, and a bit may only be set once./
'psetAttestedSigner' fails on a bit that is already set. Without that, the same
committee member could sign twice and satisfy a threshold of two alone — the
whole point of a threshold is that it counts distinct members.

/Signatures must arrive in strictly ascending signer order./
'pverifyIndexedSignatures' carries a @min_signer_index@ that is the previous
signer's index plus one, so an index may not repeat or go backwards. That is a
second, independent guard on the same property: even if the bitmap check were
weakened, a repeated index would still be refused. The ordering also makes
verification a single pass with no bookkeeping of who has been seen.

The signature witness format is one index byte followed by a 64-byte Ed25519
signature, packed end to end — 65 bytes per signer, hence
@signature_witness_byte_count@. The index selects a verification key positionally
out of the packed committee, which is why the committee's fixed 32-byte stride
matters.
-}
module Midgard.DaAttestation.Signatures (
  pattestationAssetName,
  pattestationMessage,
  psignerBitMask,
  psignerBitIsClear,
  psetAttestedSigner,
  pverifyIndexedSignatures,
) where

import Plutarch.Builtin.Crypto (pverifyEd25519Signature)
import Plutarch.Core.Internal.Builtins (pconsBS', pindexBS')
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.V3 (PTokenName (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.DaAttestation (
  pattestationAssetNamePrefix,
  pattestedSignerBitmapByteCount,
  psignatureByteCount,
  psignatureWitnessByteCount,
  pverificationKeyByteCount,
 )

{- | Aiken @attestation_asset_name@.

@DAAT@ followed by the 28-byte header hash. The length check is what bounds the
name: a longer hash would push the asset name past Cardano's 32-byte limit, and
a shorter one would let two different headers collide under a padded name.
-}
pattestationAssetName :: forall (s :: S). Term s (PByteString :--> PAsData PTokenName)
pattestationAssetName = phoistAcyclic $
  plam $ \headerHash ->
    pif
      (plengthBS # headerHash #== 28)
      (pdata (pcon (PTokenName (pattestationAssetNamePrefix <> headerHash))))
      perror

{- | Aiken @attestation_message@.

@"MidgardDAAttestationV1" ++ header_hash@ — what the committee actually signs.

The domain-separating prefix is load-bearing: without it a signature over a bare
header hash could be replayed anywhere else in the protocol that asks a
committee member to sign a hash.
-}
pattestationMessage :: forall (s :: S). Term s (PByteString :--> PByteString)
pattestationMessage = phoistAcyclic $
  plam $ \headerHash -> pconstant "MidgardDAAttestationV1" <> headerHash

{- | Aiken @signer_bit_mask@.

The bit for a signer index, most-significant-first within its byte, as a value
to add rather than a shift.
-}
psignerBitMask :: forall (s :: S). Term s (PInteger :--> PInteger)
psignerBitMask = phoistAcyclic $
  plam $ \signerIndex ->
    plet (pdiv # signerIndex # 8) $ \byteIndex ->
      pindexBS'
        # pconstant "\128\64\32\16\8\4\2\1"
        # (signerIndex - byteIndex * 8)

{- | Aiken @signer_bit_is_clear@.

Whether a bit is unset, by integer division rather than bitwise masking:
@(byte / mask) - (byte / (mask * 2)) * 2 == 0@ isolates the bit's parity at that
position.
-}
psignerBitIsClear :: forall (s :: S). Term s (PInteger :--> PInteger :--> PBool)
psignerBitIsClear = phoistAcyclic $
  plam $ \byte mask ->
    plet (pdiv # byte # mask) $ \highBitWindow ->
      plet (pdiv # byte # (mask * 2)) $ \nextWindow ->
        highBitWindow - nextWindow * 2 #== 0

{- | Aiken @set_attested_signer@.

Sets one signer's bit, /failing if it was already set/.

That failure is the one that makes a threshold mean what it says. The bitmap is
rebuilt by splicing rather than mutated, so the width check at the top also
guarantees the result keeps the fixed 32-byte width every consumer assumes.
-}
psetAttestedSigner :: forall (s :: S). Term s (PByteString :--> PInteger :--> PByteString)
psetAttestedSigner = phoistAcyclic $
  plam $ \attested signerIndex -> P.do
    byteIndex <- plet $ pdiv # signerIndex # 8
    mask <- plet $ psignerBitMask # signerIndex
    oldByte <- plet $ pindexBS' # attested # byteIndex
    pif
      ( pand'List
          [ plengthBS # attested #== pattestedSignerBitmapByteCount
          , psignerBitIsClear # oldByte # mask
          ]
      )
      ( (psliceBS # 0 # byteIndex # attested)
          <> (pconsBS' # (oldByte + mask) # pconstant "")
          <> ( psliceBS
                # (byteIndex + 1)
                # (pattestedSignerBitmapByteCount - byteIndex - 1)
                # attested
             )
      )
      perror

{- | Aiken @verify_indexed_signatures@.

Walks the packed witness sequence, verifying each Ed25519 signature against the
committee key its index selects, and returns the bitmap with every signer's bit
set.

The @min_signer_index@ carried through the recursion is the previous index plus
one, so indices must strictly ascend. Combined with 'psetAttestedSigner'
refusing an already-set bit, one committee member cannot be counted twice by any
route — not by repeating an index, not by reordering, and not by supplying two
different signatures under the same index.

Each index is also bounded above by the committee size, which is what stops a
witness selecting a key outside the packed committee.
-}
pverifyIndexedSignatures ::
  forall (s :: S).
  Term
    s
    ( PByteString -- signatures
        :--> PByteString -- message
        :--> PByteString -- packed committee
        :--> PByteString -- attested bitmap so far
        :--> PInteger -- minimum admissible signer index
        :--> PInteger -- cursor into the witness sequence
        :--> PInteger -- witness sequence length
        :--> PByteString
    )
pverifyIndexedSignatures = phoistAcyclic $
  pfix $ \self ->
    plam $ \signatures message committeeSigners inputAttested minSignerIndex cursor signaturesLen ->
      pif
        (cursor #== signaturesLen)
        inputAttested
        $ P.do
          signerIndex <- plet $ pindexBS' # signatures # cursor
          committeeLen <- plet $ pdiv # (plengthBS # committeeSigners) # pverificationKeyByteCount
          verificationKey <-
            plet $
              psliceBS
                # (signerIndex * pverificationKeyByteCount)
                # pverificationKeyByteCount
                # committeeSigners
          signature <- plet $ psliceBS # (cursor + 1) # psignatureByteCount # signatures
          pif
            ( pand'List
                [ minSignerIndex #<= signerIndex
                , signerIndex #< committeeLen
                , pverifyEd25519Signature # verificationKey # message # signature
                ]
            )
            ( self
                # signatures
                # message
                # committeeSigners
                # (psetAttestedSigner # inputAttested # signerIndex)
                # (signerIndex + 1)
                # (cursor + psignatureWitnessByteCount)
                # signaturesLen
            )
            perror
