{- |
Module      : Midgard.BoundedItem
Description : Plutarch port of @lib/midgard/bounded-item-v1.ak@.

One *item* of a Midgard transaction field, committed to in fixed-size chunks.

The problem this solves: a field's item can be far larger than anything a single
L1 datum or redeemer can carry, but a fault proof still has to be able to
authenticate a slice of it. So an item is cut into 4,095-byte chunks, each chunk
is hashed with its position, and the chunk hashes are folded into a
"Midgard.ValidationMerkle" frontier. The item's commitment is then one 32-byte
hash covering the length and that frontier — and a proof for a single chunk is
the chunk, its Merkle path, and the frontier.

Positions are inside every hash, not alongside them. A chunk hash covers the
field index, the item index and the chunk index, and the commitment covers the
field index, the item index and the total length. A chunk therefore cannot be
replayed at a different position, in a different item, or in a different field.
-}
module Midgard.BoundedItem (
  PChunkProofV1 (..),
  pversion,
  pchunkBytes,
  pchunkCount,
  pexpectedChunkLength,
  phashChunk,
  pcommitment,
  pfromBytes,
  pverifyChunk,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Codec (
  pcborInt,
  pencodeDefiniteArrayHeader,
  pencodeDefiniteBytes,
 )
import Midgard.NativeTxFieldAccess (pfieldCount)
import Midgard.ValidationMerkle (
  PFrontierPeak,
  pappendLeaf,
  pfrontierCommitment,
  pfrontierIsWellFormed,
  pverifyMembership,
 )

-- | Aiken @bounded_item_v1.version@ — @1@.
pversion :: forall (s :: S). Term s PInteger
pversion = 1

{- | Aiken @bounded_item_v1.chunk_bytes@ — @4095@.

The chunk size, chosen to sit under the limits on what a single L1 datum or
redeemer can carry.
-}
pchunkBytes :: forall (s :: S). Term s PInteger
pchunkBytes = 4095

-- | Aiken @bounded_item_v1.chunk_domain@ — @"MidgardBoundedItemChunkV1"@.
pchunkDomain :: forall (s :: S). Term s PByteString
pchunkDomain = pconstant "MidgardBoundedItemChunkV1"

-- | Aiken @bounded_item_v1.commitment_domain@ — @"MidgardBoundedItemCommitmentV1"@.
pcommitmentDomain :: forall (s :: S). Term s PByteString
pcommitmentDomain = pconstant "MidgardBoundedItemCommitmentV1"

{- | Aiken @bounded_item_v1.ChunkProofV1@.

Everything needed to authenticate one chunk against an item's commitment: where
the chunk sits, how long the whole item is, the chunk itself, and the frontier
and sibling path that tie it to the commitment.
-}
data PChunkProofV1 (s :: S) = PChunkProofV1
  { pchunkProof'version :: Term s (PAsData PInteger)
  , pchunkProof'fieldIndex :: Term s (PAsData PInteger)
  , pchunkProof'itemIndex :: Term s (PAsData PInteger)
  , pchunkProof'totalLength :: Term s (PAsData PInteger)
  , pchunkProof'chunkIndex :: Term s (PAsData PInteger)
  , pchunkProof'chunk :: Term s (PAsData PByteString)
  , pchunkProof'frontier :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pchunkProof'siblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PChunkProofV1)

-- | Aiken @bounded_item_v1.field_index_is_valid@.
pfieldIndexIsValid :: forall (s :: S). Term s (PInteger :--> PBool)
pfieldIndexIsValid = phoistAcyclic $
  plam $ \fieldIndex ->
    pand'List [0 #<= fieldIndex, fieldIndex #< pfieldCount]

{- | Aiken @bounded_item_v1.chunk_count@.

Ceiling division, with one special case: a zero-length item is **one** empty
chunk, not none. That matters — an item with no chunks at all would have an
empty frontier, and every zero-length item in every field would then share a
commitment with every other.
-}
pchunkCount :: forall (s :: S). Term s (PInteger :--> PInteger)
pchunkCount = phoistAcyclic $
  plam $ \totalLength ->
    pif
      (totalLength #< 0)
      perror
      ( pif
          (totalLength #== 0)
          1
          (pdiv # (totalLength + pchunkBytes - 1) # pchunkBytes)
      )

{- | Aiken @bounded_item_v1.expected_chunk_length@.

Every chunk is full except the last. Pinning this exactly is what stops a prover
padding or truncating a chunk to change its hash while keeping its position.
-}
pexpectedChunkLength ::
  forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pexpectedChunkLength = phoistAcyclic $
  plam $ \totalLength chunkIndex ->
    plet (pchunkCount # totalLength) $ \count ->
      pif
        (pand'List [0 #<= chunkIndex, chunkIndex #< count])
        ( pif
            (chunkIndex + 1 #< count)
            pchunkBytes
            (totalLength - chunkIndex * pchunkBytes)
        )
        perror

{- | Aiken @bounded_item_v1.hash_chunk@.

The leaf hash for one chunk. The position is inside the preimage, so the same
bytes at a different index hash differently — without that, chunks could be
permuted within an item and the frontier would not notice.
-}
phashChunk ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger :--> PByteString :--> PByteString)
phashChunk = phoistAcyclic $
  plam $ \fieldIndex itemIndex chunkIndex chunk ->
    pif
      ( pand'List
          [ pfieldIndexIsValid # fieldIndex
          , 0 #<= itemIndex
          , 0 #<= chunkIndex
          , plengthBS # chunk #<= pchunkBytes
          ]
      )
      ( pblake2b_256
          #$ pchunkDomain
          <> (pencodeDefiniteArrayHeader # 5)
          <> pcborInt pversion
          <> pcborInt fieldIndex
          <> pcborInt itemIndex
          <> pcborInt chunkIndex
          <> (pencodeDefiniteBytes # chunk)
      )
      perror

{- | Aiken @bounded_item_v1.commitment@.

The item's single 32-byte commitment: its position, its length, and the frontier
over its chunk hashes.

The length is committed separately from the frontier even though the frontier's
leaf count already implies a chunk count — because the chunk count only bounds
the length to within a chunk, and the last chunk's expected length is what pins
it exactly.
-}
pcommitment ::
  forall (s :: S).
  Term
    s
    ( PInteger
        :--> PInteger
        :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PByteString
    )
pcommitment = phoistAcyclic $
  plam $ \fieldIndex itemIndex totalLength frontier ->
    plet (pchunkCount # totalLength) $ \count ->
      pif
        ( pand'List
            [ pfieldIndexIsValid # fieldIndex
            , 0 #<= itemIndex
            ]
            #&& pfrontierIsWellFormed # count # frontier
        )
        ( pblake2b_256
            #$ pcommitmentDomain
            <> (pencodeDefiniteArrayHeader # 5)
            <> pcborInt pversion
            <> pcborInt fieldIndex
            <> pcborInt itemIndex
            <> pcborInt totalLength
            <> (pencodeDefiniteBytes #$ pfrontierCommitment # count # frontier)
        )
        perror

{- | Aiken @bounded_item_v1.from_bytes@.

Commits to a whole item held in memory — the prover's side of the scheme, and
the definition the verifier is checked against.
-}
pfromBytes ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PByteString :--> PByteString)
pfromBytes = phoistAcyclic $
  plam $ \fieldIndex itemIndex bytes ->
    plet (plengthBS # bytes) $ \totalLength ->
      pcommitment
        # fieldIndex
        # itemIndex
        # totalLength
        # ( pbuildChunkFrontier
              # fieldIndex
              # itemIndex
              # bytes
              # 0
              # (pchunkCount # totalLength)
              # pcon PNil
          )

-- | Aiken @bounded_item_v1.build_frontier@.
pbuildChunkFrontier ::
  forall (s :: S).
  Term
    s
    ( PInteger
        :--> PInteger
        :--> PByteString
        :--> PInteger
        :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PBuiltinList (PAsData PFrontierPeak)
    )
pbuildChunkFrontier = phoistAcyclic $
  pfix $ \self -> plam $ \fieldIndex itemIndex bytes chunkIndex count frontier ->
    pif
      (count #<= chunkIndex)
      frontier
      ( plet (chunkIndex * pchunkBytes) $ \offset ->
          plet (pexpectedChunkLength # (plengthBS # bytes) # chunkIndex) $ \len ->
            plet
              ( pif
                  (len #== 0)
                  (pconstant "")
                  (psliceBS # offset # len # bytes)
              )
              $ \chunk ->
                self
                  # fieldIndex
                  # itemIndex
                  # bytes
                  # (chunkIndex + 1)
                  # count
                  # ( pappendLeaf
                        # chunkIndex
                        # frontier
                        # (phashChunk # fieldIndex # itemIndex # chunkIndex # chunk)
                    )
      )

{- | Aiken @bounded_item_v1.verify_chunk@.

The verifier: a chunk belongs to an item if it is the right length for its
position, its hash sits at that position in the frontier, and the frontier
commits to the expected value.

/The inner conjunction must short-circuit./ Three of its terms are partial and
each is guarded by an earlier one: 'pexpectedChunkLength' errors on an
out-of-range index, 'phashChunk' errors on an over-long chunk, and 'pcommitment'
errors on a malformed frontier. Under a strict conjunction each of those would
fail the script where Aiken returns @False@ — see the module note in
"Midgard.ValidationMerkle".

Mixed rejection modes, as in the original: the outer guard returns @False@.
-}
pverifyChunk ::
  forall (s :: S).
  Term s (PByteString :--> PChunkProofV1 :--> PBool)
pverifyChunk = phoistAcyclic $
  plam $ \expectedCommitment proof ->
    pmatch proof $
      \PChunkProofV1
        { pchunkProof'version
        , pchunkProof'fieldIndex
        , pchunkProof'itemIndex
        , pchunkProof'totalLength
        , pchunkProof'chunkIndex
        , pchunkProof'chunk
        , pchunkProof'frontier
        , pchunkProof'siblings
        } ->
          plet (pfromData pchunkProof'fieldIndex) $ \fieldIndex ->
            plet (pfromData pchunkProof'itemIndex) $ \itemIndex ->
              plet (pfromData pchunkProof'totalLength) $ \totalLength ->
                plet (pfromData pchunkProof'chunkIndex) $ \chunkIndex ->
                  plet (pfromData pchunkProof'chunk) $ \chunk ->
                    plet (pfromData pchunkProof'frontier) $ \frontier ->
                      pif
                        ( pand'List
                            [ pfromData pchunkProof'version #== pversion
                            , pfieldIndexIsValid # fieldIndex
                            , 0 #<= itemIndex
                            , 0 #<= totalLength
                            ]
                        )
                        ( plet (pchunkCount # totalLength) $ \count ->
                            (plengthBS # expectedCommitment #== 32)
                              #&& (0 #<= chunkIndex)
                              #&& (chunkIndex #< count)
                              #&& ( plengthBS # chunk
                                      #== pexpectedChunkLength # totalLength # chunkIndex
                                  )
                              #&& ( pverifyMembership
                                      # count
                                      # frontier
                                      # chunkIndex
                                      # (phashChunk # fieldIndex # itemIndex # chunkIndex # chunk)
                                      # pfromData pchunkProof'siblings
                                  )
                              #&& ( (pcommitment # fieldIndex # itemIndex # totalLength # frontier)
                                      #== expectedCommitment
                                  )
                        )
                        (pconstant False)
