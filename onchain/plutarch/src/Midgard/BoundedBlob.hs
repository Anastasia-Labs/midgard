{- |
Module      : Midgard.BoundedBlob
Description : Plutarch port of @lib/midgard/bounded-blob-v1.ak@.

A whole transaction *field* committed to in fixed-size chunks — the sibling of
"Midgard.BoundedItem", which does the same for one item /inside/ a field.

The scheme is identical in outline: cut the bytes into 4,095-byte chunks, hash
each chunk with its position, fold the chunk hashes into a
"Midgard.ValidationMerkle" frontier, and commit to the length and that frontier
in one 32-byte hash. Positions live inside the hashes rather than beside them, so
a chunk cannot be replayed at a different index or under a different field.

=== The one place it is not the same as 'Midgard.BoundedItem'

@chunk_count(0)@ is __zero__ here and __one__ there. A bounded item treats a
zero-length item as one empty chunk, because otherwise every zero-length item in
every field would share the empty frontier and so share a commitment. A bounded
blob does not need that: its commitment covers the field index and the total
length directly, so the empty blob's commitment is already distinct per field
without a phantom chunk. The consequence is visible in 'pverifyChunk', whose
outer guard demands a /positive/ total length: an empty blob has a commitment and
no chunk proof can ever be offered against it.

Both domain separation strings differ from the item module's too, so the two
schemes cannot be confused even where their arithmetic coincides.
-}
module Midgard.BoundedBlob (
  PChunkProofV1 (..),
  pversion,
  pchunkBytes,
  pchunkCount,
  pexpectedChunkLength,
  phashChunk,
  pcommitment,
  pfromPreimage,
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
  pemptyFrontier,
  pfrontierCommitment,
  pfrontierIsWellFormed,
  pverifyMembership,
 )

-- | Aiken @bounded_blob_v1.version@ — @1@.
pversion :: forall (s :: S). Term s PInteger
pversion = 1

-- | Aiken @bounded_blob_v1.chunk_bytes@ — @4095@.
pchunkBytes :: forall (s :: S). Term s PInteger
pchunkBytes = 4095

-- | Aiken @bounded_blob_v1.chunk_domain@ — @"MidgardBoundedBlobChunkV1"@.
pchunkDomain :: forall (s :: S). Term s PByteString
pchunkDomain = pconstant "MidgardBoundedBlobChunkV1"

-- | Aiken @bounded_blob_v1.commitment_domain@ — @"MidgardBoundedBlobCommitmentV1"@.
pcommitmentDomain :: forall (s :: S). Term s PByteString
pcommitmentDomain = pconstant "MidgardBoundedBlobCommitmentV1"

{- | Aiken @bounded_blob_v1.ChunkProofV1@.

Everything needed to authenticate one chunk against a blob's commitment. Note
what is /not/ here next to 'Midgard.BoundedItem.PChunkProofV1': there is no item
index, because a blob is the whole field.
-}
data PChunkProofV1 (s :: S) = PChunkProofV1
  { pchunkProof'version :: Term s (PAsData PInteger)
  , pchunkProof'fieldIndex :: Term s (PAsData PInteger)
  , pchunkProof'totalLength :: Term s (PAsData PInteger)
  , pchunkProof'chunkIndex :: Term s (PAsData PInteger)
  , pchunkProof'chunk :: Term s (PAsData PByteString)
  , pchunkProof'frontier :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pchunkProof'siblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PChunkProofV1)

-- | Aiken @bounded_blob_v1.field_index_is_valid@.
pfieldIndexIsValid :: forall (s :: S). Term s (PInteger :--> PBool)
pfieldIndexIsValid = phoistAcyclic $
  plam $ \fieldIndex ->
    pand'List [0 #<= fieldIndex, fieldIndex #< pfieldCount]

{- | Aiken @bounded_blob_v1.chunk_count@ — ceiling division, and zero for zero.

See the module note: the empty blob has no chunks at all, which is where this
parts company with 'Midgard.BoundedItem.pchunkCount'.
-}
pchunkCount :: forall (s :: S). Term s (PInteger :--> PInteger)
pchunkCount = phoistAcyclic $
  plam $ \totalLength ->
    pif
      (totalLength #< 0)
      perror
      ( pif
          (totalLength #== 0)
          0
          (pdiv # (totalLength + pchunkBytes - 1) # pchunkBytes)
      )

{- | Aiken @bounded_blob_v1.expected_chunk_length@.

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

{- | Aiken @bounded_blob_v1.hash_chunk@.

The leaf hash for one chunk: the field it belongs to, where it sits, and its
bytes. The index bound checked here is only @0 <=@ — a chunk index past the end
is caught by 'pverifyChunk' against the count, not by the hash.
-}
phashChunk ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PByteString :--> PByteString)
phashChunk = phoistAcyclic $
  plam $ \fieldIndex chunkIndex chunk ->
    pif
      ( pand'List
          [ pfieldIndexIsValid # fieldIndex
          , 0 #<= chunkIndex
          , plengthBS # chunk #<= pchunkBytes
          ]
      )
      ( pblake2b_256
          #$ pchunkDomain
          <> (pencodeDefiniteArrayHeader # 4)
          <> pcborInt pversion
          <> pcborInt fieldIndex
          <> pcborInt chunkIndex
          <> (pencodeDefiniteBytes # chunk)
      )
      perror

{- | Aiken @bounded_blob_v1.commitment@.

The blob's single 32-byte commitment: which field it is, how long it is, and the
frontier over its chunk hashes.

The length is committed separately from the frontier even though the frontier's
leaf count already implies a chunk count — the chunk count only bounds the length
to within a chunk, and the last chunk's expected length is what pins it exactly.
-}
pcommitment ::
  forall (s :: S).
  Term
    s
    ( PInteger
        :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PByteString
    )
pcommitment = phoistAcyclic $
  plam $ \fieldIndex totalLength frontier ->
    plet (pchunkCount # totalLength) $ \count ->
      pif
        (pfieldIndexIsValid # fieldIndex #&& pfrontierIsWellFormed # count # frontier)
        ( pblake2b_256
            #$ pcommitmentDomain
            <> (pencodeDefiniteArrayHeader # 4)
            <> pcborInt pversion
            <> pcborInt fieldIndex
            <> pcborInt totalLength
            <> (pencodeDefiniteBytes #$ pfrontierCommitment # count # frontier)
        )
        perror

{- | Aiken @bounded_blob_v1.from_preimage@.

Commits to a whole field held in memory — the prover's side of the scheme, and
the definition the verifier is checked against. The empty blob takes the empty
frontier rather than the build loop, which would otherwise be asked for chunk
zero of a blob that has none.
-}
pfromPreimage ::
  forall (s :: S).
  Term s (PInteger :--> PByteString :--> PByteString)
pfromPreimage = phoistAcyclic $
  plam $ \fieldIndex preimage ->
    plet (plengthBS # preimage) $ \totalLength ->
      plet (pchunkCount # totalLength) $ \count ->
        pcommitment
          # fieldIndex
          # totalLength
          # pif
            (count #== 0)
            pemptyFrontier
            (pbuildPreimageFrontier # fieldIndex # preimage # 0 # count # pemptyFrontier)

-- | Aiken @bounded_blob_v1.build_preimage_frontier@.
pbuildPreimageFrontier ::
  forall (s :: S).
  Term
    s
    ( PInteger
        :--> PByteString
        :--> PInteger
        :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PBuiltinList (PAsData PFrontierPeak)
    )
pbuildPreimageFrontier = phoistAcyclic $
  pfix $ \self -> plam $ \fieldIndex preimage chunkIndex count frontier ->
    pif
      (count #<= chunkIndex)
      frontier
      ( plet (chunkIndex * pchunkBytes) $ \offset ->
          plet (pexpectedChunkLength # (plengthBS # preimage) # chunkIndex) $ \len ->
            self
              # fieldIndex
              # preimage
              # (chunkIndex + 1)
              # count
              # ( pappendLeaf
                    # chunkIndex
                    # frontier
                    # ( phashChunk
                          # fieldIndex
                          # chunkIndex
                          # (psliceBS # offset # len # preimage)
                      )
                )
      )

{- | Aiken @bounded_blob_v1.verify_chunk@.

A chunk belongs to a blob if it is the right length for its position, its hash
sits at that position in the frontier, and the frontier commits to the expected
value.

/The inner conjunction must short-circuit./ Three of its terms are partial and
each is guarded by an earlier one: 'pexpectedChunkLength' errors on an
out-of-range index, 'phashChunk' errors on an over-long chunk, and 'pcommitment'
errors on a malformed frontier. Under a strict conjunction each would fail the
script where Aiken returns @False@ — see the note in "Midgard.ValidationMerkle".

Mixed rejection modes, as in the original: the outer guard returns @False@. Note
that guard demands @total_length > 0@, so the empty blob is unprovable by
construction rather than by an arithmetic accident.
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
        , pchunkProof'totalLength
        , pchunkProof'chunkIndex
        , pchunkProof'chunk
        , pchunkProof'frontier
        , pchunkProof'siblings
        } ->
          plet (pfromData pchunkProof'fieldIndex) $ \fieldIndex ->
            plet (pfromData pchunkProof'totalLength) $ \totalLength ->
              plet (pfromData pchunkProof'chunkIndex) $ \chunkIndex ->
                plet (pfromData pchunkProof'chunk) $ \chunk ->
                  plet (pfromData pchunkProof'frontier) $ \frontier ->
                    pif
                      ( pand'List
                          [ pfromData pchunkProof'version #== pversion
                          , pfieldIndexIsValid # fieldIndex
                          , 0 #< totalLength
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
                                    # (phashChunk # fieldIndex # chunkIndex # chunk)
                                    # pfromData pchunkProof'siblings
                                )
                            #&& ( (pcommitment # fieldIndex # totalLength # frontier)
                                    #== expectedCommitment
                                )
                      )
                      (pconstant False)
