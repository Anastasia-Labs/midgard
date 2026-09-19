{- |
Module      : Midgard.CekBlobFrontier
Description : Plutarch port of @lib/midgard/cek-blob-frontier-v1.ak@.

A streaming commitment to a byte string too large to hold: chunks are appended
one at a time and the tree is kept as a list of __peaks__, one per complete
power-of-two subtree, so the state a proof carries is logarithmic in the number
of chunks rather than linear.

=== Peaks are ordered right to left, and only the first may be partial

The list runs from the /rightmost/, smallest subtree to the leftmost, largest
one, and heights strictly increase along it. That ordering is what makes
'pappendChunkRootV1' a fold: a new height-0 peak merges with the head while the
heights match, exactly as binary addition carries.

Only the first peak may hold a partial final chunk, which is why
'ppeaksAreWellFormedV1' takes a @first@ flag and gives that one peak a lower
minimum than the rest. Every other peak is a complete subtree and its byte
length is pinned to the maximum.

=== Well-formedness is checked twice per append

'pappendChunkRootV1' validates the frontier it is given /and/ the frontier it
produces. That is not redundant: the incoming check rejects a caller's forged
state, and the outgoing one rejects an append that would have produced an
unreachable shape — appending to a frontier whose last chunk was partial, for
instance, which the @byte_length@ equality in the guard already refuses but
which the second check pins independently.

=== Recursion is tied with 'pfix'

Every walk here — the peak merge, the well-formedness check, the aggregation,
the encoder — is self-recursive, and a cycle among top-level Plutarch term
definitions is an infinite value rather than a recursive function.
-}
module Midgard.CekBlobFrontier (
  -- * Version
  pcekBlobFrontierVersion,

  -- * Types
  PCekBlobFrontierPeakV1 (..),
  PCekBlobFrontierV1 (..),

  -- * Building
  pemptyFrontierV1,
  pappendChunkRootV1,
  pappendChunkV1,
  pfinalizeV1,
  prootFromChunksV1,

  -- * Checking and encoding
  pfrontierIsWellFormedV1,
  pencodeFrontierV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude

import Midgard.CekProof (pmaxBlobChunkBytesV1, phashBlobBranchV1, phashBlobChunkV1)
import Midgard.FraudProofs.NativeTx.Codec (
  pcborInt,
  pencodeDefiniteArrayHeader,
  pencodeDefiniteBytes,
 )
import Midgard.ValidationMerkle (pmaximumLeafCount)

--------------------------------------------------------------------------------
-- Version
--------------------------------------------------------------------------------

-- | Aiken @version@ — the leading item of every encoded frontier.
pcekBlobFrontierVersion :: forall (s :: S). Term s PInteger
pcekBlobFrontierVersion = 1

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

{- | Aiken @CekBlobFrontierPeakV1@ — one complete subtree of the chunk tree.

@height@ is the log of the leaf count, so a peak stands for @2^height@ chunks.
It is capped at 31 because the leaf count is capped at @2^32 - 1@.
-}
data PCekBlobFrontierPeakV1 (s :: S) = PCekBlobFrontierPeakV1
  { ppeak'height :: Term s (PAsData PInteger)
  , ppeak'root :: Term s (PAsData PByteString)
  , ppeak'byteLength :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekBlobFrontierPeakV1)

-- | Aiken @CekBlobFrontierV1@ — the whole streaming state.
data PCekBlobFrontierV1 (s :: S) = PCekBlobFrontierV1
  { pfrontier'count :: Term s (PAsData PInteger)
  , pfrontier'byteLength :: Term s (PAsData PInteger)
  , pfrontier'peaks :: Term s (PAsData (PBuiltinList (PAsData PCekBlobFrontierPeakV1)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekBlobFrontierV1)

type PPeakList = PBuiltinList (PAsData PCekBlobFrontierPeakV1)

--------------------------------------------------------------------------------
-- Field readers
--------------------------------------------------------------------------------

ppeakHeight, ppeakByteLength :: forall (s :: S). Term s (PCekBlobFrontierPeakV1 :--> PInteger)
ppeakHeight = phoistAcyclic $
  plam $ \peak -> pmatch peak $ \(PCekBlobFrontierPeakV1 h _ _) -> pfromData h
ppeakByteLength = phoistAcyclic $
  plam $ \peak -> pmatch peak $ \(PCekBlobFrontierPeakV1 _ _ b) -> pfromData b

ppeakRoot :: forall (s :: S). Term s (PCekBlobFrontierPeakV1 :--> PByteString)
ppeakRoot = phoistAcyclic $
  plam $ \peak -> pmatch peak $ \(PCekBlobFrontierPeakV1 _ r _) -> pfromData r

pfrontierCount, pfrontierByteLength ::
  forall (s :: S). Term s (PCekBlobFrontierV1 :--> PInteger)
pfrontierCount = phoistAcyclic $
  plam $ \f -> pmatch f $ \(PCekBlobFrontierV1 c _ _) -> pfromData c
pfrontierByteLength = phoistAcyclic $
  plam $ \f -> pmatch f $ \(PCekBlobFrontierV1 _ b _) -> pfromData b

pfrontierPeaks :: forall (s :: S). Term s (PCekBlobFrontierV1 :--> PPeakList)
pfrontierPeaks = phoistAcyclic $
  plam $ \f -> pmatch f $ \(PCekBlobFrontierV1 _ _ p) -> pfromData p

--------------------------------------------------------------------------------
-- Well-formedness
--------------------------------------------------------------------------------

{- | Aiken @power_of_two@.

Written as the Aiken writes it — a recursion, not a shift — because the height
is already bounded at 31 by the caller and the two must agree exactly on what a
negative height does, which is abort.
-}
ppowerOfTwo :: forall (s :: S). Term s (PInteger :--> PInteger)
ppowerOfTwo = phoistAcyclic $
  pfix $ \self -> plam $ \height ->
    pif (height #< 0) perror $
      pif (height #== 0) 1 (2 * (self # (height - 1)))

{- | Aiken @peaks_are_well_formed@.

Walks the peaks right to left, subtracting each subtree's leaf count from what
is left. Four things are checked at once, and each rules out a different forgery:

* strictly increasing heights, so the same subtree cannot appear twice;
* a 32-byte root, so a peak cannot carry a shorter commitment;
* a byte length between its minimum and its maximum, where only the /first/
  peak — the rightmost, smallest subtree — is allowed a partial final chunk;
* @remaining % (2 * leaves) >= leaves@, which is the bit test saying this
  subtree's height really is set in the binary expansion of the leaf count.

The last is the one that ties the peak list to the count: without it a frontier
could claim any set of subtrees it liked.
-}
ppeaksAreWellFormed ::
  forall (s :: S). Term s (PInteger :--> PInteger :--> PPeakList :--> PBool :--> PBool)
ppeaksAreWellFormed = phoistAcyclic $
  pfix $ \self -> plam $ \remaining priorHeight peaks first ->
    pelimList
      ( \peakData rest ->
          plet (pfromData peakData) $ \peak ->
            plet (ppeakHeight # peak) $ \height ->
              plet (ppowerOfTwo # height) $ \leaves ->
                plet (leaves * pmaxBlobChunkBytesV1) $ \maximum' ->
                  plet
                    (pif first ((leaves - 1) * pmaxBlobChunkBytesV1) maximum')
                    $ \minimum' ->
                      plet (ppeakByteLength # peak) $ \byteLength ->
                        pand'List
                          [ 0 #<= height
                          , height #<= 31
                          , priorHeight #< height
                          , plengthBS # (ppeakRoot # peak) #== 32
                          , minimum' #<= byteLength
                          , byteLength #<= maximum'
                          , leaves #<= (prem # remaining # (2 * leaves))
                          ]
                          #&& (self # (remaining - leaves) # height # rest # pconstant False)
      )
      (remaining #== 0)
      peaks

-- | Aiken @peak_lengths_sum@.
ppeakLengthsSum :: forall (s :: S). Term s (PPeakList :--> PInteger)
ppeakLengthsSum = phoistAcyclic $
  pfix $ \self -> plam $ \peaks ->
    pelimList
      (\peak rest -> (ppeakByteLength # pfromData peak) + (self # rest))
      0
      peaks

{- | Aiken @frontier_is_well_formed@.

The frontier's own byte length must sit between @(count - 1)@ and @count@ full
chunks — one partial chunk at the end and no more — and must equal the sum of
its peaks' lengths. The peak walk then checks the shape.
-}
pfrontierIsWellFormedV1 :: forall (s :: S). Term s (PCekBlobFrontierV1 :--> PBool)
pfrontierIsWellFormedV1 = phoistAcyclic $
  plam $ \frontier ->
    plet (pfrontierCount # frontier) $ \count ->
      plet (pfrontierByteLength # frontier) $ \byteLength ->
        plet (pfrontierPeaks # frontier) $ \peaks ->
          plet
            (pif (count #== 0) 0 ((count - 1) * pmaxBlobChunkBytesV1))
            $ \minimum' ->
              pand'List
                [ 0 #<= count
                , count #<= pmaximumLeafCount
                , minimum' #<= byteLength
                , byteLength #<= count * pmaxBlobChunkBytesV1
                , (ppeakLengthsSum # peaks) #== byteLength
                ]
                #&& (ppeaksAreWellFormed # count # (-1) # peaks # pconstant True)

--------------------------------------------------------------------------------
-- Building
--------------------------------------------------------------------------------

-- | Aiken @empty_frontier_v1@.
pemptyFrontierV1 :: forall (s :: S). Term s PCekBlobFrontierV1
pemptyFrontierV1 =
  pcon $ PCekBlobFrontierV1 (pdata 0) (pdata 0) (pdata (pcon PNil))

{- | Aiken @merge_peak@ — binary addition, carrying.

While the head has the same height as the peak being appended, the two combine
into one of the next height up and the carry continues. When the heights differ,
the appended peak goes on the front, which restores the right-to-left ordering.
-}
pmergePeak ::
  forall (s :: S). Term s (PPeakList :--> PCekBlobFrontierPeakV1 :--> PPeakList)
pmergePeak = phoistAcyclic $
  pfix $ \self -> plam $ \peaks appended ->
    pelimList
      ( \leftData rest ->
          plet (pfromData leftData) $ \left ->
            plet (ppeakHeight # appended) $ \appendedHeight ->
              pif
                (pnot # ((ppeakHeight # left) #== appendedHeight))
                (pcons # pdata appended # (pcons # leftData # rest))
                $ plet ((ppeakByteLength # left) + (ppeakByteLength # appended))
                $ \byteLength ->
                  self
                    # rest
                    # pcon
                      ( PCekBlobFrontierPeakV1
                          (pdata (appendedHeight + 1))
                          ( pdata $
                              phashBlobBranchV1
                                # (ppeakRoot # left)
                                # (ppeakRoot # appended)
                                # byteLength
                          )
                          (pdata byteLength)
                      )
      )
      (pcons # pdata appended # pcon PNil)
      peaks

{- | Aiken @append_chunk_root_v1@.

The @byte_length@ equality in the guard is the interesting clause: a frontier
whose accumulated length is not exactly @count@ full chunks already holds a
partial final chunk, and nothing may be appended after one. That is what makes
the format streaming-once rather than resumable.
-}
pappendChunkRootV1 ::
  forall (s :: S).
  Term
    s
    ( PCekBlobFrontierV1
        :--> PByteString
        :--> PInteger
        :--> PMaybe PCekBlobFrontierV1
    )
pappendChunkRootV1 = phoistAcyclic $
  plam $ \frontier chunkRoot chunkLength ->
    plet (pfrontierCount # frontier) $ \count ->
      plet (pfrontierByteLength # frontier) $ \byteLength ->
        pif
          ( pnot
              #$ pand'List
                [ pfrontierIsWellFormedV1 # frontier
                , count #< pmaximumLeafCount
                , plengthBS # chunkRoot #== 32
                , 0 #<= chunkLength
                , chunkLength #<= pmaxBlobChunkBytesV1
                , pif
                    (count #== 0)
                    (byteLength #== 0)
                    (byteLength #== count * pmaxBlobChunkBytesV1)
                ]
          )
          (pcon PNothing)
          $ plet
            ( pcon $
                PCekBlobFrontierV1
                  (pdata (count + 1))
                  (pdata (byteLength + chunkLength))
                  ( pdata $
                      pmergePeak
                        # (pfrontierPeaks # frontier)
                        # pcon
                          ( PCekBlobFrontierPeakV1
                              (pdata 0)
                              (pdata chunkRoot)
                              (pdata chunkLength)
                          )
                  )
            )
          $ \next ->
            pif
              (pfrontierIsWellFormedV1 # next)
              (pcon (PJust next))
              (pcon PNothing)

-- | Aiken @append_chunk_v1@.
pappendChunkV1 ::
  forall (s :: S).
  Term s (PCekBlobFrontierV1 :--> PByteString :--> PMaybe PCekBlobFrontierV1)
pappendChunkV1 = phoistAcyclic $
  plam $ \frontier chunk ->
    pappendChunkRootV1
      # frontier
      # (phashBlobChunkV1 # chunk)
      # (plengthBS # chunk)

{- | Aiken @aggregate_peaks@.

Folds the peaks left-wards into one, which is the tree the frontier stands for.
Note the argument order at the branch: the accumulated /right/ subtree is the
right-hand child, so the tree closes in the same direction it was built.
-}
paggregatePeaks ::
  forall (s :: S).
  Term s (PCekBlobFrontierPeakV1 :--> PPeakList :--> PCekBlobFrontierPeakV1)
paggregatePeaks = phoistAcyclic $
  pfix $ \self -> plam $ \aggregate peaks ->
    pelimList
      ( \leftData rest ->
          plet (pfromData leftData) $ \left ->
            plet ((ppeakByteLength # left) + (ppeakByteLength # aggregate)) $ \byteLength ->
              self
                # pcon
                  ( PCekBlobFrontierPeakV1
                      (pdata ((ppeakHeight # left) + 1))
                      ( pdata $
                          phashBlobBranchV1
                            # (ppeakRoot # left)
                            # (ppeakRoot # aggregate)
                            # byteLength
                      )
                      (pdata byteLength)
                  )
                # rest
      )
      aggregate
      peaks

{- | Aiken @finalize_v1@.

An empty frontier has no root — not the hash of nothing, but /nothing/. A blob
of zero chunks is not a blob, and a proof cannot be offered against one.
-}
pfinalizeV1 :: forall (s :: S). Term s (PCekBlobFrontierV1 :--> PMaybe PByteString)
pfinalizeV1 = phoistAcyclic $
  plam $ \frontier ->
    pif (pnot # (pfrontierIsWellFormedV1 # frontier)) (pcon PNothing) $
      pelimList
        (\right left -> pcon (PJust (ppeakRoot #$ paggregatePeaks # pfromData right # left)))
        (pcon PNothing)
        (pfrontierPeaks # frontier)

--------------------------------------------------------------------------------
-- Encoding
--------------------------------------------------------------------------------

-- | Aiken @encode_peak@.
pencodePeak :: forall (s :: S). Term s (PCekBlobFrontierPeakV1 :--> PByteString)
pencodePeak = phoistAcyclic $
  plam $ \peak ->
    (pencodeDefiniteArrayHeader # 3)
      <> pcborInt (ppeakHeight # peak)
      <> (pencodeDefiniteBytes # (ppeakRoot # peak))
      <> pcborInt (ppeakByteLength # peak)

-- | Aiken @encode_peak_items@.
pencodePeakItems :: forall (s :: S). Term s (PPeakList :--> PByteString)
pencodePeakItems = phoistAcyclic $
  pfix $ \self -> plam $ \peaks ->
    pelimList
      (\peak rest -> (pencodePeak # pfromData peak) <> (self # rest))
      (pconstant "")
      peaks

-- | Aiken @encode_peaks@.
pencodePeaks :: forall (s :: S). Term s (PPeakList :--> PByteString)
pencodePeaks = phoistAcyclic $
  plam $ \peaks ->
    (pencodeDefiniteArrayHeader #$ plength # peaks) <> (pencodePeakItems # peaks)

{- | Aiken @encode_frontier_v1@.

Aborts rather than declining on a malformed frontier: an encoding is what a
proof carries, and there is no correct encoding of a state that cannot exist.
-}
pencodeFrontierV1 :: forall (s :: S). Term s (PCekBlobFrontierV1 :--> PByteString)
pencodeFrontierV1 = phoistAcyclic $
  plam $ \frontier ->
    pif (pnot # (pfrontierIsWellFormedV1 # frontier)) perror $
      (pencodeDefiniteArrayHeader # 4)
        <> pcborInt pcekBlobFrontierVersion
        <> pcborInt (pfrontierCount # frontier)
        <> pcborInt (pfrontierByteLength # frontier)
        <> (pencodePeaks #$ pfrontierPeaks # frontier)

--------------------------------------------------------------------------------
-- Whole-blob convenience
--------------------------------------------------------------------------------

-- | Aiken @append_all@.
pappendAll ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PByteString)
        :--> PCekBlobFrontierV1
        :--> PMaybe PCekBlobFrontierV1
    )
pappendAll = phoistAcyclic $
  pfix $ \self -> plam $ \remaining frontier ->
    pelimList
      ( \chunk rest ->
          pmatch (pappendChunkV1 # frontier # pfromData chunk) $ \case
            PNothing -> pcon PNothing
            PJust next -> self # rest # next
      )
      (pcon (PJust frontier))
      remaining

{- | Aiken @root_from_chunks_v1@.

The whole scheme in one call, for a caller that does hold the chunks. The
streaming path and this one must agree, and they do by construction: this /is/
the streaming path, run to completion.
-}
prootFromChunksV1 ::
  forall (s :: S). Term s (PBuiltinList (PAsData PByteString) :--> PMaybe PByteString)
prootFromChunksV1 = phoistAcyclic $
  plam $ \chunks ->
    pmatch (pappendAll # chunks # pemptyFrontierV1) $ \case
      PNothing -> pcon PNothing
      PJust frontier -> pfinalizeV1 # frontier
