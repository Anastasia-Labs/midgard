{- |
Module      : Midgard.MpfProofFold
Description : Plutarch port of @lib/midgard/mpf-proof-fold-v1.ak@.

An MPF membership proof, folded __one step per transaction__.

"Midgard.MpfProof" walks a whole proof in a single script. That is fine while the
proof fits in one redeemer and one budget; when it does not, the walk has to be
suspended and resumed, and this module is the format for doing so. Each step of
the walk becomes a *frame*; the frames are committed to as leaves of a
"Midgard.ValidationMerkle" frontier under a *descriptor*; and a *control* record
carries the partial roots from one transaction to the next.

=== The fold runs backwards

'pinitialFoldControlV1' starts at the /last/ frame — @frame_count - 1@ — with the
cursor already at the descriptor's terminal cursor, and each folded frame moves
'pfoldControl'nextFrameIndex' down by one and the expected cursor back to that
frame's own. 'pfoldIsCompleteV1' accepts only when the index has reached @-1@ and
the cursor @0@. That is the same direction 'Midgard.MpfProof.pincluding' walks in
when it returns from its recursion, turned inside out so the intermediate state
is nameable.

=== Two roots, folded together

Every frame advances an /including/ root and an /excluding/ root at once, from the
same bytes. The including fold is the ordinary one. The excluding fold differs
only at the terminal frame, where a fork or a leaf contributes its neighbour or
its own suffix instead of a recursive combine — which is exactly the
@do_excluding@ divergence "Midgard.MpfProof" documents, expressed here as a
@is_terminal_frame@ flag rather than as a separate walk.

=== What binds a frame to the proof

Nothing in a frame is trusted on its own. 'pfoldProofFrameV1' re-derives the
frame's leaf hash and requires it to sit at @frame.frame_index@ in the
descriptor's frontier, so a prover cannot substitute, reorder or skip a step. The
cursor chain does the rest: each frame must arrive at the @next_cursor@ the
previous one expected, so the frames compose into one continuous path or not at
all.

=== On the neighbour representation

The frame preimage is built field by field — @nibble@, @prefix@ and @root@ are
each re-encoded here — while the frame datum uses Midgard's Aiken-compatible
@PNeighbor@. Thus both the hash preimage and the redeemer carry the canonical
single-constructor record representation.
-}
module Midgard.MpfProofFold (
  -- * Bounds
  pproofFoldVersion,
  pdigestByteCount,
  ppathNibbleCount,

  -- * The three records
  PProofFrameV1 (..),
  PProofDescriptorV1 (..),
  PProofFoldControlV1 (..),

  -- * Frames
  pencodeProofFrameV1,
  pproofFrameLeafHashV1,
  pframeIsWellFormed,

  -- * Descriptors
  pdescriptorIsWellFormedV1,
  pencodeProofDescriptorV1,

  -- * The fold
  pinitialFoldControlV1,
  pfoldProofFrameV1,
  pfoldIsCompleteV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Internal.Builtins (pconsBS')
import Plutarch.Core.Utils (pand'List)
import Plutarch.MerkleTree.Helpers (pcombine, pnibble, pnibbles, psuffix)
import Plutarch.MerkleTree.Merkling (pmerkle_16, pnull_hash, psparse_merkle_16)
import Plutarch.Prelude

import Midgard.Common.Utils (pconstrOf)
import Midgard.FraudProofs.NativeTx.Codec (
  pcborInt,
  pencodeDefiniteArrayHeader,
  pencodeDefiniteBytes,
 )
import Midgard.MpfProof.Types (PNeighbor (..), PProofStep (..))
import Midgard.ValidationMerkle (
  PFrontierPeak,
  pencodeFrontier,
  pfrontierIsWellFormed,
  pverifyMembership,
 )

--------------------------------------------------------------------------------
-- Bounds
--------------------------------------------------------------------------------

-- | Aiken @mpf_proof_fold_v1.proof_fold_version@ — @1@.
pproofFoldVersion :: forall (s :: S). Term s PInteger
pproofFoldVersion = 1

-- | Aiken @mpf_proof_fold_v1.digest_byte_count@ — @32@.
pdigestByteCount :: forall (s :: S). Term s PInteger
pdigestByteCount = 32

{- | Aiken @mpf_proof_fold_v1.path_nibble_count@ — @64@.

A Midgard path is a 32-byte digest, so 64 nibbles, so a proof can never carry
more than 64 frames. That is what bounds the descriptor and, through it, the
whole fold.
-}
ppathNibbleCount :: forall (s :: S). Term s PInteger
ppathNibbleCount = 64

-- | Aiken @mpf_proof_fold_v1.frame_domain@ — @"MidgardMpfProofFrameV1"@.
pframeDomain :: forall (s :: S). Term s PByteString
pframeDomain = pconstant "MidgardMpfProofFrameV1"

--------------------------------------------------------------------------------
-- The three records
--------------------------------------------------------------------------------

{- | Aiken @ProofFrameV1@ — one suspended step of the walk.

@cursor@ and @next_cursor@ are the walk's position before and after this step;
they are carried explicitly rather than recomputed, and then checked against the
step's own @skip@, so that a frame is self-describing enough to be committed to
on its own.
-}
data PProofFrameV1 (s :: S) = PProofFrameV1
  { pproofFrame'version :: Term s (PAsData PInteger)
  , pproofFrame'frameIndex :: Term s (PAsData PInteger)
  , pproofFrame'cursor :: Term s (PAsData PInteger)
  , pproofFrame'nextCursor :: Term s (PAsData PInteger)
  , pproofFrame'step :: Term s (PAsData PProofStep)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PProofFrameV1)

{- | Aiken @ProofDescriptorV1@ — the commitment the frames are checked against.

The frontier's peaks commit to every frame; @frame_count@ and @terminal_cursor@
say how many there are and where the walk ends, which is what lets the fold be
started and finished without ever seeing all the frames at once.
-}
data PProofDescriptorV1 (s :: S) = PProofDescriptorV1
  { pproofDescriptor'version :: Term s (PAsData PInteger)
  , pproofDescriptor'frameCount :: Term s (PAsData PInteger)
  , pproofDescriptor'terminalCursor :: Term s (PAsData PInteger)
  , pproofDescriptor'peaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PProofDescriptorV1)

{- | Aiken @ProofFoldControlV1@ — the state carried between transactions.

Both roots at once, plus where the next frame must sit and what cursor it must
end at. Nothing here is trusted: 'pfoldProofFrameV1' re-checks every field
against the descriptor before it will advance.
-}
data PProofFoldControlV1 (s :: S) = PProofFoldControlV1
  { pfoldControl'nextFrameIndex :: Term s (PAsData PInteger)
  , pfoldControl'expectedNextCursor :: Term s (PAsData PInteger)
  , pfoldControl'includingRoot :: Term s (PAsData PByteString)
  , pfoldControl'excludingRoot :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PProofFoldControlV1)

--------------------------------------------------------------------------------
-- Frames
--------------------------------------------------------------------------------

-- | Aiken @encode_frame_prefix@ — the five fields every frame kind shares.
pencodeFramePrefix ::
  forall (s :: S).
  Term s PProofFrameV1 ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString
pencodeFramePrefix frame kind itemCount =
  pmatch frame $
    \PProofFrameV1
      { pproofFrame'version
      , pproofFrame'frameIndex
      , pproofFrame'cursor
      , pproofFrame'nextCursor
      } ->
        (pencodeDefiniteArrayHeader # itemCount)
          <> pcborInt (pfromData pproofFrame'version)
          <> pcborInt (pfromData pproofFrame'frameIndex)
          <> pcborInt (pfromData pproofFrame'cursor)
          <> pcborInt (pfromData pproofFrame'nextCursor)
          <> pcborInt kind

{- | Aiken @encode_proof_frame_v1@.

The step kind is written into the preimage as its own field, and each kind
declares a different item count, so no two kinds can produce the same bytes even
where their payloads coincide.
-}
pencodeProofFrameV1 :: forall (s :: S). Term s (PProofFrameV1 :--> PByteString)
pencodeProofFrameV1 = phoistAcyclic $
  plam $ \frame ->
    pmatch frame $ \PProofFrameV1 {pproofFrame'step} ->
      pmatch (pfromData pproofFrame'step) $ \case
        PBranch {pproofStep'skip, pproofStep'neighbors} ->
          pencodeFramePrefix frame 0 7
            <> pcborInt (pfromData pproofStep'skip)
            <> (pencodeDefiniteBytes # pfromData pproofStep'neighbors)
        PFork {pproofStep'skip, pproofStep'neighbor} ->
          pmatch (pfromData pproofStep'neighbor) $
            \PNeighbor {pneighbor'nibble, pneighbor'prefix, pneighbor'root} ->
              pencodeFramePrefix frame 1 9
                <> pcborInt (pfromData pproofStep'skip)
                <> pcborInt (pfromData pneighbor'nibble)
                <> (pencodeDefiniteBytes # pfromData pneighbor'prefix)
                <> (pencodeDefiniteBytes # pfromData pneighbor'root)
        PLeaf {pproofStep'skip, pproofStep'key, pproofStep'value} ->
          pencodeFramePrefix frame 2 8
            <> pcborInt (pfromData pproofStep'skip)
            <> (pencodeDefiniteBytes # pfromData pproofStep'key)
            <> (pencodeDefiniteBytes # pfromData pproofStep'value)

-- | Aiken @proof_frame_leaf_hash_v1@ — a frame's leaf in the descriptor's frontier.
pproofFrameLeafHashV1 :: forall (s :: S). Term s (PProofFrameV1 :--> PByteString)
pproofFrameLeafHashV1 = phoistAcyclic $
  plam $ \frame -> pblake2b_256 #$ pframeDomain <> (pencodeProofFrameV1 # frame)

-- | Aiken @neighbor_is_well_formed@.
pneighborIsWellFormed :: forall (s :: S). Term s (PNeighbor :--> PBool)
pneighborIsWellFormed = phoistAcyclic $
  plam $ \neighbor ->
    pmatch neighbor $ \PNeighbor {pneighbor'nibble, pneighbor'prefix, pneighbor'root} ->
      pand'List
        [ 0 #<= pfromData pneighbor'nibble
        , pfromData pneighbor'nibble #< 16
        , plengthBS # pfromData pneighbor'prefix #<= pdigestByteCount
        , plengthBS # pfromData pneighbor'root #== pdigestByteCount
        ]

{- | The step's @skip@, read positionally.

Aiken writes this as a three-arm @when@ whose arms are @skip@, @skip@ and
@skip@ — the branch-selection hazard's exact shape on a data-encoded type (see
the README). @skip@ is field zero of all three constructors, so reading field
zero is both the safe spelling and the honest one.
-}
pstepSkip :: forall (s :: S). Term s (PAsData PProofStep) -> Term s PInteger
pstepSkip step = pasInt #$ phead # snd (pconstrOf step)

{- | Aiken @frame_is_well_formed@.

Everything a frame must satisfy before its bytes are allowed to move a root. The
cursor arithmetic (@next_cursor == cursor + 1 + skip@) is what makes the nibble
reads below total, by keeping @next_cursor - 1@ inside a 64-nibble path.

The two inequality clauses are the ones that carry weight: a fork's neighbour and
a leaf's key must branch away from the path at this step, or the step would not
be a step at all.
-}
pframeIsWellFormed ::
  forall (s :: S). Term s (PByteString :--> PProofFrameV1 :--> PBool)
pframeIsWellFormed = phoistAcyclic $
  plam $ \path frame ->
    pmatch frame $
      \PProofFrameV1
        { pproofFrame'version
        , pproofFrame'frameIndex
        , pproofFrame'cursor
        , pproofFrame'nextCursor
        , pproofFrame'step
        } ->
          plet (pfromData pproofFrame'cursor) $ \cursor ->
            plet (pfromData pproofFrame'nextCursor) $ \nextCursor ->
              plet (pstepSkip pproofFrame'step) $ \skip ->
                pand'List
                  [ pfromData pproofFrame'version #== pproofFoldVersion
                  , 0 #<= pfromData pproofFrame'frameIndex
                  , pfromData pproofFrame'frameIndex #< ppathNibbleCount
                  , 0 #<= cursor
                  , 0 #<= skip
                  , nextCursor #== cursor + 1 + skip
                  , nextCursor #<= ppathNibbleCount
                  ]
                  #&& pstepIsWellFormed path nextCursor (pfromData pproofFrame'step)

{- | The per-kind half of @frame_is_well_formed@.

Lifted out of the conjunction above because it is where the two clauses that
carry weight live: a fork's neighbour and a leaf's key must branch /away/ from
the path at this step, or the step would not be a step at all.
-}
pstepIsWellFormed ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Term s PProofStep ->
  Term s PBool
pstepIsWellFormed path nextCursor step =
  pmatch step $ \case
    PBranch {pproofStep'neighbors} ->
      plengthBS # pfromData pproofStep'neighbors #== 4 * pdigestByteCount
    PFork {pproofStep'neighbor} ->
      plet (pfromData pproofStep'neighbor) $ \neighbor ->
        pneighborIsWellFormed # neighbor #&& pforkNibbleDiffers path nextCursor neighbor
    PLeaf {pproofStep'key, pproofStep'value} ->
      plet (pfromData pproofStep'key) $ \key ->
        pand'List
          [ plengthBS # key #== pdigestByteCount
          , plengthBS # pfromData pproofStep'value #== pdigestByteCount
          ]
          #&& pnibblesDiffer path key nextCursor

-- | @nibble(path, next_cursor - 1) != neighbor.nibble@.
pforkNibbleDiffers ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Term s PNeighbor ->
  Term s PBool
pforkNibbleDiffers path nextCursor neighbor =
  pmatch neighbor $ \PNeighbor {pneighbor'nibble} ->
    pnot #$ (pnibble # path # (nextCursor - 1)) #== pfromData pneighbor'nibble

-- | @nibble(path, next_cursor - 1) != nibble(key, next_cursor - 1)@.
pnibblesDiffer ::
  forall (s :: S).
  Term s PByteString ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PBool
pnibblesDiffer path key nextCursor =
  pnot #$ (pnibble # path # (nextCursor - 1)) #== (pnibble # key # (nextCursor - 1))

--------------------------------------------------------------------------------
-- Descriptors
--------------------------------------------------------------------------------

{- | Aiken @descriptor_is_well_formed_v1@.

The last clause ties the two counters together: no frames means the walk ends
where it started, and any frames at all means it does not. Without it a
descriptor could claim frames and a terminal cursor of zero, and
'pfoldIsCompleteV1' would accept a fold that never consumed them.
-}
pdescriptorIsWellFormedV1 ::
  forall (s :: S). Term s (PProofDescriptorV1 :--> PBool)
pdescriptorIsWellFormedV1 = phoistAcyclic $
  plam $ \descriptor ->
    pmatch descriptor $
      \PProofDescriptorV1
        { pproofDescriptor'version
        , pproofDescriptor'frameCount
        , pproofDescriptor'terminalCursor
        , pproofDescriptor'peaks
        } ->
          plet (pfromData pproofDescriptor'frameCount) $ \frameCount ->
            plet (pfromData pproofDescriptor'terminalCursor) $ \terminalCursor ->
              pand'List
                [ pfromData pproofDescriptor'version #== pproofFoldVersion
                , 0 #<= frameCount
                , frameCount #<= ppathNibbleCount
                , 0 #<= terminalCursor
                , terminalCursor #<= ppathNibbleCount
                ]
                #&& (pfrontierIsWellFormed # frameCount # pfromData pproofDescriptor'peaks)
                #&& pif
                  (frameCount #== 0)
                  (terminalCursor #== 0)
                  (terminalCursor #> 0)

{- | Aiken @encode_proof_descriptor_v1@.

An @expect@ in the original, so a malformed descriptor aborts here rather than
encoding to something.
-}
pencodeProofDescriptorV1 ::
  forall (s :: S). Term s (PProofDescriptorV1 :--> PByteString)
pencodeProofDescriptorV1 = phoistAcyclic $
  plam $ \descriptor ->
    pif (pdescriptorIsWellFormedV1 # descriptor) `flip` perror $
      pmatch descriptor $
        \PProofDescriptorV1
          { pproofDescriptor'version
          , pproofDescriptor'frameCount
          , pproofDescriptor'terminalCursor
          , pproofDescriptor'peaks
          } ->
            (pencodeDefiniteArrayHeader # 4)
              <> pcborInt (pfromData pproofDescriptor'version)
              <> pcborInt (pfromData pproofDescriptor'frameCount)
              <> pcborInt (pfromData pproofDescriptor'terminalCursor)
              <> (pencodeFrontier # pfromData pproofDescriptor'peaks)

--------------------------------------------------------------------------------
-- Folding one frame
--------------------------------------------------------------------------------

-- | Aiken @do_branch@ — the sixteen-way combine at a full branch node.
pdoBranch ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString
pdoBranch path cursor nextCursor childRoot neighbors =
  pcombine
    # (pnibbles # path # cursor # (nextCursor - 1))
    #$ pmerkle_16
    # (pnibble # path # (nextCursor - 1))
    # childRoot
    # (psliceBS # 0 # 32 # neighbors)
    # (psliceBS # 32 # 32 # neighbors)
    # (psliceBS # 64 # 32 # neighbors)
    # (psliceBS # 96 # 32 # neighbors)

{- | Aiken @do_fork@ — the sparse combine at a two-way node.

Deliberately not @plutarch-onchain-lib@'s @pdo_fork@, which aborts when the
branch nibble equals the neighbour's. That check belongs to
'pframeIsWellFormed' here, and importing an abort into the fold would turn a
refusable frame into a failed script.
-}
pdoFork ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString
pdoFork path cursor nextCursor childRoot neighborNibble neighborPrefix neighborRoot =
  pcombine
    # (pnibbles # path # cursor # (nextCursor - 1))
    #$ psparse_merkle_16
    # (pnibble # path # (nextCursor - 1))
    # childRoot
    # neighborNibble
    #$ pcombine
    # neighborPrefix
    # neighborRoot

-- | Aiken @fold_including_frame@.
pfoldIncludingFrame ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PAsData PProofStep) ->
  Term s PByteString ->
  Term s PByteString
pfoldIncludingFrame path cursor nextCursor step childRoot =
  pmatch (pfromData step) $ \case
    PBranch {pproofStep'neighbors} ->
      pdoBranch path cursor nextCursor childRoot (pfromData pproofStep'neighbors)
    PFork {pproofStep'neighbor} ->
      pmatch (pfromData pproofStep'neighbor) $
        \PNeighbor {pneighbor'nibble, pneighbor'prefix, pneighbor'root} ->
          pdoFork
            path
            cursor
            nextCursor
            childRoot
            (pfromData pneighbor'nibble)
            (pfromData pneighbor'prefix)
            (pfromData pneighbor'root)
    PLeaf {pproofStep'key, pproofStep'value} ->
      plet (pfromData pproofStep'key) $ \key ->
        pdoFork
          path
          cursor
          nextCursor
          childRoot
          (pnibble # key # (nextCursor - 1))
          (psuffix # key # nextCursor)
          (pfromData pproofStep'value)

{- | Aiken @fold_excluding_frame@.

Identical to the including fold except at the terminal frame, where a fork and a
leaf stop recursing and contribute their own subtree instead. That is the
@do_excluding@ divergence "Midgard.MpfProof" documents, and it is why the two
roots have to be folded together rather than derived from one another.

Note the leaf's terminal case takes @suffix(key, cursor)@ and its non-terminal
case @nibble(key, cursor)@ with @suffix(key, next_cursor)@ — the cursor, not the
next cursor, in both. That asymmetry with the including fold is the original's.
-}
pfoldExcludingFrame ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PAsData PProofStep) ->
  Term s PByteString ->
  Term s PBool ->
  Term s PByteString
pfoldExcludingFrame path cursor nextCursor step childRoot isTerminalFrame =
  pmatch (pfromData step) $ \case
    PBranch {pproofStep'neighbors} ->
      pdoBranch path cursor nextCursor childRoot (pfromData pproofStep'neighbors)
    PFork {pproofStep'neighbor} ->
      pmatch (pfromData pproofStep'neighbor) $
        \PNeighbor {pneighbor'nibble, pneighbor'prefix, pneighbor'root} ->
          pif
            isTerminalFrame
            ( pcombine
                # (pconsBS' # pfromData pneighbor'nibble # pfromData pneighbor'prefix)
                # pfromData pneighbor'root
            )
            ( pdoFork
                path
                cursor
                nextCursor
                childRoot
                (pfromData pneighbor'nibble)
                (pfromData pneighbor'prefix)
                (pfromData pneighbor'root)
            )
    PLeaf {pproofStep'key, pproofStep'value} ->
      plet (pfromData pproofStep'key) $ \key ->
        plet (pfromData pproofStep'value) $ \value ->
          pif
            isTerminalFrame
            (pcombine # (psuffix # key # cursor) # value)
            ( pdoFork
                path
                cursor
                nextCursor
                childRoot
                (pnibble # key # cursor)
                (psuffix # key # nextCursor)
                value
            )

--------------------------------------------------------------------------------
-- The fold
--------------------------------------------------------------------------------

{- | Aiken @initial_fold_control_v1@.

The state before any frame is folded: the including root is the leaf the proof is
about, the excluding root is the null hash, and the walk is positioned at the
last frame.
-}
pinitialFoldControlV1 ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PProofDescriptorV1
        :--> PMaybe PProofFoldControlV1
    )
pinitialFoldControlV1 = phoistAcyclic $
  plam $ \key value descriptor ->
    pif (pdescriptorIsWellFormedV1 # descriptor) `flip` pcon PNothing $
      pmatch descriptor $
        \PProofDescriptorV1 {pproofDescriptor'frameCount, pproofDescriptor'terminalCursor} ->
          plet (pblake2b_256 # key) $ \path ->
            plet (pfromData pproofDescriptor'terminalCursor) $ \terminalCursor ->
              pcon . PJust . pcon $
                PProofFoldControlV1
                  { pfoldControl'nextFrameIndex =
                      pdata (pfromData pproofDescriptor'frameCount - 1)
                  , pfoldControl'expectedNextCursor = pdata terminalCursor
                  , pfoldControl'includingRoot =
                      pdata (pcombine # (psuffix # path # terminalCursor) #$ pblake2b_256 # value)
                  , pfoldControl'excludingRoot = pdata pnull_hash
                  }

{- | Aiken @fold_proof_frame_v1@.

One transaction's worth of work. Every guard is here rather than at the call
site, and all of them refuse rather than abort: a prover who supplies the wrong
frame gets no new control state, not a failed script.

The frontier membership check is the one that makes the rest of the guards worth
anything — without it a prover could hand over a well-formed frame that was never
part of the committed proof.
-}
pfoldProofFrameV1 ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PProofDescriptorV1
        :--> PProofFoldControlV1
        :--> PProofFrameV1
        :--> PBuiltinList (PAsData PByteString)
        :--> PMaybe PProofFoldControlV1
    )
pfoldProofFrameV1 = phoistAcyclic $
  plam $ \key descriptor control frame siblings ->
    pmatch descriptor $
      \PProofDescriptorV1 {pproofDescriptor'frameCount, pproofDescriptor'peaks} ->
        pmatch control $
          \PProofFoldControlV1
            { pfoldControl'nextFrameIndex
            , pfoldControl'expectedNextCursor
            , pfoldControl'includingRoot
            , pfoldControl'excludingRoot
            } ->
              pmatch frame $
                \PProofFrameV1
                  { pproofFrame'frameIndex
                  , pproofFrame'cursor
                  , pproofFrame'nextCursor
                  , pproofFrame'step
                  } ->
                    plet (pblake2b_256 # key) $ \path ->
                      plet (pfromData pproofDescriptor'frameCount) $ \frameCount ->
                        plet (pfromData pfoldControl'nextFrameIndex) $ \nextFrameIndex ->
                          plet (pfromData pproofFrame'frameIndex) $ \frameIndex ->
                            plet (pfromData pproofFrame'cursor) $ \cursor ->
                              plet (pfromData pproofFrame'nextCursor) $ \nextCursor ->
                                pif
                                  ( pand'List
                                      [ plengthBS # pfromData pfoldControl'includingRoot
                                          #== pdigestByteCount
                                      , plengthBS # pfromData pfoldControl'excludingRoot
                                          #== pdigestByteCount
                                      , 0 #<= nextFrameIndex
                                      , nextFrameIndex #< frameCount
                                      , frameIndex #== nextFrameIndex
                                      , nextCursor #== pfromData pfoldControl'expectedNextCursor
                                      ]
                                      #&& (pdescriptorIsWellFormedV1 # descriptor)
                                      #&& (pframeIsWellFormed # path # frame)
                                      #&& ( pverifyMembership
                                              # frameCount
                                              # pfromData pproofDescriptor'peaks
                                              # frameIndex
                                              # (pproofFrameLeafHashV1 # frame)
                                              # siblings
                                          )
                                  )
                                  ( pcon . PJust . pcon $
                                      PProofFoldControlV1
                                        { pfoldControl'nextFrameIndex = pdata (nextFrameIndex - 1)
                                        , pfoldControl'expectedNextCursor = pdata cursor
                                        , pfoldControl'includingRoot =
                                            pdata $
                                              pfoldIncludingFrame
                                                path
                                                cursor
                                                nextCursor
                                                pproofFrame'step
                                                (pfromData pfoldControl'includingRoot)
                                        , pfoldControl'excludingRoot =
                                            pdata $
                                              pfoldExcludingFrame
                                                path
                                                cursor
                                                nextCursor
                                                pproofFrame'step
                                                (pfromData pfoldControl'excludingRoot)
                                                (frameIndex #== frameCount - 1)
                                        }
                                  )
                                  (pcon PNothing)

{- | Aiken @fold_is_complete_v1@.

The fold has consumed every frame and walked the cursor back to the root. Both
roots are re-measured here rather than trusted, because a caller may have been
handed a control record it did not build.
-}
pfoldIsCompleteV1 :: forall (s :: S). Term s (PProofFoldControlV1 :--> PBool)
pfoldIsCompleteV1 = phoistAcyclic $
  plam $ \control ->
    pmatch control $
      \PProofFoldControlV1
        { pfoldControl'nextFrameIndex
        , pfoldControl'expectedNextCursor
        , pfoldControl'includingRoot
        , pfoldControl'excludingRoot
        } ->
          pand'List
            [ pfromData pfoldControl'nextFrameIndex #== -1
            , pfromData pfoldControl'expectedNextCursor #== 0
            , plengthBS # pfromData pfoldControl'includingRoot #== pdigestByteCount
            , plengthBS # pfromData pfoldControl'excludingRoot #== pdigestByteCount
            ]
