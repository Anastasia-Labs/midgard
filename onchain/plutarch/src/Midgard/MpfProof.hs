{- |
Module      : Midgard.MpfProof
Description : Plutarch port of @lib/midgard/mpf-proof-v1.ak@.

Canonical Midgard V1 Merkle-Patricia-Forestry proof helpers: total, fail-closed
wrappers around the MPF walk, for use on /untrusted/ dispute witnesses.

The library's own @insert@, @update@ and @delete@ are partial — an invalid proof
aborts script evaluation rather than returning a verdict. That is right for a
trusted caller and wrong for a fault proof, where the proof is exactly the thing
in dispute. Everything here returns @False@ or @Nothing@ instead, and each
entry point gates on a structural well-formedness check before walking.

=== The well-formedness gate is load-bearing, and it must be lazy

@membership_proof_is_well_formed@ and its non-membership twin bound the cursor,
the skip and the step count /before/ reading a nibble at @next_cursor - 1@.
Reading that nibble unguarded errors past the end of the path, and the
recursion below it has no base case for a negative step budget. Aiken's
@and { }@ short-circuits, so the port uses @#&&@ and not @pand'List@ — see the
strictness note in the README.

=== A divergence between the two MPF libraries

__This module does not use @plutarch-onchain-lib@'s @pexcluding@, and that is
deliberate.__ The Plutarch and Aiken MPF libraries compute @excluding@
differently, in two places, both reachable only when a proof step carries
@skip > 0@:

1. __Terminal @Fork@.__ Aiken reconstructs @combine(nibble : prefix, root)@ and
   drops the skipped path nibbles entirely. The Plutarch library prepends
   @nibbles(path, cursor, cursor + skip)@ first.
2. __Non-terminal @Leaf@.__ Aiken takes the neighbour's nibble at @cursor@; the
   Plutarch library takes it at @next_cursor - 1@, which is @cursor + skip@.

The Aiken library is also internally inconsistent about the second: its
@do_including@ uses @next_cursor - 1@ where its @do_excluding@ uses @cursor@,
and @mpf-proof-v1.ak@ copies both faithfully.

Since this port exists to /replace/ the Aiken tree, 'pdoExcluding' below
reproduces Aiken's arithmetic exactly. Using the library function would silently
change which non-membership proofs are accepted, which is a consensus change and
not a port. The two are pinned against each other in
@Testing.MpfProof@. Midgard also owns the proof data types so the nested
@Neighbor@ keeps Aiken's @Constr 0@ wire representation.
-}
module Midgard.MpfProof (
  -- * Bounds
  pdigestByteCount,
  pmaximumProofStepCount,
  pproofHasAtMostSteps,

  -- * Total verification
  phasV1,
  phasValueHash,
  pdoesNotHave,

  -- * Total root transitions
  pinsertRoot,
  pupdateRoot,
  pdeleteRoot,

  -- * The Aiken-faithful walk
  pdoExcluding,
  pdoIncludingByHash,
) where

import Data.Kind (Type)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Internal.Builtins (pconsBS')
import Plutarch.MerkleTree.Helpers (pcombine, pnibble, pnibbles, psuffix)
import Plutarch.MerkleTree.Merkling (pmerkle_16, pnull_hash, psparse_merkle_16)
import Plutarch.Prelude

import Midgard.MpfProof.Types (PNeighbor (..), PProof, PProofStep (..))

--------------------------------------------------------------------------------
-- Bounds
--------------------------------------------------------------------------------

-- | Aiken @mpf_proof_v1.digest_byte_count@.
pdigestByteCount :: forall (s :: S). Term s PInteger
pdigestByteCount = 32

-- | Aiken @mpf_proof_v1.path_nibble_count@.
ppathNibbleCount :: forall (s :: S). Term s PInteger
ppathNibbleCount = 64

{- | Aiken @mpf_proof_v1.maximum_proof_step_count@ — @64@.

A Midgard V1 path is 64 nibbles, so a well-formed proof can never carry more
than one step per nibble. This is the protocol-wide ceiling on any single
proof's step count, however it is carried.
-}
pmaximumProofStepCount :: forall (s :: S). Term s PInteger
pmaximumProofStepCount = 64

-- | Aiken @mpf_proof_v1.proof_has_at_most_steps@.
pproofHasAtMostSteps ::
  forall (s :: S). Term s (PProof :--> PInteger :--> PBool)
pproofHasAtMostSteps = phoistAcyclic $
  plam $ \proof maximum' ->
    0 #<= maximum' #&& plength # pto proof #<= maximum'

--------------------------------------------------------------------------------
-- Well-formedness
--------------------------------------------------------------------------------

-- | Aiken @mpf_proof_v1.neighbor_is_well_formed@.
pneighborIsWellFormed :: forall (s :: S). Term s (PNeighbor :--> PBool)
pneighborIsWellFormed = phoistAcyclic $
  plam $ \neighbor -> pwithNeighbor neighbor $ \nibbleValue prefix root ->
    0
      #<= nibbleValue
      #&& nibbleValue
      #< 16
      #&& plengthBS
      # prefix
      #<= pdigestByteCount
      #&& plengthBS
      # root
      #== pdigestByteCount

{- | Aiken @mpf_proof_v1.common_step_is_well_formed@.

Every step of both walks starts here, and it is what makes the nibble reads
below total: @cursor + 1 + skip <= 64@ bounds @next_cursor - 1@ inside a
64-nibble path.
-}
pcommonStepIsWellFormed ::
  forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger :--> PBool)
pcommonStepIsWellFormed = phoistAcyclic $
  plam $ \cursor skip remainingStepCount ->
    0
      #<= cursor
      #&& 0
      #<= skip
      #&& cursor
      + 1
      + skip
      #<= ppathNibbleCount
      #&& 0
      #<= remainingStepCount
      #&& remainingStepCount
      #<= pmaximumProofStepCount

{- | Aiken @mpf_proof_v1.membership_proof_is_well_formed@.

/The conjunctions are lazy on purpose./ The nibble comparisons past the first
conjunct error when the cursor is out of range, and the recursion has no base
case for a negative step budget, so a strict @and@ would fail the script where
Aiken returns @False@.
-}
pmembershipProofIsWellFormed ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PBuiltinList (PAsData PProofStep)
        :--> PInteger
        :--> PBool
    )
pmembershipProofIsWellFormed = phoistAcyclic $
  pfix $ \self -> plam $ \path cursor proof remainingStepCount ->
    pelimList
      ( \step steps ->
          pmatch (pfromData step) $ \case
            PBranch {pproofStep'skip, pproofStep'neighbors} ->
              plet (pfromData pproofStep'skip) $ \skip ->
                pcommonStepIsWellFormed
                  # cursor
                  # skip
                  # remainingStepCount
                  #&& plengthBS
                  # pfromData pproofStep'neighbors
                  #== 4
                  * pdigestByteCount
                  #&& self
                  # path
                  # (cursor + 1 + skip)
                  # steps
                  # (remainingStepCount - 1)
            PFork {pproofStep'skip, pproofStep'neighbor} ->
              plet (pfromData pproofStep'skip) $ \skip ->
                plet (cursor + 1 + skip) $ \nextCursor ->
                  plet (pfromData pproofStep'neighbor) $ \neighbor ->
                    pcommonStepIsWellFormed
                      # cursor
                      # skip
                      # remainingStepCount
                      #&& pneighborIsWellFormed
                      # neighbor
                      #&& pforkNibbleDiffers path nextCursor neighbor
                      #&& self
                      # path
                      # nextCursor
                      # steps
                      # (remainingStepCount - 1)
            PLeaf {pproofStep'skip, pproofStep'key, pproofStep'value} ->
              plet (pfromData pproofStep'skip) $ \skip ->
                plet (cursor + 1 + skip) $ \nextCursor ->
                  plet (pfromData pproofStep'key) $ \key ->
                    pcommonStepIsWellFormed
                      # cursor
                      # skip
                      # remainingStepCount
                      #&& pleafWidthsAreRight key (pfromData pproofStep'value)
                      #&& (pnibble # path # (nextCursor - 1))
                      #/== (pnibble # key # (nextCursor - 1))
                      #&& self
                      # path
                      # nextCursor
                      # steps
                      # (remainingStepCount - 1)
      )
      (0 #<= cursor #&& cursor #<= ppathNibbleCount)
      proof

{- | Aiken @mpf_proof_v1.non_membership_proof_is_well_formed@.

The same walk with the two terminal cases pulled out: a proof may /end/ on a
@Fork@ or a @Leaf@, and when it does there is no recursion to guard.

Note the one asymmetry Aiken has and the port keeps: the terminal @Leaf@ case
compares @nibble(path, next_cursor - 1)@ against @nibble(key, next_cursor - 1)@,
while the non-terminal one compares it against @nibble(key, cursor)@.
-}
pnonMembershipProofIsWellFormed ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PBuiltinList (PAsData PProofStep)
        :--> PInteger
        :--> PBool
    )
pnonMembershipProofIsWellFormed = phoistAcyclic $
  pfix $ \self -> plam $ \path cursor proof remainingStepCount ->
    pelimList
      ( \step steps ->
          pmatch (pfromData step) $ \case
            PBranch {pproofStep'skip, pproofStep'neighbors} ->
              plet (pfromData pproofStep'skip) $ \skip ->
                pcommonStepIsWellFormed
                  # cursor
                  # skip
                  # remainingStepCount
                  #&& plengthBS
                  # pfromData pproofStep'neighbors
                  #== 4
                  * pdigestByteCount
                  #&& self
                  # path
                  # (cursor + 1 + skip)
                  # steps
                  # (remainingStepCount - 1)
            PFork {pproofStep'skip, pproofStep'neighbor} ->
              plet (pfromData pproofStep'skip) $ \skip ->
                plet (cursor + 1 + skip) $ \nextCursor ->
                  plet (pfromData pproofStep'neighbor) $ \neighbor ->
                    plet
                      ( pcommonStepIsWellFormed
                          # cursor
                          # skip
                          # remainingStepCount
                          #&& pneighborIsWellFormed
                          # neighbor
                          #&& pforkNibbleDiffers path nextCursor neighbor
                      )
                      $ \here ->
                        pelimList
                          ( \_ _ ->
                              here
                                #&& self
                                # path
                                # nextCursor
                                # steps
                                # (remainingStepCount - 1)
                          )
                          here
                          steps
            PLeaf {pproofStep'skip, pproofStep'key, pproofStep'value} ->
              plet (pfromData pproofStep'skip) $ \skip ->
                plet (cursor + 1 + skip) $ \nextCursor ->
                  plet (pfromData pproofStep'key) $ \key ->
                    plet
                      ( pcommonStepIsWellFormed
                          # cursor
                          # skip
                          # remainingStepCount
                          #&& pleafWidthsAreRight key (pfromData pproofStep'value)
                      )
                      $ \here ->
                        pelimList
                          ( \_ _ ->
                              here
                                #&& (pnibble # path # (nextCursor - 1))
                                #/== (pnibble # key # cursor)
                                #&& self
                                # path
                                # nextCursor
                                # steps
                                # (remainingStepCount - 1)
                          )
                          ( here
                              #&& (pnibble # path # (nextCursor - 1))
                              #/== (pnibble # key # (nextCursor - 1))
                          )
                          steps
      )
      (0 #<= cursor #&& cursor #<= ppathNibbleCount)
      proof

-- | @nibble(path, next_cursor - 1) != neighbor.nibble@.
pforkNibbleDiffers ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Term s PNeighbor ->
  Term s PBool
pforkNibbleDiffers path nextCursor neighbor =
  pwithNeighbor neighbor $ \nibbleValue _ _ ->
    (pnibble # path # (nextCursor - 1)) #/== nibbleValue

-- | A leaf step's key and value are both a digest wide.
pleafWidthsAreRight ::
  forall (s :: S). Term s PByteString -> Term s PByteString -> Term s PBool
pleafWidthsAreRight key value =
  plengthBS # key #== pdigestByteCount #&& plengthBS # value #== pdigestByteCount

{- | Destructures a neighbour into its three unwrapped components.

Written once because every use site wants all three at once and none of them
wants the @Data@ wrappers.
-}
pwithNeighbor ::
  forall (s :: S) (r :: S -> Type).
  Term s PNeighbor ->
  (Term s PInteger -> Term s PByteString -> Term s PByteString -> Term s r) ->
  Term s r
pwithNeighbor neighbor k =
  pmatch neighbor $ \PNeighbor {pneighbor'nibble, pneighbor'prefix, pneighbor'root} ->
    k (pfromData pneighbor'nibble) (pfromData pneighbor'prefix) (pfromData pneighbor'root)

-- | Inequality; Plutarch has @#==@ but no negated form.
(#/==) :: forall (s :: S) (a :: S -> Type). PEq a => Term s a -> Term s a -> Term s PBool
x #/== y = pnot # (x #== y)

infix 4 #/==

--------------------------------------------------------------------------------
-- The Aiken-faithful walk
--------------------------------------------------------------------------------

{- | Aiken @mpf_proof_v1.do_excluding@, and the reason this module exists
separately from @plutarch-onchain-lib@.

Two of its cases differ from that library's @pexcluding@ whenever a step carries
@skip > 0@; both are called out in the module header. Reproducing Aiken's
arithmetic here is what makes the port a replacement rather than a change of
consensus.
-}
pdoExcluding ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PBuiltinList (PAsData PProofStep) :--> PByteString)
pdoExcluding = phoistAcyclic $
  pfix $ \self -> plam $ \path cursor proof ->
    pelimList
      ( \step steps ->
          pmatch (pfromData step) $ \case
            PBranch {pproofStep'skip, pproofStep'neighbors} ->
              plet (cursor + 1 + pfromData pproofStep'skip) $ \nextCursor ->
                pdoBranch
                  # path
                  # cursor
                  # nextCursor
                  # (self # path # nextCursor # steps)
                  # pfromData pproofStep'neighbors
            PFork {pproofStep'skip, pproofStep'neighbor} ->
              plet (pfromData pproofStep'neighbor) $ \neighbor ->
                pelimList
                  ( \_ _ ->
                      plet (cursor + 1 + pfromData pproofStep'skip) $ \nextCursor ->
                        pdoFork
                          # path
                          # cursor
                          # nextCursor
                          # (self # path # nextCursor # steps)
                          # neighbor
                  )
                  -- Aiken drops the skipped nibbles here; the Plutarch library
                  -- prepends them. This line is divergence (1).
                  ( pwithNeighbor neighbor $ \nibbleValue prefix root ->
                      pcombine # (pconsBS' # nibbleValue # prefix) # root
                  )
                  steps
            PLeaf {pproofStep'skip, pproofStep'key, pproofStep'value} ->
              plet (pfromData pproofStep'key) $ \key ->
                pelimList
                  ( \_ _ ->
                      plet (cursor + 1 + pfromData pproofStep'skip) $ \nextCursor ->
                        pdoFork
                          # path
                          # cursor
                          # nextCursor
                          # (self # path # nextCursor # steps)
                          #$ pcon
                            ( PNeighbor
                                { pneighbor'prefix = pdata (psuffix # key # nextCursor)
                                , -- Aiken reads the nibble at `cursor`, the
                                  -- Plutarch library at `next_cursor - 1`.
                                  -- This line is divergence (2).
                                  pneighbor'nibble = pdata (pnibble # key # cursor)
                                , pneighbor'root = pproofStep'value
                                }
                            )
                  )
                  (pcombine # (psuffix # key # cursor) # pfromData pproofStep'value)
                  steps
      )
      pnull_hash
      proof

{- | Aiken @mpf_proof_v1.do_including@ — the library's @including@ specialised to
take the /hash/ of the proved value rather than its preimage.

A published-chunk challenge authenticates its target by digest, because the
preimage never reaches L1, so the walk has to start from the digest directly.
The library's @pincluding@ hashes whatever value it is handed and is therefore
unusable for that. The recursion is otherwise identical, which
@Testing.MpfProof@ pins.
-}
pdoIncludingByHash ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PInteger
        :--> PBuiltinList (PAsData PProofStep)
        :--> PByteString
    )
pdoIncludingByHash = phoistAcyclic $
  pfix $ \self -> plam $ \path valueHash cursor proof ->
    pelimList
      ( \step steps ->
          pmatch (pfromData step) $ \case
            PBranch {pproofStep'skip, pproofStep'neighbors} ->
              plet (cursor + 1 + pfromData pproofStep'skip) $ \nextCursor ->
                pdoBranch
                  # path
                  # cursor
                  # nextCursor
                  # (self # path # valueHash # nextCursor # steps)
                  # pfromData pproofStep'neighbors
            PFork {pproofStep'skip, pproofStep'neighbor} ->
              plet (cursor + 1 + pfromData pproofStep'skip) $ \nextCursor ->
                pdoFork
                  # path
                  # cursor
                  # nextCursor
                  # (self # path # valueHash # nextCursor # steps)
                  # pfromData pproofStep'neighbor
            PLeaf {pproofStep'skip, pproofStep'key, pproofStep'value} ->
              plet (cursor + 1 + pfromData pproofStep'skip) $ \nextCursor ->
                plet (pfromData pproofStep'key) $ \key ->
                  pdoFork
                    # path
                    # cursor
                    # nextCursor
                    # (self # path # valueHash # nextCursor # steps)
                    #$ pcon
                      ( PNeighbor
                          { pneighbor'prefix = pdata (psuffix # key # nextCursor)
                          , pneighbor'nibble = pdata (pnibble # key # (nextCursor - 1))
                          , pneighbor'root = pproofStep'value
                          }
                      )
      )
      (pcombine # (psuffix # path # cursor) # valueHash)
      proof

-- | Aiken MPF branch reconstruction, shared by both proof walks.
pdoBranch ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PByteString
    )
pdoBranch = phoistAcyclic $
  plam $ \path cursor nextCursor root neighbors ->
    plet (pnibble # path # (nextCursor - 1)) $ \branch ->
      plet (pnibbles # path # cursor # (nextCursor - 1)) $ \prefix ->
        pcombine
          # prefix
          # ( pmerkle_16
                # branch
                # root
                # (psliceBS # 0 # pdigestByteCount # neighbors)
                # (psliceBS # 32 # pdigestByteCount # neighbors)
                # (psliceBS # 64 # pdigestByteCount # neighbors)
                # (psliceBS # 96 # pdigestByteCount # neighbors)
            )

-- | Aiken MPF fork reconstruction over Midgard's Aiken-compatible neighbour.
pdoFork ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PInteger
        :--> PByteString
        :--> PNeighbor
        :--> PByteString
    )
pdoFork = phoistAcyclic $
  plam $ \path cursor nextCursor root neighbor ->
    pwithNeighbor neighbor $ \neighborNibble neighborPrefix neighborRoot ->
      plet (pnibble # path # (nextCursor - 1)) $ \branch ->
        plet (pnibbles # path # cursor # (nextCursor - 1)) $ \prefix ->
          pif
            (branch #== neighborNibble)
            perror
            ( pcombine
                # prefix
                # ( psparse_merkle_16
                      # branch
                      # root
                      # neighborNibble
                      # (pcombine # neighborPrefix # neighborRoot)
                  )
            )

-- | Aiken @mpf.do_including@, starting from a value preimage.
pdoIncluding ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PInteger
        :--> PBuiltinList (PAsData PProofStep)
        :--> PByteString
    )
pdoIncluding = phoistAcyclic $
  plam $ \path value cursor proof ->
    pdoIncludingByHash # path # (pblake2b_256 # value) # cursor # proof

--------------------------------------------------------------------------------
-- Total verification
--------------------------------------------------------------------------------

{- | Aiken @mpf_proof_v1.has@.

Total, fail-closed membership verification for untrusted dispute witnesses. The
walk itself is the library's; what this adds is the width and well-formedness
gate that makes it total.
-}
phasV1 ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PByteString :--> PProof :--> PBool)
phasV1 = phoistAcyclic $
  plam $ \root key value proof ->
    plet (pblake2b_256 # key) $ \path ->
      pif
        ( plengthBS
            # root
            #== pdigestByteCount
            #&& pmembershipProofIsWellFormed
            # path
            # 0
            # pto proof
            # pmaximumProofStepCount
        )
        ((pdoIncluding # path # value # 0 # pto proof) #== root)
        (pconstant False)

{- | Aiken @mpf_proof_v1.has_value_hash@.

Membership against a 32-byte value digest. Equivalent to 'phasV1' whenever
@value_hash == blake2b_256(value)@, with the same gate plus a width check on the
digest itself.
-}
phasValueHash ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PByteString :--> PProof :--> PBool)
phasValueHash = phoistAcyclic $
  plam $ \root key valueHash proof ->
    plet (pblake2b_256 # key) $ \path ->
      pif
        ( plengthBS
            # root
            #== pdigestByteCount
            #&& plengthBS
            # valueHash
            #== pdigestByteCount
            #&& pmembershipProofIsWellFormed
            # path
            # 0
            # pto proof
            # pmaximumProofStepCount
        )
        ((pdoIncludingByHash # path # valueHash # 0 # pto proof) #== root)
        (pconstant False)

{- | Aiken @mpf_proof_v1.does_not_have@.

Total, fail-closed non-membership. This deliberately exposes the @excluding@
check as a @Bool@ rather than going through @insert@, whose invalid-proof branch
aborts script evaluation.

Uses 'pdoExcluding' rather than the library's @pexcluding@ — see the module
header.
-}
pdoesNotHave ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PProof :--> PBool)
pdoesNotHave = phoistAcyclic $
  plam $ \root key proof ->
    plet (pblake2b_256 # key) $ \path ->
      pif
        ( plengthBS
            # root
            #== pdigestByteCount
            #&& pnonMembershipProofIsWellFormed
            # path
            # 0
            # pto proof
            # pmaximumProofStepCount
        )
        ((pdoExcluding # path # 0 # pto proof) #== root)
        (pconstant False)

--------------------------------------------------------------------------------
-- Total root transitions
--------------------------------------------------------------------------------

{- | Aiken @mpf_proof_v1.insert_root@.

A new root only after a total non-membership check has made the library's
partial @insert@ precondition true.

Written as the check plus @including@ rather than as a call to @mpf.insert@:
that function re-asserts @excluding(key, proof) == root@, which 'pdoesNotHave'
has already established under Aiken's arithmetic. Routing through the Plutarch
library's @pinsert@ would re-check it under the /other/ arithmetic and could
abort where Aiken succeeds.
-}
pinsertRoot ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PByteString :--> PProof :--> PMaybe PByteString)
pinsertRoot = phoistAcyclic $
  plam $ \root key value proof ->
    pif
      (pdoesNotHave # root # key # proof)
      (pcon (PJust (pdoIncluding # (pblake2b_256 # key) # value # 0 # pto proof)))
      (pcon PNothing)

-- | Aiken @mpf_proof_v1.update_root@.
pupdateRoot ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PProof
        :--> PMaybe PByteString
    )
pupdateRoot = phoistAcyclic $
  plam $ \root key oldValue newValue proof ->
    pif
      (phasV1 # root # key # oldValue # proof)
      (pcon (PJust (pdoIncluding # (pblake2b_256 # key) # newValue # 0 # pto proof)))
      (pcon PNothing)

-- | Aiken @mpf_proof_v1.delete_root@.
pdeleteRoot ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PByteString :--> PProof :--> PMaybe PByteString)
pdeleteRoot = phoistAcyclic $
  plam $ \root key value proof ->
    pif
      (phasV1 # root # key # value # proof)
      (pcon (PJust (pdoExcluding # (pblake2b_256 # key) # 0 # pto proof)))
      (pcon PNothing)
