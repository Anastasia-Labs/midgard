{- |
Module      : Midgard.ValidationMerkle
Description : Plutarch port of @lib/midgard/validation-merkle-v1.ak@.

A Merkle mountain range, in its compact "frontier" form: instead of carrying a
whole tree, a commitment carries only the *occupied binary peaks* of the leaves
appended so far. Appending a leaf is a binary increment — a carry that merges
equal-height peaks upward — so a list of at most 32 hashes covers the full
@2^32 - 1@ leaf envelope, and a transaction-local list of a handful of items
reveals only a handful of 32-byte hashes on L1.

This is the arithmetic under the bounded-collection and bounded-item proofs,
which are in turn under the native-tx field-chunk verification that the
transaction-order mint side and the field-preimage certificate are waiting on.
It is pure: no ledger types appear here at all, which is why it can be tested
exhaustively against small trees.

=== Short-circuiting

Aiken's @and { .. }@ short-circuits; Plutarch's 'pand'List' does not, because
'pand'' is strict and UPLC application is call-by-value. Usually that costs only
budget, but here two conjunctions have a later term that /errors or diverges/
when an earlier one is false, so those use the lazy '#&&' instead. Both are
marked below. Getting this wrong turns a @False@ into a script failure, which is
the same class of divergence as the @expect@-chain rejection mode.
-}
module Midgard.ValidationMerkle (
  PFrontierPeak (..),
  PBuiltFrontier (..),
  pmaximumLeafCount,
  phashBranch,
  pemptyFrontier,
  pencodeFrontier,
  pfrontierIsWellFormed,
  pfrontierCommitment,
  pappendLeaf,
  pbuildFrontier,
  pverifyMembership,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Builtin.Data (pserialiseData)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottRec (..))

import Midgard.FraudProofs.NativeTx.Codec (
  pencodeDefiniteArrayHeader,
  pencodeDefiniteBytes,
 )

{- | Aiken @validation_merkle_v1.maximum_leaf_count@ — @2^32 - 1@.

The envelope the validation machine's counts live in. A frontier covering it
needs at most 32 peaks.
-}
pmaximumLeafCount :: forall (s :: S). Term s PInteger
pmaximumLeafCount = 4_294_967_295

-- | Aiken @validation_merkle_v1.branch_domain@ — @"MidgardValidationMerkleBranchV1"@.
pbranchDomain :: forall (s :: S). Term s PByteString
pbranchDomain = pconstant "MidgardValidationMerkleBranchV1"

-- | Aiken @validation_merkle_v1.frontier_domain@ — @"MidgardValidationMerkleFrontierV1"@.
pfrontierDomain :: forall (s :: S). Term s PByteString
pfrontierDomain = pconstant "MidgardValidationMerkleFrontierV1"

{- | Aiken @validation_merkle_v1.FrontierPeak@.

One occupied peak: the height of the perfect subtree it roots, and that
subtree's hash.

Two orderings are in play and they are opposites, which is worth holding on to.
The list stores peaks in *ascending* height — low bit first — because a frontier
is the leaf count written in binary. The leaves, however, are laid out with the
*tallest* peak covering the first of them, so 'plocatePeak' walks the heights
downward. Reading either one the other way round produces something that looks
like a working frontier for power-of-two counts and fails for every other.
-}
data PFrontierPeak (s :: S) = PFrontierPeak
  { pfrontierPeak'height :: Term s (PAsData PInteger)
  , pfrontierPeak'hash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFrontierPeak)

{- | Aiken @validation_merkle_v1.BuiltFrontier@.

The result of folding a list of leaves. Scott-encoded rather than data-encoded:
it is a return value that never crosses a @Data@ boundary.
-}
data PBuiltFrontier (s :: S) = PBuiltFrontier
  { pbuiltFrontier'count :: Term s PInteger
  , pbuiltFrontier'peaks :: Term s (PBuiltinList (PAsData PFrontierPeak))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PBuiltFrontier)

{- | Aiken @validation_merkle_v1.hash_branch@.

The domain-separated hash of two children. Both must be 32 bytes — an @expect@,
so a short hash errors rather than producing a shorter preimage that some other
pair of inputs could also produce.
-}
phashBranch ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PByteString)
phashBranch = phoistAcyclic $
  plam $ \left right ->
    pif
      (pand'List [plengthBS # left #== 32, plengthBS # right #== 32])
      (pblake2b_256 # (pbranchDomain <> left <> right))
      perror

-- | Aiken @validation_merkle_v1.empty_frontier@.
pemptyFrontier :: forall (s :: S). Term s (PBuiltinList (PAsData PFrontierPeak))
pemptyFrontier = pcon PNil

{- | Aiken @validation_merkle_v1.encode_peaks@.

Each peak as a two-element CBOR array of its height and its hash, concatenated.
The @#"82"@ is that array header written out — always the two-item form, so it
never needs the length ladder.
-}
pencodePeaks ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PFrontierPeak) :--> PByteString)
pencodePeaks = phoistAcyclic $
  pfix $ \self -> plam $ \peaks ->
    pelimList
      ( \peak rest ->
          pmatch (pfromData peak) $
            \PFrontierPeak {pfrontierPeak'height, pfrontierPeak'hash} ->
              pconstant "\x82"
                <> (pserialiseData # pforgetData pfrontierPeak'height)
                <> (pencodeDefiniteBytes # pfromData pfrontierPeak'hash)
                <> (self # rest)
      )
      (pconstant "")
      peaks

-- | Aiken @validation_merkle_v1.encode_frontier@.
pencodeFrontier ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PFrontierPeak) :--> PByteString)
pencodeFrontier = phoistAcyclic $
  plam $ \peaks ->
    (pencodeDefiniteArrayHeader # (plength # peaks)) <> (pencodePeaks # peaks)

{- | Aiken @validation_merkle_v1.expected_peaks_are_well_formed@.

Reads the count as a binary numeral, least-significant bit first: a set bit at
position @height@ demands a peak of exactly that height, a clear bit demands
none. The peaks must therefore appear in ascending height order and match the
count's binary expansion exactly — no missing peak, no extra one, and no peak at
a height the count does not call for.

The inner conjunction is lazy so a height mismatch stops the walk instead of
recursing on regardless.
-}
pexpectedPeaksAreWellFormed ::
  forall (s :: S).
  Term
    s
    ( PInteger
        :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PBool
    )
pexpectedPeaksAreWellFormed = phoistAcyclic $
  pfix $ \self -> plam $ \remainingCount height peaks ->
    pif
      (remainingCount #== 0)
      (pnull # peaks)
      ( pif
          (pmod # remainingCount # 2 #== 1)
          ( pelimList
              ( \peak rest ->
                  pmatch (pfromData peak) $
                    \PFrontierPeak {pfrontierPeak'height, pfrontierPeak'hash} ->
                      pfromData pfrontierPeak'height
                        #== height
                        #&& plengthBS # pfromData pfrontierPeak'hash
                        #== 32
                        #&& self # (pdiv # remainingCount # 2) # (height + 1) # rest
              )
              (pconstant False)
              peaks
          )
          (self # (pdiv # remainingCount # 2) # (height + 1) # peaks)
      )

{- | Aiken @validation_merkle_v1.frontier_is_well_formed@.

/Short-circuiting is load-bearing here./ 'pexpectedPeaksAreWellFormed' halves the
count toward zero, so a negative count never reaches the base case — it would
recurse until the budget ran out and the script failed, where Aiken returns
@False@. The bounds must be checked lazily ahead of it.
-}
pfrontierIsWellFormed ::
  forall (s :: S).
  Term s (PInteger :--> PBuiltinList (PAsData PFrontierPeak) :--> PBool)
pfrontierIsWellFormed = phoistAcyclic $
  plam $ \count peaks ->
    0
      #<= count
      #&& count
      #<= pmaximumLeafCount
      #&& pexpectedPeaksAreWellFormed # count # 0 # peaks

{- | Aiken @validation_merkle_v1.frontier_commitment@.

The single hash a datum carries in place of the frontier. The count is inside
the preimage, not just the peaks — without it, frontiers of different sizes
whose peak lists happen to coincide would commit alike.

Well-formedness is an @expect@, so committing to a malformed frontier errors.
-}
pfrontierCommitment ::
  forall (s :: S).
  Term s (PInteger :--> PBuiltinList (PAsData PFrontierPeak) :--> PByteString)
pfrontierCommitment = phoistAcyclic $
  plam $ \count peaks ->
    pif
      (pfrontierIsWellFormed # count # peaks)
      ( pblake2b_256
          #$ pfrontierDomain
          <> (pserialiseData # pforgetData (pdata count))
          <> (pencodeFrontier # peaks)
      )
      perror

{- | Aiken @validation_merkle_v1.append_carry@.

The binary increment. An even count means the low bit is clear, so the carry
becomes a new peak at this height and the walk stops; an odd count means a peak
of this height is already there, so the two merge and the carry propagates one
level up. Exactly the carry chain of adding one to a binary numeral.
-}
pappendCarry ::
  forall (s :: S).
  Term
    s
    ( PInteger
        :--> PInteger
        :--> PByteString
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PBuiltinList (PAsData PFrontierPeak)
    )
pappendCarry = phoistAcyclic $
  pfix $ \self -> plam $ \oldCount height carry peaks ->
    pif
      (pmod # oldCount # 2 #== 0)
      ( pcons
          # pdata (pcon (PFrontierPeak (pdata height) (pdata carry)))
          # peaks
      )
      ( pelimList
          ( \left rest ->
              pmatch (pfromData left) $
                \PFrontierPeak {pfrontierPeak'height, pfrontierPeak'hash} ->
                  pif
                    (pfromData pfrontierPeak'height #== height)
                    ( self
                        # (pdiv # oldCount # 2)
                        # (height + 1)
                        # (phashBranch # pfromData pfrontierPeak'hash # carry)
                        # rest
                    )
                    perror
          )
          perror
          peaks
      )

{- | Aiken @validation_merkle_v1.append_leaf@.

Appends one leaf, checking well-formedness on the way in /and/ on the way out.
The second check is not redundant bookkeeping: it is what pins the carry chain
to the count, so a peak list that happened to survive the increment while
disagreeing with @count + 1@ is rejected rather than carried forward.
-}
pappendLeaf ::
  forall (s :: S).
  Term
    s
    ( PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PByteString
        :--> PBuiltinList (PAsData PFrontierPeak)
    )
pappendLeaf = phoistAcyclic $
  plam $ \count peaks leafHash ->
    pif
      ( count
          #< pmaximumLeafCount
          #&& plengthBS # leafHash
          #== 32
          #&& pfrontierIsWellFormed # count # peaks
      )
      ( plet (pappendCarry # count # 0 # leafHash # peaks) $ \next ->
          pif (pfrontierIsWellFormed # (count + 1) # next) next perror
      )
      perror

-- | Aiken @validation_merkle_v1.append_all@.
pappendAll ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PByteString)
        :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PBuiltFrontier
    )
pappendAll = phoistAcyclic $
  pfix $ \self -> plam $ \leaves count peaks ->
    pelimList
      ( \leaf rest ->
          self
            # rest
            # (count + 1)
            # (pappendLeaf # count # peaks # pfromData leaf)
      )
      (pcon (PBuiltFrontier count peaks))
      leaves

-- | Aiken @validation_merkle_v1.build_frontier@.
pbuildFrontier ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PByteString) :--> PBuiltFrontier)
pbuildFrontier = phoistAcyclic $
  plam $ \leaves -> pappendAll # leaves # 0 # pemptyFrontier

{- | Aiken @validation_merkle_v1.power_of_two@.

Note it returns @1@ for a non-positive exponent rather than erroring, which is
what lets 'plocatePeak' run its height down to zero without a special case.
-}
ppowerOfTwo :: forall (s :: S). Term s (PInteger :--> PInteger)
ppowerOfTwo = phoistAcyclic $
  pfix $ \self -> plam $ \exponent' ->
    pif (exponent' #<= 0) 1 (2 * (self # (exponent' - 1)))

{- | Aiken @validation_merkle_v1.highest_bit@.

@floor(log2 value)@ for a positive value, and @0@ for zero or one.
-}
phighestBit :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
phighestBit = phoistAcyclic $
  pfix $ \self -> plam $ \value height ->
    pif (value #< 2) height (self # (pdiv # value # 2) # (height + 1))

-- | Aiken @validation_merkle_v1.PeakLocation@ — internal, so Scott-encoded.
data PPeakLocation (s :: S) = PPeakLocation
  { ppeakLocation'height :: Term s PInteger
  , ppeakLocation'localIndex :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PPeakLocation)

{- | Aiken @validation_merkle_v1.locate_peak@.

Which peak a leaf lives under, and where inside it. Walks the heights from the
tallest down — the opposite of the order the peak list is stored in, because the
tallest peak covers the earliest leaves — skipping heights the count's binary
expansion leaves clear and stepping the offset past each occupied peak it passes
over. The leaf falls in the first peak whose span reaches it.
-}
plocatePeak ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger :--> PInteger :--> PPeakLocation)
plocatePeak = phoistAcyclic $
  pfix $ \self -> plam $ \count leafIndex height offset ->
    pif (height #< 0) perror $
      plet (ppowerOfTwo # height) $ \size ->
        pif
          (pmod # (pdiv # count # size) # 2 #== 1)
          ( pif
              (leafIndex #< offset + size)
              (pcon (PPeakLocation height (leafIndex - offset)))
              (self # count # leafIndex # (height - 1) # (offset + size))
          )
          (self # count # leafIndex # (height - 1) # offset)

{- | Aiken @validation_merkle_v1.peak_hash_at@.

The hash of the peak at a given height. Errors if there is none — which cannot
happen for a height 'plocatePeak' returned against a well-formed frontier, but
/can/ happen for a height a caller supplies otherwise, so the callers below
guard it.
-}
ppeakHashAt ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PFrontierPeak) :--> PInteger :--> PByteString)
ppeakHashAt = phoistAcyclic $
  pfix $ \self -> plam $ \peaks expectedHeight ->
    pelimList
      ( \peak rest ->
          pmatch (pfromData peak) $
            \PFrontierPeak {pfrontierPeak'height, pfrontierPeak'hash} ->
              pif
                (pfromData pfrontierPeak'height #== expectedHeight)
                (pfromData pfrontierPeak'hash)
                (self # rest # expectedHeight)
      )
      perror
      peaks

{- | Aiken @validation_merkle_v1.fold_membership_path@.

Walks a leaf up to its peak. The low bit of the running index says which side
the node is on, and therefore which order the pair hashes in — getting that
backwards would let a proof for one position pass at its mirror.
-}
pfoldMembershipPath ::
  forall (s :: S).
  Term
    s
    ( PInteger
        :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PByteString
    )
pfoldMembershipPath = phoistAcyclic $
  pfix $ \self -> plam $ \localIndex current siblings ->
    pelimList
      ( \sibling rest ->
          plet (pfromData sibling) $ \sib ->
            pif
              (plengthBS # sib #== 32)
              ( plet
                  ( pif
                      (pmod # localIndex # 2 #== 0)
                      (phashBranch # current # sib)
                      (phashBranch # sib # current)
                  )
                  $ \parent -> self # (pdiv # localIndex # 2) # parent # rest
              )
              perror
      )
      current
      siblings

{- | Aiken @validation_merkle_v1.verify_membership@.

The whole point of the frontier: a leaf is in the tree if walking it up through
the supplied siblings lands on the peak the count says it should land on.

Two things worth noting. The sibling count must equal the peak's height exactly,
so a proof cannot be padded or truncated into a different position. And the
final conjunction is /lazy/: 'ppeakHashAt' errors when no peak sits at that
height, and Aiken's short-circuit means a wrong sibling count returns @False@
before that can happen.

Mixed rejection modes, as in the original: the outer guard returns @False@,
while a malformed sibling inside 'pfoldMembershipPath' errors.
-}
pverifyMembership ::
  forall (s :: S).
  Term
    s
    ( PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PInteger
        :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PBool
    )
pverifyMembership = phoistAcyclic $
  plam $ \count peaks leafIndex leafHash siblings ->
    pif
      ( pand'List
          [ pfrontierIsWellFormed # count # peaks
          , 0 #<= leafIndex
          , leafIndex #< count
          , plengthBS # leafHash #== 32
          ]
      )
      ( pmatch (plocatePeak # count # leafIndex # (phighestBit # count # 0) # 0) $
          \PPeakLocation {ppeakLocation'height, ppeakLocation'localIndex} ->
            plength # siblings
              #== ppeakLocation'height
              #&& pfoldMembershipPath # ppeakLocation'localIndex # leafHash # siblings
              #== ppeakHashAt # peaks # ppeakLocation'height
      )
      (pconstant False)
