{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.TransitionTrace
Description : Partial Plutarch port of @lib/midgard/transition-trace.ak@.

The counted-root scheme. A Midgard root is not a bare Merkle root: it is a
commitment to a @(domain, phas_root, count)@ triple. Committing the count
alongside the root is what stops a prover substituting a different tree — the
raw MPF root alone would let a tree with the same membership but a different
size pass.

Ported so far: the root domains, the count proof and membership proof types, the
functions verifying them, and the raw-root membership walk underneath. The
trace-step, event-to-step and non-membership proofs are part of the fraud-proof
layer and are not needed yet.
-}
module Midgard.TransitionTrace (
  PRootDomain (..),
  PRootCountProof (..),
  PRootMembershipProof (..),
  PRootNonMembershipProof (..),
  PIndexedTraceProof,
  PAdjacentTraceProof (..),
  PEventToStepProof (..),
  pcommitCountedRoot,
  pmpfFromMidgardRoot,
  pverifyRootCountProof,
  pverifyRootMembershipRaw,
  pverifyRootMembershipWithBytes,
  pverifyRootNonMembershipRaw,
  pverifyRootNonMembershipWithKeyBytes,
  pverifyIndexedTraceProof,
  pverifyAdjacentTraceProof,
  pverifyEventToStepProof,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Builtin.Data (pasInt, pserialiseData)
import Plutarch.Core.Utils (pand'List)
import Plutarch.MerkleTree.PatriciaForestry (
  PMerklePatriciaForestry,
  pempty,
  pfrom_root,
 )
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Types (PMerkleRoot, PProof)
import Midgard.Env qualified as Env
import Midgard.LedgerState (PTransitionStep (..), ptransitionStepV1IsValid)
import Midgard.MpfProof (pdoExcluding, phasV1)

{- | Aiken @transition_trace.RootDomain@.

Which of a block's seven roots a proof is about. Tags, in order:
@WithdrawalsRootDomain@ 0, @ForcedTransactionsV1RootDomain@ 1,
@TransactionsV1RootDomain@ 2, @DepositsRootDomain@ 3,
@TransitionTraceRootDomain@ 4, @EventToStepRootDomain@ 5,
@ValidationTracesRootDomain@ 6.

The domain is committed into the root, so a proof for one tree cannot be
replayed against another.
-}
data PRootDomain (s :: S)
  = PWithdrawalsRootDomain
  | PForcedTransactionsV1RootDomain
  | PTransactionsV1RootDomain
  | PDepositsRootDomain
  | PTransitionTraceRootDomain
  | PEventToStepRootDomain
  | PValidationTracesRootDomain
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PRootDomain)

-- | Aiken @transition_trace.RootCountProof@.
data PRootCountProof (s :: S) = PRootCountProof
  { prootCount'domain :: Term s (PAsData PRootDomain)
  , prootCount'root :: Term s (PAsData PByteString)
  , prootCount'phasRoot :: Term s (PAsData PByteString)
  , prootCount'count :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PRootCountProof)

{- | Aiken @transition_trace.RootMembershipProof<key, value>@.

Aiken parameterises this over the key and value types, and the ported call sites
disagree about the instantiation: the settlement and user-event proofs carry
@ByteArray@ on both sides, while @withdrawn-reference-input@'s step-03 carries a
@WithdrawalId@ and a whole @WithdrawalInfo@. The wire shape is the same either
way — a @Constr 0@ of seven @Data@ fields — so this port types both as 'PData'
and leaves the interpretation to the call site, which is the only place that
knows it.

Note that neither field is read by the verification below: the key and value
bytes are /arguments/ to 'pverifyRootMembershipWithBytes'. A witness that got to
supply its own encoding could present one tree entry under two different keys.
-}
data PRootMembershipProof (s :: S) = PRootMembershipProof
  { prootMembership'domain :: Term s (PAsData PRootDomain)
  , prootMembership'root :: Term s (PAsData PByteString)
  , prootMembership'phasRoot :: Term s (PAsData PMerkleRoot)
  , prootMembership'count :: Term s (PAsData PInteger)
  , prootMembership'key :: Term s PData
  , prootMembership'value :: Term s PData
  , prootMembership'proof :: Term s (PAsData PProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PRootMembershipProof)

-- | Aiken @transition_trace.counted_root_tag@ — @"MidgardRootCountV1"@.
pcountedRootTag :: forall (s :: S). Term s PByteString
pcountedRootTag = phexByteStr "4d696467617264526f6f74436f756e745631"

{- | Aiken @transition_trace.commit_counted_root@.

@
if count == 0 && phas_root == empty_merkle_tree_root {
  empty_merkle_tree_root
} else {
  blake2b_256(tag ++ serialise(domain) ++ phas_root ++ serialise(count))
}
@

The empty tree is its own commitment rather than a hash, so an empty root is
recognisable without knowing the domain.
-}
pcommitCountedRoot ::
  forall (s :: S).
  Term s (PAsData PRootDomain) ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PByteString
pcommitCountedRoot domain phasRoot count =
  pif
    (count #== 0 #&& phasRoot #== Env.pemptyMerkleTreeRoot)
    Env.pemptyMerkleTreeRoot
    ( pblake2b_256
        #$ pcountedRootTag
        <> (pserialiseData # pforgetData domain)
        <> phasRoot
        <> (pserialiseData # pforgetData (pdata count))
    )

{- | Aiken @transition_trace.phas_root_count_is_consistent@.

The count must be non-negative, and zero exactly when the raw MPF root is the
empty one. This is what stops a non-empty tree being passed off with a count of
zero, or an empty one with a positive count.
-}
pphasRootCountIsConsistent ::
  forall (s :: S). Term s PByteString -> Term s PInteger -> Term s PBool
pphasRootCountIsConsistent root count =
  pand'List
    [ count #>= 0
    , pif (root #== Env.pemptyMerkleTreeRoot) (count #== 0) (count #> 0)
    ]

-- | Aiken @transition_trace.counted_root_is_consistent@.
pcountedRootIsConsistent ::
  forall (s :: S).
  Term s (PAsData PRootDomain) ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PBool
pcountedRootIsConsistent domain root phasRoot count =
  pand'List
    [ pphasRootCountIsConsistent phasRoot count
    , pif
        (count #== 0)
        (root #== Env.pemptyMerkleTreeRoot)
        ( pand'List
            [ pnot # (root #== Env.pemptyMerkleTreeRoot)
            , root #== pcommitCountedRoot domain phasRoot count
            ]
        )
    ]

{- | Aiken @transition_trace.verify_root_count_proof@.

Binds a proof's @(domain, root, count)@ to what the caller expects, and then
checks the root really is the commitment to that domain, raw root and count.
-}
pverifyRootCountProof ::
  forall (s :: S).
  Term s PRootCountProof ->
  Term s (PAsData PRootDomain) ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PBool
pverifyRootCountProof proof expectedDomain expectedRoot expectedCount = pmatch proof $
  \(PRootCountProof {prootCount'domain, prootCount'root, prootCount'phasRoot, prootCount'count}) ->
    pand'List
      [ prootCount'domain #== expectedDomain
      , pfromData prootCount'root #== expectedRoot
      , pfromData prootCount'count #== expectedCount
      , pcountedRootIsConsistent
          prootCount'domain
          (pfromData prootCount'root)
          (pfromData prootCount'phasRoot)
          (pfromData prootCount'count)
      ]

{- | Aiken @transition_trace.mpf_from_midgard_root@.

The empty tree is carried as @env.empty_merkle_tree_root@ rather than as the
library's null hash, so an empty root has to be recognised before it is handed to
'pfrom_root' — which would otherwise treat those 32 bytes as an ordinary root and
walk a proof against a tree that does not exist.
-}
pmpfFromMidgardRoot ::
  forall (s :: S). Term s PByteString -> Term s PMerklePatriciaForestry
pmpfFromMidgardRoot root =
  pif (root #== Env.pemptyMerkleTreeRoot) pempty (pfrom_root # root)

{- | Aiken @transition_trace.verify_root_membership_raw@.

Membership under a /raw/ MPF root together with the count that root is committed
with.

__Written as an @#&&@ chain rather than @pand'List@__, here and in every verifier
below. Aiken's @and { }@ short-circuits and @pand'List@ does not, and the last
conjunct in each of these is an MPF walk that /aborts/ on a malformed proof
rather than returning @False@. Under a strict conjunction a witness rejected by an
earlier check would abort instead, which is the difference between "this proof
does not hold" and "this transaction is invalid". The @count > 0@ conjunct is not redundant with the consistency check above
it: consistency permits @(empty root, 0)@, and membership in an empty tree is
never true, so asserting it here turns a walk that could only fail into an
immediate refusal.
-}
pverifyRootMembershipRaw ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PProof ->
  Term s PBool
pverifyRootMembershipRaw root count keyBytes valueBytes proof =
  pphasRootCountIsConsistent root count
    #&& count
    #> 0
    #&& phasV1
    # root
    # keyBytes
    # valueBytes
    # proof

{- | Aiken @transition_trace.verify_root_membership_with_bytes@.

The whole membership check a dispute witness has to pass: the proof's own
@(domain, root, count)@ must be the ones the caller expects, that triple must be
a consistent counted root, and the key and value bytes must be in the raw tree
underneath it.

The key and value are passed as bytes rather than taken off the witness, because
the caller is the one that knows how they serialise — and a witness that got to
choose its own encoding could present the same tree entry under two different
keys.
-}
pverifyRootMembershipWithBytes ::
  forall (s :: S).
  Term s PRootMembershipProof ->
  Term s (PAsData PRootDomain) ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PBool
pverifyRootMembershipWithBytes witness expectedDomain expectedRoot expectedCount keyBytes valueBytes =
  pmatch witness $
    \( PRootMembershipProof
        { prootMembership'domain
        , prootMembership'root
        , prootMembership'phasRoot
        , prootMembership'count
        , prootMembership'proof
        }
      ) ->
        prootMembership'domain
          #== expectedDomain
          #&& pfromData prootMembership'root
          #== expectedRoot
          #&& pfromData prootMembership'count
          #== expectedCount
          #&& pcountedRootIsConsistent
            prootMembership'domain
            (pfromData prootMembership'root)
            (pfromData prootMembership'phasRoot)
            (pfromData prootMembership'count)
          #&& pverifyRootMembershipRaw
            (pfromData prootMembership'phasRoot)
            (pfromData prootMembership'count)
            keyBytes
            valueBytes
            (pfromData prootMembership'proof)

{- | Aiken @transition_trace.RootNonMembershipProof<key>@.

The mirror of 'PRootMembershipProof' with no value: an absence has nothing to
exhibit but the key it is absent under. @key@ is 'PData' for the same reason —
Aiken parameterises it and the call sites disagree.
-}
data PRootNonMembershipProof (s :: S) = PRootNonMembershipProof
  { prootNonMembership'domain :: Term s (PAsData PRootDomain)
  , prootNonMembership'root :: Term s (PAsData PByteString)
  , prootNonMembership'phasRoot :: Term s (PAsData PMerkleRoot)
  , prootNonMembership'count :: Term s (PAsData PInteger)
  , prootNonMembership'key :: Term s PData
  , prootNonMembership'proof :: Term s (PAsData PProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PRootNonMembershipProof)

{- | Aiken @transition_trace.IndexedTraceProof@ —
@RootMembershipProof<Int, TransitionStep>@.

An alias, as in Aiken. The instantiation is not enforced by the type here, since
this port carries the key and value as 'PData'; what enforces it is
'pverifyIndexedTraceProof', which reads both at their expected types.
-}
type PIndexedTraceProof = PRootMembershipProof

{- | Aiken @transition_trace.AdjacentTraceProof@ — two consecutive trace steps.

Adjacency is what makes a trace a /chain/ rather than a set of steps, so the
pair travels together: a fault about a link cannot be stated over one step.
-}
data PAdjacentTraceProof (s :: S) = PAdjacentTraceProof
  { padjacentTrace'lower :: Term s (PAsData PIndexedTraceProof)
  , padjacentTrace'upper :: Term s (PAsData PIndexedTraceProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAdjacentTraceProof)

{- | Aiken @transition_trace.EventToStepProof@.

Either an event /is/ mapped to a step or it is /not/, and both directions are
evidence: a fault can be "this event was never placed in the trace" as easily as
"it was placed in the wrong one".
-}
data PEventToStepProof (s :: S)
  = PEventToStepMembership
      {peventToStepMembership'membership :: Term s (PAsData PRootMembershipProof)}
  | PEventToStepNonMembership
      {peventToStepNonMembership'nonMembership :: Term s (PAsData PRootNonMembershipProof)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PEventToStepProof)

{- | Aiken @transition_trace.verify_root_non_membership_raw@.

Absence under a raw MPF root. Aiken states it as @expect _inserted =
mpf.insert(tree, key, env.empty, proof)@ — inserting a key that is already there
is impossible, so a successful insert /is/ the absence proof.

The port states the same thing without building the new root, since Aiken
discards it: the proof's @excluding@ walk must reproduce the root the caller
holds. It goes through 'Midgard.MpfProof.pdoExcluding' rather than the Plutarch
library's @pexcluding@, because the two libraries compute @excluding@ differently
whenever a step carries @skip > 0@ — see the divergence recorded in
"Midgard.MpfProof".

__Malformed proofs abort rather than return @False@__, which is also Aiken's
behaviour here: @mpf.insert@ reads past the end of the path and fails, and the
@expect@ turns that into an abort. This is /not/ the fail-closed
@mpf_proof_v1.does_not_have@, whose extra well-formedness gate would refuse
proofs Aiken accepts.
-}
pverifyRootNonMembershipRaw ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PProof ->
  Term s PBool
pverifyRootNonMembershipRaw root count keyBytes proof =
  pphasRootCountIsConsistent root count
    #&& (pdoExcluding # (pblake2b_256 # keyBytes) # 0 # pto proof)
    #== pto (pmpfFromMidgardRoot root)

{- | Aiken @transition_trace.verify_root_non_membership_with_key_bytes@.

The counted-root wrapper: the proof's @(domain, root, count)@ must be the ones
the caller expects, that triple must be a consistent counted root, and the key
must be absent from the raw tree underneath it.

Note there is no @count > 0@ conjunct here, unlike the membership twin. Absence
from an /empty/ tree is true rather than vacuous, so the zero case is a
legitimate proof rather than one that could only fail.
-}
pverifyRootNonMembershipWithKeyBytes ::
  forall (s :: S).
  Term s PRootNonMembershipProof ->
  Term s (PAsData PRootDomain) ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PBool
pverifyRootNonMembershipWithKeyBytes witness expectedDomain expectedRoot expectedCount keyBytes =
  pmatch witness $
    \( PRootNonMembershipProof
        { prootNonMembership'domain
        , prootNonMembership'root
        , prootNonMembership'phasRoot
        , prootNonMembership'count
        , prootNonMembership'proof
        }
      ) ->
        prootNonMembership'domain
          #== expectedDomain
          #&& pfromData prootNonMembership'root
          #== expectedRoot
          #&& pfromData prootNonMembership'count
          #== expectedCount
          #&& pcountedRootIsConsistent
            prootNonMembership'domain
            (pfromData prootNonMembership'root)
            (pfromData prootNonMembership'phasRoot)
            (pfromData prootNonMembership'count)
          #&& pverifyRootNonMembershipRaw
            (pfromData prootNonMembership'phasRoot)
            (pfromData prootNonMembership'count)
            keyBytes
            (pfromData prootNonMembership'proof)

{- | Aiken @transition_trace.verify_indexed_trace_proof@.

A trace step, authenticated under the block's @transition_trace_root@ and then
checked for the four things membership alone does not say.

The load-bearing one is @key == value.step_index@. The tree is keyed by index and
the step /also/ carries its index, so without this a prover could exhibit step 7
under key 3 — a genuine member of the tree, standing in for a step it is not. The
bounds are the other half of the same idea: an index outside @[0, count)@ names a
position the trace does not have.
-}
pverifyIndexedTraceProof ::
  forall (s :: S).
  Term s PIndexedTraceProof ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PBool
pverifyIndexedTraceProof witness expectedRoot expectedCount =
  pmatch witness $
    \(PRootMembershipProof {prootMembership'key, prootMembership'value, prootMembership'count}) ->
      plet (pasInt # prootMembership'key) $ \key ->
        plet (pfromData (punsafeCoerce @(PAsData PTransitionStep) prootMembership'value)) $ \step ->
          pverifyRootMembershipWithBytes
            witness
            (pdata (pcon PTransitionTraceRootDomain))
            expectedRoot
            expectedCount
            (pserialiseData # prootMembership'key)
            (pserialiseData # prootMembership'value)
            #&& pmatch
              step
              (\PTransitionStep {ptransitionStep'stepIndex} -> key #== pfromData ptransitionStep'stepIndex)
            #&& (ptransitionStepV1IsValid # step)
            #&& key
            #>= 0
            #&& key
            #< pfromData prootMembership'count

{- | Aiken @transition_trace.verify_adjacent_trace_proof@.

Two indexed proofs, plus the three things that make them a /link/: same tree,
same size, and consecutive indices. Dropping any one of the three would let a
prover pair steps from two different traces, or the same step with itself.
-}
pverifyAdjacentTraceProof ::
  forall (s :: S).
  Term s PAdjacentTraceProof ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PBool
pverifyAdjacentTraceProof witness expectedRoot expectedCount =
  pmatch witness $ \(PAdjacentTraceProof {padjacentTrace'lower, padjacentTrace'upper}) ->
    plet (pfromData padjacentTrace'lower) $ \lower ->
      plet (pfromData padjacentTrace'upper) $ \upper ->
        pmatch lower $ \lowerFields ->
          pmatch upper $ \upperFields ->
            pverifyIndexedTraceProof lower expectedRoot expectedCount
              #&& pverifyIndexedTraceProof upper expectedRoot expectedCount
              #&& prootMembership'root lowerFields
              #== prootMembership'root upperFields
              #&& prootMembership'count lowerFields
              #== prootMembership'count upperFields
              #&& pasInt
              # prootMembership'key upperFields
              #== (pasInt # prootMembership'key lowerFields) + 1

{- | Aiken @transition_trace.verify_event_to_step_proof@.

Whichever direction the witness claims, checked under @EventToStepRootDomain@
against the same root and count. The domain is a literal on both arms, so a
membership proof for one tree cannot be replayed as a non-membership proof
against another.
-}
pverifyEventToStepProof ::
  forall (s :: S).
  Term s PEventToStepProof ->
  Term s PByteString ->
  Term s PInteger ->
  Term s PBool
pverifyEventToStepProof witness expectedRoot expectedCount = pmatch witness $ \case
  PEventToStepMembership {peventToStepMembership'membership} ->
    plet (pfromData peventToStepMembership'membership) $ \membership ->
      pmatch membership $ \(PRootMembershipProof {prootMembership'key, prootMembership'value}) ->
        pverifyRootMembershipWithBytes
          membership
          (pdata (pcon PEventToStepRootDomain))
          expectedRoot
          expectedCount
          (pserialiseData # prootMembership'key)
          (pserialiseData # prootMembership'value)
  PEventToStepNonMembership {peventToStepNonMembership'nonMembership} ->
    plet (pfromData peventToStepNonMembership'nonMembership) $ \nonMembership ->
      pmatch nonMembership $ \(PRootNonMembershipProof {prootNonMembership'key}) ->
        pverifyRootNonMembershipWithKeyBytes
          nonMembership
          (pdata (pcon PEventToStepRootDomain))
          expectedRoot
          expectedCount
          (pserialiseData # prootNonMembership'key)
