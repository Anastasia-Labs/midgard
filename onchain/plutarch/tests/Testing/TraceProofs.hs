{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.TraceProofs
Description : Behavioural tests for the trace-proof half of "Midgard.TransitionTrace".

The counted-root scheme's second half: absences, indexed trace steps, adjacent
pairs and the event-to-step mapping. Everything the transition-trace fault-proof
family is built on, and nothing above it.

Each proof shape is exercised against a real one-entry MPF tree whose root is
rebuilt here from the walk's own definition — with an empty proof, @including@
reduces to @combine(suffix(path, 0), blake2b_256(value))@ and @suffix@ at cursor
0 is @0xff@ followed by the whole path. Writing it out rather than importing it
is the point: a port that agreed with itself would pass either way.

Three things the tests are shaped around:

* __An absence is not a membership with the value dropped.__ It is an
  @excluding@ walk reproducing the root, and it has no @count > 0@ conjunct —
  absence from an empty tree is true rather than vacuous, unlike membership in
  one, which could only ever fail.

* __A trace step carries its own index, and the tree is keyed by it.__ Without
  @key == value.step_index@ a prover could exhibit a genuine member of the tree
  standing in for a step it is not, which is why that case is driven with a real
  membership proof rather than a malformed one.

* __Adjacency needs three checks, not one.__ Same tree, same size, consecutive
  indices. Each is dropped separately below, because each admits a different
  forgery: steps from two traces, steps from two sizes of the same trace, and a
  step paired with itself.
-}
module Testing.TraceProofs (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (dataToBuiltinData, fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.TransitionTrace (
  PAdjacentTraceProof,
  PRootDomain,
  PEventToStepProof,
  PIndexedTraceProof,
  PRootNonMembershipProof,
  pverifyAdjacentTraceProof,
  pverifyEventToStepProof,
  pverifyIndexedTraceProof,
  pverifyRootNonMembershipWithKeyBytes,
 )
import Testing.Eval (passertEval)
import Testing.MpfTrie (
  combine,
  commonNibbles,
  emptyMerkleRoot,
  libraryNullHash,
  nibble,
  nibbles,
  singleEntryRoot,
  suffix,
  twoLeafRoot,
 )

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Trace Proof Tests"
    [ testGroup "the reference tree" treeTests
    , testGroup "verifyRootNonMembershipWithKeyBytes" nonMembershipTests
    , testGroup "verifyIndexedTraceProof" indexedTests
    , testGroup "verifyAdjacentTraceProof" adjacentTests
    , testGroup "verifyEventToStepProof" eventToStepTests
    ]

--------------------------------------------------------------------------------
-- The reference tree
--------------------------------------------------------------------------------

treeTests :: [TestTree]
treeTests =
  [ testCase "a one-entry raw root is the leaf combination" $
      traceRawRoot @?= blake2b256 (BS.cons 0xff (blake2b256 stepKeyBytes <> blake2b256 stepValueBytes))
  , testCase "the counted root commits the domain, the raw root and the count" $
      traceCountedRoot
        @?= blake2b256
          ( "MidgardRootCountV1"
              <> serialise (PD.Constr traceDomain [])
              <> traceRawRoot
              <> serialise (PD.I 1)
          )
  , -- The empty tree is its own commitment rather than a hash, so an empty root
    -- is recognisable without knowing which domain it belongs to.
    testCase "the empty tree is its own counted root" $
      commitCountedRoot traceDomain emptyMerkleRoot 0 @?= emptyMerkleRoot
  , {- The two empty roots, which are not the same 32 bytes. Midgard's sentinel is
       @blake2b_256("")@ and the MPF library's is all zeros; the walk speaks the
       library's and every root in a header speaks Midgard's. -}
    testCase "Midgard's empty root is the hash of the empty string" $
      emptyMerkleRoot @?= blake2b256 ""
  , testCase "…and it is not the MPF library's null hash" $
      assertBool "the two empty roots coincide" $ emptyMerkleRoot /= libraryNullHash
  ]

--------------------------------------------------------------------------------
-- Non-membership
--------------------------------------------------------------------------------

nonMembershipTests :: [TestTree]
nonMembershipTests =
  [ {- Absence from the empty tree, which is the case with no membership
       counterpart: @verify_root_membership_raw@ asserts @count > 0@ because
       membership in an empty tree could only fail, while absence from one is
       simply true. -}
    testCase "any key is absent from the empty tree" $
      passertEval $
        pverifyRootNonMembershipWithKeyBytes
          (nonMembershipT (nonMembership eventDomain emptyMerkleRoot emptyMerkleRoot 0 absentKey))
          (domainT eventDomain)
          (pconstant emptyMerkleRoot)
          0
          (pconstant (serialise absentKey))
  , testCase "refuses a witness naming another domain" $
      prefuses $
        pverifyRootNonMembershipWithKeyBytes
          (nonMembershipT (nonMembership traceDomain emptyMerkleRoot emptyMerkleRoot 0 absentKey))
          (domainT eventDomain)
          (pconstant emptyMerkleRoot)
          0
          (pconstant (serialise absentKey))
  , -- The counted root and its raw root have to agree, or the unwrap is a
    -- statement about a tree the header never committed.
    testCase "refuses a raw root the counted root does not commit" $
      prefuses $
        pverifyRootNonMembershipWithKeyBytes
          (nonMembershipT (nonMembership eventDomain emptyMerkleRoot traceRawRoot 0 absentKey))
          (domainT eventDomain)
          (pconstant emptyMerkleRoot)
          0
          (pconstant (serialise absentKey))
  , -- A positive count over the empty raw root is the inconsistency the
    -- count-consistency check exists for.
    testCase "refuses an empty raw root carrying a positive count" $
      prefuses $
        pverifyRootNonMembershipWithKeyBytes
          (nonMembershipT (nonMembership eventDomain emptyMerkleRoot emptyMerkleRoot 1 absentKey))
          (domainT eventDomain)
          (pconstant emptyMerkleRoot)
          1
          (pconstant (serialise absentKey))
  , testCase "refuses a witness naming another count" $
      prefuses $
        pverifyRootNonMembershipWithKeyBytes
          (nonMembershipT (nonMembership eventDomain emptyMerkleRoot emptyMerkleRoot 0 absentKey))
          (domainT eventDomain)
          (pconstant emptyMerkleRoot)
          1
          (pconstant (serialise absentKey))
  ]

--------------------------------------------------------------------------------
-- Indexed trace proofs
--------------------------------------------------------------------------------

indexedTests :: [TestTree]
indexedTests =
  [ testCase "accepts a step committed under its own index" $
      passertEval $ runIndexed (indexedProof 0 (step 0))
  , {- The check that stops a genuine member standing in for a step it is not.
       The tree really does hold this @(key, value)@ pair — it is built around it
       — and the proof really does verify; what fails is the step's own claim
       about which index it occupies. -}
    testCase "refuses a step whose own index is not the key it is under" $
      prefuses $ runIndexed (indexedProof 0 (step 1))
  , testCase "refuses a step of another schema version" $
      prefuses $ runIndexed (indexedProof 0 (stepWithVersion 2 0))
  , -- The tree is keyed by position, so a position the trace does not have
    -- cannot hold a step whatever the proof says.
    testCase "refuses a negative index" $
      prefuses $ runIndexed (indexedProof (-1) (step (-1)))
  , testCase "refuses an index at the trace's length" $
      prefuses $ runIndexed (indexedProof 1 (step 1))
  ]

--------------------------------------------------------------------------------
-- Adjacent trace proofs
--------------------------------------------------------------------------------

adjacentTests :: [TestTree]
adjacentTests =
  [ testCase "accepts two consecutive steps of one trace" $
      passertEval $ runAdjacent (adjacent (pairProof 0) (pairProof 1))
  , -- Three separate forgeries, three separate checks.
    testCase "refuses steps from two different traces" $
      prefuses $ runAdjacent (adjacent (pairProof 0) (pairProofUnder otherRoot 3 1))
  , {- The pair must agree with the trace length the caller names. Aiken checks
       this twice over — each indexed proof binds the count, and the link binds
       the two counts to each other — and the two cannot be separated by a test,
       because a count differing from its partner's also differs from the
       caller's. -}
    testCase "refuses a step claiming another trace length" $
      prefuses $ runAdjacent (adjacent (pairProof 0) (pairProofOfCount 4 1))
  , testCase "refuses a step paired with itself" $
      prefuses $ runAdjacent (adjacent (pairProof 0) (pairProof 0))
  , testCase "refuses steps two apart" $
      prefuses $ runAdjacent (adjacent (pairProof 0) (pairProof 2))
  , -- A real two-leaf trie: both proofs verify against one root, so the link
    -- checks are the only thing left standing between them.
    testCase "both members of the pair are genuinely in one tree" $
      passertEval $ runIndexedIn pairCountedRoot 3 (pairProof 0)
  , testCase "…and so is the upper" $
      passertEval $ runIndexedIn pairCountedRoot 3 (pairProof 1)
  , -- Order matters: the upper must follow the lower, not precede it.
    testCase "refuses a pair given in descending order" $
      prefuses $ runAdjacent (adjacent (pairProof 1) (pairProof 0))
  ]

--------------------------------------------------------------------------------
-- Event-to-step proofs
--------------------------------------------------------------------------------

eventToStepTests :: [TestTree]
eventToStepTests =
  [ testCase "accepts a membership witness under the event-to-step domain" $
      passertEval $
        pverifyEventToStepProof
          (eventProofT (PD.Constr 0 [membership eventDomain eventCountedRoot eventRawRoot 1 eventKey eventValue]))
          (pconstant eventCountedRoot)
          1
  , testCase "accepts a non-membership witness under the same domain" $
      passertEval $
        pverifyEventToStepProof
          ( eventProofT
              (PD.Constr 1 [nonMembership eventDomain emptyMerkleRoot emptyMerkleRoot 0 absentKey])
          )
          (pconstant emptyMerkleRoot)
          0
  , {- The domain is a literal on both arms, so neither can be replayed against
       another of the block's seven trees. -}
    testCase "refuses a membership witness under another domain" $
      prefuses $
        pverifyEventToStepProof
          (eventProofT (PD.Constr 0 [membership traceDomain traceCountedRoot traceRawRoot 1 eventKey eventValue]))
          (pconstant traceCountedRoot)
          1
  , testCase "refuses a non-membership witness under another domain" $
      prefuses $
        pverifyEventToStepProof
          ( eventProofT
              (PD.Constr 1 [nonMembership traceDomain emptyMerkleRoot emptyMerkleRoot 0 absentKey])
          )
          (pconstant emptyMerkleRoot)
          0
  , testCase "refuses a membership witness against another root" $
      prefuses $
        pverifyEventToStepProof
          (eventProofT (PD.Constr 0 [membership eventDomain eventCountedRoot eventRawRoot 1 eventKey eventValue]))
          (pconstant otherRoot)
          1
  ]

--------------------------------------------------------------------------------
-- Driving the verifiers
--------------------------------------------------------------------------------

{- | A refusal, which for these verifiers is @False@ rather than an abort.

Worth its own name because reaching for 'Testing.Eval.pfails' here would pass for
the wrong reason on any verifier that aborts, and silently pass on one that
returns the wrong verdict.
-}
prefuses :: (forall s. Term s PBool) -> Assertion
prefuses p = passertEval (pnot # p)

runIndexed :: forall s. PD.Data -> Term s PBool
runIndexed = runIndexedIn traceCountedRoot 1

runIndexedIn :: forall s. BS.ByteString -> Integer -> PD.Data -> Term s PBool
runIndexedIn root count proof =
  pverifyIndexedTraceProof (indexedT proof) (pconstant root) (pconstant count)

runAdjacent :: forall s. PD.Data -> Term s PBool
runAdjacent proof =
  pverifyAdjacentTraceProof (adjacentT proof) (pconstant pairCountedRoot) 3

--------------------------------------------------------------------------------
-- Proof payloads
--------------------------------------------------------------------------------

{- | @transition_trace.RootMembershipProof@ — seven fields, declaration order.

The proof itself is empty: every tree here holds one entry, so the walk has no
neighbours to fold in.
-}
membership ::
  Integer -> BS.ByteString -> BS.ByteString -> Integer -> PD.Data -> PD.Data -> PD.Data
membership domain root rawRoot count key value =
  membershipWithProof domain root rawRoot count key value (PD.List [])

-- | The same, with the MPF proof under the caller's control.
membershipWithProof ::
  Integer ->
  BS.ByteString ->
  BS.ByteString ->
  Integer ->
  PD.Data ->
  PD.Data ->
  PD.Data ->
  PD.Data
membershipWithProof domain root rawRoot count key value proof =
  PD.Constr 0 [PD.Constr domain [], PD.B root, PD.B rawRoot, PD.I count, key, value, proof]

-- | @transition_trace.RootNonMembershipProof@ — six fields; no value to exhibit.
nonMembership :: Integer -> BS.ByteString -> BS.ByteString -> Integer -> PD.Data -> PD.Data
nonMembership domain root rawRoot count key =
  PD.Constr 0 [PD.Constr domain [], PD.B root, PD.B rawRoot, PD.I count, key, PD.List []]

{- | An indexed trace proof over a one-step trace.

The tree is rebuilt around whatever step is passed, so the membership proof
always verifies and the only thing under test is what the /step/ claims about
itself.
-}
indexedProof :: Integer -> PD.Data -> PD.Data
indexedProof index value =
  membership traceDomain countedRoot rawRoot 1 (PD.I index) value
  where
    rawRoot = singleEntryRoot (serialise (PD.I index)) (serialise value)
    countedRoot = commitCountedRoot traceDomain rawRoot 1

{- | One member of an adjacent pair, out of a __real two-entry trace tree__
holding steps 0 and 1.

The count is 3 so that index 2 is in range and "steps two apart" is a case about
adjacency rather than about bounds; only steps 0 and 1 are actually committed,
which is all the link checks need.
-}
pairProof :: Integer -> PD.Data
pairProof = pairProofUnder pairCountedRoot 3

pairProofUnder :: BS.ByteString -> Integer -> Integer -> PD.Data
pairProofUnder root count index =
  membershipWithProof
    traceDomain
    root
    pairRawRoot
    count
    (PD.I index)
    (step index)
    (pairProofSteps index)

-- | The same, claiming a trace of another length.
pairProofOfCount :: Integer -> Integer -> PD.Data
pairProofOfCount count = pairProofUnder (commitCountedRoot traceDomain pairRawRoot count) count

{- | The two-entry trie holding steps 0 and 1, and the inclusion proof for each.

Both are built here from the walk's own definition — @suffix@, @nibbles@,
@combine@ and @sparse_merkle_16@ reimplemented below — rather than taken from
@plutarch-onchain-lib@. That is what makes the adjacency cases mean something: a
one-entry-per-step fixture would have let each proof verify against its own tree,
which is precisely the forgery the "same tree" check exists to stop.
-}
pairRawRoot :: BS.ByteString
pairRawRoot = twoLeafRoot (pairEntry 0) (pairEntry 1)

pairCountedRoot :: BS.ByteString
pairCountedRoot = commitCountedRoot traceDomain pairRawRoot 3

pairEntry :: Integer -> (BS.ByteString, BS.ByteString)
pairEntry index = (serialise (PD.I index), serialise (step index))

{- | The one-step inclusion proof for one leaf of a two-leaf trie: a @Leaf@ step
naming the /other/ leaf's path and value digest, skipping the nibbles the two
paths share.
-}
pairProofSteps :: Integer -> PD.Data
pairProofSteps index =
  PD.List
    [ PD.Constr
        2
        [ PD.I (fromIntegral (commonNibbles pathA pathB))
        , PD.B neighbourPath
        , PD.B (blake2b256 neighbourValue)
        ]
    ]
  where
    (keyA, _) = pairEntry index
    (keyB, neighbourValue) = pairEntry (1 - index)
    pathA = blake2b256 keyA
    pathB = blake2b256 keyB
    neighbourPath = pathB

adjacent :: PD.Data -> PD.Data -> PD.Data
adjacent lower upper = PD.Constr 0 [lower, upper]

--------------------------------------------------------------------------------
-- A two-leaf Merkle-Patricia trie, from the walk's definition
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Steps and keys
--------------------------------------------------------------------------------

{- | @ledger_state.TransitionStep@ — six fields, declaration order.

@phase@ is @Withdrawal@ (tag 0) throughout; nothing here reads it, and the two
roots are opaque because adjacency in this module is about indices rather than
about chaining ledger states.
-}
step :: Integer -> PD.Data
step = stepWithVersion 1

stepWithVersion :: Integer -> Integer -> PD.Data
stepWithVersion version index =
  PD.Constr
    0
    [ PD.I version
    , PD.I index
    , eventKey
    , PD.Constr 0 [] -- Withdrawal
    , PD.B (hash32 0x01)
    , PD.B (hash32 0x02)
    ]

-- | @ledger_state.WithdrawalEventKey@ over an output reference.
eventKey :: PD.Data
eventKey = PD.Constr 0 [PD.Constr 0 [PD.B (BS.replicate 32 0x71), PD.I 0]]

-- | @ledger_state.EventToStepValue@ — step 0, phase @Withdrawal@.
eventValue :: PD.Data
eventValue = PD.Constr 0 [PD.I 0, PD.Constr 0 []]

-- | A key no tree here holds.
absentKey :: PD.Data
absentKey = PD.Constr 3 [PD.Constr 0 [PD.B (BS.replicate 32 0x9a), PD.I 0]]

--------------------------------------------------------------------------------
-- The trees
--------------------------------------------------------------------------------

stepKeyBytes, stepValueBytes :: BS.ByteString
stepKeyBytes = serialise (PD.I 0)
stepValueBytes = serialise (step 0)

traceRawRoot :: BS.ByteString
traceRawRoot = singleEntryRoot stepKeyBytes stepValueBytes

traceCountedRoot :: BS.ByteString
traceCountedRoot = commitCountedRoot traceDomain traceRawRoot 1

eventRawRoot :: BS.ByteString
eventRawRoot = singleEntryRoot (serialise eventKey) (serialise eventValue)

eventCountedRoot :: BS.ByteString
eventCountedRoot = commitCountedRoot eventDomain eventRawRoot 1

otherRoot :: BS.ByteString
otherRoot = BS.replicate 32 0x52

{- | @TransitionTraceRootDomain@ is the fifth constructor of @RootDomain@ and
@EventToStepRootDomain@ the sixth.
-}
traceDomain, eventDomain :: Integer
traceDomain = 4
eventDomain = 5

--------------------------------------------------------------------------------
-- Reference encoders
--------------------------------------------------------------------------------

-- | @transition_trace.commit_counted_root@, rebuilt from the spec.
commitCountedRoot :: Integer -> BS.ByteString -> Integer -> BS.ByteString
commitCountedRoot domain rawRoot count
  | count == 0 && rawRoot == emptyMerkleRoot = emptyMerkleRoot
  | otherwise =
      blake2b256
        ( "MidgardRootCountV1"
            <> serialise (PD.Constr domain [])
            <> rawRoot
            <> serialise (PD.I count)
        )

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

serialise :: PD.Data -> BS.ByteString
serialise = fromBuiltin . Builtins.serialiseData . dataToBuiltinData

hash32 :: Int -> BS.ByteString
hash32 n = blake2b256 (BS.pack [fromIntegral n])

--------------------------------------------------------------------------------
-- Coercions
--------------------------------------------------------------------------------

indexedT :: forall s. PD.Data -> Term s PIndexedTraceProof
indexedT = punsafeFromData

adjacentT :: forall s. PD.Data -> Term s PAdjacentTraceProof
adjacentT = punsafeFromData

nonMembershipT :: forall s. PD.Data -> Term s PRootNonMembershipProof
nonMembershipT = punsafeFromData

eventProofT :: forall s. PD.Data -> Term s PEventToStepProof
eventProofT = punsafeFromData

punsafeFromData :: forall a s. PD.Data -> Term s a
punsafeFromData d = punsafeCoerce (pconstant @PData d)

{- | A @RootDomain@ by tag.

The verifiers take the expected domain as a typed value; naming it by tag here
keeps the fixture from sharing a spelling of the enum with the port, so a
renumbering fails a test instead of two copies agreeing.
-}
domainT :: forall s. Integer -> Term s (PAsData PRootDomain)
domainT n = punsafeCoerce (pconstant @PData (PD.Constr n []))
