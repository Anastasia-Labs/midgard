{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.MpfProof
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/mpf-proof-v1.ak@.

Total, fail-closed Merkle-Patricia-Forestry verification for untrusted dispute
witnesses. Where an Aiken @test@ exists in @mpf-proof-v1.test.ak@ the case here
carries the same name and the same fixtures, so a divergence between the two
implementations fails the same case on both sides.

Two groups are the port's own.

**Fail-closed, not fail-loud.** The whole point of this module over the raw MPF
library is that a malformed or oversized proof yields @False@ rather than
aborting the script. That is only true if the well-formedness conjunctions are
*lazy*: the nibble comparisons read past the end of the path when the cursor is
out of range, and the recursion below them has no base case for a negative step
budget. Several cases here would abort under a strict `pand'List`.

**The two MPF libraries disagree.** `plutarch-onchain-lib`'s @pexcluding@ and
the Aiken library's @do_excluding@ compute different roots whenever a step
carries @skip > 0@. The port reproduces Aiken's arithmetic and owns the proof
types because the upstream neighbour wire encoding also differs from Aiken.
-}
module Testing.MpfProof (tests) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import PlutusTx.Builtins qualified as Builtins
import Test.Tasty
import Test.Tasty.HUnit

import Aiken.Cbor (pdeserialise)
import Plutarch.MerkleTree.Merkling (pnull_hash)
import Plutarch.Builtin.Data (pserialiseData)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.MpfProof (
  pdeleteRoot,
  pdoExcluding,
  pdoesNotHave,
  phasV1,
  phasValueHash,
  pinsertRoot,
  pproofHasAtMostSteps,
  pupdateRoot,
 )
import Midgard.MpfProof.Types (PNeighbor (..), PProof (..), PProofStep (..))
import Testing.Eval (passertEval, pfails)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "MPF Proof Tests"
    [ membershipTests
    , nonMembershipTests
    , failClosedTests
    , rootTransitionTests
    , wireEncodingTests
    , aikenArithmeticTests
    ]

--------------------------------------------------------------------------------
-- The proof wire encoding
--------------------------------------------------------------------------------

wireEncodingTests :: TestTree
wireEncodingTests =
  testGroup
    "the proof wire encoding"
    [ -- The three step shapes themselves are right: Constr 0, 1 and 2.
      testCase "the three step shapes carry Aiken's constructor tags" $
        holds $
          pall'
            [ pserialised (proofOf [branchStep 0 "\xaa"]) #== pconstant (indefList [constr 0 ["\x00", "\x41\xaa"]])
            , pserialised (proofOf [leafStep 3 "\xdd" "\xee"])
                #== pconstant (indefList [constr 2 ["\x03", "\x41\xdd", "\x41\xee"]])
            ]
    , testCase "mpf_proof_v1_matches_the_canonical_typescript_abi_vector" $
        holds $
          pall'
            [ pserialised abiVectorProof #== pconstant abiVector
            , pnot #$ pserialised abiVectorProof #== pconstant abiVectorListNeighbour
            ]
    , testCase "mpf_proof_v1_rejects_the_obsolete_bare_list_fork_neighbor" $
        holds $
          pall'
            [ pconstant @PByteString abiVector #/= pconstant abiVectorListNeighbour
            , pconstant @PByteString (BS.take 12 abiVector)
                #== pconstant (BS.take 12 abiVectorListNeighbour)
            ]
    , testCase "mpf_proof_v1_rejects_the_obsolete_double_wrapped_fork_neighbor" $
        pfails $
          pforceFirstForkNibble $
            phexByteStr "9fd87a9f01d8799fd8799f0241bb41ccffffffff"
    ]

--------------------------------------------------------------------------------
-- Membership
--------------------------------------------------------------------------------

membershipTests :: TestTree
membershipTests =
  testGroup
    "membership"
    [ testCase "mpf_proof_v1_verifies_empty_proof_membership" $
        holds $ phasV1 # pconstant singletonRoot # pconstant key # pconstant value # emptyProof
    , testCase "mpf_proof_v1_value_hash_membership_agrees_with_the_library" $
        holds $
          pall'
            [ phasV1 # pconstant singletonRoot # pconstant key # pconstant value # emptyProof
            , phasValueHash # pconstant singletonRoot # pconstant key # pconstant (blake2b256 value) # emptyProof
            , -- A wrong digest, a wrong root and a non-32-byte digest all fail
              -- closed.
              pnot #$ phasValueHash # pconstant singletonRoot # pconstant key # pconstant (blake2b256 absentValue) # emptyProof
            , pnot #$ phasValueHash # pnull_hash # pconstant key # pconstant (blake2b256 value) # emptyProof
            , pnot #$ phasValueHash # pconstant singletonRoot # pconstant key # pconstant "\x01\x02" # emptyProof
            , -- And the one-step shape agrees with `has` on both verdicts.
              (phasValueHash # pconstant singletonRoot # pconstant key # pconstant (blake2b256 value) # divergentLeafProof)
                #== (phasV1 # pconstant singletonRoot # pconstant key # pconstant value # divergentLeafProof)
            ]
    , testCase "the step-count bound is inclusive and rejects a negative maximum" $
        holds $
          pall'
            [ pproofHasAtMostSteps # emptyProof # 0
            , pproofHasAtMostSteps # divergentLeafProof # 1
            , pnot #$ pproofHasAtMostSteps # divergentLeafProof # 0
            , pnot #$ pproofHasAtMostSteps # emptyProof # (-1)
            ]
    ]

--------------------------------------------------------------------------------
-- Non-membership
--------------------------------------------------------------------------------

nonMembershipTests :: TestTree
nonMembershipTests =
  testGroup
    "non-membership"
    [ testCase "mpf_proof_v1_verifies_empty_trie_non_membership" $
        holds $ pdoesNotHave # pnull_hash # pconstant key # emptyProof
    , testCase "mpf_proof_v1_rejects_singleton_present_as_non_membership" $
        holds $
          pnot
            #$ pdoesNotHave
            # pconstant singletonRoot
            # pconstant key
            # proofOf [leafStep 0 (blake2b256 key) (blake2b256 value)]
    , -- A fork whose neighbour sits on the *same* branch as the query key is
      -- not an absence witness.
      testCase "mpf_proof_v1_rejects_same_branch_terminal_fork_as_non_membership" $
        holds $
          pnot
            #$ pdoesNotHave
            # pconstant (combine (BS.pack [fromIntegral sameBranch]) nullHash)
            # pconstant key
            # proofOf [forkStep 0 (sameBranch, "", nullHash)]
    , testCase "mpf_proof_v1_accepts_a_divergent_terminal_leaf_non_membership" $
        holds $
          pall'
            [ pconstant @PInteger (fromIntegral (nibble queryPath 0))
                #/= pconstant (fromIntegral (nibble absentPath 0))
            , pdoesNotHave
                # pconstant (combine (suffix absentPath 0) (blake2b256 absentValue))
                # pconstant key
                # proofOf [leafStep 0 absentPath (blake2b256 absentValue)]
            ]
    , testCase "mpf_proof_v1_accepts_a_divergent_terminal_fork_non_membership" $
        holds $
          pdoesNotHave
            # pconstant (combine (BS.pack [fromIntegral differentBranch]) nullHash)
            # pconstant key
            # proofOf [forkStep 0 (differentBranch, "", nullHash)]
    , testCase "mpf_proof_v1_returns_false_for_wrong_roots" $
        holds $
          pall'
            [ pnot #$ phasV1 # pnull_hash # pconstant key # pconstant value # emptyProof
            , pnot #$ pdoesNotHave # pconstant singletonRoot # pconstant key # emptyProof
            ]
    ]

--------------------------------------------------------------------------------
-- Failing closed
--------------------------------------------------------------------------------

failClosedTests :: TestTree
failClosedTests =
  testGroup
    "failing closed"
    [ -- The library's own `insert` would abort on these; this module must
      -- return a verdict.
      testCase "mpf_proof_v1_rejects_a_malformed_fork_without_aborting" $
        holds $
          pall'
            [ pnot #$ phasV1 # pnull_hash # pconstant key # pconstant value # malformedFork
            , pnot #$ pdoesNotHave # pnull_hash # pconstant key # malformedFork
            ]
    , testCase "mpf_proof_v1_rejects_oversized_skips_and_bad_branch_bytes" $
        holds $
          pall'
            [ pnot
                #$ phasV1
                # pnull_hash
                # pconstant key
                # pconstant value
                # proofOf [forkStep 64 (1, "", nullHash)]
            , pnot
                #$ pdoesNotHave
                # pnull_hash
                # pconstant key
                # proofOf [branchStep 0 "\x00"]
            ]
    , -- Each of these would read a nibble past the end of the path, or recurse
      -- without a base case, if the well-formedness conjunction were strict.
      testCase "an out-of-range cursor is rejected rather than read" $
        holds $
          pall'
            [ pnot #$ pdoesNotHave # pnull_hash # pconstant key # proofOf [forkStep 63 (1, "", nullHash)]
            , pnot #$ pdoesNotHave # pnull_hash # pconstant key # proofOf [leafStep 64 (blake2b256 absentKey) (blake2b256 absentValue)]
            , pnot #$ phasV1 # pnull_hash # pconstant key # pconstant value # proofOf [branchStep 64 (BS.replicate 128 0x00)]
            ]
    , testCase "a negative skip is rejected rather than walked" $
        holds $
          pall'
            [ pnot #$ pdoesNotHave # pnull_hash # pconstant key # proofOf [forkStep (-1) (1, "", nullHash)]
            , pnot #$ phasV1 # pnull_hash # pconstant key # pconstant value # proofOf [branchStep (-1) (BS.replicate 128 0x00)]
            ]
    , testCase "a proof longer than the step ceiling is rejected" $
        holds $
          pnot
            #$ pdoesNotHave
            # pnull_hash
            # pconstant key
            # proofOf (replicate 65 (branchStep 0 (BS.replicate 128 0x00)))
    , testCase "a neighbour outside the nibble range is rejected" $
        holds $
          pall'
            [ pnot #$ pdoesNotHave # pnull_hash # pconstant key # proofOf [forkStep 0 (16, "", nullHash)]
            , pnot #$ pdoesNotHave # pnull_hash # pconstant key # proofOf [forkStep 0 (-1, "", nullHash)]
            ]
    , testCase "a neighbour root of the wrong width is rejected" $
        holds $
          pnot
            #$ pdoesNotHave
            # pnull_hash
            # pconstant key
            # proofOf [forkStep 0 (differentBranch, "", BS.replicate 31 0x00)]
    , testCase "a root of the wrong width is rejected" $
        holds $
          pall'
            [ pnot #$ pdoesNotHave # pconstant (BS.replicate 31 0x00) # pconstant key # emptyProof
            , pnot #$ phasV1 # pconstant (BS.replicate 31 0x00) # pconstant key # pconstant value # emptyProof
            ]
    , testCase "a branch neighbours field of the wrong width is rejected" $
        holds $
          pnot
            #$ pdoesNotHave
            # pnull_hash
            # pconstant key
            # proofOf [branchStep 0 (BS.replicate 127 0x00)]
    ]

--------------------------------------------------------------------------------
-- Root transitions
--------------------------------------------------------------------------------

rootTransitionTests :: TestTree
rootTransitionTests =
  testGroup
    "root transitions"
    [ testCase "mpf_proof_v1_total_root_mutations_do_not_call_partial_paths_first" $
        holds $
          pall'
            [ (pinsertRoot # pnull_hash # pconstant key # pconstant value # emptyProof)
                #== pjust (pconstant singletonRoot)
            , pisNothing (pinsertRoot # pconstant singletonRoot # pconstant key # pconstant value # emptyProof)
            , (pupdateRoot # pconstant singletonRoot # pconstant key # pconstant value # pconstant updatedValue # emptyProof)
                #== pjust (pconstant updatedRoot)
            , pisNothing (pupdateRoot # pnull_hash # pconstant key # pconstant value # pconstant updatedValue # emptyProof)
            , (pdeleteRoot # pconstant singletonRoot # pconstant key # pconstant value # emptyProof)
                #== pjust pnull_hash
            , pisNothing (pdeleteRoot # pnull_hash # pconstant key # pconstant value # emptyProof)
            ]
    , -- The partial library functions abort on a bad proof; these must not.
      testCase "a malformed proof yields Nothing rather than aborting" $
        holds $
          pall'
            [ pisNothing (pinsertRoot # pnull_hash # pconstant key # pconstant value # malformedFork)
            , pisNothing (pupdateRoot # pnull_hash # pconstant key # pconstant value # pconstant updatedValue # malformedFork)
            , pisNothing (pdeleteRoot # pnull_hash # pconstant key # pconstant value # malformedFork)
            ]
    ]

--------------------------------------------------------------------------------
-- The divergence between the two MPF libraries
--------------------------------------------------------------------------------

aikenArithmeticTests :: TestTree
aikenArithmeticTests =
  testGroup
    "Aiken excluding arithmetic"
    [ testCase "the port's terminal fork matches Aiken's arithmetic" $
        holds $
          (pdoExcluding # pconstant queryPath # 0 # pto skippingTerminalFork)
            #== pconstant (combine (BS.pack [fromIntegral differentBranch] <> "\xab") nullHash)
    , testCase "skipping proofs pass the well-formedness gate" $
        holds $
          pall'
            [ pdoesNotHave
                # (pdoExcluding # pconstant queryPath # 0 # pto skippingTerminalFork)
                # pconstant key
                # skippingTerminalFork
            , pdoesNotHave
                # (pdoExcluding # pconstant queryPath # 0 # pto skippingLeafThenFork)
                # pconstant key
                # skippingLeafThenFork
            ]
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

key, value, absentKey, absentValue, updatedValue :: BS.ByteString
key = "\x01\x02\x03"
value = "\x04\x05\x06"
absentKey = "\x07\x08\x09"
absentValue = "\x0a\x0b\x0c"
updatedValue = "\x07\x08\x09"

queryPath, absentPath :: BS.ByteString
queryPath = blake2b256 key
absentPath = blake2b256 absentKey

singletonRoot :: BS.ByteString
singletonRoot = combine (suffix queryPath 0) (blake2b256 value)

updatedRoot :: BS.ByteString
updatedRoot = combine (suffix queryPath 0) (blake2b256 updatedValue)

nullHash :: BS.ByteString
nullHash = BS.replicate 32 0x00

sameBranch :: Int
sameBranch = nibble queryPath 0

differentBranch :: Int
differentBranch = if sameBranch == 15 then 0 else sameBranch + 1

--------------------------------------------------------------------------------
-- Proof fixtures
--------------------------------------------------------------------------------

{- | The three-step proof of the canonical ABI vector: one of each step shape.
-}
abiVectorProof :: forall s. Term s PProof
abiVectorProof =
  proofOf [branchStep 0 "\xaa", forkStep 1 (2, "\xbb", "\xcc"), leafStep 3 "\xdd" "\xee"]

-- | What Aiken's @cbor.serialise@ produces for 'abiVectorProof'.
abiVector :: BS.ByteString
abiVector =
  BS.pack
    [ 0x9f
    , 0xd8, 0x79, 0x9f, 0x00, 0x41, 0xaa, 0xff
    , 0xd8, 0x7a, 0x9f, 0x01, 0xd8, 0x79, 0x9f, 0x02, 0x41, 0xbb, 0x41, 0xcc, 0xff, 0xff
    , 0xd8, 0x7b, 0x9f, 0x03, 0x41, 0xdd, 0x41, 0xee, 0xff
    , 0xff
    ]

-- | Serialises a proof the way a redeemer or chunk datum carries it.
pserialised :: forall s. Term s PProof -> Term s PByteString
pserialised proof = pserialiseData #$ pforgetData (pdata (pto proof))

-- | Decode enough of a one-Fork proof to force every nested constructor.
pforceFirstForkNibble :: forall s. Term s PByteString -> Term s PUnit
pforceFirstForkNibble encoded =
  pmatch (pdeserialise # encoded) $ \case
    PNothing -> perror
    PJust rawProof ->
      let proof = pfromData (punsafeCoerce rawProof :: Term s (PAsData PProof))
       in pelimList
            ( \rawStep _ ->
                pmatch (pfromData rawStep) $ \case
                  PFork {pproofStep'neighbor} ->
                    pmatch (pfromData pproofStep'neighbor) $
                      \PNeighbor {pneighbor'nibble} ->
                        plet (pfromData pneighbor'nibble) (const $ pconstant ())
                  _ -> perror
            )
            perror
            (pto proof)

-- | An indefinite-length CBOR array of already-encoded items.
indefList :: [BS.ByteString] -> BS.ByteString
indefList items = BS.pack [0x9f] <> mconcat items <> BS.pack [0xff]

-- | A CBOR @Constr n@ of already-encoded fields, for the small tags used here.
constr :: Int -> [BS.ByteString] -> BS.ByteString
constr tag fields =
  BS.pack [0xd8, fromIntegral (121 + tag), 0x9f] <> mconcat fields <> BS.pack [0xff]

-- | The same vector but with the Fork's neighbour as a bare list.
abiVectorListNeighbour :: BS.ByteString
abiVectorListNeighbour =
  BS.pack
    [ 0x9f
    , 0xd8, 0x79, 0x9f, 0x00, 0x41, 0xaa, 0xff
    , 0xd8, 0x7a, 0x9f, 0x01, 0x9f, 0x02, 0x41, 0xbb, 0x41, 0xcc, 0xff, 0xff
    , 0xd8, 0x7b, 0x9f, 0x03, 0x41, 0xdd, 0x41, 0xee, 0xff
    , 0xff
    ]

emptyProof :: forall s. Term s PProof
emptyProof = proofOf []

-- | A single leaf on a different branch from the query key.
divergentLeafProof :: forall s. Term s PProof
divergentLeafProof = proofOf [leafStep 0 absentPath (blake2b256 absentValue)]

-- | A fork whose neighbour is on the query key's own branch — not an absence.
malformedFork :: forall s. Term s PProof
malformedFork = proofOf [forkStep 0 (sameBranch, "", nullHash)]

{- | A terminal fork that skips one nibble.

Well formed: @cursor + 1 + skip = 2 <= 64@, the neighbour's nibble is in range
and differs from @nibble(path, 1)@, and its root is a digest. Reachable from any
prover.
-}
skippingTerminalFork :: forall s. Term s PProof
skippingTerminalFork = proofOf [forkStep 1 (skippingForkNibble, "\xab", nullHash)]

-- | A nibble differing from the one a skip-1 fork lands on.
skippingForkNibble :: Int
skippingForkNibble = if nibble queryPath 1 == 15 then 0 else nibble queryPath 1 + 1

{- | A leaf that skips one nibble, followed by a terminal fork, so the leaf case
is the non-terminal one.
-}
skippingLeafThenFork :: forall s. Term s PProof
skippingLeafThenFork =
  proofOf
    [ leafStep 1 absentPath (blake2b256 absentValue)
    , forkStep 0 (skippingLeafForkNibble, "", nullHash)
    ]

-- | A nibble differing from the one the second step lands on.
skippingLeafForkNibble :: Int
skippingLeafForkNibble = if nibble queryPath 2 == 15 then 0 else nibble queryPath 2 + 1

--------------------------------------------------------------------------------
-- Building proof steps
--------------------------------------------------------------------------------

proofOf :: forall s. [Term s (PAsData PProofStep)] -> Term s PProof
proofOf steps = pcon (PProof (foldr (\x acc -> pcons # x # acc) pnil steps))

branchStep :: forall s. Integer -> BS.ByteString -> Term s (PAsData PProofStep)
branchStep skip neighbors =
  pdata . pcon $
    PBranch
      { pproofStep'skip = pdata (pconstant skip)
      , pproofStep'neighbors = pdata (pconstant neighbors)
      }

forkStep ::
  forall s. Integer -> (Int, BS.ByteString, BS.ByteString) -> Term s (PAsData PProofStep)
forkStep skip (nib, prefix, root) =
  pdata . pcon $
    PFork
      { pproofStep'skip = pdata (pconstant skip)
      , pproofStep'neighbor =
          pdata . pcon $
            PNeighbor
              { pneighbor'nibble = pdata (pconstant (fromIntegral nib))
              , pneighbor'prefix = pdata (pconstant prefix)
              , pneighbor'root = pdata (pconstant root)
              }
      }

leafStep :: forall s. Integer -> BS.ByteString -> BS.ByteString -> Term s (PAsData PProofStep)
leafStep skip leafKey leafValue =
  pdata . pcon $
    PLeaf
      { pproofStep'skip = pdata (pconstant skip)
      , pproofStep'key = pdata (pconstant leafKey)
      , pproofStep'value = pdata (pconstant leafValue)
      }

--------------------------------------------------------------------------------
-- The MPF primitives, recomputed here
--------------------------------------------------------------------------------

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

combine :: BS.ByteString -> BS.ByteString -> BS.ByteString
combine left right = blake2b256 (left <> right)

-- | The nibble at @index@, high half first.
nibble :: BS.ByteString -> Int -> Int
nibble path index
  | even index = fromIntegral (BS.index path (index `div` 2)) `shiftR` 4
  | otherwise = fromIntegral (BS.index path (index `div` 2)) .&. 0x0f

-- | The MPF suffix marker plus the remaining path.
suffix :: BS.ByteString -> Int -> BS.ByteString
suffix path cursor
  | even cursor = BS.pack [0xff] <> BS.drop (cursor `div` 2) path
  | otherwise =
      BS.pack [0x00, fromIntegral (nibble path cursor)]
        <> BS.drop ((cursor + 1) `div` 2) path

--------------------------------------------------------------------------------
-- Assertion helpers
--------------------------------------------------------------------------------

holds :: (forall s. Term s PBool) -> Assertion
holds = passertEval

pall' :: forall s. [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)

pjust :: forall s a. Term s a -> Term s (PMaybe a)
pjust = pcon . PJust

pisNothing :: forall s a. Term s (PMaybe a) -> Term s PBool
pisNothing m = pmatch m $ \case
  PJust _ -> pconstant False
  PNothing -> pconstant True

(#/=) :: forall s a. PEq a => Term s a -> Term s a -> Term s PBool
x #/= y = pnot # (x #== y)
