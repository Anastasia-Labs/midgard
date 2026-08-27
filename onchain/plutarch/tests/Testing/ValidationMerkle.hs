{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.ValidationMerkle
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/validation-merkle-v1.ak@.

The frontier is pure arithmetic over hashes, with no ledger context at all, so
these tests do what the validator tests cannot: they carry a second,
independent implementation of the same scheme in Haskell and check the port
against it, leaf by leaf and proof by proof, over every tree size from one to
eight and every index inside each.

Two of these are here specifically because Plutarch's 'pand'List' is strict
where Aiken's @and { .. }@ short-circuits. A negative leaf count and a
wrong-length sibling list both have to come back as @False@; under a strict
conjunction the first diverges and the second errors. They are marked below.
-}
module Testing.ValidationMerkle (
  tests,

  -- * Reference pieces, shared with the modules built on this one
  blake2b256,
  hashBranch,
  buildFrontier,
  proofForLeaves,
  peaksT,
  siblingsT,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import PlutusCore.Data qualified as PD
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.ValidationMachine (
  PLedgerDeltaControlV1 (..),
  PValidationOneStepWitnessV1 (..),
  pemptyResolutionScheduleHash,
  pencodeLedgerDeltaControlV1,
  pencodeTerminalAcceptanceWitnessV1,
  pinitialResolutionAccumulator,
  pverifyLedgerDeltaTerminalSemanticsV1,
 )
import Midgard.ValidationMerkle (
  PBuiltFrontier (..),
  PFrontierPeak,
  pappendLeaf,
  pbuildFrontier,
  pfrontierCommitment,
  pfrontierIsWellFormed,
  phashBranch,
  pverifyMembership,
 )
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase (..),
  PValidationSourceKind (..),
  PValidationVerdict (..),
  phashWorkWitness,
  pmachineVersion,
 )
import Testing.Eval (passertEval, pfails)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Validation Merkle Tests"
    [ testGroup
        "Aiken conformance"
        [ testCase "validation_merkle_frontier_appends_and_proves_each_leaf" $
            holds aikenFrontierAppendsAndProvesEachLeaf
        , testCase "validation_merkle_membership_rejects_a_wrong_sibling" $
            holds aikenMembershipRejectsWrongSibling
        , testCase "validation_merkle_frontier_rejects_an_occupied_empty_slot" $
            holds aikenFrontierRejectsOccupiedEmptySlot
        , testCase "ledger_delta_terminal_step_binds_more_than_128_operations" $
            holds ledgerDeltaTerminalBindsMoreThan128Operations
        ]
    , testGroup
        "hashBranch"
        [ testCase "agrees with an independent recomputation" $
            holds $
              (phashBranch # pconstant (leaf 1) # pconstant (leaf 2))
                #== pconstant (hashBranch (leaf 1) (leaf 2))
        , -- Order matters, or a proof would pass at its mirror position.
          testCase "is not symmetric in its arguments" $
            holds $
              pnot
                #$ (phashBranch # pconstant (leaf 1) # pconstant (leaf 2))
                #== (phashBranch # pconstant (leaf 2) # pconstant (leaf 1))
        , testCase "rejects a left child that is not 32 bytes" $
            pfails $ phashBranch # pconstant (BS.replicate 31 0x01) # pconstant (leaf 2)
        , testCase "rejects a right child that is not 32 bytes" $
            pfails $ phashBranch # pconstant (leaf 1) # pconstant (BS.replicate 33 0x02)
        ]
    , testGroup
        "frontierIsWellFormed"
        [ testCase "accepts every frontier the reference builds, for 0..8 leaves" $
            holds $
              pall'
                [ pfrontierIsWellFormed # pconstant n # peaksT ps
                | k <- [0 .. 8]
                , let (n, ps) = buildFrontier (map leaf [1 .. k])
                ]
        , -- SHORT-CIRCUIT. A negative count never reaches the recursion's base
          -- case, so a strict conjunction would spin until the budget ran out
          -- and the script failed, where Aiken returns False.
          testCase "returns False for a negative count rather than failing" $
            holds $ pnot #$ pfrontierIsWellFormed # (-1) # peaksT (snd (buildFrontier [leaf 1]))
        , testCase "rejects a count above the maximum leaf count" $
            holds $ pnot #$ pfrontierIsWellFormed # 4_294_967_296 # peaksT []
        , testCase "rejects a peak list that is empty when the count is not" $
            holds $ pnot #$ pfrontierIsWellFormed # 1 # peaksT []
        , testCase "rejects a peak list that is not empty when the count is zero" $
            holds $ pnot #$ pfrontierIsWellFormed # 0 # peaksT [(0, leaf 1)]
        , testCase "rejects a peak at the wrong height" $
            holds $ pnot #$ pfrontierIsWellFormed # 1 # peaksT [(1, leaf 1)]
        , testCase "rejects a peak whose hash is not 32 bytes" $
            holds $ pnot #$ pfrontierIsWellFormed # 1 # peaksT [(0, BS.replicate 31 0x01)]
        , testCase "rejects an extra peak" $
            holds $ pnot #$ pfrontierIsWellFormed # 1 # peaksT [(0, leaf 1), (1, leaf 2)]
        , testCase "rejects a missing peak" $
            holds $ pnot #$ pfrontierIsWellFormed # 3 # peaksT [(1, leaf 1)]
        , -- A frontier is a binary numeral read low bit first, so the peaks
          -- ascend in height. Handing them over the other way round is the
          -- mistake that looks most like a correct frontier.
          testCase "rejects peaks given in descending height order" $
            holds $ pnot #$ pfrontierIsWellFormed # 3 # peaksT [(1, leaf 2), (0, leaf 1)]
        ]
    , testGroup
        "frontierCommitment"
        [ testCase "agrees with an independent recomputation, for 0..8 leaves" $
            holds $
              pall'
                [ (pfrontierCommitment # pconstant n # peaksT ps)
                    #== pconstant (frontierCommitment n ps)
                | k <- [0 .. 8]
                , let (n, ps) = buildFrontier (map leaf [1 .. k])
                ]
        , -- Every tree size gets its own commitment.
          testCase "distinguishes every tree size from one to eight" $
            holds $
              pallDistinctT
                [ pfrontierCommitment # pconstant n # peaksT ps
                | k <- [1 .. 8]
                , let (n, ps) = buildFrontier (map leaf [1 .. k])
                ]
        , testCase "rejects a malformed frontier" $
            pfails $ pfrontierCommitment # 1 # peaksT [(1, leaf 1)]
        ]
    , testGroup
        "buildFrontier and appendLeaf"
        [ testCase "builds the reference's count and peaks, for 0..8 leaves" $
            holds $
              pall'
                [ pmatch (pbuildFrontier # leavesT (map leaf [1 .. k])) $
                  \PBuiltFrontier {pbuiltFrontier'count, pbuiltFrontier'peaks} ->
                    pbuiltFrontier'count
                      #== pconstant n
                      #&& pbuiltFrontier'peaks
                      #== peaksT ps
                | k <- [0 .. 8]
                , let (n, ps) = buildFrontier (map leaf [1 .. k])
                ]
        , testCase "appending one leaf agrees with the reference, for 0..8 leaves" $
            holds $
              pall'
                [ (pappendLeaf # pconstant n # peaksT ps # pconstant (leaf (k + 1)))
                    #== peaksT ps'
                | k <- [0 .. 8]
                , let (n, ps) = buildFrontier (map leaf [1 .. k])
                , let (_, ps') = buildFrontier (map leaf [1 .. k + 1])
                ]
        , testCase "rejects a leaf that is not 32 bytes" $
            pfails $ pappendLeaf # 0 # peaksT [] # pconstant (BS.replicate 31 0x01)
        , testCase "rejects appending to a malformed frontier" $
            pfails $ pappendLeaf # 1 # peaksT [(1, leaf 1)] # pconstant (leaf 2)
        , testCase "rejects appending past the maximum leaf count" $
            pfails $ pappendLeaf # 4_294_967_295 # peaksT [] # pconstant (leaf 1)
        ]
    , testGroup
        "verifyMembership"
        ( [ testCase ("accepts every proof in a tree of " <> show k <> " leaves") $
            holds $
              pall'
                [ verify k i (leaf (fromIntegral i + 1)) (proofFor k i)
                | i <- [0 .. fromIntegral k - 1]
                ]
          | k <- [1 .. 8 :: Int]
          ]
            <> [ testCase "rejects a proof for the wrong leaf hash" $
                  holds $ pnot #$ verify 5 2 (leaf 99) (proofFor 5 2)
               , testCase "rejects a proof with a corrupted sibling" $
                  holds $ pnot #$ verify 5 2 (leaf 3) (corrupt (proofFor 5 2))
               , -- The mirror. `fold_membership_path` picks the hashing order
                 -- off the low bit of the running index, so a proof presented
                 -- one position over must fail even though its siblings are all
                 -- genuine members of the same subtree.
                 testCase "rejects a proof presented at the neighbouring index" $
                  holds $ pnot #$ verify 5 2 (leaf 3) (proofFor 5 3)
               , testCase "rejects a leaf index at the count" $
                  holds $ pnot #$ verify 5 5 (leaf 6) (proofFor 5 4)
               , testCase "rejects a negative leaf index" $
                  holds $ pnot #$ verify 5 (-1) (leaf 1) (proofFor 5 0)
               , testCase "rejects a leaf hash that is not 32 bytes" $
                  holds $
                    pnot
                      #$ pverifyMembership
                      # 5
                      # peaksT (snd (buildFrontier (map leaf [1 .. 5])))
                      # 2
                      # pconstant (BS.replicate 31 0x03)
                      # siblingsT (proofFor 5 2)
               , -- SHORT-CIRCUIT. `peak_hash_at` errors when no peak sits at
                 -- the height, so the sibling-count check has to be evaluated
                 -- first and has to stop the conjunction when it fails.
                 testCase "returns False for too few siblings rather than failing" $
                  holds $ pnot #$ verify 5 2 (leaf 3) (init (proofFor 5 2))
               , testCase "returns False for too many siblings rather than failing" $
                  holds $ pnot #$ verify 5 2 (leaf 3) (proofFor 5 2 <> [leaf 42])
               , testCase "returns False against a malformed frontier" $
                  holds $
                    pnot
                      #$ pverifyMembership
                      # 5
                      # peaksT [(1, leaf 1)]
                      # 2
                      # pconstant (leaf 3)
                      # siblingsT (proofFor 5 2)
               , -- Mixed rejection modes, as in the original: a sibling of the
                 -- wrong length errors inside the fold.
                 testCase "fails on a sibling that is not 32 bytes" $
                  pfails $ verify 5 2 (leaf 3) (BS.replicate 31 0x01 : tail (proofFor 5 2))
               ]
        )
    ]

aikenFrontierAppendsAndProvesEachLeaf :: forall s. Term s PBool
aikenFrontierAppendsAndProvesEachLeaf =
  plet (pconstant $ BS.replicate 32 0xaa) $ \a ->
  plet (pconstant $ BS.replicate 32 0xbb) $ \b ->
  plet (pconstant $ BS.replicate 32 0xcc) $ \c ->
  plet (pappendLeaf # 0 # pnil # a) $ \one ->
  plet (pappendLeaf # 1 # one # b) $ \two ->
  plet (pappendLeaf # 2 # two # c) $ \three ->
    pall'
      [ pfrontierIsWellFormed # 3 # three
      , pverifyMembership # 3 # three # 0 # a # (pcons # pdata b # pnil)
      , pverifyMembership # 3 # three # 1 # b # (pcons # pdata a # pnil)
      , pverifyMembership # 3 # three # 2 # c # pnil
      , pfrontierCommitment # 3 # three
          #== phexByteStr "f257467b03621e7d54b952ac6be9dd6b965bd9f86da60b367ec62b3eb1118ea0"
      , pnot #$ pfrontierCommitment # 3 # three #== pfrontierCommitment # 2 # two
      ]

aikenMembershipRejectsWrongSibling :: forall s. Term s PBool
aikenMembershipRejectsWrongSibling =
  plet (pconstant $ BS.replicate 32 0xaa) $ \a ->
  plet (pconstant $ BS.replicate 32 0xbb) $ \b ->
  plet (pappendLeaf # 0 # pnil # a) $ \one ->
  plet (pappendLeaf # 1 # one # b) $ \two ->
    pnot #$ pverifyMembership # 2 # two # 0 # a
      # (pcons # pdata (pblake2b_256 # b) # pnil)

aikenFrontierRejectsOccupiedEmptySlot :: forall s. Term s PBool
aikenFrontierRejectsOccupiedEmptySlot =
  pnot #$ pfrontierIsWellFormed # 0 # peaksT [(0, BS.replicate 32 0xaa)]

-- The Aiken regression carries the off-chain-built compact frontier rather
-- than constructing 129 leaves on-chain. This keeps the measured term on the
-- terminal resolver and proves that the operation count is not capped at 128.
ledgerDeltaTerminalBindsMoreThan128Operations :: forall s. Term s PBool
ledgerDeltaTerminalBindsMoreThan128Operations =
  plet operationPeaks $ \peaks ->
  plet (pfrontierCommitment # 129 # peaks) $ \operationRoot ->
  plet
    ( pcon $ PLedgerDeltaControlV1
        (pdata 0)
        (pdata pinitialResolutionAccumulator)
        (pdata 0)
        (pdata pnil)
        (pdata 2)
        (pdata pemptyResolutionScheduleHash)
        (pdata 0)
        (pdata pinitialResolutionAccumulator)
        (pdata pemptyResolutionScheduleHash)
        (pdata $ pconstant $ BS.replicate 32 0xbb)
        (pdata 0)
        (pdata 129)
        (pdata peaks)
        (pdata $ pconstant "")
    )
    $ \control ->
  plet (pencodeLedgerDeltaControlV1 # control) $ \controlCbor ->
  plet
    ( pcon $ PValidationMachineStateV1
        (pdata pmachineVersion)
        (pdata $ pconstant $ BS.replicate 32 0xaa)
        (pdata $ pconstant $ BS.replicate 32 0xaa)
        (pdata $ pconstant $ BS.replicate 32 0xbb)
        (pdata $ pconstant $ BS.replicate 32 0xcc)
        (pdata $ pcon PForced)
        (pdata $ pconstant $ BS.replicate 32 0xcc)
        (pdata $ pcon PLedgerDelta)
        (pdata 46)
        (pdata $ phashWorkWitness # pcon PLedgerDelta # 46 # controlCbor)
        (pdata 0)
        (pdata 0)
        (pdata $ pcon PPending)
        (pdata $ pconstant $ BS.replicate 32 0)
        (pdata operationRoot)
    )
    $ \pre ->
  pmatch pre $ \preState ->
  plet
    ( pcon $ PValidationMachineStateV1
        (pmachineState'machineVersion preState)
        (pmachineState'eventKeyHash preState)
        (pmachineState'transactionId preState)
        (pmachineState'transactionCommitment preState)
        (pmachineState'validationContextHash preState)
        (pmachineState'sourceKind preState)
        (pmachineState'priorLedgerRoot preState)
        (pdata $ pcon PTerminal)
        (pdata 47)
        ( pdata $
            phashWorkWitness
              # pcon PTerminal
              # 47
              # ( pencodeTerminalAcceptanceWitnessV1
                    # pconstant (BS.replicate 32 0xbb)
                    # 129
                    # peaks
                )
        )
        (pmachineState'executionCpu preState)
        (pmachineState'executionMemory preState)
        (pdata $ pcon PAccepted)
        (pdata $ pconstant $ BS.replicate 32 0)
        (pmachineState'ledgerDeltaRoot preState)
    )
    $ \post ->
  plet
    ( pcon $ PValidationOneStepWitnessV1
        (pdata controlCbor)
        (pdata post)
    )
    $ \witness ->
    pand'List
      [ pfrontierIsWellFormed # 129 # peaks
      , pverifyLedgerDeltaTerminalSemanticsV1 # pre # witness
      ]
  where
    operationPeaks :: forall t. Term t (PBuiltinList (PAsData PFrontierPeak))
    operationPeaks =
      peaksT
        [ (0, hexBytes "d70952a4347195627444cfbb1874f6857de1ad78f095460b76fc826cd267a589")
        , (7, hexBytes "c4029f40ad899fb153acbf60401c4b0187fc7ce03060d3cbeae09ea32de11cc9")
        ]

    hexBytes :: BS.ByteString -> BS.ByteString
    hexBytes = Base16.decodeLenient

--------------------------------------------------------------------------------
-- Running the port
--------------------------------------------------------------------------------

holds :: (forall s. Term s PBool) -> Assertion
holds = passertEval

-- | Membership against the frontier the reference builds for @k@ leaves.
verify :: forall s. Int -> Integer -> BS.ByteString -> [BS.ByteString] -> Term s PBool
verify k leafIndex leafHash siblings =
  pverifyMembership
    # pconstant count
    # peaksT peaks
    # pconstant leafIndex
    # pconstant leafHash
    # siblingsT siblings
  where
    (count, peaks) = buildFrontier (map leaf [1 .. k])

pall' :: forall s. [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)

-- | Asserts a list of byte strings are pairwise distinct.
pallDistinctT :: forall s. [Term s PByteString] -> Term s PBool
pallDistinctT xs =
  pall' [pnot #$ a #== b | (i, a) <- zip [0 :: Int ..] xs, (j, b) <- zip [0 ..] xs, i < j]

--------------------------------------------------------------------------------
-- The reference implementation
--------------------------------------------------------------------------------

-- | A distinct 32-byte leaf hash per index.
leaf :: Int -> BS.ByteString
leaf n = blake2b256 (BS.pack [fromIntegral n])

hashBranch :: BS.ByteString -> BS.ByteString -> BS.ByteString
hashBranch l r = blake2b256 ("MidgardValidationMerkleBranchV1" <> l <> r)

-- | The frontier after appending @leaves@, as @(count, peaks)@.
buildFrontier :: [BS.ByteString] -> (Integer, [(Integer, BS.ByteString)])
buildFrontier = go 0 []
  where
    go c ps [] = (c, ps)
    go c ps (l : ls) = go (c + 1) (appendCarry c 0 l ps) ls

    appendCarry oldCount height carry peaks
      | even oldCount = (height, carry) : peaks
      | otherwise = case peaks of
          ((h, hash) : rest)
            | h == height ->
                appendCarry (oldCount `div` 2) (height + 1) (hashBranch hash carry) rest
          _ -> error "reference frontier: malformed peaks"

frontierCommitment :: Integer -> [(Integer, BS.ByteString)] -> BS.ByteString
frontierCommitment count peaks =
  blake2b256 $
    "MidgardValidationMerkleFrontierV1"
      <> cborInt count
      <> encodeFrontier peaks

encodeFrontier :: [(Integer, BS.ByteString)] -> BS.ByteString
encodeFrontier peaks =
  arrayHeader (length peaks)
    <> BS.concat ["\x82" <> cborInt h <> definiteBytes hash | (h, hash) <- peaks]

{- | CBOR for a small non-negative integer.

Only the single-byte and one-following-byte forms are needed here; anything
larger is a fixture mistake rather than a case to encode.
-}
cborInt :: Integer -> BS.ByteString
cborInt n
  | n < 0 = error "reference cborInt: negative"
  | n <= 23 = BS.pack [fromIntegral n]
  | n <= 255 = BS.pack [0x18, fromIntegral n]
  | otherwise = error "reference cborInt: out of fixture range"

arrayHeader :: Int -> BS.ByteString
arrayHeader n
  | n <= 23 = BS.pack [fromIntegral (128 + n)]
  | otherwise = error "reference arrayHeader: out of fixture range"

definiteBytes :: BS.ByteString -> BS.ByteString
definiteBytes bytes
  | len <= 23 = BS.pack [fromIntegral (64 + len)] <> bytes
  | len <= 255 = BS.pack [0x58, fromIntegral len] <> bytes
  | otherwise = error "reference definiteBytes: out of fixture range"
  where
    len = BS.length bytes

{- | The sibling path proving leaf @i@ of a @k@-leaf tree.

The peaks partition the leaves into consecutive perfect subtrees in descending
height order; this finds the one holding @i@ and walks the ordinary Merkle path
inside it, bottom up — the order 'pfoldMembershipPath' consumes them in.
-}
proofFor :: Int -> Integer -> [BS.ByteString]
proofFor k = proofForLeaves (map leaf [1 .. k])

-- | 'proofFor' over arbitrary leaf hashes rather than this module's fixtures.
proofForLeaves :: [BS.ByteString] -> Integer -> [BS.ByteString]
proofForLeaves leaves leafIndex = path subLeaves (leafIndex - offset) []
  where
    (_, peaks) = buildFrontier leaves
    -- The peak list is stored low bit first (ascending height), but the leaves
    -- are laid out tallest peak first, so the walk goes over the reverse.
    (height, offset) = locate (reverse peaks) 0

    locate [] _ = error "reference proofFor: leaf index outside the frontier"
    locate ((h, _) : rest) off
      | leafIndex < off + 2 ^ h = (h, off)
      | otherwise = locate rest (off + 2 ^ h)

    subLeaves = take (2 ^ height) (drop (fromIntegral offset) leaves)

    path [_] _ acc = reverse acc
    path level idx acc =
      let sibling =
            if even idx
              then level !! fromIntegral (idx + 1)
              else level !! fromIntegral (idx - 1)
       in path (pairUp level) (idx `div` 2) (sibling : acc)

    pairUp (a : b : rest) = hashBranch a b : pairUp rest
    pairUp _ = []

-- | Flips a byte in the first sibling of a proof.
corrupt :: [BS.ByteString] -> [BS.ByteString]
corrupt (s : rest) = blake2b256 s : rest
corrupt [] = error "reference corrupt: empty proof"

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

--------------------------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------------------------

peaksT :: forall s. [(Integer, BS.ByteString)] -> Term s (PBuiltinList (PAsData PFrontierPeak))
peaksT ps =
  punsafeCoerce $
    pasList # pconstant @PData (PD.List [PD.Constr 0 [PD.I h, PD.B hash] | (h, hash) <- ps])

siblingsT :: forall s. [BS.ByteString] -> Term s (PBuiltinList (PAsData PByteString))
siblingsT = leavesT

leavesT :: forall s. [BS.ByteString] -> Term s (PBuiltinList (PAsData PByteString))
leavesT xs = punsafeCoerce (pasList # pconstant @PData (PD.List (map PD.B xs)))
