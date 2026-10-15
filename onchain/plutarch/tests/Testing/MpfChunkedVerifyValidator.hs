{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.MpfChunkedVerifyValidator
Description : Behavioural tests for the Plutarch port of
              @validators/mpf-chunked-verify.ak@ — the merkelized verifier for
              published-chunk MPF proof carriage.

The walk's own invariants are covered guard by guard in
"Testing.MpfChunkedProof" and "Testing.MpfProof", which drive the very functions
this validator delegates to. What is exercised here is the validator's own
surface: the two terminals it dispatches on, at the depth the carriage exists
for, over publication UTxOs shaped exactly as a publisher builds them — plus the
@else(_) { fail }@ that keeps every other purpose out.

The depth is the point. A 22-level all-@Branch@ ladder is past where the
redeemer-carried route fits inside the preserved 16,384-byte envelope, which is
the whole reason this script exists; at 16 steps a chunk it takes two publication
UTxOs, and the tests assert that count rather than assuming it.

Every test case here carries the name of the Aiken @test@ block it reproduces.
The root each claim names is reconstructed by folding the MPF primitives
directly — @combine@, @suffix@, @nibbles@, @merkle_16@, written out below —
rather than by calling the verifier under test, so the two sides have to agree
about the format and not merely about each other.
-}
module Testing.MpfChunkedVerifyValidator (tests) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusLedgerApi.V3 (
  Credential (..),
  Datum (..),
  OutputDatum (..),
  ScriptContext (..),
  ScriptHash (..),
  ScriptInfo (..),
  TxId (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (dataToBuiltinData, fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.MpfChunkedVerify (mpfChunkedVerifyStakeValidator)
import Testing.Eval (pfails, psucceeds)
import Testing.ScriptContextBuilder (buildScriptContext, withRewardingScript)

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "MPF Chunked Verify Validator Tests"
    [ testGroup "the fixture" fixtureTests
    , testGroup "membership terminal" membershipTests
    , testGroup "non-membership terminal" nonMembershipTests
    , testGroup "carriage" carriageTests
    , testGroup "purpose" purposeTests
    ]

--------------------------------------------------------------------------------
-- The fixture
--------------------------------------------------------------------------------

{- | The depth is what the script exists for, so it is asserted rather than
assumed: a 22-step ladder does not fit in one chunk and does not fit in the
redeemer-carried envelope either.
-}
fixtureTests :: [TestTree]
fixtureTests =
  [ testCase "the deep proof publishes as exactly two chunks" $
      length deepChunks @?= 2
  , testCase "the ladder is 22 all-Branch steps" $
      length deepProof @?= 22
  , testCase "the first chunk is packed to the per-chunk step bound" $
      chunkStepCount (head deepChunks) @?= 16
  ]

--------------------------------------------------------------------------------
-- Membership
--------------------------------------------------------------------------------

membershipTests :: [TestTree]
membershipTests =
  [ -- Honest acceptance: a 22-level proof — past the depth the redeemer-carried
    -- route can fit inside the 16,384-byte envelope — reassembled from two
    -- publication UTxOs.
    testCase "accepts a deep published membership proof" $
      psucceeds $ run (membershipClaim deepRoot [0, 1]) deepChunks
  , -- The protocol ceiling is 64 proof steps. At sixteen steps per published
    -- chunk this is the four-UTxO maximum profile pinned by the Aiken suite.
    testCase "chunked_inclusion_v1_verifies_the_protocol_maximum_step_count" $ do
      length maximumChunks @?= 4
      psucceeds $ run (membershipClaim maximumRoot [0, 1, 2, 3]) maximumChunks
  , -- A withheld chunk changes the reconstructed root.
    testCase "rejects a withheld chunk" $
      pfails $ run (membershipClaim deepRoot [0]) deepChunks
  , -- Chunk order is proof order.
    testCase "rejects reordered chunks" $
      pfails $ run (membershipClaim deepRoot [1, 0]) deepChunks
  , -- The claimed root is what the walk must reconstruct.
    testCase "rejects a substituted root" $
      pfails $ run (membershipClaim (blake2b256 "\x0b\xad\xc0\xde") [0, 1]) deepChunks
  , -- The claimed value digest is bound.
    testCase "rejects a substituted value digest" $
      pfails $
        run
          (membershipClaim deepRoot [0, 1]) {cValueHash = blake2b256 "\xd4\xc3\xb2\xa1"}
          deepChunks
  , -- The key is bound too, and it is bound before hashing: the walk hashes the
    -- claim's key itself, so a claim naming a different key reconstructs a
    -- different path and a different root.
    testCase "rejects a substituted key" $
      pfails $
        run (membershipClaim deepRoot [0, 1]) {cKeyBytes = BS.replicate 32 0x32} deepChunks
  ]

--------------------------------------------------------------------------------
-- Non-membership
--------------------------------------------------------------------------------

nonMembershipTests :: [TestTree]
nonMembershipTests =
  [ -- A canonical absence witness, dispatched through the non-membership
    -- terminal. The precondition is what makes it an absence witness at all:
    -- the leaf the trie does hold branches away from the queried path at the
    -- very first nibble.
    testCase "the absence fixture branches away at nibble 0" $
      assertBool "leaf path shares nibble 0 with the queried path" $
        nibble (blake2b256 targetKey) 0 /= nibble absentLeafPath 0
  , testCase "accepts a published absence witness" $
      psucceeds $ run absenceClaim [chunkRefInput 0 [absentLeafStep]]
  , -- A key the trie really holds cannot be proved absent through this
    -- validator either: the terminal is what changes, and the walk that
    -- reconstructs the root under it is the one that refuses.
    testCase "rejects a present key as absent" $
      pfails $
        run
          (membershipClaim deepRoot [0, 1])
            { cMode = NonMembershipMode
            , cValueHash = absentValueHash
            }
          deepChunks
  , -- The absence witness is bound to its own root as tightly as a membership
    -- proof is.
    testCase "rejects an absence claim under a substituted root" $
      pfails $
        run absenceClaim {cRoot = blake2b256 "\x0b\xad\xc0\xde"} [chunkRefInput 0 [absentLeafStep]]
  ]

--------------------------------------------------------------------------------
-- Carriage
--------------------------------------------------------------------------------

{- | The claim names publication UTxOs by position, so the positions are part of
what the walk depends on. None of these reaches the walk: 'published_proof'
fails closed and the validator turns that into a refusal.
-}
carriageTests :: [TestTree]
carriageTests =
  [ testCase "rejects an index past the end of the reference inputs" $
      pfails $ run (membershipClaim deepRoot [0, 2]) deepChunks
  , testCase "rejects a negative index" $
      pfails $ run (membershipClaim deepRoot [-1, 1]) deepChunks
  , testCase "rejects a repeated index" $
      pfails $ run (membershipClaim deepRoot [0, 0]) deepChunks
  , testCase "chunked_inclusion_v1_rejects_more_chunks_than_the_chunk_bound" $
      pfails $ run (membershipClaim deepRoot [0 .. 8]) deepChunks
  , testCase "rejects an empty chunk order for a proof that needs steps" $
      pfails $ run (membershipClaim deepRoot []) deepChunks
  , testCase "rejects a reference input carrying no datum" $
      pfails $ run (membershipClaim deepRoot [0, 1]) [head deepChunks, noDatumRefInput 1]
  , testCase "rejects a reference input whose datum is not a chunk" $
      pfails $
        run (membershipClaim deepRoot [0, 1]) [head deepChunks, notAChunkRefInput 1]
  , testCase "chunked_inclusion_v1_rejects_an_empty_chunk" $
      pfails $
        run
          (membershipClaim (membershipRootOf targetKey targetValue []) [0])
          [chunkRefInput 0 []]
  ]

--------------------------------------------------------------------------------
-- Purpose
--------------------------------------------------------------------------------

{- | Aiken's @else(_) { fail }@. The walk is reachable from @withdraw@ and from
nothing else — a merkelized validator that could also be satisfied as, say, a
minting policy would let a step delegate to a purpose the step never checked.
-}
purposeTests :: [TestTree]
purposeTests =
  [ testCase "the withdraw purpose reaches the walk" $
      psucceeds $ run (membershipClaim deepRoot [0, 1]) deepChunks
  , testCase "a minting purpose fails" $
      pfails $
        pvalidator (asMinting (context deepChunks (claimData (membershipClaim deepRoot [0, 1]))))
  , testCase "a spending purpose fails" $
      pfails $
        pvalidator (asSpending (context deepChunks (claimData (membershipClaim deepRoot [0, 1]))))
  ]

--------------------------------------------------------------------------------
-- Driving the validator
--------------------------------------------------------------------------------

run :: forall s. Claim -> [TxInInfo] -> Term s PUnit
run claim refs = pvalidator (context refs (claimData claim))

pvalidator :: forall s. ScriptContext -> Term s PUnit
pvalidator ctx = mpfChunkedVerifyStakeValidator # pconstant ctx

{- | A withdraw context whose reference inputs are exactly these, in exactly
this order. Built through 'buildScriptContext' for everything else and then
overridden, because the builder keeps reference inputs sorted by out-ref and
these are addressed by position.
-}
context :: [TxInInfo] -> PD.Data -> ScriptContext
context refs redeemer =
  case buildScriptContext (withRewardingScript (dataToBuiltinData redeemer) verifierCredential 0) of
    ScriptContext txInfo r scriptInfo ->
      ScriptContext txInfo {txInfoReferenceInputs = refs} r scriptInfo

asMinting :: ScriptContext -> ScriptContext
asMinting (ScriptContext txInfo r _) =
  ScriptContext txInfo r (MintingScript (CurrencySymbol (toBuiltin (BS.replicate 28 0x90))))

asSpending :: ScriptContext -> ScriptContext
asSpending (ScriptContext txInfo r _) =
  ScriptContext txInfo r (SpendingScript (outRefN 0) Nothing)

verifierCredential :: Credential
verifierCredential = ScriptCredential (ScriptHash (toBuiltin verifierHash))

verifierHash :: BS.ByteString
verifierHash = BS.replicate 28 0x90

--------------------------------------------------------------------------------
-- The claim
--------------------------------------------------------------------------------

data Mode = MembershipMode | NonMembershipMode

-- | @chunked_inclusion_v1.ChunkedProofClaim@, written out from its declaration.
data Claim = Claim
  { cMode :: Mode
  , cRoot :: BS.ByteString
  , cKeyBytes :: BS.ByteString
  , cValueHash :: BS.ByteString
  , cChunkIndices :: [Integer]
  }

claimData :: Claim -> PD.Data
claimData c =
  PD.Constr
    0
    [ PD.Constr (case cMode c of MembershipMode -> 0; NonMembershipMode -> 1) []
    , PD.B (cRoot c)
    , PD.B (cKeyBytes c)
    , PD.B (cValueHash c)
    , PD.List (map PD.I (cChunkIndices c))
    ]

membershipClaim :: BS.ByteString -> [Integer] -> Claim
membershipClaim root indices =
  Claim
    { cMode = MembershipMode
    , cRoot = root
    , cKeyBytes = targetKey
    , cValueHash = blake2b256 targetValue
    , cChunkIndices = indices
    }

absenceClaim :: Claim
absenceClaim =
  Claim
    { cMode = NonMembershipMode
    , cRoot = combine (suffix absentLeafPath 0) absentLeafValueHash
    , cKeyBytes = targetKey
    , cValueHash = absentValueHash
    , cChunkIndices = [0]
    }

targetKey :: BS.ByteString
targetKey = BS.replicate 32 0x31

targetValue :: BS.ByteString
targetValue = BS.pack [0xa1, 0xb2, 0xc3, 0xd4]

absentLeafValueHash :: BS.ByteString
absentLeafValueHash = BS.replicate 32 0x66

{- | @chunked_inclusion_v1.absent_value_hash@ — the digest a non-membership claim
carries in the slot the terminal does not use. Written out rather than imported,
so the claim's encoding is pinned by this module too.
-}
absentValueHash :: BS.ByteString
absentValueHash = BS.replicate 32 0x00

--------------------------------------------------------------------------------
-- The proof ladder
--------------------------------------------------------------------------------

{- | A deterministic ladder of all-@Branch@ steps — one forced branch level
each, the worst admissible shape per level. Seeds count /down/, exactly as the
Aiken fixture's @list.push@ recursion produces them.
-}
adversarialBranchSteps :: Int -> [ProofStepRef]
adversarialBranchSteps count = [BranchStep 0 (branchNeighbors seed) | seed <- [count - 1, count - 2 .. 0]]

branchNeighbors :: Int -> BS.ByteString
branchNeighbors seed = a <> b <> c <> d
  where
    a = blake2b256 (cborUInt seed)
    b = blake2b256 a
    c = blake2b256 b
    d = blake2b256 c

deepProof :: [ProofStepRef]
deepProof = adversarialBranchSteps 22

deepRoot :: BS.ByteString
deepRoot = membershipRootOf targetKey targetValue deepProof

maximumProof :: [ProofStepRef]
maximumProof = adversarialBranchSteps 64

maximumRoot :: BS.ByteString
maximumRoot = membershipRootOf targetKey targetValue maximumProof

absentLeafPath :: BS.ByteString
absentLeafPath = blake2b256 (BS.pack [0xa1, 0x5e, 0x17])

absentLeafStep :: ProofStepRef
absentLeafStep = LeafStep 0 absentLeafPath absentLeafValueHash

--------------------------------------------------------------------------------
-- The MPF fold, recomputed here
--------------------------------------------------------------------------------

-- | The two step shapes these fixtures use, with their wire tags.
data ProofStepRef
  = BranchStep Integer BS.ByteString
  | LeafStep Integer BS.ByteString BS.ByteString

stepData :: ProofStepRef -> PD.Data
stepData (BranchStep skip neighbors) = PD.Constr 0 [PD.I skip, PD.B neighbors]
stepData (LeafStep skip key value) = PD.Constr 2 [PD.I skip, PD.B key, PD.B value]

{- | The root an all-@Branch@ membership proof of @(key, value)@ proves.

Independent of the verifier under test: it mirrors the MPF fold directly rather
than calling the walk.
-}
membershipRootOf :: BS.ByteString -> BS.ByteString -> [ProofStepRef] -> BS.ByteString
membershipRootOf key value steps = go (blake2b256 key) (blake2b256 value) 0 steps
  where
    go path valueHash cursor [] = combine (suffix path cursor) valueHash
    go path valueHash cursor (BranchStep skip neighbors : rest) =
      let nextCursor = cursor + 1 + fromIntegral skip
          childRoot = go path valueHash nextCursor rest
       in combine
            (nibbles path cursor (nextCursor - 1))
            ( merkle16
                (nibble path (nextCursor - 1))
                childRoot
                (BS.take 32 neighbors)
                (BS.take 32 (BS.drop 32 neighbors))
                (BS.take 32 (BS.drop 64 neighbors))
                (BS.take 32 (BS.drop 96 neighbors))
            )
    go _ _ _ (LeafStep {} : _) = nullHash

merkle16 ::
  Int -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
merkle16 branch root n8 n4 n2 n1
  | branch <= 7 = combine (merkle8 branch root n4 n2 n1) n8
  | otherwise = combine n8 (merkle8 (branch - 8) root n4 n2 n1)

merkle8 :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
merkle8 branch root n4 n2 n1
  | branch <= 3 = combine (merkle4 branch root n2 n1) n4
  | otherwise = combine n4 (merkle4 (branch - 4) root n2 n1)

merkle4 :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
merkle4 branch root n2 n1
  | branch <= 1 = combine (merkle2 branch root n1) n2
  | otherwise = combine n2 (merkle2 (branch - 2) root n1)

merkle2 :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
merkle2 branch root n1
  | branch == 0 = combine root n1
  | otherwise = combine n1 root

nullHash :: BS.ByteString
nullHash = BS.replicate 32 0x00

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

-- | The nibbles of @path@ in @[start, end)@, one byte each.
nibbles :: BS.ByteString -> Int -> Int -> BS.ByteString
nibbles path start end = BS.pack [fromIntegral (nibble path i) | i <- [start .. end - 1]]

-- | The MPF suffix marker plus the remaining path.
suffix :: BS.ByteString -> Int -> BS.ByteString
suffix path cursor
  | even cursor = BS.pack [0xff] <> BS.drop (cursor `div` 2) path
  | otherwise =
      BS.pack [0x00, fromIntegral (nibble path cursor)]
        <> BS.drop ((cursor + 1) `div` 2) path

-- | Canonical CBOR for a small non-negative integer, which is what the seeds are.
cborUInt :: Int -> BS.ByteString
cborUInt n
  | n <= 23 = BS.pack [fromIntegral n]
  | n <= 0xff = BS.pack [24, fromIntegral n]
  | otherwise = error "cborUInt: out of fixture range"

--------------------------------------------------------------------------------
-- Publication UTxOs
--------------------------------------------------------------------------------

{- | §545 publication: each chunk is @maximum_chunk_proof_step_count@ steps of
the proof, in order, as one inline @ProofChunkDatum@.
-}
chunkStepBound :: Int
chunkStepBound = 16

deepChunks :: [TxInInfo]
deepChunks = [chunkRefInput (fromIntegral i) steps | (i, steps) <- zip [0 :: Int ..] (chunksOf deepProof)]

maximumChunks :: [TxInInfo]
maximumChunks =
  [ chunkRefInput (fromIntegral i) steps
  | (i, steps) <- zip [0 :: Int ..] (chunksOf maximumProof)
  ]

chunksOf :: [a] -> [[a]]
chunksOf [] = []
chunksOf xs = take chunkStepBound xs : chunksOf (drop chunkStepBound xs)

chunkRefInput :: Integer -> [ProofStepRef] -> TxInInfo
chunkRefInput ix steps =
  refInputWith ix (OutputDatum (Datum (dataToBuiltinData (PD.Constr 0 [PD.List (map stepData steps)]))))

noDatumRefInput :: Integer -> TxInInfo
noDatumRefInput ix = refInputWith ix NoOutputDatum

-- | An inline datum that is not a chunk at all.
notAChunkRefInput :: Integer -> TxInInfo
notAChunkRefInput ix = refInputWith ix (OutputDatum (Datum (dataToBuiltinData (PD.I 7))))

refInputWith :: Integer -> OutputDatum -> TxInInfo
refInputWith ix datum =
  TxInInfo
    (outRefN ix)
    (TxOut (scriptHashAddress (ScriptHash (toBuiltin chunkHolderHash))) mempty datum Nothing)

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId (toBuiltin (BS.replicate 32 0x11)))

chunkHolderHash :: BS.ByteString
chunkHolderHash = BS.replicate 28 0x90

-- | How many steps a publication UTxO's datum holds.
chunkStepCount :: TxInInfo -> Int
chunkStepCount (TxInInfo _ (TxOut _ _ (OutputDatum (Datum d)) _)) =
  case Builtins.builtinDataToData d of
    PD.Constr 0 [PD.List steps] -> length steps
    _ -> error "chunkStepCount: not a chunk datum"
chunkStepCount _ = error "chunkStepCount: no inline datum"
