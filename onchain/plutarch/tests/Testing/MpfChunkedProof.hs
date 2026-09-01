{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.MpfChunkedProof
Description : Behavioural tests for the Plutarch ports of
              @lib/midgard/mpf-chunked-proof-v1.ak@ and
              @lib/midgard/fraud-proofs/chunked-inclusion-v1.ak@.

Published proof chunks, and the two ways a fault-proof step consumes them.

The properties worth naming.

**Publication is permissionless because content is all that is trusted.** A
chunk UTxO carries no token and authorises nothing, so the tests build reference
inputs at an arbitrary address and the verifier still has to accept or reject on
the steps alone.

**Order is the redeemer's, not the chunks'.** The same two chunks named in the
opposite order must concatenate to a different proof, or a prover could reorder
a proof after publication.

**Every bound is applied in one place.** Duplicate indices, out-of-range
indices, more than eight chunks, an empty or oversized chunk datum, and a
concatenation over the 64-step ceiling each have their own case, because
`published_proof` is the only thing standing between a published proof and the
walk.

**A delegated claim is checked whole.** The step requires the merkelized
verifier to have run on *exactly* its claim, so changing any one of the five
fields — mode, root, key, digest, chunk order — must break the delegation.

Note the fixtures use `Branch` and `Leaf` steps. `Fork` steps are avoided
deliberately: their wire encoding is where the port and Aiken disagree, and that
divergence is pinned in `Testing.MpfProof` rather than smuggled in here.
-}
module Testing.MpfChunkedProof (tests) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import PlutusLedgerApi.V1.Value (Value)
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  Data (..),
  Datum (..),
  OutputDatum (..),
  Redeemer (..),
  ScriptHash (..),
  ScriptPurpose (..),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusTx.Builtins (dataToBuiltinData, fromBuiltin, toBuiltin)
import PlutusTx.Builtins qualified as Builtins
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.V3 (PRedeemer, PScriptPurpose, PTxInInfo)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.ChunkedInclusion (
  PPublishedProofCarriage (..),
  pdelegatedChunkMembership,
  pdelegatedChunkNonMembership,
  ppublishedChunkMembership,
  ppublishedChunkMembershipByDigest,
  ppublishedChunkNonMembership,
 )
import Midgard.MpfChunkedProof (
  PFinalizeProofRedeemer (..),
  PProofChallengeDatum (..),
  PProofChunkDatum (..),
  PProofMode (..),
  pchallengeDatumIsWellFormed,
  pchunkDatumAt,
  pchunkDatumIsWellFormed,
  pchunkIndicesAreWellFormed,
  pconcatenatePublishedSteps,
  pverifyPublishedProof,
 )
import Midgard.MpfProof.Types (PProof)
import Midgard.TransitionTrace (PRootDomain (..))
import Testing.Eval (passertEval, pfails)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "MPF Chunked Proof Tests"
    [ invariantTests
    , reassemblyTests
    , verificationTests
    , chunkedInclusionTests
    ]

--------------------------------------------------------------------------------
-- Named invariants
--------------------------------------------------------------------------------

invariantTests :: TestTree
invariantTests =
  testGroup
    "named invariants"
    [ testCase "a well-formed challenge datum is accepted" $
        holds $
          pchallengeDatumIsWellFormed
            # challengeT NonMembershipMode 1 absenceRoot targetKey targetDigest
    , testCase "every hash field is held to its exact width" $
        holds $
          pall'
            [ pnot #$ pchallengeDatumIsWellFormed # challengeWith (\c -> c {cOwner = BS.replicate 27 0x01})
            , pnot #$ pchallengeDatumIsWellFormed # challengeWith (\c -> c {cHeaderHash = BS.replicate 32 0x01})
            , pnot #$ pchallengeDatumIsWellFormed # challengeWith (\c -> c {cTargetKey = BS.replicate 31 0x01})
            , pnot #$ pchallengeDatumIsWellFormed # challengeWith (\c -> c {cTargetDigest = BS.replicate 33 0x01})
            , pnot #$ pchallengeDatumIsWellFormed # challengeWith (\c -> c {cRoot = BS.replicate 28 0x01})
            ]
    , -- Nothing is a member of an empty trie, so a membership challenge has to
      -- name a non-empty one. A non-membership challenge need not.
      testCase "a membership challenge must name a non-empty trie" $
        holds $
          pall'
            [ pnot #$ pchallengeDatumIsWellFormed # challengeWith (\c -> c {cMode = MembershipMode, cLeafCount = 0})
            , pchallengeDatumIsWellFormed # challengeWith (\c -> c {cMode = MembershipMode, cLeafCount = 1})
            , pchallengeDatumIsWellFormed # challengeWith (\c -> c {cMode = NonMembershipMode, cLeafCount = 0})
            ]
    , testCase "a negative leaf count is rejected" $
        holds $ pnot #$ pchallengeDatumIsWellFormed # challengeWith (\c -> c {cLeafCount = -1})
    , testCase "a chunk carries between one and sixteen steps" $
        holds $
          pall'
            [ pnot #$ pchunkDatumIsWellFormed # chunkT []
            , pchunkDatumIsWellFormed # chunkT [leafData]
            , pchunkDatumIsWellFormed # chunkT (replicate 16 leafData)
            , pnot #$ pchunkDatumIsWellFormed # chunkT (replicate 17 leafData)
            ]
    , -- The byte bound is measured on the datum's own serialisation, so a
      -- chunk cannot smuggle size in through step shapes the count does not
      -- see.
      testCase "a chunk within the step bound can still exceed the byte bound" $
        holds $
          pall'
            [ pchunkDatumIsWellFormed # chunkT (replicate 16 branchData)
            , pnot #$ pchunkDatumIsWellFormed # chunkT (replicate 16 fatBranchData)
            ]
    , -- An empty order is the legitimate shape for a one-leaf trie.
      testCase "an empty chunk order is well formed" $
        holds $ pchunkIndicesAreWellFormed # indicesT [] # 3
    , testCase "the chunk order is bounded at eight" $
        holds $
          pall'
            [ pchunkIndicesAreWellFormed # indicesT [0 .. 7] # 8
            , pnot #$ pchunkIndicesAreWellFormed # indicesT [0 .. 8] # 9
            ]
    , testCase "every index must be in range" $
        holds $
          pall'
            [ pnot #$ pchunkIndicesAreWellFormed # indicesT [0, 3] # 3
            , pnot #$ pchunkIndicesAreWellFormed # indicesT [-1] # 3
            , pchunkIndicesAreWellFormed # indicesT [0, 2] # 3
            ]
    , -- Rejected by name rather than left to fail the walk.
      testCase "duplicate indices are rejected by name" $
        holds $ pnot #$ pchunkIndicesAreWellFormed # indicesT [0, 1, 0] # 3
    ]

--------------------------------------------------------------------------------
-- Reassembly
--------------------------------------------------------------------------------

reassemblyTests :: TestTree
reassemblyTests =
  testGroup
    "reassembly"
    [ testCase "a published chunk is found at its index" $
        holds $ pisJust (pchunkDatumAt # refInputsT twoChunkRefs # 1)
    , testCase "an index past the end or below zero yields nothing" $
        holds $
          pall'
            [ pisNothing (pchunkDatumAt # refInputsT twoChunkRefs # 2)
            , pisNothing (pchunkDatumAt # refInputsT twoChunkRefs # (-1))
            ]
    , testCase "a reference input without an inline datum yields nothing" $
        holds $ pisNothing (pchunkDatumAt # refInputsT [noDatumRef] # 0)
    , testCase "a malformed chunk datum yields nothing" $
        holds $ pisNothing (pchunkDatumAt # refInputsT [chunkRef 0 []] # 0)
    , testCase "an empty order concatenates to the empty proof" $
        holds $
          pmatch (pconcatenatePublishedSteps # refInputsT twoChunkRefs # indicesT []) $ \case
            PNothing -> pconstant False
            PJust proof -> pnull # pto proof
    , -- Order comes from the redeemer, so the two orders differ.
      testCase "the redeemer's order is the proof's order" $
        holds $
          pall'
            [ pstepsOf (pconcatenatePublishedSteps # refInputsT twoChunkRefs # indicesT [0, 1])
                #== pconstant [branchData, leafData]
            , pstepsOf (pconcatenatePublishedSteps # refInputsT twoChunkRefs # indicesT [1, 0])
                #== pconstant [leafData, branchData]
            ]
    , testCase "one missing chunk collapses the whole concatenation" $
        holds $
          pisNothing (pconcatenatePublishedSteps # refInputsT [noDatumRef] # indicesT [0])
    ]

--------------------------------------------------------------------------------
-- Verification
--------------------------------------------------------------------------------

verificationTests :: TestTree
verificationTests =
  testGroup
    "verification"
    [ testCase "a published absence proof verifies against its challenge" $
        holds $ verifyPublished absenceChallenge [0]
    , -- The zero-step proof is the shape for a one-leaf trie, and it still has
      -- to reconstruct the root.
      testCase "an empty published proof verifies against a matching root" $
        holds $ pverifyPublishedProof # refInputsT [] # emptyTrieChallenge # redeemerT []
    , testCase "a wrong terminal is rejected" $
        holds $ pnot #$ verifyPublished membershipAgainstAbsence [0]
    , testCase "duplicate and out-of-range indices are rejected" $
        holds $
          pall'
            [ pnot #$ verifyPublished absenceChallenge [0, 0]
            , pnot #$ verifyPublished absenceChallenge [5]
            ]
    , testCase "a malformed challenge datum is rejected before the walk" $
        holds $
          pnot
            #$ pverifyPublishedProof
            # refInputsT absenceRefs
            # challengeWith (\c -> c {cTargetKey = BS.replicate 31 0x01})
            # redeemerT [0]
    , testCase "a challenge naming another root is rejected" $
        holds $
          pnot
            #$ pverifyPublishedProof
            # refInputsT absenceRefs
            # challengeWith (\c -> c {cRoot = BS.replicate 32 0x09})
            # redeemerT [0]
    ]

--------------------------------------------------------------------------------
-- Chunked inclusion
--------------------------------------------------------------------------------

chunkedInclusionTests :: TestTree
chunkedInclusionTests =
  testGroup
    "chunked inclusion"
    [ testCase "a published absence opens against the authenticated root" $
        holds $
          ppublishedChunkNonMembership
            # refInputsT absenceRefs
            # carriageT [0]
            # pconstant absenceRoot
            # pconstant targetKey
    , testCase "a published absence does not open another root" $
        holds $
          pnot
            #$ ppublishedChunkNonMembership
            # refInputsT absenceRefs
            # carriageT [0]
            # pconstant (BS.replicate 32 0x09)
            # pconstant targetKey
    , -- The preimage form and the digest form must agree; only the digest
      -- reaches a delegated verifier.
      testCase "membership by preimage and by digest agree" $
        holds $
          pall'
            [ ppublishedChunkMembership
                # refInputsT []
                # carriageT []
                # pconstant singletonRoot
                # pconstant targetKey
                # pconstant targetValue
            , ppublishedChunkMembershipByDigest
                # refInputsT []
                # carriageT []
                # pconstant singletonRoot
                # pconstant targetKey
                # pconstant targetDigest
            ]
    , testCase "a carriage naming a missing chunk opens nothing" $
        holds $
          pall'
            [ pnot
                #$ ppublishedChunkNonMembership
                # refInputsT []
                # carriageT [0]
                # pconstant absenceRoot
                # pconstant targetKey
            , pnot
                #$ ppublishedChunkMembershipByDigest
                # refInputsT []
                # carriageT [0]
                # pconstant singletonRoot
                # pconstant targetKey
                # pconstant targetDigest
            ]
    , -- The delegated route: the step contributes the binding, the verifier the
      -- walk, and the claim is compared whole.
      testCase "a delegated membership claim must match in every field" $
        holds $
          pall'
            [ delegatedMembership (claim MembershipTerminal absenceRoot targetKey targetDigest [0])
            , pnot #$ delegatedMembership (claim NonMembershipTerminal absenceRoot targetKey targetDigest [0])
            , pnot #$ delegatedMembership (claim MembershipTerminal otherRoot targetKey targetDigest [0])
            , pnot #$ delegatedMembership (claim MembershipTerminal absenceRoot otherKey targetDigest [0])
            , pnot #$ delegatedMembership (claim MembershipTerminal absenceRoot targetKey otherDigest [0])
            , pnot #$ delegatedMembership (claim MembershipTerminal absenceRoot targetKey targetDigest [1])
            ]
    , -- An absence claim carries the fixed digest, because it has no value.
      testCase "a delegated absence claim carries the fixed absent digest" $
        holds $
          pall'
            [ delegatedNonMembership (claim NonMembershipTerminal absenceRoot targetKey absentValueHash [0])
            , pnot #$ delegatedNonMembership (claim NonMembershipTerminal absenceRoot targetKey targetDigest [0])
            , pnot #$ delegatedNonMembership (claim MembershipTerminal absenceRoot targetKey absentValueHash [0])
            ]
    , testCase "a delegated claim under another script hash is not found" $
        pfails $
          pdelegatedChunkNonMembership
            # pconstant otherVerifierHash
            # redeemersT [claim NonMembershipTerminal absenceRoot targetKey absentValueHash [0]]
            # carriageT [0]
            # pconstant absenceRoot
            # pconstant targetKey
    ]

--------------------------------------------------------------------------------
-- Applying the verifiers
--------------------------------------------------------------------------------

verifyPublished :: forall s. Term s PProofChallengeDatum -> [Integer] -> Term s PBool
verifyPublished challenge indices =
  pverifyPublishedProof # refInputsT absenceRefs # challenge # redeemerT indices

delegatedMembership :: forall s. Claim -> Term s PBool
delegatedMembership c =
  pdelegatedChunkMembership
    # pconstant verifierHash
    # redeemersT [c]
    # carriageT [0]
    # pconstant absenceRoot
    # pconstant targetKey
    # pconstant targetValue

delegatedNonMembership :: forall s. Claim -> Term s PBool
delegatedNonMembership c =
  pdelegatedChunkNonMembership
    # pconstant verifierHash
    # redeemersT [c]
    # carriageT [0]
    # pconstant absenceRoot
    # pconstant targetKey

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

targetKey, otherKey, targetValue :: BS.ByteString
targetKey = blake2b256 "target"
otherKey = blake2b256 "other"
targetValue = "\x04\x05\x06"

targetDigest, otherDigest :: BS.ByteString
targetDigest = blake2b256 targetValue
otherDigest = blake2b256 "\x09"

-- | The absent key's own path, which the absence proof's leaf sits on.
absentPath :: BS.ByteString
absentPath = blake2b256 otherKey

{- | The root of a one-leaf trie holding the /other/ key, under which the target
key is absent.
-}
absenceRoot :: BS.ByteString
absenceRoot = combine (suffix absentPath 0) otherDigest

-- | The root of a one-leaf trie holding the target key at its own value.
singletonRoot :: BS.ByteString
singletonRoot = combine (suffix (blake2b256 targetKey) 0) targetDigest

otherRoot :: BS.ByteString
otherRoot = BS.replicate 32 0x09

absentValueHash :: BS.ByteString
absentValueHash = BS.replicate 32 0x00

verifierHash, otherVerifierHash :: BS.ByteString
verifierHash = BS.replicate 28 0x90
otherVerifierHash = BS.replicate 28 0x91

--------------------------------------------------------------------------------
-- Proof steps, as Data
--------------------------------------------------------------------------------

-- | @Leaf { skip: 0, key: absentPath, value: otherDigest }@ — the absence witness.
leafData :: Data
leafData = Constr 2 [I 0, B absentPath, B otherDigest]

-- | A branch step, used where the step's content does not matter.
branchData :: Data
branchData = Constr 0 [I 0, B (BS.replicate 128 0x07)]

-- | The same shape with a payload wide enough to break the per-chunk byte bound.
fatBranchData :: Data
fatBranchData = Constr 0 [I 0, B (BS.replicate 256 0x07)]

--------------------------------------------------------------------------------
-- Reference inputs
--------------------------------------------------------------------------------

-- | A reference input at an arbitrary address carrying a chunk datum inline.
chunkRef :: Integer -> [Data] -> TxInInfo
chunkRef ix steps = refInputWith ix (OutputDatum (Datum (dataToBuiltinData (Constr 0 [List steps]))))

-- | A reference input with no datum at all.
noDatumRef :: TxInInfo
noDatumRef = refInputWith 0 NoOutputDatum

refInputWith :: Integer -> OutputDatum -> TxInInfo
refInputWith ix datum =
  TxInInfo
    (TxOutRef (TxId (toBuiltin (BS.replicate 32 0x11))) ix)
    (TxOut arbitraryAddress mempty datum Nothing)

arbitraryAddress :: Address
arbitraryAddress = Address (ScriptCredential (ScriptHash (toBuiltin (BS.replicate 28 0x77)))) Nothing

-- | Two chunks, one step each, so their order is observable.
twoChunkRefs :: [TxInInfo]
twoChunkRefs = [chunkRef 0 [branchData], chunkRef 1 [leafData]]

-- | One chunk holding the absence witness.
absenceRefs :: [TxInInfo]
absenceRefs = [chunkRef 0 [leafData]]

--------------------------------------------------------------------------------
-- Challenges
--------------------------------------------------------------------------------

data Mode = MembershipMode | NonMembershipMode

data Challenge = Challenge
  { cOwner :: BS.ByteString
  , cHeaderHash :: BS.ByteString
  , cTargetKey :: BS.ByteString
  , cTargetDigest :: BS.ByteString
  , cRoot :: BS.ByteString
  , cLeafCount :: Integer
  , cMode :: Mode
  }

defaultChallenge :: Challenge
defaultChallenge =
  Challenge
    { cOwner = BS.replicate 28 0x01
    , cHeaderHash = BS.replicate 28 0x02
    , cTargetKey = targetKey
    , cTargetDigest = targetDigest
    , cRoot = absenceRoot
    , cLeafCount = 1
    , cMode = NonMembershipMode
    }

challengeWith :: forall s. (Challenge -> Challenge) -> Term s PProofChallengeDatum
challengeWith modify = challengeOf (modify defaultChallenge)

challengeT ::
  forall s.
  Mode ->
  Integer ->
  BS.ByteString ->
  BS.ByteString ->
  BS.ByteString ->
  Term s PProofChallengeDatum
challengeT mode leafCount root k digest =
  challengeOf
    defaultChallenge
      { cMode = mode
      , cLeafCount = leafCount
      , cRoot = root
      , cTargetKey = k
      , cTargetDigest = digest
      }

challengeOf :: forall s. Challenge -> Term s PProofChallengeDatum
challengeOf c =
  pcon $
    PProofChallengeDatum
      { pchallenge'proofOwner = pdata (pconstant (cOwner c))
      , pchallenge'challengedHeaderHash = pdata (pconstant (cHeaderHash c))
      , pchallenge'challengedRootDomain = pdata (pcon PTransactionsV1RootDomain)
      , pchallenge'targetKey = pdata (pconstant (cTargetKey c))
      , pchallenge'targetValueHash = pdata (pconstant (cTargetDigest c))
      , pchallenge'expectedRoot = pdata (pconstant (cRoot c))
      , pchallenge'expectedLeafCount = pdata (pconstant (cLeafCount c))
      , pchallenge'mode =
          pdata $ case cMode c of
            MembershipMode -> pcon PMembership
            NonMembershipMode -> pcon PNonMembership
      }

absenceChallenge :: forall s. Term s PProofChallengeDatum
absenceChallenge = challengeOf defaultChallenge

-- | A membership challenge pointed at an absence proof — the wrong terminal.
membershipAgainstAbsence :: forall s. Term s PProofChallengeDatum
membershipAgainstAbsence = challengeWith (\c -> c {cMode = MembershipMode})

-- | A membership challenge over a one-leaf trie, provable with zero steps.
emptyTrieChallenge :: forall s. Term s PProofChallengeDatum
emptyTrieChallenge =
  challengeWith (\c -> c {cMode = MembershipMode, cRoot = singletonRoot, cLeafCount = 1})

--------------------------------------------------------------------------------
-- Claims
--------------------------------------------------------------------------------

data Terminal = MembershipTerminal | NonMembershipTerminal

data Claim = Claim Terminal BS.ByteString BS.ByteString BS.ByteString [Integer]

claim :: Terminal -> BS.ByteString -> BS.ByteString -> BS.ByteString -> [Integer] -> Claim
claim = Claim

-- | A claim as the withdraw redeemer the verifier script would have received.
claimData :: Claim -> Data
claimData (Claim terminal root k digest indices) =
  Constr
    0
    [ Constr (case terminal of MembershipTerminal -> 0; NonMembershipTerminal -> 1) []
    , B root
    , B k
    , B digest
    , List (map I indices)
    ]

redeemersT ::
  forall s.
  [Claim] ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)))
redeemersT claims =
  pconstant
    [ ( Rewarding (ScriptCredential (ScriptHash (toBuiltin verifierHash)))
      , Redeemer (dataToBuiltinData (claimData c))
      )
    | c <- claims
    ]

--------------------------------------------------------------------------------
-- Building the Plutarch values
--------------------------------------------------------------------------------

chunkT :: forall s. [Data] -> Term s PProofChunkDatum
chunkT steps =
  pcon $
    PProofChunkDatum
      { pchunk'proofSteps = punsafeCoerceData (List steps)
      }

-- | A @Data@ literal in a field slot, for shapes the typed constructors cannot make.
punsafeCoerceData :: forall s a. Data -> Term s (PAsData a)
punsafeCoerceData = punsafeCoerceTerm . pconstant

punsafeCoerceTerm :: forall s a. Term s PData -> Term s (PAsData a)
punsafeCoerceTerm = punsafeCoerce

indicesT :: forall s. [Integer] -> Term s (PBuiltinList (PAsData PInteger))
indicesT = foldr (\x acc -> pcons # pdata (pconstant x) # acc) pnil

redeemerT :: forall s. [Integer] -> Term s PFinalizeProofRedeemer
redeemerT indices =
  pcon $
    PFinalizeProofRedeemer
      { pfinalize'orderedChunkReferenceInputIndices = pdata (indicesT indices)
      }

carriageT :: forall s. [Integer] -> Term s PPublishedProofCarriage
carriageT indices =
  pcon $
    PPublishedProofCarriage
      { pcarriage'orderedChunkReferenceInputIndices = pdata (indicesT indices)
      }

refInputsT :: forall s. [TxInInfo] -> Term s (PBuiltinList (PAsData PTxInInfo))
refInputsT = pconstant

--------------------------------------------------------------------------------
-- MPF primitives, recomputed here
--------------------------------------------------------------------------------

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

combine :: BS.ByteString -> BS.ByteString -> BS.ByteString
combine left right = blake2b256 (left <> right)

nibble :: BS.ByteString -> Int -> Int
nibble path index
  | even index = fromIntegral (BS.index path (index `div` 2)) `shiftR` 4
  | otherwise = fromIntegral (BS.index path (index `div` 2)) .&. 0x0f

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

pisJust :: forall s a. Term s (PMaybe a) -> Term s PBool
pisJust m = pmatch m $ \case
  PJust _ -> pconstant True
  PNothing -> pconstant False

pisNothing :: forall s a. Term s (PMaybe a) -> Term s PBool
pisNothing m = pnot # pisJust m

-- | The steps of a reassembled proof, as @Data@, for comparing against fixtures.
pstepsOf :: forall s. Term s (PMaybe PProof) -> Term s (PBuiltinList PData)
pstepsOf m = pmatch m $ \case
  PNothing -> perror
  PJust proof -> pmap # plam pforgetData # pto proof
