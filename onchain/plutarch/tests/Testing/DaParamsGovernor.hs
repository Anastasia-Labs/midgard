{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.DaParamsGovernor
Description : Tests for the Plutarch port of @validators/da-params-governor.ak@.

The governor's whole substance is one invariant, so most of these tests drive it
directly. The Aiken source names its own cases @da_params_governor_*@ and those
names are carried over where they correspond.

Two properties are worth more than the rest, and both get tested as properties
rather than as single points: no threshold below the governed floor is ever
admitted, and no owner set can be drained to one.
-}
module Testing.DaParamsGovernor (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import Test.Tasty
import Test.Tasty.HUnit

import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import Plutarch.LedgerApi.V3 (PPubKeyHash)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.DaAttestation (PDaParamsDatum)
import Midgard.Validators.DaParamsGovernor (
  pgovernedThresholdFloor,
  pownerQuorumMet,
  psortedUniqueLenAtMost,
  psortedUniquePackedLenAtMost,
  pvalidDatum,
 )
import Testing.Eval (passertEval, pfails)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "DA Params Governor Tests"
    [ floorTests
    , packedTests
    , listTests
    , quorumTests
    , datumTests
    ]

--------------------------------------------------------------------------------
-- governed_threshold_floor
--------------------------------------------------------------------------------

{- | @max(2, ceil(2n/3))@.

The clamp and the two-thirds term defend different attacks, so both ends are
pinned: small sets by the clamp, large sets by the ratio.
-}
floorTests :: TestTree
floorTests =
  testGroup
    "governedThresholdFloor"
    [ testCase "clamps small sets at two" $
        holds $ pall # plam (\n -> (pgovernedThresholdFloor # n) #== 2) # intsT [0, 1, 2, 3]
    , -- ceil(2n/3): 4->3, 5->4, 6->4, 9->6, 16->11
      testCase "is the two-thirds ceiling above the clamp" $
        holds $
          pand'ListT
            [ (pgovernedThresholdFloor # 4) #== 3
            , (pgovernedThresholdFloor # 5) #== 4
            , (pgovernedThresholdFloor # 6) #== 4
            , (pgovernedThresholdFloor # 9) #== 6
            , (pgovernedThresholdFloor # 16) #== 11
            ]
    , -- The floor must always be reachable, or the parameters would be
      -- unsatisfiable for that set size.
      testCase "never exceeds the set it bounds, for sets of two or more" $
        holds $
          pall
            # plam (\n -> (pgovernedThresholdFloor # n) #<= n)
            # intsT [2 .. 64]
    , -- da_params_governor_invariant_da_threshold_majority_floor: a floor at or
      -- below half would let a bare majority — or less — rotate the committee.
      testCase "always exceeds half the set" $
        holds $
          pall
            # plam (\n -> pdiv # n # 2 #< (pgovernedThresholdFloor # n))
            # intsT [2 .. 64]
    ]

--------------------------------------------------------------------------------
-- The two set walks
--------------------------------------------------------------------------------

packedTests :: TestTree
packedTests =
  testGroup
    "sortedUniquePackedLenAtMost"
    [ testCase "counts a sorted packed committee" $
        holds $ (psortedUniquePackedLenAtMost # pconstant (packed [1, 2, 3]) # 16 # 32) #== 3
    , testCase "accepts a single key" $
        holds $ (psortedUniquePackedLenAtMost # pconstant (packed [1]) # 16 # 32) #== 1
    , -- Strict ascent is the uniqueness proof: a repeated key would otherwise
      -- count twice towards the threshold.
      testCase "rejects a duplicated key" $
        fails $ psortedUniquePackedLenAtMost # pconstant (packed [1, 1]) # 16 # 32
    , testCase "rejects an unsorted committee" $
        fails $ psortedUniquePackedLenAtMost # pconstant (packed [2, 1]) # 16 # 32
    , testCase "rejects a committee above the cap" $
        fails $ psortedUniquePackedLenAtMost # pconstant (packed [1, 2, 3]) # 2 # 32
    , testCase "rejects a trailing partial key" $
        fails $
          psortedUniquePackedLenAtMost
            # pconstant (packed [1, 2] <> BS.replicate 8 0x03)
            # 16
            # 32
    , testCase "rejects an empty committee" $
        fails $ psortedUniquePackedLenAtMost # pconstant BS.empty # 16 # 32
    , testCase "rejects a non-positive cap" $
        fails $ psortedUniquePackedLenAtMost # pconstant (packed [1]) # 0 # 32
    ]

listTests :: TestTree
listTests =
  testGroup
    "sortedUniqueLenAtMost"
    [ testCase "counts a sorted owner list" $
        holds $ (psortedUniqueLenAtMost # ownersT [1, 2, 3] # 16 # 28) #== 3
    , testCase "rejects duplicated owners" $
        fails $ psortedUniqueLenAtMost # ownersT [1, 1] # 16 # 28
    , testCase "rejects unsorted owners" $
        fails $ psortedUniqueLenAtMost # ownersT [3, 2] # 16 # 28
    , testCase "rejects owners above the cap" $
        fails $ psortedUniqueLenAtMost # ownersT [1, 2, 3] # 2 # 28
    , testCase "rejects an empty owner list" $
        fails $ psortedUniqueLenAtMost # ownersT [] # 16 # 28
    , -- The width check is what stops a short or long key slipping through the
      -- ascent comparison.
      testCase "rejects an owner of the wrong width" $
        fails $ psortedUniqueLenAtMost # narrowOwnersT # 16 # 28
    ]

--------------------------------------------------------------------------------
-- owner_quorum_met
--------------------------------------------------------------------------------

quorumTests :: TestTree
quorumTests =
  testGroup
    "ownerQuorumMet"
    [ testCase "meets a threshold of two with two owner signatures" $
        holds $ pownerQuorumMet # ownersT [1, 2, 3] # ownersT [1, 2] # 2
    , testCase "falls short with one" $
        holds $ pnot #$ pownerQuorumMet # ownersT [1, 2, 3] # ownersT [1] # 2
    , -- Counting over owners, not signatories, is what makes an outsider's
      -- signature worthless however many they supply.
      testCase "ignores signatures from non-owners" $
        holds $ pnot #$ pownerQuorumMet # ownersT [1, 2, 3] # ownersT [7, 8, 9] # 2
    , testCase "counts only the owners among mixed signatories" $
        holds $ pownerQuorumMet # ownersT [1, 2, 3] # ownersT [1, 3, 9] # 2
    , testCase "meets a threshold equal to the whole set" $
        holds $ pownerQuorumMet # ownersT [1, 2, 3] # ownersT [1, 2, 3] # 3
    , testCase "falls short of a threshold above the set" $
        holds $ pnot #$ pownerQuorumMet # ownersT [1, 2] # ownersT [1, 2] # 3
    ]

--------------------------------------------------------------------------------
-- valid_datum
--------------------------------------------------------------------------------

datumTests :: TestTree
datumTests =
  testGroup
    "validDatum"
    [ testCase "da_params_governor_control_da_threshold_at_floor_accepted" $
        holds $ valid control
    , testCase "da_params_governor_control_update_threshold_at_floor_accepted" $
        holds $ valid control
    , -- Committee of 3, owners of 3: floor is 2 for both.
      testCase "da_params_governor_rejects_da_threshold_below_floor" $
        fails $ valid control {pDaThreshold = 1}
    , testCase "da_params_governor_rejects_update_threshold_below_floor" $
        fails $ valid control {pUpdateThreshold = 1}
    , testCase "rejects a da_threshold above the committee" $
        fails $ valid control {pDaThreshold = 4}
    , testCase "rejects an update_threshold above the owner set" $
        fails $ valid control {pUpdateThreshold = 4}
    , -- The hash is what lets an attestation pin its committee without carrying
      -- it, so a datum whose hash does not match its committee is meaningless.
      testCase "rejects a committee hash that is not the committee's" $
        fails $ valid control {pHashOverride = Just (BS.replicate 32 0x99)}
    , -- da_params_governor_invariant_owner_set_drain_protection
      testCase "da_params_governor_rejects_single_member_committee_datum" $
        fails $ valid control {pOwners = [1], pUpdateThreshold = 1}
    , testCase "rejects a single-key committee, whose floor exceeds it" $
        fails $ valid control {pCommittee = [1], pDaThreshold = 1}
    , testCase "rejects a duplicated committee key" $
        fails $ valid control {pCommittee = [1, 1, 2]}
    , testCase "rejects duplicated owners" $
        fails $ valid control {pOwners = [1, 1, 2]}
    , testCase "rejects a committee above the deployment cap" $
        fails $ validWith control 2 16
    , testCase "rejects a non-positive committee cap" $
        fails $ validWith control 0 16
    , -- The bitmap encoding cannot index past 256 signers.
      testCase "rejects a committee cap above the indexable ceiling" $
        fails $ validWith control 257 16
    , testCase "rejects a non-positive owner cap" $
        fails $ validWith control 16 0
    , -- A larger set raises the floor: 6 owners need 4.
      testCase "accepts a six-owner set at its two-thirds floor" $
        holds $ valid control {pOwners = [1 .. 6], pUpdateThreshold = 4}
    , testCase "rejects a six-owner set one below its floor" $
        fails $ valid control {pOwners = [1 .. 6], pUpdateThreshold = 3}
    ]

--------------------------------------------------------------------------------
-- Assertions
--------------------------------------------------------------------------------

holds :: (forall s. Term s PBool) -> Assertion
holds = passertEval

-- | Polymorphic: several of these terms evaluate to an integer, not a boolean.
fails :: forall a. (forall s. Term s a) -> Assertion
fails = pfails

pand'ListT :: forall s. [Term s PBool] -> Term s PBool
pand'ListT = foldr (#&&) (pconstant True)

intsT :: forall s. [Integer] -> Term s (PBuiltinList PInteger)
intsT ns = pconstant @(PBuiltinList PInteger) ns

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

-- | A committee key: 32 bytes with a distinguishing leading byte.
committeeKey :: Integer -> BS.ByteString
committeeKey n = BS.cons (fromIntegral n) (BS.replicate 31 0x00)

-- | An owner key hash: 28 bytes, same shape.
ownerHash :: Integer -> BS.ByteString
ownerHash n = BS.cons (fromIntegral n) (BS.replicate 27 0x00)

packed :: [Integer] -> BS.ByteString
packed = BS.concat . map committeeKey

ownersT :: forall s. [Integer] -> Term s (PBuiltinList (PAsData PPubKeyHash))
ownersT ns =
  punsafeCoerce (pasList # pconstant @PData (PD.List (map (PD.B . ownerHash) ns)))

data Params = Params
  { pCommittee :: [Integer]
  , pHashOverride :: Maybe BS.ByteString
  , pDaThreshold :: Integer
  , pOwners :: [Integer]
  , pUpdateThreshold :: Integer
  }

-- | Three committee keys and three owners, both thresholds exactly at the floor.
control :: Params
control =
  Params
    { pCommittee = [1, 2, 3]
    , pHashOverride = Nothing
    , pDaThreshold = 2
    , pOwners = [1, 2, 3]
    , pUpdateThreshold = 2
    }

valid :: forall s. Params -> Term s PBool
valid p = validWith p 16 16

validWith :: forall s. Params -> Integer -> Integer -> Term s PBool
validWith p maxCommittee maxOwners =
  pvalidDatum # datumTerm p # pconstant maxCommittee # pconstant maxOwners

{- | The datum, with @committee_signers_hash@ computed here in Haskell.

Recomputing the hash independently rather than reusing the term is what makes
the hash check meaningful: if the port hashed the wrong field, these fixtures
would disagree with it.
-}
datumTerm :: forall s. Params -> Term s PDaParamsDatum
datumTerm p = pfromData (punsafeCoerce (pconstant @PData dat))
  where
    committeeBytes = packed (pCommittee p)
    dat =
      PD.Constr
        0
        [ PD.B committeeBytes
        , PD.B (maybe (blake2b256 committeeBytes) id (pHashOverride p))
        , PD.I (pDaThreshold p)
        , PD.List (map (PD.B . ownerHash) (pOwners p))
        , PD.I (pUpdateThreshold p)
        ]

-- | Blake2b-256, computed in Haskell rather than taken from the term.
blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

-- | An owner list whose single key is 27 bytes rather than 28.
narrowOwnersT :: forall s. Term s (PBuiltinList (PAsData PPubKeyHash))
narrowOwnersT =
  punsafeCoerce (pasList # pconstant @PData (PD.List [PD.B (BS.replicate 27 0x01)]))
