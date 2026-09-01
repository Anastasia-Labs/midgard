{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.CountedMembership
Description : Tests for the counted-root scheme —
              @lib/midgard/transition-trace.ak@ and
              @settlement.valid_counted_membership@.

A Midgard root commits to a @(domain, phas_root, count)@ triple rather than to a
bare Merkle root. The point of the count is to pin the tree's /size/, so the
tests that matter are the ones that try to substitute a different tree: a
mismatched count, a mismatched domain, an empty root paired with a positive
count, and the reverse.

Membership itself is delegated to the @phas@ staking validator, so the last
group checks the delegation: the redeemer of that withdrawal must carry exactly
the root, key, value and proof being claimed.
-}
module Testing.CountedMembership (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusLedgerApi.V3 (
  Credential (..),
  Redeemer (..),
  ScriptHash (..),
  ScriptPurpose (Minting, Rewarding),
 )
import PlutusTx.Builtins (blake2b_256, dataToBuiltinData, fromBuiltin, serialiseData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Settlement (pvalidCountedMembership)
import Midgard.TransitionTrace (PRootDomain, PRootMembershipProof)
import Testing.Eval (passertEval, pfails)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Counted Membership Tests"
    [ testGroup
        "validCountedMembership"
        [ testCase "accepts a proof whose counted root reconstructs" $
            passertEval $ run defaults
        , -- The count is the whole point of the scheme: the same entries in a
          -- tree of a different size must not verify.
          testCase "rejects a count the committed root does not encode" $
            passertEval $ pnot #$ run defaults {pCount = 4}
        , testCase "rejects a domain the committed root does not encode" $
            passertEval $ pnot #$ run defaults {pDomain = 5}
        , testCase "rejects a root that is not the counted commitment" $
            passertEval $ pnot #$ run defaults {pExpectedRoot = Just (hex32 0xbb)}
        , testCase "rejects a zero count" $
            passertEval $ pnot #$ run defaults {pCount = 0}
        , testCase "rejects a negative count" $
            passertEval $ pnot #$ run defaults {pCount = -1}
        , -- A positive count against the empty tree is inconsistent, and so is
          -- an empty count against a populated one.
          testCase "rejects the empty MPF root paired with a positive count" $
            passertEval $ pnot #$ run defaults {pPhasRoot = Just emptyMerkleRoot}
        , testCase "rejects a key the witness does not name" $
            passertEval $ pnot #$ run defaults {pWitnessKey = Just "other"}
        , testCase "rejects a value the witness does not name" $
            passertEval $ pnot #$ run defaults {pWitnessValue = Just "other"}
        ]
    , testGroup
        "phas delegation"
        [ testCase "rejects a phas redeemer carrying a different root" $
            pfails $ run defaults {pRedeemerRoot = Just (hex32 0xcc)}
        , testCase "rejects a phas redeemer carrying a different key" $
            pfails $ run defaults {pRedeemerKey = Just "wrong"}
        , testCase "rejects a phas redeemer carrying a different value" $
            pfails $ run defaults {pRedeemerValue = Just "wrong"}
        , testCase "rejects a phas redeemer carrying a different proof" $
            pfails $ run defaults {pRedeemerProof = Just (PD.I 99)}
        , testCase "rejects a transaction with no phas withdrawal at all" $
            pfails $ run defaults {pPhasWithdrawal = False}
        , -- The merkelized-validator pattern passes its arguments through the
          -- withdrawal redeemer, so two withdrawals make them ambiguous.
          testCase "rejects two withdrawals by the phas validator" $
            pfails $ run defaults {pDuplicatePhas = True}
        ]
    ]

--------------------------------------------------------------------------------
-- Constants mirroring env/default.ak
--------------------------------------------------------------------------------

emptyMerkleRoot :: BS.ByteString
emptyMerkleRoot =
  unhex "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"

phasValidatorHash :: BS.ByteString
phasValidatorHash = unhex "1fc59ff54da02f2535d64b40b647a8826c8b3d914d7ba5257f5b2721"

-- | @transition_trace.counted_root_tag@ — "MidgardRootCountV1".
countedRootTag :: BS.ByteString
countedRootTag = "MidgardRootCountV1"

unhex :: String -> BS.ByteString
unhex = either (error "bad hex") id . Base16.decode . BS8.pack

hex32 :: Int -> BS.ByteString
hex32 b = BS.replicate 32 (fromIntegral b)

--------------------------------------------------------------------------------
-- The commitment, recomputed independently of the validator
--------------------------------------------------------------------------------

{- | @transition_trace.commit_counted_root@, in Haskell.

Rebuilt here rather than reused from the Plutarch side so the test disagrees
with the validator if either changes. Serialisation of the domain and the count
goes through @serialiseData@, matching Aiken's @cbor.serialise@.
-}
commitCountedRoot :: Integer -> BS.ByteString -> Integer -> BS.ByteString
commitCountedRoot domainTag phasRoot count
  | count == 0 && phasRoot == emptyMerkleRoot = emptyMerkleRoot
  | otherwise =
      fromBuiltin $
        blake2b_256
          ( toBuiltin countedRootTag
              <> serialiseData (dataToBuiltinData (PD.Constr domainTag []))
              <> toBuiltin phasRoot
              <> serialiseData (dataToBuiltinData (PD.I count))
          )

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

-- | The knobs the negative cases turn; 'defaults' verifies.
data Params = Params
  { pDomain :: Integer
  , pCount :: Integer
  , pPhasRoot :: Maybe BS.ByteString
  , pExpectedRoot :: Maybe BS.ByteString
  , pWitnessKey :: Maybe BS.ByteString
  , pWitnessValue :: Maybe BS.ByteString
  , pRedeemerRoot :: Maybe BS.ByteString
  , pRedeemerKey :: Maybe BS.ByteString
  , pRedeemerValue :: Maybe BS.ByteString
  , pRedeemerProof :: Maybe PD.Data
  , pPhasWithdrawal :: Bool
  , pDuplicatePhas :: Bool
  }

defaults :: Params
defaults =
  Params
    { pDomain = 3 -- DepositsRootDomain
    , pCount = 7
    , pPhasRoot = Nothing
    , pExpectedRoot = Nothing
    , pWitnessKey = Nothing
    , pWitnessValue = Nothing
    , pRedeemerRoot = Nothing
    , pRedeemerKey = Nothing
    , pRedeemerValue = Nothing
    , pRedeemerProof = Nothing
    , pPhasWithdrawal = True
    , pDuplicatePhas = False
    }

-- | The entry being proved: a deposit id and its info, as raw @Data@.
entryKey, entryValue :: PD.Data
entryKey = PD.B "deposit-id"
entryValue = PD.B "deposit-info"

-- | The MPF proof; opaque to everything under test.
proofData :: PD.Data
proofData = PD.List [PD.B "step-a", PD.B "step-b"]

defaultPhasRoot :: BS.ByteString
defaultPhasRoot = hex32 0xaa

-- | The domain and count the committed root actually encodes.
referenceDomain, referenceCount :: Integer
referenceDomain = 3 -- DepositsRootDomain
referenceCount = 7

--------------------------------------------------------------------------------
-- Applying the term
--------------------------------------------------------------------------------

run :: forall s. Params -> Term s PBool
run p =
  pvalidCountedMembership
    (pdata (punsafeCoerceDomain (PD.Constr (pDomain p) [])))
    (pconstant expectedRoot)
    (punsafeCoerceProof witnessData)
    (pconstant @PData entryKey)
    (pconstant @PData entryValue)
    (pconstant redeemers)
  where
    phasRoot = maybe defaultPhasRoot id (pPhasRoot p)
    {- The committed root is built from the *reference* domain and count, not
    from the tampered ones. That is what makes the mismatch cases bite: the
    settlement's root is fixed by whoever wrote the datum, and the prover then
    supplies a witness claiming some other domain or size for it. -}
    expectedRoot =
      maybe
        (commitCountedRoot referenceDomain phasRoot referenceCount)
        id
        (pExpectedRoot p)

    serialisedKey = serialisedOf entryKey
    serialisedValue = serialisedOf entryValue

    witnessKey = maybe serialisedKey id (pWitnessKey p)
    witnessValue = maybe serialisedValue id (pWitnessValue p)

    -- @RootMembershipProof { domain, root, phas_root, count, key, value, proof }@
    witnessData =
      PD.Constr
        0
        [ PD.Constr (pDomain p) []
        , PD.B expectedRoot
        , PD.B phasRoot
        , PD.I (pCount p)
        , PD.B witnessKey
        , PD.B witnessValue
        , proofData
        ]

    -- The phas validator's withdrawal redeemer: [root, key, value, proof, ..].
    phasRedeemer =
      dataToBuiltinData $
        PD.List
          [ PD.B (maybe phasRoot id (pRedeemerRoot p))
          , PD.B (maybe serialisedKey id (pRedeemerKey p))
          , PD.B (maybe serialisedValue id (pRedeemerValue p))
          , maybe proofData id (pRedeemerProof p)
          ]

    phasEntry =
      ( Rewarding (ScriptCredential (ScriptHash (toBuiltin phasValidatorHash)))
      , Redeemer phasRedeemer
      )
    unrelatedEntry =
      (Minting (CurrencySymbol (toBuiltin (BS.replicate 28 0x09))), Redeemer (dataToBuiltinData (PD.I 0)))

    redeemers =
      [unrelatedEntry]
        <> [phasEntry | pPhasWithdrawal p]
        <> [phasEntry | pDuplicatePhas p]

-- | @cbor.serialise@ of a @Data@ value, computed off-chain.
serialisedOf :: PD.Data -> BS.ByteString
serialisedOf = fromBuiltin . serialiseData . dataToBuiltinData

punsafeCoerceDomain :: forall s. PD.Data -> Term s PRootDomain
punsafeCoerceDomain d =
  pfromData (punsafeCoerce (pconstant @PData d) :: Term s (PAsData PRootDomain))

punsafeCoerceProof :: forall s. PD.Data -> Term s PRootMembershipProof
punsafeCoerceProof d =
  pfromData (punsafeCoerce (pconstant @PData d) :: Term s (PAsData PRootMembershipProof))
