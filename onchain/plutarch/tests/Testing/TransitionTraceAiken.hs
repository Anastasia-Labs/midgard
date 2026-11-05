{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.TransitionTraceAiken
Description : Exact parity vectors from @lib/midgard/transition-trace.test.ak@.

These fixtures are shared with the TypeScript implementation and intentionally
remain literal.  They pin the transition-step wire ABI and prove that the
Plutarch MPF compatibility layer accepts the same raw and typed witnesses as
the Aiken implementation.
-}
module Testing.TransitionTraceAiken (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude
import PlutusCore.Data qualified as PD
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.Env qualified as Env
import Midgard.LedgerState (
  PEventKey (..),
  PTransitionPhase (PL2Transaction),
  PTransitionStep (..),
  ptransitionStepV1IsValid,
 )
import Midgard.MpfProof.Types (PProof (..), PProofStep (..))
import Midgard.TransitionTrace (
  PRootDomain (PDepositsRootDomain),
  PRootMembershipProof (..),
  pcommitCountedRoot,
  pverifyRootMembershipRaw,
  pverifyRootMembershipWithBytes,
  pverifyRootNonMembershipRaw,
 )
import Testing.Eval (passertEval)

tests :: TestTree
tests =
  testGroup
    "Transition Trace Aiken Parity"
    [ testCase "l06_transition_step_v1_aiken_cbor_and_hash_vector_is_exact" $
        passertEval transitionStepVectorIsExact
    , testCase "l06_transition_step_v1_rejects_adjacent_schema_versions" $
        passertEval transitionStepRejectsAdjacentVersions
    , testCase "accepts_typescript_raw_membership_fixture" $
        passertEval rawMembershipFixture
    , testCase "accepts_typescript_raw_non_membership_fixture" $
        passertEval rawNonMembershipFixture
    , testCase "accepts_empty_midgard_root_non_membership_fixture" $
        passertEval emptyRootNonMembershipFixture
    , testCase "accepts_typed_membership_witness_fixture" $
        passertEval typedMembershipFixture
    ]

transitionStepVectorIsExact :: forall s. Term s PBool
transitionStepVectorIsExact =
  plet (sampleTransitionStep 1) $ \step ->
    plet (pserialiseData # pforgetData (pdata step)) $ \encoded ->
      pand'List
        [ encoded #== pconstant transitionStepCbor
        , pblake2b_256 # encoded #== pconstant transitionStepHash
        , ptransitionStepV1IsValid # step
        ]

transitionStepRejectsAdjacentVersions :: forall s. Term s PBool
transitionStepRejectsAdjacentVersions =
  pnot # (ptransitionStepV1IsValid # sampleTransitionStep 0)
    #&& pnot # (ptransitionStepV1IsValid # sampleTransitionStep 2)

rawMembershipFixture :: forall s. Term s PBool
rawMembershipFixture =
  pverifyRootMembershipRaw
    (pconstant singleRawRoot)
    1
    (pconstant "\x01")
    (pconstant "\xaa")
    emptyProof

rawNonMembershipFixture :: forall s. Term s PBool
rawNonMembershipFixture =
  pverifyRootNonMembershipRaw
    (pconstant singleRawRoot)
    1
    (pconstant "\x02")
    rawNonMembershipProof

emptyRootNonMembershipFixture :: forall s. Term s PBool
emptyRootNonMembershipFixture =
  pverifyRootNonMembershipRaw
    Env.pemptyMerkleTreeRoot
    0
    (pconstant "\x01")
    emptyProof

typedMembershipFixture :: forall s. Term s PBool
typedMembershipFixture =
  let domain = pdata (pcon PDepositsRootDomain)
   in plet (pcommitCountedRoot domain (pconstant singleTypedRoot) 1) $ \countedRoot ->
        plet
          ( pcon $
              PRootMembershipProof
                { prootMembership'domain = domain
                , prootMembership'root = pdata countedRoot
                , prootMembership'phasRoot = pdata (pconstant singleTypedRoot)
                , prootMembership'count = pdata 1
                , prootMembership'key = pconstant @PData (PD.I 0)
                , prootMembership'value = pconstant @PData (PD.B "\xaa")
                , prootMembership'proof = pdata emptyProof
                }
          )
          $ \witness ->
            pverifyRootMembershipWithBytes
              witness
              domain
              countedRoot
              1
              (pserialiseData # pconstant @PData (PD.I 0))
              (pserialiseData # pconstant @PData (PD.B "\xaa"))

sampleTransitionStep :: forall s. Integer -> Term s PTransitionStep
sampleTransitionStep schemaVersion =
  pcon $
    PTransitionStep
      { ptransitionStep'schemaVersion = pdata (pconstant schemaVersion)
      , ptransitionStep'stepIndex = pdata 2
      , ptransitionStep'eventKey =
          pdata . pcon $
            PL2TransactionEventKey
              {pl2TxEventKey'txId = pdata (pconstant $ BS.replicate 32 0x33)}
      , ptransitionStep'phase = pdata (pcon PL2Transaction)
      , ptransitionStep'preUtxosRoot = pdata (pconstant $ BS.replicate 32 0x44)
      , ptransitionStep'postUtxosRoot = pdata (pconstant $ BS.replicate 32 0x55)
      }

emptyProof :: forall s. Term s PProof
emptyProof = pcon (PProof pnil)

rawNonMembershipProof :: forall s. Term s PProof
rawNonMembershipProof =
  pcon . PProof $
    pcons
      # ( pdata . pcon $
            PLeaf
              { pproofStep'skip = pdata 0
              , pproofStep'key = pdata (pconstant rawNonMembershipLeafKey)
              , pproofStep'value = pdata (pconstant rawNonMembershipLeafValue)
              }
        )
      # pnil

singleRawRoot :: BS.ByteString
singleRawRoot = hex "8e6a655e86132c76e2ee2dfba167949b82854be7a7657567eaf4cb86daf53588"

singleTypedRoot :: BS.ByteString
singleTypedRoot = hex "0fe4e134a5df52ba919e462ade1ba22090792b63f1c99a747deadea952abf1d4"

rawNonMembershipLeafKey :: BS.ByteString
rawNonMembershipLeafKey = hex "ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25"

rawNonMembershipLeafValue :: BS.ByteString
rawNonMembershipLeafValue = hex "55951e629cad560ea5f8be280c35d8788ee84324b842fee1b41c546efb62d2d5"

transitionStepCbor :: BS.ByteString
transitionStepCbor =
  hex
    "d8799f0102d87b9f58203333333333333333333333333333333333333333333333333333333333333333ffd87b805820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555ff"

transitionStepHash :: BS.ByteString
transitionStepHash = hex "c7931b31b050e3d59769ee6df1556585c608f1af0c74decc08026ecafbc234c4"

hex :: BS.ByteString -> BS.ByteString
hex = Base16.decodeLenient
