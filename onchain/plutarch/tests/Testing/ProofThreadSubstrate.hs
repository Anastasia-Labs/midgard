{-# LANGUAGE OverloadedStrings #-}

module Testing.ProofThreadSubstrate (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Types
import Midgard.FraudProofs.ProofThreadSubstrate
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore.Data qualified as D
import Test.Tasty
import Test.Tasty.HUnit
import Testing.Eval (passertEval, pfails)
import Testing.FraudProofsFixture qualified as F

tests :: TestTree
tests =
  testGroup
    "Proof Thread Substrate"
    [ testCase "binds authenticated accepted carriage" $
        passertEval $
          pencodeVerdictSubject # (pbindAcceptedSubject # verified 0) #== pencodeVerdictSubject # subject 0 0 "" Nothing (F.txIdOf F.tx1)
    , testCase "refuses invalid scalar on accepted carriage" $ pfails $ pbindAcceptedSubject # verified 1
    , testCase "refuses wrong accepted version" $
        pfails $
          pbindAcceptedSubject
            # pmatch (verified 0) (\v -> pcon v {pverified'version = 2})
    , testCase "refuses wrong accepted tx id width" $
        pfails $
          pbindAcceptedSubject
            # pmatch (verified 0) (\v -> pcon v {pverified'txId = pconstant "short"})
    , testCase "binds forced acceptance under counted root and thread" $
        passertEval $
          pencodeVerdictSubject # bound accepted 0 #== pencodeVerdictSubject # subject 0 1 (F.serialise key) Nothing (F.txIdOf F.tx1)
    , testCase "binds forced rejection and exact reason" $
        passertEval $
          pforgetData (pdata $ pbindExactRejectionReason # bound rejected 1 # fromData reason) #== pconstant reason
    , testCase "refuses wrong direction on accepted forced leaf" $ pfails $ bound accepted 1
    , testCase "refuses wrong direction on rejected forced leaf" $ pfails $ bound rejected 0
    , testCase "refuses unknown direction" $ pfails $ bound accepted 2
    , testCase "refuses ForcedValid with invalid embedded scalar" $ pfails $ bound (claim 1 $ D.Constr 0 []) 0
    , testCase "refuses ForcedInvalid with valid embedded scalar" $ pfails $ bound (claim 0 $ D.Constr 1 [reason]) 1
    , testCase "refuses substituted header root" $
        pfails $
          bind (field 3 (D.B $ BS.replicate 32 0x55) $ fst accepted) (snd accepted) 0
    , testCase "refuses substituted forced key" $
        pfails $
          bind (fst accepted) (field 4 (D.Constr 0 [D.B $ BS.replicate 32 0x55, D.I 0]) $ snd accepted) 0
    , testCase "refuses substituted membership domain" $
        pfails $
          bind (fst accepted) (field 0 (D.Constr 2 []) $ snd accepted) 0
    , testCase "refuses substituted forced count" $
        pfails $
          bind (field 10 (D.I 2) $ fst accepted) (snd accepted) 0
    , testCase "refuses substituted thread header" $
        pfails $
          pbindForcedSubjectToThread
            # pconstant (asset $ fst accepted)
            # fromData (field 0 (D.B $ BS.replicate 32 0x55) $ fst accepted)
            # fromData (snd accepted)
            # 0
    , testCase "refuses source bytes with trailing garbage" $
        pfails $
          bind (fst accepted) (field 5 (field 1 (field 0 (D.B $ compact 0 <> "\x00") $ source 0) $ leaf 0 $ D.Constr 0 []) $ snd accepted) 0
    , testCase "refuses substituted tx id" $
        pfails $
          bind (fst accepted) (field 5 (field 0 (D.B $ BS.replicate 32 0x55) $ leaf 0 $ D.Constr 0 []) $ snd accepted) 0
    , testCase "binds every reason coordinate" $
        pfails $
          pbindExactRejectionReason # bound rejected 1 # fromData (D.Constr 18 [D.I 1, D.I 3])
    , testCase "binds the reason constructor" $
        pfails $
          pbindExactRejectionReason # bound rejected 1 # fromData (D.Constr 45 [D.I 2])
    , testCase "terminal accepts exactly both contradiction polarities" $
        passertEval $
          pterminalContradiction
            # bound accepted 0
            # pconstant True
            #&& pnot
            # (pterminalContradiction # bound accepted 0 # pconstant False)
            #&& pterminalContradiction
            # bound rejected 1
            # pconstant False
            #&& pnot
            # (pterminalContradiction # bound rejected 1 # pconstant True)
    , testCase "terminal refuses reasonless wrongful rejection" $
        pfails $
          pterminalContradiction # subject 1 1 "key" Nothing goldenId # pconstant False
    , testCase "reason lookup refuses ordinary acceptance" $
        pfails $
          prejectionReasonOf # subject 0 0 "" Nothing goldenId
    , testGroup
        "canonical carried subject mutations"
        [ testCase name $ passertEval $ pnot # (psubjectIsCanonical # fromData value)
        | (name, value) <-
            [ ("version", field 0 (D.I 2) canonical)
            , ("direction", field 1 (D.I 2) canonical)
            , ("source kind", field 2 (D.I 2) canonical)
            , ("tx id width", field 3 (D.B "short") canonical)
            , ("accepted key", field 4 (D.B "key") canonical)
            , ("accepted reason", field 5 (D.Constr 0 [reason]) canonical)
            , ("forced empty key", field 2 (D.I 1) canonical)
            ]
        ]
    , testGroup
        "target byte goldens"
        [ testCase "accepted" $
            golden
              (subject 0 0 "" Nothing goldenId)
              "860100005820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f234080"
        , testCase "forced accepted" $
            golden
              (subject 0 1 (F.serialise key) Nothing goldenId)
              "860100015820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f235827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ff80"
        , testCase "forced rejected with InputNotFound coordinates" $
            golden
              (subject 1 1 (F.serialise key) (Just reason) goldenId)
              "860101015820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f235827d8799f5820777777777777777777777777777777777777777777777777777777777777777700ff81d9050b9f0102ff"
        ]
    ]
  where
    golden :: (forall s. Term s PVerdictSubject) -> BS.ByteString -> Assertion
    golden value expected = passertEval $ pencodeVerdictSubject # value #== pconstant (hex expected)

fromData :: forall a s. (PIsData a) => D.Data -> Term s a
fromData = pfromData . punsafeCoerce . pconstant @PData
subject :: forall s. Integer -> Integer -> BS.ByteString -> Maybe D.Data -> BS.ByteString -> Term s PVerdictSubject
subject direction kind sourceKey rejection txId =
  fromData $
    D.Constr
      0
      [D.I 1, D.I direction, D.I kind, D.B txId, D.B sourceKey, maybe (D.Constr 1 []) (\r -> D.Constr 0 [r]) rejection]
canonical, reason, key :: D.Data
canonical = D.Constr 0 [D.I 1, D.I 0, D.I 0, D.B goldenId, D.B "", D.Constr 1 []]
reason = D.Constr 18 [D.I 1, D.I 2]
key = D.Constr 0 [D.B $ BS.replicate 32 0x77, D.I 0]
goldenId :: BS.ByteString
goldenId = hex "d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23"
hex :: BS.ByteString -> BS.ByteString
hex = either error id . Base16.decode
compact :: Integer -> BS.ByteString
compact = F.compactWithValidity F.tx1 (F.witnessSetHashOf F.tx1)
source :: Integer -> D.Data
source validity = D.Constr 0 [D.B $ compact validity, D.B $ F.witnessSetCborOf F.tx1, D.B $ F.fieldPreimageLengthsCborOf F.tx1]
leaf :: Integer -> D.Data -> D.Data
leaf validity verdict = D.Constr 0 [D.B $ F.txIdOf F.tx1, source validity, verdict]
verified :: forall s. Integer -> Term s PVerifiedMidgardNativeTxCompact
verified validity = pmatch
  ( pverifyNativeTxProofSourceV1
      # pconstant (F.txIdOf F.tx1)
      # pconstant (compact validity)
      # pconstant (F.witnessSetCborOf F.tx1)
      # pconstant (F.fieldPreimageLengthsCborOf F.tx1)
  )
  $ \(PPair tx _) -> tx
claim :: Integer -> D.Data -> (D.Data, D.Data)
claim validity verdict = (header, F.membershipProof 1 root rawRoot 1 key value)
  where
    value = leaf validity verdict
    rawRoot = F.singleEntryPhasRoot (F.serialise key) (F.serialise value)
    root = F.commitCountedRoot 1 rawRoot 1
    header =
      D.Constr 0 $
        [D.B "", D.B "", D.B "", D.B root]
          ++ replicate 5 (D.B "")
          ++ [D.I 0, D.I 1]
          ++ replicate 11 (D.I 0)
          ++ [D.B "", D.B "", D.I 1]
accepted, rejected :: (D.Data, D.Data)
accepted = claim 0 $ D.Constr 0 []
rejected = claim 1 $ D.Constr 1 [reason]
asset :: D.Data -> BS.ByteString
asset header = BS.replicate 4 0 <> F.blake2b224 (F.serialise header)
bound :: forall s. (D.Data, D.Data) -> Integer -> Term s PVerdictSubject
bound (header, membership) direction =
  pbindForcedSubjectToThread
    # pconstant (asset header)
    # fromData header
    # fromData membership
    # pconstant direction
bind :: forall s. D.Data -> D.Data -> Integer -> Term s PVerdictSubject
bind header membership direction = pbindForcedSubject # fromData header # fromData membership # pconstant direction
field :: Int -> D.Data -> D.Data -> D.Data
field index value (D.Constr tag fields) = D.Constr tag $ take index fields ++ [value] ++ drop (index + 1) fields
field _ _ _ = error "fixture must be constructor data"
