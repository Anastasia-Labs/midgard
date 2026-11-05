{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsFieldPreimageLengthMismatch (tests) where

import Data.ByteString qualified as BS
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import PlutusCore.Data qualified as PD
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.CanonicalDecodability (PCommittedFieldClaimV1 (PBodyFieldClaim))
import Midgard.FraudProofs.FieldPreimageLengthMismatch
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Types (PNativeTxFieldPreimageLengthsV1 (..))
import Midgard.FraudProofs.ProofThreadSubstrate (PVerdictSubject (..))
import Midgard.NativeTxFieldAccess (PFieldCarriageV1 (PInline), pmaxTransactionAggregateFieldBytes)
import Midgard.RejectionReason (PRejectionReasonV1 (PFieldPreimageLengthMismatch, PNetworkIdMismatch))
import Testing.Eval (passertEvalNoTrace, pfails)
import Testing.FraudProofsFixture

tests :: TestTree
tests =
  testGroup
    "Field preimage length mismatch"
    [ testCase "maps all nine declared lengths in wire order" $
        passertEvalNoTrace $
          pfieldPreimageLengthAtV1
            # lengths
            # 0
            #== 1
            #&& pfieldPreimageLengthAtV1
            # lengths
            # 1
            #== 2
            #&& pfieldPreimageLengthAtV1
            # lengths
            # 2
            #== 3
            #&& pfieldPreimageLengthAtV1
            # lengths
            # 3
            #== 4
            #&& pfieldPreimageLengthAtV1
            # lengths
            # 4
            #== 5
            #&& pfieldPreimageLengthAtV1
            # lengths
            # 5
            #== 6
            #&& pfieldPreimageLengthAtV1
            # lengths
            # 6
            #== 7
            #&& pfieldPreimageLengthAtV1
            # lengths
            # 7
            #== 8
            #&& pfieldPreimageLengthAtV1
            # lengths
            # 8
            #== 9
    , testCase "rejects negative declared-length coordinate" $
        pfails $
          pfieldPreimageLengthAtV1 # lengths # (-1)
    , testCase "rejects coordinate nine" $
        pfails $
          pfieldPreimageLengthAtV1 # lengths # 9
    , testCase "mismatch is decisive" $
        passertEvalNoTrace $
          pdecisiveFaultHoldsV1 # 0 # 41 # 40
    , testCase "equality is not a fault" $
        passertEvalNoTrace $
          pnot # (pdecisiveFaultHoldsV1 # 8 # 104 # 104)
    , testCase "admits the maximum actual length" $
        passertEvalNoTrace $
          pdecisiveFaultHoldsV1
            # 2
            # (pmaxTransactionAggregateFieldBytes - 1)
            # pmaxTransactionAggregateFieldBytes
    , testCase "rejects the adjacent over-bound actual length" $
        pfails $
          pdecisiveFaultHoldsV1
            # 2
            # pmaxTransactionAggregateFieldBytes
            # (pmaxTransactionAggregateFieldBytes + 1)
    , testCase "rejects a negative declared length" $
        pfails $
          pdecisiveFaultHoldsV1 # 2 # (-1) # 1
    , testCase "authenticates the committed body preimage and both lengths" $
        passertEvalNoTrace $
          pmatch authenticatedSpendInputs $ \PLengthEvidenceV1{..} ->
            pfromData plengthEvidence'fieldIndex
              #== 0
              #&& pfromData plengthEvidence'declaredLength
              #== pconstant (fromIntegral $ BS.length $ spendInputsPreimage tx1)
              #&& pfromData plengthEvidence'actualLength
              #== pconstant (fromIntegral $ BS.length $ spendInputsPreimage tx1)
    , testCase "authenticated reducer rejects substituted field bytes" $
        pfails $
          authenticatedSpendInputsWith (spendInputsPreimage tx1 <> "\x00") (fieldPreimageLengthsCborOf tx1)
    , testCase "authenticated reducer rejects non-canonical lengths CBOR" $
        pfails $
          authenticatedSpendInputsWith (spendInputsPreimage tx1) (fieldPreimageLengthsCborOf tx1 <> "\x00")
    , testCase "authenticated reducer rejects a body claim at witness field six" $
        pfails $
          authenticatedBodyClaim 6 (spendInputsPreimage tx1) (fieldPreimageLengthsCborOf tx1)
    , testCase "convicts accepted mismatch" $
        passertEvalNoTrace $
          pterminalContradictionV1 # state (subject False 0) 0 41 40
    , testCase "refuses honestly accepted equality" $
        passertEvalNoTrace $
          pnot # (pterminalContradictionV1 # state (subject False 0) 0 40 40)
    , testCase "convicts wrongfully rejected equality" $
        passertEvalNoTrace $
          pterminalContradictionV1 # state (subject True 0) 0 40 40
    , testCase "refuses honestly rejected mismatch" $
        passertEvalNoTrace $
          pnot # (pterminalContradictionV1 # state (subject True 0) 0 41 40)
    , testCase "forced binder refuses another rejection reason" $
        pfails $
          pbindForcedLengthEvidenceV1 # unrelatedSubject 0 # evidence 0 40 40
    , testCase "forced binder refuses a mutated field coordinate" $
        pfails $
          pbindForcedLengthEvidenceV1 # subject True 1 # evidence 0 40 40
    , testCase "length evidence Data ABI matches target" $
        assertWire
          (pcon $ PLengthEvidenceV1 (pdata 0) (pdata 41) (pdata 40))
          (PD.Constr 0 [PD.I 0, PD.I 41, PD.I 40])
    , testCase "bound-source Data ABI matches target" $
        assertWire
          (pcon $ PBoundSource (pdata $ subject False 0) (pdata $ pconstant "source"))
          (PD.Constr 0 [subjectData False 0, PD.B "source"])
    , testCase "pending-forced Data ABI matches target" $
        assertWire
          (pcon $ PPendingForced $ pdata 1)
          (PD.Constr 1 [PD.I 1])
    , testCase "terminal state Data ABI matches target" $
        assertWire
          (state (subject False 0) 0 41 40)
          (PD.Constr 0 [subjectData False 0, PD.I 0, PD.I 41, PD.I 40])
    ]

lengths :: forall s. Term s PNativeTxFieldPreimageLengthsV1
lengths =
  pcon $
    PNativeTxFieldPreimageLengthsV1
      { plengths'spendInputs = 1
      , plengths'referenceInputs = 2
      , plengths'outputs = 3
      , plengths'requiredObservers = 4
      , plengths'requiredSigners = 5
      , plengths'mint = 6
      , plengths'addressWitnesses = 8
      , plengths'scriptWitnesses = 7
      , plengths'redeemers = 9
      }

authenticatedSpendInputs :: forall s. Term s PLengthEvidenceV1
authenticatedSpendInputs =
  authenticatedSpendInputsWith (spendInputsPreimage tx1) (fieldPreimageLengthsCborOf tx1)

authenticatedSpendInputsWith :: forall s. BS.ByteString -> BS.ByteString -> Term s PLengthEvidenceV1
authenticatedSpendInputsWith preimage lengthsCbor = authenticatedBodyClaim 0 preimage lengthsCbor

authenticatedBodyClaim :: forall s. Integer -> BS.ByteString -> BS.ByteString -> Term s PLengthEvidenceV1
authenticatedBodyClaim fieldIndex preimage lengthsCbor =
  pmatch
    ( pverifyNativeTxProofSourceV1
        # pconstant tx1Id
        # pconstant tx1Cbor
        # pconstant (witnessSetCborOf tx1)
        # pconstant lengthsCbor
    )
    $ \(PPair verified _) ->
      pauthenticatedLengthEvidenceV1
        # verified
        # pconstant lengthsCbor
        # pcon (PBodyFieldClaim (pdata $ pconstant fieldIndex) (pdata $ pcon $ PInline $ pdata $ pconstant preimage))
        # pnil
        # pdata (pconstant certificatePolicy)

subject :: forall s. Bool -> Integer -> Term s PVerdictSubject
subject forced fieldIndex =
  pcon $
    PVerdictSubject
      (pdata 1)
      (pdata $ pconstant $ if forced then 1 else 0)
      (pdata $ pconstant $ if forced then 1 else 0)
      (pdata $ pconstant tx1Id)
      (pdata $ pconstant $ if forced then "source" else "")
      ( pdata $
          if forced
            then pcon $ PDJust $ pdata $ pcon $ PFieldPreimageLengthMismatch (pdata $ pconstant fieldIndex)
            else pcon PDNothing
      )

unrelatedSubject :: forall s. Integer -> Term s PVerdictSubject
unrelatedSubject fieldIndex =
  pmatch (subject True fieldIndex) $ \value ->
    pcon value{psubject'rejectionReason = pdata $ pcon $ PDJust $ pdata $ pcon PNetworkIdMismatch}

state :: forall s. Term s PVerdictSubject -> Integer -> Integer -> Integer -> Term s PStep03State
state verdict fieldIndex declaredLength actualLength =
  pcon $
    PStep03State
      (pdata verdict)
      (pdata $ pconstant fieldIndex)
      (pdata $ pconstant declaredLength)
      (pdata $ pconstant actualLength)

evidence :: forall s. Integer -> Integer -> Integer -> Term s PLengthEvidenceV1
evidence fieldIndex declaredLength actualLength =
  pcon $
    PLengthEvidenceV1
      (pdata $ pconstant fieldIndex)
      (pdata $ pconstant declaredLength)
      (pdata $ pconstant actualLength)

subjectData :: Bool -> Integer -> PD.Data
subjectData forced fieldIndex =
  PD.Constr
    0
    [ PD.I 1
    , PD.I $ if forced then 1 else 0
    , PD.I $ if forced then 1 else 0
    , PD.B tx1Id
    , PD.B $ if forced then "source" else ""
    , if forced
        then PD.Constr 0 [PD.Constr 0 [PD.I fieldIndex]]
        else PD.Constr 1 []
    ]

assertWire :: forall a. (PIsData a) => (forall s. Term s a) -> PD.Data -> Assertion
assertWire value expected =
  passertEvalNoTrace $
    pserialiseData # pforgetData (pdata value) #== pconstant (serialise expected)
