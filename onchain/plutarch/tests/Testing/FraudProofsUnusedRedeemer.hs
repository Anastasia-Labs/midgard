{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsUnusedRedeemer (tests) where

import Data.ByteString qualified as BS
import Plutarch.LedgerApi.Utils (PMaybeData (PDJust, PDNothing))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.BoundedItem (PChunkProofV1 (..), pfromBytes, phashChunk, pverifyChunk)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.UnusedRedeemer
import Midgard.RedeemerItemProof (phashControlV1, pinitialControlV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PUnusedRedeemer))
import Midgard.ScriptProof (pexecutionLeafHash, ppurposeLeafHash, predeemerItemLeafHash)
import Midgard.ValidationMerkle (pappendLeaf)
import Testing.Eval (passertEvalNoTrace)

tests :: TestTree
tests =
  testGroup
    "Unused-redeemer rule"
    [ testCase "purpose tag map is exact" $ passertEvalNoTrace purposeTagMap
    , testCase "exact selected pointer is marked used" $ passertEvalNoTrace $ pscanUsed 2 4 (pconstant True)
    , testCase "native selection is not marked used" $ passertEvalNoTrace $ pnot # pscanUsed 2 4 (pconstant False)
    , testCase "another pointer remains unused" $ passertEvalNoTrace $ pnot # pscanUsed 2 3 (pconstant True)
    , testCase "complete absence convicts wrongful acceptance" $ passertEvalNoTrace absenceConvicts
    , testCase "used pointer contradicts forced rejection" $ passertEvalNoTrace usedContradictsForced
    , testCase "premature absence is incomplete" $ passertEvalNoTrace prematureAbsence
    , testCase "bounded header and tail authenticate exact descriptor" $ passertEvalNoTrace headerAndTail
    , testCase "substituted item chunk refuses membership" $ passertEvalNoTrace substitutedChunk
    , testCase "scan batch is pinned" $ passertEvalNoTrace $ pmaximumScanBatch #== 16
    ]

purposeTagMap :: forall s. Term s PBool
purposeTagMap =
  pmatch (predeemerTagForPurposeKindV1 # 0) $ \case
    PJust zero ->
      zero
        #== 0
        #&& pmatch
          (predeemerTagForPurposeKindV1 # 1)
          ( \case
              PJust one ->
                one
                  #== 1
                  #&& pmatch
                    (predeemerTagForPurposeKindV1 # 2)
                    ( \case
                        PJust three ->
                          three
                            #== 3
                            #&& pmatch
                              (predeemerTagForPurposeKindV1 # 3)
                              ( \case
                                  PJust six -> six #== 6 #&& pmatch (predeemerTagForPurposeKindV1 # 4) (\case PNothing -> pconstant True; _ -> pconstant False)
                                  _ -> pconstant False
                              )
                        _ -> pconstant False
                    )
              _ -> pconstant False
          )
    _ -> pconstant False

pscanUsed :: forall s. Term s PInteger -> Term s PInteger -> Term s PBool -> Term s PBool
pscanUsed kind pointer selected =
  pmatch (poneScan kind pointer selected) $ \PReverseScanV1{preverseScan'used} -> pfromData preverseScan'used

absenceConvicts :: forall s. Term s PBool
absenceConvicts = pterminalContradictionV1 # (pdecisionV1 # poneScan 2 3 (pconstant True))

usedContradictsForced :: forall s. Term s PBool
usedContradictsForced =
  plet (ppurposeLeafHash # 2 # 4 # h28 # phexByteStr "aa") $ \purpose ->
    plet (predeemerItemLeafHash # 1 # h32) $ \target ->
      plet (pexecutionLeafHash # 3 # purpose # source # target) $ \execution ->
        let authenticated = pauthenticated 1 purpose execution
            opening = popening 2 4 (pconstant True) purpose target
         in pterminalContradictionV1 # (pdecisionV1 # (pscanSelectionV1 # (pinitialReverseScanV1 # authenticated) # opening))

prematureAbsence :: forall s. Term s PBool
prematureAbsence =
  let authenticated =
        pcon $
          PAuthenticatedRedeemerV1
            (pdata $ pbound 0 1)
            (pdata 3)
            (pdata 4)
            (pdata 2)
            (pdata 12)
            (pdata h32)
            (pdata $ predeemerItemLeafHash # 1 # h32)
            (pdata 2)
            (pdata pnil)
            (pdata 2)
            (pdata pnil)
   in pnot # (preverseScanCompleteV1 # (pinitialReverseScanV1 # authenticated))

headerAndTail :: forall s. Term s PBool
headerAndTail =
  plet (pfromBytes # 8 # 0 # item) $ \commitment ->
    plet (pinitialControlV1 # 0 # 0 # 1 # 8 # commitment # (-1) # (-1)) $ \control ->
      plet (pacceptedControl commitment (phashControlV1 # control)) $ \authenticated ->
        plet (pauthenticateItemHeaderV1 # authenticated # control # (pchunk item) # pcon PDNothing) $ \header ->
          pmatch (pauthenticateItemTailV1 # header # (pchunk item) # pcon PDNothing) $ \PAuthenticatedRedeemerV1{pauthenticatedRedeemer'purposeTag, pauthenticatedRedeemer'pointerIndex} ->
            pfromData pauthenticatedRedeemer'purposeTag #== 0 #&& pfromData pauthenticatedRedeemer'pointerIndex #== 4

substitutedChunk :: forall s. Term s PBool
substitutedChunk =
  plet (pfromBytes # 8 # 0 # item) $ \commitment ->
    pnot # (pverifyChunk # commitment # pchunk (phexByteStr "8400034100820101"))

poneScan :: forall s. Term s PInteger -> Term s PInteger -> Term s PBool -> Term s PReverseScanV1
poneScan kind pointer selected =
  plet (ppurposeLeafHash # kind # pointer # h28 # phexByteStr "aa") $ \purpose ->
    plet (predeemerItemLeafHash # 1 # h32) $ \target ->
      plet (pif selected target (pconstant "")) $ \redeemer ->
        plet (pexecutionLeafHash # pif selected 3 0 # purpose # source # redeemer) $ \execution ->
          pscanSelectionV1
            # (pinitialReverseScanV1 # pauthenticated 0 purpose execution)
            # popening kind pointer selected purpose redeemer

pauthenticated :: forall s. Term s PInteger -> Term s PByteString -> Term s PByteString -> Term s PAuthenticatedRedeemerV1
pauthenticated direction purpose execution =
  pcon $
    PAuthenticatedRedeemerV1
      (pdata $ pbound direction 1)
      (pdata 3)
      (pdata 4)
      (pdata 2)
      (pdata 12)
      (pdata h32)
      (pdata $ predeemerItemLeafHash # 1 # h32)
      (pdata 1)
      (pdata $ pappendLeaf # 0 # pnil # purpose)
      (pdata 1)
      (pdata $ pappendLeaf # 0 # pnil # execution)

popening :: forall s. Term s PInteger -> Term s PInteger -> Term s PBool -> Term s PByteString -> Term s PByteString -> Term s PSelectionOpeningV1
popening kind pointer selected _ redeemer =
  pcon $
    PSelectionOpeningV1
      (pdata 0)
      (pdata kind)
      (pdata pointer)
      (pdata h28)
      (pdata $ phexByteStr "aa")
      (pdata pnil)
      (pdata $ pif selected 3 0)
      (pdata source)
      (pdata redeemer)
      (pdata pnil)

pacceptedControl :: forall s. Term s PByteString -> Term s PByteString -> Term s PAuthenticatedControlV1
pacceptedControl _ expectedHash =
  pcon $
    PAuthenticatedControlV1
      (pdata $ pbound 0 0)
      (pdata 88)
      (pdata 12)
      (pdata expectedHash)
      (pdata 0)
      (pdata $ -1)
      (pdata $ -1)
      (pdata 1)
      (pdata 0)
      (pdata pnil)
      (pdata 0)
      (pdata pnil)

pchunk :: forall s. Term s PByteString -> Term s PChunkProofV1
pchunk bytes =
  pcon $
    PChunkProofV1
      (pdata 1)
      (pdata 8)
      (pdata 0)
      (pdata 8)
      (pdata 0)
      (pdata bytes)
      (pdata $ pappendLeaf # 0 # pnil # (phashChunk # 8 # 0 # 0 # bytes))
      (pdata pnil)

pbound :: forall s. Term s PInteger -> Term s PInteger -> Term s PBoundRedeemerV1
pbound direction redeemerIndex = pbindRedeemerV1 # psubject direction redeemerIndex # h32 # 1 # redeemerIndex

psubject :: forall s. Term s PInteger -> Term s PInteger -> Term s Subject.PVerdictSubject
psubject direction redeemerIndex =
  pcon $
    Subject.PVerdictSubject
      (pdata 1)
      (pdata direction)
      (pdata $ pif (direction #== 0) 0 1)
      (pdata $ pconstant txId)
      (pdata $ pif (direction #== 0) (pconstant "") (phexByteStr "01"))
      ( pdata $
          pif
            (direction #== 0)
            (pcon PDNothing)
            (pcon $ PDJust $ pdata $ pcon $ PUnusedRedeemer $ pdata redeemerIndex)
      )

item, h28, h32, source :: forall s. Term s PByteString
item = phexByteStr "8400044100820101"
h28 = pconstant $ BS.replicate 28 0x11
h32 = pconstant $ BS.replicate 32 0x22
source = pconstant $ BS.replicate 32 0x33

txId :: BS.ByteString
txId = BS.pack [0 .. 31]
