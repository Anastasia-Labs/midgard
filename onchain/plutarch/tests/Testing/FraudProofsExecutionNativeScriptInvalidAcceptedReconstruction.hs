{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsExecutionNativeScriptInvalidAcceptedReconstruction (tests) where

import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.ExecutionNativeScriptInvalid (
    PAuthenticatedExecutionSourceV1 (..),
    PBoundExecutionV1 (..),
    presultPending,
 )
import Midgard.FraudProofs.ExecutionNativeScriptInvalid.AcceptedReconstruction
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Testing.Eval (passertEvalNoTrace)

tests :: TestTree
tests =
    testGroup
        "Execution-native-script-invalid accepted reconstruction"
        [ testCase "purpose prefix binds the absolute execution coordinate" $ passertEvalNoTrace purposePrefixBindsAbsoluteCoordinate
        , testCase "source discovery prefers inline before reference" $ passertEvalNoTrace sourceDiscoveryPrefersInline
        , testCase "checkpoint refuses cursor mutation" $ passertEvalNoTrace checkpointRefusesCursorMutation
        , testCase "receive passes enumerate unique hashes in canonical order" $ passertEvalNoTrace receivePassesEnumerateCanonicalOrder
        , testCase "reference source index ignores references without a script" $ passertEvalNoTrace referenceSourceIndexIgnoresNoScript
        , testCase "selected pair collapses to the authenticated evaluator source" $ passertEvalNoTrace selectedPairAuthenticatesSource
        ]

purposePrefixBindsAbsoluteCoordinate :: forall s. Term s PBool
purposePrefixBindsAbsoluteCoordinate =
    plet (pinitialV1 # bound 1 # h28) $ \initial ->
        plet (pappendPurposeV1 # initial # pphaseSpend # 0 # otherH28 # phexByteStr "aa" # phexByteStr "00" # h28) $ \first ->
            plet (pappendPurposeV1 # first # pphaseSpend # 2 # h28 # phexByteStr "bb" # phexByteStr "01" # h28) $ \second ->
                pmatch second $ \PAcceptedReconstructionStateV1{preconstruction'selectedPurpose} ->
                    pmatch (pfromData preconstruction'selectedPurpose) $ \case
                        PDNothing -> pconstant False
                        PDJust selectedData -> pmatch (pfromData selectedData) $ \PSelectedPurposeV1{..} ->
                            pfromData pselectedPurpose'purposeKind
                                #== pphaseSpend
                                #&& pfromData pselectedPurpose'purposeIndex
                                #== 2
                                #&& pfromData pselectedPurpose'scriptHash
                                #== h28
                                #&& pstateIsAuthenticV1
                                # second

sourceDiscoveryPrefersInline :: forall s. Term s PBool
sourceDiscoveryPrefersInline =
    plet (selectedState 0) $ \selected ->
        plet (pappendSourceV1 # selected # inlineSource # h28) $ \inline ->
            plet (pfinishInlineSourcesV1 # inline # h28) $ \complete ->
                pmatch complete $ \PAcceptedReconstructionStateV1{preconstruction'phase, preconstruction'selectedSource} ->
                    pfromData preconstruction'phase
                        #== pphaseComplete
                        #&& pfromData preconstruction'selectedSource
                        #/= pcon PDNothing
                        #&& pstateIsAuthenticV1
                        # complete

checkpointRefusesCursorMutation :: forall s. Term s PBool
checkpointRefusesCursorMutation =
    plet (pinitialV1 # bound 1 # h28) $ \state -> pmatch state $ \PAcceptedReconstructionStateV1{..} ->
        pnot
            #$ pstateIsAuthenticV1
            # pcon
                ( PAcceptedReconstructionStateV1
                    preconstruction'bound
                    preconstruction'phase
                    (pdata 1)
                    preconstruction'executionCursor
                    preconstruction'previousKey
                    preconstruction'receiveCandidate
                    preconstruction'sourceBaseIndex
                    preconstruction'sourceCursor
                    preconstruction'selectedPurpose
                    preconstruction'selectedSource
                    preconstruction'nextExpectedScriptHash
                    preconstruction'checkpointHash
                )

receivePassesEnumerateCanonicalOrder :: forall s. Term s PBool
receivePassesEnumerateCanonicalOrder =
    plet (pfinishPurposePhaseV1 # (pinitialV1 # bound 1 # h28) # h28) $ \spendDone ->
        plet (pfinishPurposePhaseV1 # spendDone # h28) $ \mintDone ->
            plet (pfinishPurposePhaseV1 # mintDone # h28) $ \receive ->
                plet (pscanReceiveOutputV1 # (pscanReceiveOutputV1 # receive # pcon (PDJust $ pdata otherH28) # h28) # pcon (PDJust $ pdata h28) # h28) $ \passOne ->
                    plet (pfinishReceivePassV1 # passOne # h28 # h28) $ \afterFirst ->
                        plet (pscanReceiveOutputV1 # (pscanReceiveOutputV1 # afterFirst # pcon (PDJust $ pdata otherH28) # h28) # pcon (PDJust $ pdata otherH28) # h28) $ \passTwo ->
                            plet (pfinishReceivePassV1 # passTwo # h28 # h28) $ \afterSecond ->
                                pmatch afterFirst $ \PAcceptedReconstructionStateV1{preconstruction'previousKey} ->
                                    pmatch afterSecond $ \PAcceptedReconstructionStateV1{preconstruction'selectedPurpose} ->
                                        pmatch (pfromData preconstruction'selectedPurpose) $ \case
                                            PDNothing -> pconstant False
                                            PDJust selectedData -> pmatch (pfromData selectedData) $ \PSelectedPurposeV1{pselectedPurpose'purposeIndex, pselectedPurpose'scriptHash} ->
                                                pfromData preconstruction'previousKey
                                                    #== h28
                                                    #&& pfromData pselectedPurpose'purposeIndex
                                                    #== 1
                                                    #&& pfromData pselectedPurpose'scriptHash
                                                    #== otherH28
                                                    #&& pstateIsAuthenticV1
                                                    # afterSecond

referenceSourceIndexIgnoresNoScript :: forall s. Term s PBool
referenceSourceIndexIgnoresNoScript =
    plet (pfinishInlineSourcesV1 # selectedState 0 # h28) $ \references ->
        plet (padvanceReferenceWithoutSourceV1 # references # h28) $ \skipped ->
            plet (pappendSourceV1 # skipped # referenceSource # h28) $ \found ->
                pmatch skipped $ \PAcceptedReconstructionStateV1{preconstruction'fieldCursor, preconstruction'sourceCursor} ->
                    pmatch found $ \PAcceptedReconstructionStateV1{preconstruction'selectedSource} ->
                        pmatch (pfromData preconstruction'selectedSource) $ \case
                            PDNothing -> pconstant False
                            PDJust sourceData -> pmatch (pfromData sourceData) $ \PSelectedSourceV1{pselectedSource'sourceIndex} ->
                                pfromData preconstruction'fieldCursor
                                    #== 1
                                    #&& pfromData preconstruction'sourceCursor
                                    #== 0
                                    #&& pfromData pselectedSource'sourceIndex
                                    #== 0
                                    #&& pstateIsAuthenticV1
                                    # found

selectedPairAuthenticatesSource :: forall s. Term s PBool
selectedPairAuthenticatesSource =
    plet (pappendSourceV1 # selectedState 0 # inlineSource # h28) $ \selected ->
        pmatch (pauthenticatedSourceV1 # selected) $ \PAuthenticatedExecutionSourceV1{..} ->
            pfromData pauthenticatedSource'priorLedgerRoot
                #== h32
                #&& pfromData pauthenticatedSource'sourceIndex
                #== 0
                #&& pfromData pauthenticatedSource'originKind
                #== 0
                #&& pfromData pauthenticatedSource'scriptHash
                #== h28
                #&& pfromData pauthenticatedSource'compactCbor
                #== compactCbor

selectedState :: forall s. Term s PInteger -> Term s PAcceptedReconstructionStateV1
selectedState executionIndex =
    pappendPurposeV1
        # (pinitialV1 # bound executionIndex # h28)
        # pphaseSpend
        # 0
        # h28
        # phexByteStr "aa"
        # phexByteStr "00"
        # h28

inlineSource, referenceSource :: forall s. Term s PSelectedSourceV1
inlineSource = pcon $ PSelectedSourceV1 (pdata 0) (pdata 0) (pdata $ phexByteStr "00") (pdata 0) (pdata h28) (pdata 4) (pdata h32)
referenceSource = pcon $ PSelectedSourceV1 (pdata 0) (pdata 1) (pdata $ phexByteStr "01") (pdata 0) (pdata h28) (pdata 4) (pdata h32)

bound :: forall s. Term s PInteger -> Term s PBoundExecutionV1
bound executionIndex =
    pcon $
        PBoundExecutionV1
            (pdata acceptedSubject)
            (pdata h32)
            (pdata 1)
            (pdata executionIndex)
            (pdata presultPending)
            (pdata h32)
            (pdata compactCbor)

acceptedSubject :: forall s. Term s Subject.PVerdictSubject
acceptedSubject = pcon $ Subject.PVerdictSubject (pdata 1) (pdata 0) (pdata 0) (pdata txId) (pdata $ pconstant "") (pdata $ pcon PDNothing)

txId, h28, otherH28, h32, compactCbor :: forall s. Term s PByteString
txId = phexByteStr "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
h28 = phexByteStr "22222222222222222222222222222222222222222222222222222222"
otherH28 = phexByteStr "33333333333333333333333333333333333333333333333333333333"
h32 = phexByteStr "1111111111111111111111111111111111111111111111111111111111111111"
compactCbor = phexByteStr "80"
