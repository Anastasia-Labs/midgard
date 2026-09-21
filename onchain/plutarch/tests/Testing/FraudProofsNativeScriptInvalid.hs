{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsNativeScriptInvalid (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (pubKeyHashAddress, scriptHashAddress)
import PlutusLedgerApi.V1.Value (TokenName (..), singleton)
import PlutusLedgerApi.V3 (
    Credential (ScriptCredential),
    Datum (..),
    OutputDatum (..),
    PubKeyHash (..),
    Redeemer (..),
    ScriptContext (..),
    ScriptHash (..),
    ScriptPurpose (Rewarding),
    TxId (..),
    TxInInfo (..),
    TxInfo (..),
    TxOut (..),
    TxOutRef (..),
 )
import PlutusTx.Builtins (dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude
import Testing.Eval (pfailsNoTraceWithoutHoistChecks, psucceedsNoTraceWithoutHoistChecks)
import Testing.FraudProofsFixture

import Midgard.FraudProofs.NativeScriptInvalid (pauthenticatedSigner, pbindScriptIndex)
import Midgard.FraudProofs.ProofThreadSubstrate (PVerdictSubject)
import Midgard.Validators.FraudProofs.ExecutionNativeScriptInvalid qualified as Execution
import Midgard.Validators.FraudProofs.NativeScriptInvalid
import Plutarch.Unsafe (punsafeCoerce)

tests :: TestTree
tests =
    testGroup
        "Native-script-invalid"
        [ testGroup
            "forced signer authentication"
            [ testCase "a valid committed signature contributes a signer" $ succeeds $ signerCase True True True
            , testCase "a forged signature contributes no signer" $ succeeds $ signerCase True False False
            , testCase "accepted faults retain witness-key semantics" $ succeeds $ signerCase False False True
            , testCase "wrongful rejection convicts a satisfied script" $ succeeds $ runForcedStep03 True satisfiedScriptItem
            , testCase "honest rejection cannot convict an unsatisfied script" $ fails $ runForcedStep03 True unsatisfiedScriptItem
            , testCase "a forged witness cannot satisfy the rejected script" $ fails $ runForcedStep03 False satisfiedScriptItem
            , testCase "binds the exact rejected script coordinate" $ succeeds $ indexCase 0 0
            , testCase "refuses another coordinate" $ fails $ indexCase 0 1
            , testCase "refuses a negative coordinate" $ fails $ indexCase (-1) (-1)
            ]
        , testGroup
            "direct native verdict"
            [ testCase "finalizes an unsatisfied native witness" $ succeeds $ runStep03 unsatisfiedScriptItem
            , testCase "rejects a satisfied native witness" $ fails $ runStep03 satisfiedScriptItem
            , testCase "rejects malformed native bytes" $ fails $ runStep03 malformedScriptItem
            ]
        , testGroup
            "authenticated signer verdict"
            [ testCase "finalizes authenticated absence" $ succeeds $ runStep05 absentScriptItem [missingQuery]
            , testCase "rejects a satisfied script" $ fails $ runStep05 presentScriptItem [presentQuery]
            , testCase "rejects a missing signer query" $ fails $ runStep05 absentScriptItem []
            , testCase "rejects a forged absence boundary" $ fails $ runStep05 absentScriptItem [forgedMissingQuery]
            ]
        , testGroup
            "cancellation"
            [ testCase "step 03 preserves prover cancellation" $ succeeds $ step03 $ cancellationContext step03State True
            , testCase "step 04 preserves prover cancellation" $ succeeds $ step04 $ cancellationContext step04State True
            , testCase "step 05 preserves prover cancellation" $ succeeds $ step05 $ cancellationContext (step05State absentScriptItem readyPhase) True
            ]
        , testGroup
            "script cursor"
            [ testCase "starts a resumable pushdown" $ succeeds startCompoundScan
            , testCase "resumes the committed cursor" $ succeeds resumeCompoundScan
            , testCase "rejects a mutated cursor" $ fails mutatedCompoundCursor
            ]
        , testGroup
            "318-witness staged vectors"
            [ q34Case "step 03 refuses a 17-item start batch" $ \chunks -> fails $ q34Step03Budget chunks nextScript 17
            , q34Case "step 04 refuses a 17-item resume batch" $ \chunks -> fails $ q34Step04 chunks q34State16 q34Checkpoint16 17 q34State32 False
            , q34Case "step 03 exact 318-witness fit" $ \chunks -> succeeds $ q34Step03 chunks nextScript
            , q34Case "step 03 rejects the wrong successor" $ \chunks -> fails $ q34Step03 chunks stepScript
            , q34Case "step 04 exact 318-witness resume fit" $ \chunks -> succeeds $ q34Step04 chunks q34State16 q34Checkpoint16 16 q34State32 False
            , q34Case "step 04 rejects a mutated checkpoint" $ \chunks -> fails $ q34Step04 chunks q34State16 q34Checkpoint32 16 q34State32 False
            , q34Case "step 04 exact 318-witness penultimate fit" $ \chunks -> succeeds $ q34Step04 chunks q34State288 q34Checkpoint288 16 q34State304 False
            , q34Case "step 04 exact 318-witness terminal fit" $ \chunks -> succeeds $ q34Step04 chunks q34State304 q34Checkpoint304 14 q34State318 True
            , testCase "step 05 exact 318-signer frontier fit" $ succeeds q34Step05
            ]
        , testGroup
            "maximum native script"
            [ testCase "max 32-node native script starts" $ succeeds $ startMaxScript maxUnsatisfiedPayload
            , testCase "max 32-node native script finalizes" $ succeeds $ finalizeMaxScript maxUnsatisfiedPayload
            , testCase "rejects max 32-node satisfied native script" $ fails $ finalizeMaxScript maxSatisfiedPayload
            ]
        , testGroup
            "execution-native evaluator"
            [ testCase "step 03 authenticates the selected native item" $ succeeds $ runExecutionStep03 unsatisfiedScriptItem nextScript
            , testCase "step 03 rejects a substituted native item" $ fails $ runExecutionStep03 satisfiedScriptItem nextScript
            , testCase "step 03 rejects the wrong successor" $ fails $ runExecutionStep03 unsatisfiedScriptItem stepScript
            , testCase "step 04 finalizes an unsatisfied native witness" $ succeeds $ runExecutionStep04 unsatisfiedScriptItem
            , testCase "step 04 rejects a satisfied native witness" $ fails $ runExecutionStep04 satisfiedScriptItem
            , testCase "step 04 rejects malformed native bytes" $ fails $ runExecutionStep04 malformedScriptItem
            , testCase "step 06 finalizes authenticated absence" $ succeeds $ runExecutionStep06 absentScriptItem [missingQuery]
            , testCase "step 06 rejects a satisfied script" $ fails $ runExecutionStep06 presentScriptItem [presentQuery]
            , testCase "step 06 rejects a missing signer query" $ fails $ runExecutionStep06 absentScriptItem []
            , testCase "step 06 rejects a forged absence boundary" $ fails $ runExecutionStep06 absentScriptItem [forgedMissingQuery]
            , testCase "step 06 starts a resumable pushdown" $ succeeds startExecutionCompoundScan
            , testCase "step 06 resumes the committed cursor" $ succeeds resumeExecutionCompoundScan
            , testCase "step 06 rejects a mutated cursor" $ fails mutatedExecutionCompoundCursor
            , q34Case "step 04 exact staged start" $ \chunks -> succeeds $ executionQ34Step04 chunks 16 nextScript
            , q34Case "step 04 rejects the wrong successor" $ \chunks -> fails $ executionQ34Step04 chunks 16 stepScript
            , q34Case "step 04 rejects a 17-item start batch" $ \chunks -> fails $ executionQ34Step04 chunks 17 nextScript
            , q34Case "step 05 exact staged resume" $ \chunks -> succeeds $ executionQ34Step05 chunks executionQ34State16 q34Checkpoint16 16 executionQ34State32 False
            , q34Case "step 05 rejects a mutated checkpoint" $ \chunks -> fails $ executionQ34Step05 chunks executionQ34State16 q34Checkpoint32 16 executionQ34State32 False
            , q34Case "step 05 rejects a 17-item resume batch" $ \chunks -> fails $ executionQ34Step05 chunks executionQ34State16 q34Checkpoint16 17 executionQ34State32 False
            , q34Case "step 05 exact staged penultimate fit" $ \chunks -> succeeds $ executionQ34Step05 chunks executionQ34State288 q34Checkpoint288 16 executionQ34State304 False
            , q34Case "step 05 exact staged finalization" $ \chunks -> succeeds $ executionQ34Step05 chunks executionQ34State304 q34Checkpoint304 14 executionQ34State318 True
            , testCase "step 06 exact 318-signer frontier fit" $ succeeds executionQ34Step06
            , testCase "step 06 max 32-node script starts" $ succeeds $ startExecutionMaxScript maxUnsatisfiedPayload 16
            , testCase "step 06 rejects a 17-node batch" $ fails $ startExecutionMaxScript maxUnsatisfiedPayload 17
            , testCase "step 06 max 32-node script finalizes" $ succeeds $ finalizeExecutionMaxScript maxUnsatisfiedPayload
            , testCase "step 06 rejects max 32-node satisfied script" $ fails $ finalizeExecutionMaxScript maxSatisfiedPayload
            ]
        , testGroup
            "execution-native cancellation"
            [ cancelCase "step 01" executionStep01
            , cancelCase "step 02" executionStep02
            , cancelCase "step 03" executionStep03
            , cancelCase "step 04" executionStep04
            , cancelCase "step 05" executionStep05
            , cancelCase "step 06" executionStep06
            , cancelCase "accepted reconstruction init" executionAcceptedInit
            , cancelCase "accepted spend prefix" executionAcceptedSpend
            , cancelCase "accepted mint prefix" executionAcceptedMint
            , cancelCase "accepted observer prefix" executionAcceptedObserver
            , cancelCase "accepted receive prefix" executionAcceptedReceive
            , cancelCase "accepted inline source" executionAcceptedInline
            , cancelCase "accepted reference source" executionAcceptedReference
            ]
        , testGroup
            "execution-native entry steps"
            [ testCase "step 01 binds an accepted transaction" $ succeeds $ runExecutionStep01Accepted nextScript executionStep01BoundState
            , testCase "step 01 rejects the wrong successor" $ fails $ runExecutionStep01Accepted stepScript executionStep01BoundState
            , testCase "step 01 rejects a substituted bound state" $ fails $ runExecutionStep01Accepted nextScript executionStep01SubstitutedState
            , testCase "step 01 binds a forced accepted transaction" $ succeeds $ runExecutionStep01Forced 0 0 (PD.Constr 0 []) 0
            , testCase "step 01 binds a forced false rejection" $ succeeds $ runExecutionStep01Forced 1 1 (PD.Constr 1 [PD.Constr 38 [PD.I 0]]) 0
            , testCase "step 01 rejects a forced false coordinate substitution" $ fails $ runExecutionStep01Forced 1 1 (PD.Constr 1 [PD.Constr 38 [PD.I 1]]) 0
            , testCase "step 02 authenticates the exact execution and source frontiers" $ succeeds $ runExecutionStep02 executionStep02Control nextScript executionStep02AuthenticatedSource
            , testCase "step 02 rejects the wrong successor" $ fails $ runExecutionStep02 executionStep02Control stepScript executionStep02AuthenticatedSource
            , testCase "step 02 rejects a substituted source frontier" $ fails $ runExecutionStep02 executionStep02SubstitutedControl nextScript executionStep02AuthenticatedSource
            ]
        , testGroup
            "execution-native accepted reconstruction"
            [ testCase "init emits the authenticated spend state" $ succeeds $ runExecutionAcceptedInit nextScript acceptedInitialState
            , testCase "init rejects the wrong successor" $ fails $ runExecutionAcceptedInit stepScript acceptedInitialState
            , testCase "init rejects a mutated checkpoint" $ fails $ runExecutionAcceptedInit nextScript $ mutateAcceptedCheckpoint acceptedInitialState
            , testCase "spend finish advances to mint" $ succeeds $ runExecutionAcceptedSpend acceptedSpendCompleteState nextScript acceptedMintState
            , testCase "spend finish rejects an incomplete field" $ fails $ runExecutionAcceptedSpend acceptedInitialAtOwnHash nextScript acceptedMintState
            , testCase "spend finish rejects the wrong successor" $ fails $ runExecutionAcceptedSpend acceptedSpendCompleteState stepScript acceptedMintState
            , testCase "spend scan authenticates a script purpose" $ succeeds $ runExecutionAcceptedSpendScan acceptedSpendDescriptor acceptedSpendDescriptor nextScript
            , testCase "spend scan accepts published-chunk membership" $ succeeds $ runExecutionAcceptedSpendScanPublished acceptedPublishedChunks acceptedPublishedChunks
            , testCase "spend scan rejects substituted published chunks" $ fails $ runExecutionAcceptedSpendScanPublished acceptedPublishedChunks [7]
            , testCase "spend scan rejects a missing membership withdrawal" $ fails runExecutionAcceptedSpendScanWithoutMembership
            , testCase "spend scan rejects a substituted membership value" $ fails $ runExecutionAcceptedSpendScan acceptedSpendDescriptor acceptedSpendDescriptorOtherValue nextScript
            , testCase "spend scan rejects a descriptor output-index mismatch" $ fails $ runExecutionAcceptedSpendScan acceptedSpendDescriptorWrongIndex acceptedSpendDescriptorWrongIndex nextScript
            , testCase "spend scan rejects the wrong successor" $ fails $ runExecutionAcceptedSpendScan acceptedSpendDescriptor acceptedSpendDescriptor stepScript
            , testCase "mint scan selects the execution purpose" $ succeeds $ runExecutionAcceptedMintScan acceptedMintScanState nextScript acceptedMintSelectedState
            , testCase "mint scan rejects the wrong successor" $ fails $ runExecutionAcceptedMintScan acceptedMintScanState stepScript acceptedMintSelectedState
            , testCase "mint scan advances past an earlier purpose" $ succeeds $ runExecutionAcceptedMintScan acceptedMintBeforeTarget stepScript acceptedMintAfterEarlier
            , testCase "mint scan refuses a selected successor before the target" $ fails $ runExecutionAcceptedMintScan acceptedMintBeforeTarget nextScript acceptedMintAfterEarlier
            , testCase "mint finish advances to observers" $ succeeds $ runExecutionAcceptedMint acceptedMintAtOwnHash nextScript acceptedObserverState
            , testCase "mint finish rejects the wrong phase" $ fails $ runExecutionAcceptedMint acceptedInitialAtOwnHash nextScript acceptedObserverState
            , testCase "observer scan selects the execution purpose" $ succeeds $ runExecutionAcceptedObserverScan acceptedObserverScanState nextScript acceptedObserverSelectedState
            , testCase "observer scan rejects the wrong successor" $ fails $ runExecutionAcceptedObserverScan acceptedObserverScanState stepScript acceptedObserverSelectedState
            , testCase "observer scan advances past an earlier purpose" $ succeeds $ runExecutionAcceptedObserverScan acceptedObserverBeforeTarget stepScript acceptedObserverAfterEarlier
            , testCase "observer scan refuses a selected successor before the target" $ fails $ runExecutionAcceptedObserverScan acceptedObserverBeforeTarget nextScript acceptedObserverAfterEarlier
            , testCase "observer finish advances to receive" $ succeeds $ runExecutionAcceptedObserver acceptedObserverAtOwnHash nextScript acceptedReceiveState
            , testCase "observer finish rejects the wrong phase" $ fails $ runExecutionAcceptedObserver acceptedMintAtOwnHash nextScript acceptedReceiveState
            , testCase "receive scan retains the least protected script" $ succeeds $ runExecutionAcceptedReceiveScan acceptedReceiveAtOwnHash stepScript acceptedReceiveAfterFirst
            , testCase "receive scan retains the least later protected script" $ succeeds $ runExecutionAcceptedReceiveScan acceptedReceiveAfterFirst stepScript acceptedReceiveAfterSecond
            , testCase "receive scan ignores a later key output" $ succeeds $ runExecutionAcceptedReceiveScan acceptedReceiveAfterSecond stepScript acceptedReceiveAfterThird
            , testCase "receive finish selects the execution purpose" $ succeeds $ runExecutionAcceptedReceiveFinish acceptedReceiveAfterThird nextScript acceptedInlineState
            , testCase "receive finish rejects an empty candidate" $ fails $ runExecutionAcceptedReceiveFinish acceptedReceiveEmptyComplete nextScript acceptedInlineState
            , testCase "receive finish advances to another pass before the target" $ succeeds $ runExecutionAcceptedReceiveFinish acceptedReceiveRepeatFirstComplete stepScript acceptedReceiveRepeatSecondStart
            , testCase "receive finish refuses a selected successor before the target" $ fails $ runExecutionAcceptedReceiveFinish acceptedReceiveRepeatFirstComplete nextScript acceptedReceiveRepeatSecondStart
            , testCase "receive second pass skips the previous script" $ succeeds $ runExecutionAcceptedReceiveScan acceptedReceiveRepeatSecondStart stepScript acceptedReceiveRepeatSecondAfterFirst
            , testCase "receive second pass selects the next script" $ succeeds $ runExecutionAcceptedReceiveScan acceptedReceiveRepeatSecondAfterFirst stepScript acceptedReceiveRepeatSecondAfterSecond
            , testCase "receive second pass ignores the key output" $ succeeds $ runExecutionAcceptedReceiveScan acceptedReceiveRepeatSecondAfterSecond stepScript acceptedReceiveRepeatSecondComplete
            , testCase "receive second finish selects the target purpose" $ succeeds $ runExecutionAcceptedReceiveFinish acceptedReceiveRepeatSecondComplete nextScript acceptedReceiveRepeatInlineState
            , testCase "receive second finish rejects the wrong successor" $ fails $ runExecutionAcceptedReceiveFinish acceptedReceiveRepeatSecondComplete stepScript acceptedReceiveRepeatInlineState
            , testCase "inline scan emits the authenticated evaluator source" $ succeeds $ runExecutionAcceptedInlineScan acceptedInlineReceiveAtOwn nextScript acceptedInlineAuthenticatedSource
            , testCase "inline scan rejects the wrong successor" $ fails $ runExecutionAcceptedInlineScan acceptedInlineReceiveAtOwn stepScript acceptedInlineAuthenticatedSource
            , testCase "inline finish advances to references" $ succeeds $ runExecutionAcceptedInline acceptedInlineAtOwnHash nextScript acceptedReferenceState
            , testCase "inline finish rejects a mutated checkpoint" $ fails $ runExecutionAcceptedInline (mutateAcceptedCheckpoint acceptedInlineAtOwnHash) nextScript acceptedReferenceState
            , testCase "reference scan advances past an output without a script" $ succeeds $ runExecutionAcceptedReferenceScan acceptedReferenceDescriptorNoScript acceptedReferenceDescriptorNoScript stepScript acceptedReferenceNoScriptState
            , testCase "reference scan rejects a missing membership withdrawal" $ fails runExecutionAcceptedReferenceWithoutMembership
            , testCase "reference scan rejects a descriptor output-index mismatch" $ fails $ runExecutionAcceptedReferenceScan acceptedReferenceDescriptorWrongIndex acceptedReferenceDescriptorWrongIndex stepScript (acceptedReferenceNoScriptStateFor acceptedReferenceDescriptorWrongIndex)
            , testCase "reference scan advances past a foreign script" $ succeeds $ runExecutionAcceptedReferenceScan acceptedReferenceDescriptorForeign acceptedReferenceDescriptorForeign stepScript acceptedReferenceForeignState
            , testCase "reference scan refuses a selected successor for a foreign script" $ fails $ runExecutionAcceptedReferenceScan acceptedReferenceDescriptorForeign acceptedReferenceDescriptorForeign nextScript acceptedReferenceForeignState
            , testCase "reference scan emits the authenticated evaluator source" $ succeeds $ runExecutionAcceptedReferenceScan acceptedReferenceDescriptorMatching acceptedReferenceDescriptorMatching nextScript acceptedReferenceAuthenticatedSource
            , testCase "reference scan accepts published-chunk membership" $ succeeds $ runExecutionAcceptedReferenceScanPublished acceptedPublishedChunks acceptedPublishedChunks
            , testCase "reference scan rejects substituted published chunks" $ fails $ runExecutionAcceptedReferenceScanPublished acceptedPublishedChunks [7]
            , testCase "reference scan rejects the wrong successor for a matching script" $ fails $ runExecutionAcceptedReferenceScan acceptedReferenceDescriptorMatching acceptedReferenceDescriptorMatching stepScript acceptedReferenceAuthenticatedSource
            ]
        ]

forcedSubject :: BS.ByteString -> Integer -> PD.Data
forcedSubject txId index =
    PD.Constr
        0
        [ PD.I 1
        , PD.I 1
        , PD.I 1
        , PD.B txId
        , PD.B (serialise $ inputData (hash32 0x77, 0))
        , PD.Constr 0 [PD.Constr 13 [PD.I index]]
        ]

subjectTerm :: PD.Data -> Term s PVerdictSubject
subjectTerm = pfromData . punsafeCoerce . pconstant @PData

signerCase :: Bool -> Bool -> Bool -> Term s PUnit
signerCase forced valid expected =
    pif
        ( pmatch (pauthenticatedSigner # subjectTerm subject # pconstant (BS.drop 3 (addressWitnessesPreimage (tx1{tWitnesses = [Witness 0 valid]})))) $ \case
            PJust hash -> pconstant expected #&& hash #== pconstant (keyHashFor 0)
            PNothing -> pconstant (not expected)
        )
        (pconstant ())
        perror
  where
    subject = if forced then forcedSubject tx1Id 0 else acceptedSubject tx1Id

indexCase :: Integer -> Integer -> Term s PUnit
indexCase committed selected =
    pif
        (pbindScriptIndex # subjectTerm (forcedSubject tx1Id committed) # pconstant selected)
        (pconstant ())
        perror

runForcedStep03 :: Bool -> BS.ByteString -> Term s PUnit
runForcedStep03 valid scriptItem = step03 $ finalizeContext (stepDatum $ Just state) redeemer
  where
    tx = tx1{tWitnesses = [Witness 0 valid]}
    state =
        PD.Constr
            0
            [ forcedSubject (txIdOf tx) 0
            , PD.B (txIdOf tx)
            , PD.B (witnessSetHashOf tx)
            , PD.B (blake2b256 scriptItem)
            , PD.I (tValidityStart tx)
            , PD.I (tValidityEnd tx)
            ]
    redeemer =
        PD.Constr
            1
            [ PD.Constr
                0
                [ PD.I 0
                , PD.I 0
                , PD.I 0
                , PD.B scriptItem
                , witnessOpening (compactOf tx) tx (addressWitnessesPreimage tx)
                ]
            ]

succeeds, fails :: (forall s. Term s PUnit) -> Assertion
succeeds = psucceedsNoTraceWithoutHoistChecks
fails = pfailsNoTraceWithoutHoistChecks

step03, step04, step05 :: forall s. ScriptContext -> Term s PUnit
step03 ctx =
    nativeScriptInvalidStep03Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant fpPolicy)
        # pdata (pconstant fraudProofAddress)
        # pdata (pconstant certificatePolicy)
        # pconstant ctx
step04 ctx =
    nativeScriptInvalidStep04Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant certificatePolicy)
        # pconstant ctx
step05 ctx =
    nativeScriptInvalidStep05Validator
        # pdata (pconstant ctPolicy)
        # pdata (pconstant fpPolicy)
        # pdata (pconstant fraudProofAddress)
        # pconstant ctx

runStep03 :: forall s. BS.ByteString -> Term s PUnit
runStep03 scriptItem = step03 $ finalizeContext (stepDatum $ Just $ step03StateFor scriptItem) directRedeemer
  where
    directRedeemer =
        PD.Constr
            1
            [ PD.Constr
                0
                [ PD.I 0
                , PD.I 0
                , PD.I 0
                , PD.B scriptItem
                , witnessOpening (compactOf tx1) tx1 (addressWitnessesPreimage tx1)
                ]
            ]

runStep05 :: forall s. BS.ByteString -> [PD.Data] -> Term s PUnit
runStep05 scriptItem queries = step05 $ finalizeContext (stepDatum $ Just $ step05State scriptItem readyPhase) redeemer
  where
    redeemer =
        PD.Constr
            1
            [ PD.Constr
                2
                [ PD.I 0
                , PD.I 0
                , PD.I 0
                , PD.B scriptItem
                , PD.I 1
                , PD.List queries
                ]
            ]

finalizeContext :: PD.Data -> PD.Data -> ScriptContext
finalizeContext datum redeemer =
    spendContext
        datum
        redeemer
        [threadInput]
        [convictionOutput fraudProofAddress threadName]
        []
        [fraudProofMintEntry threadName]
        (singleton fpPolicy (TokenName $ toBuiltin threadName) 1)

cancellationContext :: PD.Data -> Bool -> ScriptContext
cancellationContext state signedByProver =
    let context =
            spendContext
                (stepDatum $ Just state)
                cancelRedeemer
                [threadInput]
                []
                []
                [cancelMintEntry threadName]
                mempty
     in if signedByProver then context else withoutSignatories context

withoutSignatories :: ScriptContext -> ScriptContext
withoutSignatories (ScriptContext txInfo redeemer scriptInfo) =
    ScriptContext txInfo{txInfoSignatories = []} redeemer scriptInfo

acceptedSubject :: BS.ByteString -> PD.Data
acceptedSubject txId = PD.Constr 0 [PD.I 1, PD.I 0, PD.I 0, PD.B txId, PD.B "", PD.Constr 1 []]

step03StateFor :: BS.ByteString -> PD.Data
step03StateFor scriptItem =
    PD.Constr
        0
        [ acceptedSubject tx1Id
        , PD.B tx1Id
        , PD.B $ witnessSetHashOf tx1
        , PD.B $ blake2b256 scriptItem
        , PD.I $ tValidityStart tx1
        , PD.I $ tValidityEnd tx1
        ]

step03State :: PD.Data
step03State = step03StateFor unsatisfiedScriptItem

step04State :: PD.Data
step04State =
    PD.Constr
        0
        [ acceptedSubject tx1Id
        , PD.B tx1Id
        , PD.B $ witnessSetHashOf tx1
        , PD.B $ blake2b256 unsatisfiedScriptItem
        , PD.I $ tValidityStart tx1
        , PD.I $ tValidityEnd tx1
        , PD.B $ BS.replicate 32 0x33
        , PD.B ""
        , PD.I 0
        , PD.List []
        ]

step05State :: BS.ByteString -> PD.Data -> PD.Data
step05State scriptItem phase =
    PD.Constr
        0
        [ acceptedSubject tx1Id
        , PD.B tx1Id
        , PD.B $ blake2b256 scriptItem
        , PD.I (-1)
        , PD.I (-1)
        , PD.I 1
        , PD.List oneSignerPeaks
        , phase
        ]

readyPhase :: PD.Data
readyPhase = PD.Constr 0 []

presentSigner, absentSigner :: BS.ByteString
presentSigner = BS.replicate 28 0x11
absentSigner = BS.replicate 28 0xff

signaturePayload :: BS.ByteString -> BS.ByteString
signaturePayload signer = "\x82\x00\x58\x1c" <> signer

unsatisfiedScriptItem, satisfiedScriptItem, malformedScriptItem, absentScriptItem, presentScriptItem :: BS.ByteString
unsatisfiedScriptItem = versionedScriptItem 0 nativeScriptBytes
satisfiedScriptItem = versionedScriptItem 0 $ signaturePayload $ keyHashFor 0
malformedScriptItem = versionedScriptItem 0 "\xff"
absentScriptItem = versionedScriptItem 0 $ signaturePayload absentSigner
presentScriptItem = versionedScriptItem 0 $ signaturePayload presentSigner

oneSignerPeaks :: [PD.Data]
oneSignerPeaks = [PD.Constr 0 [PD.I 0, PD.B presentSignerLeaf]]

presentSignerLeaf :: BS.ByteString
presentSignerLeaf = blake2b256 $ "MidgardSignerLeafV1" <> "\x58\x1c" <> presentSigner

missingQuery, presentQuery, forgedMissingQuery :: PD.Data
missingQuery = signerQuery absentSigner $ PD.Constr 4 [PD.List oneSignerPeaks, PD.B presentSigner, PD.List []]
presentQuery = signerQuery presentSigner $ PD.Constr 1 [PD.List oneSignerPeaks, PD.I 0, PD.List []]
forgedMissingQuery =
    signerQuery absentSigner $
        PD.Constr 4 [PD.List oneSignerPeaks, PD.B $ BS.replicate 28 0x22, PD.List []]

signerQuery :: BS.ByteString -> PD.Data -> PD.Data
signerQuery signer proof = PD.Constr 0 [PD.B signer, proof]

startCompoundScan, resumeCompoundScan, mutatedCompoundCursor :: forall s. Term s PUnit
startCompoundScan =
    step05 $
        continueContext
            (step05State compoundScriptItem readyPhase)
            (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.B compoundScriptItem, PD.I 1, PD.List []]])
            (step05State compoundScriptItem $ walkPhase compoundFirstHash)
            stepScript
            []
resumeCompoundScan =
    step05 $
        continueContext
            (step05State compoundScriptItem $ walkPhase compoundFirstHash)
            ( PD.Constr
                1
                [ PD.Constr
                    1
                    [ PD.I 0
                    , PD.I 0
                    , PD.B compoundScriptItem
                    , PD.B compoundFirstCursor
                    , PD.List compoundFrames
                    , PD.I 1
                    , PD.List [compoundMissingQuery]
                    ]
                ]
            )
            (step05State compoundScriptItem $ walkPhase compoundSecondHash)
            stepScript
            []

cancelCase :: String -> (forall s. ScriptContext -> Term s PUnit) -> TestTree
cancelCase name validator =
    testGroup
        name
        [ testCase "accepts prover cancellation" $ succeeds $ validator $ cancellationContext executionCancellationState True
        , testCase "rejects unsigned cancellation" $ fails $ validator $ cancellationContext executionCancellationState False
        ]

executionCancellationState :: PD.Data
executionCancellationState = PD.Constr 0 []

executionStep01, executionStep02, executionStep03, executionStep04, executionStep05, executionStep06 :: forall s. ScriptContext -> Term s PUnit
executionStep01 ctx =
    Execution.executionNativeScriptInvalidStep01Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant hubOracleHash)
        # pconstant ctx
executionStep02 ctx =
    Execution.executionNativeScriptInvalidStep02Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pconstant ctx
executionStep03 ctx =
    Execution.executionNativeScriptInvalidStep03Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pconstant ctx
executionStep04 ctx =
    Execution.executionNativeScriptInvalidStep04Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant fpPolicy)
        # pdata (pconstant fraudProofAddress)
        # pdata (pconstant certificatePolicy)
        # pconstant ctx
executionStep06 ctx =
    Execution.executionNativeScriptInvalidStep06Validator
        # pdata (pconstant ctPolicy)
        # pdata (pconstant fpPolicy)
        # pdata (pconstant fraudProofAddress)
        # pconstant ctx
executionStep05 ctx =
    Execution.executionNativeScriptInvalidStep05Validator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant certificatePolicy)
        # pconstant ctx

executionAcceptedInit, executionAcceptedSpend, executionAcceptedMint, executionAcceptedObserver, executionAcceptedReceive, executionAcceptedInline, executionAcceptedReference :: forall s. ScriptContext -> Term s PUnit
executionAcceptedInit ctx =
    Execution.executionNativeScriptInvalidAcceptedReconstructionInitValidator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pconstant ctx
executionAcceptedSpend ctx =
    Execution.executionNativeScriptInvalidAcceptedSpendPrefixValidator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant certificatePolicy)
        # pconstant ctx
executionAcceptedMint ctx =
    Execution.executionNativeScriptInvalidAcceptedMintPrefixValidator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant certificatePolicy)
        # pconstant ctx
executionAcceptedObserver ctx =
    Execution.executionNativeScriptInvalidAcceptedObserverPrefixValidator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant certificatePolicy)
        # pconstant ctx
executionAcceptedReceive ctx =
    Execution.executionNativeScriptInvalidAcceptedReceivePrefixValidator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant certificatePolicy)
        # pconstant ctx
executionAcceptedInline ctx =
    Execution.executionNativeScriptInvalidAcceptedInlineSourceValidator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant certificatePolicy)
        # pconstant ctx
executionAcceptedReference ctx =
    Execution.executionNativeScriptInvalidAcceptedReferenceSourceValidator
        # pdata (pconstant $ ScriptHash $ toBuiltin nextScript)
        # pdata (pconstant ctPolicy)
        # pdata (pconstant certificatePolicy)
        # pconstant ctx

runExecutionStep01Accepted :: forall s. BS.ByteString -> PD.Data -> Term s PUnit
runExecutionStep01Accepted successor outputState =
    executionStep01 $
        spendContext
            (stepDatum Nothing)
            ( PD.Constr
                1
                [ PD.Constr
                    0
                    [ PD.Constr
                        0
                        [inclusionArgs tx1Id executionStep01SourceCbor executionStep01TransactionsPhasRoot]
                    , PD.I 0
                    ]
                ]
            )
            [threadInput]
            [stepOutput successor $ Just outputState]
            executionStep01ReferenceInputs
            [phasEntry executionStep01TransactionsPhasRoot tx1Id executionStep01SourceCbor]
            mempty

executionStep01BoundState, executionStep01SubstitutedState, executionStep01Header :: PD.Data
executionStep01BoundState =
    PD.Constr
        0
        [ acceptedSubject tx1Id
        , PD.B executionStep01ValidationRoot
        , PD.I 1
        , PD.I 0
        , PD.I (-1)
        , PD.B executionStep01PriorRoot
        , PD.B executionStep01Compact
        ]
executionStep01SubstitutedState =
    case executionStep01BoundState of
        PD.Constr tag fields -> PD.Constr tag $ take 5 fields <> [PD.B $ hash32 0xfa] <> drop 6 fields
        _ -> error "unexpected execution step-01 state"
executionStep01Header =
    PD.Constr
        0
        [ PD.B executionStep01PriorRoot
        , PD.B $ hash32 0x02
        , PD.B $ hash32 0x03
        , PD.B $ hash32 0x04
        , PD.B executionStep01TransactionsRoot
        , PD.B $ hash32 0x06
        , PD.B $ hash32 0x07
        , PD.B $ hash32 0x08
        , PD.B executionStep01ValidationRoot
        , PD.I 0
        , PD.I 0
        , PD.I l2Count
        , PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.I 1
        , PD.I 100
        , PD.I 200
        , PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.B $ BS.replicate 28 0x02
        , PD.B prover
        , PD.I 1
        ]

executionStep01SourceCbor, executionStep01Compact, executionStep01TransactionsPhasRoot, executionStep01TransactionsRoot, executionStep01ValidationRoot, executionStep01PriorRoot :: BS.ByteString
executionStep01SourceCbor = sourceCborWithValidity tx1 0
executionStep01Compact = compactWithValidity tx1 (witnessSetHashOf tx1) 0
executionStep01TransactionsPhasRoot = singleEntryPhasRoot tx1Id executionStep01SourceCbor
executionStep01TransactionsRoot = commitCountedRoot transactionsDomain executionStep01TransactionsPhasRoot l2Count
executionStep01ValidationRoot = hash32 0x09
executionStep01PriorRoot = hash32 0x01

executionStep01ReferenceInputs :: [TxInInfo]
executionStep01ReferenceInputs =
    case referenceInputsWithTransactionsRoot executionStep01TransactionsRoot of
        [hub, TxInInfo ref (TxOut address value _ referenceScript)] ->
            [ hub
            , TxInInfo
                ref
                ( TxOut
                    address
                    value
                    (OutputDatum $ Datum $ dataToBuiltinData executionStep01Element)
                    referenceScript
                )
            ]
        _ -> error "unexpected execution step-01 reference fixture"
  where
    executionStep01Element = PD.Constr 0 [PD.Constr 1 [PD.Constr 0 [executionStep01Header, PD.B ""]], PD.Constr 1 []]

runExecutionStep01Forced :: forall s. Integer -> Integer -> PD.Data -> Integer -> Term s PUnit
runExecutionStep01Forced direction validity verdict executionIndex =
    executionStep01 $
        spendContext
            (stepDatum Nothing)
            ( PD.Constr
                1
                [ PD.Constr
                    0
                    [ PD.Constr 1 [PD.I 0, PD.I 0, header, membership, PD.I direction]
                    , PD.I executionIndex
                    ]
                ]
            )
            [threadInputWithName threadAssetName]
            [stepOutputWithName nextScript (Just expectedState) threadAssetName]
            []
            []
            mempty
  where
    source =
        PD.Constr
            0
            [ PD.B $ compactWithValidity tx1 (witnessSetHashOf tx1) validity
            , PD.B $ witnessSetCborOf tx1
            , PD.B $ fieldPreimageLengthsCborOf tx1
            ]
    leaf = PD.Constr 0 [PD.B tx1Id, source, verdict]
    rawRoot = singleEntryPhasRoot (serialise executionStep01ForcedKey) (serialise leaf)
    forcedRoot = commitCountedRoot forcedTransactionsDomain rawRoot 1
    membership = membershipProof forcedTransactionsDomain forcedRoot rawRoot 1 executionStep01ForcedKey leaf
    header = executionStep01ForcedHeader forcedRoot
    threadAssetName = BS.pack [0, 0, 0, 5] <> blake2b224 (serialise header)
    rejectionReason = case verdict of
        PD.Constr 1 [reason] -> PD.Constr 0 [reason]
        _ -> PD.Constr 1 []
    subject =
        PD.Constr
            0
            [ PD.I 1
            , PD.I direction
            , PD.I 1
            , PD.B tx1Id
            , PD.B $ serialise executionStep01ForcedKey
            , rejectionReason
            ]
    expectedState =
        PD.Constr
            0
            [ subject
            , PD.B executionStep01ValidationRoot
            , PD.I 1
            , PD.I executionIndex
            , PD.I (-1)
            , PD.B executionStep01PriorRoot
            , PD.B $ compactWithValidity tx1 (witnessSetHashOf tx1) validity
            ]

executionStep01ForcedKey :: PD.Data
executionStep01ForcedKey = PD.Constr 0 [PD.B $ hash32 0x71, PD.I 0]

executionStep01ForcedHeader :: BS.ByteString -> PD.Data
executionStep01ForcedHeader forcedRoot =
    PD.Constr
        0
        [ PD.B executionStep01PriorRoot
        , PD.B $ hash32 0x02
        , PD.B $ hash32 0x03
        , PD.B forcedRoot
        , PD.B $ hash32 0x05
        , PD.B $ hash32 0x06
        , PD.B $ hash32 0x07
        , PD.B $ hash32 0x08
        , PD.B executionStep01ValidationRoot
        , PD.I 0
        , PD.I 1
        , PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.I 1
        , PD.I 100
        , PD.I 200
        , PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.B $ BS.replicate 28 0x02
        , PD.B prover
        , PD.I 1
        ]

runExecutionStep02 :: forall s. PD.Data -> BS.ByteString -> PD.Data -> Term s PUnit
runExecutionStep02 control successor outputState =
    executionStep02 $
        continueContext
            executionStep02BoundState
            ( PD.Constr
                1
                [ PD.Constr
                    0
                    [ PD.I 0
                    , PD.I 0
                    , executionStep02Membership
                    , executionStep02MachineState
                    , PD.Constr 0 [PD.I 0, PD.B executionStep02StateHash, PD.List []]
                    , control
                    , PD.I 0
                    , PD.I 0
                    , PD.B executionStep02ScriptHash
                    , PD.B executionStep02PurposeSubject
                    , PD.List []
                    , PD.I 0
                    , PD.I 0
                    , PD.B executionStep02SourceKey
                    , PD.I 0
                    , PD.I $ fromIntegral $ BS.length executionStep02Item
                    , PD.B executionStep02ItemCommitment
                    , PD.List []
                    , PD.B ""
                    , PD.List []
                    ]
                ]
            )
            outputState
            successor
            []

executionStep02BoundState, executionStep02AuthenticatedSource, executionStep02Membership, executionStep02MachineState :: PD.Data
executionStep02BoundState =
    PD.Constr
        0
        [ acceptedSubject tx1Id
        , PD.B executionStep02ValidationRoot
        , PD.I 1
        , PD.I 0
        , PD.I (-1)
        , PD.B executionStep02H32
        , PD.B executionStep01Compact
        ]
executionStep02AuthenticatedSource =
    PD.Constr
        0
        [ executionStep02BoundState
        , PD.B executionStep02H32
        , PD.I 0
        , PD.I 0
        , PD.B executionStep02SourceKey
        , PD.I 0
        , PD.B executionStep02ScriptHash
        , PD.I $ fromIntegral $ BS.length executionStep02Item
        , PD.B executionStep02ItemCommitment
        , PD.B executionStep01Compact
        ]
executionStep02Membership =
    membershipProof
        6
        executionStep02ValidationRoot
        executionStep02ValidationPhasRoot
        1
        executionStep02EventKey
        executionStep02Descriptor
executionStep02MachineState =
    PD.Constr
        0
        [ PD.I 1
        , PD.B executionStep02EventKeyHash
        , PD.B tx1Id
        , PD.B executionStep02H32
        , PD.B executionStep02H32
        , PD.Constr 0 []
        , PD.B executionStep02H32
        , PD.Constr 9 []
        , PD.I 7
        , PD.B executionStep02WorkRoot
        , PD.I 0
        , PD.I 0
        , PD.Constr 0 []
        , PD.B executionStep02NoRejection
        , PD.B executionStep02H32
        ]

executionStep02Control, executionStep02SubstitutedControl :: PD.Data
executionStep02Control = executionStep02ControlFor executionStep02SourceLeaf
executionStep02SubstitutedControl = executionStep02ControlFor $ hash32 0xfa

executionStep02ControlFor :: BS.ByteString -> PD.Data
executionStep02ControlFor sourceLeaf =
    PD.Constr
        0
        [ PD.B executionStep01Compact
        , PD.B ""
        , PD.B ""
        , PD.B ""
        , PD.I 0
        , PD.B executionStep02H32
        , PD.I 0
        , PD.List []
        , PD.I 0
        , PD.B executionStep02H32
        , PD.I 1
        , PD.List [frontierPeak sourceLeaf]
        , PD.I 0
        , PD.List []
        , PD.I 1
        , PD.List [frontierPeak executionStep02PurposeLeaf]
        , PD.I 0
        , PD.List []
        , PD.List []
        , PD.I 0
        , PD.List []
        , PD.I 1
        , PD.List [frontierPeak executionStep02ExecutionLeaf]
        , PD.I 0
        , PD.I 0
        , PD.B executionStep02H32
        ]
  where
    frontierPeak leaf = PD.Constr 0 [PD.I 0, PD.B leaf]

executionStep02EventKey, executionStep02Descriptor :: PD.Data
executionStep02EventKey = PD.Constr 2 [PD.B tx1Id]
executionStep02Descriptor =
    PD.Constr
        0
        [ PD.I 1
        , PD.I 1
        , PD.B executionStep02TraceRoot
        , PD.I 0
        , PD.B executionStep02StateHash
        , PD.B executionStep02StateHash
        , PD.Constr 1 []
        , PD.B executionStep02NoRejection
        ]

executionStep02H32, executionStep02NoRejection, executionStep02ScriptHash, executionStep02PurposeSubject, executionStep02SourceKey, executionStep02Item :: BS.ByteString
executionStep02H32 = BS.replicate 32 0x11
executionStep02NoRejection = BS.replicate 32 0
executionStep02ScriptHash = BS.replicate 28 0x22
executionStep02PurposeSubject = BS.singleton 0xaa
executionStep02SourceKey = BS.singleton 0
executionStep02Item = satisfiedScriptItem

executionStep02PurposeLeaf, executionStep02SourceLeaf, executionStep02ExecutionLeaf, executionStep02ItemCommitment :: BS.ByteString
executionStep02PurposeLeaf =
    blake2b256 $
        "MidgardScriptPurposeLeafV1"
            <> cborInt 0
            <> cborInt 0
            <> cborBytes executionStep02ScriptHash
            <> cborBytes executionStep02PurposeSubject
executionStep02SourceLeaf =
    blake2b256 $
        "MidgardInlineScriptSourceLeafV1"
            <> cborInt 0
            <> cborInt 0
            <> cborBytes executionStep02ScriptHash
            <> cborInt (fromIntegral $ BS.length executionStep02Item)
            <> cborBytes executionStep02ItemCommitment
executionStep02ExecutionLeaf =
    blake2b256 $
        "MidgardScriptExecutionLeafV1"
            <> cborInt 0
            <> cborBytes executionStep02PurposeLeaf
            <> cborBytes executionStep02SourceLeaf
            <> cborBytes ""
executionStep02ItemCommitment =
    blake2b256 $
        "MidgardBoundedItemCommitmentV1"
            <> arrayHeader 5
            <> cborInt 1
            <> cborInt 6
            <> cborInt 0
            <> cborInt (fromIntegral $ BS.length executionStep02Item)
            <> cborBytes executionStep02ItemFrontierCommitment

executionStep02ItemChunkHash, executionStep02ItemFrontierCommitment :: BS.ByteString
executionStep02ItemChunkHash =
    blake2b256 $
        "MidgardBoundedItemChunkV1"
            <> arrayHeader 5
            <> cborInt 1
            <> cborInt 6
            <> cborInt 0
            <> cborInt 0
            <> cborBytes executionStep02Item
executionStep02ItemFrontierCommitment =
    blake2b256 $
        "MidgardValidationMerkleFrontierV1"
            <> serialise (PD.I 1)
            <> singletonFrontierCbor executionStep02ItemChunkHash

executionStep02ControlCbor, executionStep02WorkRoot, executionStep02EventKeyHash, executionStep02StateHash, executionStep02TraceRoot, executionStep02ValidationPhasRoot, executionStep02ValidationRoot :: BS.ByteString
executionStep02ControlCbor =
    arrayHeader 26
        <> cborBytes executionStep01Compact
        <> cborBytes ""
        <> cborBytes ""
        <> cborBytes ""
        <> cborInt 0
        <> cborBytes executionStep02H32
        <> cborInt 0
        <> arrayHeader 0
        <> cborInt 0
        <> cborBytes executionStep02H32
        <> cborInt 1
        <> singletonFrontierCbor executionStep02SourceLeaf
        <> cborInt 0
        <> arrayHeader 0
        <> cborInt 1
        <> singletonFrontierCbor executionStep02PurposeLeaf
        <> cborInt 0
        <> arrayHeader 0
        <> arrayHeader 0
        <> cborInt 0
        <> arrayHeader 0
        <> cborInt 1
        <> singletonFrontierCbor executionStep02ExecutionLeaf
        <> cborInt 0
        <> cborInt 0
        <> cborBytes executionStep02H32
executionStep02WorkRoot =
    blake2b256 $
        "MidgardValidationWorkWitnessV1"
            <> arrayHeader 3
            <> cborInt 9
            <> cborInt 7
            <> serialise (PD.B executionStep02ControlCbor)
executionStep02EventKeyHash = blake2b256 $ serialise executionStep02EventKey
executionStep02StateHash =
    blake2b256 $
        "MidgardValidationMachineStateV1"
            <> arrayHeader 15
            <> cborInt 1
            <> cborBytes executionStep02EventKeyHash
            <> cborBytes tx1Id
            <> cborBytes executionStep02H32
            <> cborBytes executionStep02H32
            <> cborInt 0
            <> cborBytes executionStep02H32
            <> cborInt 9
            <> cborInt 7
            <> cborBytes executionStep02WorkRoot
            <> cborInt 0
            <> cborInt 0
            <> cborInt 0
            <> cborBytes executionStep02NoRejection
            <> cborBytes executionStep02H32
executionStep02TraceRoot = blake2b256 $ "MidgardValidationTraceLeafV1" <> executionStep02StateHash
executionStep02ValidationPhasRoot = singleEntryPhasRoot (serialise executionStep02EventKey) (serialise executionStep02Descriptor)
executionStep02ValidationRoot = commitCountedRoot 6 executionStep02ValidationPhasRoot 1

singletonFrontierCbor :: BS.ByteString -> BS.ByteString
singletonFrontierCbor leaf = arrayHeader 1 <> arrayHeader 2 <> cborInt 0 <> cborBytes leaf

cborBytes :: BS.ByteString -> BS.ByteString
cborBytes bytes
    | len <= 23 = BS.cons (fromIntegral $ 0x40 + len) bytes
    | len <= 255 = BS.pack [0x58, fromIntegral len] <> bytes
    | len <= 65_535 = BS.pack [0x59, fromIntegral $ len `div` 256, fromIntegral len] <> bytes
    | otherwise = error "test fixture byte string exceeds the supported CBOR width"
  where
    len = BS.length bytes

runExecutionAcceptedInit :: forall s. BS.ByteString -> PD.Data -> Term s PUnit
runExecutionAcceptedInit successor outputState =
    executionAcceptedInit $
        continueContext
            acceptedBoundState
            (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0]])
            outputState
            successor
            []

runExecutionAcceptedSpend :: forall s. PD.Data -> BS.ByteString -> PD.Data -> Term s PUnit
runExecutionAcceptedSpend inputState successor outputState =
    executionAcceptedSpend $
        continueContext
            inputState
            ( PD.Constr
                1
                [ PD.Constr
                    1
                    [ PD.I 0
                    , PD.I 0
                    , bodyOpening acceptedCompact (spendInputsPreimage tx1)
                    ]
                ]
            )
            outputState
            successor
            []

runExecutionAcceptedSpendScan :: forall s. BS.ByteString -> BS.ByteString -> BS.ByteString -> Term s PUnit
runExecutionAcceptedSpendScan descriptor attestedDescriptor successor =
    executionAcceptedSpend $
        continueContextWithRedeemers
            acceptedSpendScanState
            (acceptedSpendScanRedeemer descriptor)
            acceptedSpendSelectedState
            successor
            []
            [phasEntry acceptedSpendPriorRoot acceptedSpendKey attestedDescriptor]

runExecutionAcceptedSpendScanPublished :: forall s. [Integer] -> [Integer] -> Term s PUnit
runExecutionAcceptedSpendScanPublished carriageChunks claimChunks =
    executionAcceptedSpend $
        continueContextWithRedeemers
            acceptedSpendScanState
            (acceptedSpendScanRedeemerWith acceptedSpendDescriptor $ acceptedPublishedMembership carriageChunks)
            acceptedSpendSelectedState
            nextScript
            []
            [acceptedPublishedMembershipEntry acceptedSpendPriorRoot acceptedSpendKey acceptedSpendDescriptor claimChunks]

runExecutionAcceptedSpendScanWithoutMembership :: forall s. Term s PUnit
runExecutionAcceptedSpendScanWithoutMembership =
    executionAcceptedSpend $
        continueContext
            acceptedSpendScanState
            (acceptedSpendScanRedeemer acceptedSpendDescriptor)
            acceptedSpendSelectedState
            nextScript
            []

acceptedSpendScanRedeemer :: BS.ByteString -> PD.Data
acceptedSpendScanRedeemer descriptor =
    acceptedSpendScanRedeemerWith descriptor acceptedRedeemerCarriedMembership

acceptedSpendScanRedeemerWith :: BS.ByteString -> PD.Data -> PD.Data
acceptedSpendScanRedeemerWith descriptor membership =
    PD.Constr
        1
        [ PD.Constr
            0
            [ PD.I 0
            , PD.I 0
            , bodyOpening acceptedCompact (spendInputsPreimage tx1)
            , PD.B descriptor
            , membership
            ]
        ]

runExecutionAcceptedMint :: forall s. PD.Data -> BS.ByteString -> PD.Data -> Term s PUnit
runExecutionAcceptedMint inputState successor outputState =
    executionAcceptedMint $
        continueContext
            inputState
            (PD.Constr 1 [PD.Constr 1 [PD.I 0, PD.I 0, bodyOpening acceptedCompact acceptedMintPreimage]])
            outputState
            successor
            []

runExecutionAcceptedMintScan :: forall s. PD.Data -> BS.ByteString -> PD.Data -> Term s PUnit
runExecutionAcceptedMintScan inputState successor outputState =
    executionAcceptedMint $
        continueContext
            inputState
            (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, bodyOpening acceptedCompact acceptedMintPreimage]])
            outputState
            successor
            []

runExecutionAcceptedObserver :: forall s. PD.Data -> BS.ByteString -> PD.Data -> Term s PUnit
runExecutionAcceptedObserver inputState successor outputState =
    executionAcceptedObserver $
        continueContext
            inputState
            (PD.Constr 1 [PD.Constr 1 [PD.I 0, PD.I 0, bodyOpening acceptedCompact acceptedObserverPreimage]])
            outputState
            successor
            []

runExecutionAcceptedObserverScan :: forall s. PD.Data -> BS.ByteString -> PD.Data -> Term s PUnit
runExecutionAcceptedObserverScan inputState successor outputState =
    executionAcceptedObserver $
        continueContext
            inputState
            (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, bodyOpening acceptedCompact acceptedObserverPreimage]])
            outputState
            successor
            []

runExecutionAcceptedReceiveScan :: forall s. PD.Data -> BS.ByteString -> PD.Data -> Term s PUnit
runExecutionAcceptedReceiveScan inputState successor outputState =
    executionAcceptedReceive $
        continueContext
            inputState
            (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, bodyOpening acceptedCompact acceptedOutputsPreimage]])
            outputState
            successor
            []

runExecutionAcceptedReceiveFinish :: forall s. PD.Data -> BS.ByteString -> PD.Data -> Term s PUnit
runExecutionAcceptedReceiveFinish inputState successor outputState =
    executionAcceptedReceive $
        continueContext
            inputState
            (PD.Constr 1 [PD.Constr 1 [PD.I 0, PD.I 0, bodyOpening acceptedCompact acceptedOutputsPreimage]])
            outputState
            successor
            []

runExecutionAcceptedInline :: forall s. PD.Data -> BS.ByteString -> PD.Data -> Term s PUnit
runExecutionAcceptedInline inputState successor outputState =
    executionAcceptedInline $
        continueContext
            inputState
            ( PD.Constr
                1
                [ PD.Constr
                    1
                    [ PD.I 0
                    , PD.I 0
                    , witnessOpening acceptedCompact acceptedTx (scriptWitnessesPreimage acceptedTx)
                    ]
                ]
            )
            outputState
            successor
            []

runExecutionAcceptedInlineScan :: forall s. PD.Data -> BS.ByteString -> PD.Data -> Term s PUnit
runExecutionAcceptedInlineScan inputState successor outputState =
    executionAcceptedInline $
        continueContext
            inputState
            ( PD.Constr
                1
                [ PD.Constr
                    0
                    [ PD.I 0
                    , PD.I 0
                    , witnessOpening acceptedCompact acceptedTx (scriptWitnessesPreimage acceptedTx)
                    ]
                ]
            )
            outputState
            successor
            []

runExecutionAcceptedReferenceScan :: forall s. BS.ByteString -> BS.ByteString -> BS.ByteString -> PD.Data -> Term s PUnit
runExecutionAcceptedReferenceScan descriptor attestedDescriptor successor outputState =
    executionAcceptedReference $
        continueContextWithRedeemers
            (acceptedReferenceScanState descriptor)
            (acceptedReferenceRedeemer descriptor)
            outputState
            successor
            []
            [phasEntry (acceptedReferencePriorRoot descriptor) acceptedSpendKey attestedDescriptor]

runExecutionAcceptedReferenceScanPublished :: forall s. [Integer] -> [Integer] -> Term s PUnit
runExecutionAcceptedReferenceScanPublished carriageChunks claimChunks =
    executionAcceptedReference $
        continueContextWithRedeemers
            (acceptedReferenceScanState acceptedReferenceDescriptorMatching)
            (acceptedReferenceRedeemerWith acceptedReferenceDescriptorMatching $ acceptedPublishedMembership carriageChunks)
            acceptedReferenceAuthenticatedSource
            nextScript
            []
            [ acceptedPublishedMembershipEntry
                (acceptedReferencePriorRoot acceptedReferenceDescriptorMatching)
                acceptedSpendKey
                acceptedReferenceDescriptorMatching
                claimChunks
            ]

runExecutionAcceptedReferenceWithoutMembership :: forall s. Term s PUnit
runExecutionAcceptedReferenceWithoutMembership =
    executionAcceptedReference $
        continueContext
            (acceptedReferenceScanState acceptedReferenceDescriptorNoScript)
            (acceptedReferenceRedeemer acceptedReferenceDescriptorNoScript)
            acceptedReferenceNoScriptState
            stepScript
            []

acceptedReferenceRedeemer :: BS.ByteString -> PD.Data
acceptedReferenceRedeemer descriptor =
    acceptedReferenceRedeemerWith descriptor acceptedRedeemerCarriedMembership

acceptedReferenceRedeemerWith :: BS.ByteString -> PD.Data -> PD.Data
acceptedReferenceRedeemerWith descriptor membership =
    PD.Constr
        1
        [ PD.Constr
            0
            [ PD.I 0
            , PD.I 0
            , bodyOpening acceptedCompact (referenceInputsPreimage tx1)
            , PD.B descriptor
            , membership
            ]
        ]

acceptedBoundState :: PD.Data
acceptedBoundState = acceptedBoundStateFor 0

acceptedBoundStateFor :: Integer -> PD.Data
acceptedBoundStateFor = acceptedBoundStateForRoot acceptedDefaultPriorRoot

acceptedBoundStateForRoot :: BS.ByteString -> Integer -> PD.Data
acceptedBoundStateForRoot priorRoot executionIndex =
    PD.Constr
        0
        [ acceptedSubject acceptedTxId
        , PD.B $ BS.replicate 32 0x41
        , PD.I 1
        , PD.I executionIndex
        , PD.I (-1)
        , PD.B priorRoot
        , PD.B acceptedCompact
        ]

acceptedInitialState, acceptedInitialAtOwnHash, acceptedSpendCompleteState, acceptedSpendScanState, acceptedSpendSelectedState, acceptedMintState, acceptedMintScanState, acceptedMintSelectedState, acceptedMintBeforeTarget, acceptedMintAfterEarlier, acceptedMintAtOwnHash, acceptedObserverState, acceptedObserverScanState, acceptedObserverSelectedState, acceptedObserverBeforeTarget, acceptedObserverAfterEarlier, acceptedObserverAtOwnHash, acceptedReceiveState, acceptedReceiveAtOwnHash, acceptedReceiveAfterFirst, acceptedReceiveAfterSecond, acceptedReceiveAfterThird, acceptedReceiveEmptyComplete, acceptedReceiveRepeatFirstComplete, acceptedReceiveRepeatSecondStart, acceptedReceiveRepeatSecondAfterFirst, acceptedReceiveRepeatSecondAfterSecond, acceptedReceiveRepeatSecondComplete, acceptedReceiveRepeatInlineState, acceptedInlineState, acceptedInlineReceiveAtOwn, acceptedInlineAtOwnHash, acceptedReferenceState, acceptedReferenceNoScriptState, acceptedReferenceForeignState, acceptedInlineAuthenticatedSource, acceptedReferenceAuthenticatedSource :: PD.Data
acceptedInitialState = acceptedReconstructionState 0 0 nextScript
acceptedInitialAtOwnHash = acceptedReconstructionState 0 0 stepScript
acceptedSpendCompleteState = acceptedReconstructionState 0 (fromIntegral $ length $ tSpendInputs tx1) stepScript
acceptedSpendScanState = acceptedReconstructionStateFullForRoot acceptedSpendPriorRoot 0 0 0 0 "" "" 0 0 acceptedNothing acceptedNothing stepScript
acceptedSpendSelectedState = acceptedReconstructionStateFullForRoot acceptedSpendPriorRoot 0 4 0 1 "" "" 0 0 acceptedSpendPurpose acceptedNothing nextScript
acceptedMintState = acceptedReconstructionState 1 0 nextScript
acceptedMintScanState = acceptedReconstructionState 1 0 stepScript
acceptedMintSelectedState = acceptedReconstructionStateFull 4 0 1 "" "" 0 0 acceptedMintPurpose acceptedNothing nextScript
acceptedMintBeforeTarget = acceptedReconstructionStateFullFor 1 1 0 0 "" "" 0 0 acceptedNothing acceptedNothing stepScript
acceptedMintAfterEarlier = acceptedReconstructionStateFullFor 1 1 1 1 acceptedMintPolicy "" 0 0 acceptedNothing acceptedNothing stepScript
acceptedMintAtOwnHash = acceptedReconstructionState 1 1 stepScript
acceptedObserverState = acceptedReconstructionState 2 0 nextScript
acceptedObserverScanState = acceptedReconstructionState 2 0 stepScript
acceptedObserverSelectedState = acceptedReconstructionStateFull 4 0 1 "" "" 0 0 acceptedObserverPurpose acceptedNothing nextScript
acceptedObserverBeforeTarget = acceptedReconstructionStateFullFor 1 2 0 0 "" "" 0 0 acceptedNothing acceptedNothing stepScript
acceptedObserverAfterEarlier = acceptedReconstructionStateFullFor 1 2 1 1 acceptedObserver "" 0 0 acceptedNothing acceptedNothing stepScript
acceptedObserverAtOwnHash = acceptedReconstructionState 2 1 stepScript
acceptedReceiveState = acceptedReconstructionState 3 0 nextScript
acceptedReceiveAtOwnHash = acceptedReconstructionState 3 0 stepScript
acceptedReceiveAfterFirst = acceptedReconstructionStateFull 3 1 0 "" lockedScriptHash 0 0 acceptedNothing acceptedNothing stepScript
acceptedReceiveAfterSecond = acceptedReconstructionStateFull 3 2 0 "" lockedScriptHash 0 0 acceptedNothing acceptedNothing stepScript
acceptedReceiveAfterThird = acceptedReconstructionStateFull 3 3 0 "" lockedScriptHash 0 0 acceptedNothing acceptedNothing stepScript
acceptedReceiveEmptyComplete = acceptedReconstructionStateFull 3 3 0 "" "" 0 0 acceptedNothing acceptedNothing stepScript
acceptedReceiveRepeatFirstComplete = acceptedReconstructionStateFullFor 1 3 3 0 "" lockedScriptHash 0 0 acceptedNothing acceptedNothing stepScript
acceptedReceiveRepeatSecondStart = acceptedReconstructionStateFullFor 1 3 0 1 lockedScriptHash "" 0 0 acceptedNothing acceptedNothing stepScript
acceptedReceiveRepeatSecondAfterFirst = acceptedReconstructionStateFullFor 1 3 1 1 lockedScriptHash "" 0 0 acceptedNothing acceptedNothing stepScript
acceptedReceiveRepeatSecondAfterSecond = acceptedReconstructionStateFullFor 1 3 2 1 lockedScriptHash acceptedSecondScriptHash 0 0 acceptedNothing acceptedNothing stepScript
acceptedReceiveRepeatSecondComplete = acceptedReconstructionStateFullFor 1 3 3 1 lockedScriptHash acceptedSecondScriptHash 0 0 acceptedNothing acceptedNothing stepScript
acceptedReceiveRepeatInlineState = acceptedReconstructionStateFullFor 1 4 0 2 "" "" 0 0 acceptedReceivePurpose2 acceptedNothing nextScript
acceptedInlineState = acceptedReconstructionStateFull 4 0 1 "" "" 0 0 acceptedReceivePurpose acceptedNothing nextScript
acceptedInlineReceiveAtOwn = acceptedReconstructionStateFull 4 0 1 "" "" 0 0 acceptedReceivePurpose acceptedNothing stepScript
acceptedInlineAtOwnHash = acceptedReconstructionStateWith 4 1 acceptedSelectedPurpose acceptedNothing stepScript
acceptedReferenceState = acceptedReconstructionStateWith 5 0 acceptedSelectedPurpose acceptedNothing nextScript
acceptedReferenceNoScriptState = acceptedReferenceNoScriptStateFor acceptedReferenceDescriptorNoScript
acceptedReferenceForeignState = acceptedReconstructionStateFullForRoot (acceptedReferencePriorRoot acceptedReferenceDescriptorForeign) 0 5 1 0 "" "" 0 1 acceptedSpendPurpose acceptedNothing stepScript
acceptedInlineAuthenticatedSource =
    PD.Constr
        0
        [ acceptedBoundState
        , PD.B $ BS.replicate 32 0x42
        , PD.I 0
        , PD.I 0
        , PD.B $ serialise $ PD.I 0
        , PD.I 0
        , PD.B lockedScriptHash
        , PD.I $ fromIntegral $ BS.length acceptedInlineItem
        , PD.B $ boundedItemCommitment 6 0 acceptedInlineItem
        , PD.B acceptedCompact
        ]
acceptedReferenceAuthenticatedSource =
    PD.Constr
        0
        [ acceptedBoundStateForRoot (acceptedReferencePriorRoot acceptedReferenceDescriptorMatching) 0
        , PD.B $ acceptedReferencePriorRoot acceptedReferenceDescriptorMatching
        , PD.I 0
        , PD.I 1
        , PD.B acceptedSpendKey
        , PD.I 0
        , PD.B lockedScriptHash
        , PD.I $ fromIntegral $ BS.length acceptedInlineItem
        , PD.B acceptedReferenceScriptCommitment
        , PD.B acceptedCompact
        ]

acceptedReferenceScanState :: BS.ByteString -> PD.Data
acceptedReferenceScanState descriptor =
    acceptedReconstructionStateFullForRoot
        (acceptedReferencePriorRoot descriptor)
        0
        5
        0
        0
        ""
        ""
        0
        0
        acceptedSpendPurpose
        acceptedNothing
        stepScript

acceptedReferenceNoScriptStateFor :: BS.ByteString -> PD.Data
acceptedReferenceNoScriptStateFor descriptor =
    acceptedReconstructionStateFullForRoot
        (acceptedReferencePriorRoot descriptor)
        0
        5
        1
        0
        ""
        ""
        0
        0
        acceptedSpendPurpose
        acceptedNothing
        stepScript

acceptedReconstructionState :: Integer -> Integer -> BS.ByteString -> PD.Data
acceptedReconstructionState phase fieldCursor = acceptedReconstructionStateWith phase fieldCursor acceptedNothing acceptedNothing

acceptedReconstructionStateWith :: Integer -> Integer -> PD.Data -> PD.Data -> BS.ByteString -> PD.Data
acceptedReconstructionStateWith phase fieldCursor = acceptedReconstructionStateFull phase fieldCursor 0 "" "" 0 0

acceptedReconstructionStateFull :: Integer -> Integer -> Integer -> BS.ByteString -> BS.ByteString -> Integer -> Integer -> PD.Data -> PD.Data -> BS.ByteString -> PD.Data
acceptedReconstructionStateFull = acceptedReconstructionStateFullFor 0

acceptedReconstructionStateFullFor :: Integer -> Integer -> Integer -> Integer -> BS.ByteString -> BS.ByteString -> Integer -> Integer -> PD.Data -> PD.Data -> BS.ByteString -> PD.Data
acceptedReconstructionStateFullFor = acceptedReconstructionStateFullForRoot acceptedDefaultPriorRoot

acceptedReconstructionStateFullForRoot :: BS.ByteString -> Integer -> Integer -> Integer -> Integer -> BS.ByteString -> BS.ByteString -> Integer -> Integer -> PD.Data -> PD.Data -> BS.ByteString -> PD.Data
acceptedReconstructionStateFullForRoot priorRoot boundExecutionIndex phase fieldCursor executionCursor previousKey receiveCandidate sourceBaseIndex sourceCursor selectedPurpose selectedSource nextHash =
    PD.Constr
        0
        [ acceptedBoundStateForRoot priorRoot boundExecutionIndex
        , PD.I phase
        , PD.I fieldCursor
        , PD.I executionCursor
        , PD.B previousKey
        , PD.B receiveCandidate
        , PD.I sourceBaseIndex
        , PD.I sourceCursor
        , selectedPurpose
        , selectedSource
        , PD.B nextHash
        , PD.B $ acceptedReconstructionCheckpoint priorRoot boundExecutionIndex phase fieldCursor executionCursor previousKey receiveCandidate sourceBaseIndex sourceCursor selectedPurpose selectedSource nextHash
        ]

acceptedNothing :: PD.Data
acceptedNothing = PD.Constr 1 []

acceptedSelectedPurpose :: PD.Data
acceptedSelectedPurpose = PD.Constr 0 [PD.Constr 0 [PD.I 0, PD.I 0, PD.B $ BS.replicate 28 0x43, PD.B "purpose"]]

acceptedSpendPurpose, acceptedReceivePurpose :: PD.Data
acceptedSpendPurpose = PD.Constr 0 [PD.Constr 0 [PD.I 0, PD.I 0, PD.B lockedScriptHash, PD.B acceptedSpendKey]]
acceptedReceivePurpose = PD.Constr 0 [PD.Constr 0 [PD.I 3, PD.I 0, PD.B lockedScriptHash, PD.B lockedScriptHash]]

acceptedReceivePurpose2 :: PD.Data
acceptedReceivePurpose2 = PD.Constr 0 [PD.Constr 0 [PD.I 3, PD.I 1, PD.B acceptedSecondScriptHash, PD.B acceptedSecondScriptHash]]

acceptedMintPurpose, acceptedObserverPurpose :: PD.Data
acceptedMintPurpose = PD.Constr 0 [PD.Constr 0 [PD.I 1, PD.I 0, PD.B acceptedMintPolicy, PD.B acceptedMintPolicy]]
acceptedObserverPurpose = PD.Constr 0 [PD.Constr 0 [PD.I 2, PD.I 0, PD.B acceptedObserver, PD.B acceptedObserver]]

acceptedReconstructionCheckpoint :: BS.ByteString -> Integer -> Integer -> Integer -> Integer -> BS.ByteString -> BS.ByteString -> Integer -> Integer -> PD.Data -> PD.Data -> BS.ByteString -> BS.ByteString
acceptedReconstructionCheckpoint priorRoot boundExecutionIndex phase fieldCursor executionCursor previousKey receiveCandidate sourceBaseIndex sourceCursor selectedPurpose selectedSource nextHash =
    blake2b256 $
        "midgard/fraud-proofs/execution-native-script-invalid/accepted-reconstruction-v1"
            <> acceptedSubjectEncoding
            <> definiteBytes acceptedCompact
            <> priorRoot
            <> cborInt boundExecutionIndex
            <> cborInt phase
            <> cborInt fieldCursor
            <> cborInt executionCursor
            <> definiteBytes previousKey
            <> definiteBytes receiveCandidate
            <> cborInt sourceBaseIndex
            <> cborInt sourceCursor
            <> serialise selectedPurpose
            <> serialise selectedSource
            <> wrapItem nextHash

acceptedSubjectEncoding :: BS.ByteString
acceptedSubjectEncoding =
    arrayHeader 6
        <> serialise (PD.I 1)
        <> serialise (PD.I 0)
        <> serialise (PD.I 0)
        <> wrapItem acceptedTxId
        <> wrapItem ""
        <> arrayHeader 0

acceptedCompact, acceptedBody, acceptedTxId, acceptedInlineItem :: BS.ByteString
acceptedCompact = "\x84" <> cborInt 1 <> acceptedBody <> defBytes32 (witnessSetHashOf acceptedTx) <> cborInt 1
acceptedBody =
    "\x8c"
        <> defBytes32 (blake2b256 $ spendInputsPreimage tx1)
        <> defBytes32 (blake2b256 $ referenceInputsPreimage tx1)
        <> defBytes32 (blake2b256 acceptedOutputsPreimage)
        <> cborInt (tFee tx1)
        <> cborInt (tValidityStart tx1)
        <> cborInt (tValidityEnd tx1)
        <> defBytes32 (blake2b256 acceptedObserverPreimage)
        <> defBytes32 (blake2b256 $ requiredSignersPreimage tx1)
        <> defBytes32 (blake2b256 acceptedMintPreimage)
        <> defBytes32 (hash32 0x07)
        <> defBytes32 (hash32 0x08)
        <> cborInt 1
acceptedTxId = blake2b256 $ "MidgardNativeTxBodyV1" <> cborInt 1 <> acceptedBody
acceptedInlineItem = versionedScriptItem 0 nativeScriptBytes

acceptedTx :: Tx
acceptedTx = tx1{tScripts = [(0, nativeScriptBytes)]}

acceptedOutputsPreimage :: BS.ByteString
acceptedOutputsPreimage =
    arrayHeader 3
        <> wrapItem (midgardOutputCbor (BS.cons 0x78 lockedScriptHash) 2_000_000 Nothing)
        <> wrapItem (midgardOutputCbor (BS.cons 0x78 acceptedSecondScriptHash) 2_000_001 Nothing)
        <> wrapItem (outputItem 1)

acceptedSecondScriptHash :: BS.ByteString
acceptedSecondScriptHash = BS.replicate 28 0xff

acceptedMintPolicy, acceptedObserver, acceptedMintPreimage, acceptedObserverPreimage :: BS.ByteString
acceptedMintPolicy = BS.replicate 28 0x44
acceptedObserver = BS.replicate 28 0x45
acceptedMintPreimage = arrayHeader 1 <> wrapItem ("\x82\x58\x1c" <> acceptedMintPolicy <> "\xa1\x45TOKEN\x01")
acceptedObserverPreimage = arrayHeader 1 <> wrapItem acceptedObserver

acceptedDefaultPriorRoot, acceptedSpendKey, acceptedSpendPriorRoot, acceptedSpendDescriptor, acceptedSpendDescriptorOtherValue, acceptedSpendDescriptorWrongIndex :: BS.ByteString
acceptedDefaultPriorRoot = BS.replicate 32 0x42
acceptedSpendKey = encodedInput sharedInputRef
acceptedSpendPriorRoot = singleEntryPhasRoot acceptedSpendKey acceptedSpendDescriptor
acceptedSpendDescriptor = acceptedSpendDescriptorAt 0
acceptedSpendDescriptorOtherValue = hash32 0xee
acceptedSpendDescriptorWrongIndex = acceptedSpendDescriptorAt 1

acceptedRedeemerCarriedMembership :: PD.Data
acceptedRedeemerCarriedMembership = PD.Constr 0 [emptyProof, PD.I 0]

acceptedPublishedChunks :: [Integer]
acceptedPublishedChunks = [5, 6]

acceptedPublishedMembership :: [Integer] -> PD.Data
acceptedPublishedMembership chunks =
    PD.Constr 1 [PD.Constr 0 [PD.List $ map PD.I chunks]]

acceptedPublishedMembershipEntry :: BS.ByteString -> BS.ByteString -> BS.ByteString -> [Integer] -> (ScriptPurpose, Redeemer)
acceptedPublishedMembershipEntry root key value chunks =
    ( Rewarding (ScriptCredential $ ScriptHash $ toBuiltin acceptedChunkedVerifyHash)
    , Redeemer $
        dataToBuiltinData $
            PD.Constr
                0
                [ PD.Constr 0 []
                , PD.B root
                , PD.B key
                , PD.B $ blake2b256 value
                , PD.List $ map PD.I chunks
                ]
    )

acceptedChunkedVerifyHash :: BS.ByteString
acceptedChunkedVerifyHash = hex "ea8d998a1396392158fa85afb0d202df7bd6d6ede7d3fbc05f55acd6"

acceptedSpendDescriptorAt :: Integer -> BS.ByteString
acceptedSpendDescriptorAt outputIndex =
    arrayHeader 16
        <> cborInt 1
        <> cborInt outputIndex
        <> cborInt 100
        <> defBytes32 (hash32 0x55)
        <> wrapItem (scriptAddressBytes lockedScriptHash)
        <> cborInt 2_000_000
        <> cborInt 0
        <> defBytes32 (hash32 0x56)
        <> cborInt 0
        <> cborInt (-1)
        <> wrapItem ""
        <> cborInt 0
        <> wrapItem ""
        <> acceptedSpendSummary 0x57
        <> acceptedSpendSummary 0x58
        <> acceptedSpendSummary 0x59

acceptedSpendSummary :: Int -> BS.ByteString
acceptedSpendSummary seed = arrayHeader 3 <> defBytes32 (hash32 seed) <> cborInt 0 <> cborInt 0

acceptedReferenceDescriptorNoScript, acceptedReferenceDescriptorWrongIndex, acceptedReferenceDescriptorForeign, acceptedReferenceDescriptorMatching, acceptedReferenceScriptCommitment :: BS.ByteString
acceptedReferenceDescriptorNoScript = acceptedReferenceDescriptorAt 0 (-1) "" 0 ""
acceptedReferenceDescriptorWrongIndex = acceptedReferenceDescriptorAt 1 (-1) "" 0 ""
acceptedReferenceDescriptorForeign = acceptedReferenceDescriptorAt 0 0 (BS.replicate 28 0x66) 7 (hash32 0x67)
acceptedReferenceDescriptorMatching = acceptedReferenceDescriptorAt 0 0 lockedScriptHash (fromIntegral $ BS.length acceptedInlineItem) acceptedReferenceScriptCommitment
acceptedReferenceScriptCommitment = boundedItemCommitment 2 0 acceptedInlineItem

acceptedReferencePriorRoot :: BS.ByteString -> BS.ByteString
acceptedReferencePriorRoot = singleEntryPhasRoot acceptedSpendKey

acceptedReferenceDescriptorAt :: Integer -> Integer -> BS.ByteString -> Integer -> BS.ByteString -> BS.ByteString
acceptedReferenceDescriptorAt outputIndex language scriptHash totalLength itemCommitment =
    arrayHeader 16
        <> cborInt 1
        <> cborInt outputIndex
        <> cborInt 100
        <> defBytes32 (hash32 0x61)
        <> wrapItem (pubKeyAddressBytes $ keyHashFor 0)
        <> cborInt 2_000_000
        <> cborInt 0
        <> defBytes32 (hash32 0x62)
        <> cborInt 0
        <> cborInt language
        <> wrapItem scriptHash
        <> cborInt totalLength
        <> wrapItem itemCommitment
        <> acceptedSpendSummary 0x63
        <> acceptedSpendSummary 0x64
        <> acceptedSpendSummary 0x65

mutateAcceptedCheckpoint :: PD.Data -> PD.Data
mutateAcceptedCheckpoint (PD.Constr 0 fields) = PD.Constr 0 $ take 11 fields <> [PD.B $ BS.replicate 32 0xff]
mutateAcceptedCheckpoint _ = error "unexpected accepted reconstruction state"

definiteBytes :: BS.ByteString -> BS.ByteString
definiteBytes bytes
    | len <= 23 = BS.singleton (fromIntegral $ 0x40 + len) <> bytes
    | len <= 255 = BS.pack [0x58, fromIntegral len] <> bytes
    | len <= 65535 = BS.pack [0x59, fromIntegral $ len `div` 256, fromIntegral len] <> bytes
    | otherwise = error "definite byte string exceeds fixture range"
  where
    len = BS.length bytes

executionStep04StateFor :: BS.ByteString -> PD.Data
executionStep04StateFor scriptItem =
    PD.Constr
        0
        [ PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.B $ boundedItemCommitment 6 0 scriptItem
        , PD.B tx1Id
        , PD.B $ witnessSetHashOf tx1
        , PD.B $ blake2b256 scriptItem
        , PD.I $ tValidityStart tx1
        , PD.I $ tValidityEnd tx1
        ]

executionStep03State :: BS.ByteString -> PD.Data
executionStep03State scriptItem =
    PD.Constr
        0
        [ PD.Constr
            0
            [ acceptedSubject tx1Id
            , PD.B $ BS.replicate 32 0x41
            , PD.I 1
            , PD.I 0
            , PD.I (-1)
            , PD.B $ BS.replicate 32 0x42
            , PD.B $ compactOf tx1
            ]
        , PD.B $ BS.replicate 32 0x42
        , PD.I 0
        , PD.I 0
        , PD.B ""
        , PD.I 0
        , PD.B $ BS.replicate 28 0x43
        , PD.I $ fromIntegral $ BS.length scriptItem
        , PD.B $ boundedItemCommitment 6 0 scriptItem
        , PD.B $ compactOf tx1
        ]

runExecutionStep03 :: forall s. BS.ByteString -> BS.ByteString -> Term s PUnit
runExecutionStep03 scriptItem successor =
    executionStep03 $
        continueContext
            (executionStep03State unsatisfiedScriptItem)
            (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.B scriptItem]])
            (executionStep04StateFor unsatisfiedScriptItem)
            successor
            []

boundedItemCommitment :: Integer -> Integer -> BS.ByteString -> BS.ByteString
boundedItemCommitment fieldIndex itemIndex item =
    blake2b256 $
        "MidgardBoundedItemCommitmentV1"
            <> arrayHeader 5
            <> cborInt 1
            <> cborInt fieldIndex
            <> cborInt itemIndex
            <> cborInt (fromIntegral $ BS.length item)
            <> wrapItem frontier
  where
    leaf =
        blake2b256 $
            "MidgardBoundedItemChunkV1"
                <> arrayHeader 5
                <> cborInt 1
                <> cborInt fieldIndex
                <> cborInt itemIndex
                <> cborInt 0
                <> wrapItem item
    frontier =
        blake2b256 $
            "MidgardValidationMerkleFrontierV1"
                <> cborInt 1
                <> arrayHeader 1
                <> arrayHeader 2
                <> cborInt 0
                <> wrapItem leaf

executionStep06State :: BS.ByteString -> PD.Data -> PD.Data
executionStep06State scriptItem phase =
    PD.Constr
        0
        [ PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.B $ blake2b256 scriptItem
        , PD.B tx1Id
        , PD.B $ blake2b256 scriptItem
        , PD.I (-1)
        , PD.I (-1)
        , PD.I 1
        , PD.List oneSignerPeaks
        , phase
        ]

runExecutionStep04 :: forall s. BS.ByteString -> Term s PUnit
runExecutionStep04 scriptItem = executionStep04 $ finalizeContext (stepDatum $ Just $ executionStep04StateFor scriptItem) redeemer
  where
    redeemer =
        PD.Constr
            1
            [ PD.Constr
                0
                [ PD.I 0
                , PD.I 0
                , PD.I 0
                , PD.B scriptItem
                , witnessOpening (compactOf tx1) tx1 (addressWitnessesPreimage tx1)
                ]
            ]

runExecutionStep06 :: forall s. BS.ByteString -> [PD.Data] -> Term s PUnit
runExecutionStep06 scriptItem queries = executionStep06 $ finalizeContext (stepDatum $ Just $ executionStep06State scriptItem readyPhase) redeemer
  where
    redeemer =
        PD.Constr
            1
            [ PD.Constr
                2
                [ PD.I 0
                , PD.I 0
                , PD.I 0
                , PD.B scriptItem
                , PD.I 1
                , PD.List queries
                ]
            ]
mutatedCompoundCursor =
    step05 $
        continueContext
            (step05State compoundScriptItem $ walkPhase $ BS.replicate 32 0)
            ( PD.Constr
                1
                [ PD.Constr
                    1
                    [ PD.I 0
                    , PD.I 0
                    , PD.B compoundScriptItem
                    , PD.B compoundFirstCursor
                    , PD.List compoundFrames
                    , PD.I 1
                    , PD.List [compoundMissingQuery]
                    ]
                ]
            )
            (step05State compoundScriptItem $ walkPhase compoundSecondHash)
            stepScript
            []

startExecutionCompoundScan, resumeExecutionCompoundScan, mutatedExecutionCompoundCursor :: forall s. Term s PUnit
startExecutionCompoundScan =
    executionStep06 $
        continueContext
            (executionStep06State compoundScriptItem readyPhase)
            (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.B compoundScriptItem, PD.I 1, PD.List []]])
            (executionStep06State compoundScriptItem $ walkPhase compoundFirstHash)
            stepScript
            []
resumeExecutionCompoundScan =
    executionStep06 $
        continueContext
            (executionStep06State compoundScriptItem $ walkPhase compoundFirstHash)
            ( PD.Constr
                1
                [ PD.Constr
                    1
                    [ PD.I 0
                    , PD.I 0
                    , PD.B compoundScriptItem
                    , PD.B compoundFirstCursor
                    , PD.List compoundFrames
                    , PD.I 1
                    , PD.List [compoundMissingQuery]
                    ]
                ]
            )
            (executionStep06State compoundScriptItem $ walkPhase compoundSecondHash)
            stepScript
            []
mutatedExecutionCompoundCursor =
    executionStep06 $
        continueContext
            (executionStep06State compoundScriptItem $ walkPhase $ BS.replicate 32 0)
            ( PD.Constr
                1
                [ PD.Constr
                    1
                    [ PD.I 0
                    , PD.I 0
                    , PD.B compoundScriptItem
                    , PD.B compoundFirstCursor
                    , PD.List compoundFrames
                    , PD.I 1
                    , PD.List [compoundMissingQuery]
                    ]
                ]
            )
            (executionStep06State compoundScriptItem $ walkPhase compoundSecondHash)
            stepScript
            []

continueContext :: PD.Data -> PD.Data -> PD.Data -> BS.ByteString -> [TxInInfo] -> ScriptContext
continueContext inputState redeemer outputState outputScript refs =
    continueContextWithRedeemers inputState redeemer outputState outputScript refs []

continueContextWithRedeemers :: PD.Data -> PD.Data -> PD.Data -> BS.ByteString -> [TxInInfo] -> [(ScriptPurpose, Redeemer)] -> ScriptContext
continueContextWithRedeemers inputState redeemer outputState outputScript refs redeemers =
    spendContext
        (stepDatum $ Just inputState)
        redeemer
        [threadInput]
        [stepOutput outputScript $ Just outputState]
        refs
        redeemers
        mempty

walkPhase :: BS.ByteString -> PD.Data
walkPhase cursorHash = PD.Constr 1 [PD.B cursorHash]

compoundSigner, compoundPayload, compoundScriptItem :: BS.ByteString
compoundSigner = BS.replicate 28 0x99
compoundPayload = "\x82\x01\x82" <> signaturePayload compoundSigner <> signaturePayload compoundSigner
compoundScriptItem = versionedScriptItem 0 compoundPayload

compoundFrames :: [PD.Data]
compoundFrames = [frameData 1 2 0 2]

compoundFirstCursor, compoundSecondCursor, compoundFirstHash, compoundSecondHash :: BS.ByteString
compoundFirstCursor = cursorBytes compoundPayload (1, 2, 0, 2) 3 1 0
compoundSecondCursor = cursorBytes compoundPayload (1, 2, 0, 2) 35 2 1
compoundFirstHash = cursorHash compoundFirstCursor
compoundSecondHash = cursorHash compoundSecondCursor

compoundMissingQuery :: PD.Data
compoundMissingQuery = signerQuery compoundSigner $ PD.Constr 4 [PD.List oneSignerPeaks, PD.B presentSigner, PD.List []]

cursorBytes :: BS.ByteString -> (Integer, Integer, Integer, Integer) -> Integer -> Integer -> Integer -> BS.ByteString
cursorBytes payload frame@(kind, remaining, satisfied, required) offset visited pending =
    BS.concat
        [ "\x87\x58\x20"
        , blake2b256 payload
        , "\x58\x20"
        , blake2b256 $ frameDomain <> emptyStackRoot <> encodeFrame frame
        , "\x43"
        , be3 $ fromIntegral $ BS.length payload
        , "\x43"
        , be3 offset
        , "\x43\x00\x00\x01"
        , "\x43"
        , be3 visited
        , "\x41"
        , BS.singleton $ fromIntegral pending
        ]
  where
    _ = (kind, remaining, satisfied, required)

encodeFrame :: (Integer, Integer, Integer, Integer) -> BS.ByteString
encodeFrame (kind, remaining, satisfied, required) =
    BS.singleton (fromIntegral kind) <> be3 remaining <> be3 satisfied <> be3 required

frameData :: Integer -> Integer -> Integer -> Integer -> PD.Data
frameData kind remaining satisfied required = PD.Constr 0 [PD.I kind, PD.I remaining, PD.I satisfied, PD.I required]

be3 :: Integer -> BS.ByteString
be3 value = BS.pack [fromIntegral $ value `div` 65536, fromIntegral $ value `div` 256, fromIntegral value]

frameDomain, emptyStackRoot :: BS.ByteString
frameDomain = "MidgardNativeScriptFrameV1"
emptyStackRoot = blake2b256 frameDomain

cursorHash :: BS.ByteString -> BS.ByteString
cursorHash cursor = blake2b256 $ "MidgardNativeScriptWalkV1" <> cursor

q34Case :: String -> ([BS.ByteString] -> Assertion) -> TestTree
q34Case name assertion = testCase name $ assertion =<< mapM BS.readFile q34ChunkPaths

q34ChunkPaths :: [FilePath]
q34ChunkPaths = ["tests/fixtures/native-script-invalid-q34/chunk-0.bin", "tests/fixtures/native-script-invalid-q34/chunk-1.bin", "tests/fixtures/native-script-invalid-q34/chunk-2.bin"]

q34Step03 :: forall s. [BS.ByteString] -> BS.ByteString -> Term s PUnit
q34Step03 chunks successor = q34Step03Budget chunks successor 16

q34Step03Budget :: forall s. [BS.ByteString] -> BS.ByteString -> Integer -> Term s PUnit
q34Step03Budget chunks successor budget =
    step03 $
        continueContext
            q34Step03State
            ( PD.Constr
                1
                [ PD.Constr
                    1
                    [ PD.I 0
                    , PD.I 0
                    , PD.B q34NativeScriptItem
                    , q34Opening
                    , PD.I budget
                    ]
                ]
            )
            q34State16
            successor
            (q34ReferenceInputs chunks)

q34Step04 :: forall s. [BS.ByteString] -> PD.Data -> BS.ByteString -> Integer -> PD.Data -> Bool -> Term s PUnit
q34Step04 chunks inputState checkpoint budget outputState finalizing =
    step04 $
        continueContext
            inputState
            ( PD.Constr
                1
                [ PD.Constr
                    (if finalizing then 1 else 0)
                    [PD.I 0, PD.I 0, q34Opening, PD.B checkpoint, PD.I budget]
                ]
            )
            outputState
            (if finalizing then nextScript else stepScript)
            (q34ReferenceInputs chunks)

q34Step05 :: forall s. Term s PUnit
q34Step05 =
    step05 $
        finalizeContext
            (stepDatum $ Just q34State318)
            ( PD.Constr
                1
                [ PD.Constr
                    2
                    [ PD.I 0
                    , PD.I 0
                    , PD.I 0
                    , PD.B q34NativeScriptItem
                    , PD.I 1
                    , PD.List [q34AbsentQuery]
                    ]
                ]
            )

executionQ34Step04 :: forall s. [BS.ByteString] -> Integer -> BS.ByteString -> Term s PUnit
executionQ34Step04 chunks budget successor =
    executionStep04 $
        continueContext
            executionQ34Step04State
            ( PD.Constr
                1
                [ PD.Constr
                    1
                    [ PD.I 0
                    , PD.I 0
                    , PD.B q34NativeScriptItem
                    , q34Opening
                    , PD.I budget
                    ]
                ]
            )
            executionQ34State16
            successor
            (q34ReferenceInputs chunks)

executionQ34Step05 :: forall s. [BS.ByteString] -> PD.Data -> BS.ByteString -> Integer -> PD.Data -> Bool -> Term s PUnit
executionQ34Step05 chunks inputState checkpoint budget outputState finalizing =
    executionStep05 $
        continueContext
            inputState
            ( PD.Constr
                1
                [ PD.Constr
                    (if finalizing then 1 else 0)
                    [PD.I 0, PD.I 0, q34Opening, PD.B checkpoint, PD.I budget]
                ]
            )
            outputState
            (if finalizing then nextScript else stepScript)
            (q34ReferenceInputs chunks)

executionQ34Step06 :: forall s. Term s PUnit
executionQ34Step06 =
    executionStep06 $
        finalizeContext
            (stepDatum $ Just executionQ34State318)
            ( PD.Constr
                1
                [ PD.Constr
                    2
                    [ PD.I 0
                    , PD.I 0
                    , PD.I 0
                    , PD.B q34NativeScriptItem
                    , PD.I 1
                    , PD.List [q34AbsentQuery]
                    ]
                ]
            )

executionQ34Step04State :: PD.Data
executionQ34Step04State =
    PD.Constr
        0
        [ PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.B $ blake2b256 q34NativeScriptItem
        , PD.B q34TxId
        , PD.B q34WitnessSetHash
        , PD.B $ blake2b256 q34NativeScriptItem
        , PD.I 0
        , PD.I 100
        ]

executionQ34State16, executionQ34State32, executionQ34State288, executionQ34State304, executionQ34State318 :: PD.Data
executionQ34State16 = executionQ34Step05State q34CheckpointHash16 q34Previous16 16 [peak 4 "b07c8e68e66274f6209dd8176cf2d82e6ac08eb89cac88e38f3ed414d2aa169c"]
executionQ34State32 = executionQ34Step05State q34CheckpointHash32 q34Previous32 32 q34Peaks32
executionQ34State288 = executionQ34Step05State q34CheckpointHash288 q34Previous288 288 q34Peaks288
executionQ34State304 = executionQ34Step05State q34CheckpointHash304 q34Previous304 304 q34Peaks304
executionQ34State318 =
    PD.Constr
        0
        [ PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.B $ blake2b256 q34NativeScriptItem
        , PD.B q34TxId
        , PD.B $ blake2b256 q34NativeScriptItem
        , PD.I 0
        , PD.I 100
        , PD.I 318
        , PD.List q34Peaks318
        , readyPhase
        ]

executionQ34Step05State :: BS.ByteString -> BS.ByteString -> Integer -> [PD.Data] -> PD.Data
executionQ34Step05State checkpointHash previous count peaks =
    PD.Constr
        0
        [ PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.B $ blake2b256 q34NativeScriptItem
        , PD.B q34TxId
        , PD.B q34WitnessSetHash
        , PD.B $ blake2b256 q34NativeScriptItem
        , PD.I 0
        , PD.I 100
        , PD.B checkpointHash
        , PD.B previous
        , PD.I count
        , PD.List peaks
        ]

q34State16 :: PD.Data
q34State16 =
    q34Step04State
        (hex "ad313e491443c5b3d4bb77b464c8a303f2ac71c31f4fa003711d619760386937")
        (hex "07ebf523dfab719a922bf4ff3cb790ebac51b0078132fa789fda59a9")
        16
        [peak 4 "b07c8e68e66274f6209dd8176cf2d82e6ac08eb89cac88e38f3ed414d2aa169c"]

q34Checkpoint16 :: BS.ByteString
q34Checkpoint16 = hex "865820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23410743007ff54300013e4300001043000673"

q34Step03State, q34State32, q34State288, q34State304, q34State318 :: PD.Data
q34Step03State =
    PD.Constr 0 [acceptedSubject q34TxId, PD.B q34TxId, PD.B q34WitnessSetHash, PD.B $ blake2b256 q34NativeScriptItem, PD.I 0, PD.I 100]
q34State32 = q34Step04State q34CheckpointHash32 q34Previous32 32 q34Peaks32
q34State288 = q34Step04State q34CheckpointHash288 q34Previous288 288 q34Peaks288
q34State304 = q34Step04State q34CheckpointHash304 q34Previous304 304 q34Peaks304
q34State318 = q34Step05State q34NativeScriptItem q34Peaks318 readyPhase

q34Step04State :: BS.ByteString -> BS.ByteString -> Integer -> [PD.Data] -> PD.Data
q34Step04State checkpointHash previous count peaks =
    PD.Constr
        0
        [ acceptedSubject q34TxId
        , PD.B q34TxId
        , PD.B q34WitnessSetHash
        , PD.B $ blake2b256 q34NativeScriptItem
        , PD.I 0
        , PD.I 100
        , PD.B checkpointHash
        , PD.B previous
        , PD.I count
        , PD.List peaks
        ]

q34Step05State :: BS.ByteString -> [PD.Data] -> PD.Data -> PD.Data
q34Step05State scriptItem peaks phase =
    PD.Constr 0 [acceptedSubject q34TxId, PD.B q34TxId, PD.B $ blake2b256 scriptItem, PD.I 0, PD.I 100, PD.I 318, PD.List peaks, phase]

q34Opening :: PD.Data
q34Opening =
    PD.Constr
        1
        [ PD.B q34CompactCbor
        , PD.Constr 0 [PD.B q34AddressHash, PD.B q34ScriptHash, PD.B q34RedeemerHash]
        , PD.Constr 2 [PD.I 0, PD.List [PD.I 1, PD.I 2, PD.I 3]]
        ]

q34ReferenceInputs :: [BS.ByteString] -> [TxInInfo]
q34ReferenceInputs chunks = q34CertificateInput : zipWith q34ChunkInput [1 ..] chunks

q34CertificateInput :: TxInInfo
q34CertificateInput =
    TxInInfo
        (TxOutRef (TxId $ toBuiltin q34TxId) 0)
        ( TxOut
            (scriptHashAddress $ ScriptHash $ toBuiltin $ unCS certificatePolicy)
            (adaValue 2_000_000 <> singleton certificatePolicy (TokenName $ toBuiltin ("MIDGARD_FIELD_PREIMAGE_CERT" :: BS.ByteString)) 1)
            (OutputDatum $ Datum $ dataToBuiltinData q34CertificateDatum)
            Nothing
        )

q34ChunkInput :: Integer -> BS.ByteString -> TxInInfo
q34ChunkInput index chunk =
    TxInInfo
        (TxOutRef (TxId $ toBuiltin q34TxId) index)
        ( TxOut
            (pubKeyHashAddress $ PubKeyHash $ toBuiltin prover)
            (adaValue 2_000_000)
            (OutputDatum $ Datum $ dataToBuiltinData $ PD.B chunk)
            Nothing
        )

q34CertificateDatum :: PD.Data
q34CertificateDatum =
    PD.Constr
        0
        [ PD.B prover
        , PD.B q34TxId
        , PD.I 7
        , PD.B q34AddressHash
        , PD.I 32757
        , PD.List $ map (PD.B . hex) ["26366e1cee678704dc218da2cf33cb754326bf8fe51b1de108b8ccea92e06032", "96084650107d9a17a7c4d9c37ae53af1023192c2236c0e2c7267c5646ff04744", "2a32995fefdf3483d00e1b3c654ede2dab8a1510b5d4e7ab6c585ea840052b3e"]
        ]

q34NativeScriptItem :: BS.ByteString
q34NativeScriptItem = versionedScriptItem 0 $ signaturePayload $ BS.replicate 28 0xff

q34AbsentQuery :: PD.Data
q34AbsentQuery =
    signerQuery (BS.replicate 28 0xff) $
        PD.Constr
            4
            [ PD.List q34Peaks318
            , PD.B q34Previous318
            , PD.List [PD.B $ hex "70a34b0a50899e8f064f44a2f85d7d1e15fdbd51434435d7df2c84d604d632c6"]
            ]

q34Peaks32, q34Peaks288, q34Peaks304, q34Peaks318 :: [PD.Data]
q34Peaks32 = [peak 5 "75f1bcc6cd0bc4f065a6f0e86f50c6906dd87835c24303964d4e56eb520ebc98"]
q34Peaks288 = [peak 5 "a200e88dc07a54d5ae4d1fccfe9050273fa2414859354f9e38916fdc015938e6", peak 8 "738d1ebefdc3fc46f8ee2fc6cefa29aef784667366f75d70adfaf16612036df4"]
q34Peaks304 = peak 4 "e752280c3a26eecf94be5da5a4955c274896b9f39bc12313f9a6499a207914c8" : q34Peaks288
q34Peaks318 = [peak 1 "b5b3961284cce7d7c275bf95f6f3431884938813f9d0a83cced879e384b0e8e5", peak 2 "62eea386cfae9dd430a0213803f589211bff58b13982fdabf2bd3a3677187eb8", peak 3 "3747ab530d3dd3b00b19a55cb3e9143401be1e96a41bb216e02af737f9618840"] <> q34Peaks304

peak :: Integer -> String -> PD.Data
peak height digest = PD.Constr 0 [PD.I height, PD.B $ hex digest]

q34TxId, q34CompactCbor, q34AddressHash, q34ScriptHash, q34RedeemerHash, q34WitnessSetHash :: BS.ByteString
q34TxId = hex "d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23"
q34CompactCbor = hex "84018c5820f99641fc7bc7e291a96a6de01185240bbe7bd7b2ce2a5f6d816dc8e5c68a0c525820ccfbff4fea0f54213e078ce74e65bf844a143f9b74eece6fe005bb04742d239658206268504e96c250ed6ea83a2bc508404dd8dd3b900763c9d8f595dfc47d98d7cc1a000f42402020582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820a6f688ee8982ebdbffbfa608b43c087d3dc1c996df57e4626618da1a8e0494e3582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820101010101010101010101010101010101010101010101010101010101010101058201010101010101010101010101010101010101010101010101010101010101010005820e352221d0b5ae02610c47f76a0f7ffb63f72b72275d931d18e03ab4696754a6800"
q34AddressHash = hex "be3c48be9923f633253c5f46da8e1410bb26eaa79b617322dc29528cc44b77ef"
q34ScriptHash = hex "ad4b36af564e3d2cab118663351f7d658dad69932a9e0d5e8196783f16234a8e"
q34RedeemerHash = hex "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0"
q34WitnessSetHash = hex "e352221d0b5ae02610c47f76a0f7ffb63f72b72275d931d18e03ab4696754a68"

q34Checkpoint32, q34Checkpoint288, q34Checkpoint304 :: BS.ByteString
q34Checkpoint32 = hex "865820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23410743007ff54300013e4300002043000ce3"
q34Checkpoint288 = hex "865820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23410743007ff54300013e43000120430073e3"
q34Checkpoint304 = hex "865820d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23410743007ff54300013e4300013043007a53"

q34CheckpointHash16, q34CheckpointHash32, q34CheckpointHash288, q34CheckpointHash304 :: BS.ByteString
q34CheckpointHash16 = hex "ad313e491443c5b3d4bb77b464c8a303f2ac71c31f4fa003711d619760386937"
q34CheckpointHash32 = hex "756e2088a16c3b1fa9ac93a05559a192a7beaf48db71533524d67a2742bd524a"
q34CheckpointHash288 = hex "42435ba5c9e1b73bc0462e7f117520a4f7507aa640f394d0cffb8f24e0e5cc5d"
q34CheckpointHash304 = hex "403405148e7b6bc4cb7defdfed99127b7165176b7cda0faca64ceea31dbc3b85"

q34Previous16, q34Previous32, q34Previous288, q34Previous304, q34Previous318 :: BS.ByteString
q34Previous16 = hex "07ebf523dfab719a922bf4ff3cb790ebac51b0078132fa789fda59a9"
q34Previous32 = hex "0f8101ff30ef95e19a91648426e29474d0fb04e39f9491d83cbd4411"
q34Previous288 = hex "ea14141719dc1075c008e3aabbb96e18b5c32d07c6ab77eabde3fea3"
q34Previous304 = hex "f2528f122d04b2588644a34f9d32a5aec5ed50b3335bd7030b74259a"
q34Previous318 = hex "ffa3561f45a11cafde2b91393f1a70fa537320fa3bc618804391f351"

startMaxScript, finalizeMaxScript :: forall s. BS.ByteString -> Term s PUnit
startMaxScript payload =
    let scriptItem = versionedScriptItem 0 payload
        first = maxCursor payload 16
     in step05 $
            continueContext
                (q34Step05State scriptItem q34Peaks318 readyPhase)
                (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.B scriptItem, PD.I 16, PD.List []]])
                (q34Step05State scriptItem q34Peaks318 $ walkPhase $ cursorHash first)
                stepScript
                []
finalizeMaxScript payload =
    let scriptItem = versionedScriptItem 0 payload
        first = maxCursor payload 48
        inputState = q34Step05State scriptItem q34Peaks318 $ walkPhase $ cursorHash first
        redeemer =
            PD.Constr
                1
                [ PD.Constr
                    3
                    [ PD.I 0
                    , PD.I 0
                    , PD.I 0
                    , PD.B scriptItem
                    , PD.B first
                    , PD.List [maxFrame payload 48]
                    , PD.I 15
                    , PD.List []
                    ]
                ]
     in step05 $ finalizeContext (stepDatum $ Just inputState) redeemer

startExecutionMaxScript :: forall s. BS.ByteString -> Integer -> Term s PUnit
startExecutionMaxScript payload budget =
    let scriptItem = versionedScriptItem 0 payload
        first = maxCursor payload 16
     in executionStep06 $
            continueContext
                (executionMaxState scriptItem readyPhase)
                (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.B scriptItem, PD.I budget, PD.List []]])
                (executionMaxState scriptItem $ walkPhase $ cursorHash first)
                stepScript
                []

finalizeExecutionMaxScript :: forall s. BS.ByteString -> Term s PUnit
finalizeExecutionMaxScript payload =
    let scriptItem = versionedScriptItem 0 payload
        first = maxCursor payload 48
        inputState = executionMaxState scriptItem $ walkPhase $ cursorHash first
        redeemer =
            PD.Constr
                1
                [ PD.Constr
                    3
                    [ PD.I 0
                    , PD.I 0
                    , PD.I 0
                    , PD.B scriptItem
                    , PD.B first
                    , PD.List [maxFrame payload 48]
                    , PD.I 15
                    , PD.List []
                    ]
                ]
     in executionStep06 $ finalizeContext (stepDatum $ Just inputState) redeemer

executionMaxState :: BS.ByteString -> PD.Data -> PD.Data
executionMaxState scriptItem phase =
    PD.Constr
        0
        [ PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.B $ blake2b256 scriptItem
        , PD.B q34TxId
        , PD.B $ blake2b256 scriptItem
        , PD.I 0
        , PD.I 100
        , PD.I 318
        , PD.List q34Peaks318
        , phase
        ]

maxCursor :: BS.ByteString -> Integer -> BS.ByteString
maxCursor payload budget =
    let (frame, pending) =
            if payload == maxSatisfiedPayload
                then ((1, 32 - budget `div` 2, budget `div` 2 - 1, 31), 2)
                else ((1, 32 - budget `div` 2, 0, 31), 1)
     in cursorBytes payload frame (4 + 5 * (budget `div` 2)) (1 + budget `div` 2) pending

maxFrame :: BS.ByteString -> Integer -> PD.Data
maxFrame payload budget =
    frameData 1 (32 - budget `div` 2) (if payload == maxSatisfiedPayload then budget `div` 2 - 1 else 0) 31

maxUnsatisfiedPayload, maxSatisfiedPayload :: BS.ByteString
maxUnsatisfiedPayload = "\x82\x01\x98\x1f" <> BS.concat (replicate 31 "\x82\x04\x19\x03\xe8")
maxSatisfiedPayload = "\x82\x01\x98\x1f" <> BS.concat (replicate 31 "\x82\x05\x19\x03\xe8")

hex :: String -> BS.ByteString
hex = Base16.decodeLenient . BS.pack . map (fromIntegral . fromEnum)
