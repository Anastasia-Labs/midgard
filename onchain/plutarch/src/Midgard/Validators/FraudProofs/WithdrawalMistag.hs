module Midgard.Validators.FraudProofs.WithdrawalMistag (
    withdrawalMistagStep01Validator,
    withdrawalMistagStep02Validator,
    withdrawalMistagStep03Validator,
    withdrawalMistagStep04Validator,
    withdrawalMistagStep05Validator,
) where

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize)
import Midgard.FraudProofs.FabricatedWithdrawal (pverifyCommittedWithdrawalMembershipV1)
import Midgard.FraudProofs.StructuredDataCarriage qualified as Carriage
import Midgard.FraudProofs.WithdrawalMistag
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.LedgerState (
    PHeaderV1 (..),
    PTransitionStep (..),
    PWithdrawalBody,
    PWithdrawalInfo (..),
    PWithdrawalValidity (..),
 )
import Midgard.StateQueue (pgetBlockDatumV1)
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (
    pdispatch,
    pexpectDatum,
    pexpectStateAs,
    pexpecting,
    pstep,
 )

withdrawalMistagStep01Validator ::
    forall s.
    Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
withdrawalMistagStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
            PStep01Args
                { pstep01Args'inputIndex
                , pstep01Args'outputIndex
                , pstep01Args'hubRefInputIndex
                , pstep01Args'stateQueueNodeRefInputIndex
                , pstep01Args'payload
                } <-
                pmatch args
            PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
            PStep01Payload{pstep01Payload'committedWithdrawal} <- pmatch $ punsafeCoerce @PStep01Payload $ Carriage.presolve # pstep01Args'payload # pfromData ptxInfo'referenceInputs
            membership@PRootMembershipProof{prootMembership'key, prootMembership'value} <-
                pmatch $ pfromData pstep01Payload'committedWithdrawal
            pcontinue
                computationThreadPolicy
                (pexpectDatum datum)
                (pfromData pstep01Args'inputIndex)
                (pfromData pstep01Args'outputIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                $ \_ownScriptHash threadName _prover _inputState outputScriptHash outputStateData ->
                    pif (pcategoryIsWithdrawalMistagV1 # threadName) `flip` perror $ P.do
                        PHubOracleDatum{phubOracle'stateQueue} <-
                            pmatch $ Hub.pgetDatum # pfromData ptxInfo'referenceInputs # hubOracle # pfromData pstep01Args'hubRefInputIndex
                        pgetBlockDatumV1
                            (pfromData ptxInfo'referenceInputs)
                            phubOracle'stateQueue
                            (pfromData pstep01Args'stateQueueNodeRefInputIndex)
                            $ \headerD headerHash -> P.do
                                header@PHeaderV1
                                    { pheader'eventToStepRoot
                                    , pheader'totalEventCount
                                    , pheader'transitionTraceRoot
                                    , pheader'transitionStepCount
                                    } <-
                                    pmatch $ pfromData headerD
                                PWithdrawalInfo{pwithdrawalInfo'validity} <-
                                    pmatch $ pfromData $ punsafeCoerce @(PAsData PWithdrawalInfo) prootMembership'value
                                claimedValid <-
                                    plet $ pmatch (pfromData pwithdrawalInfo'validity) $ \case
                                        PWithdrawalIsValid -> pconstant True
                                        _ -> pconstant False
                                expected <-
                                    plet $
                                        pcon $
                                            PStep01State
                                                (pdata headerHash)
                                                (punsafeCoerce prootMembership'key)
                                                (pdata $ pblake2b_256 #$ pserialiseData # prootMembership'value)
                                                (pdata claimedValid)
                                                pheader'eventToStepRoot
                                                pheader'totalEventCount
                                                pheader'transitionTraceRoot
                                                pheader'transitionStepCount
                                pexpecting (headerHash #== pchallengedHeaderHashOfV1 # threadName) $
                                    pexpecting (pverifyCommittedWithdrawalMembershipV1 (pdata $ pcon header) $ pcon membership) $
                                        pexpecting (outputScriptHash #== step02ScriptHash) $
                                            pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

withdrawalMistagStep02Validator ::
    forall s.
    Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
withdrawalMistagStep02Validator = plam $ \step03ScriptHash computationThreadPolicy ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args ->
            P.do
                PStep02Args
                    { pstep02Args'inputIndex
                    , pstep02Args'outputIndex
                    , pstep02Args'payload
                    } <-
                    pmatch args
                PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
                PStep02Payload{pstep02Payload'withdrawalInfo, pstep02Payload'eventToStep, pstep02Payload'transitionStep} <- pmatch $ punsafeCoerce @PStep02Payload $ Carriage.presolve # pstep02Args'payload # pfromData ptxInfo'referenceInputs
                pcontinue
                    computationThreadPolicy
                    (pexpectDatum datum)
                    (pfromData pstep02Args'inputIndex)
                    (pfromData pstep02Args'outputIndex)
                    ownOutRef
                    (pfromData ptxInfo'inputs)
                    (pfromData ptxInfo'outputs)
                    $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
                        state@PStep01State
                            { pstep01State'challengedHeaderHash
                            , pstep01State'withdrawalId
                            , pstep01State'withdrawalInfoHash
                            , pstep01State'claimedValid
                            } <-
                            pmatch $ pexpectStateAs @PStep01State inputState
                        PRootMembershipProof{prootMembership'value} <- pmatch $ pfromData pstep02Payload'transitionStep
                        PTransitionStep{ptransitionStep'preUtxosRoot} <-
                            pmatch $ pfromData $ punsafeCoerce @(PAsData PTransitionStep) prootMembership'value
                        expected <-
                            plet $
                                pcon $
                                    PStep02State
                                        pstep01State'challengedHeaderHash
                                        pstep01State'withdrawalId
                                        pstep01State'withdrawalInfoHash
                                        pstep01State'claimedValid
                                        ptransitionStep'preUtxosRoot
                        pexpecting
                            (pblake2b_256 # (pserialiseData # pstep02Payload'withdrawalInfo) #== pfromData pstep01State'withdrawalInfoHash)
                            ( pexpecting
                                (ptraceCoordinateIsExactV1 (pcon state) (pfromData pstep02Payload'eventToStep) (pfromData pstep02Payload'transitionStep))
                                ( pexpecting
                                    (outputScriptHash #== step03ScriptHash)
                                    (pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True))
                                )
                            )

withdrawalMistagStep03Validator ::
    forall s.
    Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
withdrawalMistagStep03Validator = plam $ \step04ScriptHash computationThreadPolicy ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep03Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
            PStep03Args
                { pstep03Args'inputIndex
                , pstep03Args'outputIndex
                , pstep03Args'payload
                } <-
                pmatch args
            PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
            PStep03Payload{pstep03Payload'withdrawalInfo, pstep03Payload'evidence} <- pmatch $ punsafeCoerce @PStep03Payload $ Carriage.presolve # pstep03Args'payload # pfromData ptxInfo'referenceInputs
            pcontinue
                computationThreadPolicy
                (pexpectDatum datum)
                (pfromData pstep03Args'inputIndex)
                (pfromData pstep03Args'outputIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
                    state <- plet $ pexpectStateAs @PStep02State inputState
                    expected <-
                        plet $
                            pclassifyLedgerEvidenceV1
                                state
                                (pfromData $ punsafeCoerce @(PAsData PWithdrawalInfo) pstep03Payload'withdrawalInfo)
                                (pfromData pstep03Payload'evidence)
                    pexpecting
                        (outputScriptHash #== step04ScriptHash)
                        (pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True))

withdrawalMistagStep04Validator ::
    forall s.
    Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
withdrawalMistagStep04Validator = plam $ \step05ScriptHash computationThreadPolicy ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep04Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
            PStep04Args{pstep04Args'inputIndex, pstep04Args'outputIndex, pstep04Args'payload} <- pmatch args
            PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
            PStep04Payload{pstep04Payload'withdrawalBody} <- pmatch $ punsafeCoerce @PStep04Payload $ Carriage.presolve # pstep04Args'payload # pfromData ptxInfo'referenceInputs
            pcontinue
                computationThreadPolicy
                (pexpectDatum datum)
                (pfromData pstep04Args'inputIndex)
                (pfromData pstep04Args'outputIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
                    state <- plet $ pexpectStateAs @PStep03State inputState
                    expected <-
                        plet $
                            pestablishMistagV1
                                state
                                (pfromData $ punsafeCoerce @(PAsData PWithdrawalBody) pstep04Payload'withdrawalBody)
                    pexpecting (outputScriptHash #== step05ScriptHash) $
                        pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

withdrawalMistagStep05Validator ::
    forall s.
    Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PAddress :--> PScriptContext :--> PUnit)
withdrawalMistagStep05Validator = plam $ \computationThreadPolicy fraudProofPolicy fraudProofAddress ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep05Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
            PStep05Args{pstep05Args'inputIndex, pstep05Args'outputIndex, pstep05Args'fraudProofMintRedeemerIndex} <- pmatch args
            PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
            pfinalize
                computationThreadPolicy
                fraudProofPolicy
                fraudProofAddress
                (pexpectDatum datum)
                (pfromData pstep05Args'inputIndex)
                (pfromData pstep05Args'outputIndex)
                (pfromData pstep05Args'fraudProofMintRedeemerIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                (pto $ pto $ pfromData ptxInfo'redeemers)
                $ \_ownScriptHash threadName _prover inputState -> P.do
                    state@PStep04State{pstep04State'challengedHeaderHash} <-
                        pmatch $ pexpectStateAs @PStep04State inputState
                    pexpecting (pcategoryIsWithdrawalMistagV1 # threadName) $
                        pexpecting (pfromData pstep04State'challengedHeaderHash #== pchallengedHeaderHashOfV1 # threadName) $
                            pexpecting (pmistagFaultIsEstablishedV1 # pcon state) (pconstant True)
