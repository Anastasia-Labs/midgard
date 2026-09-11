module Main (main) where

import Cardano.Binary qualified as CBOR
import Control.Monad (when)
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text

import Midgard.Env (environmentName)
import System.FilePath ((</>), takeFileName, takeDirectory)
import MerkleTree.Validators.Membership (membershipStakeValidator, nonMembershipStakeValidator)
import Midgard.Validators.ActiveOperators (
    activeOperatorsMintValidator,
    activeOperatorsSpendValidator,
 )
import Midgard.Validators.AvailabilityChallenge (availabilityChallengeValidator)
import Midgard.Validators.CekProgramMaterial (cekProgramMaterialSpendValidator)
import Midgard.Validators.ComputationThread (computationThreadMintValidator)
import Midgard.Validators.CorrectionLock (correctionLockSpendValidator)
import Midgard.Validators.DaAttestation (
    daAttestationValidator,
 )
import Midgard.Validators.DaParamsGovernor (
    daParamsGovernorValidator,
 )
import Midgard.Validators.Deposit (depositMintValidator, depositSpendValidator)
import Midgard.Validators.FieldPreimageCertificate (
    fieldPreimageCertificateMintValidator,
    fieldPreimageCertificateSpendValidator,
 )
import Midgard.Validators.FraudProof (fraudProofMintValidator, fraudProofSpendValidator)
import Midgard.Validators.FraudProofCatalogue (
    fraudProofCatalogueMintValidator,
    fraudProofCatalogueSpendValidator,
 )
import Midgard.Validators.FraudProofs.CanonicalDecodability (
    canonicalDecodabilityStep01Validator,
    canonicalDecodabilityStep02Validator,
 )
import Midgard.Validators.FraudProofs.CommittedFieldShape (
    committedFieldShapeStep01Validator,
    committedFieldShapeStep02Validator,
 )
import Midgard.Validators.FraudProofs.CrossBlockDuplicateEvent (
    crossBlockDuplicateEventStep01Validator,
    crossBlockDuplicateEventStep02Validator,
 )
import Midgard.Validators.FraudProofs.DaHashPreimage (
    daHashPreimageStep01Validator,
    daHashPreimageStep02Validator,
 )
import Midgard.Validators.FraudProofs.DoubleSpend (
    doubleSpendStep01Validator,
    doubleSpendStep02Validator,
    doubleSpendStep03Validator,
    doubleSpendStep04Validator,
 )
import Midgard.Validators.FraudProofs.DoubleWithdraw (
    doubleWithdrawStep01Validator,
    doubleWithdrawStep02Validator,
 )
import Midgard.Validators.FraudProofs.FabricatedDeposit (
    fabricatedDepositStep01Validator,
    fabricatedDepositStep02Validator,
    fabricatedDepositStep03Validator,
    fabricatedDepositStep04Validator,
 )
import Midgard.Validators.FraudProofs.FabricatedWithdrawal (
    fabricatedWithdrawalStep01Validator,
    fabricatedWithdrawalStep02Validator,
    fabricatedWithdrawalStep03Validator,
    fabricatedWithdrawalStep04Validator,
 )
import Midgard.Validators.FraudProofs.InputNoIdx (
    inputNoIdxStep01Validator,
    inputNoIdxStep02Validator,
    inputNoIdxStep03Validator,
    inputNoIdxStep04Validator,
 )
import Midgard.Validators.FraudProofs.InputSetUniqueness (
    inputSetUniquenessStep01Validator,
    inputSetUniquenessStep02Validator,
 )
import Midgard.Validators.FraudProofs.InvalidRange (
    invalidRangeStep01Validator,
    invalidRangeStep02Validator,
 )
import Midgard.Validators.FraudProofs.InvalidSignature (
    invalidSignatureStep01Validator,
    invalidSignatureStep02Validator,
 )
import Midgard.Validators.FraudProofs.L2TxMistag (
    l2TxMistagStep01Validator,
    l2TxMistagStep02Validator,
 )
import Midgard.Validators.FraudProofs.MinAda (
    minAdaStep01Validator,
    minAdaStep02Validator,
    minAdaStep03Validator,
    minAdaStep04Validator,
    minAdaStep05Validator,
 )
import Midgard.Validators.FraudProofs.MinFee (
    minFeeStep01Validator,
    minFeeStep02Validator,
 )
import Midgard.Validators.FraudProofs.MintAuthorization (
    mintAuthorizationStep01Validator,
    mintAuthorizationStep02Validator,
    mintAuthorizationStep03Validator,
    mintAuthorizationStep04Validator,
    mintAuthorizationStep05Validator,
 )
import Midgard.Validators.FraudProofs.MissingNativeScriptTx (
    missingNativeScriptTxStep01Validator,
    missingNativeScriptTxStep02Validator,
    missingNativeScriptTxStep03Validator,
    missingNativeScriptTxStep04Validator,
    missingNativeScriptTxStep05Validator,
    missingNativeScriptTxStep06Validator,
    missingNativeScriptTxStep07Validator,
    missingNativeScriptTxStep08Validator,
 )
import Midgard.Validators.FraudProofs.MissingNativeScriptUtxo (
    missingNativeScriptUtxoStep01Validator,
    missingNativeScriptUtxoStep02Validator,
    missingNativeScriptUtxoStep03Validator,
    missingNativeScriptUtxoStep04Validator,
    missingNativeScriptUtxoStep05Validator,
    missingNativeScriptUtxoStep06Validator,
    missingNativeScriptUtxoStep07Validator,
 )
import Midgard.Validators.FraudProofs.MissingSignature (
    missingSignatureStep01Validator,
    missingSignatureStep02Validator,
    missingSignatureStep03Validator,
    missingSignatureStep04Validator,
 )
import Midgard.Validators.FraudProofs.MpfChunkedChallenge (
    mpfChunkedChallengeValidator,
 )
import Midgard.Validators.FraudProofs.NativeScriptDecoding.AdvanceOrClose (
    nativeScriptDecodingAdvanceOrCloseValidator,
 )
import Midgard.Validators.FraudProofs.NativeScriptDecoding.BindDescriptor (
    nativeScriptDecodingBindDescriptorValidator,
 )
import Midgard.Validators.FraudProofs.NativeScriptDecoding.OpenSubject (
    nativeScriptDecodingOpenSubjectValidator,
 )
import Midgard.Validators.FraudProofs.NativeScriptDecoding.Step01 (
    nativeScriptDecodingStep01Validator,
 )
import Midgard.Validators.FraudProofs.NativeScriptDecoding.Step02 (
    nativeScriptDecodingStep02Validator,
 )
import Midgard.Validators.FraudProofs.NativeScriptDecoding.Step04 (
    nativeScriptDecodingStep04Validator,
 )
import Midgard.Validators.FraudProofs.NativeScriptInvalid (
    nativeScriptInvalidStep01Validator,
    nativeScriptInvalidStep02Validator,
    nativeScriptInvalidStep03Validator,
    nativeScriptInvalidStep04Validator,
    nativeScriptInvalidStep05Validator,
 )
import Midgard.Validators.FraudProofs.NetworkId (
    networkIdStep01Validator,
    networkIdStep02Validator,
 )
import Midgard.Validators.FraudProofs.NoInput (
    noInputStep01Validator,
    noInputStep02Validator,
    noInputStep03Validator,
    noInputStep04Validator,
 )
import Midgard.Validators.FraudProofs.NoReferenceInput (
    noReferenceInputStep01Validator,
    noReferenceInputStep02Validator,
    noReferenceInputStep03Validator,
    noReferenceInputStep04Validator,
 )
import Midgard.Validators.FraudProofs.ReferenceInputNoIdx (
    referenceInputNoIdxStep01Validator,
    referenceInputNoIdxStep02Validator,
    referenceInputNoIdxStep03Validator,
    referenceInputNoIdxStep04Validator,
 )
import Midgard.Validators.FraudProofs.TransitionTrace (
    transitionTraceAcceptedTransactionV1Validator,
    transitionTraceControlV1Validator,
    transitionTraceDepositV1Validator,
    transitionTraceDuplicateV1Validator,
    transitionTraceForcedV1Validator,
    transitionTraceL1EventV1Validator,
    transitionTraceRouteV1Validator,
    transitionTraceSourceV1Validator,
    transitionTraceWithdrawalV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.AwardTimeout (
    awardV1Validator,
    timeoutV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.Boundary (boundaryV1Validator)
import Midgard.Validators.FraudProofs.ValidationTrace.CanonicalDecodeEmpty (
    canonicalDecodeEmptySemanticV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.CanonicalDecodeItem (
    canonicalDecodeItemProofV1Validator,
    canonicalDecodeItemSettlementV1Validator,
    canonicalDecodeItemSourceV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.CanonicalDecodeItemEvidence (
    canonicalDecodeItemObserveV1Validator,
    canonicalDecodeItemSemanticV1Validator,
    canonicalDecodeProofItemV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.CanonicalDecodePrepare (
    canonicalDecodeV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.Cek (cekV1Validator)
import Midgard.Validators.FraudProofs.ValidationTrace.CekSemantics (
    cekContextStepSemanticV1Validator,
    cekCoreStepSemanticV1Validator,
    cekExecutionSelectionSemanticV1Validator,
    cekFinishSemanticV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.CompactBinding (
    compactBindingSemanticV1Validator,
    compactBindingV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.Dispute (disputeV1Validator)
import Midgard.Validators.FraudProofs.ValidationTrace.Game (gameV1Validator)
import Midgard.Validators.FraudProofs.ValidationTrace.InputSets (
    inputSetsEmptySemanticV1Validator,
    inputSetsItemSemanticV1Validator,
    inputSetsV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.LedgerDelta (
    ledgerDeltaFinalizeSemanticV1Validator,
    ledgerDeltaOperationSemanticV1Validator,
    ledgerDeltaOutputFinishSemanticV1Validator,
    ledgerDeltaOutputSemanticV1Validator,
    ledgerDeltaProofFrameSemanticV1Validator,
    ledgerDeltaReplayFinishSemanticV1Validator,
    ledgerDeltaReplaySemanticV1Validator,
    ledgerDeltaTerminalSemanticV1Validator,
    ledgerDeltaV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.NativeScripts (
    nativeScriptsEffectfulSemanticV1Validator,
    nativeScriptsNativeSemanticV1Validator,
    nativeScriptsTerminalSemanticV1Validator,
    nativeScriptsV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativePayloads (
    phaseANativeAllOrAnyContainerFramePayloadSemanticV1Validator,
    phaseANativeAllOrAnyEmptyContainerPayloadSemanticV1Validator,
    phaseANativeAtLeastContainerFramePayloadSemanticV1Validator,
    phaseANativeAtLeastEmptyContainerPayloadSemanticV1Validator,
    phaseANativeTimelockPayloadSemanticV1Validator,
    phaseANativeTokenHeadSemanticV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativeScripts (
    phaseANativeAdvanceSemanticV1Validator,
    phaseANativeFrameSemanticV1Validator,
    phaseANativeItemSemanticV1Validator,
    phaseANativeScriptsV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativeSignatures (
    phaseANativeSignatureAboveLastPayloadSemanticV1Validator,
    phaseANativeSignatureBelowFirstPayloadSemanticV1Validator,
    phaseANativeSignatureBetweenPayloadSemanticV1Validator,
    phaseANativeSignatureEmptyPayloadSemanticV1Validator,
    phaseANativeSignatureMembershipPayloadSemanticV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.PhaseAScriptPreconditions (
    phaseAScriptPreconditionsItemSemanticV1Validator,
    phaseAScriptPreconditionsSemanticV1Validator,
    phaseAScriptPreconditionsV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.ResolveInputs (
    resolveInputsFinishSemanticV1Validator,
    resolveInputsInitialSemanticV1Validator,
    resolveInputsMembershipBeginSemanticV1Validator,
    resolveInputsMembershipFinalizeSemanticV1Validator,
    resolveInputsMembershipStepSemanticV1Validator,
    resolveInputsNonMembershipSemanticV1Validator,
    resolveInputsV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.ScriptIntegrity (
    scriptIntegrityAuthenticationSemanticV1Validator,
    scriptIntegrityCompactSemanticV1Validator,
    scriptIntegrityFinalizeSemanticV1Validator,
    scriptIntegrityV1Validator,
    scriptIntegrityWitnessSetSemanticV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesRedeemerNormalization (
    scriptSourcesRedeemerEnvelopeV1Validator,
    scriptSourcesRedeemerExecutionSettlementV1Validator,
    scriptSourcesRedeemerFinalizeFrameExecutorV1Validator,
    scriptSourcesRedeemerFoldMapExecutorV1Validator,
    scriptSourcesRedeemerOuterNormalizerV1Validator,
    scriptSourcesRedeemerTraversalNormalizerV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageEight (
    scriptSourcesStageEightFinishSemanticV1Validator,
    scriptSourcesStageEightPurposeSemanticV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageEleven (
    scriptSourcesStageElevenFinishSemanticV1Validator,
    scriptSourcesStageElevenSourceSemanticV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageNine (
    scriptSourcesStageNineEffectfulMatchSemanticV1Validator,
    scriptSourcesStageNineMismatchSemanticV1Validator,
    scriptSourcesStageNineMissingSemanticV1Validator,
    scriptSourcesStageNineNativeMatchSemanticV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageOne (
    scriptSourcesStageOneFinishSemanticV1Validator,
    scriptSourcesStageOneRedeemerSemanticV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageSeven (
    scriptSourcesStageSevenFinishSemanticV1Validator,
    scriptSourcesStageSevenObserverSemanticV1Validator,
    scriptSourcesStageSevenReceiveSemanticV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageTen (
    scriptSourcesStageTenMatchSemanticV1Validator,
    scriptSourcesStageTenMismatchSemanticV1Validator,
    scriptSourcesStageTenMissingSemanticV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageTwelve (
    scriptSourcesStageTwelveFinishSemanticV1Validator,
    scriptSourcesStageTwelveRedeemerSemanticV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageZero (
    scriptSourcesStageZeroBeginSemanticV1Validator,
    scriptSourcesStageZeroFinishSemanticV1Validator,
    scriptSourcesStageZeroHashAdvanceSemanticV1Validator,
    scriptSourcesStageZeroHashBlockSemanticV1Validator,
    scriptSourcesStageZeroHashTerminalSemanticV1Validator,
    scriptSourcesV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStagesTwoToSix (
    scriptSourcesNonOutputSemanticV1Validator,
    scriptSourcesOutputProofBeginSemanticV1Validator,
    scriptSourcesOutputProofFinalizeSemanticV1Validator,
    scriptSourcesOutputProofFinishSemanticV1Validator,
    scriptSourcesOutputProofStepSemanticV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.Signatures (
    signaturesAddressItemSemanticV1Validator,
    signaturesAdvanceSemanticV1Validator,
    signaturesHandoffSemanticV1Validator,
    signaturesRequiredItemSemanticV1Validator,
    signaturesV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.Source (sourceV1Validator)
import Midgard.Validators.FraudProofs.ValidationTrace.StaticLedgerRules (
    staticLedgerRulesSemanticV1Validator,
    staticLedgerRulesV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.ValueAndMint (valueAndMintV1Validator)
import Midgard.Validators.FraudProofs.ValidationTrace.ValueAndMintSemantics (
    valueAndMintBeginSemanticV1Validator,
    valueAndMintFinalizeSemanticV1Validator,
    valueAndMintMintAssetSemanticV1Validator,
    valueAndMintMintFinishSemanticV1Validator,
    valueAndMintOutputAssetSemanticV1Validator,
    valueAndMintOutputDescriptorSemanticV1Validator,
    valueAndMintOutputFinishSemanticV1Validator,
    valueAndMintReplayAssetSemanticV1Validator,
    valueAndMintReplayBeginSemanticV1Validator,
    valueAndMintReplayFinishSemanticV1Validator,
    valueAndMintReplayInputSemanticV1Validator,
 )
import Midgard.Validators.FraudProofs.ValueNotPreserved (
    valueNotPreservedStep01Validator,
    valueNotPreservedStep02Validator,
    valueNotPreservedStep03Validator,
    valueNotPreservedStep04Validator,
 )
import Midgard.Validators.FraudProofs.WithdrawalMistag (
    withdrawalMistagStep01Validator,
    withdrawalMistagStep02Validator,
    withdrawalMistagStep03Validator,
    withdrawalMistagStep04Validator,
    withdrawalMistagStep05Validator,
 )
import Midgard.Validators.FraudProofs.WithdrawnInput (
    withdrawnInputStep01Validator,
    withdrawnInputStep02Validator,
    withdrawnInputStep03Validator,
 )
import Midgard.Validators.FraudProofs.WithdrawnReferenceInput (
    withdrawnReferenceInputStep01Validator,
    withdrawnReferenceInputStep02Validator,
    withdrawnReferenceInputStep03Validator,
 )
import Midgard.Validators.FraudProofs.ZeroInput (
    zeroInputStep01Validator,
    zeroInputStep02Validator,
 )
import Midgard.Validators.HubOracle (hubOracleMintValidator)
import Midgard.Validators.MpfChunkedVerify (mpfChunkedVerifyStakeValidator)
import Midgard.Validators.Payout (payoutMintValidator, payoutSpendValidator)
import Midgard.Validators.RegisteredOperators (
    registeredOperatorsMintValidator,
    registeredOperatorsSpendValidator,
 )
import Midgard.Validators.Reserve (reserveSpendValidator, reserveWithdrawValidator)
import Midgard.Validators.RetiredOperators (
    retiredOperatorsMintValidator,
    retiredOperatorsSpendValidator,
 )
import Midgard.Validators.Scheduler (schedulerMintValidator, schedulerSpendValidator)
import Midgard.Validators.Settlement (settlementMintValidator, settlementSpendValidator)
import Midgard.Validators.StateQueue (stateQueueMintValidator, stateQueueSpendValidator)
import Midgard.Validators.TxOrder (txOrderMintValidator, txOrderSpendValidator)
import Midgard.Validators.Withdrawal (withdrawalMintValidator, withdrawalSpendValidator)
import Midgard.Validators.Witness (witnessPublishValidator)
import Plutarch.Internal.Term
import Plutarch.Script (serialiseScript)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

{- | Runs the module's entrypoint.
| Writes compiled Plutus scripts to disk for local inspection.
-}
main :: IO ()
main = do
    putStrLn $ "Compiled Aiken environment: " <> environmentName
    args <- getArgs
    case args of
        ["only"] -> error "only requires one or more generated script paths"
        ("only" : _) -> writeAllScripts
        ["membership"] -> writeMembershipScripts
        ["min-ada"] -> writeMinAdaScripts
        ["da-params-governor"] -> do
            createDirectoryIfMissing True "generated"
            writePlutusScriptNoTrace
                "midgard.da_params_governor.mint.unapplied"
                "generated/da-params-governor-mint.unapplied.plutus.json"
                daParamsGovernorValidator
            writePlutusScriptNoTrace
                "midgard.da_params_governor.spend.unapplied"
                "generated/da-params-governor-spend.unapplied.plutus.json"
                daParamsGovernorValidator
        ["da-attestation"] -> do
            createDirectoryIfMissing True "generated"
            writePlutusScriptNoTrace
                "midgard.da_attestation.mint.unapplied"
                "generated/da-attestation-mint.unapplied.plutus.json"
                daAttestationValidator
            writePlutusScriptNoTrace
                "midgard.da_attestation.spend.unapplied"
                "generated/da-attestation-spend.unapplied.plutus.json"
                daAttestationValidator
        ["availability-challenge"] -> do
            createDirectoryIfMissing True "generated"
            writePlutusScriptNoTrace
                "midgard.availability_challenge.mint.unapplied"
                "generated/availability-challenge-mint.unapplied.plutus.json"
                availabilityChallengeValidator
            writePlutusScriptNoTrace
                "midgard.availability_challenge.spend.unapplied"
                "generated/availability-challenge-spend.unapplied.plutus.json"
                availabilityChallengeValidator
        ["correction-lock"] -> do
            createDirectoryIfMissing True "generated"
            writePlutusScriptNoTrace
                "midgard.correction_lock.spend.unapplied"
                "generated/correction-lock-spend.unapplied.plutus.json"
                correctionLockSpendValidator
        ["linked-list-contracts"] -> do
            createDirectoryIfMissing True "generated"
            writePlutusScriptNoTrace
                "midgard.active_operators.mint.unapplied"
                "generated/active-operators-mint.unapplied.plutus.json"
                activeOperatorsMintValidator
            writePlutusScriptNoTrace
                "midgard.active_operators.spend.unapplied"
                "generated/active-operators-spend.unapplied.plutus.json"
                activeOperatorsSpendValidator
            writePlutusScriptNoTrace
                "midgard.registered_operators.mint.unapplied"
                "generated/registered-operators-mint.unapplied.plutus.json"
                registeredOperatorsMintValidator
            writePlutusScriptNoTrace
                "midgard.registered_operators.spend.unapplied"
                "generated/registered-operators-spend.unapplied.plutus.json"
                registeredOperatorsSpendValidator
            writePlutusScriptNoTrace
                "midgard.retired_operators.mint.unapplied"
                "generated/retired-operators-mint.unapplied.plutus.json"
                retiredOperatorsMintValidator
            writePlutusScriptNoTrace
                "midgard.retired_operators.spend.unapplied"
                "generated/retired-operators-spend.unapplied.plutus.json"
                retiredOperatorsSpendValidator
            writePlutusScriptNoTrace
                "midgard.scheduler.mint.unapplied"
                "generated/scheduler-mint.unapplied.plutus.json"
                schedulerMintValidator
            writePlutusScriptNoTrace
                "midgard.scheduler.spend.unapplied"
                "generated/scheduler-spend.unapplied.plutus.json"
                schedulerSpendValidator
            writePlutusScriptNoTrace
                "midgard.state_queue.mint.unapplied"
                "generated/state-queue-mint.unapplied.plutus.json"
                stateQueueMintValidator
            writePlutusScriptNoTrace
                "midgard.state_queue.spend.unapplied"
                "generated/state-queue-spend.unapplied.plutus.json"
                stateQueueSpendValidator
        ["additional-fraud-proofs"] -> do
            createDirectoryIfMissing True "generated"
            writeAdditionalFraudProofScripts
        ["validation-trace-cek"] -> do
            createDirectoryIfMissing True "generated"
            writeValidationTraceCekSemanticScripts
            writeValidationTraceCekAggregateScript
        ["validation-trace-cek", "context-step"] -> do
            createDirectoryIfMissing True "generated"
            writeValidationTraceCekContextStepSemanticScript
        ["validation-trace-cek", "core-step"] -> do
            createDirectoryIfMissing True "generated"
            writeValidationTraceCekCoreStepSemanticScript
        ["validation-trace-cek", "aggregate"] -> do
            createDirectoryIfMissing True "generated"
            writeValidationTraceCekAggregateScript
        ["validation-trace-value-and-mint"] -> do
            createDirectoryIfMissing True "generated"
            writeValidationTraceValueAndMintSemanticScripts
            writePlutusScriptNoTrace
                "midgard.fraud_proofs.validation_trace.value_and_mint_v1.unapplied"
                "generated/fraud-proof-validation-trace-value-and-mint-v1.unapplied.plutus.json"
                valueAndMintV1Validator
        ["validation-trace-resolution"] -> do
            createDirectoryIfMissing True "generated"
            writeValidationTraceResolutionScripts False
        ["validation-trace-canonical-decode"] -> do
            createDirectoryIfMissing True "generated"
            writeValidationTraceCanonicalDecodeScripts
        ["validation-trace-input-ledger"] -> do
            createDirectoryIfMissing True "generated"
            writeValidationTraceInputLedgerScripts Nothing
        ["validation-trace-input-ledger", selectedIndex] -> do
            createDirectoryIfMissing True "generated"
            case readMaybe selectedIndex of
                Just index
                    | index >= 0 && index <= 15 ->
                        writeValidationTraceInputLedgerScripts $ Just index
                _ -> error "validation-trace-input-ledger index must be between 0 and 15"
        ["validation-trace-early-phases"] -> do
            createDirectoryIfMissing True "generated"
            writeValidationTraceEarlyPhaseScripts Nothing
        ["validation-trace-early-phases", selectedIndex] -> do
            createDirectoryIfMissing True "generated"
            case readMaybe selectedIndex of
                Just index
                    | index >= 0 && index <= 11 ->
                        writeValidationTraceEarlyPhaseScripts $ Just index
                _ -> error "validation-trace-early-phases index must be between 0 and 11"
        ["validation-trace-phase-a"] -> do
            createDirectoryIfMissing True "generated"
            writeValidationTracePhaseAScripts Nothing
        ["validation-trace-phase-a", selectedIndex] -> do
            createDirectoryIfMissing True "generated"
            case readMaybe selectedIndex of
                Just index
                    | index >= 0 && index <= 17 ->
                        writeValidationTracePhaseAScripts $ Just index
                _ -> error "validation-trace-phase-a index must be between 0 and 17"
        ["validation-trace-script-sources-early"] -> do
            createDirectoryIfMissing True "generated"
            writeValidationTraceScriptSourcesEarlyScripts Nothing
        ["validation-trace-script-sources-early", selectedIndex] -> do
            createDirectoryIfMissing True "generated"
            case readMaybe selectedIndex of
                Just index
                    | index >= 0 && index <= 12 ->
                        writeValidationTraceScriptSourcesEarlyScripts $ Just index
                _ -> error "validation-trace-script-sources-early index must be between 0 and 12"
        ["validation-trace-script-sources-late"] -> do
            createDirectoryIfMissing True "generated"
            writeValidationTraceScriptSourcesLateScripts Nothing
        ["validation-trace-script-sources-late", selectedIndex] -> do
            createDirectoryIfMissing True "generated"
            case readMaybe selectedIndex of
                Just index
                    | index >= 0 && index <= 15 ->
                        writeValidationTraceScriptSourcesLateScripts $ Just index
                _ -> error "validation-trace-script-sources-late index must be between 0 and 15"
        ["validation-trace-script-sources-redeemer"] -> do
            createDirectoryIfMissing True "generated"
            writeValidationTraceScriptSourcesRedeemerScripts Nothing
        ["validation-trace-script-sources-redeemer", selectedIndex] -> do
            createDirectoryIfMissing True "generated"
            case readMaybe selectedIndex of
                Just index
                    | index >= 0 && index <= 5 ->
                        writeValidationTraceScriptSourcesRedeemerScripts $ Just index
                _ -> error "validation-trace-script-sources-redeemer index must be between 0 and 5"
        ["validation-trace-native-script-integrity"] -> do
            createDirectoryIfMissing True "generated"
            writeValidationTraceNativeScriptIntegrityScripts Nothing
        ["validation-trace-native-script-integrity", selectedIndex] -> do
            createDirectoryIfMissing True "generated"
            case readMaybe selectedIndex of
                Just index
                    | index >= 0 && index <= 8 ->
                        writeValidationTraceNativeScriptIntegrityScripts $ Just index
                _ -> error "validation-trace-native-script-integrity index must be between 0 and 8"
        _ -> writeAllScripts

writeAllScripts :: IO ()
writeAllScripts = do
    writeMembershipScripts
    -- Unapplied: the two Aiken validator parameters (init UTxO, asset name) are
    -- still outstanding, so this is the deployable script only after
    -- 'Plutarch.Evaluate.applyArguments'.
    writePlutusScriptNoTrace
        "midgard.hub_oracle.mint.unapplied"
        "generated/hub-oracle-mint.unapplied.plutus.json"
        hubOracleMintValidator

    writePlutusScriptNoTrace
        "midgard.fraud_proof_catalogue.mint.unapplied"
        "generated/fraud-proof-catalogue-mint.unapplied.plutus.json"
        fraudProofCatalogueMintValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proof_catalogue.spend"
        "generated/fraud-proof-catalogue-spend.plutus.json"
        fraudProofCatalogueSpendValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proof.mint.unapplied"
        "generated/fraud-proof-mint.unapplied.plutus.json"
        fraudProofMintValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proof.spend"
        "generated/fraud-proof-spend.plutus.json"
        fraudProofSpendValidator
    writePlutusScriptNoTrace
        "midgard.reserve.spend.unapplied"
        "generated/reserve-spend.unapplied.plutus.json"
        reserveSpendValidator
    writePlutusScriptNoTrace
        "midgard.reserve.withdraw"
        "generated/reserve-withdraw.plutus.json"
        reserveWithdrawValidator
    writePlutusScriptNoTrace
        "midgard.retired_operators.mint.unapplied"
        "generated/retired-operators-mint.unapplied.plutus.json"
        retiredOperatorsMintValidator
    writePlutusScriptNoTrace
        "midgard.retired_operators.spend.unapplied"
        "generated/retired-operators-spend.unapplied.plutus.json"
        retiredOperatorsSpendValidator
    writePlutusScriptNoTrace
        "midgard.deposit.mint.unapplied"
        "generated/deposit-mint.unapplied.plutus.json"
        depositMintValidator
    writePlutusScriptNoTrace
        "midgard.deposit.spend.unapplied"
        "generated/deposit-spend.unapplied.plutus.json"
        depositSpendValidator
    writePlutusScriptNoTrace
        "midgard.withdrawal.mint.unapplied"
        "generated/withdrawal-mint.unapplied.plutus.json"
        withdrawalMintValidator
    writePlutusScriptNoTrace
        "midgard.withdrawal.spend.unapplied"
        "generated/withdrawal-spend.unapplied.plutus.json"
        withdrawalSpendValidator
    writePlutusScriptNoTrace
        "midgard.settlement.mint.unapplied"
        "generated/settlement-mint.unapplied.plutus.json"
        settlementMintValidator
    writePlutusScriptNoTrace
        "midgard.settlement.spend.unapplied"
        "generated/settlement-spend.unapplied.plutus.json"
        settlementSpendValidator
    writePlutusScriptNoTrace
        "midgard.tx_order.mint.unapplied"
        "generated/tx-order-mint.unapplied.plutus.json"
        txOrderMintValidator
    writePlutusScriptNoTrace
        "midgard.tx_order.spend.unapplied"
        "generated/tx-order-spend.unapplied.plutus.json"
        txOrderSpendValidator
    writePlutusScriptNoTrace
        "midgard.cek_program_material.spend"
        "generated/cek-program-material-spend.plutus.json"
        cekProgramMaterialSpendValidator
    writePlutusScriptNoTrace
        "midgard.active_operators.mint.unapplied"
        "generated/active-operators-mint.unapplied.plutus.json"
        activeOperatorsMintValidator
    writePlutusScriptNoTrace
        "midgard.active_operators.spend.unapplied"
        "generated/active-operators-spend.unapplied.plutus.json"
        activeOperatorsSpendValidator
    writePlutusScriptNoTrace
        "midgard.registered_operators.mint.unapplied"
        "generated/registered-operators-mint.unapplied.plutus.json"
        registeredOperatorsMintValidator
    writePlutusScriptNoTrace
        "midgard.registered_operators.spend.unapplied"
        "generated/registered-operators-spend.unapplied.plutus.json"
        registeredOperatorsSpendValidator
    writePlutusScriptNoTrace
        "midgard.computation_thread.mint.unapplied"
        "generated/computation-thread-mint.unapplied.plutus.json"
        computationThreadMintValidator
    writePlutusScriptNoTrace
        "midgard.da_attestation.mint.unapplied"
        "generated/da-attestation-mint.unapplied.plutus.json"
        daAttestationValidator
    writePlutusScriptNoTrace
        "midgard.da_attestation.spend.unapplied"
        "generated/da-attestation-spend.unapplied.plutus.json"
        daAttestationValidator
    writePlutusScriptNoTrace
        "midgard.availability_challenge.mint.unapplied"
        "generated/availability-challenge-mint.unapplied.plutus.json"
        availabilityChallengeValidator
    writePlutusScriptNoTrace
        "midgard.availability_challenge.spend.unapplied"
        "generated/availability-challenge-spend.unapplied.plutus.json"
        availabilityChallengeValidator
    writePlutusScriptNoTrace
        "midgard.correction_lock.spend.unapplied"
        "generated/correction-lock-spend.unapplied.plutus.json"
        correctionLockSpendValidator
    writePlutusScriptNoTrace
        "midgard.da_params_governor.mint.unapplied"
        "generated/da-params-governor-mint.unapplied.plutus.json"
        daParamsGovernorValidator
    writePlutusScriptNoTrace
        "midgard.da_params_governor.spend.unapplied"
        "generated/da-params-governor-spend.unapplied.plutus.json"
        daParamsGovernorValidator
    writePlutusScriptNoTrace
        "midgard.field_preimage_certificate.mint"
        "generated/field-preimage-certificate-mint.plutus.json"
        fieldPreimageCertificateMintValidator
    writePlutusScriptNoTrace
        "midgard.field_preimage_certificate.spend"
        "generated/field-preimage-certificate-spend.plutus.json"
        fieldPreimageCertificateSpendValidator
    writePlutusScriptNoTrace
        "midgard.payout.mint.unapplied"
        "generated/payout-mint.unapplied.plutus.json"
        payoutMintValidator
    writePlutusScriptNoTrace
        "midgard.payout.spend.unapplied"
        "generated/payout-spend.unapplied.plutus.json"
        payoutSpendValidator
    writePlutusScriptNoTrace
        "midgard.scheduler.mint.unapplied"
        "generated/scheduler-mint.unapplied.plutus.json"
        schedulerMintValidator
    writePlutusScriptNoTrace
        "midgard.scheduler.spend.unapplied"
        "generated/scheduler-spend.unapplied.plutus.json"
        schedulerSpendValidator
    writePlutusScriptNoTrace
        "midgard.state_queue.mint.unapplied"
        "generated/state-queue-mint.unapplied.plutus.json"
        stateQueueMintValidator
    writePlutusScriptNoTrace
        "midgard.state_queue.spend.unapplied"
        "generated/state-queue-spend.unapplied.plutus.json"
        stateQueueSpendValidator
    writePlutusScriptNoTrace
        "midgard.user_event_witness.publish.unapplied"
        "generated/user-event-witness-publish.unapplied.plutus.json"
        witnessPublishValidator
    writePlutusScriptNoTrace
        "midgard.mpf_chunked_verify.withdraw"
        "generated/mpf-chunked-verify-withdraw.plutus.json"
        mpfChunkedVerifyStakeValidator
    writeAdditionalFraudProofScripts
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.da_hash_preimage.step_01.unapplied"
        "generated/fraud-proof-da-hash-preimage-step-01.unapplied.plutus.json"
        daHashPreimageStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.da_hash_preimage.step_02.unapplied"
        "generated/fraud-proof-da-hash-preimage-step-02.unapplied.plutus.json"
        daHashPreimageStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.invalid_range.step_01.unapplied"
        "generated/fraud-proof-invalid-range-step-01.unapplied.plutus.json"
        invalidRangeStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.invalid_range.step_02.unapplied"
        "generated/fraud-proof-invalid-range-step-02.unapplied.plutus.json"
        invalidRangeStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.invalid_signature.step_01.unapplied"
        "generated/fraud-proof-invalid-signature-step-01.unapplied.plutus.json"
        invalidSignatureStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.invalid_signature.step_02.unapplied"
        "generated/fraud-proof-invalid-signature-step-02.unapplied.plutus.json"
        invalidSignatureStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.min_fee.step_01.unapplied"
        "generated/fraud-proof-min-fee-step-01.unapplied.plutus.json"
        minFeeStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.min_fee.step_02.unapplied"
        "generated/fraud-proof-min-fee-step-02.unapplied.plutus.json"
        minFeeStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.zero_input.step_01.unapplied"
        "generated/fraud-proof-zero-input-step-01.unapplied.plutus.json"
        zeroInputStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.zero_input.step_02.unapplied"
        "generated/fraud-proof-zero-input-step-02.unapplied.plutus.json"
        zeroInputStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.double_spend.step_01.unapplied"
        "generated/fraud-proof-double-spend-step-01.unapplied.plutus.json"
        doubleSpendStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.double_spend.step_02.unapplied"
        "generated/fraud-proof-double-spend-step-02.unapplied.plutus.json"
        doubleSpendStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.double_spend.step_03.unapplied"
        "generated/fraud-proof-double-spend-step-03.unapplied.plutus.json"
        doubleSpendStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.double_spend.step_04.unapplied"
        "generated/fraud-proof-double-spend-step-04.unapplied.plutus.json"
        doubleSpendStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.input_no_idx.step_01.unapplied"
        "generated/fraud-proof-input-no-idx-step-01.unapplied.plutus.json"
        inputNoIdxStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.input_no_idx.step_02.unapplied"
        "generated/fraud-proof-input-no-idx-step-02.unapplied.plutus.json"
        inputNoIdxStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.input_no_idx.step_03.unapplied"
        "generated/fraud-proof-input-no-idx-step-03.unapplied.plutus.json"
        inputNoIdxStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.input_no_idx.step_04.unapplied"
        "generated/fraud-proof-input-no-idx-step-04.unapplied.plutus.json"
        inputNoIdxStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_signature.step_01.unapplied"
        "generated/fraud-proof-missing-signature-step-01.unapplied.plutus.json"
        missingSignatureStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_signature.step_02.unapplied"
        "generated/fraud-proof-missing-signature-step-02.unapplied.plutus.json"
        missingSignatureStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_signature.step_03.unapplied"
        "generated/fraud-proof-missing-signature-step-03.unapplied.plutus.json"
        missingSignatureStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_signature.step_04.unapplied"
        "generated/fraud-proof-missing-signature-step-04.unapplied.plutus.json"
        missingSignatureStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.no_input.step_01.unapplied"
        "generated/fraud-proof-no-input-step-01.unapplied.plutus.json"
        noInputStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.no_input.step_02.unapplied"
        "generated/fraud-proof-no-input-step-02.unapplied.plutus.json"
        noInputStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.no_input.step_03.unapplied"
        "generated/fraud-proof-no-input-step-03.unapplied.plutus.json"
        noInputStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.no_input.step_04.unapplied"
        "generated/fraud-proof-no-input-step-04.unapplied.plutus.json"
        noInputStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.no_reference_input.step_01.unapplied"
        "generated/fraud-proof-no-reference-input-step-01.unapplied.plutus.json"
        noReferenceInputStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.no_reference_input.step_02.unapplied"
        "generated/fraud-proof-no-reference-input-step-02.unapplied.plutus.json"
        noReferenceInputStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.no_reference_input.step_03.unapplied"
        "generated/fraud-proof-no-reference-input-step-03.unapplied.plutus.json"
        noReferenceInputStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.no_reference_input.step_04.unapplied"
        "generated/fraud-proof-no-reference-input-step-04.unapplied.plutus.json"
        noReferenceInputStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.reference_input_no_idx.step_01.unapplied"
        "generated/fraud-proof-reference-input-no-idx-step-01.unapplied.plutus.json"
        referenceInputNoIdxStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.reference_input_no_idx.step_02.unapplied"
        "generated/fraud-proof-reference-input-no-idx-step-02.unapplied.plutus.json"
        referenceInputNoIdxStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.reference_input_no_idx.step_03.unapplied"
        "generated/fraud-proof-reference-input-no-idx-step-03.unapplied.plutus.json"
        referenceInputNoIdxStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.reference_input_no_idx.step_04.unapplied"
        "generated/fraud-proof-reference-input-no-idx-step-04.unapplied.plutus.json"
        referenceInputNoIdxStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_native_script_tx.step_01.unapplied"
        "generated/fraud-proof-missing-native-script-tx-step-01.unapplied.plutus.json"
        missingNativeScriptTxStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_native_script_tx.step_02.unapplied"
        "generated/fraud-proof-missing-native-script-tx-step-02.unapplied.plutus.json"
        missingNativeScriptTxStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_native_script_tx.step_03.unapplied"
        "generated/fraud-proof-missing-native-script-tx-step-03.unapplied.plutus.json"
        missingNativeScriptTxStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_native_script_tx.step_04.unapplied"
        "generated/fraud-proof-missing-native-script-tx-step-04.unapplied.plutus.json"
        missingNativeScriptTxStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_native_script_tx.step_05.unapplied"
        "generated/fraud-proof-missing-native-script-tx-step-05.unapplied.plutus.json"
        missingNativeScriptTxStep05Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_native_script_tx.step_06.unapplied"
        "generated/fraud-proof-missing-native-script-tx-step-06.unapplied.plutus.json"
        missingNativeScriptTxStep06Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_native_script_tx.step_07.unapplied"
        "generated/fraud-proof-missing-native-script-tx-step-07.unapplied.plutus.json"
        missingNativeScriptTxStep07Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_native_script_tx.step_08.unapplied"
        "generated/fraud-proof-missing-native-script-tx-step-08.unapplied.plutus.json"
        missingNativeScriptTxStep08Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_native_script_utxo.step_01.unapplied"
        "generated/fraud-proof-missing-native-script-utxo-step-01.unapplied.plutus.json"
        missingNativeScriptUtxoStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_native_script_utxo.step_02.unapplied"
        "generated/fraud-proof-missing-native-script-utxo-step-02.unapplied.plutus.json"
        missingNativeScriptUtxoStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_native_script_utxo.step_03.unapplied"
        "generated/fraud-proof-missing-native-script-utxo-step-03.unapplied.plutus.json"
        missingNativeScriptUtxoStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_native_script_utxo.step_04.unapplied"
        "generated/fraud-proof-missing-native-script-utxo-step-04.unapplied.plutus.json"
        missingNativeScriptUtxoStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_native_script_utxo.step_05.unapplied"
        "generated/fraud-proof-missing-native-script-utxo-step-05.unapplied.plutus.json"
        missingNativeScriptUtxoStep05Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_native_script_utxo.step_06.unapplied"
        "generated/fraud-proof-missing-native-script-utxo-step-06.unapplied.plutus.json"
        missingNativeScriptUtxoStep06Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_native_script_utxo.step_07.unapplied"
        "generated/fraud-proof-missing-native-script-utxo-step-07.unapplied.plutus.json"
        missingNativeScriptUtxoStep07Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.value_not_preserved.step_01.unapplied"
        "generated/fraud-proof-value-not-preserved-step-01.unapplied.plutus.json"
        valueNotPreservedStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.value_not_preserved.step_02.unapplied"
        "generated/fraud-proof-value-not-preserved-step-02.unapplied.plutus.json"
        valueNotPreservedStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.value_not_preserved.step_03.unapplied"
        "generated/fraud-proof-value-not-preserved-step-03.unapplied.plutus.json"
        valueNotPreservedStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.value_not_preserved.step_04.unapplied"
        "generated/fraud-proof-value-not-preserved-step-04.unapplied.plutus.json"
        valueNotPreservedStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.withdrawal_mistag.step_01.unapplied"
        "generated/fraud-proof-withdrawal-mistag-step-01.unapplied.plutus.json"
        withdrawalMistagStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.withdrawal_mistag.step_02.unapplied"
        "generated/fraud-proof-withdrawal-mistag-step-02.unapplied.plutus.json"
        withdrawalMistagStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.withdrawal_mistag.step_03.unapplied"
        "generated/fraud-proof-withdrawal-mistag-step-03.unapplied.plutus.json"
        withdrawalMistagStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.withdrawal_mistag.step_04.unapplied"
        "generated/fraud-proof-withdrawal-mistag-step-04.unapplied.plutus.json"
        withdrawalMistagStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.withdrawal_mistag.step_05.unapplied"
        "generated/fraud-proof-withdrawal-mistag-step-05.unapplied.plutus.json"
        withdrawalMistagStep05Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.withdrawn_reference_input.step_01.unapplied"
        "generated/fraud-proof-withdrawn-reference-input-step-01.unapplied.plutus.json"
        withdrawnReferenceInputStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.withdrawn_reference_input.step_02.unapplied"
        "generated/fraud-proof-withdrawn-reference-input-step-02.unapplied.plutus.json"
        withdrawnReferenceInputStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.withdrawn_reference_input.step_03.unapplied"
        "generated/fraud-proof-withdrawn-reference-input-step-03.unapplied.plutus.json"
        withdrawnReferenceInputStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.control_v1.unapplied"
        "generated/fraud-proof-transition-trace-control-v1.unapplied.plutus.json"
        transitionTraceControlV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.source_v1.unapplied"
        "generated/fraud-proof-transition-trace-source-v1.unapplied.plutus.json"
        transitionTraceSourceV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.withdrawal_v1.unapplied"
        "generated/fraud-proof-transition-trace-withdrawal-v1.unapplied.plutus.json"
        transitionTraceWithdrawalV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.forced_v1.unapplied"
        "generated/fraud-proof-transition-trace-forced-v1.unapplied.plutus.json"
        transitionTraceForcedV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.accepted_transaction_v1.unapplied"
        "generated/fraud-proof-transition-trace-accepted-transaction-v1.unapplied.plutus.json"
        transitionTraceAcceptedTransactionV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.deposit_v1.unapplied"
        "generated/fraud-proof-transition-trace-deposit-v1.unapplied.plutus.json"
        transitionTraceDepositV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.l1_event_v1.unapplied"
        "generated/fraud-proof-transition-trace-l1-event-v1.unapplied.plutus.json"
        transitionTraceL1EventV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.duplicate_v1.unapplied"
        "generated/fraud-proof-transition-trace-duplicate-v1.unapplied.plutus.json"
        transitionTraceDuplicateV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.route_v1.unapplied"
        "generated/fraud-proof-transition-trace-route-v1.unapplied.plutus.json"
        transitionTraceRouteV1Validator
    writeValidationTraceCanonicalDecodeScripts
    writeValidationTraceEarlyPhaseScripts Nothing
    writeValidationTracePhaseAScripts Nothing
    writeValidationTraceScriptSourcesEarlyScripts Nothing
    writeValidationTraceScriptSourcesLateScripts Nothing
    writeValidationTraceScriptSourcesRedeemerScripts Nothing
    writeValidationTraceNativeScriptIntegrityScripts Nothing
    writeValidationTraceInputLedgerScripts Nothing
    writeValidationTraceResolutionScripts True

writeMembershipScripts :: IO ()
writeMembershipScripts = do
    putStrLn "Writing Plutarch membership scripts to files"
    createDirectoryIfMissing True "generated"
    writePlutusScriptNoTrace
        "midgard.plutarch.phas.membership_stake"
        "generated/membership-stake.plutus.json"
        membershipStakeValidator
    writePlutusScriptNoTrace
        "midgard.plutarch.pexcludes.non_membership_stake"
        "generated/non-membership-stake.plutus.json"
        nonMembershipStakeValidator

writeAdditionalFraudProofScripts :: IO ()
writeAdditionalFraudProofScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.mpf_chunked_proof.challenge.unapplied"
        "generated/fraud-proof-mpf-chunked-proof-challenge.unapplied.plutus.json"
        mpfChunkedChallengeValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.canonical_decodability.step_01.unapplied"
        "generated/fraud-proof-canonical-decodability-step-01.unapplied.plutus.json"
        canonicalDecodabilityStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.canonical_decodability.step_02.unapplied"
        "generated/fraud-proof-canonical-decodability-step-02.unapplied.plutus.json"
        canonicalDecodabilityStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.committed_field_shape.step_01.unapplied"
        "generated/fraud-proof-committed-field-shape-step-01.unapplied.plutus.json"
        committedFieldShapeStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.committed_field_shape.step_02.unapplied"
        "generated/fraud-proof-committed-field-shape-step-02.unapplied.plutus.json"
        committedFieldShapeStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.cross_block_duplicate_event.step_01.unapplied"
        "generated/fraud-proof-cross-block-duplicate-event-step-01.unapplied.plutus.json"
        crossBlockDuplicateEventStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.cross_block_duplicate_event.step_02.unapplied"
        "generated/fraud-proof-cross-block-duplicate-event-step-02.unapplied.plutus.json"
        crossBlockDuplicateEventStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.double_withdraw.step_01.unapplied"
        "generated/fraud-proof-double-withdraw-step-01.unapplied.plutus.json"
        doubleWithdrawStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.double_withdraw.step_02.unapplied"
        "generated/fraud-proof-double-withdraw-step-02.unapplied.plutus.json"
        doubleWithdrawStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.fabricated_deposit.step_01.unapplied"
        "generated/fraud-proof-fabricated-deposit-step-01.unapplied.plutus.json"
        fabricatedDepositStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.fabricated_deposit.step_02.unapplied"
        "generated/fraud-proof-fabricated-deposit-step-02.unapplied.plutus.json"
        fabricatedDepositStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.fabricated_deposit.step_03.unapplied"
        "generated/fraud-proof-fabricated-deposit-step-03.unapplied.plutus.json"
        fabricatedDepositStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.fabricated_deposit.step_04.unapplied"
        "generated/fraud-proof-fabricated-deposit-step-04.unapplied.plutus.json"
        fabricatedDepositStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.fabricated_withdrawal.step_01.unapplied"
        "generated/fraud-proof-fabricated-withdrawal-step-01.unapplied.plutus.json"
        fabricatedWithdrawalStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.fabricated_withdrawal.step_02.unapplied"
        "generated/fraud-proof-fabricated-withdrawal-step-02.unapplied.plutus.json"
        fabricatedWithdrawalStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.fabricated_withdrawal.step_03.unapplied"
        "generated/fraud-proof-fabricated-withdrawal-step-03.unapplied.plutus.json"
        fabricatedWithdrawalStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.fabricated_withdrawal.step_04.unapplied"
        "generated/fraud-proof-fabricated-withdrawal-step-04.unapplied.plutus.json"
        fabricatedWithdrawalStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.input_set_uniqueness.step_01.unapplied"
        "generated/fraud-proof-input-set-uniqueness-step-01.unapplied.plutus.json"
        inputSetUniquenessStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.input_set_uniqueness.step_02.unapplied"
        "generated/fraud-proof-input-set-uniqueness-step-02.unapplied.plutus.json"
        inputSetUniquenessStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.l2_tx_mistag.step_01.unapplied"
        "generated/fraud-proof-l2-tx-mistag-step-01.unapplied.plutus.json"
        l2TxMistagStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.l2_tx_mistag.step_02.unapplied"
        "generated/fraud-proof-l2-tx-mistag-step-02.unapplied.plutus.json"
        l2TxMistagStep02Validator
    writeMinAdaScripts
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.mint_authorization.step_01.unapplied"
        "generated/fraud-proof-mint-authorization-step-01.unapplied.plutus.json"
        mintAuthorizationStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.mint_authorization.step_02.unapplied"
        "generated/fraud-proof-mint-authorization-step-02.unapplied.plutus.json"
        mintAuthorizationStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.mint_authorization.step_03.unapplied"
        "generated/fraud-proof-mint-authorization-step-03.unapplied.plutus.json"
        mintAuthorizationStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.mint_authorization.step_04.unapplied"
        "generated/fraud-proof-mint-authorization-step-04.unapplied.plutus.json"
        mintAuthorizationStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.mint_authorization.step_05.unapplied"
        "generated/fraud-proof-mint-authorization-step-05.unapplied.plutus.json"
        mintAuthorizationStep05Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.native_script_decoding.step_01.unapplied"
        "generated/fraud-proof-native-script-decoding-step-01.unapplied.plutus.json"
        nativeScriptDecodingStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.native_script_decoding.step_02.unapplied"
        "generated/fraud-proof-native-script-decoding-step-02.unapplied.plutus.json"
        nativeScriptDecodingStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.native_script_decoding.step_03_open_subject.unapplied"
        "generated/fraud-proof-native-script-decoding-step-03-open-subject.unapplied.plutus.json"
        nativeScriptDecodingOpenSubjectValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.native_script_decoding.step_03_bind_descriptor.unapplied"
        "generated/fraud-proof-native-script-decoding-step-03-bind-descriptor.unapplied.plutus.json"
        nativeScriptDecodingBindDescriptorValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.native_script_decoding.step_03_advance_or_close.unapplied"
        "generated/fraud-proof-native-script-decoding-step-03-advance-or-close.unapplied.plutus.json"
        nativeScriptDecodingAdvanceOrCloseValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.native_script_decoding.step_04.unapplied"
        "generated/fraud-proof-native-script-decoding-step-04.unapplied.plutus.json"
        nativeScriptDecodingStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.native_script_invalid.step_01.unapplied"
        "generated/fraud-proof-native-script-invalid-step-01.unapplied.plutus.json"
        nativeScriptInvalidStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.native_script_invalid.step_02.unapplied"
        "generated/fraud-proof-native-script-invalid-step-02.unapplied.plutus.json"
        nativeScriptInvalidStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.native_script_invalid.step_03.unapplied"
        "generated/fraud-proof-native-script-invalid-step-03.unapplied.plutus.json"
        nativeScriptInvalidStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.native_script_invalid.step_04.unapplied"
        "generated/fraud-proof-native-script-invalid-step-04.unapplied.plutus.json"
        nativeScriptInvalidStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.native_script_invalid.step_05.unapplied"
        "generated/fraud-proof-native-script-invalid-step-05.unapplied.plutus.json"
        nativeScriptInvalidStep05Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.network_id.step_01.unapplied"
        "generated/fraud-proof-network-id-step-01.unapplied.plutus.json"
        networkIdStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.network_id.step_02.unapplied"
        "generated/fraud-proof-network-id-step-02.unapplied.plutus.json"
        networkIdStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.withdrawn_input.step_01.unapplied"
        "generated/fraud-proof-withdrawn-input-step-01.unapplied.plutus.json"
        withdrawnInputStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.withdrawn_input.step_02.unapplied"
        "generated/fraud-proof-withdrawn-input-step-02.unapplied.plutus.json"
        withdrawnInputStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.withdrawn_input.step_03.unapplied"
        "generated/fraud-proof-withdrawn-input-step-03.unapplied.plutus.json"
        withdrawnInputStep03Validator

writeValidationTraceCanonicalDecodeScripts :: IO ()
writeValidationTraceCanonicalDecodeScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.canonical_decode_empty_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-canonical-decode-empty-semantic-v1.unapplied.plutus.json"
        canonicalDecodeEmptySemanticV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.canonical_decode_item_source_v1.unapplied"
        "generated/fraud-proof-validation-trace-canonical-decode-item-source-v1.unapplied.plutus.json"
        canonicalDecodeItemSourceV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.canonical_decode_item_observe_v1.unapplied"
        "generated/fraud-proof-validation-trace-canonical-decode-item-observe-v1.unapplied.plutus.json"
        canonicalDecodeItemObserveV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.canonical_decode_item_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-canonical-decode-item-semantic-v1.unapplied.plutus.json"
        canonicalDecodeItemSemanticV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.canonical_decode_item_proof_v1.unapplied"
        "generated/fraud-proof-validation-trace-canonical-decode-item-proof-v1.unapplied.plutus.json"
        canonicalDecodeItemProofV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.canonical_decode_item_settlement_v1.unapplied"
        "generated/fraud-proof-validation-trace-canonical-decode-item-settlement-v1.unapplied.plutus.json"
        canonicalDecodeItemSettlementV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.canonical_decode_v1.unapplied"
        "generated/fraud-proof-validation-trace-canonical-decode-v1.unapplied.plutus.json"
        canonicalDecodeV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.proof_item_v1"
        "generated/fraud-proof-validation-trace-proof-item-v1.plutus.json"
        canonicalDecodeProofItemV1Validator

writeValidationTraceEarlyPhaseScripts :: Maybe Int -> IO ()
writeValidationTraceEarlyPhaseScripts selectedIndex = do
    let writeAt index action = when (maybe True (== index) selectedIndex) action
    writeAt 0 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.input_sets_v1.unapplied"
            "generated/fraud-proof-validation-trace-input-sets-v1.unapplied.plutus.json"
            inputSetsV1Validator
    writeAt 1 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.input_sets_empty_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-input-sets-empty-semantic-v1.unapplied.plutus.json"
            inputSetsEmptySemanticV1Validator
    writeAt 2 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.input_sets_item_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-input-sets-item-semantic-v1.unapplied.plutus.json"
            inputSetsItemSemanticV1Validator
    writeAt 3 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.compact_binding_v1.unapplied"
            "generated/fraud-proof-validation-trace-compact-binding-v1.unapplied.plutus.json"
            compactBindingV1Validator
    writeAt 4 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.compact_binding_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-compact-binding-semantic-v1.unapplied.plutus.json"
            compactBindingSemanticV1Validator
    writeAt 5 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.static_ledger_rules_v1.unapplied"
            "generated/fraud-proof-validation-trace-static-ledger-rules-v1.unapplied.plutus.json"
            staticLedgerRulesV1Validator
    writeAt 6 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.static_ledger_rules_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-static-ledger-rules-semantic-v1.unapplied.plutus.json"
            staticLedgerRulesSemanticV1Validator
    writeAt 7 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.signatures_v1.unapplied"
            "generated/fraud-proof-validation-trace-signatures-v1.unapplied.plutus.json"
            signaturesV1Validator
    writeAt 8 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.signatures_advance_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-signatures-advance-semantic-v1.unapplied.plutus.json"
            signaturesAdvanceSemanticV1Validator
    writeAt 9 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.signatures_handoff_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-signatures-handoff-semantic-v1.unapplied.plutus.json"
            signaturesHandoffSemanticV1Validator
    writeAt 10 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.signatures_address_item_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-signatures-address-item-semantic-v1.unapplied.plutus.json"
            signaturesAddressItemSemanticV1Validator
    writeAt 11 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.signatures_required_item_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-signatures-required-item-semantic-v1.unapplied.plutus.json"
            signaturesRequiredItemSemanticV1Validator

writeValidationTracePhaseAScripts :: Maybe Int -> IO ()
writeValidationTracePhaseAScripts selectedIndex = do
    let writeAt index action = when (maybe True (== index) selectedIndex) action
    writeAt 0 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_native_scripts_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-native-scripts-v1.unapplied.plutus.json"
            phaseANativeScriptsV1Validator
    writeAt 1 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_native_scripts_advance_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-native-scripts-advance-semantic-v1.unapplied.plutus.json"
            phaseANativeAdvanceSemanticV1Validator
    writeAt 2 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_native_scripts_item_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-native-scripts-item-semantic-v1.unapplied.plutus.json"
            phaseANativeItemSemanticV1Validator
    writeAt 3 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_native_scripts_frame_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-native-scripts-frame-semantic-v1.unapplied.plutus.json"
            phaseANativeFrameSemanticV1Validator
    writeAt 4 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_native_scripts_token_head_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-native-scripts-token-head-semantic-v1.unapplied.plutus.json"
            phaseANativeTokenHeadSemanticV1Validator
    writeAt 5 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_native_scripts_timelock_payload_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-native-scripts-timelock-payload-semantic-v1.unapplied.plutus.json"
            phaseANativeTimelockPayloadSemanticV1Validator
    writeAt 6 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_native_scripts_all_or_any_container_frame_payload_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-native-scripts-all-or-any-container-frame-payload-semantic-v1.unapplied.plutus.json"
            phaseANativeAllOrAnyContainerFramePayloadSemanticV1Validator
    writeAt 7 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_native_scripts_all_or_any_empty_container_payload_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-native-scripts-all-or-any-empty-container-payload-semantic-v1.unapplied.plutus.json"
            phaseANativeAllOrAnyEmptyContainerPayloadSemanticV1Validator
    writeAt 8 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_native_scripts_at_least_container_frame_payload_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-native-scripts-at-least-container-frame-payload-semantic-v1.unapplied.plutus.json"
            phaseANativeAtLeastContainerFramePayloadSemanticV1Validator
    writeAt 9 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_native_scripts_at_least_empty_container_payload_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-native-scripts-at-least-empty-container-payload-semantic-v1.unapplied.plutus.json"
            phaseANativeAtLeastEmptyContainerPayloadSemanticV1Validator
    writeAt 10 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_native_scripts_signature_membership_payload_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-native-scripts-signature-membership-payload-semantic-v1.unapplied.plutus.json"
            phaseANativeSignatureMembershipPayloadSemanticV1Validator
    writeAt 11 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_native_scripts_signature_empty_payload_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-native-scripts-signature-empty-payload-semantic-v1.unapplied.plutus.json"
            phaseANativeSignatureEmptyPayloadSemanticV1Validator
    writeAt 12 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_native_scripts_signature_below_first_payload_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-native-scripts-signature-below-first-payload-semantic-v1.unapplied.plutus.json"
            phaseANativeSignatureBelowFirstPayloadSemanticV1Validator
    writeAt 13 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_native_scripts_signature_above_last_payload_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-native-scripts-signature-above-last-payload-semantic-v1.unapplied.plutus.json"
            phaseANativeSignatureAboveLastPayloadSemanticV1Validator
    writeAt 14 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_native_scripts_signature_between_payload_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-native-scripts-signature-between-payload-semantic-v1.unapplied.plutus.json"
            phaseANativeSignatureBetweenPayloadSemanticV1Validator
    writeAt 15 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_script_preconditions_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-script-preconditions-v1.unapplied.plutus.json"
            phaseAScriptPreconditionsV1Validator
    writeAt 16 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_script_preconditions_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-script-preconditions-semantic-v1.unapplied.plutus.json"
            phaseAScriptPreconditionsSemanticV1Validator
    writeAt 17 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_script_preconditions_item_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-script-preconditions-item-semantic-v1.unapplied.plutus.json"
            phaseAScriptPreconditionsItemSemanticV1Validator

writeValidationTraceScriptSourcesEarlyScripts :: Maybe Int -> IO ()
writeValidationTraceScriptSourcesEarlyScripts selectedIndex = do
    let writeAt index action = when (maybe True (== index) selectedIndex) action
    writeAt 0 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-v1.unapplied.plutus.json"
            scriptSourcesV1Validator
    writeAt 1 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_zero_finish_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-zero-finish-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageZeroFinishSemanticV1Validator
    writeAt 2 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_zero_begin_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-zero-begin-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageZeroBeginSemanticV1Validator
    writeAt 3 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_zero_hash_block_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-zero-hash-block-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageZeroHashBlockSemanticV1Validator
    writeAt 4 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_zero_hash_advance_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-zero-hash-advance-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageZeroHashAdvanceSemanticV1Validator
    writeAt 5 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_zero_hash_terminal_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-zero-hash-terminal-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageZeroHashTerminalSemanticV1Validator
    writeAt 6 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_finish_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-finish-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageOneFinishSemanticV1Validator
    writeAt 7 $
        writePlutusScriptAllFieldsNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageOneRedeemerSemanticV1Validator
    writeAt 8 $
        writePlutusScriptAllFieldsNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_non_output_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-non-output-semantic-v1.unapplied.plutus.json"
            scriptSourcesNonOutputSemanticV1Validator
    writeAt 9 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_output_proof_begin_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-output-proof-begin-semantic-v1.unapplied.plutus.json"
            scriptSourcesOutputProofBeginSemanticV1Validator
    writeAt 10 $
        writePlutusScriptAllFieldsNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_output_proof_step_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-output-proof-step-semantic-v1.unapplied.plutus.json"
            scriptSourcesOutputProofStepSemanticV1Validator
    writeAt 11 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_output_proof_finalize_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-output-proof-finalize-semantic-v1.unapplied.plutus.json"
            scriptSourcesOutputProofFinalizeSemanticV1Validator
    writeAt 12 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_output_proof_finish_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-output-proof-finish-semantic-v1.unapplied.plutus.json"
            scriptSourcesOutputProofFinishSemanticV1Validator

writeValidationTraceScriptSourcesLateScripts :: Maybe Int -> IO ()
writeValidationTraceScriptSourcesLateScripts selectedIndex = do
    let writeAt index action = when (maybe True (== index) selectedIndex) action
    writeAt 0 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_seven_observer_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-seven-observer-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageSevenObserverSemanticV1Validator
    writeAt 1 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_seven_receive_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-seven-receive-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageSevenReceiveSemanticV1Validator
    writeAt 2 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_seven_finish_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-seven-finish-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageSevenFinishSemanticV1Validator
    writeAt 3 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_eight_finish_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-eight-finish-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageEightFinishSemanticV1Validator
    writeAt 4 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_eight_purpose_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-eight-purpose-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageEightPurposeSemanticV1Validator
    writeAt 5 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_nine_missing_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-nine-missing-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageNineMissingSemanticV1Validator
    writeAt 6 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_nine_mismatch_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-nine-mismatch-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageNineMismatchSemanticV1Validator
    writeAt 7 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_nine_native_match_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-nine-native-match-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageNineNativeMatchSemanticV1Validator
    writeAt 8 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_nine_effectful_match_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-nine-effectful-match-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageNineEffectfulMatchSemanticV1Validator
    writeAt 9 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_ten_missing_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-ten-missing-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageTenMissingSemanticV1Validator
    writeAt 10 $
        writePlutusScriptAllFieldsNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_ten_match_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-ten-match-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageTenMatchSemanticV1Validator
    writeAt 11 $
        writePlutusScriptAllFieldsNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_ten_mismatch_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-ten-mismatch-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageTenMismatchSemanticV1Validator
    writeAt 12 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_eleven_finish_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-eleven-finish-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageElevenFinishSemanticV1Validator
    writeAt 13 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_eleven_source_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-eleven-source-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageElevenSourceSemanticV1Validator
    writeAt 14 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_twelve_finish_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-twelve-finish-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageTwelveFinishSemanticV1Validator
    writeAt 15 $
        writePlutusScriptAllFieldsNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_twelve_redeemer_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-twelve-redeemer-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageTwelveRedeemerSemanticV1Validator

writeValidationTraceScriptSourcesRedeemerScripts :: Maybe Int -> IO ()
writeValidationTraceScriptSourcesRedeemerScripts selectedIndex = do
    let writeAt index action = when (maybe True (== index) selectedIndex) action
    writeAt 0 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_envelope_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-envelope-v1.unapplied.plutus.json"
            scriptSourcesRedeemerEnvelopeV1Validator
    writeAt 1 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_traversal_normalizer_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-traversal-normalizer-v1.unapplied.plutus.json"
            scriptSourcesRedeemerTraversalNormalizerV1Validator
    writeAt 2 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_outer_normalizer_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-outer-normalizer-v1.unapplied.plutus.json"
            scriptSourcesRedeemerOuterNormalizerV1Validator
    writeAt 3 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_fold_map_executor_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-fold-map-executor-v1.unapplied.plutus.json"
            scriptSourcesRedeemerFoldMapExecutorV1Validator
    writeAt 4 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_finalize_frame_executor_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-finalize-frame-executor-v1.unapplied.plutus.json"
            scriptSourcesRedeemerFinalizeFrameExecutorV1Validator
    writeAt 5 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_execution_settlement_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-execution-settlement-v1.unapplied.plutus.json"
            scriptSourcesRedeemerExecutionSettlementV1Validator

writeValidationTraceNativeScriptIntegrityScripts :: Maybe Int -> IO ()
writeValidationTraceNativeScriptIntegrityScripts selectedIndex = do
    let writeAt index action = when (maybe True (== index) selectedIndex) action
    writeAt 0 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.native_scripts_v1.unapplied"
            "generated/fraud-proof-validation-trace-native-scripts-v1.unapplied.plutus.json"
            nativeScriptsV1Validator
    writeAt 1 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.native_scripts_terminal_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-native-scripts-terminal-semantic-v1.unapplied.plutus.json"
            nativeScriptsTerminalSemanticV1Validator
    writeAt 2 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.native_scripts_effectful_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-native-scripts-effectful-semantic-v1.unapplied.plutus.json"
            nativeScriptsEffectfulSemanticV1Validator
    writeAt 3 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.native_scripts_native_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-native-scripts-native-semantic-v1.unapplied.plutus.json"
            nativeScriptsNativeSemanticV1Validator
    writeAt 4 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_integrity_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-integrity-v1.unapplied.plutus.json"
            scriptIntegrityV1Validator
    writeAt 5 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_integrity_authentication_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-integrity-authentication-semantic-v1.unapplied.plutus.json"
            scriptIntegrityAuthenticationSemanticV1Validator
    writeAt 6 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_integrity_compact_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-integrity-compact-semantic-v1.unapplied.plutus.json"
            scriptIntegrityCompactSemanticV1Validator
    writeAt 7 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_integrity_witness_set_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-integrity-witness-set-semantic-v1.unapplied.plutus.json"
            scriptIntegrityWitnessSetSemanticV1Validator
    writeAt 8 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_integrity_finalize_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-integrity-finalize-semantic-v1.unapplied.plutus.json"
            scriptIntegrityFinalizeSemanticV1Validator

writeValidationTraceInputLedgerScripts :: Maybe Int -> IO ()
writeValidationTraceInputLedgerScripts selectedIndex = do
    let writeAt index action = when (maybe True (== index) selectedIndex) action
    writeAt 0 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.resolve_inputs_v1.unapplied"
            "generated/fraud-proof-validation-trace-resolve-inputs-v1.unapplied.plutus.json"
            resolveInputsV1Validator
    writeAt 1 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.resolve_inputs_initial_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-resolve-inputs-initial-semantic-v1.unapplied.plutus.json"
            resolveInputsInitialSemanticV1Validator
    writeAt 2 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.resolve_inputs_finish_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-resolve-inputs-finish-semantic-v1.unapplied.plutus.json"
            resolveInputsFinishSemanticV1Validator
    writeAt 3 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.resolve_inputs_membership_begin_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-resolve-inputs-membership-begin-semantic-v1.unapplied.plutus.json"
            resolveInputsMembershipBeginSemanticV1Validator
    writeAt 4 $
        writePlutusScriptAllFieldsNoTrace
            "midgard.fraud_proofs.validation_trace.resolve_inputs_membership_step_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-resolve-inputs-membership-step-semantic-v1.unapplied.plutus.json"
            resolveInputsMembershipStepSemanticV1Validator
    writeAt 5 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.resolve_inputs_membership_finalize_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-resolve-inputs-membership-finalize-semantic-v1.unapplied.plutus.json"
            resolveInputsMembershipFinalizeSemanticV1Validator
    writeAt 6 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.resolve_inputs_non_membership_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-resolve-inputs-non-membership-semantic-v1.unapplied.plutus.json"
            resolveInputsNonMembershipSemanticV1Validator
    writeAt 7 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.ledger_delta_v1.unapplied"
            "generated/fraud-proof-validation-trace-ledger-delta-v1.unapplied.plutus.json"
            ledgerDeltaV1Validator
    writeAt 8 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.ledger_delta_replay_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-ledger-delta-replay-semantic-v1.unapplied.plutus.json"
            ledgerDeltaReplaySemanticV1Validator
    writeAt 9 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.ledger_delta_replay_finish_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-ledger-delta-replay-finish-semantic-v1.unapplied.plutus.json"
            ledgerDeltaReplayFinishSemanticV1Validator
    writeAt 10 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.ledger_delta_output_finish_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-ledger-delta-output-finish-semantic-v1.unapplied.plutus.json"
            ledgerDeltaOutputFinishSemanticV1Validator
    writeAt 11 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.ledger_delta_output_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-ledger-delta-output-semantic-v1.unapplied.plutus.json"
            ledgerDeltaOutputSemanticV1Validator
    writeAt 12 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.ledger_delta_proof_frame_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-ledger-delta-proof-frame-semantic-v1.unapplied.plutus.json"
            ledgerDeltaProofFrameSemanticV1Validator
    writeAt 13 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.ledger_delta_operation_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-ledger-delta-operation-semantic-v1.unapplied.plutus.json"
            ledgerDeltaOperationSemanticV1Validator
    writeAt 14 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.ledger_delta_finalize_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-ledger-delta-finalize-semantic-v1.unapplied.plutus.json"
            ledgerDeltaFinalizeSemanticV1Validator
    writeAt 15 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.ledger_delta_terminal_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-ledger-delta-terminal-semantic-v1.unapplied.plutus.json"
            ledgerDeltaTerminalSemanticV1Validator

writeValidationTraceResolutionScripts :: Bool -> IO ()
writeValidationTraceResolutionScripts includeCek = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.award_v1.unapplied"
        "generated/fraud-proof-validation-trace-award-v1.unapplied.plutus.json"
        awardV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.timeout_v1.unapplied"
        "generated/fraud-proof-validation-trace-timeout-v1.unapplied.plutus.json"
        timeoutV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.boundary_v1.unapplied"
        "generated/fraud-proof-validation-trace-boundary-v1.unapplied.plutus.json"
        boundaryV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.dispute_v1.unapplied"
        "generated/fraud-proof-validation-trace-dispute-v1.unapplied.plutus.json"
        disputeV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.game_v1.unapplied"
        "generated/fraud-proof-validation-trace-game-v1.unapplied.plutus.json"
        gameV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.source_v1.unapplied"
        "generated/fraud-proof-validation-trace-source-v1.unapplied.plutus.json"
        sourceV1Validator
    writeValidationTraceValueAndMintSemanticScripts
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.value_and_mint_v1.unapplied"
        "generated/fraud-proof-validation-trace-value-and-mint-v1.unapplied.plutus.json"
        valueAndMintV1Validator
    -- Keep the largest validator last so an interrupted full run retains every
    -- smaller artifact. It can also be resumed alone with `validation-trace-cek`.
    when includeCek $
        do
            writeValidationTraceCekSemanticScripts
            writeValidationTraceCekAggregateScript

writeValidationTraceValueAndMintSemanticScripts :: IO ()
writeValidationTraceValueAndMintSemanticScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.value_and_mint_begin_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-value-and-mint-begin-semantic-v1.unapplied.plutus.json"
        valueAndMintBeginSemanticV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.value_and_mint_replay_begin_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-value-and-mint-replay-begin-semantic-v1.unapplied.plutus.json"
        valueAndMintReplayBeginSemanticV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.value_and_mint_replay_input_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-value-and-mint-replay-input-semantic-v1.unapplied.plutus.json"
        valueAndMintReplayInputSemanticV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.value_and_mint_replay_asset_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-value-and-mint-replay-asset-semantic-v1.unapplied.plutus.json"
        valueAndMintReplayAssetSemanticV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.value_and_mint_replay_finish_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-value-and-mint-replay-finish-semantic-v1.unapplied.plutus.json"
        valueAndMintReplayFinishSemanticV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.value_and_mint_output_descriptor_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-value-and-mint-output-descriptor-semantic-v1.unapplied.plutus.json"
        valueAndMintOutputDescriptorSemanticV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.value_and_mint_output_asset_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-value-and-mint-output-asset-semantic-v1.unapplied.plutus.json"
        valueAndMintOutputAssetSemanticV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.value_and_mint_output_finish_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-value-and-mint-output-finish-semantic-v1.unapplied.plutus.json"
        valueAndMintOutputFinishSemanticV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.value_and_mint_mint_asset_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-value-and-mint-mint-asset-semantic-v1.unapplied.plutus.json"
        valueAndMintMintAssetSemanticV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.value_and_mint_mint_finish_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-value-and-mint-mint-finish-semantic-v1.unapplied.plutus.json"
        valueAndMintMintFinishSemanticV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.value_and_mint_finalize_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-value-and-mint-finalize-semantic-v1.unapplied.plutus.json"
        valueAndMintFinalizeSemanticV1Validator

writeValidationTraceCekSemanticScripts :: IO ()
writeValidationTraceCekSemanticScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.cek_finish_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-cek-finish-semantic-v1.unapplied.plutus.json"
        cekFinishSemanticV1Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.cek_execution_selection_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-cek-execution-selection-semantic-v1.unapplied.plutus.json"
        cekExecutionSelectionSemanticV1Validator
    -- Both step verifiers expand deeply nested CEK/data-record matches.  The
    -- used-field placeholder analysis is combinatorial here (context-step was
    -- observed compiling for more than six hours), while emitting every field
    -- preserves semantics and keeps artifact generation bounded.
    writeValidationTraceCekContextStepSemanticScript
    writeValidationTraceCekCoreStepSemanticScript

writeValidationTraceCekContextStepSemanticScript :: IO ()
writeValidationTraceCekContextStepSemanticScript =
    writePlutusScriptAllFieldsNoTrace
        "midgard.fraud_proofs.validation_trace.cek_context_step_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-step-semantic-v1.unapplied.plutus.json"
        cekContextStepSemanticV1Validator

writeValidationTraceCekCoreStepSemanticScript :: IO ()
writeValidationTraceCekCoreStepSemanticScript =
    writePlutusScriptAllFieldsNoTrace
        "midgard.fraud_proofs.validation_trace.cek_core_step_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-step-semantic-v1.unapplied.plutus.json"
        cekCoreStepSemanticV1Validator

writeValidationTraceCekAggregateScript :: IO ()
writeValidationTraceCekAggregateScript =
    writePlutusScriptAllFieldsNoTrace
        "midgard.fraud_proofs.validation_trace.cek_v1.unapplied"
        "generated/fraud-proof-validation-trace-cek-v1.unapplied.plutus.json"
        cekV1Validator

encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = Text.decodeUtf8 . Base16.encode . CBOR.serialize' . serialiseScript

writeMinAdaScripts :: IO ()
writeMinAdaScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.min_ada.step_01.unapplied"
        "generated/fraud-proof-min-ada-step-01.unapplied.plutus.json"
        minAdaStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.min_ada.step_02.unapplied"
        "generated/fraud-proof-min-ada-step-02.unapplied.plutus.json"
        minAdaStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.min_ada.step_03.unapplied"
        "generated/fraud-proof-min-ada-step-03.unapplied.plutus.json"
        minAdaStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.min_ada.step_04.unapplied"
        "generated/fraud-proof-min-ada-step-04.unapplied.plutus.json"
        minAdaStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.min_ada.step_05.unapplied"
        "generated/fraud-proof-min-ada-step-05.unapplied.plutus.json"
        minAdaStep05Validator

-- NOTE: write the *compiled* script directly (like the applied-script export
-- path does via tryCompile). The previous implementation wrote the
-- CEK-*evaluated* result (evalT/evalScript), whose value read-back produces
-- broken (out-of-scope) deBruijn indices for these multi-parameter terms —
-- the exported blueprints were open UPLC terms and failed evaluation with
-- "cannot evaluate an open term" once parameters were applied off-chain.
writePlutusScriptWithInternalConfig :: InternalConfig -> Config -> String -> FilePath -> (forall s. Term s a) -> IO ()
writePlutusScriptWithInternalConfig internalConfig cfg title filepath term = do
    args <- getArgs
    let selected = case args of
            "only" : paths -> filepath `elem` paths
            _ -> True
    when selected $ do
        putStrLn $ "Compiling " <> title
        hFlush stdout
        -- Artifact builds disable repeated hoist-evaluation checks for every
        -- dependency. Record-heavy exports may additionally disable used-field
        -- placeholder analysis through the supplied internal configuration.
        case compileWithInternalConfig internalConfig cfg term of
            Left e -> print e
            Right script -> do
                let
                    scriptType = "PlutusScriptV3" :: String
                    plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
                    content = encodePretty plutusJson
                let outputPath = if environmentName == "testnet"
                        then "generated/testnet" </> takeFileName filepath
                        else filepath
                createDirectoryIfMissing True (takeDirectory outputPath)
                LBS.writeFile outputPath content
                putStrLn $ "Wrote " <> outputPath

writePlutusScript :: Config -> String -> FilePath -> (forall s. Term s a) -> IO ()
writePlutusScript = writePlutusScriptWithInternalConfig (InternalConfig True False)

-- | Writes a compiled term without traces for production script identity.
writePlutusScriptNoTrace :: String -> FilePath -> (forall s. Term s a) -> IO ()
writePlutusScriptNoTrace = writePlutusScript NoTracing

{- | Compiles pathological record-heavy terms without placeholder analysis.
This preserves semantics at a potential script-size cost and prevents these
artifacts from taking hours to compile or exhausting the generator process.
-}
writePlutusScriptAllFieldsNoTrace :: String -> FilePath -> (forall s. Term s a) -> IO ()
writePlutusScriptAllFieldsNoTrace =
    writePlutusScriptWithInternalConfig (InternalConfig False False) NoTracing
