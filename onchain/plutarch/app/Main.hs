module Main (main) where

import Cardano.Binary qualified as CBOR
import Control.Monad (when)
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Midgard.Validators.FraudProofs.ValidationTrace.CekContext qualified as CekContext
import Midgard.Validators.FraudProofs.ValidationTrace.CekContextFinalization qualified as CekContextFinalization
import Midgard.Validators.FraudProofs.ValidationTrace.CekContextItem qualified as CekContextItem
import Midgard.Validators.FraudProofs.ValidationTrace.CekContextObserver qualified as CekContextObserver
import Midgard.Validators.FraudProofs.ValidationTrace.CekContextRedeemer qualified as CekContextRedeemer
import Midgard.Validators.FraudProofs.ValidationTrace.CekSelection qualified as CekSelection

import MerkleTree.Validators.Membership (membershipStakeValidator, nonMembershipStakeValidator)
import Midgard.Env (environmentName)
import Midgard.ScriptSourcesRedeemerItemStepYield (redeemerItemStepValidator)
import Midgard.ScriptSourcesStageSevenYield (
    observerBoundValidator,
    observerItemValidator,
 )
import Midgard.Validators.ActiveOperators (
    activeOperatorsMintValidator,
    activeOperatorsSpendValidator,
 )
import Midgard.Validators.AvailabilityChallenge (availabilityChallengeBondYieldValidator, availabilityChallengeCloseYieldValidator, availabilityChallengeOpenYieldValidator, availabilityChallengeSettleYieldValidator, availabilityChallengeTimeoutYieldValidator, availabilityChallengeValidator)
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
import Midgard.Validators.FraudProofs.DistinctAssetAccumulationLimit (
    distinctAssetAccumulationLimitStep01Validator,
    distinctAssetAccumulationLimitStep02Validator,
    distinctAssetAccumulationLimitStep03Validator,
    distinctAssetAccumulationLimitStep04Validator,
    distinctAssetAccumulationLimitStep05Validator,
    distinctAssetAccumulationLimitStep06Validator,
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
import Midgard.Validators.FraudProofs.ExecutionNativeScriptInvalid (
    executionNativeScriptInvalidAcceptedInlineSourceValidator,
    executionNativeScriptInvalidAcceptedMintPrefixValidator,
    executionNativeScriptInvalidAcceptedObserverPrefixValidator,
    executionNativeScriptInvalidAcceptedReceivePrefixValidator,
    executionNativeScriptInvalidAcceptedReconstructionInitValidator,
    executionNativeScriptInvalidAcceptedReferenceSourceValidator,
    executionNativeScriptInvalidAcceptedSpendPrefixValidator,
    executionNativeScriptInvalidStep01Validator,
    executionNativeScriptInvalidStep02Validator,
    executionNativeScriptInvalidStep03Validator,
    executionNativeScriptInvalidStep04Validator,
    executionNativeScriptInvalidStep05Validator,
    executionNativeScriptInvalidStep06Validator,
 )
import Midgard.Validators.FraudProofs.ExecutionSourceScriptDecoding (
    executionSourceScriptDecodingStep01Validator,
    executionSourceScriptDecodingStep02Validator,
    executionSourceScriptDecodingStep03Validator,
    executionSourceScriptDecodingStep04Validator,
    executionSourceScriptDecodingStep05Validator,
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
import Midgard.Validators.FraudProofs.FieldItemWidthIllegal (
    fieldItemWidthIllegalStep01Validator,
    fieldItemWidthIllegalStep02Validator,
    fieldItemWidthIllegalStep03Validator,
 )
import Midgard.Validators.FraudProofs.FieldPreimageLengthMismatch (
    fieldPreimageLengthMismatchStep01Validator,
    fieldPreimageLengthMismatchStep02AcceptedValidator,
    fieldPreimageLengthMismatchStep02ForcedValidator,
    fieldPreimageLengthMismatchStep03Validator,
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
    inputSetUniquenessStep03Validator,
    inputSetUniquenessStep04Validator,
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
    minAdaTxYieldValidator,
    minAdaUtxoYieldValidator,
 )
import Midgard.Validators.FraudProofs.MinFee (
    minFeeStep01Validator,
    minFeeStep02Validator,
 )
import Midgard.Validators.FraudProofs.MintAuthorization (
    mintAuthorizationEvaluateValidator,
    mintAuthorizationStep01Validator,
    mintAuthorizationStep02Validator,
    mintAuthorizationStep03Validator,
    mintAuthorizationStep04Validator,
    mintAuthorizationStep05Validator,
    mintAuthorizationWitnessScanValidator,
 )
import Midgard.Validators.FraudProofs.MintDeclaredAssetLimit (
    mintDeclaredAssetLimitStep01Validator,
    mintDeclaredAssetLimitStep02Validator,
    mintDeclaredAssetLimitStep03Validator,
    mintDeclaredAssetLimitStep04Validator,
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
import Midgard.Validators.FraudProofs.MissingRedeemer (
    missingRedeemerStep01Validator,
    missingRedeemerStep02Validator,
    missingRedeemerStep02aValidator,
    missingRedeemerStep02bValidator,
    missingRedeemerStep03Validator,
    missingRedeemerStep04Validator,
    missingRedeemerStep05Validator,
 )
import Midgard.Validators.FraudProofs.MissingScriptSource (
    missingScriptSourceStep01Validator,
    missingScriptSourceStep02Validator,
    missingScriptSourceStep03Validator,
    missingScriptSourceStep04Validator,
    missingScriptSourceStep05Validator,
    missingScriptSourceStep06Validator,
 )
import Midgard.Validators.FraudProofs.MissingSignature (
    missingSignatureForcedSignerValidator,
    missingSignatureForcedStepValidator,
    missingSignatureForcedWitnessValidator,
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
    networkIdForcedScanValidator,
    networkIdForcedStepValidator,
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
import Midgard.Validators.FraudProofs.ObserverOrderInvalid (
    observerOrderInvalidStep01Validator,
    observerOrderInvalidStep02Validator,
    observerOrderInvalidStep03Validator,
    observerOrderInvalidStep04Validator,
 )
import Midgard.Validators.FraudProofs.ObserversForbiddenOnUntaggedNetwork (
    observersForbiddenOnUntaggedNetworkStep01Validator,
    observersForbiddenOnUntaggedNetworkStep02Validator,
 )
import Midgard.Validators.FraudProofs.OutputReferenceScriptDecoding (
    outputReferenceScriptDecodingStep01Validator,
    outputReferenceScriptDecodingStep02Validator,
    outputReferenceScriptDecodingStep03Validator,
    outputReferenceScriptDecodingStep04Validator,
    outputReferenceScriptDecodingStep05Validator,
    outputReferenceScriptDecodingStep06Validator,
 )
import Midgard.Validators.FraudProofs.ProtectedOutputSignerMissing (
    protectedOutputSignerMissingStep01Validator,
    protectedOutputSignerMissingStep02Validator,
    protectedOutputSignerMissingStep03Validator,
    protectedOutputSignerMissingStep04Validator,
    protectedOutputSignerMissingStep05Validator,
 )
import Midgard.Validators.FraudProofs.ReceivePurposeLanguage (
    receivePurposeLanguageStep01Validator,
    receivePurposeLanguageStep02Validator,
    receivePurposeLanguageStep03Validator,
 )
import Midgard.Validators.FraudProofs.RedeemerCanonicity (
    redeemerCanonicityStep01Validator,
    redeemerCanonicityStep02Validator,
    redeemerCanonicityStep03Validator,
 )
import Midgard.Validators.FraudProofs.ReferenceInputNoIdx (
    referenceInputNoIdxStep01Validator,
    referenceInputNoIdxStep02Validator,
    referenceInputNoIdxStep03Validator,
    referenceInputNoIdxStep04Validator,
 )
import Midgard.Validators.FraudProofs.ResolvedOutputNonCanonical (
    resolvedOutputNonCanonicalStep01Validator,
    resolvedOutputNonCanonicalStep02Validator,
    resolvedOutputNonCanonicalStep03Validator,
    resolvedOutputNonCanonicalStep04Validator,
    resolvedOutputNonCanonicalStep05Validator,
 )
import Midgard.Validators.FraudProofs.ScriptIntegrityHashMismatch (
    scriptIntegrityHashMismatchStep01Validator,
    scriptIntegrityHashMismatchStep02Validator,
    scriptIntegrityHashMismatchStep03Validator,
    scriptIntegrityHashMismatchStep04Validator,
    scriptIntegrityHashMismatchStep05Validator,
 )
import Midgard.Validators.FraudProofs.ScriptIntegrityHashMissing (
    scriptIntegrityHashMissingRedeemerGrammarValidator,
    scriptIntegrityHashMissingScriptGrammarValidator,
    scriptIntegrityHashMissingScriptScanValidator,
    scriptIntegrityHashMissingStep01Validator,
    scriptIntegrityHashMissingStep02Validator,
    scriptIntegrityHashMissingStep03Validator,
    scriptIntegrityHashMissingStep04Validator,
 )
import Midgard.Validators.FraudProofs.SpendInputSignerMissing (
    spendInputSignerMissingStep01Validator,
    spendInputSignerMissingStep02Validator,
    spendInputSignerMissingStep03Validator,
    spendInputSignerMissingStep04Validator,
    spendInputSignerMissingStep05Validator,
 )
import Midgard.Validators.FraudProofs.TransactionOutputNonCanonical
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
import Midgard.Validators.FraudProofs.TransitionTraceYield qualified as TransitionYield
import Midgard.Validators.FraudProofs.UnusedRedeemer (
    unusedRedeemerStep01Validator,
    unusedRedeemerStep02Validator,
    unusedRedeemerStep02aValidator,
    unusedRedeemerStep02bValidator,
    unusedRedeemerStep02cValidator,
    unusedRedeemerStep03Validator,
    unusedRedeemerStep04Validator,
    unusedRedeemerStep05Validator,
    unusedRedeemerStep06Validator,
 )
import Midgard.Validators.FraudProofs.UnusedScriptWitness (
    unusedScriptWitnessStep01Validator,
    unusedScriptWitnessStep02Validator,
    unusedScriptWitnessStep03Validator,
    unusedScriptWitnessStep04Validator,
    unusedScriptWitnessStep05Validator,
    unusedScriptWitnessStep06Validator,
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
import Midgard.Validators.FraudProofs.ValidationTrace.CekCore qualified as Core
import Midgard.Validators.FraudProofs.ValidationTrace.CekCoreArms qualified as CoreArms
import Midgard.Validators.FraudProofs.ValidationTrace.CekCoreMaterialArms qualified as CoreMaterial
import Midgard.Validators.FraudProofs.ValidationTrace.CekMaterialTraversal qualified as CekTraversal
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
import Midgard.Validators.FraudProofs.ValidationTrace.LedgerOutputDescriptorYield qualified as OutputDescriptorYield
import Midgard.Validators.FraudProofs.ValidationTrace.LedgerOutputProofYield qualified as OutputProofYield
import Midgard.Validators.FraudProofs.ValidationTrace.NativeScripts (
    nativeScriptsEffectfulSemanticV1Validator,
    nativeScriptsNativeSemanticV1Validator,
    nativeScriptsTerminalSemanticV1Validator,
    nativeScriptsV1Validator,
 )
import Midgard.Validators.FraudProofs.ValidationTrace.PhaseANativeItemYields qualified as PhaseANativeItemYields
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
import Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesMiddleYields qualified as MiddleYields
import Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesRedeemerNormalization (
    scriptSourcesRedeemerAdvanceBytesExecutorValidator,
    scriptSourcesRedeemerAdvanceIntegerExecutorValidator,
    scriptSourcesRedeemerAdvanceLargeConstructorExecutorValidator,
    scriptSourcesRedeemerAdvanceLargeFieldsExecutorValidator,
    scriptSourcesRedeemerAttachBytesExecutorValidator,
    scriptSourcesRedeemerAttachIntegerExecutorValidator,
    scriptSourcesRedeemerCekEnvelopeValidator,
    scriptSourcesRedeemerCekSettlementValidator,
    scriptSourcesRedeemerCloseExecutorValidator,
    scriptSourcesRedeemerEnvelopeV1Validator,
    scriptSourcesRedeemerExecutionSettlementV1Validator,
    scriptSourcesRedeemerFinalizeFrameExecutorV1Validator,
    scriptSourcesRedeemerFinishDataExecutorValidator,
    scriptSourcesRedeemerFoldListExecutorValidator,
    scriptSourcesRedeemerFoldMapExecutorV1Validator,
    scriptSourcesRedeemerHeadLargeConstructorExecutorValidator,
    scriptSourcesRedeemerHeadMapExecutorValidator,
    scriptSourcesRedeemerHeadScalarExecutorValidator,
    scriptSourcesRedeemerHeadSequenceExecutorValidator,
    scriptSourcesRedeemerInvalidHeaderExecutorValidator,
    scriptSourcesRedeemerInvalidTailExecutorValidator,
    scriptSourcesRedeemerOpenHeaderExecutorValidator,
    scriptSourcesRedeemerOpenTailExecutorValidator,
    scriptSourcesRedeemerOuterNormalizerV1Validator,
    scriptSourcesRedeemerSourceAuthenticatorValidator,
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
import Midgard.Validators.FraudProofs.ValueUnion qualified as ValueUnion
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
import Midgard.Validators.FraudProofs.WitnessScriptDecoding (
    witnessScriptDecodingStep01Validator,
    witnessScriptDecodingStep02Validator,
    witnessScriptDecodingStep03Validator,
    witnessScriptDecodingStep04Validator,
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
import Midgard.Validators.StateQueue (stateQueueCommitYieldValidator, stateQueueMergeYieldValidator, stateQueueMintValidator, stateQueueRemoveFraudulentYieldValidator, stateQueueRemoveUnattestedYieldValidator, stateQueueRemoveUnavailableYieldValidator, stateQueueSpendValidator)
import Midgard.Validators.TxOrder (txOrderMintValidator, txOrderSpendValidator)
import Midgard.Validators.Withdrawal (withdrawalMintValidator, withdrawalSpendValidator)
import Midgard.Validators.Witness (witnessPublishValidator)
import Midgard.ValueAssetFoldYield qualified as ValueAssetFoldYield
import Plutarch.Internal.Term
import Plutarch.Script (serialiseScript)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, takeFileName, (</>))
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
        ["operational-yields"] -> writeOperationalYieldScripts
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
        ["distinct-asset-accumulation-limit"] -> do
            createDirectoryIfMissing True "generated"
            writeDistinctAssetAccumulationLimitScripts
        ["mint-declared-asset-limit"] -> do
            createDirectoryIfMissing True "generated"
            writeMintDeclaredAssetLimitScripts
        ["witness-script-decoding"] -> do
            createDirectoryIfMissing True "generated"
            writeWitnessScriptDecodingScripts
        ["output-reference-script-decoding"] -> do
            createDirectoryIfMissing True "generated"
            writeOutputReferenceScriptDecodingScripts
        ["execution-source-script-decoding"] -> do
            createDirectoryIfMissing True "generated"
            writeExecutionSourceScriptDecodingScripts
        ["missing-redeemer"] -> do
            createDirectoryIfMissing True "generated"
            writeMissingRedeemerScripts
        ["missing-script-source"] -> do
            createDirectoryIfMissing True "generated"
            writeMissingScriptSourceScripts
        ["protected-output-signer-missing"] -> do
            createDirectoryIfMissing True "generated"
            writeProtectedOutputSignerMissingScripts
        ["resolved-output-non-canonical"] -> do
            createDirectoryIfMissing True "generated"
            writeResolvedOutputNonCanonicalScripts
        ["spend-input-signer-missing"] -> do
            createDirectoryIfMissing True "generated"
            writeSpendInputSignerMissingScripts
        ["field-item-width-illegal"] -> do
            createDirectoryIfMissing True "generated"
            writeFieldItemWidthIllegalScripts
        ["field-preimage-length-mismatch"] -> do
            createDirectoryIfMissing True "generated"
            writeFieldPreimageLengthMismatchScripts
        ["observers-forbidden-on-untagged-network"] -> do
            createDirectoryIfMissing True "generated"
            writeObserversForbiddenOnUntaggedNetworkScripts
        ["observer-order-invalid"] -> do
            createDirectoryIfMissing True "generated"
            writeObserverOrderInvalidScripts
        ["redeemer-canonicity"] -> do
            createDirectoryIfMissing True "generated"
            writeRedeemerCanonicityScripts
        ["receive-purpose-language"] -> do
            createDirectoryIfMissing True "generated"
            writeReceivePurposeLanguageScripts
        ["script-integrity-hash-mismatch"] -> do
            createDirectoryIfMissing True "generated"
            writeScriptIntegrityHashMismatchScripts
        ["script-integrity-hash-missing"] -> do
            createDirectoryIfMissing True "generated"
            writeScriptIntegrityHashMissingScripts
        ["unused-redeemer"] -> do
            createDirectoryIfMissing True "generated"
            writeUnusedRedeemerScripts
        ["unused-script-witness"] -> do
            createDirectoryIfMissing True "generated"
            writeUnusedScriptWitnessScripts
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
        ["script-sources-output"] -> mapM_ (writeValidationTraceScriptSourcesEarlyScripts . Just) [9, 12]
        ["script-sources-middle"] -> writeScriptSourcesMiddleYields >> writeValidationTraceScriptSourcesEarlyScripts (Just 8)
        ["resolve-inputs"] -> mapM_ (writeValidationTraceInputLedgerScripts . Just) [0, 1, 2, 3, 6]
        ["native-descriptors"] -> mapM_ (writeValidationTraceNativeScriptIntegrityScripts . Just) [0 .. 3]
        ["phase-a-native"] -> mapM_ (writeValidationTracePhaseAScripts . Just) ([0 .. 14] <> [18, 19])
        ["phase-a-preconditions"] -> mapM_ (writeValidationTracePhaseAScripts . Just) [15, 16, 17]
        ["signatures-advance-handoff"] -> mapM_ (writeValidationTraceEarlyPhaseScripts . Just) [8, 9]
        ["signatures-required-item"] -> writeValidationTraceEarlyPhaseScripts (Just 11)
        ["shared-item"] -> writeValidationTraceScriptSourcesRedeemerScripts Nothing
        ["cek-context"] -> writeCekContextScripts
        ["cek-selection"] -> writeCekSelectionScripts
        ["cek-core"] -> writeCekCoreScripts
        ["cek-material-traversal"] -> writeCekMaterialTraversalScripts
        ["ledger-output-proof-yields"] -> writeLedgerOutputProofYieldScripts
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
                    | index >= 0 && index <= 18 ->
                        writeValidationTraceScriptSourcesLateScripts $ Just index
                _ -> error "validation-trace-script-sources-late index must be between 0 and 18"
        ["validation-trace-script-sources-redeemer"] -> do
            createDirectoryIfMissing True "generated"
            writeValidationTraceScriptSourcesRedeemerScripts Nothing
        ["validation-trace-script-sources-redeemer", selectedIndex] -> do
            createDirectoryIfMissing True "generated"
            case readMaybe selectedIndex of
                Just index
                    | index >= 0 && index <= 26 ->
                        writeValidationTraceScriptSourcesRedeemerScripts $ Just index
                _ -> error "validation-trace-script-sources-redeemer index must be between 0 and 26"
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
    writeOperationalYieldScripts
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
        "midgard.fraud_proofs.missing_signature.forced_step.unapplied"
        "generated/fraud-proof-missing-signature-forced-step.unapplied.plutus.json"
        missingSignatureForcedStepValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_signature.forced_signer.unapplied"
        "generated/fraud-proof-missing-signature-forced-signer.unapplied.plutus.json"
        missingSignatureForcedSignerValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_signature.forced_witness.unapplied"
        "generated/fraud-proof-missing-signature-forced-witness.unapplied.plutus.json"
        missingSignatureForcedWitnessValidator
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
        "midgard.fraud_proofs.value_not_preserved.union_accepted_source.unapplied"
        "generated/fraud-proof-value-not-preserved-union-accepted-source.unapplied.plutus.json"
        ValueUnion.valueUnionAcceptedSourceValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.value_not_preserved.union_forced_source.unapplied"
        "generated/fraud-proof-value-not-preserved-union-forced-source.unapplied.plutus.json"
        ValueUnion.valueUnionForcedSourceValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.value_not_preserved.union_event.unapplied"
        "generated/fraud-proof-value-not-preserved-union-event.unapplied.plutus.json"
        ValueUnion.valueUnionEventValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.value_not_preserved.union_pre_state.unapplied"
        "generated/fraud-proof-value-not-preserved-union-pre-state.unapplied.plutus.json"
        ValueUnion.valueUnionPreStateValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.value_not_preserved.union_inputs.unapplied"
        "generated/fraud-proof-value-not-preserved-union-inputs.unapplied.plutus.json"
        ValueUnion.valueUnionInputsValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.value_not_preserved.union_input_value.unapplied"
        "generated/fraud-proof-value-not-preserved-union-input-value.unapplied.plutus.json"
        ValueUnion.valueUnionInputValueValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.value_not_preserved.union_assets.unapplied"
        "generated/fraud-proof-value-not-preserved-union-assets.unapplied.plutus.json"
        ValueUnion.valueUnionAssetsValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.value_not_preserved.union_field_grammar.unapplied"
        "generated/fraud-proof-value-not-preserved-union-field-grammar.unapplied.plutus.json"
        ValueUnion.valueUnionFieldGrammarValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.value_not_preserved.union_outputs.unapplied"
        "generated/fraud-proof-value-not-preserved-union-outputs.unapplied.plutus.json"
        ValueUnion.valueUnionOutputsValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.value_not_preserved.union_output_scan.unapplied"
        "generated/fraud-proof-value-not-preserved-union-output-scan.unapplied.plutus.json"
        ValueUnion.valueUnionOutputScanValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.value_not_preserved.union_mint.unapplied"
        "generated/fraud-proof-value-not-preserved-union-mint.unapplied.plutus.json"
        ValueUnion.valueUnionMintValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.value_not_preserved.union_update.unapplied"
        "generated/fraud-proof-value-not-preserved-union-update.unapplied.plutus.json"
        ValueUnion.valueUnionUpdateValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.value_not_preserved.union_terminal.unapplied"
        "generated/fraud-proof-value-not-preserved-union-terminal.unapplied.plutus.json"
        ValueUnion.valueUnionTerminalValidator
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
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.accepted_transaction_yields.l2_open.unapplied"
        "generated/fraud-proof-transition-trace-l2-open.unapplied.plutus.json"
        TransitionYield.l2OpenValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.accepted_transaction_yields.l2_replay.unapplied"
        "generated/fraud-proof-transition-trace-l2-replay.unapplied.plutus.json"
        TransitionYield.l2ReplayValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.accepted_transaction_yields.claim_structure.unapplied"
        "generated/fraud-proof-transition-trace-claim-structure.unapplied.plutus.json"
        TransitionYield.claimStructureValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.accepted_transaction_yields.claim_source.unapplied"
        "generated/fraud-proof-transition-trace-claim-source.unapplied.plutus.json"
        TransitionYield.claimSourceValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.accepted_transaction_yields.claim_endpoints.unapplied"
        "generated/fraud-proof-transition-trace-claim-endpoints.unapplied.plutus.json"
        TransitionYield.claimEndpointsValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.output_scan.scan_output.unapplied"
        "generated/fraud-proof-transition-trace-output-scan.unapplied.plutus.json"
        TransitionYield.outputScanValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.output_value.value_output.unapplied"
        "generated/fraud-proof-transition-trace-output-value.unapplied.plutus.json"
        TransitionYield.outputValueValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.output_summaries.summaries.unapplied"
        "generated/fraud-proof-transition-trace-output-summaries.unapplied.plutus.json"
        TransitionYield.outputSummariesValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.output_assembly.assembly.unapplied"
        "generated/fraud-proof-transition-trace-output-assembly.unapplied.plutus.json"
        TransitionYield.outputAssemblyValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.deposit_yields.projection.unapplied"
        "generated/fraud-proof-transition-trace-deposit-projection.unapplied.plutus.json"
        TransitionYield.depositProjectionValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.deposit_value.value_output.unapplied"
        "generated/fraud-proof-transition-trace-deposit-value.unapplied.plutus.json"
        TransitionYield.depositValueValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transition_trace.deposit_summaries.summaries.unapplied"
        "generated/fraud-proof-transition-trace-deposit-summaries.unapplied.plutus.json"
        TransitionYield.depositSummariesValidator
    writeValidationTraceCanonicalDecodeScripts
    writeValidationTraceEarlyPhaseScripts Nothing
    writeValidationTracePhaseAScripts Nothing
    writeValidationTraceScriptSourcesEarlyScripts Nothing
    writeScriptSourcesMiddleYields
    writeValidationTraceScriptSourcesLateScripts Nothing
    writeValidationTraceScriptSourcesRedeemerScripts Nothing
    writeValidationTraceNativeScriptIntegrityScripts Nothing
    writeValidationTraceInputLedgerScripts Nothing
    writeLedgerOutputProofYieldScripts
    -- The aggregate CEK writer still emits the shared semantic entry points.
    -- Write it before the split graph so the final artifacts are the reviewed
    -- NoTrace binder, selection and context validators below.
    writeValidationTraceResolutionScripts True
    writeCekMaterialTraversalScripts
    writeCekCoreScripts
    writeCekSelectionScripts
    writeCekContextScripts

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

writeFieldItemWidthIllegalScripts :: IO ()
writeFieldItemWidthIllegalScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.field_item_width_illegal.step_01.unapplied"
        "generated/fraud-proof-field-item-width-illegal-step-01.unapplied.plutus.json"
        fieldItemWidthIllegalStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.field_item_width_illegal.step_02.unapplied"
        "generated/fraud-proof-field-item-width-illegal-step-02.unapplied.plutus.json"
        fieldItemWidthIllegalStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.field_item_width_illegal.step_03.unapplied"
        "generated/fraud-proof-field-item-width-illegal-step-03.unapplied.plutus.json"
        fieldItemWidthIllegalStep03Validator

writeFieldPreimageLengthMismatchScripts :: IO ()
writeFieldPreimageLengthMismatchScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.field_preimage_length_mismatch.step_01.unapplied"
        "generated/fraud-proof-field-preimage-length-mismatch-step-01.unapplied.plutus.json"
        fieldPreimageLengthMismatchStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.field_preimage_length_mismatch.step_02_accepted.unapplied"
        "generated/fraud-proof-field-preimage-length-mismatch-step-02-accepted.unapplied.plutus.json"
        fieldPreimageLengthMismatchStep02AcceptedValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.field_preimage_length_mismatch.step_02_forced.unapplied"
        "generated/fraud-proof-field-preimage-length-mismatch-step-02-forced.unapplied.plutus.json"
        fieldPreimageLengthMismatchStep02ForcedValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.field_preimage_length_mismatch.step_03.unapplied"
        "generated/fraud-proof-field-preimage-length-mismatch-step-03.unapplied.plutus.json"
        fieldPreimageLengthMismatchStep03Validator

writeObserversForbiddenOnUntaggedNetworkScripts :: IO ()
writeObserversForbiddenOnUntaggedNetworkScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.observers_forbidden_on_untagged_network.step_01.unapplied"
        "generated/fraud-proof-observers-forbidden-on-untagged-network-step-01.unapplied.plutus.json"
        observersForbiddenOnUntaggedNetworkStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.observers_forbidden_on_untagged_network.step_02.unapplied"
        "generated/fraud-proof-observers-forbidden-on-untagged-network-step-02.unapplied.plutus.json"
        observersForbiddenOnUntaggedNetworkStep02Validator

writeObserverOrderInvalidScripts :: IO ()
writeObserverOrderInvalidScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.observer_order_invalid.step_01.unapplied"
        "generated/fraud-proof-observer-order-invalid-step-01.unapplied.plutus.json"
        observerOrderInvalidStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.observer_order_invalid.step_02.unapplied"
        "generated/fraud-proof-observer-order-invalid-step-02.unapplied.plutus.json"
        observerOrderInvalidStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.observer_order_invalid.step_03.unapplied"
        "generated/fraud-proof-observer-order-invalid-step-03.unapplied.plutus.json"
        observerOrderInvalidStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.observer_order_invalid.step_04.unapplied"
        "generated/fraud-proof-observer-order-invalid-step-04.unapplied.plutus.json"
        observerOrderInvalidStep04Validator

writeRedeemerCanonicityScripts :: IO ()
writeRedeemerCanonicityScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.redeemer_canonicity.step_01.unapplied"
        "generated/fraud-proof-redeemer-canonicity-step-01.unapplied.plutus.json"
        redeemerCanonicityStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.redeemer_canonicity.step_02.unapplied"
        "generated/fraud-proof-redeemer-canonicity-step-02.unapplied.plutus.json"
        redeemerCanonicityStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.redeemer_canonicity.step_03.unapplied"
        "generated/fraud-proof-redeemer-canonicity-step-03.unapplied.plutus.json"
        redeemerCanonicityStep03Validator

writeScriptIntegrityHashMismatchScripts :: IO ()
writeScriptIntegrityHashMismatchScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.script_integrity_hash_mismatch.step_01.unapplied"
        "generated/fraud-proof-script-integrity-hash-mismatch-step-01.unapplied.plutus.json"
        scriptIntegrityHashMismatchStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.script_integrity_hash_mismatch.step_02.unapplied"
        "generated/fraud-proof-script-integrity-hash-mismatch-step-02.unapplied.plutus.json"
        scriptIntegrityHashMismatchStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.script_integrity_hash_mismatch.step_03.unapplied"
        "generated/fraud-proof-script-integrity-hash-mismatch-step-03.unapplied.plutus.json"
        scriptIntegrityHashMismatchStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.script_integrity_hash_mismatch.step_04.unapplied"
        "generated/fraud-proof-script-integrity-hash-mismatch-step-04.unapplied.plutus.json"
        scriptIntegrityHashMismatchStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.script_integrity_hash_mismatch.step_05.unapplied"
        "generated/fraud-proof-script-integrity-hash-mismatch-step-05.unapplied.plutus.json"
        scriptIntegrityHashMismatchStep05Validator

writeScriptIntegrityHashMissingScripts :: IO ()
writeScriptIntegrityHashMissingScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.script_integrity_hash_missing.step_01.unapplied"
        "generated/fraud-proof-script-integrity-hash-missing-step-01.unapplied.plutus.json"
        scriptIntegrityHashMissingStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.script_integrity_hash_missing.step_02.unapplied"
        "generated/fraud-proof-script-integrity-hash-missing-step-02.unapplied.plutus.json"
        scriptIntegrityHashMissingStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.script_integrity_hash_missing.step_03.unapplied"
        "generated/fraud-proof-script-integrity-hash-missing-step-03.unapplied.plutus.json"
        scriptIntegrityHashMissingStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.script_integrity_hash_missing.script_grammar.unapplied"
        "generated/fraud-proof-script-integrity-hash-missing-script-grammar.unapplied.plutus.json"
        scriptIntegrityHashMissingScriptGrammarValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.script_integrity_hash_missing.script_scan.unapplied"
        "generated/fraud-proof-script-integrity-hash-missing-script-scan.unapplied.plutus.json"
        scriptIntegrityHashMissingScriptScanValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.script_integrity_hash_missing.redeemer_grammar.unapplied"
        "generated/fraud-proof-script-integrity-hash-missing-redeemer-grammar.unapplied.plutus.json"
        scriptIntegrityHashMissingRedeemerGrammarValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.script_integrity_hash_missing.step_04.unapplied"
        "generated/fraud-proof-script-integrity-hash-missing-step-04.unapplied.plutus.json"
        scriptIntegrityHashMissingStep04Validator

writeReceivePurposeLanguageScripts :: IO ()
writeReceivePurposeLanguageScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.receive_purpose_language.step_01.unapplied"
        "generated/fraud-proof-receive-purpose-language-step-01.unapplied.plutus.json"
        receivePurposeLanguageStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.receive_purpose_language.step_02.unapplied"
        "generated/fraud-proof-receive-purpose-language-step-02.unapplied.plutus.json"
        receivePurposeLanguageStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.receive_purpose_language.step_03.unapplied"
        "generated/fraud-proof-receive-purpose-language-step-03.unapplied.plutus.json"
        receivePurposeLanguageStep03Validator

writeDistinctAssetAccumulationLimitScripts :: IO ()
writeDistinctAssetAccumulationLimitScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.distinct_asset_accumulation_limit.step_01.unapplied"
        "generated/fraud-proof-distinct-asset-accumulation-limit-step-01.unapplied.plutus.json"
        distinctAssetAccumulationLimitStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.distinct_asset_accumulation_limit.step_02.unapplied"
        "generated/fraud-proof-distinct-asset-accumulation-limit-step-02.unapplied.plutus.json"
        distinctAssetAccumulationLimitStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.distinct_asset_accumulation_limit.step_03.unapplied"
        "generated/fraud-proof-distinct-asset-accumulation-limit-step-03.unapplied.plutus.json"
        distinctAssetAccumulationLimitStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.distinct_asset_accumulation_limit.step_04.unapplied"
        "generated/fraud-proof-distinct-asset-accumulation-limit-step-04.unapplied.plutus.json"
        distinctAssetAccumulationLimitStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.distinct_asset_accumulation_limit.step_05.unapplied"
        "generated/fraud-proof-distinct-asset-accumulation-limit-step-05.unapplied.plutus.json"
        distinctAssetAccumulationLimitStep05Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.distinct_asset_accumulation_limit.step_06.unapplied"
        "generated/fraud-proof-distinct-asset-accumulation-limit-step-06.unapplied.plutus.json"
        distinctAssetAccumulationLimitStep06Validator

writeMintDeclaredAssetLimitScripts :: IO ()
writeMintDeclaredAssetLimitScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.mint_declared_asset_limit.step_01.unapplied"
        "generated/fraud-proof-mint-declared-asset-limit-step-01.unapplied.plutus.json"
        mintDeclaredAssetLimitStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.mint_declared_asset_limit.step_02.unapplied"
        "generated/fraud-proof-mint-declared-asset-limit-step-02.unapplied.plutus.json"
        mintDeclaredAssetLimitStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.mint_declared_asset_limit.step_03.unapplied"
        "generated/fraud-proof-mint-declared-asset-limit-step-03.unapplied.plutus.json"
        mintDeclaredAssetLimitStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.mint_declared_asset_limit.step_04.unapplied"
        "generated/fraud-proof-mint-declared-asset-limit-step-04.unapplied.plutus.json"
        mintDeclaredAssetLimitStep04Validator

writeWitnessScriptDecodingScripts :: IO ()
writeWitnessScriptDecodingScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.witness_script_decoding.step_01.unapplied"
        "generated/fraud-proof-witness-script-decoding-step-01.unapplied.plutus.json"
        witnessScriptDecodingStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.witness_script_decoding.step_02.unapplied"
        "generated/fraud-proof-witness-script-decoding-step-02.unapplied.plutus.json"
        witnessScriptDecodingStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.witness_script_decoding.step_03.unapplied"
        "generated/fraud-proof-witness-script-decoding-step-03.unapplied.plutus.json"
        witnessScriptDecodingStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.witness_script_decoding.step_04.unapplied"
        "generated/fraud-proof-witness-script-decoding-step-04.unapplied.plutus.json"
        witnessScriptDecodingStep04Validator

writeOutputReferenceScriptDecodingScripts :: IO ()
writeOutputReferenceScriptDecodingScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.output_reference_script_decoding.step_01.unapplied"
        "generated/fraud-proof-output-reference-script-decoding-step-01.unapplied.plutus.json"
        outputReferenceScriptDecodingStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.output_reference_script_decoding.step_02.unapplied"
        "generated/fraud-proof-output-reference-script-decoding-step-02.unapplied.plutus.json"
        outputReferenceScriptDecodingStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.output_reference_script_decoding.step_03.unapplied"
        "generated/fraud-proof-output-reference-script-decoding-step-03.unapplied.plutus.json"
        outputReferenceScriptDecodingStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.output_reference_script_decoding.step_04.unapplied"
        "generated/fraud-proof-output-reference-script-decoding-step-04.unapplied.plutus.json"
        outputReferenceScriptDecodingStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.output_reference_script_decoding.step_05.unapplied"
        "generated/fraud-proof-output-reference-script-decoding-step-05.unapplied.plutus.json"
        outputReferenceScriptDecodingStep05Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.output_reference_script_decoding.step_06.unapplied"
        "generated/fraud-proof-output-reference-script-decoding-step-06.unapplied.plutus.json"
        outputReferenceScriptDecodingStep06Validator

writeExecutionSourceScriptDecodingScripts :: IO ()
writeExecutionSourceScriptDecodingScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.execution_source_script_decoding.step_01.unapplied"
        "generated/fraud-proof-execution-source-script-decoding-step-01.unapplied.plutus.json"
        executionSourceScriptDecodingStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.execution_source_script_decoding.step_02.unapplied"
        "generated/fraud-proof-execution-source-script-decoding-step-02.unapplied.plutus.json"
        executionSourceScriptDecodingStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.execution_source_script_decoding.step_03.unapplied"
        "generated/fraud-proof-execution-source-script-decoding-step-03.unapplied.plutus.json"
        executionSourceScriptDecodingStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.execution_source_script_decoding.step_04.unapplied"
        "generated/fraud-proof-execution-source-script-decoding-step-04.unapplied.plutus.json"
        executionSourceScriptDecodingStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.execution_source_script_decoding.step_05.unapplied"
        "generated/fraud-proof-execution-source-script-decoding-step-05.unapplied.plutus.json"
        executionSourceScriptDecodingStep05Validator

writeExecutionNativeScriptInvalidScripts :: IO ()
writeExecutionNativeScriptInvalidScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.execution_native_script_invalid.accepted_inline_source.unapplied"
        "generated/fraud-proof-execution-native-script-invalid-accepted-inline-source.unapplied.plutus.json"
        executionNativeScriptInvalidAcceptedInlineSourceValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.execution_native_script_invalid.accepted_reference_source.unapplied"
        "generated/fraud-proof-execution-native-script-invalid-accepted-reference-source.unapplied.plutus.json"
        executionNativeScriptInvalidAcceptedReferenceSourceValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.execution_native_script_invalid.accepted_spend_prefix.unapplied"
        "generated/fraud-proof-execution-native-script-invalid-accepted-spend-prefix.unapplied.plutus.json"
        executionNativeScriptInvalidAcceptedSpendPrefixValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.execution_native_script_invalid.accepted_mint_prefix.unapplied"
        "generated/fraud-proof-execution-native-script-invalid-accepted-mint-prefix.unapplied.plutus.json"
        executionNativeScriptInvalidAcceptedMintPrefixValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.execution_native_script_invalid.accepted_observer_prefix.unapplied"
        "generated/fraud-proof-execution-native-script-invalid-accepted-observer-prefix.unapplied.plutus.json"
        executionNativeScriptInvalidAcceptedObserverPrefixValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.execution_native_script_invalid.accepted_receive_prefix.unapplied"
        "generated/fraud-proof-execution-native-script-invalid-accepted-receive-prefix.unapplied.plutus.json"
        executionNativeScriptInvalidAcceptedReceivePrefixValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.execution_native_script_invalid.accepted_reconstruction_init.unapplied"
        "generated/fraud-proof-execution-native-script-invalid-accepted-reconstruction-init.unapplied.plutus.json"
        executionNativeScriptInvalidAcceptedReconstructionInitValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.execution_native_script_invalid.step_01.unapplied"
        "generated/fraud-proof-execution-native-script-invalid-step-01.unapplied.plutus.json"
        executionNativeScriptInvalidStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.execution_native_script_invalid.step_02.unapplied"
        "generated/fraud-proof-execution-native-script-invalid-step-02.unapplied.plutus.json"
        executionNativeScriptInvalidStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.execution_native_script_invalid.step_03.unapplied"
        "generated/fraud-proof-execution-native-script-invalid-step-03.unapplied.plutus.json"
        executionNativeScriptInvalidStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.execution_native_script_invalid.step_04.unapplied"
        "generated/fraud-proof-execution-native-script-invalid-step-04.unapplied.plutus.json"
        executionNativeScriptInvalidStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.execution_native_script_invalid.step_05.unapplied"
        "generated/fraud-proof-execution-native-script-invalid-step-05.unapplied.plutus.json"
        executionNativeScriptInvalidStep05Validator
    -- Retain all constructor fields: used-field analysis miscompiles this
    -- staged evaluator's resumed scan/finalize redeemers (covered by the emulator).
    writePlutusScriptAllFieldsNoTrace
        "midgard.fraud_proofs.execution_native_script_invalid.step_06.unapplied"
        "generated/fraud-proof-execution-native-script-invalid-step-06.unapplied.plutus.json"
        executionNativeScriptInvalidStep06Validator

writeMissingRedeemerScripts :: IO ()
writeMissingRedeemerScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_redeemer.step_01.unapplied"
        "generated/fraud-proof-missing-redeemer-step-01.unapplied.plutus.json"
        missingRedeemerStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_redeemer.step_02.unapplied"
        "generated/fraud-proof-missing-redeemer-step-02.unapplied.plutus.json"
        missingRedeemerStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_redeemer.step_02a.unapplied"
        "generated/fraud-proof-missing-redeemer-step-02a.unapplied.plutus.json"
        missingRedeemerStep02aValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_redeemer.step_02b.unapplied"
        "generated/fraud-proof-missing-redeemer-step-02b.unapplied.plutus.json"
        missingRedeemerStep02bValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_redeemer.step_03.unapplied"
        "generated/fraud-proof-missing-redeemer-step-03.unapplied.plutus.json"
        missingRedeemerStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_redeemer.step_04.unapplied"
        "generated/fraud-proof-missing-redeemer-step-04.unapplied.plutus.json"
        missingRedeemerStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_redeemer.step_05.unapplied"
        "generated/fraud-proof-missing-redeemer-step-05.unapplied.plutus.json"
        missingRedeemerStep05Validator

writeUnusedRedeemerScripts :: IO ()
writeUnusedRedeemerScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.unused_redeemer.step_01.unapplied"
        "generated/fraud-proof-unused-redeemer-step-01.unapplied.plutus.json"
        unusedRedeemerStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.unused_redeemer.step_02.unapplied"
        "generated/fraud-proof-unused-redeemer-step-02.unapplied.plutus.json"
        unusedRedeemerStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.unused_redeemer.step_02a.unapplied"
        "generated/fraud-proof-unused-redeemer-step-02a.unapplied.plutus.json"
        unusedRedeemerStep02aValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.unused_redeemer.step_02b.unapplied"
        "generated/fraud-proof-unused-redeemer-step-02b.unapplied.plutus.json"
        unusedRedeemerStep02bValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.unused_redeemer.step_02c.unapplied"
        "generated/fraud-proof-unused-redeemer-step-02c.unapplied.plutus.json"
        unusedRedeemerStep02cValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.unused_redeemer.step_03.unapplied"
        "generated/fraud-proof-unused-redeemer-step-03.unapplied.plutus.json"
        unusedRedeemerStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.unused_redeemer.step_04.unapplied"
        "generated/fraud-proof-unused-redeemer-step-04.unapplied.plutus.json"
        unusedRedeemerStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.unused_redeemer.step_05.unapplied"
        "generated/fraud-proof-unused-redeemer-step-05.unapplied.plutus.json"
        unusedRedeemerStep05Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.unused_redeemer.step_06.unapplied"
        "generated/fraud-proof-unused-redeemer-step-06.unapplied.plutus.json"
        unusedRedeemerStep06Validator

writeUnusedScriptWitnessScripts :: IO ()
writeUnusedScriptWitnessScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.unused_script_witness.step_01.unapplied"
        "generated/fraud-proof-unused-script-witness-step-01.unapplied.plutus.json"
        unusedScriptWitnessStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.unused_script_witness.step_02.unapplied"
        "generated/fraud-proof-unused-script-witness-step-02.unapplied.plutus.json"
        unusedScriptWitnessStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.unused_script_witness.step_03.unapplied"
        "generated/fraud-proof-unused-script-witness-step-03.unapplied.plutus.json"
        unusedScriptWitnessStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.unused_script_witness.step_04.unapplied"
        "generated/fraud-proof-unused-script-witness-step-04.unapplied.plutus.json"
        unusedScriptWitnessStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.unused_script_witness.step_05.unapplied"
        "generated/fraud-proof-unused-script-witness-step-05.unapplied.plutus.json"
        unusedScriptWitnessStep05Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.unused_script_witness.step_06.unapplied"
        "generated/fraud-proof-unused-script-witness-step-06.unapplied.plutus.json"
        unusedScriptWitnessStep06Validator

writeMissingScriptSourceScripts :: IO ()
writeMissingScriptSourceScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_script_source.step_01.unapplied"
        "generated/fraud-proof-missing-script-source-step-01.unapplied.plutus.json"
        missingScriptSourceStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_script_source.step_02.unapplied"
        "generated/fraud-proof-missing-script-source-step-02.unapplied.plutus.json"
        missingScriptSourceStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_script_source.step_03.unapplied"
        "generated/fraud-proof-missing-script-source-step-03.unapplied.plutus.json"
        missingScriptSourceStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_script_source.step_04.unapplied"
        "generated/fraud-proof-missing-script-source-step-04.unapplied.plutus.json"
        missingScriptSourceStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_script_source.step_05.unapplied"
        "generated/fraud-proof-missing-script-source-step-05.unapplied.plutus.json"
        missingScriptSourceStep05Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.missing_script_source.step_06.unapplied"
        "generated/fraud-proof-missing-script-source-step-06.unapplied.plutus.json"
        missingScriptSourceStep06Validator

writeProtectedOutputSignerMissingScripts :: IO ()
writeProtectedOutputSignerMissingScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.protected_output_signer_missing.step_01.unapplied"
        "generated/fraud-proof-protected-output-signer-missing-step-01.unapplied.plutus.json"
        protectedOutputSignerMissingStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.protected_output_signer_missing.step_02.unapplied"
        "generated/fraud-proof-protected-output-signer-missing-step-02.unapplied.plutus.json"
        protectedOutputSignerMissingStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.protected_output_signer_missing.step_03.unapplied"
        "generated/fraud-proof-protected-output-signer-missing-step-03.unapplied.plutus.json"
        protectedOutputSignerMissingStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.protected_output_signer_missing.step_04.unapplied"
        "generated/fraud-proof-protected-output-signer-missing-step-04.unapplied.plutus.json"
        protectedOutputSignerMissingStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.protected_output_signer_missing.step_05.unapplied"
        "generated/fraud-proof-protected-output-signer-missing-step-05.unapplied.plutus.json"
        protectedOutputSignerMissingStep05Validator

writeResolvedOutputNonCanonicalScripts :: IO ()
writeResolvedOutputNonCanonicalScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.resolved_output_non_canonical.step_01.unapplied"
        "generated/fraud-proof-resolved-output-non-canonical-step-01.unapplied.plutus.json"
        resolvedOutputNonCanonicalStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.resolved_output_non_canonical.step_02.unapplied"
        "generated/fraud-proof-resolved-output-non-canonical-step-02.unapplied.plutus.json"
        resolvedOutputNonCanonicalStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.resolved_output_non_canonical.step_03.unapplied"
        "generated/fraud-proof-resolved-output-non-canonical-step-03.unapplied.plutus.json"
        resolvedOutputNonCanonicalStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.resolved_output_non_canonical.step_04.unapplied"
        "generated/fraud-proof-resolved-output-non-canonical-step-04.unapplied.plutus.json"
        resolvedOutputNonCanonicalStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.resolved_output_non_canonical.step_05.unapplied"
        "generated/fraud-proof-resolved-output-non-canonical-step-05.unapplied.plutus.json"
        resolvedOutputNonCanonicalStep05Validator

writeSpendInputSignerMissingScripts :: IO ()
writeSpendInputSignerMissingScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.spend_input_signer_missing.step_01.unapplied"
        "generated/fraud-proof-spend-input-signer-missing-step-01.unapplied.plutus.json"
        spendInputSignerMissingStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.spend_input_signer_missing.step_02.unapplied"
        "generated/fraud-proof-spend-input-signer-missing-step-02.unapplied.plutus.json"
        spendInputSignerMissingStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.spend_input_signer_missing.step_03.unapplied"
        "generated/fraud-proof-spend-input-signer-missing-step-03.unapplied.plutus.json"
        spendInputSignerMissingStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.spend_input_signer_missing.step_04.unapplied"
        "generated/fraud-proof-spend-input-signer-missing-step-04.unapplied.plutus.json"
        spendInputSignerMissingStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.spend_input_signer_missing.step_05.unapplied"
        "generated/fraud-proof-spend-input-signer-missing-step-05.unapplied.plutus.json"
        spendInputSignerMissingStep05Validator

writeAdditionalFraudProofScripts :: IO ()
writeAdditionalFraudProofScripts = do
    writeFieldItemWidthIllegalScripts
    writeFieldPreimageLengthMismatchScripts
    writeObserversForbiddenOnUntaggedNetworkScripts
    writeObserverOrderInvalidScripts
    writeRedeemerCanonicityScripts
    writeDistinctAssetAccumulationLimitScripts
    writeMintDeclaredAssetLimitScripts
    writeWitnessScriptDecodingScripts
    writeOutputReferenceScriptDecodingScripts
    writeExecutionNativeScriptInvalidScripts
    writeExecutionSourceScriptDecodingScripts
    writeMissingRedeemerScripts
    writeUnusedRedeemerScripts
    writeUnusedScriptWitnessScripts
    writeMissingScriptSourceScripts
    writeScriptIntegrityHashMissingScripts
    writeProtectedOutputSignerMissingScripts
    writeResolvedOutputNonCanonicalScripts
    writeSpendInputSignerMissingScripts
    writeReceivePurposeLanguageScripts
    writeScriptIntegrityHashMismatchScripts
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
        "midgard.fraud_proofs.input_set_uniqueness.step_03.unapplied"
        "generated/fraud-proof-input-set-uniqueness-step-03.unapplied.plutus.json"
        inputSetUniquenessStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.input_set_uniqueness.step_04.unapplied"
        "generated/fraud-proof-input-set-uniqueness-step-04.unapplied.plutus.json"
        inputSetUniquenessStep04Validator
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
        "midgard.fraud_proofs.mint_authorization.evaluate.unapplied"
        "generated/fraud-proof-mint-authorization-evaluate.unapplied.plutus.json"
        mintAuthorizationEvaluateValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.mint_authorization.witness_scan.unapplied"
        "generated/fraud-proof-mint-authorization-witness-scan.unapplied.plutus.json"
        mintAuthorizationWitnessScanValidator
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
    -- Retain all constructor fields: used-field analysis miscompiles this
    -- staged evaluator's scan/finalize redeemers (covered by the emulator).
    writePlutusScriptAllFieldsNoTrace
        "midgard.fraud_proofs.native_script_invalid.step_05.unapplied"
        "generated/fraud-proof-native-script-invalid-step-05.unapplied.plutus.json"
        nativeScriptInvalidStep05Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transaction_output_non_canonical.step_01.unapplied"
        "generated/fraud-proof-transaction-output-non-canonical-step-01.unapplied.plutus.json"
        transactionOutputNonCanonicalStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transaction_output_non_canonical.step_02.unapplied"
        "generated/fraud-proof-transaction-output-non-canonical-step-02.unapplied.plutus.json"
        transactionOutputNonCanonicalStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transaction_output_non_canonical.step_03.unapplied"
        "generated/fraud-proof-transaction-output-non-canonical-step-03.unapplied.plutus.json"
        transactionOutputNonCanonicalStep03Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.transaction_output_non_canonical.step_04.unapplied"
        "generated/fraud-proof-transaction-output-non-canonical-step-04.unapplied.plutus.json"
        transactionOutputNonCanonicalStep04Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.network_id.step_01.unapplied"
        "generated/fraud-proof-network-id-step-01.unapplied.plutus.json"
        networkIdStep01Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.network_id.step_02.unapplied"
        "generated/fraud-proof-network-id-step-02.unapplied.plutus.json"
        networkIdStep02Validator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.network_id.forced_step.unapplied"
        "generated/fraud-proof-network-id-forced-step.unapplied.plutus.json"
        networkIdForcedStepValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.network_id.forced_scan.unapplied"
        "generated/fraud-proof-network-id-forced-scan.unapplied.plutus.json"
        networkIdForcedScanValidator
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

    writeAt 18 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_native_scripts_item_yields.native.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-native-scripts-item-native-yield.unapplied.plutus.json"
            PhaseANativeItemYields.nativeValidator
    writeAt 19 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.phase_a_native_scripts_item_yields.foreign.unapplied"
            "generated/fraud-proof-validation-trace-phase-a-native-scripts-item-foreign-yield.unapplied.plutus.json"
            PhaseANativeItemYields.foreignValidator

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
    writeAt 16 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_seven_observer_item_yield_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-seven-observer-item-yield-v1.unapplied.plutus.json"
            observerItemValidator
    writeAt 17 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_seven_observer_bound_yield_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-seven-observer-bound-yield-v1.unapplied.plutus.json"
            observerBoundValidator
    writeAt 18 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_redeemer_item_step_yield_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-redeemer-item-step-yield-v1.unapplied.plutus.json"
            redeemerItemStepValidator

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
    writeAt 6 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_cek_envelope.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-cek-envelope.unapplied.plutus.json"
            scriptSourcesRedeemerCekEnvelopeValidator
    writeAt 7 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_cek_settlement.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-cek-settlement.unapplied.plutus.json"
            scriptSourcesRedeemerCekSettlementValidator
    writeAt 8 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_source_authenticator.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-source-authenticator.unapplied.plutus.json"
            scriptSourcesRedeemerSourceAuthenticatorValidator
    writeAt 9 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_open_header_executor.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-open-header-executor.unapplied.plutus.json"
            scriptSourcesRedeemerOpenHeaderExecutorValidator
    writeAt 10 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_open_tail_executor.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-open-tail-executor.unapplied.plutus.json"
            scriptSourcesRedeemerOpenTailExecutorValidator
    writeAt 11 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_head_scalar_executor.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-head-scalar-executor.unapplied.plutus.json"
            scriptSourcesRedeemerHeadScalarExecutorValidator
    writeAt 12 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_head_sequence_executor.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-head-sequence-executor.unapplied.plutus.json"
            scriptSourcesRedeemerHeadSequenceExecutorValidator
    writeAt 13 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_head_map_executor.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-head-map-executor.unapplied.plutus.json"
            scriptSourcesRedeemerHeadMapExecutorValidator
    writeAt 14 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_head_large_constructor_executor.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-head-large-constructor-executor.unapplied.plutus.json"
            scriptSourcesRedeemerHeadLargeConstructorExecutorValidator
    writeAt 15 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_attach_integer_executor.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-attach-integer-executor.unapplied.plutus.json"
            scriptSourcesRedeemerAttachIntegerExecutorValidator
    writeAt 16 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_attach_bytes_executor.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-attach-bytes-executor.unapplied.plutus.json"
            scriptSourcesRedeemerAttachBytesExecutorValidator
    writeAt 17 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_fold_list_executor.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-fold-list-executor.unapplied.plutus.json"
            scriptSourcesRedeemerFoldListExecutorValidator
    writeAt 18 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_advance_integer_executor.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-advance-integer-executor.unapplied.plutus.json"
            scriptSourcesRedeemerAdvanceIntegerExecutorValidator
    writeAt 19 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_advance_bytes_executor.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-advance-bytes-executor.unapplied.plutus.json"
            scriptSourcesRedeemerAdvanceBytesExecutorValidator
    writeAt 20 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_advance_large_constructor_executor.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-advance-large-constructor-executor.unapplied.plutus.json"
            scriptSourcesRedeemerAdvanceLargeConstructorExecutorValidator
    writeAt 21 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_advance_large_fields_executor.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-advance-large-fields-executor.unapplied.plutus.json"
            scriptSourcesRedeemerAdvanceLargeFieldsExecutorValidator
    writeAt 22 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_close_executor.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-close-executor.unapplied.plutus.json"
            scriptSourcesRedeemerCloseExecutorValidator
    writeAt 23 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_finish_data_executor.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-finish-data-executor.unapplied.plutus.json"
            scriptSourcesRedeemerFinishDataExecutorValidator
    writeAt 24 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_invalid_header_executor.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-invalid-header-executor.unapplied.plutus.json"
            scriptSourcesRedeemerInvalidHeaderExecutorValidator
    writeAt 25 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_invalid_tail_executor.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-invalid-tail-executor.unapplied.plutus.json"
            scriptSourcesRedeemerInvalidTailExecutorValidator

    writeAt 26 $
        writePlutusScriptNoTrace
            "midgard.fraud_proofs.validation_trace.script_sources_stage_one_redeemer_semantic_v1.unapplied"
            "generated/fraud-proof-validation-trace-script-sources-stage-one-redeemer-semantic-v1.unapplied.plutus.json"
            scriptSourcesStageOneRedeemerSemanticV1Validator

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
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.value_and_mint_asset_fold_yield.unapplied"
        "generated/fraud-proof-validation-trace-value-and-mint-asset-fold-yield.unapplied.plutus.json"
        ValueAssetFoldYield.validator

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
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.min_ada.step_02_yields.tx.unapplied"
        "generated/fraud-proof-min-ada-tx-yield.unapplied.plutus.json"
        minAdaTxYieldValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.min_ada.step_02_yields.utxo.unapplied"
        "generated/fraud-proof-min-ada-utxo-yield.unapplied.plutus.json"
        minAdaUtxoYieldValidator

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
                let outputPath =
                        if environmentName == "testnet"
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

-- | Arm-specific withdrawals, with unapplied target deployment parameters.
writeOperationalYieldScripts :: IO ()
writeOperationalYieldScripts = do
    createDirectoryIfMissing True "generated"
    writePlutusScriptNoTrace
        "midgard.state_queue_yields.commit.unapplied"
        "generated/state-queue-yield-commit.unapplied.plutus.json"
        stateQueueCommitYieldValidator
    writePlutusScriptNoTrace
        "midgard.state_queue_yields.remove_unattested.unapplied"
        "generated/state-queue-yield-remove-unattested.unapplied.plutus.json"
        stateQueueRemoveUnattestedYieldValidator
    writePlutusScriptNoTrace
        "midgard.state_queue_yields.remove_unavailable.unapplied"
        "generated/state-queue-yield-remove-unavailable.unapplied.plutus.json"
        stateQueueRemoveUnavailableYieldValidator
    writePlutusScriptNoTrace
        "midgard.state_queue_yields.remove_fraudulent.unapplied"
        "generated/state-queue-yield-remove-fraudulent.unapplied.plutus.json"
        stateQueueRemoveFraudulentYieldValidator
    writePlutusScriptNoTrace
        "midgard.state_queue_yields.merge.unapplied"
        "generated/state-queue-yield-merge.unapplied.plutus.json"
        stateQueueMergeYieldValidator
    writePlutusScriptNoTrace
        "midgard.availability_challenge_yields.bond.unapplied"
        "generated/availability-challenge-yield-bond.unapplied.plutus.json"
        availabilityChallengeBondYieldValidator
    writePlutusScriptNoTrace
        "midgard.availability_challenge_yields.open.unapplied"
        "generated/availability-challenge-yield-open.unapplied.plutus.json"
        availabilityChallengeOpenYieldValidator
    writePlutusScriptNoTrace
        "midgard.availability_challenge_yields.settle.unapplied"
        "generated/availability-challenge-yield-settle.unapplied.plutus.json"
        availabilityChallengeSettleYieldValidator
    writePlutusScriptNoTrace
        "midgard.availability_challenge_yields.close.unapplied"
        "generated/availability-challenge-yield-close.unapplied.plutus.json"
        availabilityChallengeCloseYieldValidator
    writePlutusScriptNoTrace
        "midgard.availability_challenge_yields.timeout.unapplied"
        "generated/availability-challenge-yield-timeout.unapplied.plutus.json"
        availabilityChallengeTimeoutYieldValidator

writeLedgerOutputProofYieldScripts :: IO ()
writeLedgerOutputProofYieldScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_datum_advance_bytes_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-datum-advance-bytes-yield.unapplied.plutus.json"
        OutputProofYield.datumAdvanceBytesValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_datum_advance_integer_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-datum-advance-integer-yield.unapplied.plutus.json"
        OutputProofYield.datumAdvanceIntegerValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_datum_attach_bytes_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-datum-attach-bytes-yield.unapplied.plutus.json"
        OutputProofYield.datumAttachBytesValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_datum_attach_integer_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-datum-attach-integer-yield.unapplied.plutus.json"
        OutputProofYield.datumAttachIntegerValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_datum_close_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-datum-close-yield.unapplied.plutus.json"
        OutputProofYield.datumCloseValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_datum_finalize_frame_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-datum-finalize-frame-yield.unapplied.plutus.json"
        OutputProofYield.datumFinalizeFrameValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_datum_finish_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-datum-finish-yield.unapplied.plutus.json"
        OutputProofYield.datumFinishValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_datum_fold_list_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-datum-fold-list-yield.unapplied.plutus.json"
        OutputProofYield.datumFoldListValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_datum_fold_map_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-datum-fold-map-yield.unapplied.plutus.json"
        OutputProofYield.datumFoldMapValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_datum_head_large_constructor_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-datum-head-large-constructor-yield.unapplied.plutus.json"
        OutputProofYield.datumHeadLargeConstructorValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_datum_head_map_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-datum-head-map-yield.unapplied.plutus.json"
        OutputProofYield.datumHeadMapValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_datum_head_scalar_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-datum-head-scalar-yield.unapplied.plutus.json"
        OutputProofYield.datumHeadScalarValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_datum_head_sequence_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-datum-head-sequence-yield.unapplied.plutus.json"
        OutputProofYield.datumHeadSequenceValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_datum_large_constructor_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-datum-large-constructor-yield.unapplied.plutus.json"
        OutputProofYield.datumLargeConstructorValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_datum_large_fields_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-datum-large-fields-yield.unapplied.plutus.json"
        OutputProofYield.datumLargeFieldsValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_native_script_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-native-script-yield.unapplied.plutus.json"
        OutputProofYield.nativeScriptValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_reference_script_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-reference-script-yield.unapplied.plutus.json"
        OutputProofYield.referenceScriptValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_scalar_bytes_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-scalar-bytes-yield.unapplied.plutus.json"
        OutputProofYield.scalarBytesValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_scalar_integer_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-scalar-integer-yield.unapplied.plutus.json"
        OutputProofYield.scalarIntegerValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_script_hash_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-script-hash-yield.unapplied.plutus.json"
        OutputProofYield.scriptHashValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_span_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-span-yield.unapplied.plutus.json"
        OutputProofYield.spanValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_structure_assets_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-structure-assets-yield.unapplied.plutus.json"
        OutputProofYield.structureAssetsValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_structure_finish_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-structure-finish-yield.unapplied.plutus.json"
        OutputProofYield.structureFinishValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_structure_optional_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-structure-optional-yield.unapplied.plutus.json"
        OutputProofYield.structureOptionalValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_structure_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-structure-yield.unapplied.plutus.json"
        OutputProofYield.structureValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_proof_value_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-proof-value-yield.unapplied.plutus.json"
        OutputProofYield.valueValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_descriptor_datum_summary_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-descriptor-datum-summary-yield.unapplied.plutus.json"
        OutputDescriptorYield.datumSummaryValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_descriptor_reference_script_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-descriptor-reference-script-yield.unapplied.plutus.json"
        OutputDescriptorYield.referenceScriptValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_descriptor_scan_facts_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-descriptor-scan-facts-yield.unapplied.plutus.json"
        OutputDescriptorYield.scanFactsValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.ledger_output_descriptor_value_summary_yield.main.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-ledger-output-descriptor-value-summary-yield.unapplied.plutus.json"
        OutputDescriptorYield.valueSummaryValidator

writeCekMaterialTraversalScripts :: IO ()
writeCekMaterialTraversalScripts = do
    writePlutusScriptNoTrace
        "midgard.cek_material_traversal_v1.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-material-traversal-v1.unapplied.plutus.json"
        CekTraversal.traversalValidator
    writePlutusScriptNoTrace
        "midgard.cek_material_traversal_yields.program.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-cek-material-traversal-program-yield.unapplied.plutus.json"
        CekTraversal.programYieldValidator
    writePlutusScriptNoTrace
        "midgard.cek_material_traversal_yields.data.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-cek-material-traversal-data-yield.unapplied.plutus.json"
        CekTraversal.dataYieldValidator

writeCekCoreScripts :: IO ()
writeCekCoreScripts = do
    writePlutusScriptNoTrace
        "midgard.cek_core_arm_compute.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-arm-compute.unapplied.plutus.json"
        CoreArms.computeValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_builtin_roots.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-builtin-roots.unapplied.plutus.json"
        CoreArms.builtinRootsValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_semantic_result.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-semantic-result.unapplied.plutus.json"
        CoreArms.semanticResultValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_builtin_budget.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-builtin-budget.unapplied.plutus.json"
        CoreArms.builtinBudgetValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_direct_scalar.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-direct-scalar.unapplied.plutus.json"
        CoreArms.directScalarValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_direct_structured.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-direct-structured.unapplied.plutus.json"
        CoreArms.directStructuredValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_arm_machine.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-arm-machine.unapplied.plutus.json"
        CoreMaterial.machineValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_arm_map_conversion.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-arm-map-conversion.unapplied.plutus.json"
        CoreMaterial.mapConversionValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_semantic_pair.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-semantic-pair.unapplied.plutus.json"
        CoreMaterial.semanticPairValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_semantic_list_construct.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-semantic-list-construct.unapplied.plutus.json"
        CoreMaterial.semanticListConstructValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_semantic_list_select.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-semantic-list-select.unapplied.plutus.json"
        CoreMaterial.semanticListSelectValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_semantic_choose.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-semantic-choose.unapplied.plutus.json"
        CoreMaterial.semanticChooseValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_semantic_data_construct.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-semantic-data-construct.unapplied.plutus.json"
        CoreMaterial.semanticDataConstructValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_semantic_data_scalar.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-semantic-data-scalar.unapplied.plutus.json"
        CoreMaterial.semanticDataScalarValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_semantic_data_misc.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-semantic-data-misc.unapplied.plutus.json"
        CoreMaterial.semanticDataMiscValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_failure_known.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-failure-known.unapplied.plutus.json"
        CoreMaterial.failureKnownValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_failure_budget.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-failure-budget.unapplied.plutus.json"
        CoreMaterial.failureBudgetValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_semantic_failure_roots.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-semantic-failure-roots.unapplied.plutus.json"
        CoreMaterial.semanticFailureRootsValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_semantic_failure_material.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-semantic-failure-material.unapplied.plutus.json"
        CoreMaterial.semanticFailureMaterialValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_type_failure_roots.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-type-failure-roots.unapplied.plutus.json"
        CoreMaterial.typeFailureRootsValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_type_failure_kinds.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-type-failure-kinds.unapplied.plutus.json"
        CoreMaterial.typeFailureKindsValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_bls_budget.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-bls-budget.unapplied.plutus.json"
        CoreMaterial.blsBudgetValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_bls_roots.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-bls-roots.unapplied.plutus.json"
        CoreMaterial.blsRootsValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_bls_final.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-bls-final.unapplied.plutus.json"
        CoreMaterial.blsFinalValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_map_start_roots.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-map-start-roots.unapplied.plutus.json"
        CoreMaterial.mapStartRootsValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_map_start_budget.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-map-start-budget.unapplied.plutus.json"
        CoreMaterial.mapStartBudgetValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_map_start_nodes.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-map-start-nodes.unapplied.plutus.json"
        CoreMaterial.mapStartNodesValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_settle.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-settle.unapplied.plutus.json"
        Core.settleValidator
    writePlutusScriptNoTrace
        "midgard.cek_core_step_semantic_v1.unapplied"
        "generated/fraud-proof-validation-trace-cek-core-step-semantic-v1.unapplied.plutus.json"
        Core.bindValidator

writeCekSelectionScripts :: IO ()
writeCekSelectionScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_execution_selection_semantic_v1.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-execution-selection-semantic-v1.unapplied.plutus.json"
        CekSelection.selectionValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_execution_selection_yields.authenticate.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-cek-execution-selection-authenticate-yield.unapplied.plutus.json"
        CekSelection.authenticateValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_execution_selection_yields.successor.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-cek-execution-selection-successor-yield.unapplied.plutus.json"
        CekSelection.successorValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_execution_selection_yields.material_program.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-cek-execution-selection-material-program-yield.unapplied.plutus.json"
        CekSelection.programValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_execution_selection_yields.material_data.withdraw.unapplied"
        "generated/fraud-proof-validation-trace-cek-execution-selection-material-data-yield.unapplied.plutus.json"
        CekSelection.dataValidator

writeCekContextScripts :: IO ()
writeCekContextScripts = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_step_semantic_v1.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-step-semantic-v1.unapplied.plutus.json"
        CekContext.bindValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_control.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-control.unapplied.plutus.json"
        CekContext.controlValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_settle.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-settle.unapplied.plutus.json"
        CekContext.settleValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_reference.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-reference.unapplied.plutus.json"
        CekContext.referenceValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_spend.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-spend.unapplied.plutus.json"
        CekContext.spendValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_output.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-output.unapplied.plutus.json"
        CekContext.outputValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_signer.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-signer.unapplied.plutus.json"
        CekContext.signerValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_mint_init.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-mint-init.unapplied.plutus.json"
        CekContext.mintInitValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_mint_item.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-mint-item.unapplied.plutus.json"
        CekContext.mintItemValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_assemble.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-assemble.unapplied.plutus.json"
        CekContext.assembleValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_tx_info.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-tx-info.unapplied.plutus.json"
        CekContext.txInfoValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_seed.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-seed.unapplied.plutus.json"
        CekContext.seedValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_redeemer_begin.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-redeemer-begin.unapplied.plutus.json"
        CekContext.redeemerBeginValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_redeemer_select_authenticate.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-redeemer-select-authenticate.unapplied.plutus.json"
        CekContextRedeemer.authenticateValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_redeemer_select_initialize.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-redeemer-select-initialize.unapplied.plutus.json"
        CekContextRedeemer.initializeValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_redeemer_select_hash.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-redeemer-select-hash.unapplied.plutus.json"
        CekContextRedeemer.hashValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_redeemer_select_finish.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-redeemer-select-finish.unapplied.plutus.json"
        CekContextRedeemer.finishValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_finalize_authenticate.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-finalize-authenticate.unapplied.plutus.json"
        CekContextFinalization.authenticateValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_finalize_spend.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-finalize-spend.unapplied.plutus.json"
        CekContextFinalization.spendValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_finalize_mint.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-finalize-mint.unapplied.plutus.json"
        CekContextFinalization.mintValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_finalize_withdraw.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-finalize-withdraw.unapplied.plutus.json"
        CekContextFinalization.withdrawValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_finalize_observe.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-finalize-observe.unapplied.plutus.json"
        CekContextFinalization.observeValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_finalize_midgard.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-finalize-midgard.unapplied.plutus.json"
        CekContextFinalization.midgardValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_observer_authenticate.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-observer-authenticate.unapplied.plutus.json"
        CekContextObserver.authenticateValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_observer_fold.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-observer-fold.unapplied.plutus.json"
        CekContextObserver.foldValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_item_bind.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-item-bind.unapplied.plutus.json"
        CekContextItem.bindValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_item_return.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-item-return.unapplied.plutus.json"
        CekContextItem.returnValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_item_hash.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-item-hash.unapplied.plutus.json"
        CekContextItem.hashValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_item_finalize.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-item-finalize.unapplied.plutus.json"
        CekContextItem.finalizeValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_item_selection_continue.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-item-selection-continue.unapplied.plutus.json"
        CekContextItem.selectionContinueValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_item_selection_finish.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-item-selection-finish.unapplied.plutus.json"
        CekContextItem.selectionFinishValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_item_data_continue.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-item-data-continue.unapplied.plutus.json"
        CekContextItem.dataContinueValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_item_data_finish_descriptor.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-item-data-finish-descriptor.unapplied.plutus.json"
        CekContextItem.dataFinishDescriptorValidator
    writePlutusScriptNoTrace
        "midgard.fraud_proofs/validation_trace/cek_context_item_data_finish_value.main.spend.unapplied"
        "generated/fraud-proof-validation-trace-cek-context-item-data-finish-value.unapplied.plutus.json"
        CekContextItem.dataFinishValueValidator

writeScriptSourcesMiddleYields :: IO ()
writeScriptSourcesMiddleYields = do
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.script_sources_middle_stage_two_advance.unapplied"
        "generated/fraud-proof-validation-trace-script-sources-middle-stage-two-advance-yield.unapplied.plutus.json"
        (MiddleYields.validator 0)
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.script_sources_middle_stage_three_replay.unapplied"
        "generated/fraud-proof-validation-trace-script-sources-middle-stage-three-replay-yield.unapplied.plutus.json"
        (MiddleYields.validator 1)
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.script_sources_middle_stage_three_finish.unapplied"
        "generated/fraud-proof-validation-trace-script-sources-middle-stage-three-finish-yield.unapplied.plutus.json"
        (MiddleYields.validator 2)
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.script_sources_middle_stage_four_begin.unapplied"
        "generated/fraud-proof-validation-trace-script-sources-middle-stage-four-begin-yield.unapplied.plutus.json"
        (MiddleYields.fieldValidator 3)
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.script_sources_middle_stage_four_finish.unapplied"
        "generated/fraud-proof-validation-trace-script-sources-middle-stage-four-finish-yield.unapplied.plutus.json"
        (MiddleYields.validator 4)
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.script_sources_middle_stage_six_begin_policy.unapplied"
        "generated/fraud-proof-validation-trace-script-sources-middle-stage-six-begin-policy-yield.unapplied.plutus.json"
        (MiddleYields.fieldValidator 5)
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.script_sources_middle_stage_six_fold_asset.unapplied"
        "generated/fraud-proof-validation-trace-script-sources-middle-stage-six-fold-asset-yield.unapplied.plutus.json"
        (MiddleYields.validator 6)
    writePlutusScriptNoTrace
        "midgard.fraud_proofs.validation_trace.script_sources_middle_stage_six_finish.unapplied"
        "generated/fraud-proof-validation-trace-script-sources-middle-stage-six-finish-yield.unapplied.plutus.json"
        (MiddleYields.validator 7)
