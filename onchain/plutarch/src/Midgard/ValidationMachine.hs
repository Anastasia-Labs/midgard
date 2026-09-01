{-# LANGUAGE OverloadedStrings #-}

{- | Incremental port of Aiken @lib/midgard/validation-machine-v1.ak@.

The validation machine itself is the largest unported thing in this repo, and it
is sequenced last. The port is being landed at independently testable protocol
boundaries: structural transitions and opening witnesses, canonical controls,
resolution scheduling and accumulator hashing, early-phase witness encoders,
and leaf commitments used by the later phases.

=== Why a claim reaches into the machine at all

A committed claim (see "Midgard.ValidationClaim") authenticates the endpoints of
a machine run without running the machine. Its initial state is therefore
constrained entirely by construction: phase @CanonicalDecode@, program counter 0,
zero budget, pending verdict — and a @work_root@ that must equal the hash of the
/only/ witness that phase can legitimately start from. That witness is a field
scan positioned at the very beginning: field 0, item 0, chunk 0, no item count
yet (@-1@), nothing encoded yet (@0@).

The claim's opening encoder lives here rather than in the claim module because
it belongs to the machine's witness format. Keeping each subsequent slice here
preserves that ownership while the full one-step dispatcher is assembled.

=== The encoding

Nine elements in a definite array (@0x89@): four length-prefixed byte strings
followed by five integers. The integers are serialised as CBOR rather than
length-prefixed, which is what makes the array self-delimiting — a reader that
knows the shape can walk it without a schema.

The bounds are @expect@s in Aiken, so they __abort__ rather than return False.
That is deliberate and preserved here: an out-of-range field index is not a
witness that fails to match, it is a witness that could not have been produced by
the machine, and the difference matters to a caller that treats False as "this
challenge does not hold".
-}
module Midgard.ValidationMachine (
  PValidationOneStepWitnessV1 (..),
  PLedgerDeltaOperationProofV1 (..),
  PSignerSetProofV1 (..),
  PValidationAuxiliaryWitnessV1 (..),
  pvalidationAuxiliaryWitnessFromData,
  PValidationOneStepEvidenceV1 (..),
  PValidationProofItemDatumV1 (..),
  PCanonicalDecodeControlV1 (..),
  PCanonicalDecodeItemSourceV1 (..),
  PCanonicalDecodeItemProofV1 (..),
  PCanonicalDecodeItemObservationV1 (..),
  PObserverPurposeScanControlV1 (..),
  PReceivePurposeScanControlV1 (..),
  PScheduledInputV1 (..),
  PResolveInputOutputProofV1 (..),
  PInputSignerAuthorizationV1 (..),
  PMintFoldControlV1 (..),
  PScriptDiscoveryControlV1 (..),
  PInlineSourceHashControlV1 (..),
  PScriptSourcesControlV1 (..),
  PValidationContextV1 (..),
  PInputSetsControlV1 (..),
  PSignaturesControlV1 (..),
  PPhaseANativeScriptsControlV1 (..),
  PPhaseAScriptPreconditionsControlV1 (..),
  PResolveInputsControlV1 (..),
  PValueAccumulatorV1 (..),
  PValueAccumulatorUpdateV1 (..),
  PValueAssetMutationWitnessV1 (..),
  PLedgerDeltaPendingMutationV1 (..),
  PLedgerDeltaControlV1 (..),
  PNativeScriptsControlV1 (..),
  PValueAndMintControlV1 (..),
  PCekRedeemerContextControlV1 (..),
  PCekFinalContextControlV1 (..),
  PCekContextPartsControlV1 (..),
  PCekTxInfoAssemblyControlV1 (..),
  PCekContextControlV1 (..),
  pstructuralTransitionIsValid,
  pcanonicalDecodeControlFromWitness,
  pcanonicalDecodeControlIsBound,
  pcanonicalScanSuccessorIsExact,
  pcanonicalScanFieldSuccessorIsExact,
  pverifyCanonicalDecodeEmpty,
  pverifyCanonicalDecodeEmptySemanticsV1,
  pcanonicalArgumentHeaderSize,
  ptransactionFieldItemEncodedLength,
  pverifyCanonicalDecodeChunk,
  pverifyCanonicalDecodeChunkSemanticsV1,
  pbindCanonicalDecodeItemSourceV1,
  pobserveCanonicalDecodeItemV1,
  pverifyCanonicalDecodeItemObservationV1,
  pverifyCanonicalDecodeItemSuccessorV1,
  pverifyCanonicalDecodeItemFromSourceV1,
  pverifyCanonicalDecodeItemProofV1,
  pverifyCanonicalDecodeItem,
  pverifyCanonicalDecodeItemSemanticsV1,
  pverifyCanonicalDecode,
  pverifyCanonicalDecodeOneStepV1,
  pverifyCanonicalDecodeSemanticsV1,
  pverifyOneStepWithAuxiliary,
  pverifyOneStep,
  pverifyOneStepEvidence,
  pencodeCompactBindingWitness,
  pverifyCompactBinding,
  pverifyCompactBindingOneStepV1,
  pverifyCompactBindingSemanticsV1,
  pverifyStaticRules,
  pverifyStaticLedgerRulesOneStepV1,
  pverifyStaticLedgerRulesSemanticsV1,
  pinputSetsControlIsBound,
  pverifyInputSetsEmpty,
  pinputSetsItemSuccessorIsExact,
  pverifyInputSetsItem,
  pverifyInputSetsEmptySemanticsV1,
  pverifyInputSetsItemSemanticsV1,
  pverifyInputSets,
  pverifyInputSetsOneStepV1,
  pverifyInputSetsSemanticsV1,
  psignaturesControlIsBound,
  psignaturesSuccessorIsExact,
  psignaturesAfterRequiredSuccessorIsExact,
  psignatureAddressOrderKey,
  pverifySignatureAddressItem,
  pverifySignaturesAdvance,
  prequiredSignerMembershipIsValid,
  prequiredSignerNonMembershipIsValid,
  pverifyRequiredSignerItem,
  pverifySignaturesHandoff,
  pverifySignatures,
  pverifySignaturesAdvanceSemanticsV1,
  pverifySignatureAddressItemSemanticsV1,
  pverifyRequiredSignerItemSemanticsV1,
  pverifySignaturesHandoffSemanticsV1,
  pverifySignaturesOneStepV1,
  pphaseANativeControlIsBound,
  pphaseANativeSuccessorIsExact,
  pphaseANativeToPreconditionsIsExact,
  presetPhaseANativeControl,
  pphaseANativeCompleteScriptIsExact,
  pverifyPhaseANativeAdvanceScan,
  pverifyPhaseANativeItemScan,
  pverifyPhaseANativeAdvanceSemanticsV1,
  pverifyPhaseANativeItemSemanticsV1,
  pphaseANativeChunkWindow,
  pphaseANativeSignatureResult,
  pphaseANativeSetExecution,
  pverifyPhaseANativeTokenHeadScanV1,
  pverifyPhaseANativeTimelockPayloadScanV1,
  pverifyPhaseANativeAllOrAnyContainerFramePayloadScanV1,
  pverifyPhaseANativeAllOrAnyEmptyContainerPayloadScanV1,
  pverifyPhaseANativeAtLeastContainerFramePayloadScanV1,
  pverifyPhaseANativeAtLeastEmptyContainerPayloadScanV1,
  pverifyPhaseANativeSignaturePayloadScanV1,
  pverifyPhaseANativeTokenScan,
  pverifyPhaseANativeFrameScan,
  pverifyPhaseANativeFinalizeScan,
  pverifyPhaseANativeTokenSemanticsV1,
  pverifyPhaseANativeTokenHeadSemanticsV1,
  pverifyPhaseANativeTimelockPayloadSemanticsV1,
  pverifyPhaseANativeAllOrAnyContainerFramePayloadSemanticsV1,
  pverifyPhaseANativeAllOrAnyEmptyContainerPayloadSemanticsV1,
  pverifyPhaseANativeAtLeastContainerFramePayloadSemanticsV1,
  pverifyPhaseANativeAtLeastEmptyContainerPayloadSemanticsV1,
  pverifyPhaseANativeSignaturePayloadSemanticsV1,
  pverifyPhaseANativeSignatureMembershipPayloadSemanticsV1,
  pverifyPhaseANativeSignatureEmptyPayloadSemanticsV1,
  pverifyPhaseANativeSignatureBelowFirstPayloadSemanticsV1,
  pverifyPhaseANativeSignatureAboveLastPayloadSemanticsV1,
  pverifyPhaseANativeSignatureBetweenPayloadSemanticsV1,
  pverifyPhaseANativeContainerFrameTokenSemanticsV1,
  pverifyPhaseANativeEmptyContainerTokenSemanticsV1,
  pverifyPhaseANativeTimelockTokenSemanticsV1,
  pverifyPhaseANativeSignatureTokenSemanticsV1,
  pverifyPhaseANativeFrameSemanticsV1,
  pverifyPhaseANativeScripts,
  pverifyPhaseANativeScriptsOneStepV1,
  pverifyPhaseANativeScriptsSemanticsV1,
  pphaseAScriptPreconditionsRejection,
  pphaseAScriptPreconditionsControlIsBound,
  pphaseAScriptPreconditionsSuccessorIsExact,
  pphaseAScriptPreconditionsFinalize,
  pphaseAScriptPreconditionsWithObserver,
  pverifyPhaseAScriptPreconditions,
  pverifyPhaseAScriptPreconditionsOneStepV1,
  pverifyPhaseAScriptPreconditionsSemanticsV1,
  presolveInputsControlIsBound,
  pverifyResolveInputsInitialSemanticsV1,
  pverifyResolveInputsFinishSemanticsV1,
  pverifyResolveInputsMembershipBeginSemanticsV1,
  presolveMembershipProofResult,
  pverifyResolveInputsMembershipStepSemanticsV1,
  pverifyResolveInputsMembershipFinalizeSemanticsV1,
  pverifyResolveInputsNonMembershipSemanticsV1,
  pverifyResolveInputs,
  pverifyResolveInputsOneStepV1,
  pverifyResolveInputsSemanticsV1,
  pemptyScriptDiscoveryControl,
  pencodeScriptDiscoveryControl,
  pdecodeScriptDiscoveryControl,
  pencodeScriptSourcesDiscoveryWitness,
  pencodeInlineSourceHashControlV1,
  pdecodeInlineSourceHashControlV1,
  pencodeScriptSourcesPendingSourceWitness,
  pencodeScriptSourcesRedeemerItemWitness,
  pencodeScriptSourcesOutputProofWitness,
  pscriptSourcesStageZeroControlFromWitness,
  pscriptSourcesStageZeroControlIsBound,
  pinlineSourceHashBlockV1,
  pverifyScriptSourcesStageZeroFinishSemanticsV1,
  pverifyScriptSourcesStageZeroBeginSemanticsV1,
  pverifyScriptSourcesStageZeroHashBlockSemanticsV1,
  pverifyScriptSourcesStageZeroHashAdvanceSemanticsV1,
  pverifyScriptSourcesStageZeroHashTerminalSemanticsV1,
  pverifyScriptSourcesStageZeroSemanticsV1,
  pverifyScriptSourcesStageOneFinishRawSemanticsV1,
  pverifyScriptSourcesStageOneRedeemerBeginSemanticsV1,
  pverifyScriptSourcesStageOneRedeemerHeaderSemanticsV1,
  pverifyScriptSourcesStageOneRedeemerStepSemanticsV1,
  pverifyScriptSourcesStageOneSemanticsV1,
  pverifyScriptSourcesStageOneRedeemerSemanticsV1,
  pverifyScriptSourcesStageTwoSemanticsV1,
  pverifyScriptSourcesStageThreeFinishSemanticsV1,
  pverifyScriptSourcesStageThreeReplaySemanticsV1,
  pverifyPreparedScriptSourcesStageThreeReplayTransitionV1,
  pverifyScriptSourcesStageThreeSemanticsV1,
  pverifyScriptSourcesStageFourSemanticsV1,
  pverifyScriptSourcesStageFiveFinishSemanticsV1,
  pverifyScriptSourcesOutputProofBeginSemanticsV1,
  pverifyScriptSourcesOutputProofStepSemanticsV1,
  pverifyScriptSourcesOutputProofFinalizeSemanticsV1,
  pverifyScriptSourcesStageFiveSemanticsV1,
  pscriptSourcesStageFiveBranchV1,
  pverifyScriptSourcesStageSixEmptySemanticsV1,
  pverifyScriptSourcesStageSixFinishSemanticsV1,
  pverifyScriptSourcesStageSixBeginSemanticsV1,
  pverifyScriptSourcesStageSixAssetSemanticsV1,
  pverifyScriptSourcesStageSixSemanticsV1,
  pscriptSourcesStageSixBranchV1,
  pscriptSourcesStageSevenControlIsBound,
  pscriptSourcesStageSevenObserverScanIsComplete,
  pverifyScriptSourcesStageSevenObserverSemanticsV1,
  pverifyScriptSourcesStageSevenReceiveSemanticsV1,
  pverifyScriptSourcesStageSevenFinishSemanticsV1,
  pverifyScriptSourcesStageSevenSemanticsV1,
  pscriptSourcesStageSevenBranchV1,
  pscriptSourcesStageEightControlFromWitness,
  pscriptSourcesStageEightControlIsBound,
  pverifyScriptSourcesStageEightFinishSemanticsV1,
  pverifyScriptSourcesStageEightPurposeSemanticsV1,
  pverifyScriptSourcesStageEightSemanticsV1,
  pscriptSourcesStageEightBranchV1,
  pscriptSourcesStageNineControlFromWitness,
  pscriptSourcesStageNineControlIsBound,
  pverifyScriptSourcesStageNineMissingSemanticsV1,
  pverifyScriptSourcesStageNineMismatchSemanticsV1,
  pverifyScriptSourcesStageNineNativeMatchSemanticsV1,
  pverifyScriptSourcesStageNineEffectfulMatchSemanticsV1,
  pverifyScriptSourcesStageNineSemanticsV1,
  pscriptSourcesStageNineBranchV1,
  pscriptSourcesStageTenControlFromWitness,
  pscriptSourcesStageTenControlIsBound,
  pverifyScriptSourcesStageTenMissingSemanticsV1,
  pverifyPreparedScriptSourcesStageTenBeginTransitionV1,
  pverifyPreparedScriptSourcesStageTenMismatchTransitionV1,
  pverifyPreparedScriptSourcesStageTenAdvanceTransitionV1,
  pverifyPreparedScriptSourcesStageTenTerminalMismatchTransitionV1,
  pverifyPreparedScriptSourcesStageTenMatchTransitionV1,
  pverifyScriptSourcesStageTenMatchSemanticsV1,
  pverifyScriptSourcesStageTenMismatchSemanticsV1,
  pverifyScriptSourcesStageTenSemanticsV1,
  pscriptSourcesStageElevenControlFromWitness,
  pscriptSourcesStageElevenControlIsBound,
  pverifyScriptSourcesStageElevenFinishSemanticsV1,
  pverifyScriptSourcesStageElevenSourceSemanticsV1,
  pverifyScriptSourcesStageElevenSemanticsV1,
  pencodeNativeScriptsWitnessV1,
  pscriptSourcesStageTwelveControlFromWitness,
  pscriptSourcesStageTwelveControlIsBound,
  pverifyPreparedScriptSourcesStageTwelveBeginTransitionV1,
  pverifyScriptSourcesStageTwelveFinishSemanticsV1,
  pscriptSourcesStageTwelveRedeemerAuxiliaryIsFamily,
  pverifyScriptSourcesStageTwelveRedeemerSemanticsV1,
  pverifyScriptSourcesStageTwelveSemanticsV1,
  pscriptSourcesControlFromWitness,
  pscriptDiscoveryControlIsWellFormed,
  pscriptSourcesObserverScanIsWellFormed,
  pscriptSourcesControlIsBound,
  pscriptSourcesStageBranchV1,
  pverifyScriptSourcesNonOutputSemanticsV1,
  pverifyScriptSources,
  pverifyPreparedScriptSourcesOneStepV1,
  pverifyScriptSourcesOneStepV1,
  pscriptSourcesOutputProofResult,
  pmaxTransactionFieldIndex,
  pencodeTransactionFieldScanWitness,
  pencodeStaticRulesWitness,
  pencodeInputSetsScanWitness,
  pencodePhaseANativeScriptsScanWitness,
  pencodeSignaturesScanWitness,
  pencodePhaseAScriptPreconditionsWitness,
  pencodeTerminalRejectionWitness,
  pvalidityIntervalIsMalformed,
  predeemerTagForPurposeKindV1,
  predeemerPointerMatchesPurposeV1,
  pemptyObserverPurposeScanControl,
  pencodeObserverPurposeScanControl,
  pemptyReceivePurposeScanControl,
  pencodeReceivePurposeScanControl,
  pemptyMintFoldControl,
  pencodeMintFoldControl,
  pencodeScriptSourcesWitness,
  pinitialResolutionAccumulator,
  pemptyResolutionScheduleHash,
  presolutionScheduleNodeHash,
  ptransactionResolutionScheduleHash,
  pencodeResolveInputOutputProof,
  pencodeResolveInputsWitness,
  pdecodeValidationContext,
  pinputSetsControlFromWitness,
  psignaturesControlFromWitness,
  pphaseANativeControlFromWitness,
  pphaseAScriptPreconditionsControlFromWitness,
  pdecodeResolveInputOutputProof,
  presolveInputsControlFromWitness,
  pencodeValueAccumulatorV1,
  pvalueAccumulatorFromCbor,
  pencodeLedgerDeltaPendingMutationV1,
  pledgerDeltaPendingMutationFromCbor,
  pencodeLedgerDeltaControlV1,
  pledgerDeltaControlFromWitness,
  pencodeTerminalAcceptanceWitnessV1,
  pencodeLedgerDeltaWitnessV1,
  pnativeScriptsControlFromWitness,
  pencodeNativeScriptsControlV1,
  pnativeScriptsControlIsWellFormed,
  pnativeScriptsControlIsBound,
  pnativeScriptsNextLanguageBitmap,
  pverifyNativeScriptsTerminalSemanticsV1,
  pverifyNativeScriptsNativeSemanticsV1,
  pverifyNativeScriptsEffectfulSemanticsV1,
  pverifyNativeScripts,
  pverifyNativeScriptsOneStepV1,
  pencodeScriptIntegrityWitnessV1,
  pencodeScriptIntegrityCompactWitnessV1,
  pencodeScriptIntegrityWitnessSetWitnessV1,
  pencodeScriptIntegrityFinalizeWitnessV1,
  pscriptIntegrityControlAndStage,
  pnativeResolvedContextIsWellFormed,
  pverifyScriptIntegrityAuthentication,
  pverifyScriptIntegrityAuthenticationSemanticsV1,
  pverifyScriptIntegrityCompact,
  pverifyScriptIntegrityWitnessSet,
  pverifyScriptIntegrityFinalize,
  pverifyScriptIntegrity,
  pverifyScriptIntegrityCompactSemanticsV1,
  pverifyScriptIntegrityWitnessSetSemanticsV1,
  pverifyScriptIntegrityFinalizeSemanticsV1,
  pverifyScriptIntegrityOneStepV1,
  pinitialValueAccumulator,
  pencodeValueAndMintControlV1,
  pvalueAndMintControlFromWitness,
  pencodeValueAndMintWitnessV1,
  pencodeCekContextControlV1,
  pcekContextControlFromCbor,
  pencodeCekRedeemerContextControlV1,
  phashCekRedeemerContextControlV1,
  pinitialCekRedeemerContextControlV1,
  pencodeCekFinalContextControlV1,
  phashCekFinalContextControlV1,
  pencodeCekContextPartsControlV1,
  phashCekContextPartsControlV1,
  pencodeCekTxInfoAssemblyControlV1,
  phashCekTxInfoAssemblyControlV1,
  pencodeCekWitnessV1,
  pprependCekObserverItemV1,
  pfinalizeCekObserverItemsV1,
  pcekContextControlIsWellFormed,
  pcekRedeemerContextControlIsWellFormed,
  pcompletedCekRedeemerContextMatches,
  pcekContextSuccessorIsExact,
  pverifyCekExecutionSelection,
  pverifyCekCoreAdvanced,
  pverifyCekCoreStep,
  pcekContextAuxiliaryMatchesStage,
  pverifyCekContextStep,
  pverifyCekCompleted,
  pverifyCek,
  pverifyCekOneStepV1,
  pvalueAndMintSuccessorIsExact,
  pvalueAndMintStageZero,
  pvalueAndMintStageOne,
  pcompleteValueInputReplay,
  pvalueAndMintStageTwoFinish,
  pvalueAndMintStageTwoReplay,
  pvalueAndMintStageTwoAsset,
  pvalueAndMintStageTwo,
  pvalueAndMintStageThreeFinish,
  pvalueAndMintStageThreeDescriptor,
  pvalueAndMintStageThreeAsset,
  pvalueAndMintStageThree,
  pvalueAndMintStageFourFinish,
  pvalueAndMintStageFourAsset,
  pvalueAndMintStageFour,
  pvalueAndMintStageFive,
  pverifyValueAndMint,
  pverifyValueAndMintOneStepV1,
  pledgerDeltaSuccessorIsExact,
  pledgerDeltaOperationProofIsValid,
  pledgerDeltaOperationStep,
  pledgerDeltaPendingStep,
  pledgerDeltaReplayStep,
  pledgerDeltaStageOne,
  pledgerDeltaOutputStep,
  pledgerDeltaStageTwo,
  pledgerDeltaStageThree,
  pledgerDeltaControlIsWellFormed,
  pverifyLedgerDelta,
  pverifyLedgerDeltaReplayFinishSemanticsV1,
  pverifyLedgerDeltaOutputFinishSemanticsV1,
  pverifyLedgerDeltaTerminalSemanticsV1,
  pverifyLedgerDeltaOperationSemanticsV1,
  pverifyLedgerDeltaReplaySemanticsV1,
  pverifyLedgerDeltaOutputSemanticsV1,
  pverifyLedgerDeltaProofFrameSemanticsV1,
  pverifyLedgerDeltaFinalizeSemanticsV1,
  pverifyLedgerDeltaOneStepV1,
  pverifyCekReferenceContextAdvance,
  pverifyCekReferenceContextItem,
  pverifyCekReferenceContextStep,
  pverifyCekSpendContextAdvance,
  pverifyCekSpendContextItem,
  pverifyCekSpendContextStep,
  pverifyCekOutputContextAdvance,
  pverifyCekOutputContextItem,
  pverifyCekOutputContextStep,
  pverifyCekSignerContextAdvance,
  pverifyCekSignerContextItem,
  pverifyCekObserverContextAdvance,
  pverifyCekObserverContextItem,
  pverifyCekObserverContextStep,
  pverifyCekMintContextInit,
  pverifyCekMintContextAdvance,
  pverifyCekMintContextItem,
  pverifyCekRedeemerContextSelect,
  pverifyCekRedeemerContextAdvanced,
  pverifyCekRedeemerContextStep,
  pverifyCekContextFinalizeControl,
  pverifyCekContextFinalizeSpendControl,
  pverifyCekContextFinalize,
  pverifyCekContextAssembleControl,
  pverifyCekTxInfoFinalizeControl,
  pverifyCekContextSeedControl,
  papplyValueAssetMutation,
  presolvedInputAccumulatorSuccessor,
  pmintAssetLeafHash,
  pledgerDeltaOperationLeafHash,
  pminAdaOutputOverheadBytes,
  pminAdaLovelaceV1,
  poutputMeetsMinAdaV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Data.ByteString qualified as BS
import Aiken.Cbor (pdeserialise)
import Plutarch.Builtin.Crypto (pblake2b_224, pblake2b_256, pverifyEd25519Signature)
import Plutarch.Core.Internal.Builtins (pindexBS')
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.MerkleTree.Merkling (pnull_hash)
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottRec (..))
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.NativeTx.Codec (
  pdecodeDefiniteArrayHeaderAt,
  pdecodeCanonicalIntAt,
  pdecodeDefiniteBytesAt,
  pdecodeDefiniteMapHeaderAt,
  pencodeDefiniteArrayHeader,
  pencodeDefiniteBytes,
  pencodeDefiniteMapHeader,
 )
import Midgard.FraudProofs.NativeTx.Compact qualified as NativeCompact
import Midgard.FraudProofs.NativeTx.Components (
  pdecodeMidgardAddressWitnessCbor,
  pdecodeMidgardTxInputCbor,
  pencodeMidgardTxInput,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddress (..),
  PMidgardCredential (..),
  PMidgardAddressWitness (..),
  PMidgardTxInput (..),
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxWitnessSetCompact (..),
  PNativeTxFieldPreimageLengthsV1 (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.CekData qualified as CekData
import Midgard.CekConstant qualified as CekConstant
import Midgard.CekMachine qualified as CekMachine
import Midgard.CekProof qualified as CekProof
import Midgard.Blake2b224Trace qualified as Blake2b224
import Midgard.BoundedCollection (PItemProofV1 (..), pmaxTxSizeDerivedItemCount, pverifyBoundedCollectionItem)
import Midgard.BoundedItem qualified as BoundedItem
import Midgard.LedgerOutput qualified as LedgerOutput
import Midgard.LedgerOutputCommitment qualified as OutputCommitment
import Midgard.LedgerOutputProof qualified as LedgerOutputProof
import Midgard.MpfProofFold qualified as ProofFold
import Midgard.MpfProof qualified as MpfProof
import Midgard.MpfProof.Types (PProof (..), PProofStep)
import Midgard.NativeTxFieldAccess qualified as NativeField
import Midgard.NativeScriptScan qualified as NativeScriptScan
import Midgard.RedeemerItemProof qualified as RedeemerItemProof
import Midgard.ScriptContext qualified as ScriptContext
import Midgard.ScriptLanguageViews qualified as ScriptLanguageViews
import Midgard.ScriptProof qualified as ScriptProof
import Midgard.ValidationMerkle (PBuiltFrontier (..), PFrontierPeak (..), pappendLeaf, pencodeFrontier, pfrontierCommitment, pfrontierIsWellFormed, pverifyMembership)
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase (..),
  PValidationVerdict (..),
  pcborInt,
  phashWorkWitness,
  phashRejectionCode,
  phashValidationContext,
  pmachineStateIsWellFormed,
 )

presolutionAccumulatorDomain, presolutionScheduleDomain :: forall s. Term s PByteString
presolutionAccumulatorDomain = pconstant "MidgardResolvedInputsAccumulatorV1"
presolutionScheduleDomain = pconstant "MidgardInputResolutionScheduleV1"

pmintAssetLeafDomain, pledgerDeltaOperationDomain :: forall s. Term s PByteString
pmintAssetLeafDomain = pconstant "MidgardMintAssetLeafV1"
pledgerDeltaOperationDomain = pconstant "MidgardValidationLedgerDeltaOperationV1"

pconsensusProfileV1Id :: forall s. Term s PByteString
pconsensusProfileV1Id = pconstant "midgard-consensus-v1"

-- | Fixed UTxO-entry overhead from the target ledger's minimum-Ada rule.
pminAdaOutputOverheadBytes :: forall s. Term s PInteger
pminAdaOutputOverheadBytes = 160

-- | Aiken @validation_machine_v1.min_ada_lovelace_v1@.
pminAdaLovelaceV1 :: forall s. Term s (PInteger :--> PInteger :--> PInteger)
pminAdaLovelaceV1 = phoistAcyclic $ plam $ \coinsPerUtxoByte serializedOutputBytes ->
  coinsPerUtxoByte * (pminAdaOutputOverheadBytes + serializedOutputBytes)

-- | Aiken @validation_machine_v1.output_meets_min_ada_v1@.
poutputMeetsMinAdaV1 :: forall s. Term s (PInteger :--> PInteger :--> PInteger :--> PBool)
poutputMeetsMinAdaV1 = phoistAcyclic $ plam $ \coinsPerUtxoByte serializedOutputBytes lovelace ->
  lovelace #>= pminAdaLovelaceV1 # coinsPerUtxoByte # serializedOutputBytes

data PValidationOneStepWitnessV1 (s :: S) = PValidationOneStepWitnessV1
  { poneStep'workWitnessCbor :: Term s (PAsData PByteString)
  , poneStep'claimedSuccessor :: Term s (PAsData PValidationMachineStateV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationOneStepWitnessV1)

data PCanonicalDecodeControlV1 (s :: S) = PCanonicalDecodeControlV1
  { pcanonicalControl'compactCbor :: Term s (PAsData PByteString)
  , pcanonicalControl'witnessSetCompactCbor :: Term s (PAsData PByteString)
  , pcanonicalControl'fieldPreimageLengthsCbor :: Term s (PAsData PByteString)
  , pcanonicalControl'contextCbor :: Term s (PAsData PByteString)
  , pcanonicalControl'fieldIndex :: Term s (PAsData PInteger)
  , pcanonicalControl'itemIndex :: Term s (PAsData PInteger)
  , pcanonicalControl'chunkIndex :: Term s (PAsData PInteger)
  , pcanonicalControl'itemCount :: Term s (PAsData PInteger)
  , pcanonicalControl'encodedLength :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCanonicalDecodeControlV1)

data PCanonicalDecodeItemSourceV1 (s :: S) = PCanonicalDecodeItemSourceV1
  { pcanonicalSource'expectedFieldCommitment :: Term s (PAsData PByteString)
  , pcanonicalSource'expectedFieldLength :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCanonicalDecodeItemSourceV1)

data PCanonicalDecodeItemProofV1 (s :: S) = PCanonicalDecodeItemProofV1
  { pcanonicalProof'activeItemCount :: Term s (PAsData PInteger)
  , pcanonicalProof'itemEncodingIsValid :: Term s (PAsData PBool)
  , pcanonicalProof'nextEncodedLength :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCanonicalDecodeItemProofV1)

data PCanonicalDecodeItemObservationV1 (s :: S) = PCanonicalDecodeItemObservationV1
  { pcanonicalObservation'collectionProof :: Term s (PAsData PItemProofV1)
  , pcanonicalObservation'itemLength :: Term s (PAsData PInteger)
  , pcanonicalObservation'itemCommitment :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCanonicalDecodeItemObservationV1)

data PObserverPurposeScanControlV1 (s :: S) = PObserverPurposeScanControlV1
  { pobserverScan'totalCount :: Term s (PAsData PInteger)
  , pobserverScan'seen :: Term s (PAsData PInteger)
  , pobserverScan'previousHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PObserverPurposeScanControlV1)

data PReceivePurposeScanControlV1 (s :: S) = PReceivePurposeScanControlV1
  { preceiveScan'sourceCount :: Term s (PAsData PInteger)
  , preceiveScan'sourcePeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , preceiveScan'receiveCount :: Term s (PAsData PInteger)
  , preceiveScan'previousHash :: Term s (PAsData PByteString)
  , preceiveScan'candidateHash :: Term s (PAsData PByteString)
  , preceiveScan'descriptorPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PReceivePurposeScanControlV1)

data PScheduledInputV1 (s :: S) = PScheduledInputV1
  { pscheduledInput'sourceKind :: Term s (PAsData PInteger)
  , pscheduledInput'key :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScheduledInputV1)

data PResolveInputOutputProofV1 (s :: S) = PResolveInputOutputProofV1
  { presolveOutputProof'sourceKind :: Term s (PAsData PInteger)
  , presolveOutputProof'key :: Term s (PAsData PByteString)
  , presolveOutputProof'nextScheduleHash :: Term s (PAsData PByteString)
  , presolveOutputProof'descriptorCbor :: Term s (PAsData PByteString)
  , presolveOutputProof'outputProof :: Term s (PAsData LedgerOutputProof.PLedgerOutputProofControlV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PResolveInputOutputProofV1)

data PInputSignerAuthorizationV1 (s :: S)
  = PInputSignerAuthorized
  | PInputSignerMissing
  | PInputSignerProofMalformed
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PInputSignerAuthorizationV1)

data PMintFoldControlV1 (s :: S) = PMintFoldControlV1
  { pmintFold'policyCount :: Term s (PAsData PInteger)
  , pmintFold'policyCursor :: Term s (PAsData PInteger)
  , pmintFold'previousPolicy :: Term s (PAsData PByteString)
  , pmintFold'activePolicy :: Term s (PAsData PByteString)
  , pmintFold'itemLength :: Term s (PAsData PInteger)
  , pmintFold'itemCommitment :: Term s (PAsData PByteString)
  , pmintFold'itemCursor :: Term s (PAsData PInteger)
  , pmintFold'assetsRemaining :: Term s (PAsData PInteger)
  , pmintFold'policyAssetCursor :: Term s (PAsData PInteger)
  , pmintFold'previousAsset :: Term s (PAsData PByteString)
  , pmintFold'assetCount :: Term s (PAsData PInteger)
  , pmintFold'assetPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintFoldControlV1)

data PMintChunkWindowV1 (s :: S) = PMintChunkWindowV1
  (Term s PByteString)
  (Term s PInteger)
  (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PMintChunkWindowV1)

data PScriptDiscoveryControlV1 (s :: S) = PScriptDiscoveryControlV1
  { pscriptDiscovery'purposeCursor :: Term s (PAsData PInteger)
  , pscriptDiscovery'sourceCursor :: Term s (PAsData PInteger)
  , pscriptDiscovery'redeemerCursor :: Term s (PAsData PInteger)
  , pscriptDiscovery'currentPurposeKind :: Term s (PAsData PInteger)
  , pscriptDiscovery'currentPurposeIndex :: Term s (PAsData PInteger)
  , pscriptDiscovery'currentScriptHash :: Term s (PAsData PByteString)
  , pscriptDiscovery'currentSubject :: Term s (PAsData PByteString)
  , pscriptDiscovery'matchedSourceIndex :: Term s (PAsData PInteger)
  , pscriptDiscovery'matchedLanguageTag :: Term s (PAsData PInteger)
  , pscriptDiscovery'matchedSourceLeaf :: Term s (PAsData PByteString)
  , pscriptDiscovery'usedInlineBitmap :: Term s (PAsData PInteger)
  , pscriptDiscovery'usedRedeemerBitmap :: Term s (PAsData PInteger)
  , pscriptDiscovery'redeemerItemControlHash :: Term s (PAsData PByteString)
  , pscriptDiscovery'executionCount :: Term s (PAsData PInteger)
  , pscriptDiscovery'executionPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptDiscoveryControlV1)

data PInlineSourceHashControlV1 (s :: S) = PInlineSourceHashControlV1
  { pinlineSource'version :: Term s (PAsData PInteger)
  , pinlineSource'sourceIndex :: Term s (PAsData PInteger)
  , pinlineSource'sourceTotalCount :: Term s (PAsData PInteger)
  , pinlineSource'languageTag :: Term s (PAsData PInteger)
  , pinlineSource'payloadOffset :: Term s (PAsData PInteger)
  , pinlineSource'payloadLength :: Term s (PAsData PInteger)
  , pinlineSource'itemLength :: Term s (PAsData PInteger)
  , pinlineSource'itemCommitment :: Term s (PAsData PByteString)
  , pinlineSource'hashControl :: Term s (PAsData Blake2b224.PBlake2b224TraceControlV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PInlineSourceHashControlV1)

data PScriptSourcesControlV1 (s :: S) = PScriptSourcesControlV1
  { pscriptSources'compactCbor :: Term s (PAsData PByteString)
  , pscriptSources'witnessSetCompactCbor :: Term s (PAsData PByteString)
  , pscriptSources'fieldPreimageLengthsCbor :: Term s (PAsData PByteString)
  , pscriptSources'contextCbor :: Term s (PAsData PByteString)
  , pscriptSources'resolvedInputCount :: Term s (PAsData PInteger)
  , pscriptSources'resolvedInputsAccumulator :: Term s (PAsData PByteString)
  , pscriptSources'signerCount :: Term s (PAsData PInteger)
  , pscriptSources'signerFrontierCommitment :: Term s (PAsData PByteString)
  , pscriptSources'resolvedItemPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pscriptSources'stage :: Term s (PAsData PInteger)
  , pscriptSources'sourceCount :: Term s (PAsData PInteger)
  , pscriptSources'sourcePeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pscriptSources'redeemerCount :: Term s (PAsData PInteger)
  , pscriptSources'redeemerPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pscriptSources'replayCursor :: Term s (PAsData PInteger)
  , pscriptSources'replayAccumulator :: Term s (PAsData PByteString)
  , pscriptSources'replayRemainingScheduleHash :: Term s (PAsData PByteString)
  , pscriptSources'spendIndex :: Term s (PAsData PInteger)
  , pscriptSources'purposeCount :: Term s (PAsData PInteger)
  , pscriptSources'purposePeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pscriptSources'outputCursor :: Term s (PAsData PInteger)
  , pscriptSources'outputCount :: Term s (PAsData PInteger)
  , pscriptSources'outputPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pscriptSources'outputTotalCount :: Term s (PAsData PInteger)
  , pscriptSources'receiveScan :: Term s (PAsData PReceivePurposeScanControlV1)
  , pscriptSources'sourceTotalCount :: Term s (PAsData PInteger)
  , pscriptSources'redeemerTotalCount :: Term s (PAsData PInteger)
  , pscriptSources'observerScan :: Term s (PAsData PObserverPurposeScanControlV1)
  , pscriptSources'discovery :: Term s (PAsData PScriptDiscoveryControlV1)
  , pscriptSources'outputProof :: Term s (PAsData (PMaybeData LedgerOutputProof.PLedgerOutputProofControlV1))
  , pscriptSources'pendingSourceCbor :: Term s (PAsData PByteString)
  , pscriptSources'mintFold :: Term s (PAsData PMintFoldControlV1)
  , pscriptSources'resolutionScheduleHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesControlV1)

data PValidationContextV1 (s :: S) = PValidationContextV1
  { pvalidationContext'blockEndTimeMs :: Term s (PAsData PInteger)
  , pvalidationContext'expectedNetworkId :: Term s (PAsData PInteger)
  , pvalidationContext'minFeeA :: Term s (PAsData PInteger)
  , pvalidationContext'minFeeB :: Term s (PAsData PInteger)
  , pvalidationContext'blockSlot :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationContextV1)

data PInputSetsControlV1 (s :: S) = PInputSetsControlV1
  { pinputSets'compactCbor :: Term s (PAsData PByteString)
  , pinputSets'witnessSetCompactCbor :: Term s (PAsData PByteString)
  , pinputSets'fieldPreimageLengthsCbor :: Term s (PAsData PByteString)
  , pinputSets'contextCbor :: Term s (PAsData PByteString)
  , pinputSets'spendCount :: Term s (PAsData PInteger)
  , pinputSets'referenceCount :: Term s (PAsData PInteger)
  , pinputSets'spendSeen :: Term s (PAsData PInteger)
  , pinputSets'referenceSeen :: Term s (PAsData PInteger)
  , pinputSets'previousKey :: Term s (PAsData PByteString)
  , pinputSets'resolutionScheduleHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PInputSetsControlV1)

data PSignaturesControlV1 (s :: S) = PSignaturesControlV1
  { psignatures'compactCbor :: Term s (PAsData PByteString)
  , psignatures'witnessSetCompactCbor :: Term s (PAsData PByteString)
  , psignatures'fieldPreimageLengthsCbor :: Term s (PAsData PByteString)
  , psignatures'contextCbor :: Term s (PAsData PByteString)
  , psignatures'resolutionScheduleHash :: Term s (PAsData PByteString)
  , psignatures'stage :: Term s (PAsData PInteger)
  , psignatures'addressCount :: Term s (PAsData PInteger)
  , psignatures'requiredCount :: Term s (PAsData PInteger)
  , psignatures'addressSeen :: Term s (PAsData PInteger)
  , psignatures'requiredSeen :: Term s (PAsData PInteger)
  , psignatures'previousOrderKey :: Term s (PAsData PByteString)
  , psignatures'previousSignerHash :: Term s (PAsData PByteString)
  , psignatures'signerCount :: Term s (PAsData PInteger)
  , psignatures'signerPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , psignatures'invalidSignatureSeen :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSignaturesControlV1)

data PPhaseANativeScriptsControlV1 (s :: S) = PPhaseANativeScriptsControlV1
  { pphaseANative'compactCbor :: Term s (PAsData PByteString)
  , pphaseANative'witnessSetCompactCbor :: Term s (PAsData PByteString)
  , pphaseANative'fieldPreimageLengthsCbor :: Term s (PAsData PByteString)
  , pphaseANative'contextCbor :: Term s (PAsData PByteString)
  , pphaseANative'resolutionScheduleHash :: Term s (PAsData PByteString)
  , pphaseANative'stage :: Term s (PAsData PInteger)
  , pphaseANative'scriptCount :: Term s (PAsData PInteger)
  , pphaseANative'scriptSeen :: Term s (PAsData PInteger)
  , pphaseANative'containsNonNativeScript :: Term s (PAsData PInteger)
  , pphaseANative'itemLength :: Term s (PAsData PInteger)
  , pphaseANative'itemCommitment :: Term s (PAsData PByteString)
  , pphaseANative'cursor :: Term s (PAsData PInteger)
  , pphaseANative'stackRoot :: Term s (PAsData PByteString)
  , pphaseANative'stackDepth :: Term s (PAsData PInteger)
  , pphaseANative'nodeCount :: Term s (PAsData PInteger)
  , pphaseANative'result :: Term s (PAsData PInteger)
  , pphaseANative'signerCount :: Term s (PAsData PInteger)
  , pphaseANative'signerPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pphaseANative'continuationCbor :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPhaseANativeScriptsControlV1)

data PPhaseAScriptPreconditionsControlV1 (s :: S) = PPhaseAScriptPreconditionsControlV1
  { pphaseAPreconditions'compactCbor :: Term s (PAsData PByteString)
  , pphaseAPreconditions'witnessSetCompactCbor :: Term s (PAsData PByteString)
  , pphaseAPreconditions'fieldPreimageLengthsCbor :: Term s (PAsData PByteString)
  , pphaseAPreconditions'contextCbor :: Term s (PAsData PByteString)
  , pphaseAPreconditions'resolutionScheduleHash :: Term s (PAsData PByteString)
  , pphaseAPreconditions'signerCount :: Term s (PAsData PInteger)
  , pphaseAPreconditions'signerFrontierCommitment :: Term s (PAsData PByteString)
  , pphaseAPreconditions'containsNonNativeScript :: Term s (PAsData PInteger)
  , pphaseAPreconditions'observerCount :: Term s (PAsData PInteger)
  , pphaseAPreconditions'observerSeen :: Term s (PAsData PInteger)
  , pphaseAPreconditions'previousObserver :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPhaseAScriptPreconditionsControlV1)

data PResolveInputsControlV1 (s :: S) = PResolveInputsControlV1
  { presolveInputs'compactCbor :: Term s (PAsData PByteString)
  , presolveInputs'witnessSetCompactCbor :: Term s (PAsData PByteString)
  , presolveInputs'fieldPreimageLengthsCbor :: Term s (PAsData PByteString)
  , presolveInputs'contextCbor :: Term s (PAsData PByteString)
  , presolveInputs'cursor :: Term s (PAsData PInteger)
  , presolveInputs'accumulator :: Term s (PAsData PByteString)
  , presolveInputs'remainingScheduleHash :: Term s (PAsData PByteString)
  , presolveInputs'signerCount :: Term s (PAsData PInteger)
  , presolveInputs'signerFrontierCommitment :: Term s (PAsData PByteString)
  , presolveInputs'pending :: Term s (PAsData (PMaybeData PResolveInputOutputProofV1))
  , presolveInputs'resolutionScheduleHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PResolveInputsControlV1)

data PValueAccumulatorV1 (s :: S) = PValueAccumulatorV1
  { pvalueAccumulator'lovelaceDelta :: Term s (PAsData PInteger)
  , pvalueAccumulator'assetRoot :: Term s (PAsData PByteString)
  , pvalueAccumulator'seenAssetCount :: Term s (PAsData PInteger)
  , pvalueAccumulator'nonzeroAssetCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValueAccumulatorV1)

data PValueAccumulatorUpdateV1 (s :: S)
  = PValueAccumulatorUpdated (Term s (PAsData PValueAccumulatorV1))
  | PValueAccumulatorAssetLimitExceeded
  | PValueAccumulatorMutationInvalid
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValueAccumulatorUpdateV1)

data PValueAssetMutationWitnessV1 (s :: S) = PValueAssetMutationWitnessV1
  { pvalueMutation'deltaWasPresent :: Term s (PAsData PBool)
  , pvalueMutation'oldDelta :: Term s (PAsData PInteger)
  , pvalueMutation'deltaProof :: Term s (PAsData (PBuiltinList (PAsData PProofStep)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValueAssetMutationWitnessV1)

data PLedgerDeltaPendingMutationV1 (s :: S) = PLedgerDeltaPendingMutationV1
  { ppendingMutation'authorizationStage :: Term s (PAsData PInteger)
  , ppendingMutation'operationKind :: Term s (PAsData PInteger)
  , ppendingMutation'key :: Term s (PAsData PByteString)
  , ppendingMutation'value :: Term s (PAsData PByteString)
  , ppendingMutation'descriptor :: Term s (PAsData ProofFold.PProofDescriptorV1)
  , ppendingMutation'foldControl :: Term s (PAsData ProofFold.PProofFoldControlV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerDeltaPendingMutationV1)

data PLedgerDeltaControlV1 (s :: S) = PLedgerDeltaControlV1
  { pledgerDelta'resolvedInputCount :: Term s (PAsData PInteger)
  , pledgerDelta'resolvedInputsAccumulator :: Term s (PAsData PByteString)
  , pledgerDelta'outputCount :: Term s (PAsData PInteger)
  , pledgerDelta'outputDescriptorPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pledgerDelta'stage :: Term s (PAsData PInteger)
  , pledgerDelta'replayScheduleHash :: Term s (PAsData PByteString)
  , pledgerDelta'replayCursor :: Term s (PAsData PInteger)
  , pledgerDelta'replayAccumulator :: Term s (PAsData PByteString)
  , pledgerDelta'replayRemainingScheduleHash :: Term s (PAsData PByteString)
  , pledgerDelta'currentLedgerRoot :: Term s (PAsData PByteString)
  , pledgerDelta'outputCursor :: Term s (PAsData PInteger)
  , pledgerDelta'operationCount :: Term s (PAsData PInteger)
  , pledgerDelta'operationPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pledgerDelta'pendingMutationCbor :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerDeltaControlV1)

data PNativeScriptsControlV1 (s :: S) = PNativeScriptsControlV1
  { pnativeControl'compactCbor :: Term s (PAsData PByteString)
  , pnativeControl'witnessSetCompactCbor :: Term s (PAsData PByteString)
  , pnativeControl'fieldPreimageLengthsCbor :: Term s (PAsData PByteString)
  , pnativeControl'contextCbor :: Term s (PAsData PByteString)
  , pnativeControl'resolvedInputCount :: Term s (PAsData PInteger)
  , pnativeControl'resolvedInputsAccumulator :: Term s (PAsData PByteString)
  , pnativeControl'spendInputCount :: Term s (PAsData PInteger)
  , pnativeControl'resolvedItemPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pnativeControl'signerCount :: Term s (PAsData PInteger)
  , pnativeControl'signerFrontierCommitment :: Term s (PAsData PByteString)
  , pnativeControl'sourceCount :: Term s (PAsData PInteger)
  , pnativeControl'sourcePeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pnativeControl'redeemerCount :: Term s (PAsData PInteger)
  , pnativeControl'redeemerPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pnativeControl'purposeCount :: Term s (PAsData PInteger)
  , pnativeControl'purposePeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pnativeControl'outputCount :: Term s (PAsData PInteger)
  , pnativeControl'outputPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pnativeControl'outputDescriptorPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pnativeControl'mintCount :: Term s (PAsData PInteger)
  , pnativeControl'mintPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pnativeControl'executionCount :: Term s (PAsData PInteger)
  , pnativeControl'executionPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pnativeControl'executionCursor :: Term s (PAsData PInteger)
  , pnativeControl'languageBitmap :: Term s (PAsData PInteger)
  , pnativeControl'resolutionScheduleHash :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeScriptsControlV1)

data PValueAndMintControlV1 (s :: S) = PValueAndMintControlV1
  { pvalueAndMint'nativeControl :: Term s (PAsData PNativeScriptsControlV1)
  , pvalueAndMint'stage :: Term s (PAsData PInteger)
  , pvalueAndMint'replayScheduleHash :: Term s (PAsData PByteString)
  , pvalueAndMint'replayCursor :: Term s (PAsData PInteger)
  , pvalueAndMint'replayAssetCursor :: Term s (PAsData PInteger)
  , pvalueAndMint'replayValueHash :: Term s (PAsData PByteString)
  , pvalueAndMint'replayAccumulator :: Term s (PAsData PByteString)
  , pvalueAndMint'replayRemainingScheduleHash :: Term s (PAsData PByteString)
  , pvalueAndMint'outputCursor :: Term s (PAsData PInteger)
  , pvalueAndMint'outputAssetCursor :: Term s (PAsData PInteger)
  , pvalueAndMint'mintCursor :: Term s (PAsData PInteger)
  , pvalueAndMint'valueAccumulator :: Term s (PAsData PValueAccumulatorV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValueAndMintControlV1)

data PCekRedeemerContextControlV1 (s :: S) = PCekRedeemerContextControlV1
  { pcekRedeemer'cursor :: Term s (PAsData PInteger)
  , pcekRedeemer'mapItems :: Term s (PAsData CekData.PDataSequenceSummaryV1)
  , pcekRedeemer'activeScanHash :: Term s (PAsData PByteString)
  , pcekRedeemer'activeRedeemerLeaf :: Term s (PAsData PByteString)
  , pcekRedeemer'activePurpose :: Term s (PAsData CekData.PDataSummaryV1)
  , pcekRedeemer'currentRedeemer :: Term s (PAsData CekData.PDataSummaryV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekRedeemerContextControlV1)

data PCekFinalContextControlV1 (s :: S) = PCekFinalContextControlV1
  { pcekFinal'txInfo :: Term s (PAsData CekData.PDataSummaryV1)
  , pcekFinal'redeemer :: Term s (PAsData CekData.PDataSummaryV1)
  , pcekFinal'scriptInfo :: Term s (PAsData CekData.PDataSummaryV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekFinalContextControlV1)

data PCekContextPartsControlV1 (s :: S) = PCekContextPartsControlV1
  { pcekParts'redeemerItems :: Term s (PAsData CekData.PDataSequenceSummaryV1)
  , pcekParts'redeemer :: Term s (PAsData CekData.PDataSummaryV1)
  , pcekParts'scriptInfo :: Term s (PAsData CekData.PDataSummaryV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekContextPartsControlV1)

data PCekTxInfoAssemblyControlV1 (s :: S) = PCekTxInfoAssemblyControlV1
  { pcekAssembly'tailFields :: Term s (PAsData CekData.PDataSequenceSummaryV1)
  , pcekAssembly'redeemer :: Term s (PAsData CekData.PDataSummaryV1)
  , pcekAssembly'scriptInfo :: Term s (PAsData CekData.PDataSummaryV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekTxInfoAssemblyControlV1)

data PCekContextControlV1 (s :: S) = PCekContextControlV1
  { pcekContext'stage :: Term s (PAsData PInteger)
  , pcekContext'languageTag :: Term s (PAsData PInteger)
  , pcekContext'programTermRoot :: Term s (PAsData PByteString)
  , pcekContext'programEnvelopeHash :: Term s (PAsData PByteString)
  , pcekContext'purposeKind :: Term s (PAsData PInteger)
  , pcekContext'purposeIndex :: Term s (PAsData PInteger)
  , pcekContext'scriptHash :: Term s (PAsData PByteString)
  , pcekContext'subject :: Term s (PAsData PByteString)
  , pcekContext'redeemerLeaf :: Term s (PAsData PByteString)
  , pcekContext'redeemerContextControlHash :: Term s (PAsData PByteString)
  , pcekContext'executionMemoryLimit :: Term s (PAsData PInteger)
  , pcekContext'executionCpuLimit :: Term s (PAsData PInteger)
  , pcekContext'referenceItems :: Term s (PAsData CekData.PDataSequenceSummaryV1)
  , pcekContext'spendItems :: Term s (PAsData CekData.PDataSequenceSummaryV1)
  , pcekContext'outputItems :: Term s (PAsData CekData.PDataSequenceSummaryV1)
  , pcekContext'signerItems :: Term s (PAsData CekData.PDataSequenceSummaryV1)
  , pcekContext'observerCount :: Term s (PAsData PInteger)
  , pcekContext'observerItems :: Term s (PAsData CekData.PDataSequenceSummaryV1)
  , pcekContext'previousObserver :: Term s (PAsData PByteString)
  , pcekContext'observerSummary :: Term s (PAsData CekData.PDataSummaryV1)
  , pcekContext'mintCursor :: Term s (PAsData PInteger)
  , pcekContext'currentMintPolicy :: Term s (PAsData PByteString)
  , pcekContext'currentMintAssets :: Term s (PAsData CekData.PDataSequenceSummaryV1)
  , pcekContext'mintPolicies :: Term s (PAsData CekData.PDataSequenceSummaryV1)
  , pcekContext'mintSummary :: Term s (PAsData CekData.PDataSummaryV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekContextControlV1)

data PLedgerDeltaOperationProofV1 (s :: S) = PLedgerDeltaOperationProofV1
  { pledgerOperationProof'descriptor :: Term s (PAsData ProofFold.PProofDescriptorV1)
  , pledgerOperationProof'operationCount :: Term s (PAsData PInteger)
  , pledgerOperationProof'operationPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pledgerOperationProof'operationIndex :: Term s (PAsData PInteger)
  , pledgerOperationProof'operationSiblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerDeltaOperationProofV1)

data PSignerSetProofV1 (s :: S)
  = PNoSignerSetProof
  | PSignerMembershipProof
      (Term s (PAsData (PBuiltinList (PAsData PFrontierPeak))))
      (Term s (PAsData PInteger))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PEmptySignerSetProof
      (Term s (PAsData (PBuiltinList (PAsData PFrontierPeak))))
  | PSignerBelowFirstProof
      (Term s (PAsData (PBuiltinList (PAsData PFrontierPeak))))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PSignerAboveLastProof
      (Term s (PAsData (PBuiltinList (PAsData PFrontierPeak))))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PSignerBetweenProof
      (Term s (PAsData (PBuiltinList (PAsData PFrontierPeak))))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSignerSetProofV1)

-- | Aiken @ValidationAuxiliaryWitnessV1@. Constructor order is the datum ABI.
data PValidationAuxiliaryWitnessV1 (s :: S)
  = PNoAuxiliaryWitness
  | PTransactionFieldChunkWitness
      (Term s (PAsData PItemProofV1))
      (Term s (PAsData BoundedItem.PChunkProofV1))
  | PRequiredSignerItemWitness
      (Term s (PAsData PItemProofV1))
      (Term s (PAsData BoundedItem.PChunkProofV1))
      (Term s (PAsData PSignerSetProofV1))
  | PNativeScriptTokenWitness
      (Term s (PAsData BoundedItem.PChunkProofV1))
      (Term s (PAsData (PMaybeData BoundedItem.PChunkProofV1)))
      (Term s (PAsData PSignerSetProofV1))
  | PNativeScriptFrameWitness
      (Term s (PAsData NativeScriptScan.PNativeScriptFrameV1))
  | PScheduledLedgerMembershipWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PProof))
      (Term s (PAsData PSignerSetProofV1))
  | PScheduledLedgerNonMembershipWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PProof))
  | PResolvedInputReplayWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
  | PScriptPurposeScanWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PScriptSourceScanWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PRedeemerScanBeginWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PNativeExecutionScanWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData BoundedItem.PChunkProofV1))
  | PCekCoreStepWitness
      (Term s (PAsData CekMachine.PCoreStepEvidenceV1))
  | PCekResolvedContextItemWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PCekOutputContextItemWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PCekSignerContextItemWitness
      (Term s (PAsData (PBuiltinList (PAsData PFrontierPeak))))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PCekMintContextItemWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PCekRedeemerContextSelectWitness
      (Term s (PAsData PCekRedeemerContextControlV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PRedeemerItemStepWitness
      (Term s (PAsData (PMaybeData PCekRedeemerContextControlV1)))
      (Term s (PAsData RedeemerItemProof.PRedeemerItemProofControlV1))
      (Term s (PAsData RedeemerItemProof.PRedeemerItemProofWitnessV1))
  | PCekContextFinalizeWitness
      (Term s (PAsData PCekRedeemerContextControlV1))
  | PCekContextFinalizeSpendWitness
      (Term s (PAsData PCekRedeemerContextControlV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PCekContextAssembleWitness
      (Term s (PAsData PCekContextPartsControlV1))
  | PCekTxInfoFinalizeWitness
      (Term s (PAsData PCekTxInfoAssemblyControlV1))
  | PCekContextSeedWitness
      (Term s (PAsData PCekFinalContextControlV1))
  | PValueInputAssetWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData (PBuiltinList (PAsData PFrontierPeak))))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData PValueAssetMutationWitnessV1))
  | PValueOutputAssetWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData (PBuiltinList (PAsData PFrontierPeak))))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData PValueAssetMutationWitnessV1))
  | PValueMintAssetWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData PValueAssetMutationWitnessV1))
  | PLedgerDeltaReplayWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
  | PLedgerDeltaOutputWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PTransactionRedeemerItemBeginWitness
      (Term s (PAsData PItemProofV1))
  | PTransactionFieldItemWitness
      (Term s (PAsData PItemProofV1))
      (Term s (PAsData PByteString))
  | PLedgerOutputProofBeginWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PLedgerOutputProofStepWitness
      (Term s (PAsData LedgerOutputProof.PLedgerOutputProofWitnessV1))
  | PLedgerOutputProofFinalizeWitness
      (Term s (PAsData PByteString))
      (Term s (PAsData PSignerSetProofV1))
  | PLedgerDeltaProofFrameWitness
      (Term s (PAsData ProofFold.PProofFrameV1))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PLedgerDeltaOperationWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PLedgerDeltaOperationProofV1))
  | PScriptSourceHashBlockWitness
      (Term s (PAsData BoundedItem.PChunkProofV1))
      (Term s (PAsData (PMaybeData BoundedItem.PChunkProofV1)))
  | PNativeExecutionDescriptorWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PAsData (PMaybeData BoundedItem.PChunkProofV1)))
      (Term s (PAsData (PBuiltinList (PAsData PFrontierPeak))))
  | PValueOutputDescriptorWitness
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PMintFoldAssetWitness
      (Term s (PAsData BoundedItem.PChunkProofV1))
      (Term s (PAsData (PMaybeData BoundedItem.PChunkProofV1)))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationAuxiliaryWitnessV1)

-- | Strict outer decoder for Aiken @ValidationAuxiliaryWitnessV1@ data.
-- Plutarch's derived matcher does not reject unknown tags or surplus fields.
pvalidationAuxiliaryWitnessFromData :: forall s.
  Term s (PData :--> PValidationAuxiliaryWitnessV1)
pvalidationAuxiliaryWitnessFromData = phoistAcyclic $ plam $ \dat ->
  pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
    pif
      (tag #>= 0 #&& tag #< 40)
      ( plet (pindexBS' # pconstant auxiliaryArities # tag) $ \expectedArity ->
          pif
            (plength # fields #== expectedArity)
            (punsafeCoerce dat)
            perror
      )
      perror
  where
    auxiliaryArities :: BS.ByteString
    auxiliaryArities =
      BS.pack
        [ 0, 2, 3, 3, 1, 6, 4, 4, 5, 8
        , 5, 16, 1, 5, 3, 4, 5, 12, 3, 1
        , 5, 1, 1, 1, 11, 9, 6, 4, 3, 1
        , 2, 4, 1, 2, 2, 4, 2, 17, 3, 2
        ]

pdecodeValidationAuxiliaryWitnessV1 :: forall s.
  Term s (PAsData PValidationAuxiliaryWitnessV1) ->
  Term s PValidationAuxiliaryWitnessV1
pdecodeValidationAuxiliaryWitnessV1 auxiliary =
  pvalidationAuxiliaryWitnessFromData # pforgetData auxiliary

data PValidationOneStepEvidenceV1 (s :: S) = PValidationOneStepEvidenceV1
  { poneStepEvidence'transition :: Term s (PAsData PValidationOneStepWitnessV1)
  , poneStepEvidence'auxiliary :: Term s (PAsData PValidationAuxiliaryWitnessV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationOneStepEvidenceV1)

data PValidationProofItemDatumV1 (s :: S) = PValidationProofItemDatumV1
  { pproofItem'version :: Term s (PAsData PInteger)
  , pproofItem'transactionId :: Term s (PAsData PByteString)
  , pproofItem'transactionCommitment :: Term s (PAsData PByteString)
  , pproofItem'collectionProof :: Term s (PAsData PItemProofV1)
  , pproofItem'itemCbor :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationProofItemDatumV1)

pencodeDataSequenceSummaryV1 :: forall s. Term s (CekData.PDataSequenceSummaryV1 :--> PByteString)
pencodeDataSequenceSummaryV1 = phoistAcyclic $ plam $ \summary -> pmatch summary $ \s ->
  pconstant "\x84"
    <> (pencodeDefiniteBytes # pfromData (CekData.pseq'root s))
    <> pcborInt (pfromData (CekData.pseq'length s))
    <> pcborInt (pfromData (CekData.pseq'payloadCborLength s))
    <> pcborInt (pfromData (CekData.pseq'memory s))

pdataSequenceSummaryFromData :: forall s. Term s (PData :--> CekData.PDataSequenceSummaryV1)
pdataSequenceSummaryFromData = phoistAcyclic $ plam $ \dat ->
  plet (pasList # dat) $ \items ->
    pif (plength # items #== 4)
      ( pcon $ CekData.PDataSequenceSummaryV1
          (pdata $ pasByteStr # (pelemAt # 0 # items))
          (pdata $ pasInt # (pelemAt # 1 # items))
          (pdata $ pasInt # (pelemAt # 2 # items))
          (pdata $ pasInt # (pelemAt # 3 # items))
      )
      perror

pencodeDataSummaryV1 :: forall s. Term s (CekData.PDataSummaryV1 :--> PByteString)
pencodeDataSummaryV1 = phoistAcyclic $ plam $ \summary -> pmatch summary $ \s ->
  pconstant "\x83"
    <> (pencodeDefiniteBytes # pfromData (CekData.psummary'root s))
    <> pcborInt (pfromData (CekData.psummary'cborLength s))
    <> pcborInt (pfromData (CekData.psummary'memory s))

pdataSummaryFromData :: forall s. Term s (PData :--> CekData.PDataSummaryV1)
pdataSummaryFromData = phoistAcyclic $ plam $ \dat ->
  plet (pasList # dat) $ \items ->
    pif (plength # items #== 3)
      ( pcon $ CekData.PDataSummaryV1
          (pdata $ pasByteStr # (pelemAt # 0 # items))
          (pdata $ pasInt # (pelemAt # 1 # items))
          (pdata $ pasInt # (pelemAt # 2 # items))
      )
      perror

pemptyDataSummaryV1 :: forall s. Term s CekData.PDataSummaryV1
pemptyDataSummaryV1 = pcon $ CekData.PDataSummaryV1 (pdata $ pconstant "") (pdata 0) (pdata 0)

pdataSequenceSummaryIsWellFormed :: forall s. Term s (CekData.PDataSequenceSummaryV1 :--> PBool)
pdataSequenceSummaryIsWellFormed = phoistAcyclic $ plam $ \summary -> pmatch summary $ \s ->
  pand'List
    [ plengthBS # pfromData (CekData.pseq'root s) #== 32
    , pfromData (CekData.pseq'length s) #>= 0
    , pfromData (CekData.pseq'payloadCborLength s) #>= 0
    , pfromData (CekData.pseq'memory s) #>= 0
    ]

poptionalDataSummaryIsWellFormed :: forall s. Term s (CekData.PDataSummaryV1 :--> PBool)
poptionalDataSummaryIsWellFormed = phoistAcyclic $ plam $ \summary -> pmatch summary $ \s ->
  pif (pfromData (CekData.psummary'root s) #== pconstant "")
    (pfromData (CekData.psummary'cborLength s) #== 0 #&& pfromData (CekData.psummary'memory s) #== 0)
    ( plengthBS # pfromData (CekData.psummary'root s) #== 32
        #&& pfromData (CekData.psummary'cborLength s) #>= 0
        #&& pfromData (CekData.psummary'memory s) #>= 0
    )

pdataSummaryRoot :: forall s. Term s (CekData.PDataSummaryV1 :--> PByteString)
pdataSummaryRoot = phoistAcyclic $ plam $ \summary ->
  pmatch summary $ \s -> pfromData (CekData.psummary'root s)

pdataSequenceSummaryLength :: forall s. Term s (CekData.PDataSequenceSummaryV1 :--> PInteger)
pdataSequenceSummaryLength = phoistAcyclic $ plam $ \summary ->
  pmatch summary $ \s -> pfromData (CekData.pseq'length s)

pencodeCekContextControlV1 :: forall s. Term s (PCekContextControlV1 :--> PByteString)
pencodeCekContextControlV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pencodeDefiniteArrayHeader # 25
    <> pcborInt (pfromData $ pcekContext'stage c)
    <> pcborInt (pfromData $ pcekContext'languageTag c)
    <> (pencodeDefiniteBytes # pfromData (pcekContext'programTermRoot c))
    <> (pencodeDefiniteBytes # pfromData (pcekContext'programEnvelopeHash c))
    <> pcborInt (pfromData $ pcekContext'purposeKind c)
    <> pcborInt (pfromData $ pcekContext'purposeIndex c)
    <> (pencodeDefiniteBytes # pfromData (pcekContext'scriptHash c))
    <> (pencodeDefiniteBytes # pfromData (pcekContext'subject c))
    <> (pencodeDefiniteBytes # pfromData (pcekContext'redeemerLeaf c))
    <> (pencodeDefiniteBytes # pfromData (pcekContext'redeemerContextControlHash c))
    <> pcborInt (pfromData $ pcekContext'executionMemoryLimit c)
    <> pcborInt (pfromData $ pcekContext'executionCpuLimit c)
    <> (pencodeDataSequenceSummaryV1 # pfromData (pcekContext'referenceItems c))
    <> (pencodeDataSequenceSummaryV1 # pfromData (pcekContext'spendItems c))
    <> (pencodeDataSequenceSummaryV1 # pfromData (pcekContext'outputItems c))
    <> (pencodeDataSequenceSummaryV1 # pfromData (pcekContext'signerItems c))
    <> pcborInt (pfromData $ pcekContext'observerCount c)
    <> (pencodeDataSequenceSummaryV1 # pfromData (pcekContext'observerItems c))
    <> (pencodeDefiniteBytes # pfromData (pcekContext'previousObserver c))
    <> (pencodeDataSummaryV1 # pfromData (pcekContext'observerSummary c))
    <> pcborInt (pfromData $ pcekContext'mintCursor c)
    <> (pencodeDefiniteBytes # pfromData (pcekContext'currentMintPolicy c))
    <> (pencodeDataSequenceSummaryV1 # pfromData (pcekContext'currentMintAssets c))
    <> (pencodeDataSequenceSummaryV1 # pfromData (pcekContext'mintPolicies c))
    <> (pencodeDataSummaryV1 # pfromData (pcekContext'mintSummary c))

pcekContextControlFromCbor :: forall s. Term s (PByteString :--> PCekContextControlV1)
pcekContextControlFromCbor = phoistAcyclic $ plam $ \controlCbor ->
  pmatch (pdeserialise # controlCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \items ->
      pif (plength # items #== 25)
        ( pcon $ PCekContextControlV1
            (pdata $ pasInt # (pelemAt # 0 # items))
            (pdata $ pasInt # (pelemAt # 1 # items))
            (pdata $ pasByteStr # (pelemAt # 2 # items))
            (pdata $ pasByteStr # (pelemAt # 3 # items))
            (pdata $ pasInt # (pelemAt # 4 # items))
            (pdata $ pasInt # (pelemAt # 5 # items))
            (pdata $ pasByteStr # (pelemAt # 6 # items))
            (pdata $ pasByteStr # (pelemAt # 7 # items))
            (pdata $ pasByteStr # (pelemAt # 8 # items))
            (pdata $ pasByteStr # (pelemAt # 9 # items))
            (pdata $ pasInt # (pelemAt # 10 # items))
            (pdata $ pasInt # (pelemAt # 11 # items))
            (pdata $ pdataSequenceSummaryFromData # (pelemAt # 12 # items))
            (pdata $ pdataSequenceSummaryFromData # (pelemAt # 13 # items))
            (pdata $ pdataSequenceSummaryFromData # (pelemAt # 14 # items))
            (pdata $ pdataSequenceSummaryFromData # (pelemAt # 15 # items))
            (pdata $ pasInt # (pelemAt # 16 # items))
            (pdata $ pdataSequenceSummaryFromData # (pelemAt # 17 # items))
            (pdata $ pasByteStr # (pelemAt # 18 # items))
            (pdata $ pdataSummaryFromData # (pelemAt # 19 # items))
            (pdata $ pasInt # (pelemAt # 20 # items))
            (pdata $ pasByteStr # (pelemAt # 21 # items))
            (pdata $ pdataSequenceSummaryFromData # (pelemAt # 22 # items))
            (pdata $ pdataSequenceSummaryFromData # (pelemAt # 23 # items))
            (pdata $ pdataSummaryFromData # (pelemAt # 24 # items))
        )
        perror

pencodeCekRedeemerContextControlV1 :: forall s. Term s (PCekRedeemerContextControlV1 :--> PByteString)
pencodeCekRedeemerContextControlV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pconstant "\x86"
    <> pcborInt (pfromData $ pcekRedeemer'cursor c)
    <> (pencodeDataSequenceSummaryV1 # pfromData (pcekRedeemer'mapItems c))
    <> (pencodeDefiniteBytes # pfromData (pcekRedeemer'activeScanHash c))
    <> (pencodeDefiniteBytes # pfromData (pcekRedeemer'activeRedeemerLeaf c))
    <> (pencodeDataSummaryV1 # pfromData (pcekRedeemer'activePurpose c))
    <> (pencodeDataSummaryV1 # pfromData (pcekRedeemer'currentRedeemer c))

phashCekRedeemerContextControlV1 :: forall s. Term s (PCekRedeemerContextControlV1 :--> PByteString)
phashCekRedeemerContextControlV1 = phoistAcyclic $ plam $ \control ->
  pblake2b_256 # (pconstant "MidgardCekRedeemerContextControlV1" <> (pencodeCekRedeemerContextControlV1 # control))

pinitialCekRedeemerContextControlV1 :: forall s. Term s PCekRedeemerContextControlV1
pinitialCekRedeemerContextControlV1 = pcon $ PCekRedeemerContextControlV1
  (pdata 0)
  (pdata CekData.pemptyDataPairSummaryV1)
  (pdata $ pconstant "")
  (pdata $ pconstant "")
  (pdata pemptyDataSummaryV1)
  (pdata pemptyDataSummaryV1)

pencodeCekFinalContextControlV1 :: forall s. Term s (PCekFinalContextControlV1 :--> PByteString)
pencodeCekFinalContextControlV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pconstant "\x83"
    <> (pencodeDataSummaryV1 # pfromData (pcekFinal'txInfo c))
    <> (pencodeDataSummaryV1 # pfromData (pcekFinal'redeemer c))
    <> (pencodeDataSummaryV1 # pfromData (pcekFinal'scriptInfo c))

phashCekFinalContextControlV1 :: forall s. Term s (PCekFinalContextControlV1 :--> PByteString)
phashCekFinalContextControlV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pif
    ( pand'List
        [ poptionalDataSummaryIsWellFormed # pfromData (pcekFinal'txInfo c)
        , poptionalDataSummaryIsWellFormed # pfromData (pcekFinal'redeemer c)
        , poptionalDataSummaryIsWellFormed # pfromData (pcekFinal'scriptInfo c)
        , pdataSummaryRoot # pfromData (pcekFinal'txInfo c) #/= pconstant ""
        , pdataSummaryRoot # pfromData (pcekFinal'redeemer c) #/= pconstant ""
        , pdataSummaryRoot # pfromData (pcekFinal'scriptInfo c) #/= pconstant ""
        ]
    )
    (pblake2b_256 # (pconstant "MidgardCekFinalContextControlV1" <> (pencodeCekFinalContextControlV1 # control)))
    perror

pencodeCekContextPartsControlV1 :: forall s. Term s (PCekContextPartsControlV1 :--> PByteString)
pencodeCekContextPartsControlV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pconstant "\x83"
    <> (pencodeDataSequenceSummaryV1 # pfromData (pcekParts'redeemerItems c))
    <> (pencodeDataSummaryV1 # pfromData (pcekParts'redeemer c))
    <> (pencodeDataSummaryV1 # pfromData (pcekParts'scriptInfo c))

phashCekContextPartsControlV1 :: forall s. Term s (PCekContextPartsControlV1 :--> PByteString)
phashCekContextPartsControlV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pif
    ( pand'List
        [ pdataSequenceSummaryIsWellFormed # pfromData (pcekParts'redeemerItems c)
        , poptionalDataSummaryIsWellFormed # pfromData (pcekParts'redeemer c)
        , poptionalDataSummaryIsWellFormed # pfromData (pcekParts'scriptInfo c)
        , pdataSummaryRoot # pfromData (pcekParts'redeemer c) #/= pconstant ""
        , pdataSummaryRoot # pfromData (pcekParts'scriptInfo c) #/= pconstant ""
        ]
    )
    (pblake2b_256 # (pconstant "MidgardCekContextPartsControlV1" <> (pencodeCekContextPartsControlV1 # control)))
    perror

pencodeCekTxInfoAssemblyControlV1 :: forall s. Term s (PCekTxInfoAssemblyControlV1 :--> PByteString)
pencodeCekTxInfoAssemblyControlV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pconstant "\x83"
    <> (pencodeDataSequenceSummaryV1 # pfromData (pcekAssembly'tailFields c))
    <> (pencodeDataSummaryV1 # pfromData (pcekAssembly'redeemer c))
    <> (pencodeDataSummaryV1 # pfromData (pcekAssembly'scriptInfo c))

phashCekTxInfoAssemblyControlV1 :: forall s. Term s (PCekTxInfoAssemblyControlV1 :--> PByteString)
phashCekTxInfoAssemblyControlV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pif
    ( pand'List
        [ pdataSequenceSummaryIsWellFormed # pfromData (pcekAssembly'tailFields c)
        , poptionalDataSummaryIsWellFormed # pfromData (pcekAssembly'redeemer c)
        , poptionalDataSummaryIsWellFormed # pfromData (pcekAssembly'scriptInfo c)
        , pdataSequenceSummaryLength # pfromData (pcekAssembly'tailFields c) #> 0
        , pdataSummaryRoot # pfromData (pcekAssembly'redeemer c) #/= pconstant ""
        , pdataSummaryRoot # pfromData (pcekAssembly'scriptInfo c) #/= pconstant ""
        ]
    )
    (pblake2b_256 # (pconstant "MidgardCekTxInfoAssemblyControlV1" <> (pencodeCekTxInfoAssemblyControlV1 # control)))
    perror

pencodeCekWitnessV1 :: forall s.
  Term s
    ( PNativeScriptsControlV1
        :--> PByteString
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PByteString
        :--> PInteger
        :--> PInteger
        :--> PByteString
        :--> PByteString
    )
pencodeCekWitnessV1 = phoistAcyclic $ plam $ \nativeControl contextControlCbor executionCursor completedCpu completedMemory activeStateHash executionCpuLimit executionMemoryLimit programEnvelopeHash ->
  pmatch nativeControl $ \native ->
    pif
      ( pand'List
          [ executionCursor #>= 0
          , executionCursor #<= pfromData (pnativeControl'executionCount native)
          , completedCpu #>= 0
          , completedMemory #>= 0
          , activeStateHash #== pconstant "" #|| plengthBS # activeStateHash #== 32
          , executionCpuLimit #>= 0
          , executionMemoryLimit #>= 0
          , programEnvelopeHash #== pconstant "" #|| plengthBS # programEnvelopeHash #== 32
          ]
      )
      ( pconstant "\x89"
          <> (pencodeDefiniteBytes # (pencodeNativeScriptsControlV1 # nativeControl))
          <> (pencodeDefiniteBytes # contextControlCbor)
          <> pcborInt executionCursor
          <> pcborInt completedCpu
          <> pcborInt completedMemory
          <> (pencodeDefiniteBytes # activeStateHash)
          <> (pencodeDefiniteBytes # programEnvelopeHash)
          <> pcborInt executionCpuLimit
          <> pcborInt executionMemoryLimit
      )
      perror

-- | Aiken @prepend_cek_observer_item_v1@.
pprependCekObserverItemV1 :: forall s.
  Term s
    ( PByteString
        :--> PBool
        :--> CekData.PDataSequenceSummaryV1
        :--> CekData.PDataSequenceSummaryV1
    )
pprependCekObserverItemV1 = phoistAcyclic $ plam $ \observerHash midgardEncoding tailSummary ->
  pif
    (plengthBS # observerHash #== 28)
    ( plet (CekData.psemanticDataSummaryV1 # pforgetData (pdata observerHash)) $ \observerSummary ->
        pif
          midgardEncoding
          (CekData.pprependDataListSummaryV1 # observerSummary # tailSummary)
          ( plet
              ( CekData.psemanticDataSummaryV1
                  # (pforgetData $ pconstrBuiltin # 1 # (pcons # pforgetData (pdata observerHash) # pnil))
              )
              $ \credentialSummary ->
              plet (CekData.psemanticDataSummaryV1 # pforgetData (pdata (pconstant @PInteger 0))) $ \quantitySummary ->
                CekData.pprependDataPairSummaryV1
                  # credentialSummary
                  # quantitySummary
                  # tailSummary
          )
    )
    perror

-- | Aiken @finalize_cek_observer_items_v1@.
pfinalizeCekObserverItemsV1 :: forall s.
  Term s (CekData.PDataSequenceSummaryV1 :--> PBool :--> CekData.PDataSummaryV1)
pfinalizeCekObserverItemsV1 = phoistAcyclic $ plam $ \items midgardEncoding ->
  pif midgardEncoding
    (CekData.plistDataSummaryV1 # items)
    (CekData.pmapDataSummaryV1 # items)

pemptyObserverItemsV1 :: forall s.
  Term s (PInteger :--> CekData.PDataSequenceSummaryV1)
pemptyObserverItemsV1 = phoistAcyclic $ plam $ \languageTag ->
  pif (languageTag #== 128)
    CekData.pemptyDataListSummaryV1
    CekData.pemptyDataPairSummaryV1

pmintContextIsPristine :: forall s. Term s (PCekContextControlV1 :--> PBool)
pmintContextIsPristine = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pand'List
    [ pfromData (pcekContext'mintCursor c) #== 0
    , pfromData (pcekContext'currentMintPolicy c) #== pconstant ""
    , pfromData (pcekContext'currentMintAssets c) #== CekData.pemptyDataPairSummaryV1
    , pfromData (pcekContext'mintPolicies c) #== CekData.pemptyDataPairSummaryV1
    , pfromData (pcekContext'mintSummary c) #== pemptyDataSummaryV1
    ]

pobserverContextIsPristine :: forall s. Term s (PCekContextControlV1 :--> PBool)
pobserverContextIsPristine = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pand'List
    [ pfromData (pcekContext'observerCount c) #== 0
    , pfromData (pcekContext'observerItems c)
        #== pemptyObserverItemsV1 # pfromData (pcekContext'languageTag c)
    , pfromData (pcekContext'previousObserver c) #== pconstant ""
    , pfromData (pcekContext'observerSummary c) #== pemptyDataSummaryV1
    ]

pobserverContextIsPartial :: forall s. Term s (PCekContextControlV1 :--> PBool)
pobserverContextIsPartial = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  plet (pfromData $ pcekContext'observerCount c) $ \observerCount ->
  plet (pfromData $ pcekContext'observerItems c) $ \observerItems ->
  plet (pdataSequenceSummaryLength # observerItems) $ \observerLength ->
    pand'List
      [ observerCount #>= 0
      , observerCount #<= pmaxTxSizeDerivedItemCount
      , observerLength #<= observerCount
      , pfromData (pcekContext'observerSummary c) #== pemptyDataSummaryV1
      , pif
          (observerLength #== 0)
          ( pand'List
              [ observerCount #== 0
              , observerItems #== pemptyObserverItemsV1 # pfromData (pcekContext'languageTag c)
              , pfromData (pcekContext'previousObserver c) #== pconstant ""
              ]
          )
          ( observerCount #> 0
              #&& plengthBS # pfromData (pcekContext'previousObserver c) #== 28
          )
      ]

pobserverContextIsComplete :: forall s. Term s (PCekContextControlV1 :--> PBool)
pobserverContextIsComplete = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  plet (pfromData $ pcekContext'observerCount c) $ \observerCount ->
  plet (pfromData $ pcekContext'observerItems c) $ \observerItems ->
    pand'List
      [ observerCount #>= 0
      , observerCount #<= pmaxTxSizeDerivedItemCount
      , pdataSequenceSummaryLength # observerItems #== observerCount
      , plengthBS # (pdataSummaryRoot # pfromData (pcekContext'observerSummary c)) #== 32
      , pfromData (pcekContext'observerSummary c)
          #== pfinalizeCekObserverItemsV1
            # observerItems
            # (pfromData (pcekContext'languageTag c) #== 128)
      , pif
          (observerCount #== 0)
          ( observerItems #== pemptyObserverItemsV1 # pfromData (pcekContext'languageTag c)
              #&& pfromData (pcekContext'previousObserver c) #== pconstant ""
          )
          (plengthBS # pfromData (pcekContext'previousObserver c) #== 28)
      ]

pmintContextIsFinal :: forall s.
  Term s (PNativeScriptsControlV1 :--> PCekContextControlV1 :--> PBool)
pmintContextIsFinal = phoistAcyclic $ plam $ \nativeControl control ->
  pmatch nativeControl $ \native ->
  pmatch control $ \c ->
    pand'List
      [ pobserverContextIsComplete # control
      , pfromData (pcekContext'mintCursor c) #== pfromData (pnativeControl'mintCount native)
      , pfromData (pcekContext'currentMintPolicy c) #== pconstant ""
      , pfromData (pcekContext'currentMintAssets c) #== CekData.pemptyDataPairSummaryV1
      , plengthBS # (pdataSummaryRoot # pfromData (pcekContext'mintSummary c)) #== 32
      ]

-- | Aiken @cek_context_control_is_well_formed@.
pcekContextControlIsWellFormed :: forall s.
  Term s (PNativeScriptsControlV1 :--> PCekContextControlV1 :--> PBool)
pcekContextControlIsWellFormed = phoistAcyclic $ plam $ \nativeControl control ->
  pmatch nativeControl $ \native ->
  pmatch control $ \c ->
  plet (pfromData (pnativeControl'resolvedInputCount native) - pfromData (pnativeControl'spendInputCount native)) $ \referenceCount ->
  plet (pfromData $ pcekContext'stage c) $ \stage ->
  plet (pfromData $ pcekContext'languageTag c) $ \languageTag ->
  plet (pfromData $ pcekContext'referenceItems c) $ \referenceItems ->
  plet (pfromData $ pcekContext'spendItems c) $ \spendItems ->
  plet (pfromData $ pcekContext'outputItems c) $ \outputItems ->
  plet (pfromData $ pcekContext'signerItems c) $ \signerItems ->
  plet (pfromData $ pcekContext'mintCursor c) $ \mintCursor ->
  plet (pfromData $ pcekContext'currentMintAssets c) $ \currentMintAssets ->
  plet (pfromData $ pcekContext'redeemerContextControlHash c) $ \redeemerControlHash ->
  plet (plengthBS # redeemerControlHash #== 32) $ \redeemerScanIsBound ->
    pand'List
      [ stage #>= 0
      , stage #<= 13
      , languageTag #== 3 #|| languageTag #== 128
      , plengthBS # pfromData (pcekContext'programTermRoot c) #== 32
      , plengthBS # pfromData (pcekContext'programEnvelopeHash c) #== 32
      , pfromData (pcekContext'purposeKind c) #>= 0
      , pfromData (pcekContext'purposeKind c) #<= 3
      , pfromData (pcekContext'purposeIndex c) #>= 0
      , plengthBS # pfromData (pcekContext'scriptHash c) #== 28
      , plengthBS # pfromData (pcekContext'redeemerLeaf c) #== 32
      , redeemerControlHash #== pconstant "" #|| redeemerScanIsBound
      , pfromData (pcekContext'executionMemoryLimit c) #>= 0
      , pfromData (pcekContext'executionCpuLimit c) #>= 0
      , pdataSequenceSummaryIsWellFormed # referenceItems
      , pdataSequenceSummaryIsWellFormed # spendItems
      , pdataSequenceSummaryIsWellFormed # outputItems
      , pdataSequenceSummaryIsWellFormed # signerItems
      , pdataSequenceSummaryIsWellFormed # pfromData (pcekContext'observerItems c)
      , poptionalDataSummaryIsWellFormed # pfromData (pcekContext'observerSummary c)
      , mintCursor #>= 0
      , mintCursor #<= pfromData (pnativeControl'mintCount native)
      , pfromData (pcekContext'currentMintPolicy c) #== pconstant ""
          #|| plengthBS # pfromData (pcekContext'currentMintPolicy c) #== 28
      , pdataSequenceSummaryIsWellFormed # currentMintAssets
      , pdataSequenceSummaryIsWellFormed # pfromData (pcekContext'mintPolicies c)
      , poptionalDataSummaryIsWellFormed # pfromData (pcekContext'mintSummary c)
      , pdataSequenceSummaryLength # referenceItems #<= referenceCount
      , pdataSequenceSummaryLength # spendItems #<= pfromData (pnativeControl'spendInputCount native)
      , pdataSequenceSummaryLength # outputItems #<= pfromData (pnativeControl'outputCount native)
      , pif (stage #== 0)
          ( pand'List
              [ pfromData (pcekContext'executionMemoryLimit c) #== 0
              , pfromData (pcekContext'executionCpuLimit c) #== 0
              , pdataSequenceSummaryLength # referenceItems #== 0
              , pdataSequenceSummaryLength # spendItems #== 0
              , pdataSequenceSummaryLength # outputItems #== 0
              , pdataSequenceSummaryLength # signerItems #== 0
              , pobserverContextIsPristine # control
              , pmintContextIsPristine # control
              ]
          )
          $ pif (stage #== 1)
            ( pand'List
                [ redeemerScanIsBound
                , pdataSequenceSummaryLength # spendItems #== 0
                , pdataSequenceSummaryLength # outputItems #== 0
                , pdataSequenceSummaryLength # signerItems #== 0
                , pobserverContextIsPristine # control
                , pmintContextIsPristine # control
                ]
            )
          $ pif (stage #== 2)
            ( pand'List
                [ redeemerScanIsBound
                , pdataSequenceSummaryLength # referenceItems #== referenceCount
                , pdataSequenceSummaryLength # outputItems #== 0
                , pdataSequenceSummaryLength # signerItems #== 0
                , pobserverContextIsPristine # control
                , pmintContextIsPristine # control
                ]
            )
          $ pif (stage #== 3)
            ( pand'List
                [ redeemerScanIsBound
                , pdataSequenceSummaryLength # referenceItems #== referenceCount
                , pdataSequenceSummaryLength # spendItems #== pfromData (pnativeControl'spendInputCount native)
                , pdataSequenceSummaryLength # signerItems #== 0
                , pobserverContextIsPristine # control
                , pmintContextIsPristine # control
                ]
            )
          $ pif (stage #== 4)
            ( pand'List
                [ redeemerScanIsBound
                , pdataSequenceSummaryLength # referenceItems #== referenceCount
                , pdataSequenceSummaryLength # spendItems #== pfromData (pnativeControl'spendInputCount native)
                , pdataSequenceSummaryLength # outputItems #== pfromData (pnativeControl'outputCount native)
                , pdataSequenceSummaryLength # signerItems #<= pfromData (pnativeControl'signerCount native)
                , pobserverContextIsPristine # control
                , pmintContextIsPristine # control
                ]
            )
          $ pif (stage #== 5)
            ( pand'List
                [ redeemerScanIsBound
                , pdataSequenceSummaryLength # signerItems #== pfromData (pnativeControl'signerCount native)
                , pobserverContextIsPartial # control
                , pmintContextIsPristine # control
                ]
            )
          $ pif (stage #== 6)
            ( pand'List
                [ redeemerScanIsBound
                , pdataSequenceSummaryLength # signerItems #== pfromData (pnativeControl'signerCount native)
                , pobserverContextIsComplete # control
                , pmintContextIsPristine # control
                ]
            )
          $ pif (stage #== 7)
            (pconstant False)
          $ pif (stage #== 8)
            ( pand'List
                [ redeemerScanIsBound
                , pdataSequenceSummaryLength # signerItems #== pfromData (pnativeControl'signerCount native)
                , pobserverContextIsComplete # control
                , pfromData (pnativeControl'mintCount native) #> 0
                , pif (mintCursor #== 0)
                    ( pand'List
                        [ pfromData (pcekContext'currentMintPolicy c) #== pconstant ""
                        , currentMintAssets #== CekData.pemptyDataPairSummaryV1
                        , pfromData (pcekContext'mintPolicies c) #== CekData.pemptyDataPairSummaryV1
                        ]
                    )
                    ( plengthBS # pfromData (pcekContext'currentMintPolicy c) #== 28
                        #&& pdataSequenceSummaryLength # currentMintAssets #> 0
                    )
                , pdataSummaryRoot # pfromData (pcekContext'mintSummary c) #== pconstant ""
                ]
            )
          $ pif (stage #== 9)
            ( pand'List
                [ pdataSequenceSummaryLength # signerItems #== pfromData (pnativeControl'signerCount native)
                , pmintContextIsFinal # nativeControl # control
                , redeemerScanIsBound
                ]
            )
            ( pand'List
                [ pdataSequenceSummaryLength # signerItems #== pfromData (pnativeControl'signerCount native)
                , pmintContextIsFinal # nativeControl # control
                , redeemerScanIsBound
                ]
            )
      ]

-- | Aiken @cek_redeemer_context_control_is_well_formed@.
pcekRedeemerContextControlIsWellFormed :: forall s.
  Term s (PInteger :--> PCekRedeemerContextControlV1 :--> PBool)
pcekRedeemerContextControlIsWellFormed = phoistAcyclic $ plam $ \redeemerCount control ->
  pmatch control $ \c ->
  plet (pfromData $ pcekRedeemer'cursor c) $ \cursor ->
  plet (pfromData $ pcekRedeemer'mapItems c) $ \mapItems ->
  plet (pfromData $ pcekRedeemer'activeScanHash c) $ \activeScanHash ->
  plet (pfromData $ pcekRedeemer'activePurpose c) $ \activePurpose ->
    pand'List
      [ cursor #>= 0
      , cursor #<= redeemerCount
      , pdataSequenceSummaryIsWellFormed # mapItems
      , pdataSequenceSummaryLength # mapItems #<= cursor
      , poptionalDataSummaryIsWellFormed # activePurpose
      , poptionalDataSummaryIsWellFormed # pfromData (pcekRedeemer'currentRedeemer c)
      , pif
          (activeScanHash #== pconstant "")
          ( pfromData (pcekRedeemer'activeRedeemerLeaf c) #== pconstant ""
              #&& activePurpose #== pemptyDataSummaryV1
          )
          ( pand'List
              [ cursor #< redeemerCount
              , plengthBS # activeScanHash #== 32
              , plengthBS # pfromData (pcekRedeemer'activeRedeemerLeaf c) #== 32
              , activePurpose #== pemptyDataSummaryV1
                  #|| plengthBS # (pdataSummaryRoot # activePurpose) #== 32
              ]
          )
      ]

-- | Aiken @completed_redeemer_context_matches@.
pcompletedCekRedeemerContextMatches :: forall s.
  Term s
    ( PNativeScriptsControlV1
        :--> PCekContextControlV1
        :--> PCekRedeemerContextControlV1
        :--> PBool
    )
pcompletedCekRedeemerContextMatches = phoistAcyclic $ plam $ \nativeControl contextControl redeemers ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
  pmatch redeemers $ \redeemer ->
    pand'List
      [ pcekRedeemerContextControlIsWellFormed
          # pfromData (pnativeControl'redeemerCount native)
          # redeemers
      , phashCekRedeemerContextControlV1 # redeemers
          #== pfromData (pcekContext'redeemerContextControlHash context)
      , pfromData (pcekRedeemer'cursor redeemer) #== pfromData (pnativeControl'redeemerCount native)
      , pfromData (pcekRedeemer'activeScanHash redeemer) #== pconstant ""
      , plengthBS # (pdataSummaryRoot # pfromData (pcekRedeemer'currentRedeemer redeemer)) #== 32
      ]

-- | Aiken @cek_context_successor_is_exact@.
pcekContextSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1
        :--> PCekContextControlV1
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
    )
pcekContextSuccessorIsExact = phoistAcyclic $ plam $ \pre witness nativeControl contextControl executionCursor completedCpu completedMemory ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch (pfromData $ poneStep'claimedSuccessor stepWitness) $ \postState ->
  pmatch contextControl $ \context ->
    pand'List
      [ pfromData (pmachineState'phase postState) #== pcon PCek
      , pfromData (pmachineState'executionCpu postState) #== pfromData (pmachineState'executionCpu preState)
      , pfromData (pmachineState'executionMemory postState) #== pfromData (pmachineState'executionMemory preState)
      , pfromData (pmachineState'workRoot postState)
          #== phashWorkWitness
            # pcon PCek
            # (pfromData (pmachineState'programCounter preState) + 1)
            # ( pencodeCekWitnessV1
                  # nativeControl
                  # (pencodeCekContextControlV1 # contextControl)
                  # executionCursor
                  # completedCpu
                  # completedMemory
                  # pconstant ""
                  # 0
                  # 0
                  # pfromData (pcekContext'programEnvelopeHash context)
              )
      ]

pmaxAggregateFieldPreimageBytes :: forall s. Term s PInteger
pmaxAggregateFieldPreimageBytes = 32_768

prejectPlutusScriptInvalid :: forall s. Term s PByteString
prejectPlutusScriptInvalid = pconstant "E_PLUTUS_SCRIPT_INVALID"

prejectedSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1
        :--> PValidationMachineStateV1
        :--> PByteString
        :--> PBool
    )
prejectedSuccessorIsExact = phoistAcyclic $ plam $ \pre post rejectionCode ->
  pmatch pre $ \preState ->
  pmatch post $ \postState ->
    pand'List
      [ pfromData (pmachineState'phase postState) #== pcon PTerminal
      , pfromData (pmachineState'verdict postState) #== pcon PRejected
      , pfromData (pmachineState'rejectionCodeHash postState)
          #== phashRejectionCode # rejectionCode
      , pfromData (pmachineState'workRoot postState)
          #== phashWorkWitness
            # pcon PTerminal
            # (pfromData (pmachineState'programCounter preState) + 1)
            # ( pencodeTerminalRejectionWitness
                  # rejectionCode
                  # pfromData (pmachineState'priorLedgerRoot preState)
              )
      ]

pinitialCekContextControl :: forall s.
  Term s
    ( PInteger :--> PInteger :--> PInteger
        :--> PByteString :--> PByteString :--> PByteString
        :--> PByteString :--> PByteString
        :--> PCekContextControlV1
    )
pinitialCekContextControl = phoistAcyclic $ plam $ \languageTag purposeKind purposeIndex scriptHash subject redeemerLeaf programTermRoot programEnvelopeHash ->
  pcon $ PCekContextControlV1
    (pdata 0)
    (pdata languageTag)
    (pdata programTermRoot)
    (pdata programEnvelopeHash)
    (pdata purposeKind)
    (pdata purposeIndex)
    (pdata scriptHash)
    (pdata subject)
    (pdata redeemerLeaf)
    (pdata $ pconstant "")
    (pdata 0)
    (pdata 0)
    (pdata CekData.pemptyDataListSummaryV1)
    (pdata CekData.pemptyDataListSummaryV1)
    (pdata CekData.pemptyDataListSummaryV1)
    (pdata CekData.pemptyDataListSummaryV1)
    (pdata 0)
    (pdata $ pemptyObserverItemsV1 # languageTag)
    (pdata $ pconstant "")
    (pdata pemptyDataSummaryV1)
    (pdata 0)
    (pdata $ pconstant "")
    (pdata CekData.pemptyDataPairSummaryV1)
    (pdata CekData.pemptyDataPairSummaryV1)
    (pdata pemptyDataSummaryV1)

pcekSelectionSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1
        :--> PInteger :--> PInteger :--> PInteger
        :--> PInteger :--> PInteger :--> PInteger
        :--> PByteString :--> PByteString :--> PByteString
        :--> PByteString :--> PByteString :--> PBool
    )
pcekSelectionSuccessorIsExact = phoistAcyclic $ plam $ \pre witness nativeControl executionCursor completedCpu completedMemory languageTag purposeKind purposeIndex scriptHash subject redeemerLeaf programTermRoot programEnvelopeHash ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch (pfromData $ poneStep'claimedSuccessor stepWitness) $ \postState ->
  pmatch nativeControl $ \native ->
    pif
      (languageTag #== 3 #&& purposeKind #== 3)
      (prejectedSuccessorIsExact # pre # pfromData (poneStep'claimedSuccessor stepWitness) # prejectPlutusScriptInvalid)
      ( pif
          (languageTag #== 0)
          ( plet (executionCursor + 1) $ \nextCursor ->
              pif
                (nextCursor #== pfromData (pnativeControl'executionCount native))
                ( pand'List
                    [ pfromData (pmachineState'phase postState) #== pcon PValueAndMint
                    , pfromData (pmachineState'executionCpu postState) #== pfromData (pmachineState'executionCpu preState)
                    , pfromData (pmachineState'executionMemory postState) #== pfromData (pmachineState'executionMemory preState)
                    , pfromData (pmachineState'workRoot postState)
                        #== phashWorkWitness
                          # pcon PValueAndMint
                          # (pfromData (pmachineState'programCounter preState) + 1)
                          # (pencodeValueAndMintWitnessV1 # nativeControl)
                    ]
                )
                ( pand'List
                    [ pfromData (pmachineState'phase postState) #== pcon PCek
                    , pfromData (pmachineState'executionCpu postState) #== pfromData (pmachineState'executionCpu preState)
                    , pfromData (pmachineState'executionMemory postState) #== pfromData (pmachineState'executionMemory preState)
                    , pfromData (pmachineState'workRoot postState)
                        #== phashWorkWitness
                          # pcon PCek
                          # (pfromData (pmachineState'programCounter preState) + 1)
                          # ( pencodeCekWitnessV1
                                # nativeControl # pconstant "" # nextCursor
                                # completedCpu # completedMemory
                                # pconstant "" # 0 # 0 # pconstant ""
                            )
                    ]
                )
          )
          ( pcekContextSuccessorIsExact
              # pre # witness # nativeControl
              # ( pinitialCekContextControl
                    # languageTag # purposeKind # purposeIndex
                    # scriptHash # subject # redeemerLeaf
                    # programTermRoot # programEnvelopeHash
                )
              # executionCursor # completedCpu # completedMemory
          )
      )

pfirstSourceChunkIdentityMatches :: forall s.
  Term s
    ( PInteger :--> PByteString
        :--> BoundedItem.PChunkProofV1 :--> PBool
    )
pfirstSourceChunkIdentityMatches = phoistAcyclic $ plam $ \originKind sourceKey proof ->
  pmatch proof $ \p ->
    pif
      (originKind #== 0)
      ( pmatch (pdeserialise # sourceKey) $ \case
          PNothing -> perror
          PJust sourceIndexData ->
            plet (pasInt # sourceIndexData) $ \sourceIndex ->
              pand'List
                [ sourceIndex #>= 0
                , pcborInt sourceIndex #== sourceKey
                , pfromData (BoundedItem.pchunkProof'fieldIndex p) #== 6
                , pfromData (BoundedItem.pchunkProof'itemIndex p) #== sourceIndex
                ]
      )
      ( pif
          (originKind #== 1)
          ( pmatch (pdeserialise # sourceKey) $ \case
              PNothing -> perror
              PJust sourceKeyData ->
                plet (pasList # sourceKeyData) $ \fields ->
                  pif
                    (plength # fields #== 2)
                    ( plet (pasInt # (pelemAt # 1 # fields)) $ \outputIndex ->
                        pand'List
                          [ outputIndex #>= 0
                          , outputIndex #<= 65_535
                          , pfromData (BoundedItem.pchunkProof'fieldIndex p) #== 2
                          , pfromData (BoundedItem.pchunkProof'itemIndex p) #== outputIndex
                          ]
                    )
                    perror
          )
          (pconstant False)
      )

-- | Aiken @verify_cek_execution_selection@ after unpacking its auxiliary witness.
pverifyCekExecutionSelection :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1
        :--> PInteger :--> PInteger :--> PInteger
        :--> PInteger :--> PInteger :--> PInteger :--> PInteger
        :--> PByteString :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PInteger :--> PInteger :--> PByteString :--> PInteger
        :--> PByteString :--> PBuiltinList (PAsData PByteString)
        :--> PByteString :--> PBuiltinList (PAsData PByteString)
        :--> BoundedItem.PChunkProofV1 :--> PBool
    )
pverifyCekExecutionSelection = phoistAcyclic $ plam $ \pre witness nativeControl executionCursor completedCpu completedMemory executionIndex languageTag purposeKind purposeIndex scriptHash subject purposeSiblings sourceIndex originKind sourceKey scriptTotalLength scriptItemCommitment sourceSiblings redeemerLeaf executionSiblings firstChunkProof ->
  pmatch nativeControl $ \native ->
  pmatch firstChunkProof $ \chunkProof ->
  plet
    (ScriptProof.ppurposeLeafHash # purposeKind # purposeIndex # scriptHash # subject)
    $ \purposeLeaf ->
  plet
    ( ScriptProof.psourceDescriptorLeafHash
        # originKind # sourceKey # languageTag # scriptHash
        # scriptTotalLength # scriptItemCommitment
    )
    $ \sourceLeaf ->
  plet
    (ScriptProof.pexecutionLeafHash # languageTag # purposeLeaf # sourceLeaf # redeemerLeaf)
    $ \executionLeaf ->
  pmatch
    ( NativeScriptScan.pversionedScriptHeaderV1
        # pfromData (BoundedItem.pchunkProof'chunk chunkProof)
        # scriptTotalLength
    )
    $ \case
      PNothing -> perror
      PJust header -> pmatch header $ \headerFields ->
        pif
          ( pand'List
              [ executionIndex #== executionCursor
              , executionCursor #< pfromData (pnativeControl'executionCount native)
              , pfromData (pnativeControl'executionCount native) #== pfromData (pnativeControl'purposeCount native)
              , scriptTotalLength #> 0
              , scriptTotalLength #<= pmaxAggregateFieldPreimageBytes
              , plengthBS # scriptItemCommitment #== 32
              , pfromData (BoundedItem.pchunkProof'chunkIndex chunkProof) #== 0
              , pfromData (BoundedItem.pchunkProof'totalLength chunkProof) #== scriptTotalLength
              , pfirstSourceChunkIdentityMatches # originKind # sourceKey # firstChunkProof
              , BoundedItem.pverifyChunk # scriptItemCommitment # firstChunkProof
              , pfromData (NativeScriptScan.pheader'languageTag headerFields) #== languageTag
              , pverifyMembership
                  # pfromData (pnativeControl'purposeCount native)
                  # pfromData (pnativeControl'purposePeaks native)
                  # executionIndex # purposeLeaf # purposeSiblings
              , pverifyMembership
                  # pfromData (pnativeControl'sourceCount native)
                  # pfromData (pnativeControl'sourcePeaks native)
                  # sourceIndex # sourceLeaf # sourceSiblings
              , pverifyMembership
                  # pfromData (pnativeControl'executionCount native)
                  # pfromData (pnativeControl'executionPeaks native)
                  # executionIndex # executionLeaf # executionSiblings
              ]
          )
          ( pif
              (languageTag #== 0)
              ( redeemerLeaf #== pconstant ""
                  #&& pcekSelectionSuccessorIsExact
                    # pre # witness # nativeControl
                    # executionCursor # completedCpu # completedMemory
                    # languageTag # purposeKind # purposeIndex
                    # scriptHash # subject # redeemerLeaf
                    # pconstant "" # pconstant ""
              )
              ( pif
                  ( pand'List
                      [ languageTag #== 3 #|| languageTag #== 128
                      , plengthBS # redeemerLeaf #== 32
                      , pfromData (NativeScriptScan.pheader'payloadLength headerFields)
                          #<= CekProof.pmaxProgramEnvelopeCborBytes
                      , pfromData (NativeScriptScan.pheader'payloadOffset headerFields)
                          + pfromData (NativeScriptScan.pheader'payloadLength headerFields)
                          #<= plengthBS # pfromData (BoundedItem.pchunkProof'chunk chunkProof)
                      ]
                  )
                  ( plet
                      ( psliceBS
                          # pfromData (NativeScriptScan.pheader'payloadOffset headerFields)
                          # pfromData (NativeScriptScan.pheader'payloadLength headerFields)
                          # pfromData (BoundedItem.pchunkProof'chunk chunkProof)
                      )
                      $ \envelopeCbor ->
                        pmatch (CekProof.pinspectProgramEnvelopeV1 # envelopeCbor) $ \case
                          PNothing -> pconstant False
                          PJust envelope -> pmatch envelope $ \envelopeFields ->
                            pcekSelectionSuccessorIsExact
                              # pre # witness # nativeControl
                              # executionCursor # completedCpu # completedMemory
                              # languageTag # purposeKind # purposeIndex
                              # scriptHash # subject # redeemerLeaf
                              # pfromData (CekProof.penvelope'termRoot envelopeFields)
                              # ( CekProof.phashProgramEnvelopeV1
                                    # 1 # 1 # 0
                                    # pfromData (CekProof.penvelope'termRoot envelopeFields)
                                    # pfromData (CekProof.penvelope'nodeCount envelopeFields)
                                    # pfromData (CekProof.penvelope'materialByteLength envelopeFields)
                                )
                  )
                  (pconstant False)
              )
          )
          (pconstant False)

-- | Route an authenticated CEK core transition through the validation machine.
pverifyCekCoreAdvanced :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1
        :--> PInteger :--> PInteger :--> PInteger
        :--> PByteString :--> PInteger :--> PInteger :--> PByteString
        :--> CekMachine.PMachineStateV1 :--> CekMachine.PMachineStateV1
        :--> PBool :--> PBool
    )
pverifyCekCoreAdvanced = phoistAcyclic $ plam $ \pre witness nativeControl executionCursor completedCpu completedMemory activeStateHash executionCpuLimit executionMemoryLimit programEnvelopeHash stepPre stepPost coreStepIsValid ->
  pmatch pre $ \preState ->
  pmatch witness $ \validationWitness ->
  pmatch (pfromData $ poneStep'claimedSuccessor validationWitness) $ \postState ->
  pmatch nativeControl $ \native ->
  pmatch stepPre $ \corePre ->
  pmatch stepPost $ \corePost ->
  plet (completedCpu + pfromData (CekMachine.pstate'cpu corePost)) $ \nextCpu ->
  plet (completedMemory + pfromData (CekMachine.pstate'memory corePost)) $ \nextMemory ->
  plet
    ( pfromData (CekMachine.pstate'cpu corePost) #> executionCpuLimit
        #|| pfromData (CekMachine.pstate'memory corePost) #> executionMemoryLimit
    )
    $ \budgetExceeded ->
  pif
    ( pand'List
        [ plengthBS # activeStateHash #== 32
        , plengthBS # programEnvelopeHash #== 32
        , executionCursor #>= 0
        , executionCursor #< pfromData (pnativeControl'executionCount native)
        , executionCpuLimit #> 0
        , executionMemoryLimit #> 0
        , pfromData (CekMachine.pstate'executionIndex corePre) #== executionCursor
        , CekMachine.phashStateV1 # stepPre #== activeStateHash
        , pfromData (pmachineState'executionCpu preState)
            #== completedCpu + pfromData (CekMachine.pstate'cpu corePre)
        , pfromData (pmachineState'executionMemory preState)
            #== completedMemory + pfromData (CekMachine.pstate'memory corePre)
        , coreStepIsValid
        ]
    )
    ( pif
        (budgetExceeded #|| pfromData (CekMachine.pstate'mode corePost) #== CekMachine.pmodeHaltError)
        ( pand'List
            [ pfromData (pmachineState'executionCpu postState) #== nextCpu
            , pfromData (pmachineState'executionMemory postState) #== nextMemory
            , prejectedSuccessorIsExact
                # pre # pfromData (poneStep'claimedSuccessor validationWitness)
                # prejectPlutusScriptInvalid
            ]
        )
        ( pif
            (pfromData (CekMachine.pstate'mode corePost) #== CekMachine.pmodeHaltSuccess)
            ( plet (executionCursor + 1) $ \nextCursor ->
                pand'List
                  [ pfromData (CekMachine.pstate'cpu corePost) #<= executionCpuLimit
                  , pfromData (CekMachine.pstate'memory corePost) #<= executionMemoryLimit
                  , pfromData (pmachineState'executionCpu postState) #== nextCpu
                  , pfromData (pmachineState'executionMemory postState) #== nextMemory
                  , pif
                      (nextCursor #== pfromData (pnativeControl'executionCount native))
                      ( pand'List
                          [ pfromData (pmachineState'phase postState) #== pcon PValueAndMint
                          , pfromData (pmachineState'workRoot postState)
                              #== phashWorkWitness
                                # pcon PValueAndMint
                                # (pfromData (pmachineState'programCounter preState) + 1)
                                # (pencodeValueAndMintWitnessV1 # nativeControl)
                          ]
                      )
                      ( pand'List
                          [ pfromData (pmachineState'phase postState) #== pcon PCek
                          , pfromData (pmachineState'workRoot postState)
                              #== phashWorkWitness
                                # pcon PCek
                                # (pfromData (pmachineState'programCounter preState) + 1)
                                # ( pencodeCekWitnessV1
                                      # nativeControl # pconstant "" # nextCursor
                                      # nextCpu # nextMemory
                                      # pconstant "" # 0 # 0 # pconstant ""
                                  )
                          ]
                      )
                  ]
            )
            ( pand'List
                [ pfromData (pmachineState'phase postState) #== pcon PCek
                , pfromData (pmachineState'executionCpu postState) #== nextCpu
                , pfromData (pmachineState'executionMemory postState) #== nextMemory
                , pfromData (pmachineState'workRoot postState)
                    #== phashWorkWitness
                      # pcon PCek
                      # (pfromData (pmachineState'programCounter preState) + 1)
                      # ( pencodeCekWitnessV1
                            # nativeControl # pconstant "" # executionCursor
                            # completedCpu # completedMemory
                            # (CekMachine.phashStateV1 # stepPost)
                            # executionCpuLimit # executionMemoryLimit
                            # programEnvelopeHash
                        )
                ]
            )
        )
    )
    (pconstant False)

-- | Aiken @verify_cek_core_step@ after unpacking its auxiliary witness.
pverifyCekCoreStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1
        :--> PInteger :--> PInteger :--> PInteger
        :--> PByteString :--> PInteger :--> PInteger :--> PByteString
        :--> CekMachine.PMachineStateV1 :--> CekMachine.PMachineStateV1
        :--> CekMachine.PCoreStepWitnessV1 :--> PBool
    )
pverifyCekCoreStep = phoistAcyclic $ plam $ \pre witness nativeControl executionCursor completedCpu completedMemory activeStateHash executionCpuLimit executionMemoryLimit programEnvelopeHash stepPre stepPost stepWitness ->
  pverifyCekCoreAdvanced
    # pre # witness # nativeControl
    # executionCursor # completedCpu # completedMemory
    # activeStateHash # executionCpuLimit # executionMemoryLimit
    # programEnvelopeHash # stepPre # stepPost
    # (CekMachine.pverifyCoreStepV1 # stepPre # stepPost # stepWitness)

pcekContextWithStageAndHash :: forall s.
  Term s (PCekContextControlV1 :--> PInteger :--> PByteString :--> PCekContextControlV1)
pcekContextWithStageAndHash = phoistAcyclic $ plam $ \control stage controlHash ->
  pmatch control $ \c ->
    pcon $ PCekContextControlV1
      (pdata stage)
      (pcekContext'languageTag c)
      (pcekContext'programTermRoot c)
      (pcekContext'programEnvelopeHash c)
      (pcekContext'purposeKind c)
      (pcekContext'purposeIndex c)
      (pcekContext'scriptHash c)
      (pcekContext'subject c)
      (pcekContext'redeemerLeaf c)
      (pdata controlHash)
      (pcekContext'executionMemoryLimit c)
      (pcekContext'executionCpuLimit c)
      (pcekContext'referenceItems c)
      (pcekContext'spendItems c)
      (pcekContext'outputItems c)
      (pcekContext'signerItems c)
      (pcekContext'observerCount c)
      (pcekContext'observerItems c)
      (pcekContext'previousObserver c)
      (pcekContext'observerSummary c)
      (pcekContext'mintCursor c)
      (pcekContext'currentMintPolicy c)
      (pcekContext'currentMintAssets c)
      (pcekContext'mintPolicies c)
      (pcekContext'mintSummary c)

pcekContextWithStageHashAndLimits :: forall s.
  Term s
    ( PCekContextControlV1 :--> PInteger :--> PByteString
        :--> PInteger :--> PInteger :--> PCekContextControlV1
    )
pcekContextWithStageHashAndLimits = phoistAcyclic $ plam $ \control stage controlHash memoryLimit cpuLimit ->
  pmatch control $ \c ->
    pcon $ PCekContextControlV1
      (pdata stage)
      (pcekContext'languageTag c)
      (pcekContext'programTermRoot c)
      (pcekContext'programEnvelopeHash c)
      (pcekContext'purposeKind c)
      (pcekContext'purposeIndex c)
      (pcekContext'scriptHash c)
      (pcekContext'subject c)
      (pcekContext'redeemerLeaf c)
      (pdata controlHash)
      (pdata memoryLimit)
      (pdata cpuLimit)
      (pcekContext'referenceItems c)
      (pcekContext'spendItems c)
      (pcekContext'outputItems c)
      (pcekContext'signerItems c)
      (pcekContext'observerCount c)
      (pcekContext'observerItems c)
      (pcekContext'previousObserver c)
      (pcekContext'observerSummary c)
      (pcekContext'mintCursor c)
      (pcekContext'currentMintPolicy c)
      (pcekContext'currentMintAssets c)
      (pcekContext'mintPolicies c)
      (pcekContext'mintSummary c)

pverifyCekInitialRedeemerBegin :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PInteger :--> PInteger :--> PInteger :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekInitialRedeemerBegin = phoistAcyclic $ plam $ \pre witness nativeControl contextControl itemIndex itemCount totalLength itemCommitment siblings executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
  pmatch (predeemerTagForPurposeKindV1 # pfromData (pcekContext'purposeKind context)) $ \case
    PNothing -> perror
    PJust purposeTag ->
      plet (ScriptProof.predeemerItemLeafHash # itemIndex # itemCommitment) $ \leaf ->
      plet
        ( RedeemerItemProof.pinitialControlV1
            # RedeemerItemProof.pmodeDescriptor
            # itemIndex # itemCount # totalLength # itemCommitment
            # purposeTag # pfromData (pcekContext'purposeIndex context)
        )
        $ \itemControl ->
          pand'List
            [ pfromData (pcekContext'stage context) #== 0
            , itemCount #== pfromData (pnativeControl'redeemerCount native)
            , leaf #== pfromData (pcekContext'redeemerLeaf context)
            , pverifyMembership
                # pfromData (pnativeControl'redeemerCount native)
                # pfromData (pnativeControl'redeemerPeaks native)
                # itemIndex # leaf # siblings
            , pcekContextSuccessorIsExact
                # pre # witness # nativeControl
                # ( pcekContextWithStageAndHash
                      # contextControl # 0
                      # (RedeemerItemProof.phashControlV1 # itemControl)
                  )
                # executionCursor # completedCpu # completedMemory
            ]

pverifyCekInitialRedeemerStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> RedeemerItemProof.PRedeemerItemProofControlV1
        :--> RedeemerItemProof.PRedeemerItemProofWitnessV1
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekInitialRedeemerStep = phoistAcyclic $ plam $ \pre witness nativeControl contextControl itemControl itemWitness executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
  pmatch itemControl $ \item ->
  pmatch (predeemerTagForPurposeKindV1 # pfromData (pcekContext'purposeKind context)) $ \case
    PNothing -> perror
    PJust purposeTag ->
      plet
        ( pand'List
            [ pfromData (pcekContext'stage context) #== 0
            , pfromData (RedeemerItemProof.predeemerControl'mode item) #== RedeemerItemProof.pmodeDescriptor
            , pfromData (RedeemerItemProof.predeemerControl'itemCount item)
                #== pfromData (pnativeControl'redeemerCount native)
            , pfromData (RedeemerItemProof.predeemerControl'expectedPurposeTag item) #== purposeTag
            , pfromData (RedeemerItemProof.predeemerControl'expectedPointerIndex item)
                #== pfromData (pcekContext'purposeIndex context)
            , ScriptProof.predeemerItemLeafHash
                # pfromData (RedeemerItemProof.predeemerControl'itemIndex item)
                # pfromData (RedeemerItemProof.predeemerControl'itemCommitment item)
                #== pfromData (pcekContext'redeemerLeaf context)
            , RedeemerItemProof.phashControlV1 # itemControl
                #== pfromData (pcekContext'redeemerContextControlHash context)
            ]
        )
        $ \currentMatches ->
      pmatch (RedeemerItemProof.pstepV1 # itemControl # itemWitness) $ \case
        PNothing -> pconstant False
        PJust result -> pmatch result $ \case
          RedeemerItemProof.PRedeemerItemProofInvalid -> pconstant False
          RedeemerItemProof.PRedeemerItemProofAdvanced nextData ->
            plet (pfromData nextData) $ \next ->
            pmatch next $ \nextFields ->
              currentMatches
                #&& pif
                  (pfromData (RedeemerItemProof.predeemerControl'stage nextFields) #== RedeemerItemProof.pstageTerminal)
                  ( plet pinitialCekRedeemerContextControlV1 $ \redeemerContext ->
                      pcekContextSuccessorIsExact
                        # pre # witness # nativeControl
                        # ( pcekContextWithStageHashAndLimits
                              # contextControl # 1
                              # (phashCekRedeemerContextControlV1 # redeemerContext)
                              # pfromData (RedeemerItemProof.predeemerControl'executionMemory nextFields)
                              # pfromData (RedeemerItemProof.predeemerControl'executionSteps nextFields)
                          )
                        # executionCursor # completedCpu # completedMemory
                  )
                  ( pcekContextSuccessorIsExact
                      # pre # witness # nativeControl
                      # ( pcekContextWithStageAndHash
                            # contextControl # 0
                            # (RedeemerItemProof.phashControlV1 # next)
                        )
                      # executionCursor # completedCpu # completedMemory
                  )

pcekContextWithCollections :: forall s.
  Term s
    ( PCekContextControlV1
        :--> PInteger
        :--> CekData.PDataSequenceSummaryV1
        :--> CekData.PDataSequenceSummaryV1
        :--> CekData.PDataSequenceSummaryV1
        :--> CekData.PDataSequenceSummaryV1
        :--> PCekContextControlV1
    )
pcekContextWithCollections = phoistAcyclic $ plam $ \control stage references spends outputs signers ->
  pmatch control $ \c ->
    pcon $ PCekContextControlV1
      (pdata stage)
      (pcekContext'languageTag c)
      (pcekContext'programTermRoot c)
      (pcekContext'programEnvelopeHash c)
      (pcekContext'purposeKind c)
      (pcekContext'purposeIndex c)
      (pcekContext'scriptHash c)
      (pcekContext'subject c)
      (pcekContext'redeemerLeaf c)
      (pcekContext'redeemerContextControlHash c)
      (pcekContext'executionMemoryLimit c)
      (pcekContext'executionCpuLimit c)
      (pdata references)
      (pdata spends)
      (pdata outputs)
      (pdata signers)
      (pcekContext'observerCount c)
      (pcekContext'observerItems c)
      (pcekContext'previousObserver c)
      (pcekContext'observerSummary c)
      (pcekContext'mintCursor c)
      (pcekContext'currentMintPolicy c)
      (pcekContext'currentMintAssets c)
      (pcekContext'mintPolicies c)
      (pcekContext'mintSummary c)

pverifyCekReferenceContextAdvance :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekReferenceContextAdvance = phoistAcyclic $ plam $ \pre witness nativeControl contextControl executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
    plet
      ( pfromData (pnativeControl'resolvedInputCount native)
          - pfromData (pnativeControl'spendInputCount native)
      )
      $ \referenceCount ->
        pdataSequenceSummaryLength # pfromData (pcekContext'referenceItems context) #== referenceCount
          #&& pcekContextSuccessorIsExact
            # pre # witness # nativeControl
            # ( pcekContextWithCollections # contextControl # 2
                  # pfromData (pcekContext'referenceItems context)
                  # pfromData (pcekContext'spendItems context)
                  # pfromData (pcekContext'outputItems context)
                  # pfromData (pcekContext'signerItems context)
              )
            # executionCursor # completedCpu # completedMemory

pverifyCekReferenceContextItem :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PInteger :--> PInteger :--> PByteString :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekReferenceContextItem = phoistAcyclic $ plam $ \pre witness nativeControl contextControl sourceKind itemIndex key descriptorCbor siblings executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
    pmatch
      ( ScriptContext.pprependResolvedDescriptorTxInInfoV1
          # pfromData (pnativeControl'resolvedInputCount native)
          # pfromData (pnativeControl'resolvedItemPeaks native)
          # pfromData (pnativeControl'spendInputCount native)
          # sourceKind # itemIndex # key # descriptorCbor # siblings
          # (pfromData (pcekContext'languageTag context) #== 128)
          # pfromData (pcekContext'referenceItems context)
      )
      $ \case
        PNothing -> pconstant False
        PJust nextItems ->
          sourceKind #== 1
            #&& pcekContextSuccessorIsExact
              # pre # witness # nativeControl
              # ( pcekContextWithCollections # contextControl
                    # pfromData (pcekContext'stage context) # nextItems
                    # pfromData (pcekContext'spendItems context)
                    # pfromData (pcekContext'outputItems context)
                    # pfromData (pcekContext'signerItems context)
                )
              # executionCursor # completedCpu # completedMemory

-- | Aiken @verify_cek_reference_context_step@.
pverifyCekReferenceContextStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PNativeScriptsControlV1
        :--> PCekContextControlV1 :--> PInteger :--> PInteger :--> PInteger
        :--> PBool
    )
pverifyCekReferenceContextStep = phoistAcyclic $ plam $ \pre witness auxiliary nativeControl contextControl executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
  plet
    ( pfromData (pnativeControl'resolvedInputCount native)
        - pfromData (pnativeControl'spendInputCount native)
    )
    $ \referenceCount ->
      pif
        ( pdataSequenceSummaryLength # pfromData (pcekContext'referenceItems context)
            #== referenceCount
        )
        ( pmatch auxiliary $ \case
            PNoAuxiliaryWitness ->
              pverifyCekReferenceContextAdvance
                # pre # witness # nativeControl # contextControl
                # executionCursor # completedCpu # completedMemory
            _ -> pconstant False
        )
        ( pmatch auxiliary $ \case
            PCekResolvedContextItemWitness sourceKind itemIndex key descriptorCbor siblings ->
              pverifyCekReferenceContextItem
                # pre # witness # nativeControl # contextControl
                # pfromData sourceKind # pfromData itemIndex # pfromData key
                # pfromData descriptorCbor # pfromData siblings
                # executionCursor # completedCpu # completedMemory
            _ -> pconstant False
        )

pverifyCekSpendContextAdvance :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekSpendContextAdvance = phoistAcyclic $ plam $ \pre witness nativeControl contextControl executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
    pdataSequenceSummaryLength # pfromData (pcekContext'spendItems context)
      #== pfromData (pnativeControl'spendInputCount native)
      #&& pcekContextSuccessorIsExact
        # pre # witness # nativeControl
        # ( pcekContextWithCollections # contextControl # 3
              # pfromData (pcekContext'referenceItems context)
              # pfromData (pcekContext'spendItems context)
              # pfromData (pcekContext'outputItems context)
              # pfromData (pcekContext'signerItems context)
          )
        # executionCursor # completedCpu # completedMemory

pverifyCekSpendContextItem :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PInteger :--> PInteger :--> PByteString :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekSpendContextItem = phoistAcyclic $ plam $ \pre witness nativeControl contextControl sourceKind itemIndex key descriptorCbor siblings executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
    pmatch
      ( ScriptContext.pprependResolvedDescriptorTxInInfoV1
          # pfromData (pnativeControl'resolvedInputCount native)
          # pfromData (pnativeControl'resolvedItemPeaks native)
          # pfromData (pnativeControl'spendInputCount native)
          # sourceKind # itemIndex # key # descriptorCbor # siblings
          # (pfromData (pcekContext'languageTag context) #== 128)
          # pfromData (pcekContext'spendItems context)
      )
      $ \case
        PNothing -> pconstant False
        PJust nextItems ->
          sourceKind #== 0
            #&& pcekContextSuccessorIsExact
              # pre # witness # nativeControl
              # ( pcekContextWithCollections # contextControl
                    # pfromData (pcekContext'stage context)
                    # pfromData (pcekContext'referenceItems context) # nextItems
                    # pfromData (pcekContext'outputItems context)
                    # pfromData (pcekContext'signerItems context)
                )
              # executionCursor # completedCpu # completedMemory

-- | Aiken @verify_cek_spend_context_step@.
pverifyCekSpendContextStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PNativeScriptsControlV1
        :--> PCekContextControlV1 :--> PInteger :--> PInteger :--> PInteger
        :--> PBool
    )
pverifyCekSpendContextStep = phoistAcyclic $ plam $ \pre witness auxiliary nativeControl contextControl executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
  pif
    ( pdataSequenceSummaryLength # pfromData (pcekContext'spendItems context)
        #== pfromData (pnativeControl'spendInputCount native)
    )
    ( pmatch auxiliary $ \case
        PNoAuxiliaryWitness ->
          pverifyCekSpendContextAdvance
            # pre # witness # nativeControl # contextControl
            # executionCursor # completedCpu # completedMemory
        _ -> pconstant False
    )
    ( pmatch auxiliary $ \case
        PCekResolvedContextItemWitness sourceKind itemIndex key descriptorCbor siblings ->
          pverifyCekSpendContextItem
            # pre # witness # nativeControl # contextControl
            # pfromData sourceKind # pfromData itemIndex # pfromData key
            # pfromData descriptorCbor # pfromData siblings
            # executionCursor # completedCpu # completedMemory
        _ -> pconstant False
    )

pverifyCekOutputContextAdvance :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekOutputContextAdvance = phoistAcyclic $ plam $ \pre witness nativeControl contextControl executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
    pdataSequenceSummaryLength # pfromData (pcekContext'outputItems context)
      #== pfromData (pnativeControl'outputCount native)
      #&& pcekContextSuccessorIsExact
        # pre # witness # nativeControl
        # ( pcekContextWithCollections # contextControl # 4
              # pfromData (pcekContext'referenceItems context)
              # pfromData (pcekContext'spendItems context)
              # pfromData (pcekContext'outputItems context)
              # pfromData (pcekContext'signerItems context)
          )
        # executionCursor # completedCpu # completedMemory

pverifyCekOutputContextItem :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PInteger :--> PByteString :--> PBuiltinList (PAsData PByteString)
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekOutputContextItem = phoistAcyclic $ plam $ \pre witness nativeControl contextControl outputIndex descriptorCbor siblings executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
    pmatch
      ( ScriptContext.pprependOutputDescriptorV1
          # pfromData (pnativeControl'outputCount native)
          # pfromData (pnativeControl'outputDescriptorPeaks native)
          # outputIndex # descriptorCbor # siblings
          # (pfromData (pcekContext'languageTag context) #== 128)
          # pfromData (pcekContext'outputItems context)
      )
      $ \case
        PNothing -> pconstant False
        PJust nextItems ->
          pcekContextSuccessorIsExact
            # pre # witness # nativeControl
            # ( pcekContextWithCollections # contextControl
                  # pfromData (pcekContext'stage context)
                  # pfromData (pcekContext'referenceItems context)
                  # pfromData (pcekContext'spendItems context) # nextItems
                  # pfromData (pcekContext'signerItems context)
              )
            # executionCursor # completedCpu # completedMemory

-- | Aiken @verify_cek_output_context_step@.
pverifyCekOutputContextStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PNativeScriptsControlV1
        :--> PCekContextControlV1 :--> PInteger :--> PInteger :--> PInteger
        :--> PBool
    )
pverifyCekOutputContextStep = phoistAcyclic $ plam $ \pre witness auxiliary nativeControl contextControl executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
  pif
    ( pdataSequenceSummaryLength # pfromData (pcekContext'outputItems context)
        #== pfromData (pnativeControl'outputCount native)
    )
    ( pmatch auxiliary $ \case
        PNoAuxiliaryWitness ->
          pverifyCekOutputContextAdvance
            # pre # witness # nativeControl # contextControl
            # executionCursor # completedCpu # completedMemory
        _ -> pconstant False
    )
    ( pmatch auxiliary $ \case
        PCekOutputContextItemWitness outputIndex descriptorCbor siblings ->
          pverifyCekOutputContextItem
            # pre # witness # nativeControl # contextControl
            # pfromData outputIndex # pfromData descriptorCbor # pfromData siblings
            # executionCursor # completedCpu # completedMemory
        _ -> pconstant False
    )

pverifyCekSignerContextAdvance :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekSignerContextAdvance = phoistAcyclic $ plam $ \pre witness nativeControl contextControl executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
    pdataSequenceSummaryLength # pfromData (pcekContext'signerItems context)
      #== pfromData (pnativeControl'signerCount native)
      #&& pcekContextSuccessorIsExact
        # pre # witness # nativeControl
        # ( pcekContextWithCollections # contextControl # 5
              # pfromData (pcekContext'referenceItems context)
              # pfromData (pcekContext'spendItems context)
              # pfromData (pcekContext'outputItems context)
              # pfromData (pcekContext'signerItems context)
          )
        # executionCursor # completedCpu # completedMemory

pverifyCekSignerContextItem :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PBuiltinList (PAsData PFrontierPeak) :--> PInteger :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekSignerContextItem = phoistAcyclic $ plam $ \pre witness nativeControl contextControl peaks signerIndex signerHash siblings executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
    pmatch
      ( ScriptContext.pprependSignerV1
          # pfromData (pnativeControl'signerCount native)
          # pfromData (pnativeControl'signerFrontierCommitment native)
          # peaks # signerIndex # signerHash # siblings
          # pfromData (pcekContext'signerItems context)
      )
      $ \case
        PNothing -> pconstant False
        PJust nextItems ->
          pcekContextSuccessorIsExact
            # pre # witness # nativeControl
            # ( pcekContextWithCollections # contextControl
                  # pfromData (pcekContext'stage context)
                  # pfromData (pcekContext'referenceItems context)
                  # pfromData (pcekContext'spendItems context)
                  # pfromData (pcekContext'outputItems context) # nextItems
              )
            # executionCursor # completedCpu # completedMemory

pcekContextWithObservers :: forall s.
  Term s
    ( PCekContextControlV1
        :--> PInteger
        :--> PInteger
        :--> CekData.PDataSequenceSummaryV1
        :--> PByteString
        :--> CekData.PDataSummaryV1
        :--> PCekContextControlV1
    )
pcekContextWithObservers = phoistAcyclic $ plam $ \control stage observerCount observerItems previousObserver observerSummary ->
  pmatch control $ \c ->
    pcon $ PCekContextControlV1
      (pdata stage)
      (pcekContext'languageTag c)
      (pcekContext'programTermRoot c)
      (pcekContext'programEnvelopeHash c)
      (pcekContext'purposeKind c)
      (pcekContext'purposeIndex c)
      (pcekContext'scriptHash c)
      (pcekContext'subject c)
      (pcekContext'redeemerLeaf c)
      (pcekContext'redeemerContextControlHash c)
      (pcekContext'executionMemoryLimit c)
      (pcekContext'executionCpuLimit c)
      (pcekContext'referenceItems c)
      (pcekContext'spendItems c)
      (pcekContext'outputItems c)
      (pcekContext'signerItems c)
      (pdata observerCount)
      (pdata observerItems)
      (pdata previousObserver)
      (pdata observerSummary)
      (pcekContext'mintCursor c)
      (pcekContext'currentMintPolicy c)
      (pcekContext'currentMintAssets c)
      (pcekContext'mintPolicies c)
      (pcekContext'mintSummary c)

-- | The no-auxiliary completion branches of Aiken
-- @verify_cek_observer_context_step@. Item-opening witnesses are handled by the
-- separate observer scan helper.
pverifyCekObserverContextAdvance :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekObserverContextAdvance = phoistAcyclic $ plam $ \pre witness nativeControl contextControl executionCursor completedCpu completedMemory ->
  pmatch pre $ \preState ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pnativeControl'compactCbor native)
        # pfromData (pnativeControl'witnessSetCompactCbor native)
        # pfromData (pnativeControl'fieldPreimageLengthsCbor native)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  pmatch (pverified'txCompact verified) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
    plet (pbodyCompact'requiredObserversHash body) $ \observerCommitment ->
    plet (pfromData $ pcekContext'observerCount context) $ \observerCount ->
    plet (pfromData $ pcekContext'observerItems context) $ \observerItems ->
    plet (pfromData (pcekContext'languageTag context) #== 128) $ \midgardEncoding ->
      pverified'version verified #== 1
        #&& pif
          (observerCommitment #== NativeField.pemptyFieldCommitment)
          ( pobserverContextIsPristine # contextControl
              #&& pcekContextSuccessorIsExact
                # pre # witness # nativeControl
                # ( pcekContextWithObservers # contextControl # 6 # observerCount
                      # observerItems
                      # pfromData (pcekContext'previousObserver context)
                      # (pfinalizeCekObserverItemsV1 # observerItems # midgardEncoding)
                  )
                # executionCursor # completedCpu # completedMemory
          )
          ( observerCount #> 0
              #&& pdataSequenceSummaryLength # observerItems #== observerCount
              #&& pcekContextSuccessorIsExact
                # pre # witness # nativeControl
                # ( pcekContextWithObservers # contextControl # 6 # observerCount
                      # observerItems
                      # pfromData (pcekContext'previousObserver context)
                      # (pfinalizeCekObserverItemsV1 # observerItems # midgardEncoding)
                  )
                # executionCursor # completedCpu # completedMemory
          )

-- | The item-opening branch of Aiken @verify_cek_observer_context_step@ after
-- unwrapping @TransactionFieldChunkWitness@ from the auxiliary witness sum.
pverifyCekObserverContextItem :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PItemProofV1 :--> BoundedItem.PChunkProofV1
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekObserverContextItem = phoistAcyclic $ plam $ \pre witness nativeControl contextControl collectionProof chunkProof executionCursor completedCpu completedMemory ->
  pmatch pre $ \preState ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
  pmatch collectionProof $ \collection ->
  pmatch chunkProof $ \chunk ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pnativeControl'compactCbor native)
        # pfromData (pnativeControl'witnessSetCompactCbor native)
        # pfromData (pnativeControl'fieldPreimageLengthsCbor native)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  pmatch (pverified'txCompact verified) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
    plet (pbodyCompact'requiredObserversHash body) $ \observerCommitment ->
    plet (pfromData $ pcekContext'observerCount context) $ \observerCount ->
    plet (pfromData $ pcekContext'observerItems context) $ \observerItems ->
    plet
      ( pif
          (observerCount #== 0)
          (pfromData $ pitemProof'itemCount collection)
          observerCount
      )
      $ \activeCount ->
    plet (activeCount - pdataSequenceSummaryLength # observerItems - 1) $ \expectedIndex ->
    plet (pfromData $ BoundedItem.pchunkProof'chunk chunk) $ \observerHash ->
    plet
      ( pprependCekObserverItemV1
          # observerHash
          # (pfromData (pcekContext'languageTag context) #== 128)
          # observerItems
      )
      $ \nextItems ->
        pand'List
          [ pverified'version verified #== 1
          , activeCount #> 0
          , activeCount #<= pmaxTxSizeDerivedItemCount
          , expectedIndex #>= 0
          , pfromData (pitemProof'fieldIndex collection) #== 3
          , pfromData (pitemProof'itemCount collection) #== activeCount
          , pfromData (pitemProof'itemIndex collection) #== expectedIndex
          , pfromData (pitemProof'itemLength collection) #== 28
          , pfromData (BoundedItem.pchunkProof'fieldIndex chunk) #== 3
          , pfromData (BoundedItem.pchunkProof'itemIndex chunk) #== expectedIndex
          , pfromData (BoundedItem.pchunkProof'totalLength chunk) #== 28
          , pfromData (BoundedItem.pchunkProof'chunkIndex chunk) #== 0
          , BoundedItem.pchunkCount
              # pfromData (BoundedItem.pchunkProof'totalLength chunk)
              #== 1
          , pverifyBoundedCollectionItem # observerCommitment # collectionProof
          , BoundedItem.pverifyChunk
              # pfromData (pitemProof'itemCommitment collection)
              # chunkProof
          , pif
              (pdataSequenceSummaryLength # observerItems #== 0)
              (pfromData (pcekContext'previousObserver context) #== pconstant "")
              (observerHash #< pfromData (pcekContext'previousObserver context))
          , pcekContextSuccessorIsExact
              # pre # witness # nativeControl
              # ( pcekContextWithObservers # contextControl
                    # pfromData (pcekContext'stage context)
                    # activeCount # nextItems # observerHash
                    # pfromData (pcekContext'observerSummary context)
                )
              # executionCursor # completedCpu # completedMemory
          ]

-- | Aiken @verify_cek_observer_context_step@.
pverifyCekObserverContextStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PNativeScriptsControlV1
        :--> PCekContextControlV1 :--> PInteger :--> PInteger :--> PInteger
        :--> PBool
    )
pverifyCekObserverContextStep = phoistAcyclic $ plam $ \pre witness auxiliary nativeControl contextControl executionCursor completedCpu completedMemory ->
  pmatch auxiliary $ \case
    PNoAuxiliaryWitness ->
      pverifyCekObserverContextAdvance
        # pre # witness # nativeControl # contextControl
        # executionCursor # completedCpu # completedMemory
    PTransactionFieldChunkWitness collectionProof chunkProof ->
      pverifyCekObserverContextItem
        # pre # witness # nativeControl # contextControl
        # pfromData collectionProof # pfromData chunkProof
        # executionCursor # completedCpu # completedMemory
    _ -> pconstant False

pcekContextWithMint :: forall s.
  Term s
    ( PCekContextControlV1
        :--> PInteger
        :--> PInteger
        :--> PByteString
        :--> CekData.PDataSequenceSummaryV1
        :--> CekData.PDataSequenceSummaryV1
        :--> CekData.PDataSummaryV1
        :--> PCekContextControlV1
    )
pcekContextWithMint = phoistAcyclic $ plam $ \control stage cursor policy assets policies mintSummary ->
  pmatch control $ \c ->
    pcon $ PCekContextControlV1
      (pdata stage)
      (pcekContext'languageTag c)
      (pcekContext'programTermRoot c)
      (pcekContext'programEnvelopeHash c)
      (pcekContext'purposeKind c)
      (pcekContext'purposeIndex c)
      (pcekContext'scriptHash c)
      (pcekContext'subject c)
      (pcekContext'redeemerLeaf c)
      (pcekContext'redeemerContextControlHash c)
      (pcekContext'executionMemoryLimit c)
      (pcekContext'executionCpuLimit c)
      (pcekContext'referenceItems c)
      (pcekContext'spendItems c)
      (pcekContext'outputItems c)
      (pcekContext'signerItems c)
      (pcekContext'observerCount c)
      (pcekContext'observerItems c)
      (pcekContext'previousObserver c)
      (pcekContext'observerSummary c)
      (pdata cursor)
      (pdata policy)
      (pdata assets)
      (pdata policies)
      (pdata mintSummary)

pprependCekMintAssetSummary :: forall s.
  Term s
    ( PByteString
        :--> PInteger
        :--> CekData.PDataSequenceSummaryV1
        :--> CekData.PDataSequenceSummaryV1
    )
pprependCekMintAssetSummary = phoistAcyclic $ plam $ \assetName quantity tailSummary ->
  CekData.pprependDataPairSummaryV1
    # (CekData.psemanticDataSummaryV1 # pforgetData (pdata assetName))
    # (CekData.psemanticDataSummaryV1 # pforgetData (pdata quantity))
    # tailSummary

pfinalizeCurrentCekMintPolicy :: forall s.
  Term s (PCekContextControlV1 :--> CekData.PDataSequenceSummaryV1)
pfinalizeCurrentCekMintPolicy = phoistAcyclic $ plam $ \control ->
  pmatch control $ \c ->
    pif
      (plengthBS # pfromData (pcekContext'currentMintPolicy c) #== 28)
      ( CekData.pprependDataPairSummaryV1
          # ( CekData.psemanticDataSummaryV1
                # pforgetData (pcekContext'currentMintPolicy c)
            )
          # (CekData.pmapDataSummaryV1 # pfromData (pcekContext'currentMintAssets c))
          # pfromData (pcekContext'mintPolicies c)
      )
      perror

-- | Aiken @verify_cek_mint_context_init@ after selecting its no-auxiliary branch.
pverifyCekMintContextInit :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekMintContextInit = phoistAcyclic $ plam $ \pre witness nativeControl contextControl executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
    plet (pfromData $ pnativeControl'mintCount native) $ \mintCount ->
      pcekContextSuccessorIsExact
        # pre # witness # nativeControl
        # ( pif
              (mintCount #== 0)
              ( pcekContextWithMint # contextControl # 9 # 0 # pconstant ""
                  # CekData.pemptyDataPairSummaryV1
                  # CekData.pemptyDataPairSummaryV1
                  # (CekData.pmapDataSummaryV1 # CekData.pemptyDataPairSummaryV1)
              )
              ( pcekContextWithMint # contextControl # 8
                  # pfromData (pcekContext'mintCursor context)
                  # pfromData (pcekContext'currentMintPolicy context)
                  # pfromData (pcekContext'currentMintAssets context)
                  # pfromData (pcekContext'mintPolicies context)
                  # pfromData (pcekContext'mintSummary context)
              )
          )
        # executionCursor # completedCpu # completedMemory

-- | The completed no-auxiliary branch of Aiken @verify_cek_mint_context_item@.
pverifyCekMintContextAdvance :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekMintContextAdvance = phoistAcyclic $ plam $ \pre witness nativeControl contextControl executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
    pif
      (pfromData (pcekContext'mintCursor context) #== pfromData (pnativeControl'mintCount native))
      ( plet (pfinalizeCurrentCekMintPolicy # contextControl) $ \finalPolicies ->
          pcekContextSuccessorIsExact
            # pre # witness # nativeControl
            # ( pcekContextWithMint # contextControl # 9
                  # pfromData (pcekContext'mintCursor context)
                  # pconstant ""
                  # CekData.pemptyDataPairSummaryV1
                  # finalPolicies
                  # (CekData.pmapDataSummaryV1 # finalPolicies)
              )
            # executionCursor # completedCpu # completedMemory
      )
      (pconstant False)

-- | The item branch of Aiken @verify_cek_mint_context_item@ after unwrapping
-- @CekMintContextItemWitness@ from the auxiliary witness sum.
pverifyCekMintContextItem :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PInteger :--> PByteString :--> PByteString :--> PInteger
        :--> PBuiltinList (PAsData PByteString)
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekMintContextItem = phoistAcyclic $ plam $ \pre witness nativeControl contextControl mintIndex policyId assetName quantity siblings executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
    plet
      (pfromData (pnativeControl'mintCount native) - pfromData (pcekContext'mintCursor context) - 1)
      $ \expectedIndex ->
    plet
      ( mintIndex #== expectedIndex
          #&& pverifyMembership
            # pfromData (pnativeControl'mintCount native)
            # pfromData (pnativeControl'mintPeaks native)
            # mintIndex
            # (pmintAssetLeafHash # policyId # assetName # quantity)
            # siblings
      )
      $ \membershipVerified ->
    plet (pfromData $ pcekContext'currentMintPolicy context) $ \currentPolicy ->
    plet (pfromData $ pcekContext'currentMintAssets context) $ \currentAssets ->
      pif
        (currentPolicy #== pconstant "")
        ( membershipVerified
            #&& pcekContextSuccessorIsExact
              # pre # witness # nativeControl
              # ( pcekContextWithMint # contextControl
                    # pfromData (pcekContext'stage context)
                    # (pfromData (pcekContext'mintCursor context) + 1)
                    # policyId
                    # (pprependCekMintAssetSummary # assetName # quantity # currentAssets)
                    # pfromData (pcekContext'mintPolicies context)
                    # pfromData (pcekContext'mintSummary context)
                )
              # executionCursor # completedCpu # completedMemory
        )
        ( pif
            (policyId #== currentPolicy)
            ( membershipVerified
                #&& pcekContextSuccessorIsExact
                  # pre # witness # nativeControl
                  # ( pcekContextWithMint # contextControl
                        # pfromData (pcekContext'stage context)
                        # (pfromData (pcekContext'mintCursor context) + 1)
                        # currentPolicy
                        # (pprependCekMintAssetSummary # assetName # quantity # currentAssets)
                        # pfromData (pcekContext'mintPolicies context)
                        # pfromData (pcekContext'mintSummary context)
                    )
                  # executionCursor # completedCpu # completedMemory
            )
            ( plet (pfinalizeCurrentCekMintPolicy # contextControl) $ \nextPolicies ->
                membershipVerified
                  #&& policyId #< currentPolicy
                  #&& pcekContextSuccessorIsExact
                    # pre # witness # nativeControl
                    # ( pcekContextWithMint # contextControl
                          # pfromData (pcekContext'stage context)
                          # (pfromData (pcekContext'mintCursor context) + 1)
                          # policyId
                          # ( pprependCekMintAssetSummary # assetName # quantity
                                # CekData.pemptyDataPairSummaryV1
                            )
                          # nextPolicies
                          # pfromData (pcekContext'mintSummary context)
                      )
                    # executionCursor # completedCpu # completedMemory
            )
        )

pcekRedeemerContextWithActive :: forall s.
  Term s
    ( PCekRedeemerContextControlV1
        :--> PByteString
        :--> PByteString
        :--> CekData.PDataSummaryV1
        :--> PCekRedeemerContextControlV1
    )
pcekRedeemerContextWithActive = phoistAcyclic $ plam $ \control scanHash redeemerLeaf purpose ->
  pmatch control $ \c ->
    pcon $ PCekRedeemerContextControlV1
      (pcekRedeemer'cursor c)
      (pcekRedeemer'mapItems c)
      (pdata scanHash)
      (pdata redeemerLeaf)
      (pdata purpose)
      (pcekRedeemer'currentRedeemer c)

pcekRedeemerContextMainSuccessor :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PCekRedeemerContextControlV1
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pcekRedeemerContextMainSuccessor = phoistAcyclic $ plam $ \pre witness nativeControl contextControl nextRedeemers executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch nextRedeemers $ \redeemers ->
    plet
      (pfromData (pcekRedeemer'cursor redeemers) #== pfromData (pnativeControl'redeemerCount native))
      $ \completed ->
        pcekRedeemerContextControlIsWellFormed
          # pfromData (pnativeControl'redeemerCount native)
          # nextRedeemers
          #&& pif
            completed
            ( pif
                ( pfromData (pcekRedeemer'activeScanHash redeemers) #== pconstant ""
                    #&& plengthBS
                      # (pdataSummaryRoot # pfromData (pcekRedeemer'currentRedeemer redeemers))
                      #== 32
                )
                ( pcekContextSuccessorIsExact
                    # pre # witness # nativeControl
                    # ( pcekContextWithStageAndHash # contextControl # 10
                          # (phashCekRedeemerContextControlV1 # nextRedeemers)
                      )
                    # executionCursor # completedCpu # completedMemory
                )
                perror
            )
            ( pcekContextSuccessorIsExact
                # pre # witness # nativeControl
                # ( pcekContextWithStageAndHash # contextControl
                      # 9 # (phashCekRedeemerContextControlV1 # nextRedeemers)
                  )
                # executionCursor # completedCpu # completedMemory
            )

-- | The selection branch of Aiken @verify_cek_redeemer_data_step@ after
-- unwrapping @CekRedeemerContextSelectWitness@ from the auxiliary witness sum.
pverifyCekRedeemerContextSelect :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PCekRedeemerContextControlV1
        :--> PInteger :--> PInteger :--> PInteger :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PInteger :--> PInteger :--> PInteger :--> PByteString :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekRedeemerContextSelect = phoistAcyclic $ plam $ \pre witness nativeControl contextControl control itemIndex itemCount totalLength itemCommitment redeemerSiblings purposeFrontierIndex purposeKind purposeIndex scriptHash subject purposeSiblings executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
  pmatch control $ \redeemers ->
    plet (ScriptProof.predeemerItemLeafHash # itemIndex # itemCommitment) $ \redeemerLeaf ->
    plet (ScriptProof.ppurposeLeafHash # purposeKind # purposeIndex # scriptHash # subject) $ \purposeLeaf ->
    plet
      ( pand'List
          [ pcekRedeemerContextControlIsWellFormed
              # pfromData (pnativeControl'redeemerCount native) # control
          , phashCekRedeemerContextControlV1 # control
              #== pfromData (pcekContext'redeemerContextControlHash context)
          , pfromData (pcekRedeemer'activeScanHash redeemers) #== pconstant ""
          ]
      )
      $ \currentMatches ->
    plet
      ( pand'List
          [ itemIndex
              #== pfromData (pnativeControl'redeemerCount native)
                - pfromData (pcekRedeemer'cursor redeemers) - 1
          , itemCount #== pfromData (pnativeControl'redeemerCount native)
          , pverifyMembership
              # pfromData (pnativeControl'redeemerCount native)
              # pfromData (pnativeControl'redeemerPeaks native)
              # itemIndex # redeemerLeaf # redeemerSiblings
          , purposeFrontierIndex #>= 0
          , purposeFrontierIndex #< pfromData (pnativeControl'purposeCount native)
          , pverifyMembership
              # pfromData (pnativeControl'purposeCount native)
              # pfromData (pnativeControl'purposePeaks native)
              # purposeFrontierIndex # purposeLeaf # purposeSiblings
          ]
      )
      $ \membershipMatches ->
    pmatch (predeemerTagForPurposeKindV1 # purposeKind) $ \case
      PNothing -> pconstant False
      PJust purposeTag ->
        pmatch
          ( ScriptContext.pscriptPurposeSummaryV1
              # purposeKind # scriptHash # subject
              # (pfromData (pcekContext'languageTag context) #== 128)
          )
          $ \case
            PNothing ->
              pif
                ( currentMatches #&& membershipMatches
                    #&& purposeKind #== 3
                    #&& pfromData (pcekContext'languageTag context) #== 3
                )
                ( plet
                    ( RedeemerItemProof.pinitialControlV1
                        # RedeemerItemProof.pmodeDescriptor
                        # itemIndex # itemCount # totalLength # itemCommitment
                        # purposeTag # purposeIndex
                    )
                    $ \itemControl ->
                      pcekRedeemerContextMainSuccessor
                        # pre # witness # nativeControl # contextControl
                        # ( pcekRedeemerContextWithActive # control
                              # (RedeemerItemProof.phashControlV1 # itemControl)
                              # redeemerLeaf # pemptyDataSummaryV1
                          )
                        # executionCursor # completedCpu # completedMemory
                )
                (pconstant False)
            PJust purpose ->
              plet
                ( RedeemerItemProof.pinitialControlV1
                    # RedeemerItemProof.pmodeData
                    # itemIndex # itemCount # totalLength # itemCommitment
                    # purposeTag # purposeIndex
                )
                $ \itemControl ->
                  currentMatches #&& membershipMatches
                    #&& pcekRedeemerContextMainSuccessor
                      # pre # witness # nativeControl # contextControl
                      # ( pcekRedeemerContextWithActive # control
                            # (RedeemerItemProof.phashControlV1 # itemControl)
                            # redeemerLeaf # purpose
                        )
                      # executionCursor # completedCpu # completedMemory

pcekRedeemerContextAfterDescriptor :: forall s.
  Term s (PCekRedeemerContextControlV1 :--> PCekRedeemerContextControlV1)
pcekRedeemerContextAfterDescriptor = phoistAcyclic $ plam $ \control ->
  pmatch control $ \c ->
    pcon $ PCekRedeemerContextControlV1
      (pdata $ pfromData (pcekRedeemer'cursor c) + 1)
      (pcekRedeemer'mapItems c)
      (pdata $ pconstant "")
      (pdata $ pconstant "")
      (pdata pemptyDataSummaryV1)
      (pcekRedeemer'currentRedeemer c)

pcekRedeemerContextAfterData :: forall s.
  Term s
    ( PCekRedeemerContextControlV1
        :--> CekData.PDataSummaryV1
        :--> CekData.PDataSummaryV1
        :--> PCekRedeemerContextControlV1
    )
pcekRedeemerContextAfterData = phoistAcyclic $ plam $ \control summary nextCurrent ->
  pmatch control $ \c ->
    pcon $ PCekRedeemerContextControlV1
      (pdata $ pfromData (pcekRedeemer'cursor c) + 1)
      ( pdata
          $ CekData.pprependDataPairSummaryV1
            # pfromData (pcekRedeemer'activePurpose c)
            # summary
            # pfromData (pcekRedeemer'mapItems c)
      )
      (pdata $ pconstant "")
      (pdata $ pconstant "")
      (pdata pemptyDataSummaryV1)
      (pdata nextCurrent)

pcekRedeemerContextWithScanHash :: forall s.
  Term s (PCekRedeemerContextControlV1 :--> PByteString :--> PCekRedeemerContextControlV1)
pcekRedeemerContextWithScanHash = phoistAcyclic $ plam $ \control scanHash ->
  pmatch control $ \c ->
    pcon $ PCekRedeemerContextControlV1
      (pcekRedeemer'cursor c)
      (pcekRedeemer'mapItems c)
      (pdata scanHash)
      (pcekRedeemer'activeRedeemerLeaf c)
      (pcekRedeemer'activePurpose c)
      (pcekRedeemer'currentRedeemer c)

-- | Apply an authenticated advanced redeemer-item control to the CEK context.
pverifyCekRedeemerContextAdvanced :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PCekRedeemerContextControlV1
        :--> RedeemerItemProof.PRedeemerItemProofControlV1
        :--> RedeemerItemProof.PRedeemerItemProofControlV1
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekRedeemerContextAdvanced = phoistAcyclic $ plam $ \pre witness nativeControl contextControl redeemerControl itemControl next executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
  pmatch redeemerControl $ \redeemers ->
  pmatch itemControl $ \item ->
    plet
      ( pand'List
          [ pcekRedeemerContextControlIsWellFormed
              # pfromData (pnativeControl'redeemerCount native) # redeemerControl
          , phashCekRedeemerContextControlV1 # redeemerControl
              #== pfromData (pcekContext'redeemerContextControlHash context)
          , pfromData (RedeemerItemProof.predeemerControl'itemCount item)
              #== pfromData (pnativeControl'redeemerCount native)
          , pfromData (RedeemerItemProof.predeemerControl'itemIndex item)
              #== pfromData (pnativeControl'redeemerCount native)
                - pfromData (pcekRedeemer'cursor redeemers) - 1
          , ScriptProof.predeemerItemLeafHash
              # pfromData (RedeemerItemProof.predeemerControl'itemIndex item)
              # pfromData (RedeemerItemProof.predeemerControl'itemCommitment item)
              #== pfromData (pcekRedeemer'activeRedeemerLeaf redeemers)
          , pfromData (pcekRedeemer'activeScanHash redeemers)
              #== RedeemerItemProof.phashControlV1 # itemControl
          ]
      )
      $ \currentMatches ->
    pmatch next $ \nextFields ->
      plet
        ( pif
            (pfromData (RedeemerItemProof.predeemerControl'stage nextFields) #== RedeemerItemProof.pstageTerminal)
            ( pif
                (pfromData (RedeemerItemProof.predeemerControl'mode nextFields) #== RedeemerItemProof.pmodeDescriptor)
                (pcekRedeemerContextAfterDescriptor # redeemerControl)
                ( pmatch (RedeemerItemProof.pfinalizeV1 # next) $ \case
                    PNothing -> perror
                    PJust summary ->
                      pcekRedeemerContextAfterData
                        # redeemerControl # summary
                        # ( pif
                              ( pfromData (pcekRedeemer'activeRedeemerLeaf redeemers)
                                  #== pfromData (pcekContext'redeemerLeaf context)
                              )
                              summary
                              (pfromData $ pcekRedeemer'currentRedeemer redeemers)
                          )
                )
            )
            ( pcekRedeemerContextWithScanHash # redeemerControl
                # (RedeemerItemProof.phashControlV1 # next)
            )
        )
        $ \nextRedeemers ->
          currentMatches
            #&& pif
              (pfromData (RedeemerItemProof.predeemerControl'mode nextFields) #== RedeemerItemProof.pmodeDescriptor)
              ( pand'List
                  [ pfromData (RedeemerItemProof.predeemerControl'expectedPurposeTag item) #== 6
                  , pfromData (pcekContext'languageTag context) #== 3
                  , pfromData (pcekRedeemer'activePurpose redeemers) #== pemptyDataSummaryV1
                  ]
              )
              ( plengthBS
                  # (pdataSummaryRoot # pfromData (pcekRedeemer'activePurpose redeemers))
                  #== 32
              )
            #&& pcekRedeemerContextMainSuccessor
              # pre # witness # nativeControl # contextControl # nextRedeemers
              # executionCursor # completedCpu # completedMemory

-- | The item-step branch of Aiken @verify_cek_redeemer_data_step@ after
-- unwrapping @RedeemerItemStepWitness@ with a present redeemer control.
pverifyCekRedeemerContextStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PCekRedeemerContextControlV1
        :--> RedeemerItemProof.PRedeemerItemProofControlV1
        :--> RedeemerItemProof.PRedeemerItemProofWitnessV1
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekRedeemerContextStep = phoistAcyclic $ plam $ \pre witness nativeControl contextControl redeemerControl itemControl itemWitness executionCursor completedCpu completedMemory ->
  pmatch (RedeemerItemProof.pstepV1 # itemControl # itemWitness) $ \case
    PNothing -> pconstant False
    PJust stepResult -> pmatch stepResult $ \case
      RedeemerItemProof.PRedeemerItemProofInvalid -> pconstant False
      RedeemerItemProof.PRedeemerItemProofAdvanced nextData ->
        pverifyCekRedeemerContextAdvanced
          # pre # witness # nativeControl # contextControl # redeemerControl
          # itemControl # pfromData nextData
          # executionCursor # completedCpu # completedMemory

pcekContextFinalizeWithScriptInfo :: forall s.
  Term s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1
        :--> PCekContextControlV1
        :--> PCekRedeemerContextControlV1
        :--> CekData.PDataSummaryV1
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
    )
pcekContextFinalizeWithScriptInfo = phoistAcyclic $ plam $ \pre witness nativeControl contextControl redeemers scriptInfo executionCursor completedCpu completedMemory ->
  pmatch redeemers $ \redeemer ->
    plet
      ( pcon $ PCekContextPartsControlV1
          (pcekRedeemer'mapItems redeemer)
          (pcekRedeemer'currentRedeemer redeemer)
          (pdata scriptInfo)
      )
      $ \parts ->
        pcompletedCekRedeemerContextMatches # nativeControl # contextControl # redeemers
          #&& pcekContextSuccessorIsExact
            # pre
            # witness
            # nativeControl
            # ( pcekContextWithStageAndHash
                  # contextControl
                  # 11
                  # (phashCekContextPartsControlV1 # parts)
              )
            # executionCursor
            # completedCpu
            # completedMemory

-- | The non-spending branch of Aiken @verify_cek_context_finalize@ after
-- unwrapping @CekContextFinalizeWitness@ from the auxiliary witness sum.
pverifyCekContextFinalizeControl :: forall s.
  Term s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1
        :--> PCekContextControlV1
        :--> PCekRedeemerContextControlV1
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
    )
pverifyCekContextFinalizeControl = phoistAcyclic $ plam $ \pre witness nativeControl contextControl redeemers executionCursor completedCpu completedMemory ->
  pmatch contextControl $ \context ->
    plet (pfromData (pcekContext'languageTag context) #== 128) $ \midgardEncoding ->
      pmatch
        ( pif
            midgardEncoding
            ( ScriptContext.pscriptPurposeSummaryV1
                # pfromData (pcekContext'purposeKind context)
                # pfromData (pcekContext'scriptHash context)
                # pfromData (pcekContext'subject context)
                # pconstant True
            )
            ( ScriptContext.pcardanoScriptInfoSummaryV1
                # pfromData (pcekContext'purposeKind context)
                # pfromData (pcekContext'scriptHash context)
                # pfromData (pcekContext'subject context)
                # pcon PDNothing
            )
        )
        $ \case
          PNothing -> pconstant False
          PJust scriptInfo ->
            pcekContextFinalizeWithScriptInfo
              # pre
              # witness
              # nativeControl
              # contextControl
              # redeemers
              # scriptInfo
              # executionCursor
              # completedCpu
              # completedMemory

-- | The Cardano spending branch of Aiken @verify_cek_context_finalize@ after
-- unwrapping @CekContextFinalizeSpendWitness@ from the auxiliary witness sum.
pverifyCekContextFinalizeSpendControl :: forall s.
  Term s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1
        :--> PCekContextControlV1
        :--> PCekRedeemerContextControlV1
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
    )
pverifyCekContextFinalizeSpendControl = phoistAcyclic $ plam $ \pre witness nativeControl contextControl redeemers itemIndex key descriptorCbor siblings executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
    plet
      ( ScriptProof.presolvedContextItemLeafHash
          # 0
          # itemIndex
          # key
          # descriptorCbor
      )
      $ \leaf ->
        pif
          ( pand'List
              [ pfromData (pcekContext'languageTag context) #/= 128
              , pfromData (pcekContext'purposeKind context) #== 0
              , itemIndex #== pfromData (pcekContext'purposeIndex context)
              , key #== pfromData (pcekContext'subject context)
              , pverifyMembership
                  # pfromData (pnativeControl'resolvedInputCount native)
                  # pfromData (pnativeControl'resolvedItemPeaks native)
                  # itemIndex
                  # leaf
                  # siblings
              ]
          )
          ( pmatch
              ( ScriptContext.pcardanoSpendScriptInfoFromDescriptorV1
                  # pfromData (pcekContext'subject context)
                  # descriptorCbor
              )
              $ \case
                PNothing -> pconstant False
                PJust scriptInfo ->
                  pcekContextFinalizeWithScriptInfo
                    # pre
                    # witness
                    # nativeControl
                    # contextControl
                    # redeemers
                    # scriptInfo
                    # executionCursor
                    # completedCpu
                    # completedMemory
          )
          (pconstant False)

-- | Aiken @verify_cek_context_finalize@.
pverifyCekContextFinalize :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PNativeScriptsControlV1
        :--> PCekContextControlV1 :--> PInteger :--> PInteger :--> PInteger
        :--> PBool
    )
pverifyCekContextFinalize = phoistAcyclic $ plam $ \pre witness auxiliary nativeControl contextControl executionCursor completedCpu completedMemory ->
  pmatch contextControl $ \context ->
  pif
    ( pfromData (pcekContext'languageTag context) #/= 128
        #&& pfromData (pcekContext'purposeKind context) #== 0
    )
    ( pmatch auxiliary $ \case
        PCekContextFinalizeSpendWitness redeemers itemIndex key descriptorCbor siblings ->
          pverifyCekContextFinalizeSpendControl
            # pre # witness # nativeControl # contextControl # pfromData redeemers
            # pfromData itemIndex # pfromData key # pfromData descriptorCbor
            # pfromData siblings # executionCursor # completedCpu # completedMemory
        _ -> pconstant False
    )
    ( pmatch auxiliary $ \case
        PCekContextFinalizeWitness redeemers ->
          pverifyCekContextFinalizeControl
            # pre # witness # nativeControl # contextControl # pfromData redeemers
            # executionCursor # completedCpu # completedMemory
        _ -> pconstant False
    )

-- | Aiken @verify_cek_context_assemble@ after unwrapping
-- @CekContextAssembleWitness@ from the auxiliary witness sum.
pverifyCekContextAssembleControl :: forall s.
  Term s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1
        :--> PCekContextControlV1
        :--> PCekContextPartsControlV1
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
    )
pverifyCekContextAssembleControl = phoistAcyclic $ plam $ \pre witness nativeControl contextControl partsControl executionCursor completedCpu completedMemory ->
  pmatch pre $ \preState ->
  pmatch contextControl $ \context ->
  pmatch partsControl $ \parts ->
    plet
      ( pcon $ PCekTxInfoAssemblyControlV1
          ( pdata
              $ ScriptContext.ptxInfoTailFieldsSummaryV1
                # (pfromData (pcekContext'languageTag context) #== 128)
                # pfromData (pcekContext'observerSummary context)
                # pfromData (pcekContext'signerItems context)
                # pfromData (pcekContext'mintSummary context)
                # pfromData (pcekParts'redeemerItems parts)
                # pfromData (pmachineState'transactionId preState)
          )
          (pcekParts'redeemer parts)
          (pcekParts'scriptInfo parts)
      )
      $ \assembly ->
        phashCekContextPartsControlV1 # partsControl
          #== pfromData (pcekContext'redeemerContextControlHash context)
          #&& pcekContextSuccessorIsExact
            # pre
            # witness
            # nativeControl
            # ( pcekContextWithStageAndHash
                  # contextControl
                  # 12
                  # (phashCekTxInfoAssemblyControlV1 # assembly)
              )
            # executionCursor
            # completedCpu
            # completedMemory

-- | Aiken @verify_cek_tx_info_finalize@ after unwrapping
-- @CekTxInfoFinalizeWitness@ from the auxiliary witness sum.
pverifyCekTxInfoFinalizeControl :: forall s.
  Term s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1
        :--> PCekContextControlV1
        :--> PCekTxInfoAssemblyControlV1
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
    )
pverifyCekTxInfoFinalizeControl = phoistAcyclic $ plam $ \pre witness nativeControl contextControl assemblyControl executionCursor completedCpu completedMemory ->
  pmatch nativeControl $ \native ->
  pmatch contextControl $ \context ->
  pmatch assemblyControl $ \assembly ->
  pmatch (NativeCompact.pdecodeNativeTxCompactV1 # pfromData (pnativeControl'compactCbor native)) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
    plet
      ( ScriptContext.ptxInfoFromTailSummaryV1
          # (pfromData (pcekContext'languageTag context) #== 128)
          # pfromData (pcekContext'spendItems context)
          # pfromData (pcekContext'referenceItems context)
          # pfromData (pcekContext'outputItems context)
          # pbodyCompact'fee body
          # pbodyCompact'validityIntervalStart body
          # pbodyCompact'validityIntervalEnd body
          # pfromData (pcekContext'observerSummary context)
          # pfromData (pcekContext'mintSummary context)
          # pfromData (pcekAssembly'tailFields assembly)
      )
      $ \txInfo ->
      plet
        ( pcon $ PCekFinalContextControlV1
            (pdata txInfo)
            (pcekAssembly'redeemer assembly)
            (pcekAssembly'scriptInfo assembly)
        )
        $ \finalContext ->
          phashCekTxInfoAssemblyControlV1 # assemblyControl
            #== pfromData (pcekContext'redeemerContextControlHash context)
            #&& pcekContextSuccessorIsExact
              # pre
              # witness
              # nativeControl
              # ( pcekContextWithStageAndHash
                    # contextControl
                    # 13
                    # (phashCekFinalContextControlV1 # finalContext)
                )
              # executionCursor
              # completedCpu
              # completedMemory

pcekContextExecutionSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1
        :--> PCekContextControlV1
        :--> CekData.PDataSummaryV1
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
    )
pcekContextExecutionSuccessorIsExact = phoistAcyclic $ plam $ \pre witness nativeControl contextControl contextSummary executionCursor completedCpu completedMemory ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch (pfromData $ poneStep'claimedSuccessor stepWitness) $ \postState ->
  pmatch contextControl $ \context ->
    plet (CekConstant.psemanticDataConstantRootV1 # contextSummary) $ \contextValue ->
    plet (CekProof.phashContextConstantTermV1 # contextValue) $ \contextTerm ->
    plet
      ( pcon $ CekMachine.PMachineStateV1
          (pdata CekMachine.pmodeCompute)
          (pdata executionCursor)
          (pdata $ CekProof.phashApplicationTermV1 # pfromData (pcekContext'programTermRoot context) # contextTerm)
          (pdata CekProof.pemptyEnvironmentRootV1)
          (pdata CekProof.pemptyContinuationRootV1)
          (pdata 0)
          (pdata 0)
          (pdata 0)
      )
      $ \initialState ->
        pand'List
          [ pfromData (pmachineState'phase postState) #== pcon PCek
          , pfromData (pmachineState'executionCpu postState) #== pfromData (pmachineState'executionCpu preState)
          , pfromData (pmachineState'executionMemory postState) #== pfromData (pmachineState'executionMemory preState)
          , pfromData (pmachineState'workRoot postState)
              #== phashWorkWitness
                # pcon PCek
                # (pfromData (pmachineState'programCounter preState) + 1)
                # ( pencodeCekWitnessV1
                      # nativeControl
                      # pconstant ""
                      # executionCursor
                      # completedCpu
                      # completedMemory
                      # (CekMachine.phashStateV1 # initialState)
                      # pfromData (pcekContext'executionCpuLimit context)
                      # pfromData (pcekContext'executionMemoryLimit context)
                      # pfromData (pcekContext'programEnvelopeHash context)
                  )
          ]

-- | Aiken @verify_cek_context_seed@ after unwrapping
-- @CekContextSeedWitness@ from the auxiliary witness sum.
pverifyCekContextSeedControl :: forall s.
  Term s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1
        :--> PCekContextControlV1
        :--> PCekFinalContextControlV1
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
    )
pverifyCekContextSeedControl = phoistAcyclic $ plam $ \pre witness nativeControl contextControl finalControl executionCursor completedCpu completedMemory ->
  pmatch contextControl $ \context ->
  pmatch finalControl $ \final ->
    plet
      ( ScriptContext.pscriptContextSummaryV1
          # pfromData (pcekFinal'txInfo final)
          # pfromData (pcekFinal'redeemer final)
          # pfromData (pcekFinal'scriptInfo final)
      )
      $ \contextSummary ->
        phashCekFinalContextControlV1 # finalControl
          #== pfromData (pcekContext'redeemerContextControlHash context)
          #&& pcekContextExecutionSuccessorIsExact
            # pre
            # witness
            # nativeControl
            # contextControl
            # contextSummary
            # executionCursor
            # completedCpu
            # completedMemory

pcekContextAuxiliaryMatchesStage :: forall s.
  Term s
    ( PCekContextControlV1 :--> PValidationAuxiliaryWitnessV1 :--> PBool )
pcekContextAuxiliaryMatchesStage = phoistAcyclic $ plam $ \contextControl auxiliary ->
  pmatch contextControl $ \context ->
  plet (pfromData $ pcekContext'stage context) $ \stage ->
    pif (stage #== 0)
      ( pif
          (pfromData (pcekContext'redeemerContextControlHash context) #== pconstant "")
          (pmatch auxiliary $ \case PRedeemerScanBeginWitness {} -> pconstant True; _ -> pconstant False)
          ( pmatch auxiliary $ \case
              PRedeemerItemStepWitness redeemerControl _ _ ->
                pmatch (pfromData redeemerControl) $ \case
                  PDNothing -> pconstant True
                  PDJust _ -> pconstant False
              _ -> pconstant False
          )
      ) $
    pif (stage #== 1 #|| stage #== 2)
      (pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; PCekResolvedContextItemWitness {} -> pconstant True; _ -> pconstant False) $
    pif (stage #== 3)
      (pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; PCekOutputContextItemWitness {} -> pconstant True; _ -> pconstant False) $
    pif (stage #== 4)
      (pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; PCekSignerContextItemWitness {} -> pconstant True; _ -> pconstant False) $
    pif (stage #== 5)
      (pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; PTransactionFieldChunkWitness {} -> pconstant True; _ -> pconstant False) $
    pif (stage #== 6)
      (pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False) $
    pif (stage #== 7) (pconstant False) $
    pif (stage #== 8)
      (pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; PCekMintContextItemWitness {} -> pconstant True; _ -> pconstant False) $
    pif (stage #== 9)
      ( pmatch auxiliary $ \case
          PCekRedeemerContextSelectWitness {} -> pconstant True
          PRedeemerItemStepWitness redeemerControl _ _ ->
            pmatch (pfromData redeemerControl) $ \case
              PDNothing -> pconstant False
              PDJust _ -> pconstant True
          _ -> pconstant False
      ) $
    pif (stage #== 10)
      ( pif
          ( pfromData (pcekContext'languageTag context) #/= 128
              #&& pfromData (pcekContext'purposeKind context) #== 0
          )
          (pmatch auxiliary $ \case PCekContextFinalizeSpendWitness {} -> pconstant True; _ -> pconstant False)
          (pmatch auxiliary $ \case PCekContextFinalizeWitness {} -> pconstant True; _ -> pconstant False)
      ) $
    pif (stage #== 11)
      (pmatch auxiliary $ \case PCekContextAssembleWitness {} -> pconstant True; _ -> pconstant False) $
    pif (stage #== 12)
      (pmatch auxiliary $ \case PCekTxInfoFinalizeWitness {} -> pconstant True; _ -> pconstant False) $
    pif (stage #== 13)
      (pmatch auxiliary $ \case PCekContextSeedWitness {} -> pconstant True; _ -> pconstant False)
      (pconstant False)

-- | Aiken @verify_cek_context_step@ with the auxiliary witness sum intact.
pverifyCekContextStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1
        :--> PNativeScriptsControlV1 :--> PCekContextControlV1
        :--> PInteger :--> PInteger :--> PInteger :--> PBool
    )
pverifyCekContextStep = phoistAcyclic $ plam $ \pre witness auxiliary nativeControl contextControl executionCursor completedCpu completedMemory ->
  pmatch contextControl $ \context ->
  plet (pfromData $ pcekContext'stage context) $ \stage ->
  pif (pcekContextAuxiliaryMatchesStage # contextControl # auxiliary)
    (pif (stage #== 0)
      ( pif
          (pfromData (pcekContext'redeemerContextControlHash context) #== pconstant "")
          ( pmatch auxiliary $ \case
              PRedeemerScanBeginWitness itemIndex itemCount totalLength itemCommitment siblings ->
                pverifyCekInitialRedeemerBegin
                  # pre # witness # nativeControl # contextControl
                  # pfromData itemIndex # pfromData itemCount # pfromData totalLength
                  # pfromData itemCommitment # pfromData siblings
                  # executionCursor # completedCpu # completedMemory
              _ -> pconstant False
          )
          ( pmatch auxiliary $ \case
              PRedeemerItemStepWitness redeemerControl itemControl itemWitness ->
                pmatch (pfromData redeemerControl) $ \case
                  PDNothing ->
                    pverifyCekInitialRedeemerStep
                      # pre # witness # nativeControl # contextControl
                      # pfromData itemControl # pfromData itemWitness
                      # executionCursor # completedCpu # completedMemory
                  PDJust _ -> pconstant False
              _ -> pconstant False
          )
      ) $
    pif (stage #== 1)
      ( pmatch auxiliary $ \case
          PNoAuxiliaryWitness ->
            pverifyCekReferenceContextAdvance
              # pre # witness # nativeControl # contextControl
              # executionCursor # completedCpu # completedMemory
          PCekResolvedContextItemWitness sourceKind itemIndex key descriptorCbor siblings ->
            pverifyCekReferenceContextItem
              # pre # witness # nativeControl # contextControl
              # pfromData sourceKind # pfromData itemIndex # pfromData key
              # pfromData descriptorCbor # pfromData siblings
              # executionCursor # completedCpu # completedMemory
          _ -> pconstant False
      ) $
    pif (stage #== 2)
      ( pmatch auxiliary $ \case
          PNoAuxiliaryWitness ->
            pverifyCekSpendContextAdvance
              # pre # witness # nativeControl # contextControl
              # executionCursor # completedCpu # completedMemory
          PCekResolvedContextItemWitness sourceKind itemIndex key descriptorCbor siblings ->
            pverifyCekSpendContextItem
              # pre # witness # nativeControl # contextControl
              # pfromData sourceKind # pfromData itemIndex # pfromData key
              # pfromData descriptorCbor # pfromData siblings
              # executionCursor # completedCpu # completedMemory
          _ -> pconstant False
      ) $
    pif (stage #== 3)
      ( pmatch auxiliary $ \case
          PNoAuxiliaryWitness ->
            pverifyCekOutputContextAdvance
              # pre # witness # nativeControl # contextControl
              # executionCursor # completedCpu # completedMemory
          PCekOutputContextItemWitness outputIndex descriptorCbor siblings ->
            pverifyCekOutputContextItem
              # pre # witness # nativeControl # contextControl
              # pfromData outputIndex # pfromData descriptorCbor # pfromData siblings
              # executionCursor # completedCpu # completedMemory
          _ -> pconstant False
      ) $
    pif (stage #== 4)
      ( pmatch auxiliary $ \case
          PNoAuxiliaryWitness ->
            pverifyCekSignerContextAdvance
              # pre # witness # nativeControl # contextControl
              # executionCursor # completedCpu # completedMemory
          PCekSignerContextItemWitness peaks signerIndex signerHash siblings ->
            pverifyCekSignerContextItem
              # pre # witness # nativeControl # contextControl
              # pfromData peaks # pfromData signerIndex # pfromData signerHash
              # pfromData siblings # executionCursor # completedCpu # completedMemory
          _ -> pconstant False
      ) $
    pif (stage #== 5)
      ( pmatch auxiliary $ \case
          PNoAuxiliaryWitness ->
            pverifyCekObserverContextAdvance
              # pre # witness # nativeControl # contextControl
              # executionCursor # completedCpu # completedMemory
          PTransactionFieldChunkWitness collectionProof chunkProof ->
            pverifyCekObserverContextItem
              # pre # witness # nativeControl # contextControl
              # pfromData collectionProof # pfromData chunkProof
              # executionCursor # completedCpu # completedMemory
          _ -> pconstant False
      ) $
    pif (stage #== 6)
      ( pmatch auxiliary $ \case
          PNoAuxiliaryWitness ->
            pverifyCekMintContextInit
              # pre # witness # nativeControl # contextControl
              # executionCursor # completedCpu # completedMemory
          _ -> pconstant False
      ) $
    pif (stage #== 7) (pconstant False) $
    pif (stage #== 8)
      ( pmatch auxiliary $ \case
          PNoAuxiliaryWitness ->
            pverifyCekMintContextAdvance
              # pre # witness # nativeControl # contextControl
              # executionCursor # completedCpu # completedMemory
          PCekMintContextItemWitness mintIndex policyId assetName quantity siblings ->
            pverifyCekMintContextItem
              # pre # witness # nativeControl # contextControl
              # pfromData mintIndex # pfromData policyId # pfromData assetName
              # pfromData quantity # pfromData siblings
              # executionCursor # completedCpu # completedMemory
          _ -> pconstant False
      ) $
    pif (stage #== 9)
      ( pmatch auxiliary $ \case
          PCekRedeemerContextSelectWitness control itemIndex itemCount totalLength itemCommitment redeemerSiblings purposeFrontierIndex purposeKind purposeIndex scriptHash subject purposeSiblings ->
            pverifyCekRedeemerContextSelect
              # pre # witness # nativeControl # contextControl # pfromData control
              # pfromData itemIndex # pfromData itemCount # pfromData totalLength
              # pfromData itemCommitment # pfromData redeemerSiblings
              # pfromData purposeFrontierIndex # pfromData purposeKind
              # pfromData purposeIndex # pfromData scriptHash # pfromData subject
              # pfromData purposeSiblings # executionCursor # completedCpu # completedMemory
          PRedeemerItemStepWitness redeemerControl itemControl itemWitness ->
            pmatch (pfromData redeemerControl) $ \case
              PDNothing -> pconstant False
              PDJust active ->
                pverifyCekRedeemerContextStep
                  # pre # witness # nativeControl # contextControl
                  # pfromData active # pfromData itemControl # pfromData itemWitness
                  # executionCursor # completedCpu # completedMemory
          _ -> pconstant False
      ) $
    pif (stage #== 10)
      ( pmatch auxiliary $ \case
          PCekContextFinalizeWitness redeemerControl ->
            pverifyCekContextFinalizeControl
              # pre # witness # nativeControl # contextControl # pfromData redeemerControl
              # executionCursor # completedCpu # completedMemory
          PCekContextFinalizeSpendWitness redeemerControl itemIndex key descriptorCbor siblings ->
            pverifyCekContextFinalizeSpendControl
              # pre # witness # nativeControl # contextControl # pfromData redeemerControl
              # pfromData itemIndex # pfromData key # pfromData descriptorCbor
              # pfromData siblings # executionCursor # completedCpu # completedMemory
          _ -> pconstant False
      ) $
    pif (stage #== 11)
      ( pmatch auxiliary $ \case
          PCekContextAssembleWitness control ->
            pverifyCekContextAssembleControl
              # pre # witness # nativeControl # contextControl # pfromData control
              # executionCursor # completedCpu # completedMemory
          _ -> pconstant False
      ) $
    pif (stage #== 12)
      ( pmatch auxiliary $ \case
          PCekTxInfoFinalizeWitness control ->
            pverifyCekTxInfoFinalizeControl
              # pre # witness # nativeControl # contextControl # pfromData control
              # executionCursor # completedCpu # completedMemory
          _ -> pconstant False
      ) $
    pif (stage #== 13)
      ( pmatch auxiliary $ \case
          PCekContextSeedWitness control ->
            pverifyCekContextSeedControl
              # pre # witness # nativeControl # contextControl # pfromData control
              # executionCursor # completedCpu # completedMemory
          _ -> pconstant False
      )
      (pconstant False))
    (pconstant False)

-- | The no-execution successor branch of Aiken @verify_cek@.
pverifyCekCompleted :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PNativeScriptsControlV1
        :--> PBool
    )
pverifyCekCompleted = phoistAcyclic $ plam $ \pre witness auxiliary nativeControl ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch auxiliary $ \case
    PNoAuxiliaryWitness ->
      pmatch (pfromData $ poneStep'claimedSuccessor stepWitness) $ \postState ->
        pand'List
          [ pfromData (pmachineState'phase postState) #== pcon PValueAndMint
          , pmachineState'executionCpu postState #== pmachineState'executionCpu preState
          , pmachineState'executionMemory postState #== pmachineState'executionMemory preState
          , pfromData (pmachineState'workRoot postState)
              #== phashWorkWitness
                # pcon PValueAndMint
                # (pfromData (pmachineState'programCounter preState) + 1)
                # (pencodeValueAndMintWitnessV1 # nativeControl)
          ]
    _ -> pconstant False

-- | Aiken @verify_cek@. This authenticates and canonically re-encodes the
-- nine-field CEK work witness before routing it to selection, context
-- construction, or core execution.
pverifyCek :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyCek = phoistAcyclic $ plam $ \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet (pfromData $ poneStep'workWitnessCbor stepWitness) $ \workWitnessCbor ->
  pmatch (pdeserialise # workWitnessCbor) $ \case
    PNothing -> perror
    PJust dat ->
      plet (pasList # dat) $ \items ->
      pif (plength # items #== 9)
        ( plet (pasByteStr # (pelemAt # 0 # items)) $ \nativeControlCbor ->
          plet (pasByteStr # (pelemAt # 1 # items)) $ \contextControlCbor ->
          plet (pasInt # (pelemAt # 2 # items)) $ \executionCursor ->
          plet (pasInt # (pelemAt # 3 # items)) $ \completedCpu ->
          plet (pasInt # (pelemAt # 4 # items)) $ \completedMemory ->
          plet (pasByteStr # (pelemAt # 5 # items)) $ \activeStateHash ->
          plet (pasByteStr # (pelemAt # 6 # items)) $ \programEnvelopeHash ->
          plet (pasInt # (pelemAt # 7 # items)) $ \executionCpuLimit ->
          plet (pasInt # (pelemAt # 8 # items)) $ \executionMemoryLimit ->
          plet (pnativeScriptsControlFromWitness # nativeControlCbor) $ \nativeControl ->
          pmatch nativeControl $ \native ->
          pmatch
            ( NativeCompact.pverifyNativeTxProofSourceV1
                # pfromData (pmachineState'transactionId preState)
                # pfromData (pnativeControl'compactCbor native)
                # pfromData (pnativeControl'witnessSetCompactCbor native)
                # pfromData (pnativeControl'fieldPreimageLengthsCbor native)
            )
            $ \(PPair verifiedSource _) ->
          pmatch verifiedSource $ \verified ->
            pand'List
              [ pverified'version verified #== 1
              , NativeCompact.pnativeTxProofCommitmentV1
                  # pfromData (pnativeControl'compactCbor native)
                  # pfromData (pnativeControl'witnessSetCompactCbor native)
                  # pfromData (pnativeControl'fieldPreimageLengthsCbor native)
                  #== pfromData (pmachineState'transactionCommitment preState)
              , phashValidationContext # pfromData (pnativeControl'contextCbor native)
                  #== pfromData (pmachineState'validationContextHash preState)
              , pnativeScriptsControlIsWellFormed # nativeControl
              , pfromData (pnativeControl'executionCursor native)
                  #== pfromData (pnativeControl'executionCount native)
              , executionCursor #>= 0
              , executionCursor #<= pfromData (pnativeControl'executionCount native)
              , completedCpu #>= 0
              , completedMemory #>= 0
              , workWitnessCbor
                  #== pencodeCekWitnessV1
                    # nativeControl # contextControlCbor # executionCursor
                    # completedCpu # completedMemory # activeStateHash
                    # executionCpuLimit # executionMemoryLimit # programEnvelopeHash
              , pif
                  (activeStateHash #== pconstant "")
                  ( pif
                      (contextControlCbor #== pconstant "")
                      ( pand'List
                          [ executionCpuLimit #== 0
                          , executionMemoryLimit #== 0
                          , programEnvelopeHash #== pconstant ""
                          , pif
                              ( executionCursor #== pfromData (pnativeControl'executionCount native)
                                  #|| pfromData (pnativeControl'languageBitmap native) #== 0
                              )
                              (pverifyCekCompleted # pre # witness # auxiliary # nativeControl)
                              ( pmatch auxiliary $ \case
                                  PNativeExecutionScanWitness executionIndex languageTag purposeKind purposeIndex scriptHash subject purposeSiblings sourceIndex originKind sourceKey scriptTotalLength scriptItemCommitment sourceSiblings redeemerLeaf executionSiblings firstChunkProof ->
                                    pverifyCekExecutionSelection
                                      # pre # witness # nativeControl
                                      # executionCursor # completedCpu # completedMemory
                                      # pfromData executionIndex # pfromData languageTag
                                      # pfromData purposeKind # pfromData purposeIndex
                                      # pfromData scriptHash # pfromData subject
                                      # pfromData purposeSiblings # pfromData sourceIndex
                                      # pfromData originKind # pfromData sourceKey
                                      # pfromData scriptTotalLength # pfromData scriptItemCommitment
                                      # pfromData sourceSiblings # pfromData redeemerLeaf
                                      # pfromData executionSiblings # pfromData firstChunkProof
                                  _ -> pconstant False
                              )
                          ]
                      )
                      ( plet (pcekContextControlFromCbor # contextControlCbor) $ \contextControl ->
                        pmatch contextControl $ \context ->
                          pand'List
                            [ executionCursor #< pfromData (pnativeControl'executionCount native)
                            , executionCpuLimit #== 0
                            , executionMemoryLimit #== 0
                            , programEnvelopeHash #== pfromData (pcekContext'programEnvelopeHash context)
                            , contextControlCbor #== pencodeCekContextControlV1 # contextControl
                            , pcekContextControlIsWellFormed # nativeControl # contextControl
                            , pverifyCekContextStep
                                # pre # witness # auxiliary # nativeControl # contextControl
                                # executionCursor # completedCpu # completedMemory
                            ]
                      )
                  )
                  ( pand'List
                      [ contextControlCbor #== pconstant ""
                      , pmatch auxiliary $ \case
                          PCekCoreStepWitness evidence ->
                            pmatch (pfromData evidence) $ \(CekMachine.PCoreStepEvidenceV1 stepPre stepPost coreWitness) ->
                              pverifyCekCoreStep
                                # pre # witness # nativeControl
                                # executionCursor # completedCpu # completedMemory
                                # activeStateHash # executionCpuLimit # executionMemoryLimit
                                # programEnvelopeHash # pfromData stepPre # pfromData stepPost
                                # pfromData coreWitness
                          _ -> pconstant False
                      ]
                  )
              ]
        )
        perror

-- | Aiken @verify_cek_one_step_v1@.
pverifyCekOneStepV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyCekOneStepV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch pre $ \preState ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transition auxiliary) ->
  plet (pfromData transition) $ \witness ->
    pand'List
      [ pfromData (pmachineState'phase preState) #== pcon PCek
      , pstructuralTransitionIsValid # pre # witness
      , pverifyCek # pre # witness # pdecodeValidationAuxiliaryWitnessV1 auxiliary
      ]

pvalueAndMintWithStage :: forall s.
  Term s (PValueAndMintControlV1 :--> PInteger :--> PValueAndMintControlV1)
pvalueAndMintWithStage = phoistAcyclic $ plam $ \control stage ->
  pmatch control $ \c ->
    pcon $ PValueAndMintControlV1
      (pvalueAndMint'nativeControl c)
      (pdata stage)
      (pvalueAndMint'replayScheduleHash c)
      (pvalueAndMint'replayCursor c)
      (pvalueAndMint'replayAssetCursor c)
      (pvalueAndMint'replayValueHash c)
      (pvalueAndMint'replayAccumulator c)
      (pvalueAndMint'replayRemainingScheduleHash c)
      (pvalueAndMint'outputCursor c)
      (pvalueAndMint'outputAssetCursor c)
      (pvalueAndMint'mintCursor c)
      (pvalueAndMint'valueAccumulator c)

pvalueAndMintStageZeroSuccessor :: forall s.
  Term s (PValueAndMintControlV1 :--> PByteString :--> PValueAndMintControlV1)
pvalueAndMintStageZeroSuccessor = phoistAcyclic $ plam $ \control scheduleHash ->
  pmatch control $ \c ->
    pcon $ PValueAndMintControlV1
      (pvalueAndMint'nativeControl c)
      (pdata 1)
      (pdata scheduleHash)
      (pvalueAndMint'replayCursor c)
      (pvalueAndMint'replayAssetCursor c)
      (pvalueAndMint'replayValueHash c)
      (pvalueAndMint'replayAccumulator c)
      (pdata scheduleHash)
      (pvalueAndMint'outputCursor c)
      (pvalueAndMint'outputAssetCursor c)
      (pvalueAndMint'mintCursor c)
      (pvalueAndMint'valueAccumulator c)

-- | Aiken @value_and_mint_successor_is_exact@.
pvalueAndMintSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValueAndMintControlV1 :--> PBool
    )
pvalueAndMintSuccessorIsExact = phoistAcyclic $ plam $ \pre witness nextControl ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch (pfromData $ poneStep'claimedSuccessor stepWitness) $ \postState ->
    pand'List
      [ pfromData (pmachineState'phase postState) #== pcon PValueAndMint
      , pfromData (pmachineState'workRoot postState)
          #== phashWorkWitness
            # pcon PValueAndMint
            # (pfromData (pmachineState'programCounter preState) + 1)
            # (pencodeValueAndMintControlV1 # nextControl)
      ]

-- | Aiken @value_and_mint_stage_zero@.
pvalueAndMintStageZero :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PValueAndMintControlV1
        :--> PBool
    )
pvalueAndMintStageZero = phoistAcyclic $ plam $ \pre witness auxiliary control ->
  pmatch control $ \c ->
  plet (pfromData $ pvalueAndMint'nativeControl c) $ \nativeControl ->
  pmatch nativeControl $ \native ->
  plet (pfromData $ pnativeControl'resolutionScheduleHash native) $ \scheduleHash ->
    pand'List
      [ pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False
      , pfromData (pvalueAndMint'replayScheduleHash c) #== pemptyResolutionScheduleHash
      , pfromData (pvalueAndMint'replayCursor c) #== 0
      , pfromData (pvalueAndMint'replayAssetCursor c) #== 0
      , pfromData (pvalueAndMint'replayValueHash c)
          #== preplicateBS # 32 # (pintegerToByte # 0)
      , pfromData (pvalueAndMint'replayAccumulator c) #== pinitialResolutionAccumulator
      , pfromData (pvalueAndMint'replayRemainingScheduleHash c) #== pemptyResolutionScheduleHash
      , pfromData (pvalueAndMint'outputCursor c) #== 0
      , pfromData (pvalueAndMint'outputAssetCursor c) #== 0
      , pfromData (pvalueAndMint'mintCursor c) #== 0
      , pfromData (pvalueAndMint'valueAccumulator c) #== pinitialValueAccumulator
      , scheduleHash #/= pemptyResolutionScheduleHash
      , pvalueAndMintSuccessorIsExact
          # pre # witness # (pvalueAndMintStageZeroSuccessor # control # scheduleHash)
      ]

-- | Aiken @value_and_mint_stage_one@.
pvalueAndMintStageOne :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PValueAndMintControlV1
        :--> PBool
    )
pvalueAndMintStageOne = phoistAcyclic $ plam $ \pre witness auxiliary control ->
  pand'List
    [ pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False
    , pvalueAndMintSuccessorIsExact
        # pre # witness # (pvalueAndMintWithStage # control # 2)
    ]

-- | Aiken @complete_value_input_replay@.
pcompleteValueInputReplay :: forall s.
  Term s
    ( PValueAndMintControlV1 :--> PInteger :--> PByteString :--> PByteString
        :--> PByteString :--> PValueAccumulatorV1 :--> PValueAndMintControlV1
    )
pcompleteValueInputReplay = phoistAcyclic $ plam $ \control sourceKind key value nextScheduleHash nextValueAccumulator ->
  pmatch control $ \c ->
    pcon $ PValueAndMintControlV1
      (pvalueAndMint'nativeControl c)
      (pvalueAndMint'stage c)
      (pvalueAndMint'replayScheduleHash c)
      (pdata $ pfromData (pvalueAndMint'replayCursor c) + 1)
      (pdata 0)
      (pdata $ preplicateBS # 32 # (pintegerToByte # 0))
      ( pdata
          $ presolvedInputAccumulatorSuccessor
          # pfromData (pvalueAndMint'replayAccumulator c)
          # sourceKind # key # value
      )
      (pdata nextScheduleHash)
      (pvalueAndMint'outputCursor c)
      (pvalueAndMint'outputAssetCursor c)
      (pvalueAndMint'mintCursor c)
      (pdata nextValueAccumulator)

pvalueAccumulatorAddLovelace :: forall s.
  Term s (PValueAccumulatorV1 :--> PInteger :--> PValueAccumulatorV1)
pvalueAccumulatorAddLovelace = phoistAcyclic $ plam $ \accumulator lovelace ->
  pmatch accumulator $ \a ->
    pcon $ PValueAccumulatorV1
      (pdata $ pfromData (pvalueAccumulator'lovelaceDelta a) + lovelace)
      (pvalueAccumulator'assetRoot a)
      (pvalueAccumulator'seenAssetCount a)
      (pvalueAccumulator'nonzeroAssetCount a)

pvalueAndMintBeginInputAssets :: forall s.
  Term s
    ( PValueAndMintControlV1 :--> PByteString :--> PValueAccumulatorV1
        :--> PValueAndMintControlV1
    )
pvalueAndMintBeginInputAssets = phoistAcyclic $ plam $ \control descriptorCbor nextValueAccumulator ->
  pmatch control $ \c ->
    pcon $ PValueAndMintControlV1
      (pvalueAndMint'nativeControl c)
      (pvalueAndMint'stage c)
      (pvalueAndMint'replayScheduleHash c)
      (pvalueAndMint'replayCursor c)
      (pdata 1)
      (pdata $ pblake2b_256 # descriptorCbor)
      (pvalueAndMint'replayAccumulator c)
      (pvalueAndMint'replayRemainingScheduleHash c)
      (pvalueAndMint'outputCursor c)
      (pvalueAndMint'outputAssetCursor c)
      (pvalueAndMint'mintCursor c)
      (pdata nextValueAccumulator)

-- | The completed-schedule branch of Aiken @value_and_mint_stage_two@.
pvalueAndMintStageTwoFinish :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PValueAndMintControlV1
        :--> PBool
    )
pvalueAndMintStageTwoFinish = phoistAcyclic $ plam $ \pre witness auxiliary control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native ->
    pand'List
      [ pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False
      , pfromData (pvalueAndMint'replayRemainingScheduleHash c) #== pemptyResolutionScheduleHash
      , pfromData (pvalueAndMint'replayCursor c) #== pfromData (pnativeControl'resolvedInputCount native)
      , pfromData (pvalueAndMint'replayAssetCursor c) #== 0
      , pfromData (pvalueAndMint'replayValueHash c)
          #== preplicateBS # 32 # (pintegerToByte # 0)
      , pfromData (pvalueAndMint'replayAccumulator c)
          #== pfromData (pnativeControl'resolvedInputsAccumulator native)
      , pvalueAndMintSuccessorIsExact
          # pre # witness # (pvalueAndMintWithStage # control # 3)
      ]

-- | The descriptor-opening branch of Aiken @value_and_mint_stage_two@ after
-- unpacking its @ResolvedInputReplayWitness@.
pvalueAndMintStageTwoReplay :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValueAndMintControlV1 :--> PInteger :--> PByteString
        :--> PByteString :--> PByteString :--> PBool
    )
pvalueAndMintStageTwoReplay = phoistAcyclic $ plam $ \pre witness control sourceKind key nextScheduleHash value ->
  pmatch control $ \c ->
  pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native ->
  pif
    ( pand'List
        [ sourceKind #== 0 #|| sourceKind #== 1
        , presolutionScheduleNodeHash # sourceKind # key # nextScheduleHash
            #== pfromData (pvalueAndMint'replayRemainingScheduleHash c)
        , pfromData (pvalueAndMint'replayCursor c)
            #< pfromData (pnativeControl'resolvedInputCount native)
        ]
    )
    ( plet (OutputCommitment.pdecodeLedgerOutputCommitment # value) $ \descriptor ->
      pmatch descriptor $ \d ->
      plet
        ( pif
            (sourceKind #== 0)
            ( pvalueAccumulatorAddLovelace
                # pfromData (pvalueAndMint'valueAccumulator c)
                # pfromData (OutputCommitment.poutputCommitment'lovelace d)
            )
            (pfromData $ pvalueAndMint'valueAccumulator c)
        )
        $ \nextValueAccumulator ->
      plet
        ( pif
            ( sourceKind #== 1
                #|| pfromData (OutputCommitment.poutputCommitment'assetCount d) #== 0
            )
            ( pcompleteValueInputReplay
                # control # sourceKind # key # value # nextScheduleHash
                # nextValueAccumulator
            )
            (pvalueAndMintBeginInputAssets # control # value # nextValueAccumulator)
        )
        $ \nextControl ->
          pvalueAndMintSuccessorIsExact # pre # witness # nextControl
    )
    (pconstant False)

pvalueAndMintAdvanceInputAsset :: forall s.
  Term s
    ( PValueAndMintControlV1 :--> PValueAccumulatorV1
        :--> PValueAndMintControlV1
    )
pvalueAndMintAdvanceInputAsset = phoistAcyclic $ plam $ \control nextValueAccumulator ->
  pmatch control $ \c ->
    pcon $ PValueAndMintControlV1
      (pvalueAndMint'nativeControl c)
      (pvalueAndMint'stage c)
      (pvalueAndMint'replayScheduleHash c)
      (pvalueAndMint'replayCursor c)
      (pdata $ pfromData (pvalueAndMint'replayAssetCursor c) + 1)
      (pvalueAndMint'replayValueHash c)
      (pvalueAndMint'replayAccumulator c)
      (pvalueAndMint'replayRemainingScheduleHash c)
      (pvalueAndMint'outputCursor c)
      (pvalueAndMint'outputAssetCursor c)
      (pvalueAndMint'mintCursor c)
      (pdata nextValueAccumulator)

-- | The active-asset branch of Aiken @value_and_mint_stage_two@ after
-- unpacking its @ValueInputAssetWitness@.
pvalueAndMintStageTwoAsset :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValueAndMintControlV1 :--> PInteger :--> PByteString
        :--> PByteString :--> PByteString :--> PInteger :--> PByteString
        :--> PByteString :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PBuiltinList (PAsData PByteString)
        :--> PValueAssetMutationWitnessV1 :--> PBool
    )
pvalueAndMintStageTwoAsset = phoistAcyclic $ plam $ \pre witness control sourceKind key nextScheduleHash descriptorCbor assetIndex policyId assetName quantity assetPeaks assetSiblings mutation ->
  pmatch control $ \c ->
  pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native ->
  pif
    ( pand'List
        [ sourceKind #== 0
        , presolutionScheduleNodeHash # sourceKind # key # nextScheduleHash
            #== pfromData (pvalueAndMint'replayRemainingScheduleHash c)
        , pfromData (pvalueAndMint'replayCursor c)
            #< pfromData (pnativeControl'resolvedInputCount native)
        , pblake2b_256 # descriptorCbor #== pfromData (pvalueAndMint'replayValueHash c)
        , assetIndex #== pfromData (pvalueAndMint'replayAssetCursor c) - 1
        ]
    )
    ( plet (OutputCommitment.pdecodeLedgerOutputCommitment # descriptorCbor) $ \descriptor ->
      pif
        ( OutputCommitment.pverifyOutputAssetMembership
            # descriptor # assetIndex # policyId # assetName # quantity
            # assetPeaks # assetSiblings
        )
        ( pmatch
            ( papplyValueAssetMutation
                # pfromData (pvalueAndMint'valueAccumulator c)
                # (policyId <> assetName) # quantity # mutation
            )
            $ \case
              PValueAccumulatorAssetLimitExceeded ->
                pmatch witness $ \stepWitness ->
                  prejectedSuccessorIsExact
                    # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                    # pconstant "E_ASSET_COUNT"
              PValueAccumulatorMutationInvalid -> pconstant False
              PValueAccumulatorUpdated nextValueData ->
                plet (pfromData nextValueData) $ \nextValueAccumulator ->
                pmatch descriptor $ \d ->
                plet
                  ( pif
                      ( pfromData (pvalueAndMint'replayAssetCursor c)
                          #== pfromData (OutputCommitment.poutputCommitment'assetCount d)
                      )
                      ( pcompleteValueInputReplay
                          # control # sourceKind # key # descriptorCbor
                          # nextScheduleHash # nextValueAccumulator
                      )
                      (pvalueAndMintAdvanceInputAsset # control # nextValueAccumulator)
                  )
                  $ \nextControl ->
                    pvalueAndMintSuccessorIsExact # pre # witness # nextControl
        )
        (pconstant False)
    )
    (pconstant False)

-- | Aiken @value_and_mint_stage_two@ with the auxiliary sum intact.
pvalueAndMintStageTwo :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PValueAndMintControlV1
        :--> PBool
    )
pvalueAndMintStageTwo = phoistAcyclic $ plam $ \pre witness auxiliary control ->
  pmatch control $ \c ->
  pif
    (pfromData (pvalueAndMint'replayRemainingScheduleHash c) #== pemptyResolutionScheduleHash)
    (pvalueAndMintStageTwoFinish # pre # witness # auxiliary # control)
    ( pif
        (pfromData (pvalueAndMint'replayAssetCursor c) #== 0)
        ( pmatch auxiliary $ \case
            PResolvedInputReplayWitness sourceKind key nextScheduleHash value ->
              pvalueAndMintStageTwoReplay
                # pre # witness # control # pfromData sourceKind # pfromData key
                # pfromData nextScheduleHash # pfromData value
            _ -> perror
        )
        ( pmatch auxiliary $ \case
            PValueInputAssetWitness sourceKind key nextScheduleHash descriptorCbor assetIndex policyId assetName quantity assetPeaks assetSiblings mutation ->
              pvalueAndMintStageTwoAsset
                # pre # witness # control # pfromData sourceKind # pfromData key
                # pfromData nextScheduleHash # pfromData descriptorCbor
                # pfromData assetIndex # pfromData policyId # pfromData assetName
                # pfromData quantity # pfromData assetPeaks # pfromData assetSiblings
                # pfromData mutation
            _ -> perror
        )
    )

pvalueAndMintCompleteOutput :: forall s.
  Term s
    ( PValueAndMintControlV1 :--> PValueAccumulatorV1
        :--> PValueAndMintControlV1
    )
pvalueAndMintCompleteOutput = phoistAcyclic $ plam $ \control nextValueAccumulator ->
  pmatch control $ \c ->
    pcon $ PValueAndMintControlV1
      (pvalueAndMint'nativeControl c)
      (pvalueAndMint'stage c)
      (pvalueAndMint'replayScheduleHash c)
      (pvalueAndMint'replayCursor c)
      (pvalueAndMint'replayAssetCursor c)
      (pvalueAndMint'replayValueHash c)
      (pvalueAndMint'replayAccumulator c)
      (pvalueAndMint'replayRemainingScheduleHash c)
      (pdata $ pfromData (pvalueAndMint'outputCursor c) + 1)
      (pvalueAndMint'outputAssetCursor c)
      (pvalueAndMint'mintCursor c)
      (pdata nextValueAccumulator)

pvalueAndMintBeginOutputAssets :: forall s.
  Term s
    ( PValueAndMintControlV1 :--> PByteString :--> PValueAccumulatorV1
        :--> PValueAndMintControlV1
    )
pvalueAndMintBeginOutputAssets = phoistAcyclic $ plam $ \control descriptorCbor nextValueAccumulator ->
  pmatch control $ \c ->
    pcon $ PValueAndMintControlV1
      (pvalueAndMint'nativeControl c)
      (pvalueAndMint'stage c)
      (pvalueAndMint'replayScheduleHash c)
      (pvalueAndMint'replayCursor c)
      (pvalueAndMint'replayAssetCursor c)
      (pdata $ pblake2b_256 # descriptorCbor)
      (pvalueAndMint'replayAccumulator c)
      (pvalueAndMint'replayRemainingScheduleHash c)
      (pvalueAndMint'outputCursor c)
      (pdata 1)
      (pvalueAndMint'mintCursor c)
      (pdata nextValueAccumulator)

-- | The completed-output branch of Aiken @value_and_mint_stage_three@.
pvalueAndMintStageThreeFinish :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PValueAndMintControlV1
        :--> PBool
    )
pvalueAndMintStageThreeFinish = phoistAcyclic $ plam $ \pre witness auxiliary control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native ->
    pand'List
      [ pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False
      , pfromData (pvalueAndMint'outputCursor c) #== pfromData (pnativeControl'outputCount native)
      , pfromData (pvalueAndMint'outputAssetCursor c) #== 0
      , pfromData (pvalueAndMint'replayValueHash c)
          #== preplicateBS # 32 # (pintegerToByte # 0)
      , pvalueAndMintSuccessorIsExact
          # pre # witness # (pvalueAndMintWithStage # control # 4)
      ]

-- | The descriptor-opening branch of Aiken @value_and_mint_stage_three@ after
-- unpacking its @ValueOutputDescriptorWitness@.
pvalueAndMintStageThreeDescriptor :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValueAndMintControlV1 :--> PInteger :--> PByteString
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pvalueAndMintStageThreeDescriptor = phoistAcyclic $ plam $ \pre witness control outputIndex descriptorCbor siblings ->
  pmatch control $ \c ->
  pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native ->
  pif
    ( pand'List
        [ outputIndex #== pfromData (pvalueAndMint'outputCursor c)
        , pverifyMembership
            # pfromData (pnativeControl'outputCount native)
            # pfromData (pnativeControl'outputDescriptorPeaks native)
            # outputIndex
            # (ScriptProof.poutputDescriptorLeafHash # outputIndex # descriptorCbor)
            # siblings
        ]
    )
    ( plet (OutputCommitment.pdecodeLedgerOutputCommitment # descriptorCbor) $ \descriptor ->
      pmatch descriptor $ \d ->
      plet
        ( pvalueAccumulatorAddLovelace
            # pfromData (pvalueAndMint'valueAccumulator c)
            # (0 - pfromData (OutputCommitment.poutputCommitment'lovelace d))
        )
        $ \nextValueAccumulator ->
      plet
        ( pif
            (pfromData (OutputCommitment.poutputCommitment'assetCount d) #== 0)
            (pvalueAndMintCompleteOutput # control # nextValueAccumulator)
            (pvalueAndMintBeginOutputAssets # control # descriptorCbor # nextValueAccumulator)
        )
        $ \nextControl ->
          pvalueAndMintSuccessorIsExact # pre # witness # nextControl
    )
    (pconstant False)

pvalueAndMintAdvanceOutputAsset :: forall s.
  Term s
    ( PValueAndMintControlV1 :--> PValueAccumulatorV1
        :--> PValueAndMintControlV1
    )
pvalueAndMintAdvanceOutputAsset = phoistAcyclic $ plam $ \control nextValueAccumulator ->
  pmatch control $ \c ->
    pcon $ PValueAndMintControlV1
      (pvalueAndMint'nativeControl c)
      (pvalueAndMint'stage c)
      (pvalueAndMint'replayScheduleHash c)
      (pvalueAndMint'replayCursor c)
      (pvalueAndMint'replayAssetCursor c)
      (pvalueAndMint'replayValueHash c)
      (pvalueAndMint'replayAccumulator c)
      (pvalueAndMint'replayRemainingScheduleHash c)
      (pvalueAndMint'outputCursor c)
      (pdata $ pfromData (pvalueAndMint'outputAssetCursor c) + 1)
      (pvalueAndMint'mintCursor c)
      (pdata nextValueAccumulator)

pvalueAndMintCompleteOutputAsset :: forall s.
  Term s
    ( PValueAndMintControlV1 :--> PValueAccumulatorV1
        :--> PValueAndMintControlV1
    )
pvalueAndMintCompleteOutputAsset = phoistAcyclic $ plam $ \control nextValueAccumulator ->
  pmatch control $ \c ->
    pcon $ PValueAndMintControlV1
      (pvalueAndMint'nativeControl c)
      (pvalueAndMint'stage c)
      (pvalueAndMint'replayScheduleHash c)
      (pvalueAndMint'replayCursor c)
      (pvalueAndMint'replayAssetCursor c)
      (pdata $ preplicateBS # 32 # (pintegerToByte # 0))
      (pvalueAndMint'replayAccumulator c)
      (pvalueAndMint'replayRemainingScheduleHash c)
      (pdata $ pfromData (pvalueAndMint'outputCursor c) + 1)
      (pdata 0)
      (pvalueAndMint'mintCursor c)
      (pdata nextValueAccumulator)

-- | The active-asset branch of Aiken @value_and_mint_stage_three@ after
-- unpacking its @ValueOutputAssetWitness@.
pvalueAndMintStageThreeAsset :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValueAndMintControlV1 :--> PInteger :--> PByteString
        :--> PInteger :--> PByteString :--> PByteString :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PBuiltinList (PAsData PByteString)
        :--> PValueAssetMutationWitnessV1 :--> PBool
    )
pvalueAndMintStageThreeAsset = phoistAcyclic $ plam $ \pre witness control outputIndex descriptorCbor assetIndex policyId assetName quantity assetPeaks assetSiblings mutation ->
  pmatch control $ \c ->
  pif
    ( pand'List
        [ outputIndex #== pfromData (pvalueAndMint'outputCursor c)
        , assetIndex #== pfromData (pvalueAndMint'outputAssetCursor c) - 1
        , pblake2b_256 # descriptorCbor #== pfromData (pvalueAndMint'replayValueHash c)
        ]
    )
    ( plet (OutputCommitment.pdecodeLedgerOutputCommitment # descriptorCbor) $ \descriptor ->
      pif
        ( OutputCommitment.pverifyOutputAssetMembership
            # descriptor # assetIndex # policyId # assetName # quantity
            # assetPeaks # assetSiblings
        )
        ( pmatch
            ( papplyValueAssetMutation
                # pfromData (pvalueAndMint'valueAccumulator c)
                # (policyId <> assetName) # (0 - quantity) # mutation
            )
            $ \case
              PValueAccumulatorAssetLimitExceeded ->
                pmatch witness $ \stepWitness ->
                  prejectedSuccessorIsExact
                    # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                    # pconstant "E_ASSET_COUNT"
              PValueAccumulatorMutationInvalid -> pconstant False
              PValueAccumulatorUpdated nextValueData ->
                plet (pfromData nextValueData) $ \nextValueAccumulator ->
                pmatch descriptor $ \d ->
                plet
                  ( pif
                      ( pfromData (pvalueAndMint'outputAssetCursor c)
                          #== pfromData (OutputCommitment.poutputCommitment'assetCount d)
                      )
                      (pvalueAndMintCompleteOutputAsset # control # nextValueAccumulator)
                      (pvalueAndMintAdvanceOutputAsset # control # nextValueAccumulator)
                  )
                  $ \nextControl ->
                    pvalueAndMintSuccessorIsExact # pre # witness # nextControl
        )
        (pconstant False)
    )
    (pconstant False)

-- | Aiken @value_and_mint_stage_three@ with the auxiliary sum intact.
pvalueAndMintStageThree :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PValueAndMintControlV1
        :--> PBool
    )
pvalueAndMintStageThree = phoistAcyclic $ plam $ \pre witness auxiliary control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native ->
  pif
    (pfromData (pvalueAndMint'outputCursor c) #== pfromData (pnativeControl'outputCount native))
    (pvalueAndMintStageThreeFinish # pre # witness # auxiliary # control)
    ( pif
        (pfromData (pvalueAndMint'outputAssetCursor c) #== 0)
        ( pmatch auxiliary $ \case
            PValueOutputDescriptorWitness outputIndex descriptorCbor siblings ->
              pvalueAndMintStageThreeDescriptor
                # pre # witness # control # pfromData outputIndex
                # pfromData descriptorCbor # pfromData siblings
            _ -> perror
        )
        ( pmatch auxiliary $ \case
            PValueOutputAssetWitness outputIndex descriptorCbor assetIndex policyId assetName quantity assetPeaks assetSiblings mutation ->
              pvalueAndMintStageThreeAsset
                # pre # witness # control # pfromData outputIndex
                # pfromData descriptorCbor # pfromData assetIndex
                # pfromData policyId # pfromData assetName # pfromData quantity
                # pfromData assetPeaks # pfromData assetSiblings # pfromData mutation
            _ -> perror
        )
    )

pvalueAndMintAdvanceMint :: forall s.
  Term s
    ( PValueAndMintControlV1 :--> PValueAccumulatorV1
        :--> PValueAndMintControlV1
    )
pvalueAndMintAdvanceMint = phoistAcyclic $ plam $ \control nextValueAccumulator ->
  pmatch control $ \c ->
    pcon $ PValueAndMintControlV1
      (pvalueAndMint'nativeControl c)
      (pvalueAndMint'stage c)
      (pvalueAndMint'replayScheduleHash c)
      (pvalueAndMint'replayCursor c)
      (pvalueAndMint'replayAssetCursor c)
      (pvalueAndMint'replayValueHash c)
      (pvalueAndMint'replayAccumulator c)
      (pvalueAndMint'replayRemainingScheduleHash c)
      (pvalueAndMint'outputCursor c)
      (pvalueAndMint'outputAssetCursor c)
      (pdata $ pfromData (pvalueAndMint'mintCursor c) + 1)
      (pdata nextValueAccumulator)

-- | The completed-mint branch of Aiken @value_and_mint_stage_four@.
pvalueAndMintStageFourFinish :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PValueAndMintControlV1
        :--> PBool
    )
pvalueAndMintStageFourFinish = phoistAcyclic $ plam $ \pre witness auxiliary control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native ->
    pand'List
      [ pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False
      , pfromData (pvalueAndMint'mintCursor c) #== pfromData (pnativeControl'mintCount native)
      , pvalueAndMintSuccessorIsExact
          # pre # witness # (pvalueAndMintWithStage # control # 5)
      ]

-- | The active-asset branch of Aiken @value_and_mint_stage_four@ after
-- unpacking its @ValueMintAssetWitness@.
pvalueAndMintStageFourAsset :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValueAndMintControlV1 :--> PInteger :--> PByteString
        :--> PByteString :--> PInteger :--> PBuiltinList (PAsData PByteString)
        :--> PValueAssetMutationWitnessV1 :--> PBool
    )
pvalueAndMintStageFourAsset = phoistAcyclic $ plam $ \pre witness control mintIndex policyId assetName quantity siblings mutation ->
  pmatch control $ \c ->
  pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native ->
  pif
    ( pand'List
        [ mintIndex #== pfromData (pvalueAndMint'mintCursor c)
        , plengthBS # policyId #== 28
        , plengthBS # assetName #<= 32
        , quantity #/= 0
        , pverifyMembership
            # pfromData (pnativeControl'mintCount native)
            # pfromData (pnativeControl'mintPeaks native)
            # mintIndex # (pmintAssetLeafHash # policyId # assetName # quantity)
            # siblings
        ]
    )
    ( pmatch
        ( papplyValueAssetMutation
            # pfromData (pvalueAndMint'valueAccumulator c)
            # (policyId <> assetName) # quantity # mutation
        )
        $ \case
          PValueAccumulatorAssetLimitExceeded ->
            pmatch witness $ \stepWitness ->
              prejectedSuccessorIsExact
                # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                # pconstant "E_ASSET_COUNT"
          PValueAccumulatorMutationInvalid -> pconstant False
          PValueAccumulatorUpdated nextValueData ->
            pvalueAndMintSuccessorIsExact
              # pre # witness
              # (pvalueAndMintAdvanceMint # control # pfromData nextValueData)
    )
    (pconstant False)

-- | Aiken @value_and_mint_stage_four@ with the auxiliary sum intact.
pvalueAndMintStageFour :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PValueAndMintControlV1
        :--> PBool
    )
pvalueAndMintStageFour = phoistAcyclic $ plam $ \pre witness auxiliary control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native ->
  pif
    (pfromData (pvalueAndMint'mintCursor c) #== pfromData (pnativeControl'mintCount native))
    (pvalueAndMintStageFourFinish # pre # witness # auxiliary # control)
    ( pmatch auxiliary $ \case
        PValueMintAssetWitness mintIndex policyId assetName quantity siblings mutation ->
          pvalueAndMintStageFourAsset
            # pre # witness # control # pfromData mintIndex # pfromData policyId
            # pfromData assetName # pfromData quantity # pfromData siblings
            # pfromData mutation
        _ -> perror
    )

-- | Aiken @value_and_mint_stage_five@.
pvalueAndMintStageFive :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PValueAndMintControlV1
        :--> PNativeTxBodyCompact :--> PBool
    )
pvalueAndMintStageFive = phoistAcyclic $ plam $ \pre witness auxiliary control body ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch (pfromData $ poneStep'claimedSuccessor stepWitness) $ \postState ->
  pmatch control $ \c ->
  pmatch (pfromData $ pvalueAndMint'nativeControl c) $ \native ->
  pmatch (pfromData $ pvalueAndMint'valueAccumulator c) $ \accumulator ->
  pmatch body $ \txBody ->
    pand'List
      [ pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False
      , pfromData (pvalueAndMint'replayCursor c) #== pfromData (pnativeControl'resolvedInputCount native)
      , pfromData (pvalueAndMint'replayAssetCursor c) #== 0
      , pfromData (pvalueAndMint'replayValueHash c)
          #== preplicateBS # 32 # (pintegerToByte # 0)
      , pfromData (pvalueAndMint'replayAccumulator c)
          #== pfromData (pnativeControl'resolvedInputsAccumulator native)
      , pfromData (pvalueAndMint'replayRemainingScheduleHash c) #== pemptyResolutionScheduleHash
      , pfromData (pvalueAndMint'outputCursor c) #== pfromData (pnativeControl'outputCount native)
      , pfromData (pvalueAndMint'outputAssetCursor c) #== 0
      , pfromData (pvalueAndMint'mintCursor c) #== pfromData (pnativeControl'mintCount native)
      , pif
          ( pfromData (pvalueAccumulator'lovelaceDelta accumulator)
              - pbodyCompact'fee txBody #/= 0
              #|| pfromData (pvalueAccumulator'nonzeroAssetCount accumulator) #/= 0
          )
          ( prejectedSuccessorIsExact
              # pre # pfromData (poneStep'claimedSuccessor stepWitness)
              # pconstant "E_VALUE_NOT_PRESERVED"
          )
          ( pand'List
              [ pfromData (pmachineState'phase postState) #== pcon PLedgerDelta
              , pfromData (pmachineState'workRoot postState)
                  #== phashWorkWitness
                    # pcon PLedgerDelta
                    # (pfromData (pmachineState'programCounter preState) + 1)
                    # ( pencodeLedgerDeltaWitnessV1
                          # pfromData (pvalueAndMint'nativeControl c)
                          # pfromData (pmachineState'priorLedgerRoot preState)
                          # pfromData (pvalueAndMint'replayScheduleHash c)
                      )
              ]
          )
      ]

-- | Aiken @verify_value_and_mint@.
pverifyValueAndMint :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyValueAndMint = phoistAcyclic $ plam $ \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pvalueAndMintControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  plet (pfromData $ pvalueAndMint'nativeControl c) $ \nativeControl ->
  pmatch nativeControl $ \native ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pnativeControl'compactCbor native)
        # pfromData (pnativeControl'witnessSetCompactCbor native)
        # pfromData (pnativeControl'fieldPreimageLengthsCbor native)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  pmatch (pverified'txCompact verified) $ \compact ->
  plet (pfromData $ pvalueAndMint'valueAccumulator c) $ \accumulator ->
  pmatch accumulator $ \value ->
  plet (pfromData $ pvalueAndMint'stage c) $ \stage ->
    pand'List
      [ NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (pnativeControl'compactCbor native)
          # pfromData (pnativeControl'witnessSetCompactCbor native)
          # pfromData (pnativeControl'fieldPreimageLengthsCbor native)
          #== pfromData (pmachineState'transactionCommitment preState)
      , phashValidationContext # pfromData (pnativeControl'contextCbor native)
          #== pfromData (pmachineState'validationContextHash preState)
      , pnativeScriptsControlIsWellFormed # nativeControl
      , pfromData (pnativeControl'executionCursor native)
          #== pfromData (pnativeControl'executionCount native)
      , plengthBS # pfromData (pvalueAndMint'replayScheduleHash c) #== 32
      , plengthBS # pfromData (pvalueAndMint'replayValueHash c) #== 32
      , plengthBS # pfromData (pvalueAndMint'replayAccumulator c) #== 32
      , plengthBS # pfromData (pvalueAndMint'replayRemainingScheduleHash c) #== 32
      , pfromData (pvalueAndMint'replayCursor c) #>= 0
      , pfromData (pvalueAndMint'replayCursor c)
          #<= pfromData (pnativeControl'resolvedInputCount native)
      , pfromData (pvalueAndMint'replayAssetCursor c) #>= 0
      , pfromData (pvalueAndMint'replayAssetCursor c) #<= LedgerOutput.pmaxDistinctAssetCount
      , pif
          ( stage #== 2 #&& pfromData (pvalueAndMint'replayAssetCursor c) #> 0
              #|| stage #== 3 #&& pfromData (pvalueAndMint'outputAssetCursor c) #> 0
          )
          (pconstant True)
          ( pfromData (pvalueAndMint'replayValueHash c)
              #== preplicateBS # 32 # (pintegerToByte # 0)
          )
      , pfromData (pvalueAndMint'outputCursor c) #>= 0
      , pfromData (pvalueAndMint'outputCursor c)
          #<= pfromData (pnativeControl'outputCount native)
      , pfromData (pvalueAndMint'outputAssetCursor c) #>= 0
      , pfromData (pvalueAndMint'outputAssetCursor c) #<= LedgerOutput.pmaxDistinctAssetCount
      , plengthBS # pfromData (pvalueAccumulator'assetRoot value) #== 32
      , pfromData (pvalueAccumulator'seenAssetCount value) #>= 0
      , pfromData (pvalueAccumulator'seenAssetCount value) #<= LedgerOutput.pmaxDistinctAssetCount
      , pfromData (pvalueAccumulator'nonzeroAssetCount value) #>= 0
      , pfromData (pvalueAccumulator'nonzeroAssetCount value)
          #<= pfromData (pvalueAccumulator'seenAssetCount value)
      , pfromData (pvalueAndMint'mintCursor c) #>= 0
      , pfromData (pvalueAndMint'mintCursor c) #<= pfromData (pnativeControl'mintCount native)
      , stage #>= 0
      , stage #<= 5
      , pfromData (poneStep'workWitnessCbor stepWitness)
          #== pencodeValueAndMintControlV1 # control
      , pif (stage #== 0)
          (pvalueAndMintStageZero # pre # witness # auxiliary # control) $
        pif (stage #== 1)
          (pvalueAndMintStageOne # pre # witness # auxiliary # control) $
        pif (stage #== 2)
          (pvalueAndMintStageTwo # pre # witness # auxiliary # control) $
        pif (stage #== 3)
          (pvalueAndMintStageThree # pre # witness # auxiliary # control) $
        pif (stage #== 4)
          (pvalueAndMintStageFour # pre # witness # auxiliary # control)
          ( pvalueAndMintStageFive
              # pre # witness # auxiliary # control # pcompact'body compact
          )
      ]

-- | Aiken @verify_value_and_mint_one_step_v1@.
pverifyValueAndMintOneStepV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyValueAndMintOneStepV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch pre $ \preState ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transition auxiliary) ->
  plet (pfromData transition) $ \witness ->
    pand'List
      [ pfromData (pmachineState'phase preState) #== pcon PValueAndMint
      , pstructuralTransitionIsValid # pre # witness
      , pverifyValueAndMint # pre # witness # pdecodeValidationAuxiliaryWitnessV1 auxiliary
      ]

-- | Aiken @verify_compact_binding@.
pverifyCompactBinding :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyCompactBinding = phoistAcyclic $ plam $ \pre witness ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch (pdeserialise # pfromData (poneStep'workWitnessCbor stepWitness)) $ \case
    PNothing -> perror
    PJust dat ->
      plet (pasList # dat) $ \items ->
      pif (plength # items #== 6)
        ( plet (pasByteStr # (pelemAt # 0 # items)) $ \transactionId ->
          plet (pasByteStr # (pelemAt # 1 # items)) $ \commitment ->
          plet (pasByteStr # (pelemAt # 2 # items)) $ \compactCbor ->
          plet (pasByteStr # (pelemAt # 3 # items)) $ \witnessSetCompactCbor ->
          plet (pasByteStr # (pelemAt # 4 # items)) $ \fieldPreimageLengthsCbor ->
          plet (pasByteStr # (pelemAt # 5 # items)) $ \contextCbor ->
          pmatch
            ( NativeCompact.pverifyNativeTxProofSourceV1
                # transactionId # compactCbor # witnessSetCompactCbor
                # fieldPreimageLengthsCbor
            )
            $ \(PPair verifiedSource _) ->
          pmatch verifiedSource $ \verified ->
          pmatch (pfromData $ poneStep'claimedSuccessor stepWitness) $ \post ->
            pand'List
              [ pverified'version verified #== 1
              , transactionId #== pfromData (pmachineState'transactionId preState)
              , commitment #== pfromData (pmachineState'transactionCommitment preState)
              , commitment
                  #== NativeCompact.pnativeTxProofCommitmentV1
                    # compactCbor # witnessSetCompactCbor # fieldPreimageLengthsCbor
              , phashValidationContext # contextCbor
                  #== pfromData (pmachineState'validationContextHash preState)
              , pfromData (poneStep'workWitnessCbor stepWitness)
                  #== pencodeCompactBindingWitness
                    # transactionId # commitment # compactCbor # witnessSetCompactCbor
                    # fieldPreimageLengthsCbor # contextCbor
              , pfromData (pmachineState'phase post) #== pcon PStaticLedgerRules
              , pfromData (pmachineState'workRoot post)
                  #== phashWorkWitness
                    # pcon PStaticLedgerRules
                    # (pfromData (pmachineState'programCounter preState) + 1)
                    # ( pencodeStaticRulesWitness
                          # compactCbor # witnessSetCompactCbor
                          # fieldPreimageLengthsCbor # contextCbor
                      )
              ]
        )
        perror

-- | Aiken @verify_compact_binding_one_step_v1@.
pverifyCompactBindingOneStepV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyCompactBindingOneStepV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch pre $ \preState ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transition auxiliary) ->
  plet (pfromData transition) $ \witness ->
  pif (pfromData (pmachineState'phase preState) #== pcon PCompactBinding)
    ( pif (pstructuralTransitionIsValid # pre # witness)
        ( pif
            (pmatch (pdecodeValidationAuxiliaryWitnessV1 auxiliary) $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False)
            (pverifyCompactBinding # pre # witness)
            (pconstant False)
        )
        (pconstant False)
    )
    (pconstant False)

-- | Aiken @verify_compact_binding_semantics_v1@.
pverifyCompactBindingSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyCompactBindingSemanticsV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transition auxiliary) ->
  pif
    (pmatch (pdecodeValidationAuxiliaryWitnessV1 auxiliary) $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False)
    (pverifyCompactBinding # pre # pfromData transition)
    (pconstant False)

pstaticRulesRejection :: forall s.
  Term s
    ( PNativeTxBodyCompact :--> PInteger :--> PValidationContextV1
        :--> PMaybe PByteString
    )
pstaticRulesRejection = phoistAcyclic $ plam $ \body canonicalTxSize context ->
  pmatch body $ \txBody ->
  pmatch context $ \ctx ->
  pif
    ( pbodyCompact'networkId txBody #/= 255
        #&& pbodyCompact'networkId txBody #/= pfromData (pvalidationContext'expectedNetworkId ctx)
    )
    (pcon $ PJust $ pconstant "E_NETWORK_ID_MISMATCH")
    ( pif
        ( pbodyCompact'fee txBody
            #< pfromData (pvalidationContext'minFeeA ctx) * canonicalTxSize
              + pfromData (pvalidationContext'minFeeB ctx)
        )
        (pcon $ PJust $ pconstant "E_MIN_FEE")
        (pcon PNothing)
    )

-- | Aiken @verify_static_rules@.
pverifyStaticRules :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyStaticRules = phoistAcyclic $ plam $ \pre witness ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch (pdeserialise # pfromData (poneStep'workWitnessCbor stepWitness)) $ \case
    PNothing -> perror
    PJust dat ->
      plet (pasList # dat) $ \items ->
      pif (plength # items #== 4)
        ( plet (pasByteStr # (pelemAt # 0 # items)) $ \compactCbor ->
          plet (pasByteStr # (pelemAt # 1 # items)) $ \witnessSetCompactCbor ->
          plet (pasByteStr # (pelemAt # 2 # items)) $ \fieldPreimageLengthsCbor ->
          plet (pasByteStr # (pelemAt # 3 # items)) $ \contextCbor ->
          pmatch
            ( NativeCompact.pverifyNativeTxProofSourceV1
                # pfromData (pmachineState'transactionId preState)
                # compactCbor # witnessSetCompactCbor # fieldPreimageLengthsCbor
            )
            $ \(PPair verifiedSource _) ->
          pmatch verifiedSource $ \verified ->
          plet
            (NativeCompact.pdecodeNativeTxFieldPreimageLengthsV1 # fieldPreimageLengthsCbor)
            $ \fieldLengths ->
          plet (pdecodeValidationContext # contextCbor) $ \context ->
          plet (pverified'txCompact verified) $ \compact ->
          pmatch compact $ \txCompact ->
          pif
            ( pand'List
                [ NativeCompact.pnativeTxProofCommitmentV1
                    # compactCbor # witnessSetCompactCbor # fieldPreimageLengthsCbor
                    #== pfromData (pmachineState'transactionCommitment preState)
                , phashValidationContext # contextCbor
                    #== pfromData (pmachineState'validationContextHash preState)
                , pfromData (poneStep'workWitnessCbor stepWitness)
                    #== pencodeStaticRulesWitness
                      # compactCbor # witnessSetCompactCbor
                      # fieldPreimageLengthsCbor # contextCbor
                ]
            )
            ( pmatch
                ( pstaticRulesRejection
                    # pcompact'body txCompact
                    # (NativeCompact.pnativeTxCanonicalSizeV1 # compact # fieldLengths)
                    # context
                )
                $ \case
                  PJust rejectionCode ->
                    prejectedSuccessorIsExact
                      # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                      # rejectionCode
                  PNothing ->
                    pmatch (pcompact'body txCompact) $ \body ->
                    plet
                      ( pif (pbodyCompact'spendInputsHash body #== NativeField.pemptyFieldCommitment)
                          0 (-1)
                      )
                      $ \spendCount ->
                    plet
                      ( pif (pbodyCompact'referenceInputsHash body #== NativeField.pemptyFieldCommitment)
                          0 (-1)
                      )
                      $ \referenceCount ->
                    pmatch (pfromData $ poneStep'claimedSuccessor stepWitness) $ \post ->
                      pfromData (pmachineState'phase post) #== pcon PInputSets
                        #&& pfromData (pmachineState'workRoot post)
                          #== phashWorkWitness
                            # pcon PInputSets
                            # (pfromData (pmachineState'programCounter preState) + 1)
                            # ( pencodeInputSetsScanWitness
                                  # compactCbor # witnessSetCompactCbor
                                  # fieldPreimageLengthsCbor # contextCbor
                                  # spendCount # referenceCount # 0 # 0
                                  # pconstant "" # pemptyResolutionScheduleHash
                              )
            )
            (pconstant False)
        )
        perror

pverifyStaticLedgerRulesOneStepV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyStaticLedgerRulesOneStepV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch pre $ \preState ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transition auxiliary) ->
  plet (pfromData transition) $ \witness ->
  pif (pfromData (pmachineState'phase preState) #== pcon PStaticLedgerRules)
    ( pif (pstructuralTransitionIsValid # pre # witness)
        ( pif
            (pmatch (pdecodeValidationAuxiliaryWitnessV1 auxiliary) $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False)
            (pverifyStaticRules # pre # witness)
            (pconstant False)
        )
        (pconstant False)
    )
    (pconstant False)

pverifyStaticLedgerRulesSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyStaticLedgerRulesSemanticsV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transition auxiliary) ->
  pif
    (pmatch (pdecodeValidationAuxiliaryWitnessV1 auxiliary) $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False)
    (pverifyStaticRules # pre # pfromData transition)
    (pconstant False)

ptransactionFieldCommitment :: forall s.
  Term s
    ( PNativeTxCompact :--> PNativeTxWitnessSetCompact
        :--> PInteger :--> PByteString
    )
ptransactionFieldCommitment = phoistAcyclic $ plam $ \compact witnessSet fieldIndex ->
  pmatch compact $ \txCompact ->
  pmatch (pcompact'body txCompact) $ \body ->
  pmatch witnessSet $ \ws ->
  pif (fieldIndex #== 0) (pbodyCompact'spendInputsHash body) $
  pif (fieldIndex #== 1) (pbodyCompact'referenceInputsHash body) $
  pif (fieldIndex #== 2) (pbodyCompact'outputsHash body) $
  pif (fieldIndex #== 3) (pbodyCompact'requiredObserversHash body) $
  pif (fieldIndex #== 4) (pbodyCompact'requiredSignersHash body) $
  pif (fieldIndex #== 5) (pbodyCompact'mintHash body) $
  pif (fieldIndex #== 6) (pfromData $ pwitnessSetCompact'scriptTxWitsHash ws) $
  pif (fieldIndex #== 7) (pfromData $ pwitnessSetCompact'addrTxWitsHash ws) $
  pif (fieldIndex #== 8) (pfromData $ pwitnessSetCompact'redeemerTxWitsHash ws) perror

pinputSetsControlIsBound :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInputSetsControlV1 :--> PVerifiedMidgardNativeTxCompact :--> PBool
    )
pinputSetsControlIsBound = phoistAcyclic $ plam $ \pre witness control verified ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch verified $ \verifiedSource ->
  pmatch (pverified'txCompact verifiedSource) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
  plet (pbodyCompact'spendInputsHash body #== NativeField.pemptyFieldCommitment) $ \spendIsEmpty ->
  plet (pbodyCompact'referenceInputsHash body #== NativeField.pemptyFieldCommitment) $ \referenceIsEmpty ->
  plet (pfromData (pinputSets'spendSeen c) + pfromData (pinputSets'referenceSeen c)) $ \seen ->
  plet
    ( pand'List
        [ pverified'version verifiedSource #== 1
        , NativeCompact.pnativeTxProofCommitmentV1
            # pfromData (pinputSets'compactCbor c)
            # pfromData (pinputSets'witnessSetCompactCbor c)
            # pfromData (pinputSets'fieldPreimageLengthsCbor c)
            #== pfromData (pmachineState'transactionCommitment preState)
        , phashValidationContext # pfromData (pinputSets'contextCbor c)
            #== pfromData (pmachineState'validationContextHash preState)
        , pfromData (pinputSets'spendCount c) #>= (-1)
        , pfromData (pinputSets'spendCount c) #<= pmaxTxSizeDerivedItemCount
        , pfromData (pinputSets'referenceCount c) #>= (-1)
        , pfromData (pinputSets'referenceCount c) #<= pmaxTxSizeDerivedItemCount
        , pfromData (pinputSets'spendSeen c) #>= 0
        , pfromData (pinputSets'referenceSeen c) #>= 0
        , pif spendIsEmpty
            (pfromData (pinputSets'spendCount c) #== 0 #&& pfromData (pinputSets'spendSeen c) #== 0)
            ( pif (pfromData (pinputSets'spendCount c) #== (-1))
                (pfromData (pinputSets'spendSeen c) #== 0)
                ( pfromData (pinputSets'spendCount c) #> 0
                    #&& pfromData (pinputSets'spendSeen c) #<= pfromData (pinputSets'spendCount c)
                )
            )
        , pif referenceIsEmpty
            (pfromData (pinputSets'referenceCount c) #== 0 #&& pfromData (pinputSets'referenceSeen c) #== 0)
            ( pif (pfromData (pinputSets'referenceCount c) #== (-1))
                (pfromData (pinputSets'referenceSeen c) #== 0)
                ( pfromData (pinputSets'referenceCount c) #> 0
                    #&& pfromData (pinputSets'referenceSeen c) #<= pfromData (pinputSets'referenceCount c)
                )
            )
        , pif (seen #== 0)
            ( pfromData (pinputSets'previousKey c) #== pconstant ""
                #&& pfromData (pinputSets'resolutionScheduleHash c) #== pemptyResolutionScheduleHash
            )
            (plengthBS # pfromData (pinputSets'previousKey c) #== 38)
        , plengthBS # pfromData (pinputSets'resolutionScheduleHash c) #== 32
        ]
    )
    $ \shapeIsValid ->
      pif shapeIsValid
        ( pfromData (poneStep'workWitnessCbor stepWitness)
            #== pencodeInputSetsScanWitness
              # pfromData (pinputSets'compactCbor c)
              # pfromData (pinputSets'witnessSetCompactCbor c)
              # pfromData (pinputSets'fieldPreimageLengthsCbor c)
              # pfromData (pinputSets'contextCbor c)
              # pfromData (pinputSets'spendCount c)
              # pfromData (pinputSets'referenceCount c)
              # pfromData (pinputSets'spendSeen c)
              # pfromData (pinputSets'referenceSeen c)
              # pfromData (pinputSets'previousKey c)
              # pfromData (pinputSets'resolutionScheduleHash c)
        )
        (pconstant False)

pverifyInputSetsEmpty :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInputSetsControlV1 :--> PBool
    )
pverifyInputSetsEmpty = phoistAcyclic $ plam $ \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pinputSets'compactCbor c)
        # pfromData (pinputSets'witnessSetCompactCbor c)
        # pfromData (pinputSets'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified _) ->
    pand'List
      [ pinputSetsControlIsBound # pre # witness # control # verified
      , pfromData (pinputSets'spendCount c) #== 0
      , pfromData (pinputSets'spendSeen c) #== 0
      , pfromData (pinputSets'referenceSeen c) #== 0
      , prejectedSuccessorIsExact
          # pre # pfromData (poneStep'claimedSuccessor stepWitness)
          # pconstant "E_EMPTY_INPUTS"
      ]

pinputSetsItemSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationMachineStateV1
        :--> PInputSetsControlV1 :--> PNativeTxBodyCompact
        :--> PNativeTxWitnessSetCompact
        :--> PInteger :--> PInteger :--> PInteger :--> PInteger
        :--> PByteString :--> PByteString :--> PBool
    )
pinputSetsItemSuccessorIsExact = phoistAcyclic $ plam $ \pre post control body witnessSet spendCount referenceCount spendSeen referenceSeen key scheduleHash ->
  pmatch pre $ \preState ->
  pmatch post $ \postState ->
  pmatch control $ \c ->
  pmatch body $ \txBody ->
  pmatch witnessSet $ \ws ->
  plet
    ( spendCount #> 0 #&& referenceCount #>= 0
        #&& spendSeen #== spendCount #&& referenceSeen #== referenceCount
    )
    $ \complete ->
  pif complete
    ( pif (pvalidityIntervalIsMalformed # pbodyCompact'validityIntervalStart txBody # pbodyCompact'validityIntervalEnd txBody)
        (prejectedSuccessorIsExact # pre # post # pconstant "E_INVALID_VALIDITY_INTERVAL_FORMAT")
        ( plet
            ( pif (pfromData (pwitnessSetCompact'addrTxWitsHash ws) #== NativeField.pemptyFieldCommitment)
                0 (-1)
            )
            $ \addressCount ->
          plet
            ( pif (pbodyCompact'requiredSignersHash txBody #== NativeField.pemptyFieldCommitment)
                0 (-1)
            )
            $ \requiredCount ->
            pfromData (pmachineState'phase postState) #== pcon PSignatures
              #&& pfromData (pmachineState'workRoot postState)
                #== phashWorkWitness
                  # pcon PSignatures
                  # (pfromData (pmachineState'programCounter preState) + 1)
                  # ( pencodeSignaturesScanWitness
                        # pfromData (pinputSets'compactCbor c)
                        # pfromData (pinputSets'witnessSetCompactCbor c)
                        # pfromData (pinputSets'fieldPreimageLengthsCbor c)
                        # pfromData (pinputSets'contextCbor c)
                        # scheduleHash # 0 # addressCount # requiredCount
                        # 0 # 0 # pconstant "" # pconstant "" # 0 # pnil # 0
                    )
        )
    )
    ( pfromData (pmachineState'phase postState) #== pcon PInputSets
        #&& pfromData (pmachineState'workRoot postState)
          #== phashWorkWitness
            # pcon PInputSets
            # (pfromData (pmachineState'programCounter preState) + 1)
            # ( pencodeInputSetsScanWitness
                  # pfromData (pinputSets'compactCbor c)
                  # pfromData (pinputSets'witnessSetCompactCbor c)
                  # pfromData (pinputSets'fieldPreimageLengthsCbor c)
                  # pfromData (pinputSets'contextCbor c)
                  # spendCount # referenceCount # spendSeen # referenceSeen
                  # key # scheduleHash
              )
    )

pverifyInputSetsItem :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInputSetsControlV1 :--> PItemProofV1 :--> BoundedItem.PChunkProofV1
        :--> PBool
    )
pverifyInputSetsItem = phoistAcyclic $ plam $ \pre witness control collectionProof chunkProof ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch collectionProof $ \item ->
  pmatch chunkProof $ \chunk ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pinputSets'compactCbor c)
        # pfromData (pinputSets'witnessSetCompactCbor c)
        # pfromData (pinputSets'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified witnessSet) ->
  pmatch verified $ \verifiedSource ->
  pmatch (pverified'txCompact verifiedSource) $ \compact ->
  plet (pfromData $ pitemProof'fieldIndex item) $ \fieldIndex ->
  plet
    (ptransactionFieldCommitment # pverified'txCompact verifiedSource # witnessSet # fieldIndex)
    $ \expectedFieldCommitment ->
  plet (pif (fieldIndex #== 0) (pfromData $ pinputSets'spendCount c) (pfromData $ pinputSets'referenceCount c)) $ \priorCount ->
  plet (pif (priorCount #== (-1)) (pfromData $ pitemProof'itemCount item) priorCount) $ \activeCount ->
  plet (pif (fieldIndex #== 0) (pfromData $ pinputSets'spendSeen c) (pfromData $ pinputSets'referenceSeen c)) $ \priorSeen ->
  plet (pfromData $ BoundedItem.pchunkProof'chunk chunk) $ \key ->
  plet (pdecodeMidgardTxInputCbor # key) $ \input ->
  plet (presolutionScheduleNodeHash # fieldIndex # key # pfromData (pinputSets'resolutionScheduleHash c)) $ \nextScheduleHash ->
  plet (pif (fieldIndex #== 0) activeCount (pfromData $ pinputSets'spendCount c)) $ \nextSpendCount ->
  plet (pif (fieldIndex #== 1) activeCount (pfromData $ pinputSets'referenceCount c)) $ \nextReferenceCount ->
  plet (pif (fieldIndex #== 0) (pfromData (pinputSets'spendSeen c) + 1) (pfromData $ pinputSets'spendSeen c)) $ \nextSpendSeen ->
  plet (pif (fieldIndex #== 1) (pfromData (pinputSets'referenceSeen c) + 1) (pfromData $ pinputSets'referenceSeen c)) $ \nextReferenceSeen ->
  plet (plengthBS # pfromData (pinputSets'previousKey c) #> 0 #&& key #== pfromData (pinputSets'previousKey c)) $ \duplicate ->
  plet (pfromData (pinputSets'previousKey c) #== pconstant "" #|| key #< pfromData (pinputSets'previousKey c)) $ \correctlyOrdered ->
  pif
    ( pand'List
        [ pinputSetsControlIsBound # pre # witness # control # verified
        , pfromData (pinputSets'spendCount c) #/= 0
        , fieldIndex #== 0 #|| fieldIndex #== 1
        , priorCount #== (-1) #|| priorCount #== pfromData (pitemProof'itemCount item)
        , activeCount #> 0
        , activeCount #<= pmaxTxSizeDerivedItemCount
        , priorSeen #< activeCount
        , pfromData (pitemProof'itemCount item) #== activeCount
        , pfromData (pitemProof'itemIndex item) #>= 0
        , pfromData (pitemProof'itemIndex item) #< activeCount
        , pfromData (pitemProof'itemLength item) #== pfromData (BoundedItem.pchunkProof'totalLength chunk)
        , pfromData (BoundedItem.pchunkProof'fieldIndex chunk) #== fieldIndex
        , pfromData (BoundedItem.pchunkProof'itemIndex chunk) #== pfromData (pitemProof'itemIndex item)
        , pfromData (BoundedItem.pchunkProof'chunkIndex chunk) #== 0
        , BoundedItem.pchunkCount # pfromData (BoundedItem.pchunkProof'totalLength chunk) #== 1
        , pencodeMidgardTxInput # input #== key
        , pverifyBoundedCollectionItem # expectedFieldCommitment # collectionProof
        , BoundedItem.pverifyChunk # pfromData (pitemProof'itemCommitment item) # chunkProof
        ]
    )
    ( pif duplicate
        ( prejectedSuccessorIsExact
            # pre # pfromData (poneStep'claimedSuccessor stepWitness)
            # pconstant "E_DUPLICATE_INPUT_IN_TX"
        )
        ( correctlyOrdered
            #&& pinputSetsItemSuccessorIsExact
              # pre # pfromData (poneStep'claimedSuccessor stepWitness) # control
              # pcompact'body compact # witnessSet
              # nextSpendCount # nextReferenceCount # nextSpendSeen # nextReferenceSeen
              # key # nextScheduleHash
        )
    )
    (pconstant False)

pverifyInputSetsEmptySemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyInputSetsEmptySemanticsV1 = phoistAcyclic $ plam $ \pre witness ->
  pmatch witness $ \stepWitness ->
    pverifyInputSetsEmpty # pre # witness
      # (pinputSetsControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))

pverifyInputSetsItemSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PItemProofV1 :--> BoundedItem.PChunkProofV1 :--> PBool
    )
pverifyInputSetsItemSemanticsV1 = phoistAcyclic $ plam $ \pre witness collectionProof chunkProof ->
  pmatch witness $ \stepWitness ->
    pverifyInputSetsItem # pre # witness
      # (pinputSetsControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
      # collectionProof # chunkProof

pverifyInputSets :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyInputSets = phoistAcyclic $ plam $ \pre witness auxiliary ->
  pmatch witness $ \stepWitness ->
  plet (pinputSetsControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness)) $ \control ->
  pmatch auxiliary $ \case
    PNoAuxiliaryWitness -> pverifyInputSetsEmpty # pre # witness # control
    PTransactionFieldChunkWitness collectionProof chunkProof ->
      pverifyInputSetsItem # pre # witness # control
        # pfromData collectionProof # pfromData chunkProof
    _ -> pconstant False

pverifyInputSetsOneStepV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyInputSetsOneStepV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch pre $ \preState ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transition auxiliary) ->
  plet (pfromData transition) $ \witness ->
  pif (pfromData (pmachineState'phase preState) #== pcon PInputSets)
    ( pif (pstructuralTransitionIsValid # pre # witness)
        (pverifyInputSets # pre # witness # pdecodeValidationAuxiliaryWitnessV1 auxiliary)
        (pconstant False)
    )
    (pconstant False)

pverifyInputSetsSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyInputSetsSemanticsV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transition auxiliary) ->
    pverifyInputSets # pre # pfromData transition # pdecodeValidationAuxiliaryWitnessV1 auxiliary

psignaturesWithStage :: forall s.
  Term s (PSignaturesControlV1 :--> PInteger :--> PSignaturesControlV1)
psignaturesWithStage = phoistAcyclic $ plam $ \control stage ->
  pmatch control $ \c -> pcon $ PSignaturesControlV1
    (psignatures'compactCbor c)
    (psignatures'witnessSetCompactCbor c)
    (psignatures'fieldPreimageLengthsCbor c)
    (psignatures'contextCbor c)
    (psignatures'resolutionScheduleHash c)
    (pdata stage)
    (psignatures'addressCount c)
    (psignatures'requiredCount c)
    (psignatures'addressSeen c)
    (psignatures'requiredSeen c)
    (psignatures'previousOrderKey c)
    (psignatures'previousSignerHash c)
    (psignatures'signerCount c)
    (psignatures'signerPeaks c)
    (psignatures'invalidSignatureSeen c)

psignaturesAfterAddress :: forall s.
  Term s
    ( PSignaturesControlV1 :--> PInteger :--> PInteger :--> PByteString
        :--> PByteString :--> PInteger :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PInteger :--> PSignaturesControlV1
    )
psignaturesAfterAddress = phoistAcyclic $ plam $
  \control stage addressCount previousOrderKey previousSignerHash signerCount signerPeaks invalidSeen ->
  pmatch control $ \c -> pcon $ PSignaturesControlV1
    (psignatures'compactCbor c)
    (psignatures'witnessSetCompactCbor c)
    (psignatures'fieldPreimageLengthsCbor c)
    (psignatures'contextCbor c)
    (psignatures'resolutionScheduleHash c)
    (pdata stage)
    (pdata addressCount)
    (psignatures'requiredCount c)
    (pdata $ pfromData (psignatures'addressSeen c) + 1)
    (psignatures'requiredSeen c)
    (pdata previousOrderKey)
    (pdata previousSignerHash)
    (pdata signerCount)
    (pdata signerPeaks)
    (pdata invalidSeen)

psignaturesAfterRequired :: forall s.
  Term s (PSignaturesControlV1 :--> PInteger :--> PSignaturesControlV1)
psignaturesAfterRequired = phoistAcyclic $ plam $ \control requiredCount ->
  pmatch control $ \c -> pcon $ PSignaturesControlV1
    (psignatures'compactCbor c)
    (psignatures'witnessSetCompactCbor c)
    (psignatures'fieldPreimageLengthsCbor c)
    (psignatures'contextCbor c)
    (psignatures'resolutionScheduleHash c)
    (psignatures'stage c)
    (psignatures'addressCount c)
    (pdata requiredCount)
    (psignatures'addressSeen c)
    (pdata $ pfromData (psignatures'requiredSeen c) + 1)
    (psignatures'previousOrderKey c)
    (psignatures'previousSignerHash c)
    (psignatures'signerCount c)
    (psignatures'signerPeaks c)
    (psignatures'invalidSignatureSeen c)

psignaturesControlIsBound :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PSignaturesControlV1 :--> PVerifiedMidgardNativeTxCompact
        :--> PNativeTxWitnessSetCompact :--> PBool
    )
psignaturesControlIsBound = phoistAcyclic $ plam $ \pre witness control verified witnessSet ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch verified $ \verifiedSource ->
  pmatch (pverified'txCompact verifiedSource) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
  pmatch witnessSet $ \ws ->
  plet (pfromData (psignatures'addressCount c) #>= 0 #&& pfromData (psignatures'addressSeen c) #== pfromData (psignatures'addressCount c)) $ \addressComplete ->
  plet (pfromData (psignatures'requiredCount c) #>= 0 #&& pfromData (psignatures'requiredSeen c) #== pfromData (psignatures'requiredCount c)) $ \requiredComplete ->
  plet (pfromData (pwitnessSetCompact'addrTxWitsHash ws) #== NativeField.pemptyFieldCommitment) $ \addressIsEmpty ->
  plet (pbodyCompact'requiredSignersHash body #== NativeField.pemptyFieldCommitment) $ \requiredIsEmpty ->
  plet
    ( pand'List
        [ pverified'version verifiedSource #== 1
        , NativeCompact.pnativeTxProofCommitmentV1
            # pfromData (psignatures'compactCbor c)
            # pfromData (psignatures'witnessSetCompactCbor c)
            # pfromData (psignatures'fieldPreimageLengthsCbor c)
            #== pfromData (pmachineState'transactionCommitment preState)
        , phashValidationContext # pfromData (psignatures'contextCbor c)
            #== pfromData (pmachineState'validationContextHash preState)
        , plengthBS # pfromData (psignatures'resolutionScheduleHash c) #== 32
        , pfromData (psignatures'stage c) #>= 0
        , pfromData (psignatures'stage c) #<= 2
        , pfromData (psignatures'addressCount c) #>= (-1)
        , pfromData (psignatures'addressCount c) #<= pmaxTxSizeDerivedItemCount
        , pfromData (psignatures'requiredCount c) #>= (-1)
        , pfromData (psignatures'requiredCount c) #<= pmaxTxSizeDerivedItemCount
        , pfromData (psignatures'addressSeen c) #>= 0
        , pfromData (psignatures'requiredSeen c) #>= 0
        , pif addressIsEmpty
            (pfromData (psignatures'addressCount c) #== 0 #&& pfromData (psignatures'addressSeen c) #== 0)
            ( pif (pfromData (psignatures'addressCount c) #== (-1))
                (pfromData (psignatures'addressSeen c) #== 0)
                (pfromData (psignatures'addressCount c) #> 0 #&& pfromData (psignatures'addressSeen c) #<= pfromData (psignatures'addressCount c))
            )
        , pif requiredIsEmpty
            (pfromData (psignatures'requiredCount c) #== 0 #&& pfromData (psignatures'requiredSeen c) #== 0)
            ( pif (pfromData (psignatures'requiredCount c) #== (-1))
                (pfromData (psignatures'requiredSeen c) #== 0)
                (pfromData (psignatures'requiredCount c) #> 0 #&& pfromData (psignatures'requiredSeen c) #<= pfromData (psignatures'requiredCount c))
            )
        , pfromData (psignatures'signerCount c) #>= 0
        , pfromData (psignatures'signerCount c) #<= pfromData (psignatures'addressSeen c)
        , pfrontierIsWellFormed # pfromData (psignatures'signerCount c) # pfromData (psignatures'signerPeaks c)
        , pfromData (psignatures'invalidSignatureSeen c) #== 0 #|| pfromData (psignatures'invalidSignatureSeen c) #== 1
        , pfromData (psignatures'invalidSignatureSeen c) #<= pfromData (psignatures'addressSeen c)
        , pif (pfromData (psignatures'stage c) #== 0)
            ( pfromData (psignatures'requiredSeen c) #== 0
                #&& pif (pfromData (psignatures'addressSeen c) #== 0)
                  ( pfromData (psignatures'previousOrderKey c) #== pconstant ""
                      #&& pfromData (psignatures'previousSignerHash c) #== pconstant ""
                      #&& pfromData (psignatures'signerCount c) #== 0
                  )
                  ( plengthBS # pfromData (psignatures'previousOrderKey c) #> 0
                      #&& plengthBS # pfromData (psignatures'previousSignerHash c) #== 28
                      #&& pfromData (psignatures'signerCount c) #> 0
                  )
            )
            ( addressComplete
                #&& pfromData (psignatures'previousOrderKey c) #== pconstant ""
                #&& pfromData (psignatures'previousSignerHash c) #== pconstant ""
                #&& pif (pfromData (psignatures'stage c) #== 2) requiredComplete (pconstant True)
            )
        ]
    )
    $ \shapeIsValid ->
      shapeIsValid
        #&& pfromData (poneStep'workWitnessCbor stepWitness)
          #== pencodeSignaturesScanWitness
            # pfromData (psignatures'compactCbor c)
            # pfromData (psignatures'witnessSetCompactCbor c)
            # pfromData (psignatures'fieldPreimageLengthsCbor c)
            # pfromData (psignatures'contextCbor c)
            # pfromData (psignatures'resolutionScheduleHash c)
            # pfromData (psignatures'stage c)
            # pfromData (psignatures'addressCount c)
            # pfromData (psignatures'requiredCount c)
            # pfromData (psignatures'addressSeen c)
            # pfromData (psignatures'requiredSeen c)
            # pfromData (psignatures'previousOrderKey c)
            # pfromData (psignatures'previousSignerHash c)
            # pfromData (psignatures'signerCount c)
            # pfromData (psignatures'signerPeaks c)
            # pfromData (psignatures'invalidSignatureSeen c)

psignaturesSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationMachineStateV1
        :--> PSignaturesControlV1 :--> PBool
    )
psignaturesSuccessorIsExact = phoistAcyclic $ plam $ \pre post control ->
  pmatch pre $ \preState ->
  pmatch post $ \postState ->
  pmatch control $ \c ->
    pfromData (pmachineState'phase postState) #== pcon PSignatures
      #&& pfromData (pmachineState'workRoot postState)
        #== phashWorkWitness # pcon PSignatures
          # (pfromData (pmachineState'programCounter preState) + 1)
          # ( pencodeSignaturesScanWitness
                # pfromData (psignatures'compactCbor c)
                # pfromData (psignatures'witnessSetCompactCbor c)
                # pfromData (psignatures'fieldPreimageLengthsCbor c)
                # pfromData (psignatures'contextCbor c)
                # pfromData (psignatures'resolutionScheduleHash c)
                # pfromData (psignatures'stage c)
                # pfromData (psignatures'addressCount c)
                # pfromData (psignatures'requiredCount c)
                # pfromData (psignatures'addressSeen c)
                # pfromData (psignatures'requiredSeen c)
                # pfromData (psignatures'previousOrderKey c)
                # pfromData (psignatures'previousSignerHash c)
                # pfromData (psignatures'signerCount c)
                # pfromData (psignatures'signerPeaks c)
                # pfromData (psignatures'invalidSignatureSeen c)
            )

psignaturesAfterRequiredSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationMachineStateV1
        :--> PSignaturesControlV1 :--> PBool
    )
psignaturesAfterRequiredSuccessorIsExact = phoistAcyclic $ plam $ \pre post control ->
  pmatch control $ \c ->
  pif (pfromData (psignatures'invalidSignatureSeen c) #== 1)
    (prejectedSuccessorIsExact # pre # post # pconstant "E_INVALID_SIGNATURE")
    (psignaturesSuccessorIsExact # pre # post # (psignaturesWithStage # control # 2))

psignatureAddressOrderKey :: forall s.
  Term s (PByteString :--> PByteString :--> PInteger :--> PByteString)
psignatureAddressOrderKey = phoistAcyclic $ plam $ \signerHash witnessCbor itemIndex ->
  signerHash <> witnessCbor <> pcborInt itemIndex

psignerFrontierMatches :: forall s.
  Term s
    ( PInteger :--> PByteString :--> PBuiltinList (PAsData PFrontierPeak) :--> PBool )
psignerFrontierMatches = phoistAcyclic $ plam $ \signerCount commitment peaks ->
  pfrontierIsWellFormed # signerCount # peaks
    #&& pfrontierCommitment # signerCount # peaks #== commitment

psignerMembershipIsValid :: forall s.
  Term s
    ( PByteString :--> PInteger :--> PByteString
        :--> PBuiltinList (PAsData PFrontierPeak) :--> PInteger
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
psignerMembershipIsValid = phoistAcyclic $ plam $ \signerHash signerCount commitment peaks signerIndex siblings ->
  plengthBS # signerHash #== 28
    #&& psignerFrontierMatches # signerCount # commitment # peaks
    #&& pverifyMembership # signerCount # peaks # signerIndex
      # (ScriptProof.psignerLeafHash # signerHash) # siblings

psignerNonMembershipIsValid :: forall s.
  Term s
    ( PByteString :--> PInteger :--> PByteString :--> PSignerSetProofV1 :--> PBool )
psignerNonMembershipIsValid = phoistAcyclic $ plam $ \signerHash signerCount commitment proof ->
  pmatch proof $ \case
    PEmptySignerSetProof peaksD ->
      signerCount #== 0
        #&& psignerFrontierMatches # signerCount # commitment # pfromData peaksD
    PSignerBelowFirstProof peaksD firstHashD siblingsD ->
      signerHash #< pfromData firstHashD
        #&& psignerMembershipIsValid # pfromData firstHashD # signerCount # commitment
          # pfromData peaksD # 0 # pfromData siblingsD
    PSignerAboveLastProof peaksD lastHashD siblingsD ->
      signerCount #> 0
        #&& pfromData lastHashD #< signerHash
        #&& psignerMembershipIsValid # pfromData lastHashD # signerCount # commitment
          # pfromData peaksD # (signerCount - 1) # pfromData siblingsD
    PSignerBetweenProof peaksD lowerIndexD lowerHashD lowerSiblingsD upperHashD upperSiblingsD ->
      pfromData lowerIndexD #>= 0
        #&& pfromData lowerIndexD + 1 #< signerCount
        #&& pfromData lowerHashD #< signerHash
        #&& signerHash #< pfromData upperHashD
        #&& psignerMembershipIsValid # pfromData lowerHashD # signerCount # commitment
          # pfromData peaksD # pfromData lowerIndexD # pfromData lowerSiblingsD
        #&& psignerMembershipIsValid # pfromData upperHashD # signerCount # commitment
          # pfromData peaksD # (pfromData lowerIndexD + 1) # pfromData upperSiblingsD
    _ -> pconstant False

prequiredSignerMembershipIsValid :: forall s.
  Term s (PByteString :--> PSignaturesControlV1 :--> PSignerSetProofV1 :--> PBool)
prequiredSignerMembershipIsValid = phoistAcyclic $ plam $ \signerHash control proof ->
  pmatch control $ \c ->
  pmatch proof $ \case
    PSignerMembershipProof peaksD signerIndexD siblingsD ->
      pfromData peaksD #== pfromData (psignatures'signerPeaks c)
        #&& psignerMembershipIsValid
          # signerHash # pfromData (psignatures'signerCount c)
          # (pfrontierCommitment # pfromData (psignatures'signerCount c) # pfromData (psignatures'signerPeaks c))
          # pfromData peaksD # pfromData signerIndexD # pfromData siblingsD
    _ -> pconstant False

prequiredSignerNonMembershipIsValid :: forall s.
  Term s (PByteString :--> PSignaturesControlV1 :--> PSignerSetProofV1 :--> PBool)
prequiredSignerNonMembershipIsValid = phoistAcyclic $ plam $ \signerHash control proof ->
  pmatch control $ \c ->
    psignerNonMembershipIsValid
      # signerHash # pfromData (psignatures'signerCount c)
      # (pfrontierCommitment # pfromData (psignatures'signerCount c) # pfromData (psignatures'signerPeaks c))
      # proof

pverifySignatureAddressItem :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PSignaturesControlV1 :--> PItemProofV1 :--> BoundedItem.PChunkProofV1
        :--> PBool
    )
pverifySignatureAddressItem = phoistAcyclic $ plam $ \pre witness control collectionProof chunkProof ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch collectionProof $ \item ->
  pmatch chunkProof $ \chunk ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (psignatures'compactCbor c)
        # pfromData (psignatures'witnessSetCompactCbor c)
        # pfromData (psignatures'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified witnessSet) ->
  pmatch witnessSet $ \ws ->
  plet (pif (pfromData (psignatures'addressCount c) #== (-1)) (pfromData $ pitemProof'itemCount item) (pfromData $ psignatures'addressCount c)) $ \activeCount ->
  plet (pfromData $ BoundedItem.pchunkProof'chunk chunk) $ \witnessCbor ->
  plet (pdecodeMidgardAddressWitnessCbor # witnessCbor) $ \addressWitness ->
  pmatch addressWitness $ \aw ->
  plet (pblake2b_224 # pfromData (paddressWitness'verificationKey aw)) $ \signerHash ->
  plet (psignatureAddressOrderKey # signerHash # witnessCbor # pfromData (pitemProof'itemIndex item)) $ \orderKey ->
  plet (pfromData (psignatures'previousSignerHash c) #/= signerHash) $ \newSigner ->
  plet (pif newSigner (pfromData (psignatures'signerCount c) + 1) (pfromData $ psignatures'signerCount c)) $ \nextSignerCount ->
  plet (pif newSigner (pappendLeaf # pfromData (psignatures'signerCount c) # pfromData (psignatures'signerPeaks c) # (ScriptProof.psignerLeafHash # signerHash)) (pfromData $ psignatures'signerPeaks c)) $ \nextSignerPeaks ->
  plet
    ( pif
        ( pfromData (psignatures'invalidSignatureSeen c) #== 0
            #&& pnot # (pverifyEd25519Signature # pfromData (paddressWitness'verificationKey aw) # pfromData (pmachineState'transactionId preState) # pfromData (paddressWitness'signature aw))
        )
        1 (pfromData $ psignatures'invalidSignatureSeen c)
    )
    $ \nextInvalidSeen ->
  plet (pfromData (psignatures'addressSeen c) + 1) $ \nextAddressSeen ->
  plet
    ( pif (nextAddressSeen #== activeCount)
        (psignaturesAfterAddress # control # 1 # activeCount # pconstant "" # pconstant "" # nextSignerCount # nextSignerPeaks # nextInvalidSeen)
        (psignaturesAfterAddress # control # 0 # activeCount # orderKey # signerHash # nextSignerCount # nextSignerPeaks # nextInvalidSeen)
    )
    $ \nextControl ->
    pand'List
      [ psignaturesControlIsBound # pre # witness # control # verified # witnessSet
      , pfromData (psignatures'stage c) #== 0
      , pfromData (psignatures'addressCount c) #/= 0
      , pfromData (pitemProof'fieldIndex item) #== 7
      , pfromData (BoundedItem.pchunkProof'fieldIndex chunk) #== 7
      , pfromData (psignatures'addressCount c) #== (-1) #|| pfromData (psignatures'addressCount c) #== pfromData (pitemProof'itemCount item)
      , activeCount #> 0
      , activeCount #<= pmaxTxSizeDerivedItemCount
      , pfromData (psignatures'addressSeen c) #< activeCount
      , pfromData (pitemProof'itemCount item) #== activeCount
      , pfromData (pitemProof'itemLength item) #== pfromData (BoundedItem.pchunkProof'totalLength chunk)
      , pfromData (BoundedItem.pchunkProof'itemIndex chunk) #== pfromData (pitemProof'itemIndex item)
      , pfromData (BoundedItem.pchunkProof'chunkIndex chunk) #== 0
      , BoundedItem.pchunkCount # pfromData (BoundedItem.pchunkProof'totalLength chunk) #== 1
      , pverifyBoundedCollectionItem # pfromData (pwitnessSetCompact'addrTxWitsHash ws) # collectionProof
      , BoundedItem.pverifyChunk # pfromData (pitemProof'itemCommitment item) # chunkProof
      , pfromData (psignatures'previousOrderKey c) #== pconstant "" #|| pfromData (psignatures'previousOrderKey c) #< orderKey
      , psignaturesSuccessorIsExact # pre # pfromData (poneStep'claimedSuccessor stepWitness) # nextControl
      ]

pverifySignaturesAdvance :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PSignaturesControlV1 :--> PBool
    )
pverifySignaturesAdvance = phoistAcyclic $ plam $ \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (psignatures'compactCbor c)
        # pfromData (psignatures'witnessSetCompactCbor c)
        # pfromData (psignatures'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified witnessSet) ->
  pif (psignaturesControlIsBound # pre # witness # control # verified # witnessSet)
    ( pif (pfromData (psignatures'stage c) #== 0)
        ( pfromData (psignatures'addressCount c) #== 0
            #&& psignaturesSuccessorIsExact # pre
              # pfromData (poneStep'claimedSuccessor stepWitness)
              # (psignaturesWithStage # control # 1)
        )
        ( pif (pfromData (psignatures'stage c) #== 1)
            ( pfromData (psignatures'requiredCount c) #== 0
                #&& psignaturesAfterRequiredSuccessorIsExact # pre
                  # pfromData (poneStep'claimedSuccessor stepWitness) # control
            )
            (pconstant False)
        )
    )
    (pconstant False)

pverifyRequiredSignerItem :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PSignaturesControlV1 :--> PItemProofV1 :--> BoundedItem.PChunkProofV1
        :--> PSignerSetProofV1 :--> PBool
    )
pverifyRequiredSignerItem = phoistAcyclic $ plam $ \pre witness control collectionProof chunkProof signerProof ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch collectionProof $ \item ->
  pmatch chunkProof $ \chunk ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (psignatures'compactCbor c)
        # pfromData (psignatures'witnessSetCompactCbor c)
        # pfromData (psignatures'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified witnessSet) ->
  pmatch verified $ \verifiedSource ->
  pmatch (pverified'txCompact verifiedSource) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
  plet (pif (pfromData (psignatures'requiredCount c) #== (-1)) (pfromData $ pitemProof'itemCount item) (pfromData $ psignatures'requiredCount c)) $ \activeCount ->
  plet (pfromData $ BoundedItem.pchunkProof'chunk chunk) $ \signerHash ->
  plet (psignaturesAfterRequired # control # activeCount) $ \nextControl ->
  plet (prequiredSignerMembershipIsValid # signerHash # control # signerProof) $ \present ->
  plet (prequiredSignerNonMembershipIsValid # signerHash # control # signerProof) $ \absent ->
  pif
    ( pand'List
        [ psignaturesControlIsBound # pre # witness # control # verified # witnessSet
        , pfromData (psignatures'stage c) #== 1
        , pfromData (psignatures'requiredCount c) #/= 0
        , pfromData (pitemProof'fieldIndex item) #== 4
        , pfromData (BoundedItem.pchunkProof'fieldIndex chunk) #== 4
        , pfromData (psignatures'requiredCount c) #== (-1) #|| pfromData (psignatures'requiredCount c) #== pfromData (pitemProof'itemCount item)
        , activeCount #> 0
        , activeCount #<= pmaxTxSizeDerivedItemCount
        , pfromData (psignatures'requiredSeen c) #< activeCount
        , pfromData (pitemProof'itemCount item) #== activeCount
        , pfromData (pitemProof'itemIndex item) #== pfromData (psignatures'requiredSeen c)
        , pfromData (pitemProof'itemLength item) #== 28
        , pfromData (BoundedItem.pchunkProof'totalLength chunk) #== 28
        , pfromData (BoundedItem.pchunkProof'itemIndex chunk) #== pfromData (pitemProof'itemIndex item)
        , pfromData (BoundedItem.pchunkProof'chunkIndex chunk) #== 0
        , plengthBS # signerHash #== 28
        , pverifyBoundedCollectionItem # pbodyCompact'requiredSignersHash body # collectionProof
        , BoundedItem.pverifyChunk # pfromData (pitemProof'itemCommitment item) # chunkProof
        ]
    )
    ( pif absent
        (prejectedSuccessorIsExact # pre # pfromData (poneStep'claimedSuccessor stepWitness) # pconstant "E_MISSING_REQUIRED_WITNESS")
        ( pif present
            ( pif (pfromData (psignatures'requiredSeen c) + 1 #== activeCount)
                (psignaturesAfterRequiredSuccessorIsExact # pre # pfromData (poneStep'claimedSuccessor stepWitness) # nextControl)
                (psignaturesSuccessorIsExact # pre # pfromData (poneStep'claimedSuccessor stepWitness) # nextControl)
            )
            (pconstant False)
        )
    )
    (pconstant False)

pverifySignaturesHandoff :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PSignaturesControlV1 :--> PBool
    )
pverifySignaturesHandoff = phoistAcyclic $ plam $ \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (psignatures'compactCbor c)
        # pfromData (psignatures'witnessSetCompactCbor c)
        # pfromData (psignatures'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified witnessSet) ->
  pmatch witnessSet $ \ws ->
  pmatch (pfromData $ poneStep'claimedSuccessor stepWitness) $ \post ->
  plet (pif (pfromData (pwitnessSetCompact'scriptTxWitsHash ws) #== NativeField.pemptyFieldCommitment) 0 (-1)) $ \scriptCount ->
    pand'List
      [ psignaturesControlIsBound # pre # witness # control # verified # witnessSet
      , pfromData (psignatures'stage c) #== 2
      , pfromData (pmachineState'phase post) #== pcon PPhaseANativeScripts
      , pfromData (pmachineState'workRoot post)
          #== phashWorkWitness # pcon PPhaseANativeScripts
            # (pfromData (pmachineState'programCounter preState) + 1)
            # ( pencodePhaseANativeScriptsScanWitness
                  # pfromData (psignatures'compactCbor c)
                  # pfromData (psignatures'witnessSetCompactCbor c)
                  # pfromData (psignatures'fieldPreimageLengthsCbor c)
                  # pfromData (psignatures'contextCbor c)
                  # pfromData (psignatures'resolutionScheduleHash c)
                  # 0 # scriptCount # 0 # 0 # 0 # pconstant ""
                  # 0 # pconstant "" # 0 # 0 # (-1)
                  # pfromData (psignatures'signerCount c)
                  # pfromData (psignatures'signerPeaks c)
                  # pconstant ""
              )
      ]

pverifySignatures :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifySignatures = phoistAcyclic $ plam $ \pre witness auxiliary ->
  pmatch witness $ \stepWitness ->
  plet (psignaturesControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness)) $ \control ->
  pmatch control $ \c ->
  pmatch auxiliary $ \case
    PNoAuxiliaryWitness ->
      pif (pfromData (psignatures'stage c) #== 2)
        (pverifySignaturesHandoff # pre # witness # control)
        (pverifySignaturesAdvance # pre # witness # control)
    PTransactionFieldChunkWitness collectionProof chunkProof ->
      pverifySignatureAddressItem # pre # witness # control
        # pfromData collectionProof # pfromData chunkProof
    PRequiredSignerItemWitness collectionProof chunkProof signerProof ->
      pverifyRequiredSignerItem # pre # witness # control
        # pfromData collectionProof # pfromData chunkProof # pfromData signerProof
    _ -> pconstant False

pverifySignaturesAdvanceSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifySignaturesAdvanceSemanticsV1 = phoistAcyclic $ plam $ \pre witness ->
  pmatch witness $ \stepWitness ->
    pverifySignaturesAdvance # pre # witness
      # (psignaturesControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))

pverifySignatureAddressItemSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PItemProofV1 :--> BoundedItem.PChunkProofV1 :--> PBool
    )
pverifySignatureAddressItemSemanticsV1 = phoistAcyclic $ plam $ \pre witness collectionProof chunkProof ->
  pmatch witness $ \stepWitness ->
    pverifySignatureAddressItem # pre # witness
      # (psignaturesControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
      # collectionProof # chunkProof

pverifyRequiredSignerItemSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PItemProofV1 :--> BoundedItem.PChunkProofV1 :--> PSignerSetProofV1
        :--> PBool
    )
pverifyRequiredSignerItemSemanticsV1 = phoistAcyclic $ plam $ \pre witness collectionProof chunkProof signerProof ->
  pmatch witness $ \stepWitness ->
    pverifyRequiredSignerItem # pre # witness
      # (psignaturesControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
      # collectionProof # chunkProof # signerProof

pverifySignaturesHandoffSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifySignaturesHandoffSemanticsV1 = phoistAcyclic $ plam $ \pre witness ->
  pmatch witness $ \stepWitness ->
    pverifySignaturesHandoff # pre # witness
      # (psignaturesControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))

pverifySignaturesOneStepV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifySignaturesOneStepV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch pre $ \preState ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transition auxiliary) ->
  plet (pfromData transition) $ \witness ->
  pif (pfromData (pmachineState'phase preState) #== pcon PSignatures)
    ( pif (pstructuralTransitionIsValid # pre # witness)
        (pverifySignatures # pre # witness # pdecodeValidationAuxiliaryWitnessV1 auxiliary)
        (pconstant False)
    )
    (pconstant False)

pencodePhaseANativeControlV1 :: forall s.
  Term s (PPhaseANativeScriptsControlV1 :--> PByteString)
pencodePhaseANativeControlV1 = phoistAcyclic $ plam $ \control ->
  pmatch control $ \c ->
    pencodePhaseANativeScriptsScanWitness
      # pfromData (pphaseANative'compactCbor c)
      # pfromData (pphaseANative'witnessSetCompactCbor c)
      # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
      # pfromData (pphaseANative'contextCbor c)
      # pfromData (pphaseANative'resolutionScheduleHash c)
      # pfromData (pphaseANative'stage c)
      # pfromData (pphaseANative'scriptCount c)
      # pfromData (pphaseANative'scriptSeen c)
      # pfromData (pphaseANative'containsNonNativeScript c)
      # pfromData (pphaseANative'itemLength c)
      # pfromData (pphaseANative'itemCommitment c)
      # pfromData (pphaseANative'cursor c)
      # pfromData (pphaseANative'stackRoot c)
      # pfromData (pphaseANative'stackDepth c)
      # pfromData (pphaseANative'nodeCount c)
      # pfromData (pphaseANative'result c)
      # pfromData (pphaseANative'signerCount c)
      # pfromData (pphaseANative'signerPeaks c)
      # pfromData (pphaseANative'continuationCbor c)

pphaseANativeControlIsBound :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PPhaseANativeScriptsControlV1 :--> PVerifiedMidgardNativeTxCompact
        :--> PNativeTxWitnessSetCompact :--> PBool
    )
pphaseANativeControlIsBound = phoistAcyclic $ plam $ \pre witness control verified witnessSet ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch verified $ \verifiedSource ->
  pmatch witnessSet $ \ws ->
  plet (pfromData (pphaseANative'continuationCbor c) #/= pconstant "") $ \isLateContinuation ->
  plet (pfromData (pwitnessSetCompact'scriptTxWitsHash ws) #== NativeField.pemptyFieldCommitment) $ \scriptsAreEmpty ->
  plet
    ( pif isLateContinuation
        ( plet (pnativeScriptsControlFromWitness # pfromData (pphaseANative'continuationCbor c)) $ \nativeControl ->
          pmatch nativeControl $ \native ->
            pand'List
              [ pfromData (pphaseANative'resolutionScheduleHash c)
                  #== pblake2b_256 # pfromData (pphaseANative'continuationCbor c)
              , pfromData (pphaseANative'stage c) #> 0
              , pfromData (pphaseANative'scriptCount c) #== 1
              , pfromData (pphaseANative'scriptSeen c) #== 0
              , pfromData (pphaseANative'containsNonNativeScript c) #== 0
              , pnativeScriptsControlIsWellFormed # nativeControl
              , pfromData (pphaseANative'continuationCbor c)
                  #== pencodeNativeScriptsControlV1 # nativeControl
              , pfromData (pnativeControl'compactCbor native) #== pfromData (pphaseANative'compactCbor c)
              , pfromData (pnativeControl'witnessSetCompactCbor native) #== pfromData (pphaseANative'witnessSetCompactCbor c)
              , pfromData (pnativeControl'fieldPreimageLengthsCbor native) #== pfromData (pphaseANative'fieldPreimageLengthsCbor c)
              , pfromData (pnativeControl'contextCbor native) #== pfromData (pphaseANative'contextCbor c)
              , pfromData (pnativeControl'executionCursor native) #< pfromData (pnativeControl'executionCount native)
              , pfromData (pnativeControl'signerCount native) #== pfromData (pphaseANative'signerCount c)
              , pfromData (pnativeControl'signerFrontierCommitment native)
                  #== pfrontierCommitment
                    # pfromData (pphaseANative'signerCount c)
                    # pfromData (pphaseANative'signerPeaks c)
              ]
        )
        ( pif scriptsAreEmpty
            (pfromData (pphaseANative'scriptCount c) #== 0 #&& pfromData (pphaseANative'scriptSeen c) #== 0)
            ( pif (pfromData (pphaseANative'scriptCount c) #== (-1))
                (pfromData (pphaseANative'scriptSeen c) #== 0)
                (pfromData (pphaseANative'scriptCount c) #> 0 #&& pfromData (pphaseANative'scriptSeen c) #<= pfromData (pphaseANative'scriptCount c))
            )
        )
    )
    $ \sourceShapeIsValid ->
  plet
    ( pif (pfromData (pphaseANative'stage c) #== 0)
        ( pand'List
            [ pnot # isLateContinuation
            , pfromData (pphaseANative'itemLength c) #== 0
            , pfromData (pphaseANative'itemCommitment c) #== pconstant ""
            , pfromData (pphaseANative'cursor c) #== 0
            , pfromData (pphaseANative'stackRoot c) #== pconstant ""
            , pfromData (pphaseANative'stackDepth c) #== 0
            , pfromData (pphaseANative'nodeCount c) #== 0
            , pfromData (pphaseANative'result c) #== (-1)
            ]
        )
        ( pand'List
            [ pfromData (pphaseANative'scriptCount c) #> 0
            , pfromData (pphaseANative'scriptSeen c) #< pfromData (pphaseANative'scriptCount c)
            , pfromData (pphaseANative'itemLength c) #> 0
            , plengthBS # pfromData (pphaseANative'itemCommitment c) #== 32
            , pif (pfromData (pphaseANative'stackRoot c) #== pconstant "")
                (pfromData (pphaseANative'stackDepth c) #== 0)
                (plengthBS # pfromData (pphaseANative'stackRoot c) #== 32 #&& pfromData (pphaseANative'stackDepth c) #> 0)
            , pif (pfromData (pphaseANative'stage c) #== 2)
                (pfromData (pphaseANative'result c) #>= 0)
                (pfromData (pphaseANative'result c) #== (-1))
            ]
        )
    )
    $ \stageShapeIsValid ->
      pand'List
        [ pverified'version verifiedSource #== 1
        , NativeCompact.pnativeTxProofCommitmentV1
            # pfromData (pphaseANative'compactCbor c)
            # pfromData (pphaseANative'witnessSetCompactCbor c)
            # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
            #== pfromData (pmachineState'transactionCommitment preState)
        , phashValidationContext # pfromData (pphaseANative'contextCbor c)
            #== pfromData (pmachineState'validationContextHash preState)
        , plengthBS # pfromData (pphaseANative'resolutionScheduleHash c) #== 32
        , pfromData (pphaseANative'stage c) #>= 0
        , pfromData (pphaseANative'stage c) #<= 8
        , pfromData (pphaseANative'scriptCount c) #>= (-1)
        , pfromData (pphaseANative'scriptCount c) #<= pmaxTxSizeDerivedItemCount
        , pfromData (pphaseANative'scriptSeen c) #>= 0
        , sourceShapeIsValid
        , pfromData (pphaseANative'containsNonNativeScript c) #== 0
            #|| pfromData (pphaseANative'containsNonNativeScript c) #== 1
        , pfromData (pphaseANative'signerCount c) #>= 0
        , pfromData (pphaseANative'signerCount c) #<= pmaxTxSizeDerivedItemCount
        , pfrontierIsWellFormed # pfromData (pphaseANative'signerCount c) # pfromData (pphaseANative'signerPeaks c)
        , pfromData (pphaseANative'itemLength c) #>= 0
        , pfromData (pphaseANative'itemLength c) #<= pmaxAggregateFieldPreimageBytes
        , pfromData (pphaseANative'cursor c) #>= 0
        , pfromData (pphaseANative'cursor c) #<= pfromData (pphaseANative'itemLength c)
        , pfromData (pphaseANative'stackDepth c) #>= 0
        , pfromData (pphaseANative'stackDepth c) #<= NativeScriptScan.pmaxNativeScriptDepth
        , pfromData (pphaseANative'nodeCount c) #>= 0
        , pfromData (pphaseANative'nodeCount c) #<= NativeScriptScan.pmaxNativeScriptNodes
        , pfromData (pphaseANative'result c) #>= (-1)
        , pfromData (pphaseANative'result c) #<= 1
        , stageShapeIsValid
        , pfromData (poneStep'workWitnessCbor stepWitness) #== pencodePhaseANativeControlV1 # control
        ]

pphaseANativeSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationMachineStateV1
        :--> PPhaseANativeScriptsControlV1 :--> PBool
    )
pphaseANativeSuccessorIsExact = phoistAcyclic $ plam $ \pre post control ->
  pmatch pre $ \preState ->
  pmatch post $ \postState ->
    pfromData (pmachineState'phase postState) #== pcon PPhaseANativeScripts
      #&& pfromData (pmachineState'workRoot postState)
        #== phashWorkWitness # pcon PPhaseANativeScripts
          # (pfromData (pmachineState'programCounter preState) + 1)
          # (pencodePhaseANativeControlV1 # control)

pphaseANativeToPreconditionsIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationMachineStateV1
        :--> PPhaseANativeScriptsControlV1 :--> PBool
    )
pphaseANativeToPreconditionsIsExact = phoistAcyclic $ plam $ \pre post control ->
  pmatch pre $ \preState ->
  pmatch post $ \postState ->
  pmatch control $ \c ->
    pfromData (pmachineState'phase postState) #== pcon PPhaseAScriptPreconditions
      #&& pfromData (pmachineState'workRoot postState)
        #== phashWorkWitness # pcon PPhaseAScriptPreconditions
          # (pfromData (pmachineState'programCounter preState) + 1)
          # ( pencodePhaseAScriptPreconditionsWitness
                # pfromData (pphaseANative'compactCbor c)
                # pfromData (pphaseANative'witnessSetCompactCbor c)
                # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
                # pfromData (pphaseANative'contextCbor c)
                # pfromData (pphaseANative'resolutionScheduleHash c)
                # pfromData (pphaseANative'signerCount c)
                # (pfrontierCommitment # pfromData (pphaseANative'signerCount c) # pfromData (pphaseANative'signerPeaks c))
                # (pfromData (pphaseANative'containsNonNativeScript c) #== 1)
                # 0 # 0 # pconstant ""
            )

presetPhaseANativeControl :: forall s.
  Term s
    ( PPhaseANativeScriptsControlV1 :--> PInteger :--> PInteger :--> PInteger
        :--> PPhaseANativeScriptsControlV1
    )
presetPhaseANativeControl = phoistAcyclic $ plam $ \control scriptCount scriptSeen containsNonNative ->
  pmatch control $ \c -> pcon $ PPhaseANativeScriptsControlV1
    (pphaseANative'compactCbor c)
    (pphaseANative'witnessSetCompactCbor c)
    (pphaseANative'fieldPreimageLengthsCbor c)
    (pphaseANative'contextCbor c)
    (pphaseANative'resolutionScheduleHash c)
    (pdata 0) (pdata scriptCount) (pdata scriptSeen) (pdata containsNonNative)
    (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata $ pconstant "")
    (pdata 0) (pdata 0) (pdata (-1))
    (pphaseANative'signerCount c) (pphaseANative'signerPeaks c)
    (pphaseANative'continuationCbor c)

pnativeScriptsAdvanceExecutionCursor :: forall s.
  Term s (PNativeScriptsControlV1 :--> PNativeScriptsControlV1)
pnativeScriptsAdvanceExecutionCursor = phoistAcyclic $ plam $ \control ->
  pmatch control $ \c -> pcon $ PNativeScriptsControlV1
    (pnativeControl'compactCbor c)
    (pnativeControl'witnessSetCompactCbor c)
    (pnativeControl'fieldPreimageLengthsCbor c)
    (pnativeControl'contextCbor c)
    (pnativeControl'resolvedInputCount c)
    (pnativeControl'resolvedInputsAccumulator c)
    (pnativeControl'spendInputCount c)
    (pnativeControl'resolvedItemPeaks c)
    (pnativeControl'signerCount c)
    (pnativeControl'signerFrontierCommitment c)
    (pnativeControl'sourceCount c) (pnativeControl'sourcePeaks c)
    (pnativeControl'redeemerCount c) (pnativeControl'redeemerPeaks c)
    (pnativeControl'purposeCount c) (pnativeControl'purposePeaks c)
    (pnativeControl'outputCount c) (pnativeControl'outputPeaks c)
    (pnativeControl'outputDescriptorPeaks c)
    (pnativeControl'mintCount c) (pnativeControl'mintPeaks c)
    (pnativeControl'executionCount c) (pnativeControl'executionPeaks c)
    (pdata $ pfromData (pnativeControl'executionCursor c) + 1)
    (pnativeControl'languageBitmap c)
    (pnativeControl'resolutionScheduleHash c)

pphaseANativeCompleteScriptIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationMachineStateV1
        :--> PPhaseANativeScriptsControlV1 :--> PInteger :--> PInteger :--> PInteger
        :--> PBool
    )
pphaseANativeCompleteScriptIsExact = phoistAcyclic $ plam $ \pre post control scriptCount scriptSeen containsNonNative ->
  pmatch pre $ \preState ->
  pmatch post $ \postState ->
  pmatch control $ \c ->
  pif (pfromData (pphaseANative'continuationCbor c) #/= pconstant "")
    ( plet (pnativeScriptsControlFromWitness # pfromData (pphaseANative'continuationCbor c)) $ \nativeControl ->
      plet (pnativeScriptsAdvanceExecutionCursor # nativeControl) $ \nextNativeControl ->
        pand'List
          [ scriptCount #== 1
          , scriptSeen #== 1
          , containsNonNative #== 0
          , pfromData (pmachineState'phase postState) #== pcon PNativeScripts
          , pfromData (pmachineState'workRoot postState)
              #== phashWorkWitness # pcon PNativeScripts
                # (pfromData (pmachineState'programCounter preState) + 1)
                # (pencodeNativeScriptsControlV1 # nextNativeControl)
          ]
    )
    ( plet (presetPhaseANativeControl # control # scriptCount # scriptSeen # containsNonNative) $ \nextControl ->
      pif (scriptSeen #== scriptCount)
        (pphaseANativeToPreconditionsIsExact # pre # post # nextControl)
        (pphaseANativeSuccessorIsExact # pre # post # nextControl)
    )

pverifyPhaseANativeAdvanceScan :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PPhaseANativeScriptsControlV1 :--> PBool
    )
pverifyPhaseANativeAdvanceScan = phoistAcyclic $ plam $ \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pphaseANative'compactCbor c)
        # pfromData (pphaseANative'witnessSetCompactCbor c)
        # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified witnessSet) ->
    pand'List
      [ pphaseANativeControlIsBound # pre # witness # control # verified # witnessSet
      , pfromData (pphaseANative'stage c) #== 0
      , pfromData (pphaseANative'scriptCount c) #== 0
      , pphaseANativeToPreconditionsIsExact
          # pre # pfromData (poneStep'claimedSuccessor stepWitness) # control
      ]

pverifyPhaseANativeItemScan :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PPhaseANativeScriptsControlV1 :--> PItemProofV1 :--> BoundedItem.PChunkProofV1
        :--> PBool
    )
pverifyPhaseANativeItemScan = phoistAcyclic $ plam $ \pre witness control collectionProof chunkProof ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch collectionProof $ \item ->
  pmatch chunkProof $ \chunk ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pphaseANative'compactCbor c)
        # pfromData (pphaseANative'witnessSetCompactCbor c)
        # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified witnessSet) ->
  pmatch witnessSet $ \ws ->
  plet (pif (pfromData (pphaseANative'scriptCount c) #== (-1)) (pfromData $ pitemProof'itemCount item) (pfromData $ pphaseANative'scriptCount c)) $ \activeCount ->
  pif
    ( pand'List
        [ pphaseANativeControlIsBound # pre # witness # control # verified # witnessSet
        , pfromData (pphaseANative'stage c) #== 0
        , pfromData (pphaseANative'scriptCount c) #/= 0
        , pfromData (pphaseANative'scriptCount c) #== (-1) #|| pfromData (pphaseANative'scriptCount c) #== pfromData (pitemProof'itemCount item)
        , activeCount #> 0
        , activeCount #<= pmaxTxSizeDerivedItemCount
        , pfromData (pitemProof'fieldIndex item) #== 6
        , pfromData (pitemProof'itemCount item) #== activeCount
        , pfromData (pitemProof'itemIndex item) #== pfromData (pphaseANative'scriptSeen c)
        , pfromData (pitemProof'itemLength item) #> 0
        , pfromData (pitemProof'itemLength item) #<= pmaxAggregateFieldPreimageBytes
        , pfromData (BoundedItem.pchunkProof'fieldIndex chunk) #== 6
        , pfromData (BoundedItem.pchunkProof'itemIndex chunk) #== pfromData (pitemProof'itemIndex item)
        , pfromData (BoundedItem.pchunkProof'totalLength chunk) #== pfromData (pitemProof'itemLength item)
        , pfromData (BoundedItem.pchunkProof'chunkIndex chunk) #== 0
        , pverifyBoundedCollectionItem # pfromData (pwitnessSetCompact'scriptTxWitsHash ws) # collectionProof
        , BoundedItem.pverifyChunk # pfromData (pitemProof'itemCommitment item) # chunkProof
        ]
    )
    ( pmatch
        ( NativeScriptScan.pversionedScriptHeaderV1
            # pfromData (BoundedItem.pchunkProof'chunk chunk)
            # pfromData (pitemProof'itemLength item)
        )
        $ \case
        PNothing -> prejectedSuccessorIsExact
          # pre # pfromData (poneStep'claimedSuccessor stepWitness)
          # pconstant "E_INVALID_FIELD_TYPE"
        PJust itemHeader -> pmatch itemHeader $ \h ->
          pif (pfromData (NativeScriptScan.pheader'languageTag h) #== 0)
            ( plet
                ( pcon $ PPhaseANativeScriptsControlV1
                    (pphaseANative'compactCbor c)
                    (pphaseANative'witnessSetCompactCbor c)
                    (pphaseANative'fieldPreimageLengthsCbor c)
                    (pphaseANative'contextCbor c)
                    (pphaseANative'resolutionScheduleHash c)
                    (pdata 1) (pdata activeCount) (pphaseANative'scriptSeen c)
                    (pphaseANative'containsNonNativeScript c)
                    (pitemProof'itemLength item) (pitemProof'itemCommitment item)
                    (NativeScriptScan.pheader'payloadOffset h)
                    (pphaseANative'stackRoot c) (pphaseANative'stackDepth c)
                    (pphaseANative'nodeCount c) (pphaseANative'result c)
                    (pphaseANative'signerCount c) (pphaseANative'signerPeaks c)
                    (pphaseANative'continuationCbor c)
                )
                $ \nextControl ->
                  pphaseANativeSuccessorIsExact
                    # pre # pfromData (poneStep'claimedSuccessor stepWitness) # nextControl
            )
            ( pphaseANativeCompleteScriptIsExact
                # pre # pfromData (poneStep'claimedSuccessor stepWitness) # control
                # activeCount # (pfromData (pphaseANative'scriptSeen c) + 1) # 1
            )
    )
    (pconstant False)

pverifyPhaseANativeAdvanceSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyPhaseANativeAdvanceSemanticsV1 = phoistAcyclic $ plam $ \pre witness ->
  pmatch witness $ \stepWitness ->
  plet
    (pphaseANativeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
    pif (pfromData (pphaseANative'stage c) #== 2)
      (pverifyPhaseANativeFinalizeScan # pre # witness # control)
      (pverifyPhaseANativeAdvanceScan # pre # witness # control)

pverifyPhaseANativeItemSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PItemProofV1 :--> BoundedItem.PChunkProofV1 :--> PBool
    )
pverifyPhaseANativeItemSemanticsV1 = phoistAcyclic $ plam $ \pre witness collectionProof chunkProof ->
  pmatch witness $ \stepWitness ->
    pverifyPhaseANativeItemScan # pre # witness
      # (pphaseANativeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
      # collectionProof # chunkProof

pphaseANativeSetExecution :: forall s.
  Term s
    ( PPhaseANativeScriptsControlV1 :--> PInteger :--> PInteger :--> PByteString
        :--> PInteger :--> PInteger :--> PInteger :--> PPhaseANativeScriptsControlV1
    )
pphaseANativeSetExecution = phoistAcyclic $ plam $
  \control stage cursor stackRoot stackDepth nodeCount result ->
  pmatch control $ \c -> pcon $ PPhaseANativeScriptsControlV1
    (pphaseANative'compactCbor c)
    (pphaseANative'witnessSetCompactCbor c)
    (pphaseANative'fieldPreimageLengthsCbor c)
    (pphaseANative'contextCbor c)
    (pphaseANative'resolutionScheduleHash c)
    (pdata stage)
    (pphaseANative'scriptCount c)
    (pphaseANative'scriptSeen c)
    (pphaseANative'containsNonNativeScript c)
    (pphaseANative'itemLength c)
    (pphaseANative'itemCommitment c)
    (pdata cursor)
    (pdata stackRoot)
    (pdata stackDepth)
    (pdata nodeCount)
    (pdata result)
    (pphaseANative'signerCount c)
    (pphaseANative'signerPeaks c)
    (pphaseANative'continuationCbor c)

pphaseANativeChunkWindow :: forall s.
  Term s
    ( PPhaseANativeScriptsControlV1 :--> BoundedItem.PChunkProofV1
        :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PMaybe (PPair PByteString PInteger)
    )
pphaseANativeChunkWindow = phoistAcyclic $ plam $ \control chunkProof nextChunkProof ->
  pmatch control $ \c ->
  pmatch chunkProof $ \chunk ->
  plet (pdiv # pfromData (pphaseANative'cursor c) # BoundedItem.pchunkBytes) $ \expectedChunkIndex ->
  plet (BoundedItem.pchunkCount # pfromData (pphaseANative'itemLength c)) $ \chunkCount ->
  plet
    ( pand'List
        [ pfromData (BoundedItem.pchunkProof'fieldIndex chunk) #== 6
        , pfromData (BoundedItem.pchunkProof'itemIndex chunk) #== pfromData (pphaseANative'scriptSeen c)
        , pfromData (BoundedItem.pchunkProof'totalLength chunk) #== pfromData (pphaseANative'itemLength c)
        , pfromData (BoundedItem.pchunkProof'chunkIndex chunk) #== expectedChunkIndex
        , BoundedItem.pverifyChunk # pfromData (pphaseANative'itemCommitment c) # chunkProof
        ]
    )
    $ \currentMatches ->
  pif (pnot # currentMatches) (pcon PNothing) $
  pif (expectedChunkIndex + 1 #< chunkCount)
    ( pmatch nextChunkProof $ \case
        PDNothing -> pcon PNothing
        PDJust nextProofD ->
          plet (pfromData nextProofD) $ \nextProof ->
          pmatch nextProof $ \next ->
          pif
            ( pand'List
                [ pfromData (BoundedItem.pchunkProof'fieldIndex next) #== 6
                , pfromData (BoundedItem.pchunkProof'itemIndex next) #== pfromData (pphaseANative'scriptSeen c)
                , pfromData (BoundedItem.pchunkProof'totalLength next) #== pfromData (pphaseANative'itemLength c)
                , pfromData (BoundedItem.pchunkProof'chunkIndex next) #== expectedChunkIndex + 1
                , BoundedItem.pverifyChunk # pfromData (pphaseANative'itemCommitment c) # nextProof
                ]
            )
            ( pcon $ PJust $ pcon $ PPair
                (pfromData (BoundedItem.pchunkProof'chunk chunk) <> pfromData (BoundedItem.pchunkProof'chunk next))
                (pfromData (pphaseANative'cursor c) - expectedChunkIndex * BoundedItem.pchunkBytes)
            )
            (pcon PNothing)
    )
    ( pmatch nextChunkProof $ \case
        PDNothing -> pcon $ PJust $ pcon $ PPair
          (pfromData $ BoundedItem.pchunkProof'chunk chunk)
          (pfromData (pphaseANative'cursor c) - expectedChunkIndex * BoundedItem.pchunkBytes)
        PDJust _ -> pcon PNothing
    )

pphaseANativeSignatureResult :: forall s.
  Term s
    ( PByteString :--> PPhaseANativeScriptsControlV1 :--> PSignerSetProofV1
        :--> PMaybe PBool
    )
pphaseANativeSignatureResult = phoistAcyclic $ plam $ \keyHash control proof ->
  pmatch control $ \c ->
  plet
    (pfrontierCommitment # pfromData (pphaseANative'signerCount c) # pfromData (pphaseANative'signerPeaks c))
    $ \commitment ->
  pmatch proof $ \case
    PSignerMembershipProof peaksD signerIndexD siblingsD ->
      pif
        ( psignerMembershipIsValid
            # keyHash # pfromData (pphaseANative'signerCount c) # commitment
            # pfromData peaksD # pfromData signerIndexD # pfromData siblingsD
        )
        (pcon $ PJust $ pconstant True)
        (pcon PNothing)
    _ ->
      pif
        ( psignerNonMembershipIsValid
            # keyHash # pfromData (pphaseANative'signerCount c) # commitment # proof
        )
        (pcon $ PJust $ pconstant False)
        (pcon PNothing)

pverifyPhaseANativeTokenHeadScanV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PPhaseANativeScriptsControlV1 :--> BoundedItem.PChunkProofV1
        :--> PMaybeData BoundedItem.PChunkProofV1 :--> PBool
    )
pverifyPhaseANativeTokenHeadScanV1 = phoistAcyclic $ plam $
  \pre witness control chunkProof nextChunkProof ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pphaseANative'compactCbor c)
        # pfromData (pphaseANative'witnessSetCompactCbor c)
        # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified witnessSet) ->
  pif
    ( pphaseANativeControlIsBound # pre # witness # control # verified # witnessSet
        #&& pfromData (pphaseANative'stage c) #== 1
    )
    ( pmatch (pphaseANativeChunkWindow # control # chunkProof # nextChunkProof) $ \case
        PNothing -> pconstant False
        PJust authenticated -> pmatch authenticated $ \(PPair windowBytes windowOffset) ->
          pmatch (NativeScriptScan.ptokenHeadAtV1 # windowBytes # windowOffset) $ \case
            PNothing -> prejectedSuccessorIsExact
              # pre # pfromData (poneStep'claimedSuccessor stepWitness)
              # pconstant "E_INVALID_FIELD_TYPE"
            PJust headValue -> pmatch headValue $ \h ->
              pif (pnot # (NativeScriptScan.ptokenHeadIsWellFormedV1 # headValue))
                ( prejectedSuccessorIsExact
                    # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                    # pconstant "E_INVALID_FIELD_TYPE"
                )
                ( plet (pfromData (pphaseANative'nodeCount c) + 1) $ \nextNodeCount ->
                  plet
                    ( pfromData (pphaseANative'cursor c)
                        + pfromData (NativeScriptScan.ptokenHead'nextOffset h)
                        - windowOffset
                    )
                    $ \nextOffset ->
                  pif (nextNodeCount #> NativeScriptScan.pmaxNativeScriptNodes)
                    ( prejectedSuccessorIsExact
                        # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                        # pconstant "E_NATIVE_SCRIPT_NODE_COUNT"
                    )
                    ( nextOffset #<= pfromData (pphaseANative'itemLength c)
                        #&& pphaseANativeSuccessorIsExact
                          # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                          # ( pphaseANativeSetExecution # control
                                # (pfromData (NativeScriptScan.ptokenHead'tag h) + 3)
                                # nextOffset
                                # pfromData (pphaseANative'stackRoot c)
                                # pfromData (pphaseANative'stackDepth c)
                                # nextNodeCount # pfromData (pphaseANative'result c)
                            )
                    )
                )
    )
    (pconstant False)

pverifyPhaseANativeTimelockPayloadScanV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PPhaseANativeScriptsControlV1 :--> BoundedItem.PChunkProofV1
        :--> PMaybeData BoundedItem.PChunkProofV1 :--> PBool
    )
pverifyPhaseANativeTimelockPayloadScanV1 = phoistAcyclic $ plam $
  \pre witness control chunkProof nextChunkProof ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pphaseANative'compactCbor c)
        # pfromData (pphaseANative'witnessSetCompactCbor c)
        # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified witnessSet) ->
  pmatch verified $ \verifiedSource ->
  pmatch (pverified'txCompact verifiedSource) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
  pif
    ( pphaseANativeControlIsBound # pre # witness # control # verified # witnessSet
        #&& ( pfromData (pphaseANative'stage c) #== 7
                #|| pfromData (pphaseANative'stage c) #== 8
            )
    )
    ( pmatch (pphaseANativeChunkWindow # control # chunkProof # nextChunkProof) $ \case
        PNothing -> pconstant False
        PJust authenticated -> pmatch authenticated $ \(PPair windowBytes windowOffset) ->
          pmatch
            ( NativeScriptScan.ptimelockPayloadAtV1
                # windowBytes # windowOffset # pfromData (pphaseANative'cursor c)
                # (pfromData (pphaseANative'stage c) - 3)
            )
            $ \case
              PNothing -> prejectedSuccessorIsExact
                # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                # pconstant "E_INVALID_FIELD_TYPE"
              PJust token -> pmatch token $ \t ->
                plet
                  ( pif (pfromData (NativeScriptScan.ptoken'kind t) #== NativeScriptScan.pafterNode)
                      ( pbodyCompact'validityIntervalStart body #>= 0
                          #&& pbodyCompact'validityIntervalStart body #>= pfromData (NativeScriptScan.ptoken'slot t)
                      )
                      ( pbodyCompact'validityIntervalEnd body #>= 0
                          #&& pbodyCompact'validityIntervalEnd body #<= pfromData (NativeScriptScan.ptoken'slot t)
                      )
                  )
                  $ \valid ->
                pfromData (NativeScriptScan.ptoken'nextOffset t) #<= pfromData (pphaseANative'itemLength c)
                  #&& pphaseANativeSuccessorIsExact
                    # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                    # ( pphaseANativeSetExecution # control # 2
                          # pfromData (NativeScriptScan.ptoken'nextOffset t)
                          # pfromData (pphaseANative'stackRoot c)
                          # pfromData (pphaseANative'stackDepth c)
                          # pfromData (pphaseANative'nodeCount c)
                          # pif valid 1 0
                      )
    )
    (pconstant False)

pverifyPhaseANativeContainerPayloadScanV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PPhaseANativeScriptsControlV1 :--> BoundedItem.PChunkProofV1
        :--> PMaybeData BoundedItem.PChunkProofV1 :--> PInteger :--> PBool
        :--> PBool
    )
pverifyPhaseANativeContainerPayloadScanV1 = phoistAcyclic $ plam $
  \pre witness control chunkProof nextChunkProof payloadMode expectEmpty ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pphaseANative'compactCbor c)
        # pfromData (pphaseANative'witnessSetCompactCbor c)
        # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified witnessSet) ->
  plet (pfromData $ pphaseANative'stage c) $ \stage ->
  pif
    ( pphaseANativeControlIsBound # pre # witness # control # verified # witnessSet
        #&& pif (payloadMode #== 0)
          (stage #== 4 #|| stage #== 5)
          (stage #== 6)
    )
    ( pmatch (pphaseANativeChunkWindow # control # chunkProof # nextChunkProof) $ \case
        PNothing -> pconstant False
        PJust authenticated -> pmatch authenticated $ \(PPair windowBytes windowOffset) ->
          plet
            ( pif (payloadMode #== 0)
                ( NativeScriptScan.pallOrAnyPayloadAtV1
                    # windowBytes # windowOffset # pfromData (pphaseANative'cursor c)
                    # (stage - 3)
                )
                ( NativeScriptScan.patLeastPayloadAtV1
                    # windowBytes # windowOffset # pfromData (pphaseANative'cursor c)
                )
            )
            $ \payload ->
          pmatch payload $ \case
            PNothing -> prejectedSuccessorIsExact
              # pre # pfromData (poneStep'claimedSuccessor stepWitness)
              # pconstant "E_INVALID_FIELD_TYPE"
            PJust token -> pmatch token $ \t ->
              pif expectEmpty
                ( pif (pfromData (NativeScriptScan.ptoken'childCount t) #/= 0)
                    (pconstant False)
                    ( pmatch (NativeScriptScan.pemptyContainerResultV1 # token) $ \case
                        PNothing -> pconstant False
                        PJust valid ->
                          pfromData (NativeScriptScan.ptoken'nextOffset t) #<= pfromData (pphaseANative'itemLength c)
                            #&& pphaseANativeSuccessorIsExact
                              # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                              # ( pphaseANativeSetExecution # control # 2
                                    # pfromData (NativeScriptScan.ptoken'nextOffset t)
                                    # pfromData (pphaseANative'stackRoot c)
                                    # pfromData (pphaseANative'stackDepth c)
                                    # pfromData (pphaseANative'nodeCount c)
                                    # pif valid 1 0
                                )
                    )
                )
                ( pif (pfromData (NativeScriptScan.ptoken'childCount t) #<= 0)
                    (pconstant False)
                    ( pmatch
                        ( NativeScriptScan.pframeForTokenV1
                            # token # pfromData (pphaseANative'stackRoot c)
                        )
                        $ \case
                          PNothing -> pconstant False
                          PJust frame ->
                            plet (pfromData (pphaseANative'stackDepth c) + 1) $ \nextDepth ->
                            pif (nextDepth #> NativeScriptScan.pmaxNativeScriptDepth)
                              ( prejectedSuccessorIsExact
                                  # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                                  # pconstant "E_NATIVE_SCRIPT_DEPTH"
                              )
                              ( pfromData (NativeScriptScan.ptoken'nextOffset t) #<= pfromData (pphaseANative'itemLength c)
                                  #&& pphaseANativeSuccessorIsExact
                                    # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                                    # ( pphaseANativeSetExecution # control # 1
                                          # pfromData (NativeScriptScan.ptoken'nextOffset t)
                                          # (NativeScriptScan.phashFrameV1 # frame)
                                          # nextDepth
                                          # pfromData (pphaseANative'nodeCount c)
                                          # pfromData (pphaseANative'result c)
                                      )
                              )
                    )
                )
    )
    (pconstant False)

pverifyPhaseANativeAllOrAnyContainerFramePayloadScanV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PPhaseANativeScriptsControlV1 :--> BoundedItem.PChunkProofV1
        :--> PMaybeData BoundedItem.PChunkProofV1 :--> PBool
    )
pverifyPhaseANativeAllOrAnyContainerFramePayloadScanV1 = phoistAcyclic $ plam $
  \pre witness control chunkProof nextChunkProof ->
    pverifyPhaseANativeContainerPayloadScanV1
      # pre # witness # control # chunkProof # nextChunkProof # 0 # pconstant False

pverifyPhaseANativeAllOrAnyEmptyContainerPayloadScanV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PPhaseANativeScriptsControlV1 :--> BoundedItem.PChunkProofV1
        :--> PMaybeData BoundedItem.PChunkProofV1 :--> PBool
    )
pverifyPhaseANativeAllOrAnyEmptyContainerPayloadScanV1 = phoistAcyclic $ plam $
  \pre witness control chunkProof nextChunkProof ->
    pverifyPhaseANativeContainerPayloadScanV1
      # pre # witness # control # chunkProof # nextChunkProof # 0 # pconstant True

pverifyPhaseANativeAtLeastContainerFramePayloadScanV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PPhaseANativeScriptsControlV1 :--> BoundedItem.PChunkProofV1
        :--> PMaybeData BoundedItem.PChunkProofV1 :--> PBool
    )
pverifyPhaseANativeAtLeastContainerFramePayloadScanV1 = phoistAcyclic $ plam $
  \pre witness control chunkProof nextChunkProof ->
    pverifyPhaseANativeContainerPayloadScanV1
      # pre # witness # control # chunkProof # nextChunkProof # 1 # pconstant False

pverifyPhaseANativeAtLeastEmptyContainerPayloadScanV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PPhaseANativeScriptsControlV1 :--> BoundedItem.PChunkProofV1
        :--> PMaybeData BoundedItem.PChunkProofV1 :--> PBool
    )
pverifyPhaseANativeAtLeastEmptyContainerPayloadScanV1 = phoistAcyclic $ plam $
  \pre witness control chunkProof nextChunkProof ->
    pverifyPhaseANativeContainerPayloadScanV1
      # pre # witness # control # chunkProof # nextChunkProof # 1 # pconstant True

pverifyPhaseANativeSignaturePayloadScanV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PPhaseANativeScriptsControlV1 :--> BoundedItem.PChunkProofV1
        :--> PMaybeData BoundedItem.PChunkProofV1 :--> PSignerSetProofV1
        :--> PBool
    )
pverifyPhaseANativeSignaturePayloadScanV1 = phoistAcyclic $ plam $
  \pre witness control chunkProof nextChunkProof signerProof ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pphaseANative'compactCbor c)
        # pfromData (pphaseANative'witnessSetCompactCbor c)
        # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified witnessSet) ->
  pif
    ( pphaseANativeControlIsBound # pre # witness # control # verified # witnessSet
        #&& pfromData (pphaseANative'stage c) #== 3
    )
    ( pmatch (pphaseANativeChunkWindow # control # chunkProof # nextChunkProof) $ \case
        PNothing -> pconstant False
        PJust authenticated -> pmatch authenticated $ \(PPair windowBytes windowOffset) ->
          pmatch
            ( NativeScriptScan.psignaturePayloadAtV1
                # windowBytes # windowOffset # pfromData (pphaseANative'cursor c)
            )
            $ \case
              PNothing -> prejectedSuccessorIsExact
                # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                # pconstant "E_INVALID_FIELD_TYPE"
              PJust token -> pmatch token $ \t ->
                pmatch
                  ( pphaseANativeSignatureResult
                      # pfromData (NativeScriptScan.ptoken'keyHash t)
                      # control # signerProof
                  )
                  $ \case
                    PNothing -> pconstant False
                    PJust valid ->
                      pfromData (NativeScriptScan.ptoken'nextOffset t) #<= pfromData (pphaseANative'itemLength c)
                        #&& pphaseANativeSuccessorIsExact
                          # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                          # ( pphaseANativeSetExecution # control # 2
                                # pfromData (NativeScriptScan.ptoken'nextOffset t)
                                # pfromData (pphaseANative'stackRoot c)
                                # pfromData (pphaseANative'stackDepth c)
                                # pfromData (pphaseANative'nodeCount c)
                                # pif valid 1 0
                            )
    )
    (pconstant False)

pverifyPhaseANativeTokenScan :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PPhaseANativeScriptsControlV1 :--> BoundedItem.PChunkProofV1
        :--> PMaybeData BoundedItem.PChunkProofV1 :--> PSignerSetProofV1
        :--> PInteger :--> PBool
    )
pverifyPhaseANativeTokenScan = phoistAcyclic $ plam $
  \pre witness control chunkProof nextChunkProof signerProof tokenMode ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pphaseANative'compactCbor c)
        # pfromData (pphaseANative'witnessSetCompactCbor c)
        # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified witnessSet) ->
  pmatch verified $ \verifiedSource ->
  pmatch (pverified'txCompact verifiedSource) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
  pif
    ( pphaseANativeControlIsBound # pre # witness # control # verified # witnessSet
        #&& pfromData (pphaseANative'stage c) #== 1
    )
    ( pmatch (pphaseANativeChunkWindow # control # chunkProof # nextChunkProof) $ \case
        PNothing -> pconstant False
        PJust authenticated -> pmatch authenticated $ \(PPair windowBytes windowOffset) ->
          pmatch (NativeScriptScan.ptokenAtV1 # windowBytes # windowOffset # pfromData (pphaseANative'cursor c)) $ \case
            PNothing -> prejectedSuccessorIsExact
              # pre # pfromData (poneStep'claimedSuccessor stepWitness)
              # pconstant "E_INVALID_FIELD_TYPE"
            PJust token -> pmatch token $ \t ->
              plet (pfromData (pphaseANative'nodeCount c) + 1) $ \nextNodeCount ->
              pif (nextNodeCount #> NativeScriptScan.pmaxNativeScriptNodes)
                ( prejectedSuccessorIsExact
                    # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                    # pconstant "E_NATIVE_SCRIPT_NODE_COUNT"
                )
                ( pif
                    ( pfromData (NativeScriptScan.ptoken'kind t) #>= NativeScriptScan.pallNode
                        #&& pfromData (NativeScriptScan.ptoken'kind t) #<= NativeScriptScan.patLeastNode
                        #&& pfromData (NativeScriptScan.ptoken'childCount t) #> 0
                    )
                    ( pmatch (NativeScriptScan.pframeForTokenV1 # token # pfromData (pphaseANative'stackRoot c)) $ \case
                        PNothing -> pconstant False
                        PJust frame ->
                          plet (pfromData (pphaseANative'stackDepth c) + 1) $ \nextDepth ->
                          pif (nextDepth #> NativeScriptScan.pmaxNativeScriptDepth)
                            ( prejectedSuccessorIsExact
                                # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                                # pconstant "E_NATIVE_SCRIPT_DEPTH"
                            )
                            ( pand'List
                                [ tokenMode #/= 1
                                , pmatch signerProof $ \case PNoSignerSetProof -> pconstant True; _ -> pconstant False
                                , pfromData (NativeScriptScan.ptoken'nextOffset t) #<= pfromData (pphaseANative'itemLength c)
                                , pphaseANativeSuccessorIsExact
                                    # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                                    # ( pphaseANativeSetExecution # control # 1
                                          # pfromData (NativeScriptScan.ptoken'nextOffset t)
                                          # (NativeScriptScan.phashFrameV1 # frame)
                                          # nextDepth # nextNodeCount # (-1)
                                      )
                                ]
                            )
                    )
                    ( plet
                        ( pif (pfromData (NativeScriptScan.ptoken'kind t) #== NativeScriptScan.psignatureNode)
                            ( pif (tokenMode #/= 0)
                                (pphaseANativeSignatureResult # pfromData (NativeScriptScan.ptoken'keyHash t) # control # signerProof)
                                (pcon PNothing)
                            )
                            ( pif (pfromData (NativeScriptScan.ptoken'kind t) #== NativeScriptScan.pafterNode)
                                ( pif
                                    (tokenMode #/= 1 #&& pmatch signerProof (\case PNoSignerSetProof -> pconstant True; _ -> pconstant False))
                                    (pcon $ PJust $ pbodyCompact'validityIntervalStart body #>= 0 #&& pbodyCompact'validityIntervalStart body #>= pfromData (NativeScriptScan.ptoken'slot t))
                                    (pcon PNothing)
                                )
                                ( pif (pfromData (NativeScriptScan.ptoken'kind t) #== NativeScriptScan.pbeforeNode)
                                    ( pif
                                        (tokenMode #/= 1 #&& pmatch signerProof (\case PNoSignerSetProof -> pconstant True; _ -> pconstant False))
                                        (pcon $ PJust $ pbodyCompact'validityIntervalEnd body #>= 0 #&& pbodyCompact'validityIntervalEnd body #<= pfromData (NativeScriptScan.ptoken'slot t))
                                        (pcon PNothing)
                                    )
                                    ( pif
                                        (tokenMode #/= 1 #&& pmatch signerProof (\case PNoSignerSetProof -> pconstant True; _ -> pconstant False))
                                        (NativeScriptScan.pemptyContainerResultV1 # token)
                                        (pcon PNothing)
                                    )
                                )
                            )
                        )
                        $ \result ->
                      pmatch result $ \case
                        PNothing -> pconstant False
                        PJust valid ->
                          pfromData (NativeScriptScan.ptoken'nextOffset t) #<= pfromData (pphaseANative'itemLength c)
                            #&& pphaseANativeSuccessorIsExact
                              # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                              # ( pphaseANativeSetExecution # control # 2
                                    # pfromData (NativeScriptScan.ptoken'nextOffset t)
                                    # pfromData (pphaseANative'stackRoot c)
                                    # pfromData (pphaseANative'stackDepth c)
                                    # nextNodeCount # pif valid 1 0
                                )
                    )
                )
    )
    (pconstant False)

pverifyPhaseANativeTypedTokenScanV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PPhaseANativeScriptsControlV1 :--> BoundedItem.PChunkProofV1
        :--> PMaybeData BoundedItem.PChunkProofV1 :--> PSignerSetProofV1
        :--> PInteger :--> PBool
    )
pverifyPhaseANativeTypedTokenScanV1 = phoistAcyclic $ plam $
  \pre witness control chunkProof nextChunkProof signerProof expectedFamily ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pphaseANative'compactCbor c)
        # pfromData (pphaseANative'witnessSetCompactCbor c)
        # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified witnessSet) ->
  pmatch verified $ \verifiedSource ->
  pmatch (pverified'txCompact verifiedSource) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
  pif
    ( pphaseANativeControlIsBound # pre # witness # control # verified # witnessSet
        #&& pfromData (pphaseANative'stage c) #== 1
    )
    ( pmatch (pphaseANativeChunkWindow # control # chunkProof # nextChunkProof) $ \case
        PNothing -> pconstant False
        PJust authenticated -> pmatch authenticated $ \(PPair windowBytes windowOffset) ->
          pmatch (NativeScriptScan.ptokenHeadAtV1 # windowBytes # windowOffset) $ \case
            PNothing -> prejectedSuccessorIsExact
              # pre # pfromData (poneStep'claimedSuccessor stepWitness)
              # pconstant "E_INVALID_FIELD_TYPE"
            PJust headValue -> pmatch headValue $ \h ->
              plet (pfromData $ NativeScriptScan.ptokenHead'tag h) $ \tag ->
              plet
                ( pif (expectedFamily #<= 1)
                    (tag #>= NativeScriptScan.pallNode #&& tag #<= NativeScriptScan.patLeastNode)
                    ( pif (expectedFamily #== 2)
                        (tag #== NativeScriptScan.pafterNode #|| tag #== NativeScriptScan.pbeforeNode)
                        (tag #== NativeScriptScan.psignatureNode)
                    )
                )
                $ \isExpected ->
              pif (pnot # isExpected)
                ( pif
                    (tag #>= NativeScriptScan.psignatureNode #&& tag #<= NativeScriptScan.pbeforeNode)
                    (pconstant False)
                    ( prejectedSuccessorIsExact
                        # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                        # pconstant "E_INVALID_FIELD_TYPE"
                    )
                )
                ( plet
                    ( pif (expectedFamily #<= 1)
                        ( NativeScriptScan.pcontainerTokenFromHeadV1
                            # windowBytes # windowOffset
                            # pfromData (pphaseANative'cursor c) # headValue
                        )
                        ( pif (expectedFamily #== 2)
                            ( NativeScriptScan.ptimelockTokenFromHeadV1
                                # windowBytes # windowOffset
                                # pfromData (pphaseANative'cursor c) # headValue
                            )
                            ( NativeScriptScan.psignatureTokenFromHeadV1
                                # windowBytes # windowOffset
                                # pfromData (pphaseANative'cursor c) # headValue
                            )
                        )
                    )
                    $ \parsedToken ->
                  pmatch parsedToken $ \case
                    PNothing -> prejectedSuccessorIsExact
                      # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                      # pconstant "E_INVALID_FIELD_TYPE"
                    PJust token -> pmatch token $ \t ->
                      plet (pfromData (pphaseANative'nodeCount c) + 1) $ \nextNodeCount ->
                      pif (nextNodeCount #> NativeScriptScan.pmaxNativeScriptNodes)
                        ( prejectedSuccessorIsExact
                            # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                            # pconstant "E_NATIVE_SCRIPT_NODE_COUNT"
                        )
                        ( pif (expectedFamily #== 0)
                            ( pif (pfromData (NativeScriptScan.ptoken'childCount t) #<= 0)
                                (pconstant False)
                                ( pmatch
                                    ( NativeScriptScan.pframeForTokenV1
                                        # token # pfromData (pphaseANative'stackRoot c)
                                    )
                                    $ \case
                                      PNothing -> pconstant False
                                      PJust frame ->
                                        plet (pfromData (pphaseANative'stackDepth c) + 1) $ \nextDepth ->
                                        pif (nextDepth #> NativeScriptScan.pmaxNativeScriptDepth)
                                          ( prejectedSuccessorIsExact
                                              # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                                              # pconstant "E_NATIVE_SCRIPT_DEPTH"
                                          )
                                          ( pfromData (NativeScriptScan.ptoken'nextOffset t)
                                                #<= pfromData (pphaseANative'itemLength c)
                                              #&& pphaseANativeSuccessorIsExact
                                                # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                                                # ( pphaseANativeSetExecution # control # 1
                                                      # pfromData (NativeScriptScan.ptoken'nextOffset t)
                                                      # (NativeScriptScan.phashFrameV1 # frame)
                                                      # nextDepth # nextNodeCount
                                                      # pfromData (pphaseANative'result c)
                                                  )
                                          )
                                )
                            )
                            ( pif (expectedFamily #== 1)
                                ( pif (pfromData (NativeScriptScan.ptoken'childCount t) #/= 0)
                                    (pconstant False)
                                    ( pmatch (NativeScriptScan.pemptyContainerResultV1 # token) $ \case
                                        PNothing -> pconstant False
                                        PJust valid ->
                                          pfromData (NativeScriptScan.ptoken'nextOffset t)
                                              #<= pfromData (pphaseANative'itemLength c)
                                            #&& pphaseANativeSuccessorIsExact
                                              # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                                              # ( pphaseANativeSetExecution # control # 2
                                                    # pfromData (NativeScriptScan.ptoken'nextOffset t)
                                                    # pfromData (pphaseANative'stackRoot c)
                                                    # pfromData (pphaseANative'stackDepth c)
                                                    # nextNodeCount # pif valid 1 0
                                                )
                                    )
                                )
                                ( pif (expectedFamily #== 2)
                                    ( plet
                                        ( pif
                                            (pfromData (NativeScriptScan.ptoken'kind t) #== NativeScriptScan.pafterNode)
                                            ( pbodyCompact'validityIntervalStart body #>= 0
                                                #&& pbodyCompact'validityIntervalStart body
                                                  #>= pfromData (NativeScriptScan.ptoken'slot t)
                                            )
                                            ( pbodyCompact'validityIntervalEnd body #>= 0
                                                #&& pbodyCompact'validityIntervalEnd body
                                                  #<= pfromData (NativeScriptScan.ptoken'slot t)
                                            )
                                        )
                                        $ \valid ->
                                      pfromData (NativeScriptScan.ptoken'nextOffset t)
                                          #<= pfromData (pphaseANative'itemLength c)
                                        #&& pphaseANativeSuccessorIsExact
                                          # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                                          # ( pphaseANativeSetExecution # control # 2
                                                # pfromData (NativeScriptScan.ptoken'nextOffset t)
                                                # pfromData (pphaseANative'stackRoot c)
                                                # pfromData (pphaseANative'stackDepth c)
                                                # nextNodeCount # pif valid 1 0
                                            )
                                    )
                                    ( pmatch
                                        ( pphaseANativeSignatureResult
                                            # pfromData (NativeScriptScan.ptoken'keyHash t)
                                            # control # signerProof
                                        )
                                        $ \case
                                          PNothing -> pconstant False
                                          PJust valid ->
                                            pfromData (NativeScriptScan.ptoken'nextOffset t)
                                                #<= pfromData (pphaseANative'itemLength c)
                                              #&& pphaseANativeSuccessorIsExact
                                                # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                                                # ( pphaseANativeSetExecution # control # 2
                                                      # pfromData (NativeScriptScan.ptoken'nextOffset t)
                                                      # pfromData (pphaseANative'stackRoot c)
                                                      # pfromData (pphaseANative'stackDepth c)
                                                      # nextNodeCount # pif valid 1 0
                                                  )
                                    )
                                )
                            )
                        )
                )
    )
    (pconstant False)

pverifyPhaseANativeFrameScan :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PPhaseANativeScriptsControlV1 :--> NativeScriptScan.PNativeScriptFrameV1
        :--> PBool
    )
pverifyPhaseANativeFrameScan = phoistAcyclic $ plam $ \pre witness control frame ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pphaseANative'compactCbor c)
        # pfromData (pphaseANative'witnessSetCompactCbor c)
        # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified witnessSet) ->
  pif
    ( pand'List
        [ pphaseANativeControlIsBound # pre # witness # control # verified # witnessSet
        , pfromData (pphaseANative'stage c) #== 2
        , plengthBS # pfromData (pphaseANative'stackRoot c) #== 32
        , pfromData (pphaseANative'stackDepth c) #> 0
        , NativeScriptScan.phashFrameV1 # frame #== pfromData (pphaseANative'stackRoot c)
        ]
    )
    ( pmatch (NativeScriptScan.papplyChildV1 # frame # (pfromData (pphaseANative'result c) #== 1)) $ \case
        PNothing -> pconstant False
        PJust frameResult -> pmatch frameResult $ \case
          NativeScriptScan.PNativeFramePending nextFrameD ->
            plet (pfromData nextFrameD) $ \nextFrame ->
              pphaseANativeSuccessorIsExact
                # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                # ( pphaseANativeSetExecution # control # 1
                      # pfromData (pphaseANative'cursor c)
                      # (NativeScriptScan.phashFrameV1 # nextFrame)
                      # pfromData (pphaseANative'stackDepth c)
                      # pfromData (pphaseANative'nodeCount c) # (-1)
                  )
          NativeScriptScan.PNativeFrameComplete tailD validD ->
            pphaseANativeSuccessorIsExact
              # pre # pfromData (poneStep'claimedSuccessor stepWitness)
              # ( pphaseANativeSetExecution # control # 2
                    # pfromData (pphaseANative'cursor c) # pfromData tailD
                    # (pfromData (pphaseANative'stackDepth c) - 1)
                    # pfromData (pphaseANative'nodeCount c)
                    # pif (pfromData validD) 1 0
                )
    )
    (pconstant False)

pverifyPhaseANativeFinalizeScan :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PPhaseANativeScriptsControlV1 :--> PBool
    )
pverifyPhaseANativeFinalizeScan = phoistAcyclic $ plam $ \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pphaseANative'compactCbor c)
        # pfromData (pphaseANative'witnessSetCompactCbor c)
        # pfromData (pphaseANative'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified witnessSet) ->
  pif
    ( pand'List
        [ pphaseANativeControlIsBound # pre # witness # control # verified # witnessSet
        , pfromData (pphaseANative'stage c) #== 2
        , pfromData (pphaseANative'stackRoot c) #== pconstant ""
        , pfromData (pphaseANative'stackDepth c) #== 0
        ]
    )
    ( pif (pfromData (pphaseANative'cursor c) #/= pfromData (pphaseANative'itemLength c))
        (prejectedSuccessorIsExact # pre # pfromData (poneStep'claimedSuccessor stepWitness) # pconstant "E_INVALID_FIELD_TYPE")
        ( pif (pfromData (pphaseANative'result c) #== 0)
            (prejectedSuccessorIsExact # pre # pfromData (poneStep'claimedSuccessor stepWitness) # pconstant "E_NATIVE_SCRIPT_INVALID")
            ( pphaseANativeCompleteScriptIsExact
                # pre # pfromData (poneStep'claimedSuccessor stepWitness) # control
                # pfromData (pphaseANative'scriptCount c)
                # (pfromData (pphaseANative'scriptSeen c) + 1)
                # pfromData (pphaseANative'containsNonNativeScript c)
            )
        )
    )
    (pconstant False)

pverifyPhaseANativeTokenSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1 :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PSignerSetProofV1 :--> PBool
    )
pverifyPhaseANativeTokenSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof signerProof ->
  pmatch witness $ \stepWitness ->
    pverifyPhaseANativeTokenScan # pre # witness
      # (pphaseANativeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
      # chunkProof # nextChunkProof # signerProof # 2

pverifyPhaseANativeTokenHeadSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1 :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PBool
    )
pverifyPhaseANativeTokenHeadSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof ->
  pmatch witness $ \stepWitness ->
    pverifyPhaseANativeTokenHeadScanV1 # pre # witness
      # (pphaseANativeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
      # chunkProof # nextChunkProof

pverifyPhaseANativeTimelockPayloadSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1 :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PBool
    )
pverifyPhaseANativeTimelockPayloadSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof ->
  pmatch witness $ \stepWitness ->
    pverifyPhaseANativeTimelockPayloadScanV1 # pre # witness
      # (pphaseANativeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
      # chunkProof # nextChunkProof

pverifyPhaseANativeAllOrAnyContainerFramePayloadSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1 :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PBool
    )
pverifyPhaseANativeAllOrAnyContainerFramePayloadSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof ->
  pmatch witness $ \stepWitness ->
    pverifyPhaseANativeAllOrAnyContainerFramePayloadScanV1 # pre # witness
      # (pphaseANativeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
      # chunkProof # nextChunkProof

pverifyPhaseANativeAllOrAnyEmptyContainerPayloadSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1 :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PBool
    )
pverifyPhaseANativeAllOrAnyEmptyContainerPayloadSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof ->
  pmatch witness $ \stepWitness ->
    pverifyPhaseANativeAllOrAnyEmptyContainerPayloadScanV1 # pre # witness
      # (pphaseANativeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
      # chunkProof # nextChunkProof

pverifyPhaseANativeAtLeastContainerFramePayloadSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1 :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PBool
    )
pverifyPhaseANativeAtLeastContainerFramePayloadSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof ->
  pmatch witness $ \stepWitness ->
    pverifyPhaseANativeAtLeastContainerFramePayloadScanV1 # pre # witness
      # (pphaseANativeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
      # chunkProof # nextChunkProof

pverifyPhaseANativeAtLeastEmptyContainerPayloadSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1 :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PBool
    )
pverifyPhaseANativeAtLeastEmptyContainerPayloadSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof ->
  pmatch witness $ \stepWitness ->
    pverifyPhaseANativeAtLeastEmptyContainerPayloadScanV1 # pre # witness
      # (pphaseANativeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
      # chunkProof # nextChunkProof

pverifyPhaseANativeSignaturePayloadSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1 :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PSignerSetProofV1 :--> PBool
    )
pverifyPhaseANativeSignaturePayloadSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof signerProof ->
  pmatch witness $ \stepWitness ->
    pverifyPhaseANativeSignaturePayloadScanV1 # pre # witness
      # (pphaseANativeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
      # chunkProof # nextChunkProof # signerProof

pverifyPhaseANativeSignatureMembershipPayloadSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1 :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PBuiltinList (PAsData PFrontierPeak) :--> PInteger
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pverifyPhaseANativeSignatureMembershipPayloadSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof peaks signerIndex siblings ->
    pverifyPhaseANativeSignaturePayloadSemanticsV1
      # pre # witness # chunkProof # nextChunkProof
      # pcon (PSignerMembershipProof (pdata peaks) (pdata signerIndex) (pdata siblings))

pverifyPhaseANativeSignatureEmptyPayloadSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1 :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PBuiltinList (PAsData PFrontierPeak) :--> PBool
    )
pverifyPhaseANativeSignatureEmptyPayloadSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof peaks ->
    pverifyPhaseANativeSignaturePayloadSemanticsV1
      # pre # witness # chunkProof # nextChunkProof
      # pcon (PEmptySignerSetProof $ pdata peaks)

pverifyPhaseANativeSignatureBelowFirstPayloadSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1 :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PBuiltinList (PAsData PFrontierPeak) :--> PByteString
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pverifyPhaseANativeSignatureBelowFirstPayloadSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof peaks firstSignerHash siblings ->
    pverifyPhaseANativeSignaturePayloadSemanticsV1
      # pre # witness # chunkProof # nextChunkProof
      # pcon (PSignerBelowFirstProof (pdata peaks) (pdata firstSignerHash) (pdata siblings))

pverifyPhaseANativeSignatureAboveLastPayloadSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1 :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PBuiltinList (PAsData PFrontierPeak) :--> PByteString
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pverifyPhaseANativeSignatureAboveLastPayloadSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof peaks lastSignerHash siblings ->
    pverifyPhaseANativeSignaturePayloadSemanticsV1
      # pre # witness # chunkProof # nextChunkProof
      # pcon (PSignerAboveLastProof (pdata peaks) (pdata lastSignerHash) (pdata siblings))

pverifyPhaseANativeSignatureBetweenPayloadSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1 :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PBuiltinList (PAsData PFrontierPeak) :--> PInteger :--> PByteString
        :--> PBuiltinList (PAsData PByteString) :--> PByteString
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pverifyPhaseANativeSignatureBetweenPayloadSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof peaks lowerIndex lowerSignerHash lowerSiblings upperSignerHash upperSiblings ->
    pverifyPhaseANativeSignaturePayloadSemanticsV1
      # pre # witness # chunkProof # nextChunkProof
      # pcon
        ( PSignerBetweenProof
            (pdata peaks) (pdata lowerIndex) (pdata lowerSignerHash)
            (pdata lowerSiblings) (pdata upperSignerHash) (pdata upperSiblings)
        )

pverifyPhaseANativeContainerFrameTokenSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1 :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PBool
    )
pverifyPhaseANativeContainerFrameTokenSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof ->
  pmatch witness $ \stepWitness ->
    pverifyPhaseANativeTypedTokenScanV1 # pre # witness
      # (pphaseANativeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
      # chunkProof # nextChunkProof # pcon PNoSignerSetProof # 0

pverifyPhaseANativeEmptyContainerTokenSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1 :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PBool
    )
pverifyPhaseANativeEmptyContainerTokenSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof ->
  pmatch witness $ \stepWitness ->
    pverifyPhaseANativeTypedTokenScanV1 # pre # witness
      # (pphaseANativeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
      # chunkProof # nextChunkProof # pcon PNoSignerSetProof # 1

pverifyPhaseANativeTimelockTokenSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1 :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PBool
    )
pverifyPhaseANativeTimelockTokenSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof ->
  pmatch witness $ \stepWitness ->
    pverifyPhaseANativeTypedTokenScanV1 # pre # witness
      # (pphaseANativeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
      # chunkProof # nextChunkProof # pcon PNoSignerSetProof # 2

pverifyPhaseANativeSignatureTokenSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1 :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PSignerSetProofV1 :--> PBool
    )
pverifyPhaseANativeSignatureTokenSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof signerProof ->
  pmatch witness $ \stepWitness ->
    pverifyPhaseANativeTypedTokenScanV1 # pre # witness
      # (pphaseANativeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
      # chunkProof # nextChunkProof # signerProof # 3

pverifyPhaseANativeFrameSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> NativeScriptScan.PNativeScriptFrameV1 :--> PBool
    )
pverifyPhaseANativeFrameSemanticsV1 = phoistAcyclic $ plam $ \pre witness frame ->
  pmatch witness $ \stepWitness ->
    pverifyPhaseANativeFrameScan # pre # witness
      # (pphaseANativeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
      # frame

pverifyPhaseANativeScripts :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyPhaseANativeScripts = phoistAcyclic $ plam $ \pre witness auxiliary ->
  pmatch witness $ \stepWitness ->
  plet
    (pphaseANativeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  plet (pfromData $ pphaseANative'stage c) $ \stage ->
  pmatch auxiliary $ \case
    PNoAuxiliaryWitness ->
      pif (stage #== 2)
        (pverifyPhaseANativeFinalizeScan # pre # witness # control)
        (pverifyPhaseANativeAdvanceScan # pre # witness # control)
    PTransactionFieldChunkWitness collectionProofD chunkProofD ->
      pverifyPhaseANativeItemScan # pre # witness # control
        # pfromData collectionProofD # pfromData chunkProofD
    PNativeScriptTokenWitness chunkProofD nextChunkProofD signerProofD ->
      plet (pfromData signerProofD) $ \signerProof ->
      plet
        (pmatch signerProof $ \case PNoSignerSetProof -> pconstant True; _ -> pconstant False)
        $ \hasNoSignerProof ->
      pif (stage #== 1)
        ( hasNoSignerProof
            #&& pverifyPhaseANativeTokenHeadScanV1 # pre # witness # control
              # pfromData chunkProofD # pfromData nextChunkProofD
        )
        ( pif (stage #== 3)
            ( pverifyPhaseANativeSignaturePayloadScanV1 # pre # witness # control
                # pfromData chunkProofD # pfromData nextChunkProofD # signerProof
            )
            ( pif (stage #== 4 #|| stage #== 5)
                ( hasNoSignerProof
                    #&& ( pverifyPhaseANativeAllOrAnyContainerFramePayloadScanV1
                            # pre # witness # control # pfromData chunkProofD
                            # pfromData nextChunkProofD
                          #|| pverifyPhaseANativeAllOrAnyEmptyContainerPayloadScanV1
                            # pre # witness # control # pfromData chunkProofD
                            # pfromData nextChunkProofD
                        )
                )
                ( pif (stage #== 6)
                    ( hasNoSignerProof
                        #&& ( pverifyPhaseANativeAtLeastContainerFramePayloadScanV1
                                # pre # witness # control # pfromData chunkProofD
                                # pfromData nextChunkProofD
                              #|| pverifyPhaseANativeAtLeastEmptyContainerPayloadScanV1
                                # pre # witness # control # pfromData chunkProofD
                                # pfromData nextChunkProofD
                            )
                    )
                    ( pif (stage #== 7 #|| stage #== 8)
                        ( hasNoSignerProof
                            #&& pverifyPhaseANativeTimelockPayloadScanV1
                              # pre # witness # control # pfromData chunkProofD
                              # pfromData nextChunkProofD
                        )
                        (pconstant False)
                    )
                )
            )
        )
    PNativeScriptFrameWitness frameD ->
      pverifyPhaseANativeFrameScan # pre # witness # control # pfromData frameD
    _ -> pconstant False

pverifyPhaseANativeScriptsOneStepV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyPhaseANativeScriptsOneStepV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch pre $ \preState ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transitionD auxiliaryD) ->
  plet (pfromData transitionD) $ \witness ->
  pfromData (pmachineState'phase preState) #== pcon PPhaseANativeScripts
    #&& pstructuralTransitionIsValid # pre # witness
    #&& pverifyPhaseANativeScripts # pre # witness # pdecodeValidationAuxiliaryWitnessV1 auxiliaryD

pverifyPhaseANativeScriptsSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyPhaseANativeScriptsSemanticsV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transitionD auxiliaryD) ->
    pverifyPhaseANativeScripts # pre # pfromData transitionD # pdecodeValidationAuxiliaryWitnessV1 auxiliaryD

pphaseAScriptPreconditionsRejection :: forall s.
  Term s (PInteger :--> PBool :--> PBool :--> PByteString :--> PInteger :--> PMaybe PByteString)
pphaseAScriptPreconditionsRejection = phoistAcyclic $ plam $
  \observerCount hasRedeemers containsNonNativeScript scriptIntegrityHash networkId ->
  plet (pconstant $ BS.replicate 32 0) $ \zeroHash ->
  plet
    (containsNonNativeScript #|| hasRedeemers #|| scriptIntegrityHash #/= zeroHash)
    $ \requiresPlutusEvaluation ->
  pif (pnot # requiresPlutusEvaluation)
    (pcon PNothing)
    ( pif (scriptIntegrityHash #== zeroHash)
        (pcon $ PJust $ pconstant "E_INVALID_FIELD_TYPE")
        ( pif (observerCount #> 0 #&& networkId #== 255)
            (pcon $ PJust $ pconstant "E_INVALID_FIELD_TYPE")
            (pcon PNothing)
        )
    )

pphaseAScriptPreconditionsControlIsBound :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PPhaseAScriptPreconditionsControlV1 :--> PByteString :--> PBool
    )
pphaseAScriptPreconditionsControlIsBound = phoistAcyclic $ plam $
  \pre witness control observerCommitment ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  plet (observerCommitment #== NativeField.pemptyFieldCommitment) $ \observersAreEmpty ->
    pand'List
      [ NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (pphaseAPreconditions'compactCbor c)
          # pfromData (pphaseAPreconditions'witnessSetCompactCbor c)
          # pfromData (pphaseAPreconditions'fieldPreimageLengthsCbor c)
          #== pfromData (pmachineState'transactionCommitment preState)
      , phashValidationContext # pfromData (pphaseAPreconditions'contextCbor c)
          #== pfromData (pmachineState'validationContextHash preState)
      , plengthBS # pfromData (pphaseAPreconditions'resolutionScheduleHash c) #== 32
      , pfromData (pphaseAPreconditions'signerCount c) #>= 0
      , plengthBS # pfromData (pphaseAPreconditions'signerFrontierCommitment c) #== 32
      , pfromData (pphaseAPreconditions'containsNonNativeScript c) #== 0
          #|| pfromData (pphaseAPreconditions'containsNonNativeScript c) #== 1
      , pfromData (pphaseAPreconditions'observerCount c) #>= 0
      , pfromData (pphaseAPreconditions'observerCount c) #<= pmaxTxSizeDerivedItemCount
      , pfromData (pphaseAPreconditions'observerSeen c) #>= 0
      , pif observersAreEmpty
          ( pand'List
              [ pfromData (pphaseAPreconditions'observerCount c) #== 0
              , pfromData (pphaseAPreconditions'observerSeen c) #== 0
              , pfromData (pphaseAPreconditions'previousObserver c) #== pconstant ""
              ]
          )
          ( pif (pfromData (pphaseAPreconditions'observerCount c) #== 0)
              ( pfromData (pphaseAPreconditions'observerSeen c) #== 0
                  #&& pfromData (pphaseAPreconditions'previousObserver c) #== pconstant ""
              )
              ( pfromData (pphaseAPreconditions'observerCount c) #> 0
                  #&& pfromData (pphaseAPreconditions'observerSeen c)
                    #<= pfromData (pphaseAPreconditions'observerCount c)
                  #&& pif (pfromData (pphaseAPreconditions'observerSeen c) #== 0)
                    (pfromData (pphaseAPreconditions'previousObserver c) #== pconstant "")
                    (plengthBS # pfromData (pphaseAPreconditions'previousObserver c) #== 28)
              )
          )
      , pfromData (poneStep'workWitnessCbor stepWitness)
          #== pencodePhaseAScriptPreconditionsWitness
            # pfromData (pphaseAPreconditions'compactCbor c)
            # pfromData (pphaseAPreconditions'witnessSetCompactCbor c)
            # pfromData (pphaseAPreconditions'fieldPreimageLengthsCbor c)
            # pfromData (pphaseAPreconditions'contextCbor c)
            # pfromData (pphaseAPreconditions'resolutionScheduleHash c)
            # pfromData (pphaseAPreconditions'signerCount c)
            # pfromData (pphaseAPreconditions'signerFrontierCommitment c)
            # (pfromData (pphaseAPreconditions'containsNonNativeScript c) #== 1)
            # pfromData (pphaseAPreconditions'observerCount c)
            # pfromData (pphaseAPreconditions'observerSeen c)
            # pfromData (pphaseAPreconditions'previousObserver c)
      ]

pphaseAScriptPreconditionsSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationMachineStateV1
        :--> PPhaseAScriptPreconditionsControlV1 :--> PBool
    )
pphaseAScriptPreconditionsSuccessorIsExact = phoistAcyclic $ plam $ \pre post control ->
  pmatch pre $ \preState ->
  pmatch post $ \postState ->
  pmatch control $ \c ->
    pfromData (pmachineState'phase postState) #== pcon PPhaseAScriptPreconditions
      #&& pfromData (pmachineState'workRoot postState)
        #== phashWorkWitness # pcon PPhaseAScriptPreconditions
          # (pfromData (pmachineState'programCounter preState) + 1)
          # ( pencodePhaseAScriptPreconditionsWitness
                # pfromData (pphaseAPreconditions'compactCbor c)
                # pfromData (pphaseAPreconditions'witnessSetCompactCbor c)
                # pfromData (pphaseAPreconditions'fieldPreimageLengthsCbor c)
                # pfromData (pphaseAPreconditions'contextCbor c)
                # pfromData (pphaseAPreconditions'resolutionScheduleHash c)
                # pfromData (pphaseAPreconditions'signerCount c)
                # pfromData (pphaseAPreconditions'signerFrontierCommitment c)
                # (pfromData (pphaseAPreconditions'containsNonNativeScript c) #== 1)
                # pfromData (pphaseAPreconditions'observerCount c)
                # pfromData (pphaseAPreconditions'observerSeen c)
                # pfromData (pphaseAPreconditions'previousObserver c)
            )

pphaseAScriptPreconditionsFinalize :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PPhaseAScriptPreconditionsControlV1
        :--> PVerifiedMidgardNativeTxCompact :--> PBool :--> PInteger :--> PBool
    )
pphaseAScriptPreconditionsFinalize = phoistAcyclic $ plam $
  \pre witness control verifiedSource hasRedeemers observerCount ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch verifiedSource $ \verified ->
  pmatch (pverified'txCompact verified) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
  pmatch
    ( pphaseAScriptPreconditionsRejection
        # observerCount # hasRedeemers
        # (pfromData (pphaseAPreconditions'containsNonNativeScript c) #== 1)
        # pbodyCompact'scriptIntegrityHash body # pbodyCompact'networkId body
    )
    $ \case
      PJust rejectionCode ->
        prejectedSuccessorIsExact
          # pre # pfromData (poneStep'claimedSuccessor stepWitness) # rejectionCode
      PNothing ->
        pmatch (pfromData $ poneStep'claimedSuccessor stepWitness) $ \postState ->
          pfromData (pmachineState'phase postState) #== pcon PResolveInputs
            #&& pfromData (pmachineState'workRoot postState)
              #== phashWorkWitness # pcon PResolveInputs
                # (pfromData (pmachineState'programCounter preState) + 1)
                # ( pencodeResolveInputsWitness
                      # pfromData (pphaseAPreconditions'compactCbor c)
                      # pfromData (pphaseAPreconditions'witnessSetCompactCbor c)
                      # pfromData (pphaseAPreconditions'fieldPreimageLengthsCbor c)
                      # pfromData (pphaseAPreconditions'contextCbor c)
                      # 0 # pinitialResolutionAccumulator
                      # pfromData (pphaseAPreconditions'resolutionScheduleHash c)
                      # pfromData (pphaseAPreconditions'signerCount c)
                      # pfromData (pphaseAPreconditions'signerFrontierCommitment c)
                      # pcon PDNothing
                      # pfromData (pphaseAPreconditions'resolutionScheduleHash c)
                  )

pphaseAScriptPreconditionsWithObserver :: forall s.
  Term s
    ( PPhaseAScriptPreconditionsControlV1 :--> PInteger :--> PInteger
        :--> PByteString :--> PPhaseAScriptPreconditionsControlV1
    )
pphaseAScriptPreconditionsWithObserver = phoistAcyclic $ plam $
  \control observerCount observerSeen previousObserver ->
  pmatch control $ \c ->
    pcon $ PPhaseAScriptPreconditionsControlV1
      (pphaseAPreconditions'compactCbor c)
      (pphaseAPreconditions'witnessSetCompactCbor c)
      (pphaseAPreconditions'fieldPreimageLengthsCbor c)
      (pphaseAPreconditions'contextCbor c)
      (pphaseAPreconditions'resolutionScheduleHash c)
      (pphaseAPreconditions'signerCount c)
      (pphaseAPreconditions'signerFrontierCommitment c)
      (pphaseAPreconditions'containsNonNativeScript c)
      (pdata observerCount)
      (pdata observerSeen)
      (pdata previousObserver)

pverifyPhaseAScriptPreconditions :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyPhaseAScriptPreconditions = phoistAcyclic $ plam $ \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    ( pphaseAScriptPreconditionsControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pphaseAPreconditions'compactCbor c)
        # pfromData (pphaseAPreconditions'witnessSetCompactCbor c)
        # pfromData (pphaseAPreconditions'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource witnessSet) ->
  pmatch verifiedSource $ \verified ->
  pmatch (pverified'txCompact verified) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
  pmatch witnessSet $ \ws ->
  plet (pbodyCompact'requiredObserversHash body) $ \observerCommitment ->
  plet
    (pfromData (pwitnessSetCompact'redeemerTxWitsHash ws) #/= NativeField.pemptyFieldCommitment)
    $ \hasRedeemers ->
  pif
    ( pverified'version verified #== 1
        #&& pphaseAScriptPreconditionsControlIsBound
          # pre # witness # control # observerCommitment
    )
    ( pif (observerCommitment #== NativeField.pemptyFieldCommitment)
        ( pmatch auxiliary $ \case
            PNoAuxiliaryWitness ->
              pphaseAScriptPreconditionsFinalize
                # pre # witness # control # verifiedSource # hasRedeemers # 0
            _ -> pconstant False
        )
        ( pif
            ( pfromData (pphaseAPreconditions'observerCount c) #> 0
                #&& pfromData (pphaseAPreconditions'observerSeen c)
                  #== pfromData (pphaseAPreconditions'observerCount c)
            )
            ( pmatch auxiliary $ \case
                PNoAuxiliaryWitness ->
                  pphaseAScriptPreconditionsFinalize
                    # pre # witness # control # verifiedSource # hasRedeemers
                    # pfromData (pphaseAPreconditions'observerCount c)
                _ -> pconstant False
            )
            ( pmatch auxiliary $ \case
                PTransactionFieldChunkWitness collectionProofD chunkProofD ->
                  plet (pfromData collectionProofD) $ \collectionProof ->
                  plet (pfromData chunkProofD) $ \chunkProof ->
                  pmatch collectionProof $ \item ->
                  pmatch chunkProof $ \chunk ->
                  plet
                    ( pif (pfromData (pphaseAPreconditions'observerCount c) #== 0)
                        (pfromData $ pitemProof'itemCount item)
                        (pfromData $ pphaseAPreconditions'observerCount c)
                    )
                    $ \activeCount ->
                  plet (pfromData $ BoundedItem.pchunkProof'chunk chunk) $ \observerHash ->
                  pif
                    ( pand'List
                        [ activeCount #> 0
                        , activeCount #<= pmaxTxSizeDerivedItemCount
                        , pfromData (pitemProof'fieldIndex item) #== 3
                        , pfromData (pitemProof'itemCount item) #== activeCount
                        , pfromData (pitemProof'itemIndex item)
                            #== pfromData (pphaseAPreconditions'observerSeen c)
                        , pfromData (pitemProof'itemLength item) #== 28
                        , pfromData (BoundedItem.pchunkProof'fieldIndex chunk) #== 3
                        , pfromData (BoundedItem.pchunkProof'itemIndex chunk)
                            #== pfromData (pitemProof'itemIndex item)
                        , pfromData (BoundedItem.pchunkProof'totalLength chunk) #== 28
                        , pfromData (BoundedItem.pchunkProof'chunkIndex chunk) #== 0
                        , BoundedItem.pchunkCount
                            # pfromData (BoundedItem.pchunkProof'totalLength chunk)
                            #== 1
                        , pverifyBoundedCollectionItem # observerCommitment # collectionProof
                        , BoundedItem.pverifyChunk
                            # pfromData (pitemProof'itemCommitment item) # chunkProof
                        ]
                    )
                    ( pif
                        ( pfromData (pphaseAPreconditions'observerSeen c) #> 0
                            #&& pnot
                              # ( pfromData (pphaseAPreconditions'previousObserver c)
                                    #< observerHash
                                )
                        )
                        ( prejectedSuccessorIsExact
                            # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                            # pconstant "E_INVALID_FIELD_TYPE"
                        )
                        ( pphaseAScriptPreconditionsSuccessorIsExact
                            # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                            # ( pphaseAScriptPreconditionsWithObserver # control
                                  # activeCount
                                  # (pfromData (pphaseAPreconditions'observerSeen c) + 1)
                                  # observerHash
                              )
                        )
                    )
                    (pconstant False)
                _ -> pconstant False
            )
        )
    )
    (pconstant False)

pverifyPhaseAScriptPreconditionsOneStepV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyPhaseAScriptPreconditionsOneStepV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch pre $ \preState ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transitionD auxiliaryD) ->
  plet (pfromData transitionD) $ \witness ->
    pfromData (pmachineState'phase preState) #== pcon PPhaseAScriptPreconditions
      #&& pstructuralTransitionIsValid # pre # witness
      #&& pverifyPhaseAScriptPreconditions
        # pre # witness # pdecodeValidationAuxiliaryWitnessV1 auxiliaryD

pverifyPhaseAScriptPreconditionsSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyPhaseAScriptPreconditionsSemanticsV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transitionD auxiliaryD) ->
    pverifyPhaseAScriptPreconditions
      # pre # pfromData transitionD # pdecodeValidationAuxiliaryWitnessV1 auxiliaryD

pvalidityIntervalContainsSlot :: forall s.
  Term s (PNativeTxBodyCompact :--> PInteger :--> PBool)
pvalidityIntervalContainsSlot = phoistAcyclic $ plam $ \body blockSlot ->
  pmatch body $ \txBody ->
    ( pbodyCompact'validityIntervalStart txBody #< 0
        #|| blockSlot #>= pbodyCompact'validityIntervalStart txBody
    )
      #&& ( pbodyCompact'validityIntervalEnd txBody #< 0
              #|| blockSlot #<= pbodyCompact'validityIntervalEnd txBody
           )

presolveInputsControlIsBound :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PResolveInputsControlV1 :--> PBool
    )
presolveInputsControlIsBound = phoistAcyclic $ plam $ \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  plet (pfromData $ presolveInputs'cursor c) $ \cursor ->
  plet
    ( pmatch (pfromData $ presolveInputs'pending c) $ \case
        PDNothing -> pconstant True
        PDJust pendingD ->
          plet (pfromData pendingD) $ \pending ->
          pmatch pending $ \active ->
          plet
            ( OutputCommitment.pdecodeLedgerOutputCommitment
                # pfromData (presolveOutputProof'descriptorCbor active)
            )
            $ \descriptor ->
          pmatch descriptor $ \output ->
          plet (pdecodeMidgardTxInputCbor # pfromData (presolveOutputProof'key active)) $ \input ->
          pmatch input $ \scheduledInput ->
          plet (pfromData $ presolveOutputProof'outputProof active) $ \outputProof ->
          pmatch outputProof $ \proof ->
            pand'List
              [ cursor #> 0
              , pfromData (presolveOutputProof'sourceKind active) #== 0
                  #|| pfromData (presolveOutputProof'sourceKind active) #== 1
              , presolutionScheduleNodeHash
                  # pfromData (presolveOutputProof'sourceKind active)
                  # pfromData (presolveOutputProof'key active)
                  # pfromData (presolveOutputProof'nextScheduleHash active)
                  #== pfromData (presolveInputs'remainingScheduleHash c)
              , pfromData (ptxInput'outputIndex scheduledInput)
                  #== pfromData (OutputCommitment.poutputCommitment'outputIndex output)
              , LedgerOutputProof.pcontrolIsWellFormed # outputProof
              , pfromData (LedgerOutputProof.pproof'outputIndex proof)
                  #== pfromData (OutputCommitment.poutputCommitment'outputIndex output)
              , pfromData (LedgerOutputProof.pproof'totalLength proof)
                  #== pfromData (OutputCommitment.poutputCommitment'totalLength output)
              , pfromData (LedgerOutputProof.pproof'itemCommitment proof)
                  #== pfromData (OutputCommitment.poutputCommitment'itemCommitment output)
              ]
    )
    $ \pendingIsWellFormed ->
    pand'List
      [ NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (presolveInputs'compactCbor c)
          # pfromData (presolveInputs'witnessSetCompactCbor c)
          # pfromData (presolveInputs'fieldPreimageLengthsCbor c)
          #== pfromData (pmachineState'transactionCommitment preState)
      , phashValidationContext # pfromData (presolveInputs'contextCbor c)
          #== pfromData (pmachineState'validationContextHash preState)
      , plengthBS # pfromData (presolveInputs'accumulator c) #== 32
      , plengthBS # pfromData (presolveInputs'remainingScheduleHash c) #== 32
      , plengthBS # pfromData (presolveInputs'resolutionScheduleHash c) #== 32
      , pif (cursor #<= 1)
          ( pfromData (presolveInputs'remainingScheduleHash c)
              #== pfromData (presolveInputs'resolutionScheduleHash c)
          )
          (pconstant True)
      , pfromData (presolveInputs'signerCount c) #>= 0
      , plengthBS # pfromData (presolveInputs'signerFrontierCommitment c) #== 32
      , cursor #>= 0
      , pendingIsWellFormed
      , pfromData (poneStep'workWitnessCbor stepWitness)
          #== pencodeResolveInputsWitness
            # pfromData (presolveInputs'compactCbor c)
            # pfromData (presolveInputs'witnessSetCompactCbor c)
            # pfromData (presolveInputs'fieldPreimageLengthsCbor c)
            # pfromData (presolveInputs'contextCbor c)
            # cursor
            # pfromData (presolveInputs'accumulator c)
            # pfromData (presolveInputs'remainingScheduleHash c)
            # pfromData (presolveInputs'signerCount c)
            # pfromData (presolveInputs'signerFrontierCommitment c)
            # pfromData (presolveInputs'pending c)
            # pfromData (presolveInputs'resolutionScheduleHash c)
      ]

-- Initial transitions require an empty pending proof. Specialize the common
-- binding checks so this script does not retain the unreachable
-- ledger-output proof validation branch from 'presolveInputsControlIsBound'.
presolveInputsControlIsBoundWithoutPending :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PResolveInputsControlV1 :--> PBool
    )
presolveInputsControlIsBoundWithoutPending = phoistAcyclic $ plam $ \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  plet (pfromData $ presolveInputs'cursor c) $ \cursor ->
    pand'List
      [ NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (presolveInputs'compactCbor c)
          # pfromData (presolveInputs'witnessSetCompactCbor c)
          # pfromData (presolveInputs'fieldPreimageLengthsCbor c)
          #== pfromData (pmachineState'transactionCommitment preState)
      , phashValidationContext # pfromData (presolveInputs'contextCbor c)
          #== pfromData (pmachineState'validationContextHash preState)
      , plengthBS # pfromData (presolveInputs'accumulator c) #== 32
      , plengthBS # pfromData (presolveInputs'remainingScheduleHash c) #== 32
      , plengthBS # pfromData (presolveInputs'resolutionScheduleHash c) #== 32
      , pif (cursor #<= 1)
          ( pfromData (presolveInputs'remainingScheduleHash c)
              #== pfromData (presolveInputs'resolutionScheduleHash c)
          )
          (pconstant True)
      , pfromData (presolveInputs'signerCount c) #>= 0
      , plengthBS # pfromData (presolveInputs'signerFrontierCommitment c) #== 32
      , cursor #>= 0
      , pfromData (presolveInputs'pending c) #== pcon PDNothing
      , pfromData (poneStep'workWitnessCbor stepWitness)
          #== pencodeResolveInputsWitness
            # pfromData (presolveInputs'compactCbor c)
            # pfromData (presolveInputs'witnessSetCompactCbor c)
            # pfromData (presolveInputs'fieldPreimageLengthsCbor c)
            # pfromData (presolveInputs'contextCbor c)
            # cursor
            # pfromData (presolveInputs'accumulator c)
            # pfromData (presolveInputs'remainingScheduleHash c)
            # pfromData (presolveInputs'signerCount c)
            # pfromData (presolveInputs'signerFrontierCommitment c)
            # pfromData (presolveInputs'pending c)
            # pfromData (presolveInputs'resolutionScheduleHash c)
      ]

presolveInputsSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PResolveInputsControlV1 :--> PBool
    )
presolveInputsSuccessorIsExact = phoistAcyclic $ plam $ \pre witness nextControl ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch (pfromData $ poneStep'claimedSuccessor stepWitness) $ \postState ->
  pmatch nextControl $ \next ->
    pfromData (pmachineState'phase postState) #== pcon PResolveInputs
      #&& pfromData (pmachineState'workRoot postState)
        #== phashWorkWitness # pcon PResolveInputs
          # (pfromData (pmachineState'programCounter preState) + 1)
          # ( pencodeResolveInputsWitness
                # pfromData (presolveInputs'compactCbor next)
                # pfromData (presolveInputs'witnessSetCompactCbor next)
                # pfromData (presolveInputs'fieldPreimageLengthsCbor next)
                # pfromData (presolveInputs'contextCbor next)
                # pfromData (presolveInputs'cursor next)
                # pfromData (presolveInputs'accumulator next)
                # pfromData (presolveInputs'remainingScheduleHash next)
                # pfromData (presolveInputs'signerCount next)
                # pfromData (presolveInputs'signerFrontierCommitment next)
                # pfromData (presolveInputs'pending next)
                # pfromData (presolveInputs'resolutionScheduleHash next)
            )

presolveInputsInitialStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PResolveInputsControlV1 :--> PNativeTxBodyCompact
        :--> PValidationContextV1 :--> PBool
    )
presolveInputsInitialStep = phoistAcyclic $ plam $ \pre witness control body context ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch context $ \ctx ->
    pif
      ( pvalidityIntervalContainsSlot
          # body # pfromData (pvalidationContext'blockSlot ctx)
      )
      ( presolveInputsSuccessorIsExact
          # pre # witness
          # ( pcon $ PResolveInputsControlV1
                (presolveInputs'compactCbor c)
                (presolveInputs'witnessSetCompactCbor c)
                (presolveInputs'fieldPreimageLengthsCbor c)
                (presolveInputs'contextCbor c)
                (pdata 1)
                (presolveInputs'accumulator c)
                (presolveInputs'remainingScheduleHash c)
                (presolveInputs'signerCount c)
                (presolveInputs'signerFrontierCommitment c)
                (pdata $ pcon PDNothing)
                (presolveInputs'resolutionScheduleHash c)
            )
      )
      ( prejectedSuccessorIsExact
          # pre # pfromData (poneStep'claimedSuccessor stepWitness)
          # pconstant "E_VALIDITY_INTERVAL_MISMATCH"
      )

pverifyResolveInputsInitialSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyResolveInputsInitialSemanticsV1 = phoistAcyclic $ plam $ \pre witness ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet (presolveInputsControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness)) $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (presolveInputs'compactCbor c)
        # pfromData (presolveInputs'witnessSetCompactCbor c)
        # pfromData (presolveInputs'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  pmatch (pverified'txCompact verified) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
  plet (pdecodeValidationContext # pfromData (presolveInputs'contextCbor c)) $ \context ->
  pmatch context $ \ctx ->
    pand'List
      [ pverified'version verified #== 1
      , presolveInputsControlIsBoundWithoutPending # pre # witness # control
      , pfromData (presolveInputs'cursor c) #== 0
      , presolveInputsInitialStep # pre # witness # control # pcon body # pcon ctx
      ]

presolveInputsFinishStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PResolveInputsControlV1 :--> PBool
    )
presolveInputsFinishStep = phoistAcyclic $ plam $ \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch (pfromData $ poneStep'claimedSuccessor stepWitness) $ \postState ->
    pfromData (pmachineState'phase postState) #== pcon PScriptSources
      #&& pfromData (pmachineState'workRoot postState)
        #== phashWorkWitness # pcon PScriptSources
          # (pfromData (pmachineState'programCounter preState) + 1)
          # ( pencodeScriptSourcesWitness
                # pfromData (presolveInputs'compactCbor c)
                # pfromData (presolveInputs'witnessSetCompactCbor c)
                # pfromData (presolveInputs'fieldPreimageLengthsCbor c)
                # pfromData (presolveInputs'contextCbor c)
                # (pfromData (presolveInputs'cursor c) - 1)
                # pfromData (presolveInputs'accumulator c)
                # pfromData (presolveInputs'signerCount c)
                # pfromData (presolveInputs'signerFrontierCommitment c)
                # pnil
                # 0 # 0 # pnil # 0 # pnil
                # 0 # pinitialResolutionAccumulator # pemptyResolutionScheduleHash
                # 0 # 0 # pnil # 0 # 0 # pnil # 0
                # pemptyReceivePurposeScanControl
                # 0 # 0 # pemptyObserverPurposeScanControl
                # pemptyMintFoldControl
                # pfromData (presolveInputs'resolutionScheduleHash c)
            )

pverifyResolveInputsFinishSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyResolveInputsFinishSemanticsV1 = phoistAcyclic $ plam $ \pre witness ->
  pmatch witness $ \stepWitness ->
  plet (presolveInputsControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness)) $ \control ->
  pmatch control $ \c ->
    pand'List
      [ presolveInputsControlIsBound # pre # witness # control
      , pfromData (presolveInputs'cursor c) #> 0
      , pfromData (presolveInputs'pending c) #== pcon PDNothing
      , pfromData (presolveInputs'remainingScheduleHash c) #== pemptyResolutionScheduleHash
      , presolveInputsFinishStep # pre # witness # control
      ]

presolveInputsWithPending :: forall s.
  Term s
    ( PResolveInputsControlV1 :--> PMaybeData PResolveInputOutputProofV1
        :--> PResolveInputsControlV1
    )
presolveInputsWithPending = phoistAcyclic $ plam $ \control pending ->
  pmatch control $ \c ->
    pcon $ PResolveInputsControlV1
      (presolveInputs'compactCbor c)
      (presolveInputs'witnessSetCompactCbor c)
      (presolveInputs'fieldPreimageLengthsCbor c)
      (presolveInputs'contextCbor c)
      (presolveInputs'cursor c)
      (presolveInputs'accumulator c)
      (presolveInputs'remainingScheduleHash c)
      (presolveInputs'signerCount c)
      (presolveInputs'signerFrontierCommitment c)
      (pdata pending)
      (presolveInputs'resolutionScheduleHash c)

presolveMembershipBeginStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PResolveInputsControlV1 :--> PInteger :--> PByteString
        :--> PByteString :--> PByteString :--> PProof :--> PSignerSetProofV1
        :--> PBool
    )
presolveMembershipBeginStep = phoistAcyclic $ plam $
  \pre witness control sourceKind key nextScheduleHash descriptorCbor proof signerProof ->
  pmatch pre $ \preState ->
  pmatch control $ \c ->
  plet (OutputCommitment.pdecodeLedgerOutputCommitment # descriptorCbor) $ \descriptor ->
  pmatch descriptor $ \output ->
  plet (pdecodeMidgardTxInputCbor # key) $ \input ->
  pmatch input $ \scheduledInput ->
  plet
    ( pcon $ PResolveInputOutputProofV1
        (pdata sourceKind)
        (pdata key)
        (pdata nextScheduleHash)
        (pdata descriptorCbor)
        ( pdata $ LedgerOutputProof.pinitialControlV1
            # pfromData (OutputCommitment.poutputCommitment'outputIndex output)
            # pfromData (OutputCommitment.poutputCommitment'totalLength output)
            # pfromData (OutputCommitment.poutputCommitment'itemCommitment output)
        )
    )
    $ \pending ->
    pand'List
      [ pfromData (presolveInputs'pending c) #== pcon PDNothing
      , signerProof #== pcon PNoSignerSetProof
      , sourceKind #== 0 #|| sourceKind #== 1
      , presolutionScheduleNodeHash # sourceKind # key # nextScheduleHash
          #== pfromData (presolveInputs'remainingScheduleHash c)
      , pfromData (ptxInput'outputIndex scheduledInput)
          #== pfromData (OutputCommitment.poutputCommitment'outputIndex output)
      , pfromData (OutputCommitment.poutputCommitment'totalLength output) #> 0
      , MpfProof.phasV1
          # pfromData (pmachineState'priorLedgerRoot preState)
          # key # descriptorCbor # proof
      , presolveInputsSuccessorIsExact
          # pre # witness
          # (presolveInputsWithPending # control # pcon (PDJust $ pdata pending))
      ]

pverifyResolveInputsMembershipBeginSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInteger :--> PByteString :--> PByteString :--> PByteString
        :--> PProof :--> PSignerSetProofV1 :--> PBool
    )
pverifyResolveInputsMembershipBeginSemanticsV1 = phoistAcyclic $ plam $
  \pre witness sourceKind key nextScheduleHash descriptorCbor proof signerProof ->
  pmatch witness $ \stepWitness ->
  plet (presolveInputsControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness)) $ \control ->
  pmatch control $ \c ->
    pand'List
      [ presolveInputsControlIsBound # pre # witness # control
      , pfromData (presolveInputs'cursor c) #> 0
      , pfromData (presolveInputs'pending c) #== pcon PDNothing
      , pfromData (presolveInputs'remainingScheduleHash c) #/= pemptyResolutionScheduleHash
      , presolveMembershipBeginStep
          # pre # witness # control # sourceKind # key # nextScheduleHash
          # descriptorCbor # proof # signerProof
      ]

presolveMembershipProofResult :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PResolveInputsControlV1 :--> PResolveInputOutputProofV1
        :--> LedgerOutputProof.PLedgerOutputProofStepResultV1 :--> PBool
    )
presolveMembershipProofResult = phoistAcyclic $ plam $
  \pre witness control pending result ->
  pmatch witness $ \stepWitness ->
  pmatch pending $ \active ->
  pmatch result $ \case
    LedgerOutputProof.PLedgerOutputProofAdvanced nextOutputProofD ->
      plet
        ( pcon $ PResolveInputOutputProofV1
            (presolveOutputProof'sourceKind active)
            (presolveOutputProof'key active)
            (presolveOutputProof'nextScheduleHash active)
            (presolveOutputProof'descriptorCbor active)
            nextOutputProofD
        )
        $ \nextPending ->
        presolveInputsSuccessorIsExact
          # pre # witness
          # ( presolveInputsWithPending # control
                # (pcon $ PDJust $ pdata nextPending)
            )
    LedgerOutputProof.PLedgerOutputProofInvalidOutput ->
      prejectedSuccessorIsExact
        # pre # pfromData (poneStep'claimedSuccessor stepWitness)
        # pconstant "E_INVALID_OUTPUT"
    LedgerOutputProof.PLedgerOutputProofInvalidReferenceScript ->
      prejectedSuccessorIsExact
        # pre # pfromData (poneStep'claimedSuccessor stepWitness)
        # pconstant "E_INVALID_FIELD_TYPE"
    LedgerOutputProof.PLedgerOutputProofNativeScriptNodeLimit ->
      prejectedSuccessorIsExact
        # pre # pfromData (poneStep'claimedSuccessor stepWitness)
        # pconstant "E_NATIVE_SCRIPT_NODE_COUNT"
    LedgerOutputProof.PLedgerOutputProofNativeScriptDepthLimit ->
      prejectedSuccessorIsExact
        # pre # pfromData (poneStep'claimedSuccessor stepWitness)
        # pconstant "E_NATIVE_SCRIPT_DEPTH"

presolveMembershipProofStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PResolveInputsControlV1 :--> PResolveInputOutputProofV1
        :--> LedgerOutputProof.PLedgerOutputProofWitnessV1 :--> PBool
    )
presolveMembershipProofStep = phoistAcyclic $ plam $
  \pre witness control pending proofWitness ->
  pmatch pending $ \active ->
  pmatch
    ( LedgerOutputProof.pstepV1
        # pfromData (presolveOutputProof'outputProof active) # proofWitness
    )
    $ \case
      PNothing -> pconstant False
      PJust result ->
        presolveMembershipProofResult
          # pre # witness # control # pcon active # result

pverifyResolveInputsMembershipStepSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> LedgerOutputProof.PLedgerOutputProofWitnessV1 :--> PBool
    )
pverifyResolveInputsMembershipStepSemanticsV1 = phoistAcyclic $ plam $
  \pre witness proofWitness ->
  pmatch witness $ \stepWitness ->
  plet (presolveInputsControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness)) $ \control ->
  pmatch control $ \c ->
    pmatch (pfromData $ presolveInputs'pending c) $ \case
      PDNothing -> pconstant False
      PDJust pendingD ->
        pand'List
          [ presolveInputsControlIsBound # pre # witness # control
          , presolveMembershipProofStep
              # pre # witness # control # pfromData pendingD # proofWitness
          ]

ppaymentCredentialSignerAuthorization :: forall s.
  Term s
    ( PMidgardCredential :--> PInteger :--> PByteString :--> PSignerSetProofV1
        :--> PInputSignerAuthorizationV1
    )
ppaymentCredentialSignerAuthorization = phoistAcyclic $ plam $
  \credential signerCount signerCommitment signerProof ->
  pmatch credential $ \case
    PMidgardScriptCredential _ ->
      pif (signerProof #== pcon PNoSignerSetProof)
        (pcon PInputSignerAuthorized)
        (pcon PInputSignerProofMalformed)
    PMidgardPubKeyCredential signerHashD ->
      pmatch signerProof $ \case
        PSignerMembershipProof peaksD signerIndexD siblingsD ->
          pif
            ( psignerMembershipIsValid
                # pfromData signerHashD # signerCount # signerCommitment
                # pfromData peaksD # pfromData signerIndexD # pfromData siblingsD
            )
            (pcon PInputSignerAuthorized)
            (pcon PInputSignerProofMalformed)
        _ ->
          pif
            ( psignerNonMembershipIsValid
                # pfromData signerHashD # signerCount # signerCommitment # signerProof
            )
            (pcon PInputSignerMissing)
            (pcon PInputSignerProofMalformed)

pinputSignerAuthorization :: forall s.
  Term s
    ( PInteger :--> PMidgardAddress :--> PInteger :--> PByteString
        :--> PSignerSetProofV1 :--> PInputSignerAuthorizationV1
    )
pinputSignerAuthorization = phoistAcyclic $ plam $
  \sourceKind address signerCount signerCommitment signerProof ->
  pmatch address $ \a ->
  pif (sourceKind #== 1)
    ( pif (signerProof #== pcon PNoSignerSetProof)
        (pcon PInputSignerAuthorized)
        (pcon PInputSignerProofMalformed)
    )
    ( pif (sourceKind #== 0)
        ( ppaymentCredentialSignerAuthorization
            # pfromData (paddress'paymentCredential a)
            # signerCount # signerCommitment # signerProof
        )
        perror
    )

presolveInputsAfterFinalize :: forall s.
  Term s
    ( PResolveInputsControlV1 :--> PResolveInputOutputProofV1 :--> PByteString
        :--> PResolveInputsControlV1
    )
presolveInputsAfterFinalize = phoistAcyclic $ plam $ \control pending descriptorCbor ->
  pmatch control $ \c ->
  pmatch pending $ \active ->
    pcon $ PResolveInputsControlV1
      (presolveInputs'compactCbor c)
      (presolveInputs'witnessSetCompactCbor c)
      (presolveInputs'fieldPreimageLengthsCbor c)
      (presolveInputs'contextCbor c)
      (pdata $ pfromData (presolveInputs'cursor c) + 1)
      ( pdata $ presolvedInputAccumulatorSuccessor
          # pfromData (presolveInputs'accumulator c)
          # pfromData (presolveOutputProof'sourceKind active)
          # pfromData (presolveOutputProof'key active)
          # descriptorCbor
      )
      (presolveOutputProof'nextScheduleHash active)
      (presolveInputs'signerCount c)
      (presolveInputs'signerFrontierCommitment c)
      (pdata $ pcon PDNothing)
      (presolveInputs'resolutionScheduleHash c)

presolveMembershipProofFinalize :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PResolveInputsControlV1 :--> PResolveInputOutputProofV1
        :--> PByteString :--> PSignerSetProofV1 :--> PBool
    )
presolveMembershipProofFinalize = phoistAcyclic $ plam $
  \pre witness control pending descriptorCbor signerProof ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch pending $ \active ->
  plet (OutputCommitment.pdecodeLedgerOutputCommitment # descriptorCbor) $ \descriptor ->
  pmatch descriptor $ \output ->
  pif
    ( descriptorCbor #== pfromData (presolveOutputProof'descriptorCbor active)
        #&& LedgerOutputProof.pdescriptorIsExactV1
          # pfromData (presolveOutputProof'outputProof active) # descriptor
    )
    ( pmatch
        ( LedgerOutput.pdecodeCanonicalAddressBytes
            # pfromData (OutputCommitment.poutputCommitment'address output)
        )
        $ \case
          PNothing -> perror
          PJust address ->
            pmatch
              ( pinputSignerAuthorization
                  # pfromData (presolveOutputProof'sourceKind active)
                  # address
                  # pfromData (presolveInputs'signerCount c)
                  # pfromData (presolveInputs'signerFrontierCommitment c)
                  # signerProof
              )
              $ \case
                PInputSignerMissing ->
                  prejectedSuccessorIsExact
                    # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                    # pconstant "E_MISSING_REQUIRED_WITNESS"
                PInputSignerProofMalformed -> pconstant False
                PInputSignerAuthorized ->
                  presolveInputsSuccessorIsExact
                    # pre # witness
                    # (presolveInputsAfterFinalize # control # pending # descriptorCbor)
    )
    (pconstant False)

pverifyResolveInputsMembershipFinalizeSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PByteString :--> PSignerSetProofV1 :--> PBool
    )
pverifyResolveInputsMembershipFinalizeSemanticsV1 = phoistAcyclic $ plam $
  \pre witness descriptorCbor signerProof ->
  pmatch witness $ \stepWitness ->
  plet (presolveInputsControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness)) $ \control ->
  pmatch control $ \c ->
    pmatch (pfromData $ presolveInputs'pending c) $ \case
      PDNothing -> pconstant False
      PDJust pendingD ->
        pand'List
          [ presolveInputsControlIsBound # pre # witness # control
          , presolveMembershipProofFinalize
              # pre # witness # control # pfromData pendingD
              # descriptorCbor # signerProof
          ]

presolveNonMembershipStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PResolveInputsControlV1 :--> PInteger :--> PByteString
        :--> PByteString :--> PProof :--> PBool
    )
presolveNonMembershipStep = phoistAcyclic $ plam $
  \pre witness control sourceKind key nextScheduleHash proof ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
    pand'List
      [ presolutionScheduleNodeHash # sourceKind # key # nextScheduleHash
          #== pfromData (presolveInputs'remainingScheduleHash c)
      , MpfProof.pdoesNotHave
          # pfromData (pmachineState'priorLedgerRoot preState) # key # proof
      , prejectedSuccessorIsExact
          # pre # pfromData (poneStep'claimedSuccessor stepWitness)
          # pconstant "E_INPUT_NOT_FOUND"
      ]

pverifyResolveInputsNonMembershipSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInteger :--> PByteString :--> PByteString :--> PProof :--> PBool
    )
pverifyResolveInputsNonMembershipSemanticsV1 = phoistAcyclic $ plam $
  \pre witness sourceKind key nextScheduleHash proof ->
  pmatch witness $ \stepWitness ->
  plet (presolveInputsControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness)) $ \control ->
  pmatch control $ \c ->
    pand'List
      [ presolveInputsControlIsBound # pre # witness # control
      , pfromData (presolveInputs'cursor c) #> 0
      , pfromData (presolveInputs'pending c) #== pcon PDNothing
      , pfromData (presolveInputs'remainingScheduleHash c) #/= pemptyResolutionScheduleHash
      , presolveNonMembershipStep
          # pre # witness # control # sourceKind # key # nextScheduleHash # proof
      ]

presolveLookupStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PResolveInputsControlV1
        :--> PBool
    )
presolveLookupStep = phoistAcyclic $ plam $ \pre witness auxiliary control ->
  pmatch control $ \c ->
  pmatch (pfromData $ presolveInputs'pending c) $ \case
    PDJust pendingD ->
      plet (pfromData pendingD) $ \pending ->
      pmatch auxiliary $ \case
        PLedgerOutputProofStepWitness proofWitnessD ->
          presolveMembershipProofStep
            # pre # witness # control # pending # pfromData proofWitnessD
        PLedgerOutputProofFinalizeWitness descriptorCborD signerProofD ->
          presolveMembershipProofFinalize
            # pre # witness # control # pending
            # pfromData descriptorCborD # pfromData signerProofD
        _ -> pconstant False
    PDNothing ->
      pmatch auxiliary $ \case
        PScheduledLedgerMembershipWitness
            sourceKindD keyD nextScheduleHashD valueD proofD signerProofD ->
          presolveMembershipBeginStep
            # pre # witness # control
            # pfromData sourceKindD # pfromData keyD
            # pfromData nextScheduleHashD # pfromData valueD
            # pfromData proofD # pfromData signerProofD
        PScheduledLedgerNonMembershipWitness
            sourceKindD keyD nextScheduleHashD proofD ->
          presolveNonMembershipStep
            # pre # witness # control
            # pfromData sourceKindD # pfromData keyD
            # pfromData nextScheduleHashD # pfromData proofD
        _ -> pconstant False

pverifyResolveInputs :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyResolveInputs = phoistAcyclic $ plam $ \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (presolveInputsControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (presolveInputs'compactCbor c)
        # pfromData (presolveInputs'witnessSetCompactCbor c)
        # pfromData (presolveInputs'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  pmatch (pverified'txCompact verified) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
  plet (pdecodeValidationContext # pfromData (presolveInputs'contextCbor c)) $ \context ->
  pmatch context $ \ctx ->
  pif
    ( pverified'version verified #== 1
        #&& presolveInputsControlIsBound # pre # witness # control
    )
    ( pif (pfromData (presolveInputs'cursor c) #== 0)
        ( pmatch auxiliary $ \case
            PNoAuxiliaryWitness ->
              presolveInputsInitialStep
                # pre # witness # control # pcon body # pcon ctx
            _ -> pconstant False
        )
        ( pif
            ( pfromData (presolveInputs'pending c) #== pcon PDNothing
                #&& pfromData (presolveInputs'remainingScheduleHash c)
                  #== pemptyResolutionScheduleHash
            )
            ( pmatch auxiliary $ \case
                PNoAuxiliaryWitness ->
                  presolveInputsFinishStep # pre # witness # control
                _ -> pconstant False
            )
            (presolveLookupStep # pre # witness # auxiliary # control)
        )
    )
    (pconstant False)

pverifyResolveInputsOneStepV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyResolveInputsOneStepV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch pre $ \preState ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transitionD auxiliaryD) ->
  plet (pfromData transitionD) $ \witness ->
    pfromData (pmachineState'phase preState) #== pcon PResolveInputs
      #&& pstructuralTransitionIsValid # pre # witness
      #&& pverifyResolveInputs # pre # witness # pdecodeValidationAuxiliaryWitnessV1 auxiliaryD

pverifyResolveInputsSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyResolveInputsSemanticsV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transitionD auxiliaryD) ->
    pverifyResolveInputs # pre # pfromData transitionD # pdecodeValidationAuxiliaryWitnessV1 auxiliaryD

pemptyScriptDiscoveryControl :: forall s. Term s PScriptDiscoveryControlV1
pemptyScriptDiscoveryControl = pcon $ PScriptDiscoveryControlV1
  (pdata 0) (pdata 0) (pdata 0) (pdata $ -1) (pdata $ -1)
  (pdata $ pconstant "") (pdata $ pconstant "")
  (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
  (pdata 0) (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata pnil)

pencodeScriptDiscoveryControl :: forall s.
  Term s (PScriptDiscoveryControlV1 :--> PByteString)
pencodeScriptDiscoveryControl = phoistAcyclic $ plam $ \control ->
  pmatch control $ \c ->
    pencodeDefiniteArrayHeader # 15
      <> pcborInt (pfromData $ pscriptDiscovery'purposeCursor c)
      <> pcborInt (pfromData $ pscriptDiscovery'sourceCursor c)
      <> pcborInt (pfromData $ pscriptDiscovery'redeemerCursor c)
      <> pcborInt (pfromData $ pscriptDiscovery'currentPurposeKind c)
      <> pcborInt (pfromData $ pscriptDiscovery'currentPurposeIndex c)
      <> (pencodeDefiniteBytes # pfromData (pscriptDiscovery'currentScriptHash c))
      <> (pencodeDefiniteBytes # pfromData (pscriptDiscovery'currentSubject c))
      <> pcborInt (pfromData $ pscriptDiscovery'matchedSourceIndex c)
      <> pcborInt (pfromData $ pscriptDiscovery'matchedLanguageTag c)
      <> (pencodeDefiniteBytes # pfromData (pscriptDiscovery'matchedSourceLeaf c))
      <> pcborInt (pfromData $ pscriptDiscovery'usedInlineBitmap c)
      <> pcborInt (pfromData $ pscriptDiscovery'usedRedeemerBitmap c)
      <> (pencodeDefiniteBytes # pfromData (pscriptDiscovery'redeemerItemControlHash c))
      <> pcborInt (pfromData $ pscriptDiscovery'executionCount c)
      <> (pencodeFrontier # pfromData (pscriptDiscovery'executionPeaks c))

pdecodeScriptDiscoveryControl :: forall s.
  Term s (PByteString :--> PScriptDiscoveryControlV1)
pdecodeScriptDiscoveryControl = phoistAcyclic $ plam $ \discoveryCbor ->
  pmatch (pdeserialise # discoveryCbor) $ \case
    PNothing -> perror
    PJust dat ->
      plet (pasList # dat) $ \items ->
      pif (plength # items #== 15)
        ( pcon $ PScriptDiscoveryControlV1
            (di 0 items) (di 1 items) (di 2 items) (di 3 items) (di 4 items)
            (db 5 items) (db 6 items) (di 7 items) (di 8 items) (db 9 items)
            (di 10 items) (di 11 items) (db 12 items) (di 13 items)
            (dp 14 items)
        )
        perror
  where
    di index xs = pdata $ pasInt # (pelemAt # index # xs)
    db index xs = pdata $ pasByteStr # (pelemAt # index # xs)
    dp index xs = pdata $ pdecodeFrontierPeakItems # (pasList # (pelemAt # index # xs))

pscriptSourcesWithStageDiscovery :: forall s.
  Term s
    ( PScriptSourcesControlV1 :--> PInteger :--> PScriptDiscoveryControlV1
        :--> PScriptSourcesControlV1
    )
pscriptSourcesWithStageDiscovery = phoistAcyclic $ plam $ \control stage discovery ->
  pmatch control $ \c ->
    pcon $ PScriptSourcesControlV1
      (pscriptSources'compactCbor c)
      (pscriptSources'witnessSetCompactCbor c)
      (pscriptSources'fieldPreimageLengthsCbor c)
      (pscriptSources'contextCbor c)
      (pscriptSources'resolvedInputCount c)
      (pscriptSources'resolvedInputsAccumulator c)
      (pscriptSources'signerCount c)
      (pscriptSources'signerFrontierCommitment c)
      (pscriptSources'resolvedItemPeaks c)
      (pdata stage)
      (pscriptSources'sourceCount c)
      (pscriptSources'sourcePeaks c)
      (pscriptSources'redeemerCount c)
      (pscriptSources'redeemerPeaks c)
      (pscriptSources'replayCursor c)
      (pscriptSources'replayAccumulator c)
      (pscriptSources'replayRemainingScheduleHash c)
      (pscriptSources'spendIndex c)
      (pscriptSources'purposeCount c)
      (pscriptSources'purposePeaks c)
      (pscriptSources'outputCursor c)
      (pscriptSources'outputCount c)
      (pscriptSources'outputPeaks c)
      (pscriptSources'outputTotalCount c)
      (pscriptSources'receiveScan c)
      (pscriptSources'sourceTotalCount c)
      (pscriptSources'redeemerTotalCount c)
      (pscriptSources'observerScan c)
      (pdata discovery)
      (pdata $ pcon PDNothing)
      (pdata $ pconstant "")
      (pscriptSources'mintFold c)
      (pscriptSources'resolutionScheduleHash c)

pencodeScriptSourcesDiscoveryWitness :: forall s.
  Term s
    ( PScriptSourcesControlV1 :--> PInteger :--> PScriptDiscoveryControlV1
        :--> PByteString
    )
pencodeScriptSourcesDiscoveryWitness = phoistAcyclic $ plam $
  \control stage discovery ->
  pif (stage #>= 8)
    ( plet
        ( pencodeScriptSourcesBaseControl
            # (pscriptSourcesWithStageDiscovery # control # stage # discovery)
        )
        $ \base ->
          pencodeDefiniteArrayHeader # 31
            <> (psliceBS # 2 # (plengthBS # base - 2) # base)
            <> (pencodeDefiniteBytes # (pencodeScriptDiscoveryControl # discovery))
    )
    perror

pencodeInlineSourceHashControlV1 :: forall s.
  Term s (PInlineSourceHashControlV1 :--> PByteString)
pencodeInlineSourceHashControlV1 = phoistAcyclic $ plam $ \control ->
  pmatch control $ \c ->
  pif
    ( pand'List
        [ pfromData (pinlineSource'version c) #== 1
        , pfromData (pinlineSource'sourceIndex c) #>= 0
        , pfromData (pinlineSource'sourceTotalCount c)
            #> pfromData (pinlineSource'sourceIndex c)
        , pfromData (pinlineSource'languageTag c) #== 0
            #|| pfromData (pinlineSource'languageTag c) #== 3
            #|| pfromData (pinlineSource'languageTag c) #== 128
        , pfromData (pinlineSource'payloadOffset c) #> 0
        , pfromData (pinlineSource'payloadLength c) #>= 0
        , pfromData (pinlineSource'itemLength c)
            #== pfromData (pinlineSource'payloadOffset c)
              + pfromData (pinlineSource'payloadLength c)
        , pfromData (pinlineSource'itemLength c) #> 0
        , pfromData (pinlineSource'itemLength c) #<= pmaxAggregateFieldPreimageBytes
        , plengthBS # pfromData (pinlineSource'itemCommitment c) #== 32
        , Blake2b224.pcontrolIsWellFormed # pfromData (pinlineSource'hashControl c)
        ]
    )
    ( pconstant "\x89"
        <> pcborInt 1
        <> pcborInt (pfromData $ pinlineSource'sourceIndex c)
        <> pcborInt (pfromData $ pinlineSource'sourceTotalCount c)
        <> pcborInt (pfromData $ pinlineSource'languageTag c)
        <> pcborInt (pfromData $ pinlineSource'payloadOffset c)
        <> pcborInt (pfromData $ pinlineSource'payloadLength c)
        <> pcborInt (pfromData $ pinlineSource'itemLength c)
        <> (pencodeDefiniteBytes # pfromData (pinlineSource'itemCommitment c))
        <> ( pencodeDefiniteBytes
              # (Blake2b224.pencodeControlV1 # pfromData (pinlineSource'hashControl c))
           )
    )
    perror

pdecodeInlineSourceHashControlV1 :: forall s.
  Term s (PByteString :--> PInlineSourceHashControlV1)
pdecodeInlineSourceHashControlV1 = phoistAcyclic $ plam $ \controlCbor ->
  pmatch (pdeserialise # controlCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \items ->
      pif (plength # items #== 9)
        ( plet
            ( pcon $ PInlineSourceHashControlV1
                (pdata $ pasInt # (pelemAt # 0 # items))
                (pdata $ pasInt # (pelemAt # 1 # items))
                (pdata $ pasInt # (pelemAt # 2 # items))
                (pdata $ pasInt # (pelemAt # 3 # items))
                (pdata $ pasInt # (pelemAt # 4 # items))
                (pdata $ pasInt # (pelemAt # 5 # items))
                (pdata $ pasInt # (pelemAt # 6 # items))
                (pdata $ pasByteStr # (pelemAt # 7 # items))
                ( pdata $ Blake2b224.pdecodeControlV1
                    # (pasByteStr # (pelemAt # 8 # items))
                )
            )
            $ \control ->
              pif (pencodeInlineSourceHashControlV1 # control #== controlCbor)
                control perror
        )
        perror

pencodeScriptSourcesBaseControl :: forall s.
  Term s (PScriptSourcesControlV1 :--> PByteString)
pencodeScriptSourcesBaseControl = phoistAcyclic $ plam $ \control ->
  pmatch control $ \c ->
    pencodeScriptSourcesWitness
      # pfromData (pscriptSources'compactCbor c)
      # pfromData (pscriptSources'witnessSetCompactCbor c)
      # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
      # pfromData (pscriptSources'contextCbor c)
      # pfromData (pscriptSources'resolvedInputCount c)
      # pfromData (pscriptSources'resolvedInputsAccumulator c)
      # pfromData (pscriptSources'signerCount c)
      # pfromData (pscriptSources'signerFrontierCommitment c)
      # pfromData (pscriptSources'resolvedItemPeaks c)
      # pfromData (pscriptSources'stage c)
      # pfromData (pscriptSources'sourceCount c)
      # pfromData (pscriptSources'sourcePeaks c)
      # pfromData (pscriptSources'redeemerCount c)
      # pfromData (pscriptSources'redeemerPeaks c)
      # pfromData (pscriptSources'replayCursor c)
      # pfromData (pscriptSources'replayAccumulator c)
      # pfromData (pscriptSources'replayRemainingScheduleHash c)
      # pfromData (pscriptSources'spendIndex c)
      # pfromData (pscriptSources'purposeCount c)
      # pfromData (pscriptSources'purposePeaks c)
      # pfromData (pscriptSources'outputCursor c)
      # pfromData (pscriptSources'outputCount c)
      # pfromData (pscriptSources'outputPeaks c)
      # pfromData (pscriptSources'outputTotalCount c)
      # pfromData (pscriptSources'receiveScan c)
      # pfromData (pscriptSources'sourceTotalCount c)
      # pfromData (pscriptSources'redeemerTotalCount c)
      # pfromData (pscriptSources'observerScan c)
      # pfromData (pscriptSources'mintFold c)
      # pfromData (pscriptSources'resolutionScheduleHash c)

pencodeScriptSourcesPendingSourceWitness :: forall s.
  Term s (PScriptSourcesControlV1 :--> PByteString :--> PByteString)
pencodeScriptSourcesPendingSourceWitness = phoistAcyclic $ plam $
  \control pendingSourceCbor ->
  pmatch control $ \c ->
  pif
    ( pfromData (pscriptSources'stage c) #== 0
        #&& pendingSourceCbor #/= pconstant ""
    )
    ( plet (pencodeScriptSourcesBaseControl # control) $ \base ->
        (pencodeDefiniteArrayHeader # 31)
          <> (psliceBS # 2 # (plengthBS # base - 2) # base)
          <> (pencodeDefiniteBytes # pendingSourceCbor)
    )
    perror

pencodeScriptSourcesRedeemerItemWitness :: forall s.
  Term s (PScriptSourcesControlV1 :--> PByteString :--> PByteString)
pencodeScriptSourcesRedeemerItemWitness = phoistAcyclic $ plam $
  \control redeemerItemControlHash ->
  pmatch control $ \c ->
  pif
    ( pfromData (pscriptSources'stage c) #== 1
        #&& plengthBS # redeemerItemControlHash #== 32
    )
    ( plet (pencodeScriptSourcesBaseControl # control) $ \base ->
        (pencodeDefiniteArrayHeader # 31)
          <> (psliceBS # 2 # (plengthBS # base - 2) # base)
          <> (pencodeDefiniteBytes # redeemerItemControlHash)
    )
    perror

pencodeScriptSourcesOutputProofWitness :: forall s.
  Term s
    ( PScriptSourcesControlV1 :--> LedgerOutputProof.PLedgerOutputProofControlV1
        :--> PByteString
    )
pencodeScriptSourcesOutputProofWitness = phoistAcyclic $ plam $
  \control outputProof ->
  pmatch control $ \c ->
  pif (pfromData (pscriptSources'stage c) #== 5)
    ( plet (pencodeScriptSourcesBaseControl # control) $ \base ->
        (pencodeDefiniteArrayHeader # 31)
          <> (psliceBS # 2 # (plengthBS # base - 2) # base)
          <> ( pencodeDefiniteBytes
                # (LedgerOutputProof.pencodeControlV1 # outputProof)
             )
    )
    perror

pscriptSourcesControlFromDataItems :: forall s.
  Term s
    ( PBuiltinList PData :--> PByteString :--> PScriptSourcesControlV1 )
pscriptSourcesControlFromDataItems = phoistAcyclic $ plam $ \items pendingSourceCbor ->
  plet (pasList # (pelemAt # 24 # items)) $ \receiveItems ->
  plet (pasList # (pelemAt # 27 # items)) $ \observerItems ->
  plet (pasList # (pelemAt # 28 # items)) $ \mintItems ->
  pif
    ( plength # items #>= 30
        #&& plength # receiveItems #== 6
        #&& plength # observerItems #== 3
        #&& plength # mintItems #== 12
        #&& plengthBS # (pasByteStr # (pelemAt # 29 # items)) #== 32
    )
    ( plet
        ( pcon $ PReceivePurposeScanControlV1
            (di 0 receiveItems) (dp 1 receiveItems) (di 2 receiveItems)
            (db 3 receiveItems) (db 4 receiveItems) (dp 5 receiveItems)
        )
        $ \receiveScan ->
      plet
        ( pcon $ PObserverPurposeScanControlV1
            (di 0 observerItems) (di 2 observerItems) (db 1 observerItems)
        )
        $ \observerScan ->
      plet
        ( pcon $ PMintFoldControlV1
            (di 0 mintItems) (di 1 mintItems) (db 2 mintItems)
            (db 3 mintItems) (di 4 mintItems) (db 5 mintItems)
            (di 6 mintItems) (di 7 mintItems) (di 8 mintItems)
            (db 9 mintItems) (di 10 mintItems) (dp 11 mintItems)
        )
        $ \mintFold ->
        pcon $ PScriptSourcesControlV1
          (db 0 items) (db 1 items) (db 2 items) (db 3 items)
          (di 4 items) (db 5 items) (di 6 items) (db 7 items)
          (dp 8 items) (di 9 items) (di 10 items) (dp 11 items)
          (di 12 items) (dp 13 items) (di 14 items) (db 15 items)
          (db 16 items) (di 17 items) (di 18 items) (dp 19 items)
          (di 20 items) (di 21 items) (dp 22 items) (di 23 items)
          (pdata receiveScan) (di 25 items) (di 26 items) (pdata observerScan)
          (pdata pemptyScriptDiscoveryControl) (pdata $ pcon PDNothing)
          (pdata pendingSourceCbor) (pdata mintFold) (db 29 items)
    )
    perror
  where
    di index xs = pdata $ pasInt # (pelemAt # index # xs)
    db index xs = pdata $ pasByteStr # (pelemAt # index # xs)
    dp index xs = pdata $ pdecodeFrontierPeakItems # (pasList # (pelemAt # index # xs))

pscriptSourcesStageZeroControlFromWitness :: forall s.
  Term s (PByteString :--> PScriptSourcesControlV1)
pscriptSourcesStageZeroControlFromWitness = phoistAcyclic $ plam $ \workWitnessCbor ->
  pmatch (pdeserialise # workWitnessCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \items ->
      pif (plength # items #== 30)
        (pscriptSourcesControlFromDataItems # items # pconstant "")
        ( pif (plength # items #== 31)
            ( plet (pscriptSourcesControlFromDataItems # items # pconstant "") $ \base ->
              pmatch base $ \c ->
              pif
                ( pfromData (pscriptSources'stage c) #== 0
                    #|| pfromData (pscriptSources'stage c) #== 1
                )
                ( pcon $ PScriptSourcesControlV1
                    (pscriptSources'compactCbor c)
                    (pscriptSources'witnessSetCompactCbor c)
                    (pscriptSources'fieldPreimageLengthsCbor c)
                    (pscriptSources'contextCbor c)
                    (pscriptSources'resolvedInputCount c)
                    (pscriptSources'resolvedInputsAccumulator c)
                    (pscriptSources'signerCount c)
                    (pscriptSources'signerFrontierCommitment c)
                    (pscriptSources'resolvedItemPeaks c)
                    (pscriptSources'stage c)
                    (pscriptSources'sourceCount c)
                    (pscriptSources'sourcePeaks c)
                    (pscriptSources'redeemerCount c)
                    (pscriptSources'redeemerPeaks c)
                    (pscriptSources'replayCursor c)
                    (pscriptSources'replayAccumulator c)
                    (pscriptSources'replayRemainingScheduleHash c)
                    (pscriptSources'spendIndex c)
                    (pscriptSources'purposeCount c)
                    (pscriptSources'purposePeaks c)
                    (pscriptSources'outputCursor c)
                    (pscriptSources'outputCount c)
                    (pscriptSources'outputPeaks c)
                    (pscriptSources'outputTotalCount c)
                    (pscriptSources'receiveScan c)
                    (pscriptSources'sourceTotalCount c)
                    (pscriptSources'redeemerTotalCount c)
                    (pscriptSources'observerScan c)
                    (pscriptSources'discovery c)
                    (pscriptSources'outputProof c)
                    (pdata $ pasByteStr # (pelemAt # 30 # items))
                    (pscriptSources'mintFold c)
                    (pscriptSources'resolutionScheduleHash c)
                )
                ( pif (pfromData (pscriptSources'stage c) #== 5)
                    ( pcon $ PScriptSourcesControlV1
                        (pscriptSources'compactCbor c)
                        (pscriptSources'witnessSetCompactCbor c)
                        (pscriptSources'fieldPreimageLengthsCbor c)
                        (pscriptSources'contextCbor c)
                        (pscriptSources'resolvedInputCount c)
                        (pscriptSources'resolvedInputsAccumulator c)
                        (pscriptSources'signerCount c)
                        (pscriptSources'signerFrontierCommitment c)
                        (pscriptSources'resolvedItemPeaks c)
                        (pscriptSources'stage c)
                        (pscriptSources'sourceCount c)
                        (pscriptSources'sourcePeaks c)
                        (pscriptSources'redeemerCount c)
                        (pscriptSources'redeemerPeaks c)
                        (pscriptSources'replayCursor c)
                        (pscriptSources'replayAccumulator c)
                        (pscriptSources'replayRemainingScheduleHash c)
                        (pscriptSources'spendIndex c)
                        (pscriptSources'purposeCount c)
                        (pscriptSources'purposePeaks c)
                        (pscriptSources'outputCursor c)
                        (pscriptSources'outputCount c)
                        (pscriptSources'outputPeaks c)
                        (pscriptSources'outputTotalCount c)
                        (pscriptSources'receiveScan c)
                        (pscriptSources'sourceTotalCount c)
                        (pscriptSources'redeemerTotalCount c)
                        (pscriptSources'observerScan c)
                        (pscriptSources'discovery c)
                        ( pdata $ pcon $ PDJust $ pdata
                            $ LedgerOutputProof.pdecodeControlV1
                              # (pasByteStr # (pelemAt # 30 # items))
                        )
                        (pscriptSources'pendingSourceCbor c)
                        (pscriptSources'mintFold c)
                        (pscriptSources'resolutionScheduleHash c)
                    )
                    perror
                )
            )
            perror
        )

pscriptSourcesStageZeroWorkWitnessHasCanonicalShape :: forall s.
  Term s (PByteString :--> PByteString :--> PBool)
pscriptSourcesStageZeroWorkWitnessHasCanonicalShape = phoistAcyclic $ plam $
  \workWitnessCbor pendingSourceCbor ->
  pmatch (pdecodeDefiniteArrayHeaderAt # workWitnessCbor # 0) $ \(PPair offset itemCount) ->
  plet (pif (pendingSourceCbor #== pconstant "") 30 31) $ \expectedItemCount ->
    offset #== 2
      #&& itemCount #== expectedItemCount
      #&& pif (pendingSourceCbor #== pconstant "")
        (pconstant True)
        ( plet (pencodeDefiniteBytes # pendingSourceCbor) $ \encodedPending ->
          plet (plengthBS # workWitnessCbor - plengthBS # encodedPending) $ \pendingOffset ->
            pendingOffset #>= offset
              #&& psliceBS # pendingOffset # (plengthBS # encodedPending) # workWitnessCbor
                #== encodedPending
        )

pscriptSourcesStageZeroControlIsBound :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PBool
    )
pscriptSourcesStageZeroControlIsBound = phoistAcyclic $ plam $ \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  plet (pfromData $ pscriptSources'pendingSourceCbor c) $ \pendingSourceCbor ->
  plet
    ( pif (pendingSourceCbor #== pconstant "")
        (pconstant True)
        ( plet (pdecodeInlineSourceHashControlV1 # pendingSourceCbor) $ \pending ->
          pmatch pending $ \active ->
            pfromData (pinlineSource'sourceIndex active)
                #== pfromData (pscriptSources'sourceCount c)
              #&& pfromData (pinlineSource'sourceTotalCount active)
                #== pfromData (pscriptSources'sourceTotalCount c)
        )
    )
    $ \pendingIsBound ->
    pand'List
      [ pfromData (pscriptSources'stage c) #== 0
      , NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (pscriptSources'compactCbor c)
          # pfromData (pscriptSources'witnessSetCompactCbor c)
          # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
          #== pfromData (pmachineState'transactionCommitment preState)
      , phashValidationContext # pfromData (pscriptSources'contextCbor c)
          #== pfromData (pmachineState'validationContextHash preState)
      , pfromData (pscriptSources'resolvedInputCount c) #>= 0
      , plengthBS # pfromData (pscriptSources'resolvedInputsAccumulator c) #== 32
      , pfromData (pscriptSources'signerCount c) #>= 0
      , plengthBS # pfromData (pscriptSources'signerFrontierCommitment c) #== 32
      , pfromData (pscriptSources'resolvedItemPeaks c) #== pnil
      , pfromData (pscriptSources'replayCursor c) #== 0
      , plengthBS # pfromData (pscriptSources'replayAccumulator c) #== 32
      , plengthBS # pfromData (pscriptSources'replayRemainingScheduleHash c) #== 32
      , plengthBS # pfromData (pscriptSources'resolutionScheduleHash c) #== 32
      , pfromData (pscriptSources'spendIndex c) #== 0
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'sourceCount c)
          # pfromData (pscriptSources'sourcePeaks c)
      , pfromData (pscriptSources'sourceTotalCount c)
          #>= pfromData (pscriptSources'sourceCount c)
      , pfromData (pscriptSources'sourceTotalCount c) #<= pmaxTxSizeDerivedItemCount
      , pfromData (pscriptSources'redeemerCount c) #== 0
      , pfromData (pscriptSources'redeemerPeaks c) #== pnil
      , pfromData (pscriptSources'redeemerTotalCount c) #== 0
      , pfromData (pscriptSources'purposeCount c) #== 0
      , pfromData (pscriptSources'purposePeaks c) #== pnil
      , pfromData (pscriptSources'outputCursor c) #== 0
      , pfromData (pscriptSources'outputCount c) #== 0
      , pfromData (pscriptSources'outputPeaks c) #== pnil
      , pfromData (pscriptSources'outputTotalCount c) #== 0
      , pfromData (pscriptSources'receiveScan c) #== pemptyReceivePurposeScanControl
      , pfromData (pscriptSources'observerScan c) #== pemptyObserverPurposeScanControl
      , pfromData (pscriptSources'mintFold c) #== pemptyMintFoldControl
      , pfromData (pscriptSources'discovery c) #== pemptyScriptDiscoveryControl
      , pfromData (pscriptSources'outputProof c) #== pcon PDNothing
      , pendingIsBound
      , pscriptSourcesStageZeroWorkWitnessHasCanonicalShape
          # pfromData (poneStep'workWitnessCbor stepWitness) # pendingSourceCbor
      ]

pscriptSourcesStageZeroPrefixBeforeStage :: forall s.
  Term s (PScriptSourcesControlV1 :--> PInteger :--> PByteString)
pscriptSourcesStageZeroPrefixBeforeStage = phoistAcyclic $ plam $ \control itemCount ->
  pmatch control $ \c ->
    (pencodeDefiniteArrayHeader # itemCount)
      <> (pencodeDefiniteBytes # pfromData (pscriptSources'compactCbor c))
      <> (pencodeDefiniteBytes # pfromData (pscriptSources'witnessSetCompactCbor c))
      <> (pencodeDefiniteBytes # pfromData (pscriptSources'fieldPreimageLengthsCbor c))
      <> (pencodeDefiniteBytes # pfromData (pscriptSources'contextCbor c))
      <> pcborInt (pfromData $ pscriptSources'resolvedInputCount c)
      <> (pencodeDefiniteBytes # pfromData (pscriptSources'resolvedInputsAccumulator c))
      <> pcborInt (pfromData $ pscriptSources'signerCount c)
      <> (pencodeDefiniteBytes # pfromData (pscriptSources'signerFrontierCommitment c))
      <> (pencodeFrontier # pfromData (pscriptSources'resolvedItemPeaks c))

pscriptSourcesStageZeroSuccessorWorkIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PByteString :--> PBool
    )
pscriptSourcesStageZeroSuccessorWorkIsExact = phoistAcyclic $ plam $
  \pre witness nextWorkWitnessCbor ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch (pfromData $ poneStep'claimedSuccessor stepWitness) $ \postState ->
    pfromData (pmachineState'phase postState) #== pcon PScriptSources
      #&& pfromData (pmachineState'workRoot postState)
        #== phashWorkWitness # pcon PScriptSources
          # (pfromData (pmachineState'programCounter preState) + 1)
          # nextWorkWitnessCbor

pscriptSourcesStageZeroFinishSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PBool
    )
pscriptSourcesStageZeroFinishSuccessorIsExact = phoistAcyclic $ plam $
  \pre witness control ->
  pmatch witness $ \stepWitness ->
  plet (pscriptSourcesStageZeroPrefixBeforeStage # control # 30) $ \prefix ->
  plet (plengthBS # prefix) $ \stageOffset ->
  plet (pfromData $ poneStep'workWitnessCbor stepWitness) $ \workWitnessCbor ->
  plet (plengthBS # workWitnessCbor) $ \workLength ->
    pif (psliceBS # 0 # stageOffset # workWitnessCbor #== prefix)
      ( pif (psliceBS # stageOffset # 1 # workWitnessCbor #== pconstant "\x00")
          ( pscriptSourcesStageZeroSuccessorWorkIsExact
              # pre # witness
              # ( prefix <> pconstant "\x01"
                    <> ( psliceBS # (stageOffset + 1)
                          # (workLength - stageOffset - 1) # workWitnessCbor
                       )
                )
          )
          perror
      )
      perror

pscriptSourcesStageZeroBeginSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PInteger :--> PByteString :--> PBool
    )
pscriptSourcesStageZeroBeginSuccessorIsExact = phoistAcyclic $ plam $
  \pre witness control sourceTotalCount pendingSourceCbor ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  plet (pencodeObserverPurposeScanControl # pfromData (pscriptSources'observerScan c)) $
    \encodedObserverScan ->
  plet
    ( pcborInt (pfromData $ pscriptSources'sourceTotalCount c)
        <> pcborInt (pfromData $ pscriptSources'redeemerTotalCount c)
        <> encodedObserverScan
        <> (pencodeMintFoldControl # pfromData (pscriptSources'mintFold c))
        <> (pencodeDefiniteBytes # pfromData (pscriptSources'resolutionScheduleHash c))
    )
    $ \oldSuffix ->
  plet (pfromData $ poneStep'workWitnessCbor stepWitness) $ \workWitnessCbor ->
  plet (plengthBS # workWitnessCbor - plengthBS # oldSuffix) $ \suffixOffset ->
  pif
    ( suffixOffset #>= 2
        #&& psliceBS # suffixOffset # (plengthBS # oldSuffix) # workWitnessCbor
          #== oldSuffix
    )
    ( pscriptSourcesStageZeroSuccessorWorkIsExact
        # pre # witness
        # ( (pencodeDefiniteArrayHeader # 31)
              <> (psliceBS # 2 # (suffixOffset - 2) # workWitnessCbor)
              <> pcborInt sourceTotalCount
              <> pcborInt (pfromData $ pscriptSources'redeemerTotalCount c)
              <> encodedObserverScan
              <> (pencodeMintFoldControl # pfromData (pscriptSources'mintFold c))
              <> (pencodeDefiniteBytes # pfromData (pscriptSources'resolutionScheduleHash c))
              <> (pencodeDefiniteBytes # pendingSourceCbor)
          )
    )
    perror

pscriptSourcesStageZeroPendingSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PByteString :--> PBool
    )
pscriptSourcesStageZeroPendingSuccessorIsExact = phoistAcyclic $ plam $
  \pre witness control pendingSourceCbor ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  plet (pencodeDefiniteBytes # pfromData (pscriptSources'pendingSourceCbor c)) $
    \oldPending ->
  plet (pfromData $ poneStep'workWitnessCbor stepWitness) $ \workWitnessCbor ->
  plet (plengthBS # workWitnessCbor - plengthBS # oldPending) $ \pendingOffset ->
  pif
    ( pendingOffset #>= 2
        #&& psliceBS # pendingOffset # (plengthBS # oldPending) # workWitnessCbor
          #== oldPending
    )
    ( pscriptSourcesStageZeroSuccessorWorkIsExact
        # pre # witness
        # ( (psliceBS # 0 # pendingOffset # workWitnessCbor)
              <> (pencodeDefiniteBytes # pendingSourceCbor)
          )
    )
    perror

pscriptSourcesStageZeroTerminalSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak) :--> PBool
    )
pscriptSourcesStageZeroTerminalSuccessorIsExact = phoistAcyclic $ plam $
  \pre witness control sourceCount sourcePeaks ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  plet
    ( (pscriptSourcesStageZeroPrefixBeforeStage # control # 31)
        <> pconstant "\x00"
    )
    $ \prefix ->
  plet (plengthBS # prefix) $ \sourceOffset ->
  plet
    ( pcborInt (pfromData $ pscriptSources'sourceCount c)
        <> (pencodeFrontier # pfromData (pscriptSources'sourcePeaks c))
    )
    $ \oldSourceFields ->
  plet (sourceOffset + plengthBS # oldSourceFields) $ \afterSourceOffset ->
  plet (pencodeDefiniteBytes # pfromData (pscriptSources'pendingSourceCbor c)) $
    \oldPending ->
  plet (pfromData $ poneStep'workWitnessCbor stepWitness) $ \workWitnessCbor ->
  plet (plengthBS # workWitnessCbor - plengthBS # oldPending) $ \pendingOffset ->
  pif
    ( pendingOffset #>= afterSourceOffset
        #&& psliceBS # 0 # sourceOffset # workWitnessCbor #== prefix
        #&& psliceBS # sourceOffset # (plengthBS # oldSourceFields) # workWitnessCbor
          #== oldSourceFields
        #&& psliceBS # pendingOffset # (plengthBS # oldPending) # workWitnessCbor
          #== oldPending
    )
    ( pscriptSourcesStageZeroSuccessorWorkIsExact
        # pre # witness
        # ( (pencodeDefiniteArrayHeader # 30)
              <> (psliceBS # 2 # (sourceOffset - 2) # prefix)
              <> pcborInt sourceCount
              <> (pencodeFrontier # sourcePeaks)
              <> ( psliceBS # afterSourceOffset
                    # (pendingOffset - afterSourceOffset) # workWitnessCbor
                 )
          )
    )
    perror

pinlineSourceHashBlockV1 :: forall s.
  Term s
    ( PInlineSourceHashControlV1 :--> BoundedItem.PChunkProofV1
        :--> PMaybeData BoundedItem.PChunkProofV1 :--> PMaybe PByteString
    )
pinlineSourceHashBlockV1 = phoistAcyclic $ plam $ \pending chunkProof nextChunkProof ->
  pmatch pending $ \active ->
  plet (pfromData $ pinlineSource'hashControl active) $ \hashControl ->
  pmatch hashControl $ \hash ->
  pif (pfromData (Blake2b224.pctl'stage hash) #/= Blake2b224.pstageReady)
    (pcon PNothing)
    ( plet
        ( pfromData (Blake2b224.pctl'totalLength hash)
            - pfromData (Blake2b224.pctl'cursor hash)
        )
        $ \remaining ->
      plet (pif (remaining #< Blake2b224.pblockBytes) remaining Blake2b224.pblockBytes) $
        \blockLength ->
      plet
        (pif (pfromData (Blake2b224.pctl'cursor hash) #== 0) (blockLength - 1) blockLength)
        $ \contentLength ->
      plet
        ( pif (pfromData (Blake2b224.pctl'cursor hash) #== 0)
            (pfromData $ pinlineSource'payloadOffset active)
            ( pfromData (pinlineSource'payloadOffset active)
                + pfromData (Blake2b224.pctl'cursor hash) - 1
            )
        )
        $ \itemCursor ->
      plet
        (pif (contentLength #== 0) 0 (pdiv # itemCursor # BoundedItem.pchunkBytes))
        $ \expectedChunkIndex ->
      plet
        ( pif (contentLength #== 0)
            (pfromData $ pinlineSource'payloadOffset active)
            (itemCursor - expectedChunkIndex * BoundedItem.pchunkBytes)
        )
        $ \offset ->
      pmatch chunkProof $ \chunk ->
      pif
        ( pand'List
            [ pfromData (Blake2b224.pctl'totalLength hash)
                #== pfromData (pinlineSource'payloadLength active) + 1
            , pfromData (BoundedItem.pchunkProof'fieldIndex chunk) #== 6
            , pfromData (BoundedItem.pchunkProof'itemIndex chunk)
                #== pfromData (pinlineSource'sourceIndex active)
            , pfromData (BoundedItem.pchunkProof'totalLength chunk)
                #== pfromData (pinlineSource'itemLength active)
            , pfromData (BoundedItem.pchunkProof'chunkIndex chunk)
                #== expectedChunkIndex
            , BoundedItem.pverifyChunk
                # pfromData (pinlineSource'itemCommitment active) # chunkProof
            , offset #>= 0
            , offset #<= plengthBS # pfromData (BoundedItem.pchunkProof'chunk chunk)
            ]
        )
        ( plet
            (plengthBS # pfromData (BoundedItem.pchunkProof'chunk chunk) - offset)
            $ \available ->
          plet (contentLength #> available) $ \crossesChunk ->
          plet
            ( pif crossesChunk
                ( pmatch nextChunkProof $ \case
                    PDNothing -> pcon PNothing
                    PDJust nextD ->
                      plet (pfromData nextD) $ \next ->
                      pmatch next $ \nextChunk ->
                      pif
                        ( pand'List
                            [ pfromData (BoundedItem.pchunkProof'fieldIndex nextChunk) #== 6
                            , pfromData (BoundedItem.pchunkProof'itemIndex nextChunk)
                                #== pfromData (pinlineSource'sourceIndex active)
                            , pfromData (BoundedItem.pchunkProof'totalLength nextChunk)
                                #== pfromData (pinlineSource'itemLength active)
                            , pfromData (BoundedItem.pchunkProof'chunkIndex nextChunk)
                                #== expectedChunkIndex + 1
                            , BoundedItem.pverifyChunk
                                # pfromData (pinlineSource'itemCommitment active) # next
                            ]
                        )
                        ( pcon $ PJust
                            $ pfromData (BoundedItem.pchunkProof'chunk chunk)
                              <> pfromData (BoundedItem.pchunkProof'chunk nextChunk)
                        )
                        (pcon PNothing)
                )
                ( pif (nextChunkProof #== pcon PDNothing)
                    (pcon $ PJust $ pfromData (BoundedItem.pchunkProof'chunk chunk))
                    (pcon PNothing)
                )
            )
            $ \authenticatedWindow ->
          pmatch authenticatedWindow $ \case
            PNothing -> pcon PNothing
            PJust window ->
              plet (psliceBS # offset # contentLength # window) $ \content ->
              plet
                ( pif (pfromData (Blake2b224.pctl'cursor hash) #== 0)
                    ( (preplicateBS # 1
                          # (pintegerToByte # pfromData (pinlineSource'languageTag active))
                      )
                        <> content
                    )
                    content
                )
                $ \block ->
                pif (plengthBS # block #== blockLength)
                  (pcon $ PJust block)
                  (pcon PNothing)
        )
        (pcon PNothing)
    )

pverifyScriptSourcesStageZeroFinishSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyScriptSourcesStageZeroFinishSemanticsV1 = phoistAcyclic $ plam $ \pre witness ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  plet
    (NativeCompact.pdecodeNativeTxWitnessSetCompact # pfromData (pscriptSources'witnessSetCompactCbor c))
    $ \witnessSet ->
  pmatch witnessSet $ \ws ->
  plet (pfromData $ pwitnessSetCompact'scriptTxWitsHash ws) $ \scriptCommitment ->
  plet
    ( pif (scriptCommitment #== NativeField.pemptyFieldCommitment)
        ( pfromData (pscriptSources'sourceCount c) #== 0
            #&& pfromData (pscriptSources'sourceTotalCount c) #== 0
            #&& pfromData (pscriptSources'sourcePeaks c) #== pnil
        )
        ( pfromData (pscriptSources'sourceTotalCount c) #> 0
            #&& pfromData (pscriptSources'sourceCount c)
              #== pfromData (pscriptSources'sourceTotalCount c)
        )
    )
    $ \sourceScanIsComplete ->
    pand'List
      [ pscriptSourcesStageZeroControlIsBound # pre # witness # control
      , pfromData (pscriptSources'pendingSourceCbor c) #== pconstant ""
      , sourceScanIsComplete
      , pscriptSourcesStageZeroFinishSuccessorIsExact # pre # witness # control
      ]

pverifyScriptSourcesStageZeroBeginSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PItemProofV1 :--> BoundedItem.PChunkProofV1 :--> PBool
    )
pverifyScriptSourcesStageZeroBeginSemanticsV1 = phoistAcyclic $ plam $
  \pre witness collectionProof chunkProof ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  plet
    (NativeCompact.pdecodeNativeTxWitnessSetCompact # pfromData (pscriptSources'witnessSetCompactCbor c))
    $ \witnessSet ->
  pmatch witnessSet $ \ws ->
  pmatch collectionProof $ \item ->
  pmatch chunkProof $ \chunk ->
  plet
    ( pif (pfromData (pscriptSources'sourceTotalCount c) #== 0)
        (pfromData $ pitemProof'itemCount item)
        (pfromData $ pscriptSources'sourceTotalCount c)
    )
    $ \activeTotalCount ->
  plet
    ( NativeScriptScan.pversionedScriptHeaderV1
        # pfromData (BoundedItem.pchunkProof'chunk chunk)
        # pfromData (pitemProof'itemLength item)
    )
    $ \header ->
  plet
    ( pmatch header $ \case
        PNothing ->
          prejectedSuccessorIsExact
            # pre # pfromData (poneStep'claimedSuccessor stepWitness)
            # pconstant "E_INVALID_FIELD_TYPE"
        PJust itemHeader ->
          pmatch itemHeader $ \h ->
          plet
            ( pcon $ PInlineSourceHashControlV1
                (pdata 1)
                (pscriptSources'sourceCount c)
                (pdata activeTotalCount)
                (NativeScriptScan.pheader'languageTag h)
                (NativeScriptScan.pheader'payloadOffset h)
                (NativeScriptScan.pheader'payloadLength h)
                (pitemProof'itemLength item)
                (pitemProof'itemCommitment item)
                ( pdata $ Blake2b224.pinitialControlV1
                    # (pfromData (NativeScriptScan.pheader'payloadLength h) + 1)
                )
            )
            $ \pending ->
            pscriptSourcesStageZeroBeginSuccessorIsExact
              # pre # witness # control # activeTotalCount
              # (pencodeInlineSourceHashControlV1 # pending)
    )
    $ \headerResult ->
    pand'List
      [ pscriptSourcesStageZeroControlIsBound # pre # witness # control
      , pfromData (pscriptSources'pendingSourceCbor c) #== pconstant ""
      , activeTotalCount #> 0
      , activeTotalCount #<= pmaxTxSizeDerivedItemCount
      , pfromData (pitemProof'itemLength item) #> 0
      , pfromData (pitemProof'itemLength item) #<= pmaxAggregateFieldPreimageBytes
      , pfromData (pitemProof'fieldIndex item) #== 6
      , pfromData (pitemProof'itemCount item) #== activeTotalCount
      , pfromData (pitemProof'itemIndex item)
          #== pfromData (pscriptSources'sourceCount c)
      , pfromData (BoundedItem.pchunkProof'fieldIndex chunk) #== 6
      , pfromData (BoundedItem.pchunkProof'itemIndex chunk)
          #== pfromData (pitemProof'itemIndex item)
      , pfromData (BoundedItem.pchunkProof'totalLength chunk)
          #== pfromData (pitemProof'itemLength item)
      , pfromData (BoundedItem.pchunkProof'chunkIndex chunk) #== 0
      , pverifyBoundedCollectionItem
          # pfromData (pwitnessSetCompact'scriptTxWitsHash ws) # collectionProof
      , BoundedItem.pverifyChunk
          # pfromData (pitemProof'itemCommitment item) # chunkProof
      , headerResult
      ]

pinlineSourceWithHashControl :: forall s.
  Term s
    ( PInlineSourceHashControlV1 :--> Blake2b224.PBlake2b224TraceControlV1
        :--> PInlineSourceHashControlV1
    )
pinlineSourceWithHashControl = phoistAcyclic $ plam $ \pending hashControl ->
  pmatch pending $ \active ->
    pcon $ PInlineSourceHashControlV1
      (pinlineSource'version active)
      (pinlineSource'sourceIndex active)
      (pinlineSource'sourceTotalCount active)
      (pinlineSource'languageTag active)
      (pinlineSource'payloadOffset active)
      (pinlineSource'payloadLength active)
      (pinlineSource'itemLength active)
      (pinlineSource'itemCommitment active)
      (pdata hashControl)

pverifyScriptSourcesStageZeroHashBlockSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1
        :--> PMaybeData BoundedItem.PChunkProofV1 :--> PBool
    )
pverifyScriptSourcesStageZeroHashBlockSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  plet (pfromData $ pscriptSources'pendingSourceCbor c) $ \pendingCbor ->
  pif (pendingCbor #== pconstant "") perror $
  plet (pdecodeInlineSourceHashControlV1 # pendingCbor) $ \pending ->
  pmatch (pinlineSourceHashBlockV1 # pending # chunkProof # nextChunkProof) $ \case
    PNothing -> pconstant False
    PJust block ->
      pmatch
        ( Blake2b224.pstepV1
            # pfromData (pmatch pending $ \active -> pinlineSource'hashControl active)
            # (pcon $ PJust block)
        )
        $ \case
          PNothing -> perror
          PJust nextHashControl ->
            pscriptSourcesStageZeroControlIsBound # pre # witness # control
              #&& pscriptSourcesStageZeroPendingSuccessorIsExact
                # pre # witness # control
                # ( pencodeInlineSourceHashControlV1
                      # (pinlineSourceWithHashControl # pending # nextHashControl)
                  )

pverifyScriptSourcesStageZeroHashAdvanceSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyScriptSourcesStageZeroHashAdvanceSemanticsV1 = phoistAcyclic $ plam $
  \pre witness ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  plet (pfromData $ pscriptSources'pendingSourceCbor c) $ \pendingCbor ->
  pif (pendingCbor #== pconstant "") perror $
  plet (pdecodeInlineSourceHashControlV1 # pendingCbor) $ \pending ->
  plet (pfromData $ pmatch pending $ \active -> pinlineSource'hashControl active) $
    \hashControl ->
  pmatch hashControl $ \hash ->
  pif
    ( pfromData (Blake2b224.pctl'stage hash) #== Blake2b224.pstageReady
        #|| pfromData (Blake2b224.pctl'stage hash) #== Blake2b224.pstageTerminal
    )
    perror
    ( pmatch (Blake2b224.pstepV1 # hashControl # pcon PNothing) $ \case
        PNothing -> perror
        PJust nextHashControl ->
          pscriptSourcesStageZeroControlIsBound # pre # witness # control
            #&& pscriptSourcesStageZeroPendingSuccessorIsExact
              # pre # witness # control
              # ( pencodeInlineSourceHashControlV1
                    # (pinlineSourceWithHashControl # pending # nextHashControl)
                )
    )

pverifyScriptSourcesStageZeroHashTerminalSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyScriptSourcesStageZeroHashTerminalSemanticsV1 = phoistAcyclic $ plam $
  \pre witness ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  plet (pfromData $ pscriptSources'pendingSourceCbor c) $ \pendingCbor ->
  pif (pendingCbor #== pconstant "") perror $
  plet (pdecodeInlineSourceHashControlV1 # pendingCbor) $ \pending ->
  pmatch
    ( Blake2b224.pdigestV1
        # pfromData (pmatch pending $ \active -> pinlineSource'hashControl active)
    )
    $ \case
      PNothing -> perror
      PJust scriptHash ->
        pmatch pending $ \active ->
        plet (pfromData (pscriptSources'sourceCount c) + 1) $ \nextSourceCount ->
        plet
          ( pappendLeaf
              # pfromData (pscriptSources'sourceCount c)
              # pfromData (pscriptSources'sourcePeaks c)
              # ( ScriptProof.pinlineSourceLeafHash
                    # pfromData (pscriptSources'sourceCount c)
                    # pfromData (pinlineSource'languageTag active)
                    # scriptHash
                    # pfromData (pinlineSource'itemLength active)
                    # pfromData (pinlineSource'itemCommitment active)
                )
          )
          $ \nextSourcePeaks ->
          pscriptSourcesStageZeroControlIsBound # pre # witness # control
            #&& pscriptSourcesStageZeroTerminalSuccessorIsExact
              # pre # witness # control # nextSourceCount # nextSourcePeaks

pverifyScriptSourcesStageZeroSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageZeroSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  plet (pfromData $ pscriptSources'pendingSourceCbor c) $ \pendingCbor ->
  pif (pendingCbor #== pconstant "")
    ( pmatch auxiliary $ \case
        PNoAuxiliaryWitness ->
          pverifyScriptSourcesStageZeroFinishSemanticsV1 # pre # witness
        PTransactionFieldChunkWitness collectionProofD chunkProofD ->
          pverifyScriptSourcesStageZeroBeginSemanticsV1
            # pre # witness # pfromData collectionProofD # pfromData chunkProofD
        _ -> pconstant False
    )
    ( plet (pdecodeInlineSourceHashControlV1 # pendingCbor) $ \pending ->
      pmatch pending $ \active ->
      plet (pfromData $ pinlineSource'hashControl active) $ \hashControl ->
      pmatch hashControl $ \hash ->
      pif (pfromData (Blake2b224.pctl'stage hash) #== Blake2b224.pstageReady)
        ( pmatch auxiliary $ \case
            PScriptSourceHashBlockWitness chunkProofD nextChunkProofD ->
              pverifyScriptSourcesStageZeroHashBlockSemanticsV1
                # pre # witness # pfromData chunkProofD # pfromData nextChunkProofD
            _ -> pconstant False
        )
        ( pmatch auxiliary $ \case
            PNoAuxiliaryWitness ->
              pif (pfromData (Blake2b224.pctl'stage hash) #== Blake2b224.pstageTerminal)
                (pverifyScriptSourcesStageZeroHashTerminalSemanticsV1 # pre # witness)
                (pverifyScriptSourcesStageZeroHashAdvanceSemanticsV1 # pre # witness)
            _ -> pconstant False
        )
    )

pverifyScriptSourcesStageOneFinishRawSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyScriptSourcesStageOneFinishRawSemanticsV1 = phoistAcyclic $ plam $
  \pre witness ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet (pfromData $ poneStep'workWitnessCbor stepWitness) $ \workWitnessCbor ->
  plet (pscriptSourcesStageZeroControlFromWitness # workWitnessCbor) $ \control ->
  pmatch control $ \c ->
  plet
    (NativeCompact.pdecodeNativeTxWitnessSetCompact # pfromData (pscriptSources'witnessSetCompactCbor c))
    $ \witnessSet ->
  pmatch witnessSet $ \ws ->
  plet (pfromData $ pwitnessSetCompact'redeemerTxWitsHash ws) $ \redeemerCommitment ->
  plet
    ( pif (redeemerCommitment #== NativeField.pemptyFieldCommitment)
        ( pfromData (pscriptSources'redeemerCount c) #== 0
            #&& pfromData (pscriptSources'redeemerTotalCount c) #== 0
            #&& pfromData (pscriptSources'redeemerPeaks c) #== pnil
        )
        ( pfromData (pscriptSources'redeemerTotalCount c) #> 0
            #&& pfromData (pscriptSources'redeemerCount c)
              #== pfromData (pscriptSources'redeemerTotalCount c)
            #&& pfrontierIsWellFormed
              # pfromData (pscriptSources'redeemerCount c)
              # pfromData (pscriptSources'redeemerPeaks c)
        )
    )
    $ \scanIsComplete ->
  plet (pscriptSourcesStageZeroPrefixBeforeStage # control # 30) $ \stagePrefix ->
  plet (plengthBS # stagePrefix) $ \stageOffset ->
  plet (plengthBS # workWitnessCbor) $ \workLength ->
  plet
    ( stagePrefix <> pconstant "\x02"
        <> ( psliceBS # (stageOffset + 1)
              # (workLength - stageOffset - 1) # workWitnessCbor
           )
    )
    $ \nextWorkWitnessCbor ->
    pand'List
      [ pencodeScriptSourcesBaseControl # control #== workWitnessCbor
      , pfromData (pscriptSources'stage c) #== 1
      , NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (pscriptSources'compactCbor c)
          # pfromData (pscriptSources'witnessSetCompactCbor c)
          # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
          #== pfromData (pmachineState'transactionCommitment preState)
      , pfromData (pscriptSources'sourceTotalCount c)
          #== pfromData (pscriptSources'sourceCount c)
      , pfromData (pscriptSources'resolvedItemPeaks c) #== pnil
      , pfromData (pscriptSources'replayCursor c) #== 0
      , pfromData (pscriptSources'spendIndex c) #== 0
      , pfromData (pscriptSources'purposeCount c) #== 0
      , pfromData (pscriptSources'outputCursor c) #== 0
      , pfromData (pscriptSources'outputCount c) #== 0
      , pfromData (pscriptSources'receiveScan c) #== pemptyReceivePurposeScanControl
      , plengthBS # redeemerCommitment #== 32
      , plengthBS # pfromData (pscriptSources'resolutionScheduleHash c) #== 32
      , pfromData (pscriptSources'redeemerTotalCount c) #<= pmaxTxSizeDerivedItemCount
      , scanIsComplete
      , psliceBS # 0 # stageOffset # workWitnessCbor #== stagePrefix
      , psliceBS # stageOffset # 1 # workWitnessCbor #== pconstant "\x01"
      , pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness # nextWorkWitnessCbor
      ]

pscriptSourcesEarlyControlIsBound :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PInteger :--> PBool
    )
pscriptSourcesEarlyControlIsBound = phoistAcyclic $ plam $
  \pre witness control expectedStage ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  plet (pfromData $ pscriptSources'pendingSourceCbor c) $ \pendingCbor ->
  plet
    ( pif (pendingCbor #/= pconstant "")
        ( pif (expectedStage #== 1)
            (pencodeScriptSourcesRedeemerItemWitness # control # pendingCbor)
            perror
        )
        ( pif (expectedStage #== 5)
            ( pmatch (pfromData $ pscriptSources'outputProof c) $ \case
                PDNothing -> pencodeScriptSourcesBaseControl # control
                PDJust outputProofD ->
                  pencodeScriptSourcesOutputProofWitness
                    # control # pfromData outputProofD
            )
            (pencodeScriptSourcesBaseControl # control)
        )
    )
    $ \exactControlCbor ->
    pand'List
      [ pfromData (pscriptSources'stage c) #== expectedStage
      , NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (pscriptSources'compactCbor c)
          # pfromData (pscriptSources'witnessSetCompactCbor c)
          # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
          #== pfromData (pmachineState'transactionCommitment preState)
      , phashValidationContext # pfromData (pscriptSources'contextCbor c)
          #== pfromData (pmachineState'validationContextHash preState)
      , plengthBS # pfromData (pscriptSources'resolvedInputsAccumulator c) #== 32
      , plengthBS # pfromData (pscriptSources'signerFrontierCommitment c) #== 32
      , pfromData (pscriptSources'resolvedInputCount c) #>= 0
      , pfromData (pscriptSources'signerCount c) #>= 0
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'sourceCount c)
          # pfromData (pscriptSources'sourcePeaks c)
      , pfromData (pscriptSources'sourceTotalCount c)
          #>= pfromData (pscriptSources'sourceCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'redeemerCount c)
          # pfromData (pscriptSources'redeemerPeaks c)
      , pfromData (pscriptSources'redeemerTotalCount c)
          #>= pfromData (pscriptSources'redeemerCount c)
      , plengthBS # pfromData (pscriptSources'replayAccumulator c) #== 32
      , plengthBS # pfromData (pscriptSources'replayRemainingScheduleHash c) #== 32
      , pfromData (pscriptSources'replayCursor c) #>= 0
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'replayCursor c)
          # pfromData (pscriptSources'resolvedItemPeaks c)
      , pfromData (pscriptSources'spendIndex c) #>= 0
      , pfromData (pscriptSources'spendIndex c)
          #<= pfromData (pscriptSources'replayCursor c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'purposeCount c)
          # pfromData (pscriptSources'purposePeaks c)
      , pfromData (pscriptSources'outputCursor c) #>= 0
      , pfromData (pscriptSources'outputCursor c)
          #<= pfromData (pscriptSources'outputCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'outputCount c)
          # pfromData (pscriptSources'outputPeaks c)
      , pfromData (pscriptSources'outputTotalCount c)
          #>= pfromData (pscriptSources'outputCount c)
      , pfromData (pscriptSources'outputTotalCount c) #<= pmaxTxSizeDerivedItemCount
      , pif (expectedStage #== 1)
          (pendingCbor #== pconstant "" #|| plengthBS # pendingCbor #== 32)
          (pendingCbor #== pconstant "")
      , pif (expectedStage #== 5)
          ( pmatch (pfromData $ pscriptSources'outputProof c) $ \case
              PDNothing -> pconstant True
              PDJust outputProofD ->
                plet (pfromData outputProofD) $ \outputProof ->
                pmatch outputProof $ \proof ->
                  LedgerOutputProof.pcontrolIsWellFormed # outputProof
                    #&& pfromData (LedgerOutputProof.pproof'outputIndex proof)
                      #== pfromData (pscriptSources'outputCursor c)
                    #&& pfromData (LedgerOutputProof.pproof'outputIndex proof)
                      #< pfromData (pscriptSources'outputCount c)
          )
          (pfromData (pscriptSources'outputProof c) #== pcon PDNothing)
      , pif (expectedStage #< 4)
          ( pfromData (pscriptSources'outputCount c) #== 0
              #&& pfromData (pscriptSources'outputTotalCount c) #== 0
          )
          ( pif (expectedStage #== 4)
              (pfromData (pscriptSources'outputCursor c) #== 0)
              (pconstant True)
          )
      , pif (expectedStage #== 4)
          (pconstant True)
          ( pfromData (pscriptSources'outputTotalCount c)
              #== pfromData (pscriptSources'outputCount c)
          )
      , pscriptSourcesReceiveScanIsWellFormed # expectedStage # control
      , pfromData (pscriptSources'observerScan c) #== pemptyObserverPurposeScanControl
      , pscriptSourcesMintFoldIsWellFormed
          # expectedStage # pfromData (pscriptSources'mintFold c)
      , pfromData (pscriptSources'discovery c) #== pemptyScriptDiscoveryControl
      , pfromData (poneStep'workWitnessCbor stepWitness) #== exactControlCbor
      ]

pscriptSourcesReceiveScanIsWellFormed :: forall s.
  Term s (PInteger :--> PScriptSourcesControlV1 :--> PBool)
pscriptSourcesReceiveScanIsWellFormed = phoistAcyclic $ plam $ \stage control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'receiveScan c) $ \scan ->
  plet
    ( pif (stage #< 5) 0
        (pif (stage #== 5) (pfromData $ pscriptSources'outputCursor c)
          (pfromData $ pscriptSources'outputCount c))
    )
    $ \descriptorCount ->
    pand'List
      [ pfrontierIsWellFormed
          # descriptorCount
          # pfromData (preceiveScan'descriptorPeaks scan)
      , pfromData (preceiveScan'sourceCount scan) #>= 0
      , pfromData (preceiveScan'sourceCount scan)
          #<= pfromData (pscriptSources'outputCount c)
      , pfrontierIsWellFormed
          # pfromData (preceiveScan'sourceCount scan)
          # pfromData (preceiveScan'sourcePeaks scan)
      , pfromData (preceiveScan'receiveCount scan) #== 0
      , pfromData (preceiveScan'previousHash scan) #== pconstant ""
      , pfromData (preceiveScan'candidateHash scan) #== pconstant ""
      , pif (stage #< 5)
          (pcon scan #== pemptyReceivePurposeScanControl)
          (pconstant True)
      ]

pscriptSourcesMintFoldActiveIsEmpty :: forall s.
  Term s (PMintFoldControlV1 :--> PBool)
pscriptSourcesMintFoldActiveIsEmpty = phoistAcyclic $ plam $ \fold ->
  pmatch fold $ \f ->
    pand'List
      [ pfromData (pmintFold'activePolicy f) #== pconstant ""
      , pfromData (pmintFold'itemLength f) #== 0
      , pfromData (pmintFold'itemCommitment f) #== pconstant ""
      , pfromData (pmintFold'itemCursor f) #== 0
      , pfromData (pmintFold'assetsRemaining f) #== 0
      , pfromData (pmintFold'policyAssetCursor f) #== 0
      , pfromData (pmintFold'previousAsset f) #== pconstant ""
      ]

pscriptSourcesMintFoldIsWellFormed :: forall s.
  Term s (PInteger :--> PMintFoldControlV1 :--> PBool)
pscriptSourcesMintFoldIsWellFormed = phoistAcyclic $ plam $ \stage fold ->
  pmatch fold $ \f ->
  plet
    ( pand'List
        [ pfromData (pmintFold'policyCount f) #>= -1
        , pfromData (pmintFold'policyCount f) #<= pmaxTxSizeDerivedItemCount
        , pfromData (pmintFold'policyCursor f) #>= 0
        , pfromData (pmintFold'assetCount f) #>= 0
        , pfromData (pmintFold'assetCount f) #<= LedgerOutput.pmaxDistinctAssetCount
        , pfrontierIsWellFormed
            # pfromData (pmintFold'assetCount f)
            # pfromData (pmintFold'assetPeaks f)
        , pif (pfromData (pmintFold'policyCursor f) #== 0)
            (pfromData (pmintFold'previousPolicy f) #== pconstant "")
            (plengthBS # pfromData (pmintFold'previousPolicy f) #== 28)
        ]
    )
    $ \commonIsValid ->
  pif (stage #< 6)
    (fold #== pemptyMintFoldControl)
    ( pif (stage #== 6)
        ( commonIsValid
            #&& pif (pfromData (pmintFold'policyCount f) #== -1)
              (fold #== pemptyMintFoldControl)
              ( pif (pfromData (pmintFold'activePolicy f) #== pconstant "")
                  ( pfromData (pmintFold'policyCursor f)
                      #<= pfromData (pmintFold'policyCount f)
                      #&& pscriptSourcesMintFoldActiveIsEmpty # fold
                  )
                  ( pand'List
                      [ pfromData (pmintFold'policyCursor f)
                          #< pfromData (pmintFold'policyCount f)
                      , plengthBS # pfromData (pmintFold'activePolicy f) #== 28
                      , pfromData (pmintFold'itemLength f) #> 0
                      , pfromData (pmintFold'itemLength f)
                          #<= pmaxAggregateFieldPreimageBytes
                      , plengthBS # pfromData (pmintFold'itemCommitment f) #== 32
                      , pfromData (pmintFold'itemCursor f) #> 0
                      , pfromData (pmintFold'itemCursor f)
                          #< pfromData (pmintFold'itemLength f)
                      , pfromData (pmintFold'assetsRemaining f) #> 0
                      , pfromData (pmintFold'policyAssetCursor f) #>= 0
                      , pif (pfromData (pmintFold'policyAssetCursor f) #== 0)
                          (pfromData (pmintFold'previousAsset f) #== pconstant "")
                          (plengthBS # pfromData (pmintFold'previousAsset f) #<= 32)
                      , pfromData (pmintFold'assetCount f)
                          + pfromData (pmintFold'assetsRemaining f)
                          #<= LedgerOutput.pmaxDistinctAssetCount
                      ]
                  )
              )
        )
        ( commonIsValid
            #&& pfromData (pmintFold'policyCount f) #>= 0
            #&& pfromData (pmintFold'policyCursor f)
              #== pfromData (pmintFold'policyCount f)
            #&& pscriptSourcesMintFoldActiveIsEmpty # fold
        )
    )

pscriptSourcesStageOneControlIsBound :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PBool
    )
pscriptSourcesStageOneControlIsBound = phoistAcyclic $ plam $
  \pre witness control ->
    pscriptSourcesEarlyControlIsBound # pre # witness # control # 1

pscriptSourcesStageOneCommonControlIsInitial :: forall s.
  Term s (PScriptSourcesControlV1 :--> PBool)
pscriptSourcesStageOneCommonControlIsInitial = phoistAcyclic $ plam $ \control ->
  pmatch control $ \c ->
    pand'List
      [ pfromData (pscriptSources'sourceTotalCount c)
          #== pfromData (pscriptSources'sourceCount c)
      , pfromData (pscriptSources'resolvedItemPeaks c) #== pnil
      , pfromData (pscriptSources'replayCursor c) #== 0
      , pfromData (pscriptSources'spendIndex c) #== 0
      , pfromData (pscriptSources'purposeCount c) #== 0
      , pfromData (pscriptSources'outputCursor c) #== 0
      , pfromData (pscriptSources'outputCount c) #== 0
      , pfromData (pscriptSources'receiveScan c) #== pemptyReceivePurposeScanControl
      ]

pscriptSourcesStageOneBeginSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PInteger :--> PByteString :--> PBool
    )
pscriptSourcesStageOneBeginSuccessorIsExact = phoistAcyclic $ plam $
  \pre witness control redeemerTotalCount pendingCbor ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  plet (pencodeObserverPurposeScanControl # pfromData (pscriptSources'observerScan c)) $
    \encodedObserverScan ->
  plet
    ( pcborInt (pfromData $ pscriptSources'sourceTotalCount c)
        <> pcborInt (pfromData $ pscriptSources'redeemerTotalCount c)
        <> encodedObserverScan
        <> (pencodeMintFoldControl # pfromData (pscriptSources'mintFold c))
        <> (pencodeDefiniteBytes # pfromData (pscriptSources'resolutionScheduleHash c))
    )
    $ \oldSuffix ->
  plet (pfromData $ poneStep'workWitnessCbor stepWitness) $ \workWitnessCbor ->
  plet (plengthBS # workWitnessCbor - plengthBS # oldSuffix) $ \suffixOffset ->
  pif
    ( suffixOffset #>= 2
        #&& psliceBS # suffixOffset # (plengthBS # oldSuffix) # workWitnessCbor
          #== oldSuffix
    )
    ( pscriptSourcesStageZeroSuccessorWorkIsExact
        # pre # witness
        # ( (pencodeDefiniteArrayHeader # 31)
              <> (psliceBS # 2 # (suffixOffset - 2) # workWitnessCbor)
              <> pcborInt (pfromData $ pscriptSources'sourceTotalCount c)
              <> pcborInt redeemerTotalCount
              <> encodedObserverScan
              <> (pencodeMintFoldControl # pfromData (pscriptSources'mintFold c))
              <> (pencodeDefiniteBytes # pfromData (pscriptSources'resolutionScheduleHash c))
              <> (pencodeDefiniteBytes # pendingCbor)
          )
    )
    perror

pverifyScriptSourcesStageOneRedeemerBeginSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PItemProofV1 :--> PBool
    )
pverifyScriptSourcesStageOneRedeemerBeginSemanticsV1 = phoistAcyclic $ plam $
  \pre witness collectionProof ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource witnessSet) ->
  pmatch verifiedSource $ \verified ->
  pmatch witnessSet $ \ws ->
  pmatch collectionProof $ \item ->
  plet
    ( pif (pfromData (pscriptSources'redeemerTotalCount c) #== 0)
        (pfromData $ pitemProof'itemCount item)
        (pfromData $ pscriptSources'redeemerTotalCount c)
    )
    $ \activeTotalCount ->
  plet
    ( RedeemerItemProof.pinitialControlV1
        # RedeemerItemProof.pmodeData
        # pfromData (pitemProof'itemIndex item)
        # activeTotalCount
        # pfromData (pitemProof'itemLength item)
        # pfromData (pitemProof'itemCommitment item)
        # (-1) # (-1)
    )
    $ \itemControl ->
  plet (RedeemerItemProof.phashControlV1 # itemControl) $ \pendingCbor ->
    pand'List
      [ pverified'version verified #== 1
      , pscriptSourcesStageOneControlIsBound # pre # witness # control
      , pscriptSourcesStageOneCommonControlIsInitial # control
      , pfromData (pscriptSources'pendingSourceCbor c) #== pconstant ""
      , activeTotalCount #> 0
      , activeTotalCount #<= pmaxTxSizeDerivedItemCount
      , pfromData (pitemProof'fieldIndex item) #== 8
      , pfromData (pitemProof'itemCount item) #== activeTotalCount
      , pfromData (pitemProof'itemIndex item)
          #== pfromData (pscriptSources'redeemerCount c)
      , pverifyBoundedCollectionItem
          # pfromData (pwitnessSetCompact'redeemerTxWitsHash ws) # collectionProof
      , pscriptSourcesStageOneBeginSuccessorIsExact
          # pre # witness # control # activeTotalCount # pendingCbor
      ]

pscriptSourcesStageOneTerminalSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak) :--> PBool
    )
pscriptSourcesStageOneTerminalSuccessorIsExact = phoistAcyclic $ plam $
  \pre witness control redeemerCount redeemerPeaks ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  plet
    ( (pscriptSourcesStageZeroPrefixBeforeStage # control # 31)
        <> pconstant "\x01"
        <> pcborInt (pfromData $ pscriptSources'sourceCount c)
        <> (pencodeFrontier # pfromData (pscriptSources'sourcePeaks c))
    )
    $ \prefix ->
  plet (plengthBS # prefix) $ \redeemerOffset ->
  plet
    ( pcborInt (pfromData $ pscriptSources'redeemerCount c)
        <> (pencodeFrontier # pfromData (pscriptSources'redeemerPeaks c))
    )
    $ \oldRedeemerFields ->
  plet (redeemerOffset + plengthBS # oldRedeemerFields) $ \afterRedeemerOffset ->
  plet (pencodeDefiniteBytes # pfromData (pscriptSources'pendingSourceCbor c)) $
    \oldPending ->
  plet (pfromData $ poneStep'workWitnessCbor stepWitness) $ \workWitnessCbor ->
  plet (plengthBS # workWitnessCbor - plengthBS # oldPending) $ \pendingOffset ->
  pif
    ( pendingOffset #>= afterRedeemerOffset
        #&& psliceBS # 0 # redeemerOffset # workWitnessCbor #== prefix
        #&& psliceBS # redeemerOffset # (plengthBS # oldRedeemerFields) # workWitnessCbor
          #== oldRedeemerFields
        #&& psliceBS # pendingOffset # (plengthBS # oldPending) # workWitnessCbor
          #== oldPending
    )
    ( pscriptSourcesStageZeroSuccessorWorkIsExact
        # pre # witness
        # ( (pencodeDefiniteArrayHeader # 30)
              <> (psliceBS # 2 # (redeemerOffset - 2) # prefix)
              <> pcborInt redeemerCount
              <> (pencodeFrontier # redeemerPeaks)
              <> ( psliceBS # afterRedeemerOffset
                    # (pendingOffset - afterRedeemerOffset) # workWitnessCbor
                 )
          )
    )
    perror

pverifyScriptSourcesStageOneRedeemerStepSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> RedeemerItemProof.PRedeemerItemProofControlV1
        :--> RedeemerItemProof.PRedeemerItemProofWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageOneRedeemerStepSemanticsV1 = phoistAcyclic $ plam $
  \pre witness itemControl itemWitness ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  pmatch itemControl $ \item ->
  plet
    ( pand'List
        [ pverified'version verified #== 1
        , pscriptSourcesStageOneControlIsBound # pre # witness # control
        , pscriptSourcesStageOneCommonControlIsInitial # control
        , RedeemerItemProof.pcontrolIsWellFormed # itemControl
        , pfromData (RedeemerItemProof.predeemerControl'mode item)
            #== RedeemerItemProof.pmodeData
        , pfromData (RedeemerItemProof.predeemerControl'itemIndex item)
            #== pfromData (pscriptSources'redeemerCount c)
        , pfromData (RedeemerItemProof.predeemerControl'itemCount item)
            #== pfromData (pscriptSources'redeemerTotalCount c)
        , RedeemerItemProof.phashControlV1 # itemControl
            #== pfromData (pscriptSources'pendingSourceCbor c)
        ]
    )
    $ \currentMatches ->
  pmatch (RedeemerItemProof.pstepV1 # itemControl # itemWitness) $ \case
    PNothing -> pconstant False
    PJust result -> pmatch result $ \case
      RedeemerItemProof.PRedeemerItemProofInvalid ->
        currentMatches
          #&& prejectedSuccessorIsExact
            # pre # pfromData (poneStep'claimedSuccessor stepWitness)
            # pconstant "E_INVALID_FIELD_TYPE"
      RedeemerItemProof.PRedeemerItemProofAdvanced nextD ->
        plet (pfromData nextD) $ \next ->
        pmatch next $ \nextFields ->
        pif
          ( pfromData (RedeemerItemProof.predeemerControl'stage nextFields)
              #== RedeemerItemProof.pstageTerminal
          )
          ( pmatch (RedeemerItemProof.pfinalizeV1 # next) $ \case
              PNothing -> pconstant False
              PJust _ ->
                plet (pfromData (pscriptSources'redeemerCount c) + 1) $ \nextCount ->
                plet
                  ( pappendLeaf
                      # pfromData (pscriptSources'redeemerCount c)
                      # pfromData (pscriptSources'redeemerPeaks c)
                      # ( ScriptProof.predeemerItemLeafHash
                            # pfromData (pscriptSources'redeemerCount c)
                            # pfromData (RedeemerItemProof.predeemerControl'itemCommitment nextFields)
                        )
                  )
                  $ \nextPeaks ->
                    currentMatches
                      #&& pscriptSourcesStageOneTerminalSuccessorIsExact
                        # pre # witness # control # nextCount # nextPeaks
          )
          ( currentMatches
              #&& pscriptSourcesStageZeroPendingSuccessorIsExact
                # pre # witness # control # (RedeemerItemProof.phashControlV1 # next)
          )

pverifyScriptSourcesStageOneRedeemerHeaderSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> RedeemerItemProof.PRedeemerItemProofControlV1
        :--> RedeemerItemProof.PRedeemerItemProofWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageOneRedeemerHeaderSemanticsV1 = phoistAcyclic $ plam $
  \pre witness itemControl itemWitness ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  pmatch itemControl $ \item ->
  plet
    ( pand'List
        [ pverified'version verified #== 1
        , pscriptSourcesStageOneControlIsBound # pre # witness # control
        , pscriptSourcesStageOneCommonControlIsInitial # control
        , RedeemerItemProof.pcontrolIsWellFormed # itemControl
        , pfromData (RedeemerItemProof.predeemerControl'mode item)
            #== RedeemerItemProof.pmodeData
        , pfromData (RedeemerItemProof.predeemerControl'stage item)
            #== RedeemerItemProof.pstageHeader
        , pfromData (RedeemerItemProof.predeemerControl'itemIndex item)
            #== pfromData (pscriptSources'redeemerCount c)
        , pfromData (RedeemerItemProof.predeemerControl'itemCount item)
            #== pfromData (pscriptSources'redeemerTotalCount c)
        , RedeemerItemProof.phashControlV1 # itemControl
            #== pfromData (pscriptSources'pendingSourceCbor c)
        ]
    )
    $ \currentMatches ->
  pmatch (RedeemerItemProof.pheaderProofStepV1 # itemControl # itemWitness) $ \case
    PNothing -> pconstant False
    PJust result -> pmatch result $ \case
      RedeemerItemProof.PRedeemerItemProofInvalid ->
        currentMatches
          #&& prejectedSuccessorIsExact
            # pre # pfromData (poneStep'claimedSuccessor stepWitness)
            # pconstant "E_INVALID_FIELD_TYPE"
      RedeemerItemProof.PRedeemerItemProofAdvanced nextD ->
        plet (pfromData nextD) $ \next ->
          currentMatches
            #&& pscriptSourcesStageZeroPendingSuccessorIsExact
              # pre # witness # control # (RedeemerItemProof.phashControlV1 # next)

pverifyScriptSourcesStageOneSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageOneSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource witnessSet) ->
  pmatch verifiedSource $ \verified ->
  pmatch witnessSet $ \ws ->
  plet (pfromData $ pwitnessSetCompact'redeemerTxWitsHash ws) $ \redeemerCommitment ->
  plet (pfromData $ pscriptSources'pendingSourceCbor c) $ \pendingCbor ->
  plet
    ( pverified'version verified #== 1
        #&& pscriptSourcesStageOneControlIsBound # pre # witness # control
    )
    $ \controlIsBound ->
  pif (pnot # controlIsBound) (pconstant False) $
  pif (redeemerCommitment #== NativeField.pemptyFieldCommitment)
    ( pmatch auxiliary $ \case
        PNoAuxiliaryWitness ->
          pverifyScriptSourcesStageOneFinishRawSemanticsV1 # pre # witness
        _ -> pconstant False
    )
    ( pif
        ( pendingCbor #== pconstant ""
            #&& pfromData (pscriptSources'redeemerTotalCount c) #> 0
            #&& pfromData (pscriptSources'redeemerCount c)
              #== pfromData (pscriptSources'redeemerTotalCount c)
        )
        ( pmatch auxiliary $ \case
            PNoAuxiliaryWitness ->
              pverifyScriptSourcesStageOneFinishRawSemanticsV1 # pre # witness
            _ -> pconstant False
        )
        ( pif (pendingCbor #== pconstant "")
            ( pmatch auxiliary $ \case
                PTransactionRedeemerItemBeginWitness collectionProofD ->
                  pverifyScriptSourcesStageOneRedeemerBeginSemanticsV1
                    # pre # witness # pfromData collectionProofD
                _ -> pconstant False
            )
            ( pmatch auxiliary $ \case
                PRedeemerItemStepWitness redeemerControlD itemControlD itemWitnessD ->
                  pif (pfromData redeemerControlD #== pcon PDNothing)
                    ( pverifyScriptSourcesStageOneRedeemerStepSemanticsV1
                        # pre # witness # pfromData itemControlD # pfromData itemWitnessD
                    )
                    (pconstant False)
                _ -> pconstant False
            )
        )
    )

pverifyScriptSourcesStageOneRedeemerSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageOneRedeemerSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource witnessSet) ->
  pmatch verifiedSource $ \verified ->
  pmatch witnessSet $ \ws ->
  plet (pfromData $ pwitnessSetCompact'redeemerTxWitsHash ws) $ \redeemerCommitment ->
  plet (pfromData $ pscriptSources'pendingSourceCbor c) $ \pendingCbor ->
  plet
    ( pfromData (pscriptSources'redeemerTotalCount c) #> 0
        #&& pfromData (pscriptSources'redeemerCount c)
          #== pfromData (pscriptSources'redeemerTotalCount c)
    )
    $ \scanIsComplete ->
  pif
    ( pverified'version verified #== 1
        #&& pscriptSourcesStageOneControlIsBound # pre # witness # control
        #&& pscriptSourcesStageOneCommonControlIsInitial # control
    )
    ( pmatch auxiliary $ \case
        PTransactionRedeemerItemBeginWitness collectionProofD ->
          plet (pfromData collectionProofD) $ \collectionProof ->
          pmatch collectionProof $ \item ->
          plet
            ( pif (pfromData (pscriptSources'redeemerTotalCount c) #== 0)
                (pfromData $ pitemProof'itemCount item)
                (pfromData $ pscriptSources'redeemerTotalCount c)
            )
            $ \activeTotalCount ->
          plet
            ( RedeemerItemProof.pinitialControlV1
                # RedeemerItemProof.pmodeData
                # pfromData (pitemProof'itemIndex item)
                # activeTotalCount
                # pfromData (pitemProof'itemLength item)
                # pfromData (pitemProof'itemCommitment item)
                # (-1) # (-1)
            )
            $ \itemControl ->
          plet (RedeemerItemProof.phashControlV1 # itemControl) $ \nextPendingCbor ->
            pand'List
              [ redeemerCommitment #/= NativeField.pemptyFieldCommitment
              , pnot # scanIsComplete
              , pendingCbor #== pconstant ""
              , activeTotalCount #> 0
              , activeTotalCount #<= pmaxTxSizeDerivedItemCount
              , pfromData (pitemProof'fieldIndex item) #== 8
              , pfromData (pitemProof'itemCount item) #== activeTotalCount
              , pfromData (pitemProof'itemIndex item)
                  #== pfromData (pscriptSources'redeemerCount c)
              , pverifyBoundedCollectionItem # redeemerCommitment # collectionProof
              , pscriptSourcesStageOneBeginSuccessorIsExact
                  # pre # witness # control # activeTotalCount # nextPendingCbor
              ]
        PRedeemerItemStepWitness redeemerControlD itemControlD itemWitnessD ->
          plet (pfromData itemControlD) $ \itemControl ->
          plet (pfromData itemWitnessD) $ \itemWitness ->
          pmatch itemControl $ \item ->
          plet
            ( pand'List
                [ RedeemerItemProof.pcontrolIsWellFormed # itemControl
                , pfromData (RedeemerItemProof.predeemerControl'mode item)
                    #== RedeemerItemProof.pmodeData
                , pfromData (RedeemerItemProof.predeemerControl'itemIndex item)
                    #== pfromData (pscriptSources'redeemerCount c)
                , pfromData (RedeemerItemProof.predeemerControl'itemCount item)
                    #== pfromData (pscriptSources'redeemerTotalCount c)
                , RedeemerItemProof.phashControlV1 # itemControl #== pendingCbor
                ]
            )
            $ \currentMatches ->
          pif
            ( pfromData redeemerControlD #== pcon PDNothing
                #&& redeemerCommitment #/= NativeField.pemptyFieldCommitment
                #&& pendingCbor #/= pconstant ""
            )
            ( pmatch (RedeemerItemProof.pstepV1 # itemControl # itemWitness) $ \case
                PNothing -> pconstant False
                PJust result -> pmatch result $ \case
                  RedeemerItemProof.PRedeemerItemProofInvalid ->
                    currentMatches
                      #&& prejectedSuccessorIsExact
                        # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                        # pconstant "E_INVALID_FIELD_TYPE"
                  RedeemerItemProof.PRedeemerItemProofAdvanced nextD ->
                    plet (pfromData nextD) $ \next ->
                    pmatch next $ \nextFields ->
                    pif
                      ( pfromData (RedeemerItemProof.predeemerControl'stage nextFields)
                          #== RedeemerItemProof.pstageTerminal
                      )
                      ( pmatch (RedeemerItemProof.pfinalizeV1 # next) $ \case
                          PNothing -> pconstant False
                          PJust _ ->
                            plet (pfromData (pscriptSources'redeemerCount c) + 1) $ \nextCount ->
                            plet
                              ( pappendLeaf
                                  # pfromData (pscriptSources'redeemerCount c)
                                  # pfromData (pscriptSources'redeemerPeaks c)
                                  # ( ScriptProof.predeemerItemLeafHash
                                        # pfromData (pscriptSources'redeemerCount c)
                                        # pfromData (RedeemerItemProof.predeemerControl'itemCommitment nextFields)
                                    )
                              )
                              $ \nextPeaks ->
                                currentMatches
                                  #&& pscriptSourcesStageOneTerminalSuccessorIsExact
                                    # pre # witness # control # nextCount # nextPeaks
                      )
                      ( currentMatches
                          #&& pscriptSourcesStageZeroPendingSuccessorIsExact
                            # pre # witness # control # (RedeemerItemProof.phashControlV1 # next)
                      )
            )
            (pconstant False)
        _ -> pconstant False
    )
    (pconstant False)

pscriptSourcesSetStageAndReplayRemaining :: forall s.
  Term s
    ( PScriptSourcesControlV1 :--> PInteger :--> PByteString
        :--> PScriptSourcesControlV1
    )
pscriptSourcesSetStageAndReplayRemaining = phoistAcyclic $ plam $
  \control stage replayRemainingScheduleHash ->
  pmatch control $ \c ->
    pcon $ PScriptSourcesControlV1
      (pscriptSources'compactCbor c)
      (pscriptSources'witnessSetCompactCbor c)
      (pscriptSources'fieldPreimageLengthsCbor c)
      (pscriptSources'contextCbor c)
      (pscriptSources'resolvedInputCount c)
      (pscriptSources'resolvedInputsAccumulator c)
      (pscriptSources'signerCount c)
      (pscriptSources'signerFrontierCommitment c)
      (pscriptSources'resolvedItemPeaks c)
      (pdata stage)
      (pscriptSources'sourceCount c)
      (pscriptSources'sourcePeaks c)
      (pscriptSources'redeemerCount c)
      (pscriptSources'redeemerPeaks c)
      (pscriptSources'replayCursor c)
      (pscriptSources'replayAccumulator c)
      (pdata replayRemainingScheduleHash)
      (pscriptSources'spendIndex c)
      (pscriptSources'purposeCount c)
      (pscriptSources'purposePeaks c)
      (pscriptSources'outputCursor c)
      (pscriptSources'outputCount c)
      (pscriptSources'outputPeaks c)
      (pscriptSources'outputTotalCount c)
      (pscriptSources'receiveScan c)
      (pscriptSources'sourceTotalCount c)
      (pscriptSources'redeemerTotalCount c)
      (pscriptSources'observerScan c)
      (pscriptSources'discovery c)
      (pscriptSources'outputProof c)
      (pscriptSources'pendingSourceCbor c)
      (pscriptSources'mintFold c)
      (pscriptSources'resolutionScheduleHash c)

pverifyScriptSourcesStageTwoSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageTwoSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  plet
    ( pscriptSourcesSetStageAndReplayRemaining
        # control # 3 # pfromData (pscriptSources'resolutionScheduleHash c)
    )
    $ \nextControl ->
  plet (pencodeScriptSourcesBaseControl # nextControl) $ \nextWorkWitnessCbor ->
  plet
    ( pmatch auxiliary $ \case
        PNoAuxiliaryWitness -> pconstant True
        _ -> pconstant False
    )
    $ \hasNoAuxiliary ->
    pand'List
      [ pverified'version verified #== 1
      , pscriptSourcesEarlyControlIsBound # pre # witness # control # 2
      , hasNoAuxiliary
      , pfromData (pscriptSources'resolvedInputCount c) #> 0
      , pfromData (pscriptSources'replayCursor c) #== 0
      , pfromData (pscriptSources'resolvedItemPeaks c) #== pnil
      , pfromData (pscriptSources'replayAccumulator c)
          #== pinitialResolutionAccumulator
      , pfromData (pscriptSources'replayRemainingScheduleHash c)
          #== pemptyResolutionScheduleHash
      , pfromData (pscriptSources'spendIndex c) #== 0
      , pfromData (pscriptSources'purposeCount c) #== 0
      , pfromData (pscriptSources'purposePeaks c) #== pnil
      , pfromData (pscriptSources'resolutionScheduleHash c)
          #/= pemptyResolutionScheduleHash
      , pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness # nextWorkWitnessCbor
      ]

pverifyScriptSourcesStageThreeFinishSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageThreeFinishSemanticsV1 = phoistAcyclic $ plam $
  \pre witness ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  plet
    ( pscriptSourcesSetStageAndReplayRemaining
        # control # 4 # pfromData (pscriptSources'replayRemainingScheduleHash c)
    )
    $ \nextControl ->
  plet (pencodeScriptSourcesBaseControl # nextControl) $ \nextWorkWitnessCbor ->
    pand'List
      [ pscriptSourcesEarlyControlIsBound # pre # witness # control # 3
      , pfromData (pscriptSources'replayRemainingScheduleHash c)
          #== pemptyResolutionScheduleHash
      , pfromData (pscriptSources'replayCursor c)
          #== pfromData (pscriptSources'resolvedInputCount c)
      , pfromData (pscriptSources'replayAccumulator c)
          #== pfromData (pscriptSources'resolvedInputsAccumulator c)
      , pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness # nextWorkWitnessCbor
      ]

pscriptSourcesReplaySourceFrontier :: forall s.
  Term s
    ( PScriptSourcesControlV1 :--> PInteger :--> PByteString
        :--> OutputCommitment.PLedgerOutputCommitmentV1 :--> PBuiltFrontier
    )
pscriptSourcesReplaySourceFrontier = phoistAcyclic $ plam $
  \control sourceKind key descriptor ->
  pmatch control $ \c ->
  pmatch descriptor $ \output ->
  plet (pfromData $ OutputCommitment.poutputCommitment'referenceScriptLanguage output) $
    \language ->
  pif (sourceKind #== 1 #&& language #/= -1)
    ( plet (pfromData (pscriptSources'sourceCount c) + 1) $ \nextCount ->
      plet
        ( pappendLeaf
            # pfromData (pscriptSources'sourceCount c)
            # pfromData (pscriptSources'sourcePeaks c)
            # ( ScriptProof.preferenceSourceLeafHash
                  # key # language
                  # pfromData (OutputCommitment.poutputCommitment'referenceScriptHash output)
                  # pfromData (OutputCommitment.poutputCommitment'referenceScriptTotalLength output)
                  # pfromData (OutputCommitment.poutputCommitment'referenceScriptItemCommitment output)
              )
        )
        $ \nextPeaks -> pcon $ PBuiltFrontier nextCount nextPeaks
    )
    ( pcon $ PBuiltFrontier
        (pfromData $ pscriptSources'sourceCount c)
        (pfromData $ pscriptSources'sourcePeaks c)
    )

pscriptSourcesReplayPurposeFrontier :: forall s.
  Term s
    ( PScriptSourcesControlV1 :--> PInteger :--> PByteString
        :--> OutputCommitment.PLedgerOutputCommitmentV1 :--> PBuiltFrontier
    )
pscriptSourcesReplayPurposeFrontier = phoistAcyclic $ plam $
  \control sourceKind key descriptor ->
  pmatch control $ \c ->
  pmatch descriptor $ \output ->
  pmatch
    ( LedgerOutput.pdecodeCanonicalAddressBytes
        # pfromData (OutputCommitment.poutputCommitment'address output)
    )
    $ \case
      PNothing -> perror
      PJust address ->
        pmatch address $ \a ->
        pif (sourceKind #== 0)
          ( pmatch (pfromData $ paddress'paymentCredential a) $ \case
              PMidgardScriptCredential scriptHashD ->
                plet (pfromData (pscriptSources'purposeCount c) + 1) $ \nextCount ->
                plet
                  ( pappendLeaf
                      # pfromData (pscriptSources'purposeCount c)
                      # pfromData (pscriptSources'purposePeaks c)
                      # ( ScriptProof.ppurposeLeafHash
                            # 0 # pfromData (pscriptSources'spendIndex c)
                            # pfromData scriptHashD # key
                        )
                  )
                  $ \nextPeaks -> pcon $ PBuiltFrontier nextCount nextPeaks
              PMidgardPubKeyCredential _ ->
                pcon $ PBuiltFrontier
                  (pfromData $ pscriptSources'purposeCount c)
                  (pfromData $ pscriptSources'purposePeaks c)
          )
          ( pcon $ PBuiltFrontier
              (pfromData $ pscriptSources'purposeCount c)
              (pfromData $ pscriptSources'purposePeaks c)
          )

pscriptSourcesReplaySuccessor :: forall s.
  Term s
    ( PScriptSourcesControlV1 :--> PInteger :--> PByteString
        :--> PByteString :--> PByteString :--> PBuiltFrontier
        :--> PBuiltFrontier :--> PScriptSourcesControlV1
    )
pscriptSourcesReplaySuccessor = phoistAcyclic $ plam $
  \control sourceKind key nextScheduleHash value nextSources nextPurposes ->
  pmatch control $ \c ->
  pmatch nextSources $ \sources ->
  pmatch nextPurposes $ \purposes ->
  plet (pfromData $ pscriptSources'replayCursor c) $ \replayCursor ->
    pcon $ PScriptSourcesControlV1
      (pscriptSources'compactCbor c)
      (pscriptSources'witnessSetCompactCbor c)
      (pscriptSources'fieldPreimageLengthsCbor c)
      (pscriptSources'contextCbor c)
      (pscriptSources'resolvedInputCount c)
      (pscriptSources'resolvedInputsAccumulator c)
      (pscriptSources'signerCount c)
      (pscriptSources'signerFrontierCommitment c)
      ( pdata $ pappendLeaf
          # replayCursor
          # pfromData (pscriptSources'resolvedItemPeaks c)
          # (ScriptProof.presolvedContextItemLeafHash # sourceKind # replayCursor # key # value)
      )
      (pdata 3)
      (pdata $ pbuiltFrontier'count sources)
      (pdata $ pbuiltFrontier'peaks sources)
      (pscriptSources'redeemerCount c)
      (pscriptSources'redeemerPeaks c)
      (pdata $ replayCursor + 1)
      ( pdata $ presolvedInputAccumulatorSuccessor
          # pfromData (pscriptSources'replayAccumulator c)
          # sourceKind # key # value
      )
      (pdata nextScheduleHash)
      ( pdata $ pfromData (pscriptSources'spendIndex c)
          + pif (sourceKind #== 0) 1 0
      )
      (pdata $ pbuiltFrontier'count purposes)
      (pdata $ pbuiltFrontier'peaks purposes)
      (pscriptSources'outputCursor c)
      (pscriptSources'outputCount c)
      (pscriptSources'outputPeaks c)
      (pscriptSources'outputTotalCount c)
      (pscriptSources'receiveScan c)
      (pdata $ pbuiltFrontier'count sources)
      (pscriptSources'redeemerTotalCount c)
      (pscriptSources'observerScan c)
      (pscriptSources'discovery c)
      (pscriptSources'outputProof c)
      (pscriptSources'pendingSourceCbor c)
      (pscriptSources'mintFold c)
      (pscriptSources'resolutionScheduleHash c)

pscriptSourcesReplayItem :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PInteger :--> PByteString
        :--> PByteString :--> PByteString :--> PBool
    )
pscriptSourcesReplayItem = phoistAcyclic $ plam $
  \pre witness control sourceKind key nextScheduleHash value ->
  pmatch control $ \c ->
  plet (OutputCommitment.pdecodeLedgerOutputCommitment # value) $ \descriptor ->
  pif
    ( presolutionScheduleNodeHash # sourceKind # key # nextScheduleHash
        #== pfromData (pscriptSources'replayRemainingScheduleHash c)
    )
    ( plet
        (pscriptSourcesReplaySourceFrontier # control # sourceKind # key # descriptor)
        $ \nextSources ->
      plet
        (pscriptSourcesReplayPurposeFrontier # control # sourceKind # key # descriptor)
        $ \nextPurposes ->
      plet
        ( pscriptSourcesReplaySuccessor
            # control # sourceKind # key # nextScheduleHash # value
            # nextSources # nextPurposes
        )
        $ \nextControl ->
        pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness # (pencodeScriptSourcesBaseControl # nextControl)
    )
    (pconstant False)

pverifyScriptSourcesStageThreeReplaySemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInteger :--> PByteString :--> PByteString :--> PByteString
        :--> PBool
    )
pverifyScriptSourcesStageThreeReplaySemanticsV1 = phoistAcyclic $ plam $
  \pre witness sourceKind key nextScheduleHash value ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
    pand'List
      [ pscriptSourcesEarlyControlIsBound # pre # witness # control # 3
      , pfromData (pscriptSources'replayRemainingScheduleHash c)
          #/= pemptyResolutionScheduleHash
      , pscriptSourcesReplayItem
          # pre # witness # control # sourceKind # key # nextScheduleHash # value
      ]

pverifyPreparedScriptSourcesStageThreeReplayTransitionV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInteger :--> PByteString :--> PByteString :--> PByteString
        :--> PBool
    )
pverifyPreparedScriptSourcesStageThreeReplayTransitionV1 = phoistAcyclic $ plam $
  \pre witness sourceKind key nextScheduleHash value ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
    pfromData (pscriptSources'stage c) #== 3
      #&& pfromData (pscriptSources'replayRemainingScheduleHash c)
        #/= pemptyResolutionScheduleHash
      #&& pscriptSourcesReplayItem
        # pre # witness # control # sourceKind # key # nextScheduleHash # value

pverifyScriptSourcesStageThreeSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageThreeSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch auxiliary $ \case
    PNoAuxiliaryWitness ->
      pverifyScriptSourcesStageThreeFinishSemanticsV1 # pre # witness
    PResolvedInputReplayWitness sourceKindD keyD nextScheduleHashD valueD ->
      pverifyScriptSourcesStageThreeReplaySemanticsV1
        # pre # witness # pfromData sourceKindD # pfromData keyD
        # pfromData nextScheduleHashD # pfromData valueD
    _ -> pconstant False

pscriptSourcesSetOutputFrontier :: forall s.
  Term s
    ( PScriptSourcesControlV1 :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak) :--> PInteger
        :--> PScriptSourcesControlV1
    )
pscriptSourcesSetOutputFrontier = phoistAcyclic $ plam $
  \control outputCount outputPeaks outputTotalCount ->
  pmatch control $ \c ->
    pcon $ PScriptSourcesControlV1
      (pscriptSources'compactCbor c)
      (pscriptSources'witnessSetCompactCbor c)
      (pscriptSources'fieldPreimageLengthsCbor c)
      (pscriptSources'contextCbor c)
      (pscriptSources'resolvedInputCount c)
      (pscriptSources'resolvedInputsAccumulator c)
      (pscriptSources'signerCount c)
      (pscriptSources'signerFrontierCommitment c)
      (pscriptSources'resolvedItemPeaks c)
      (pscriptSources'stage c)
      (pscriptSources'sourceCount c)
      (pscriptSources'sourcePeaks c)
      (pscriptSources'redeemerCount c)
      (pscriptSources'redeemerPeaks c)
      (pscriptSources'replayCursor c)
      (pscriptSources'replayAccumulator c)
      (pscriptSources'replayRemainingScheduleHash c)
      (pscriptSources'spendIndex c)
      (pscriptSources'purposeCount c)
      (pscriptSources'purposePeaks c)
      (pscriptSources'outputCursor c)
      (pdata outputCount)
      (pdata outputPeaks)
      (pdata outputTotalCount)
      (pscriptSources'receiveScan c)
      (pscriptSources'sourceTotalCount c)
      (pscriptSources'redeemerTotalCount c)
      (pscriptSources'observerScan c)
      (pscriptSources'discovery c)
      (pscriptSources'outputProof c)
      (pscriptSources'pendingSourceCbor c)
      (pscriptSources'mintFold c)
      (pscriptSources'resolutionScheduleHash c)

pverifyScriptSourcesStageFourSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageFourSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  pmatch (pverified'txCompact verified) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
  plet (pbodyCompact'outputsHash body) $ \outputCommitment ->
  plet
    ( pscriptSourcesSetStageAndReplayRemaining
        # control # 5 # pfromData (pscriptSources'replayRemainingScheduleHash c)
    )
    $ \stageFiveControl ->
  plet
    ( pmatch auxiliary $ \case
        PNoAuxiliaryWitness -> pconstant True
        _ -> pconstant False
    )
    $ \hasNoAuxiliary ->
  plet
    ( pscriptSourcesEarlyControlIsBound # pre # witness # control # 4
        #&& pverified'version verified #== 1
    )
    $ \controlIsBound ->
  pif (outputCommitment #== NativeField.pemptyFieldCommitment)
    ( controlIsBound
        #&& hasNoAuxiliary
        #&& pfromData (pscriptSources'outputCursor c) #== 0
        #&& pfromData (pscriptSources'outputCount c) #== 0
        #&& pfromData (pscriptSources'outputTotalCount c) #== 0
        #&& pfromData (pscriptSources'outputPeaks c) #== pnil
        #&& pfromData (pscriptSources'receiveScan c)
          #== pemptyReceivePurposeScanControl
        #&& pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness # (pencodeScriptSourcesBaseControl # stageFiveControl)
    )
    ( pif
        ( pfromData (pscriptSources'outputTotalCount c) #> 0
            #&& pfromData (pscriptSources'outputCount c)
              #== pfromData (pscriptSources'outputTotalCount c)
        )
        ( controlIsBound
            #&& hasNoAuxiliary
            #&& pfromData (pscriptSources'outputCursor c) #== 0
            #&& pfromData (pscriptSources'receiveScan c)
              #== pemptyReceivePurposeScanControl
            #&& pscriptSourcesStageZeroSuccessorWorkIsExact
              # pre # witness # (pencodeScriptSourcesBaseControl # stageFiveControl)
        )
        ( pmatch auxiliary $ \case
            PTransactionRedeemerItemBeginWitness collectionProofD ->
              plet (pfromData collectionProofD) $ \collectionProof ->
              pmatch collectionProof $ \item ->
              plet
                ( pif (pfromData (pscriptSources'outputTotalCount c) #== 0)
                    (pfromData $ pitemProof'itemCount item)
                    (pfromData $ pscriptSources'outputTotalCount c)
                )
                $ \activeTotalCount ->
              plet (pfromData (pscriptSources'outputCount c) + 1) $ \nextCount ->
              plet
                ( pappendLeaf
                    # pfromData (pscriptSources'outputCount c)
                    # pfromData (pscriptSources'outputPeaks c)
                    # ( ScriptProof.poutputItemLeafHash
                          # pfromData (pscriptSources'outputCount c)
                          # pfromData (pitemProof'itemCommitment item)
                      )
                )
                $ \nextPeaks ->
              plet
                ( pscriptSourcesSetOutputFrontier
                    # control # nextCount # nextPeaks # activeTotalCount
                )
                $ \nextControl ->
                pand'List
                  [ controlIsBound
                  , pfromData (pscriptSources'outputCursor c) #== 0
                  , pfromData (pscriptSources'receiveScan c)
                      #== pemptyReceivePurposeScanControl
                  , activeTotalCount #> 0
                  , activeTotalCount #<= pmaxTxSizeDerivedItemCount
                  , pfromData (pitemProof'fieldIndex item) #== 2
                  , pfromData (pitemProof'itemCount item) #== activeTotalCount
                  , pfromData (pitemProof'itemIndex item)
                      #== pfromData (pscriptSources'outputCount c)
                  , pfromData (pitemProof'itemLength item) #<= 16_384
                  , pverifyBoundedCollectionItem # outputCommitment # collectionProof
                  , pscriptSourcesStageZeroSuccessorWorkIsExact
                      # pre # witness # (pencodeScriptSourcesBaseControl # nextControl)
                  ]
            _ -> perror
        )
    )

pverifyScriptSourcesStageFiveFinishSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageFiveFinishSemanticsV1 = phoistAcyclic $ plam $
  \pre witness ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  plet
    ( pscriptSourcesSetStageAndReplayRemaining
        # control # 6 # pfromData (pscriptSources'replayRemainingScheduleHash c)
    )
    $ \nextControl ->
    pand'List
      [ pscriptSourcesEarlyControlIsBound # pre # witness # control # 5
      , pfromData (pscriptSources'outputCursor c)
          #== pfromData (pscriptSources'outputCount c)
      , pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness # (pencodeScriptSourcesBaseControl # nextControl)
      ]

pscriptSourcesSetOutputProof :: forall s.
  Term s
    ( PScriptSourcesControlV1
        :--> PMaybeData LedgerOutputProof.PLedgerOutputProofControlV1
        :--> PScriptSourcesControlV1
    )
pscriptSourcesSetOutputProof = phoistAcyclic $ plam $ \control outputProof ->
  pmatch control $ \c ->
    pcon $ PScriptSourcesControlV1
      (pscriptSources'compactCbor c)
      (pscriptSources'witnessSetCompactCbor c)
      (pscriptSources'fieldPreimageLengthsCbor c)
      (pscriptSources'contextCbor c)
      (pscriptSources'resolvedInputCount c)
      (pscriptSources'resolvedInputsAccumulator c)
      (pscriptSources'signerCount c)
      (pscriptSources'signerFrontierCommitment c)
      (pscriptSources'resolvedItemPeaks c)
      (pscriptSources'stage c)
      (pscriptSources'sourceCount c)
      (pscriptSources'sourcePeaks c)
      (pscriptSources'redeemerCount c)
      (pscriptSources'redeemerPeaks c)
      (pscriptSources'replayCursor c)
      (pscriptSources'replayAccumulator c)
      (pscriptSources'replayRemainingScheduleHash c)
      (pscriptSources'spendIndex c)
      (pscriptSources'purposeCount c)
      (pscriptSources'purposePeaks c)
      (pscriptSources'outputCursor c)
      (pscriptSources'outputCount c)
      (pscriptSources'outputPeaks c)
      (pscriptSources'outputTotalCount c)
      (pscriptSources'receiveScan c)
      (pscriptSources'sourceTotalCount c)
      (pscriptSources'redeemerTotalCount c)
      (pscriptSources'observerScan c)
      (pscriptSources'discovery c)
      (pdata outputProof)
      (pscriptSources'pendingSourceCbor c)
      (pscriptSources'mintFold c)
      (pscriptSources'resolutionScheduleHash c)

pverifyScriptSourcesOutputProofBeginSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInteger :--> PInteger :--> PByteString
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pverifyScriptSourcesOutputProofBeginSemanticsV1 = phoistAcyclic $ plam $
  \pre witness outputIndex totalLength itemCommitment siblings ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  plet
    ( LedgerOutputProof.pinitialControlV1
        # outputIndex # totalLength # itemCommitment
    )
    $ \outputProof ->
  plet
    (pscriptSourcesSetOutputProof # control # pcon (PDJust $ pdata outputProof))
    $ \nextControl ->
    pand'List
      [ pscriptSourcesEarlyControlIsBound # pre # witness # control # 5
      , pfromData (pscriptSources'outputCursor c)
          #< pfromData (pscriptSources'outputCount c)
      , pfromData (pscriptSources'outputProof c) #== pcon PDNothing
      , outputIndex #== pfromData (pscriptSources'outputCursor c)
      , totalLength #> 0
      , plengthBS # itemCommitment #== 32
      , pverifyMembership
          # pfromData (pscriptSources'outputCount c)
          # pfromData (pscriptSources'outputPeaks c)
          # outputIndex
          # (ScriptProof.poutputItemLeafHash # outputIndex # itemCommitment)
          # siblings
      , pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness
          # (pencodeScriptSourcesOutputProofWitness # nextControl # outputProof)
      ]

pverifyScriptSourcesOutputProofStepSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> LedgerOutputProof.PLedgerOutputProofWitnessV1 :--> PBool
    )
pverifyScriptSourcesOutputProofStepSemanticsV1 = phoistAcyclic $ plam $
  \pre witness proofWitness ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'outputProof c) $ \case
    PDNothing -> perror
    PDJust outputProofD ->
      plet (pfromData outputProofD) $ \outputProof ->
      pmatch (LedgerOutputProof.pstepV1 # outputProof # proofWitness) $ \case
        PNothing -> pconstant False
        PJust result ->
          pscriptSourcesOutputProofResult
            # pre # witness # control # outputProof # result

pscriptSourcesOutputProofResult :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1
        :--> LedgerOutputProof.PLedgerOutputProofControlV1
        :--> LedgerOutputProof.PLedgerOutputProofStepResultV1 :--> PBool
    )
pscriptSourcesOutputProofResult = phoistAcyclic $ plam $
  \pre witness control outputProof result ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  plet
    ( pand'List
        [ pscriptSourcesEarlyControlIsBound # pre # witness # control # 5
        , pfromData (pscriptSources'outputCursor c)
            #< pfromData (pscriptSources'outputCount c)
        , pnot # (LedgerOutputProof.pterminalIsExactV1 # outputProof)
        ]
    )
    $ \currentMatches ->
  pmatch result $ \case
    LedgerOutputProof.PLedgerOutputProofAdvanced nextOutputProofD ->
      plet (pfromData nextOutputProofD) $ \nextOutputProof ->
      plet
        ( pscriptSourcesSetOutputProof
            # control # pcon (PDJust $ pdata nextOutputProof)
        )
        $ \nextControl ->
        currentMatches
          #&& pscriptSourcesStageZeroSuccessorWorkIsExact
            # pre # witness
            # ( pencodeScriptSourcesOutputProofWitness
                  # nextControl # nextOutputProof
              )
    LedgerOutputProof.PLedgerOutputProofInvalidOutput ->
      currentMatches
        #&& prejectedSuccessorIsExact
          # pre # pfromData (poneStep'claimedSuccessor stepWitness)
          # pconstant "E_INVALID_OUTPUT"
    LedgerOutputProof.PLedgerOutputProofInvalidReferenceScript ->
      currentMatches
        #&& prejectedSuccessorIsExact
          # pre # pfromData (poneStep'claimedSuccessor stepWitness)
          # pconstant "E_INVALID_FIELD_TYPE"
    LedgerOutputProof.PLedgerOutputProofNativeScriptNodeLimit ->
      currentMatches
        #&& prejectedSuccessorIsExact
          # pre # pfromData (poneStep'claimedSuccessor stepWitness)
          # pconstant "E_NATIVE_SCRIPT_NODE_COUNT"
    LedgerOutputProof.PLedgerOutputProofNativeScriptDepthLimit ->
      currentMatches
        #&& prejectedSuccessorIsExact
          # pre # pfromData (poneStep'claimedSuccessor stepWitness)
          # pconstant "E_NATIVE_SCRIPT_DEPTH"

pprotectedOutputAuthorization :: forall s.
  Term s
    ( PMidgardAddress :--> PInteger :--> PByteString :--> PSignerSetProofV1
        :--> PInputSignerAuthorizationV1
    )
pprotectedOutputAuthorization = phoistAcyclic $ plam $
  \address signerCount signerCommitment signerProof ->
  pmatch address $ \a ->
  pif (pfromData $ paddress'protected a)
    ( ppaymentCredentialSignerAuthorization
        # pfromData (paddress'paymentCredential a)
        # signerCount # signerCommitment # signerProof
    )
    ( pif (signerProof #== pcon PNoSignerSetProof)
        (pcon PInputSignerAuthorized)
        (pcon PInputSignerProofMalformed)
    )

preceiveSourceSuccessor :: forall s.
  Term s
    ( PReceivePurposeScanControlV1 :--> PMidgardAddress
        :--> PReceivePurposeScanControlV1
    )
preceiveSourceSuccessor = phoistAcyclic $ plam $ \scan address ->
  pmatch scan $ \s ->
  pmatch address $ \a ->
  pif (pfromData $ paddress'protected a)
    ( pmatch (pfromData $ paddress'paymentCredential a) $ \case
        PMidgardPubKeyCredential _ -> scan
        PMidgardScriptCredential scriptHashD ->
          pcon $ PReceivePurposeScanControlV1
            (pdata $ pfromData (preceiveScan'sourceCount s) + 1)
            ( pdata $ pappendLeaf
                # pfromData (preceiveScan'sourceCount s)
                # pfromData (preceiveScan'sourcePeaks s)
                # ( ScriptProof.ppurposeLeafHash
                      # 3 # pfromData (preceiveScan'sourceCount s)
                      # pfromData scriptHashD # pfromData scriptHashD
                  )
            )
            (preceiveScan'receiveCount s)
            (preceiveScan'previousHash s)
            (preceiveScan'candidateHash s)
            (preceiveScan'descriptorPeaks s)
    )
    scan

pscriptSourcesAfterOutputFinalize :: forall s.
  Term s
    ( PScriptSourcesControlV1 :--> PByteString :--> PMidgardAddress
        :--> PScriptSourcesControlV1
    )
pscriptSourcesAfterOutputFinalize = phoistAcyclic $ plam $
  \control descriptorCbor address ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'receiveScan c) $ \scan ->
  plet
    ( pcon $ PReceivePurposeScanControlV1
        (preceiveScan'sourceCount scan)
        (preceiveScan'sourcePeaks scan)
        (preceiveScan'receiveCount scan)
        (preceiveScan'previousHash scan)
        (preceiveScan'candidateHash scan)
        ( pdata $ pappendLeaf
            # pfromData (pscriptSources'outputCursor c)
            # pfromData (preceiveScan'descriptorPeaks scan)
            # ( ScriptProof.poutputDescriptorLeafHash
                  # pfromData (pscriptSources'outputCursor c) # descriptorCbor
              )
        )
    )
    $ \scanWithDescriptor ->
  plet (preceiveSourceSuccessor # scanWithDescriptor # address) $ \nextScan ->
    pcon $ PScriptSourcesControlV1
      (pscriptSources'compactCbor c)
      (pscriptSources'witnessSetCompactCbor c)
      (pscriptSources'fieldPreimageLengthsCbor c)
      (pscriptSources'contextCbor c)
      (pscriptSources'resolvedInputCount c)
      (pscriptSources'resolvedInputsAccumulator c)
      (pscriptSources'signerCount c)
      (pscriptSources'signerFrontierCommitment c)
      (pscriptSources'resolvedItemPeaks c)
      (pscriptSources'stage c)
      (pscriptSources'sourceCount c)
      (pscriptSources'sourcePeaks c)
      (pscriptSources'redeemerCount c)
      (pscriptSources'redeemerPeaks c)
      (pscriptSources'replayCursor c)
      (pscriptSources'replayAccumulator c)
      (pscriptSources'replayRemainingScheduleHash c)
      (pscriptSources'spendIndex c)
      (pscriptSources'purposeCount c)
      (pscriptSources'purposePeaks c)
      (pdata $ pfromData (pscriptSources'outputCursor c) + 1)
      (pscriptSources'outputCount c)
      (pscriptSources'outputPeaks c)
      (pscriptSources'outputTotalCount c)
      (pdata nextScan)
      (pscriptSources'sourceTotalCount c)
      (pscriptSources'redeemerTotalCount c)
      (pscriptSources'observerScan c)
      (pscriptSources'discovery c)
      (pdata $ pcon PDNothing)
      (pscriptSources'pendingSourceCbor c)
      (pscriptSources'mintFold c)
      (pscriptSources'resolutionScheduleHash c)

pverifyScriptSourcesOutputProofFinalizeSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PByteString :--> PSignerSetProofV1 :--> PBool
    )
pverifyScriptSourcesOutputProofFinalizeSemanticsV1 = phoistAcyclic $ plam $
  \pre witness descriptorCbor signerProof ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'outputProof c) $ \case
    PDNothing -> perror
    PDJust outputProofD ->
      plet (pfromData outputProofD) $ \outputProof ->
      plet (OutputCommitment.pdecodeLedgerOutputCommitment # descriptorCbor) $ \descriptor ->
      pmatch descriptor $ \output ->
      plet
        ( pand'List
            [ pscriptSourcesEarlyControlIsBound # pre # witness # control # 5
            , pfromData (pscriptSources'outputCursor c)
                #< pfromData (pscriptSources'outputCount c)
            , LedgerOutputProof.pterminalIsExactV1 # outputProof
            ]
        )
        $ \currentMatches ->
      pif
        ( LedgerOutputProof.pdescriptorIsExactV1 # outputProof # descriptor )
        ( pmatch
            ( LedgerOutput.pdecodeCanonicalAddressBytes
                # pfromData (OutputCommitment.poutputCommitment'address output)
            )
            $ \case
              PNothing -> perror
              PJust address ->
                pmatch
                  ( pprotectedOutputAuthorization
                      # address
                      # pfromData (pscriptSources'signerCount c)
                      # pfromData (pscriptSources'signerFrontierCommitment c)
                      # signerProof
                  )
                  $ \case
                    PInputSignerMissing ->
                      currentMatches
                        #&& prejectedSuccessorIsExact
                          # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                          # pconstant "E_MISSING_REQUIRED_WITNESS"
                    PInputSignerProofMalformed -> pconstant False
                    PInputSignerAuthorized ->
                      currentMatches
                        #&& pscriptSourcesStageZeroSuccessorWorkIsExact
                          # pre # witness
                          # ( pencodeScriptSourcesBaseControl
                                # ( pscriptSourcesAfterOutputFinalize
                                      # control # descriptorCbor # address
                                  )
                            )
        )
        (pconstant False)

pscriptSourcesStageFiveBranchV1 :: forall s.
  Term s
    ( PInteger :--> PInteger
        :--> PMaybeData LedgerOutputProof.PLedgerOutputProofControlV1
        :--> PValidationAuxiliaryWitnessV1 :--> PInteger
    )
pscriptSourcesStageFiveBranchV1 = phoistAcyclic $ plam $
  \outputCursor outputCount outputProof auxiliary ->
  pif (outputCursor #== outputCount)
    ( pmatch outputProof $ \case
        PDNothing -> pmatch auxiliary $ \case
          PNoAuxiliaryWitness -> 1
          _ -> 0
        PDJust _ -> 0
    )
    ( pmatch outputProof $ \case
        PDNothing -> pmatch auxiliary $ \case
          PLedgerOutputProofBeginWitness _ _ _ _ -> 2
          _ -> 0
        PDJust outputProofD ->
          pif (LedgerOutputProof.pterminalIsExactV1 # pfromData outputProofD)
            ( pmatch auxiliary $ \case
                PLedgerOutputProofFinalizeWitness _ _ -> 4
                _ -> 0
            )
            ( pmatch auxiliary $ \case
                PLedgerOutputProofStepWitness _ -> 3
                _ -> 0
            )
    )

pverifyScriptSourcesStageFiveSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageFiveSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  plet
    ( pverified'version verified #== 1
        #&& pscriptSourcesEarlyControlIsBound # pre # witness # control # 5
    )
    $ \controlIsBound ->
  plet
    ( pscriptSourcesStageFiveBranchV1
        # pfromData (pscriptSources'outputCursor c)
        # pfromData (pscriptSources'outputCount c)
        # pfromData (pscriptSources'outputProof c) # auxiliary
    )
    $ \branch ->
  pif (branch #== 1)
    ( controlIsBound
        #&& pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness
          # ( pencodeScriptSourcesBaseControl
                # ( pscriptSourcesSetStageAndReplayRemaining
                      # control # 6
                      # pfromData (pscriptSources'replayRemainingScheduleHash c)
                  )
            )
    )
    ( pif (branch #== 2)
        ( controlIsBound
            #&& pmatch auxiliary (\case
              PLedgerOutputProofBeginWitness
                  outputIndexD totalLengthD itemCommitmentD siblingsD ->
                pverifyScriptSourcesOutputProofBeginSemanticsV1
                  # pre # witness
                  # pfromData outputIndexD # pfromData totalLengthD
                  # pfromData itemCommitmentD # pfromData siblingsD
              _ -> pconstant False)
        )
        ( pif (branch #== 3)
            ( controlIsBound
                #&& pmatch auxiliary (\case
                  PLedgerOutputProofStepWitness proofWitnessD ->
                    pverifyScriptSourcesOutputProofStepSemanticsV1
                      # pre # witness # pfromData proofWitnessD
                  _ -> pconstant False)
            )
            ( pif (branch #== 4)
                ( controlIsBound
                    #&& pmatch auxiliary (\case
                      PLedgerOutputProofFinalizeWitness descriptorCborD signerProofD ->
                        pverifyScriptSourcesOutputProofFinalizeSemanticsV1
                          # pre # witness # pfromData descriptorCborD
                          # pfromData signerProofD
                      _ -> pconstant False)
                )
                (pconstant False)
            )
        )
    )

pscriptSourcesSetStageOutputCursorMintFold :: forall s.
  Term s
    ( PScriptSourcesControlV1 :--> PInteger :--> PInteger
        :--> PMintFoldControlV1 :--> PScriptSourcesControlV1
    )
pscriptSourcesSetStageOutputCursorMintFold = phoistAcyclic $ plam $
  \control stage outputCursor mintFold ->
  pmatch control $ \c ->
    pcon $ PScriptSourcesControlV1
      (pscriptSources'compactCbor c)
      (pscriptSources'witnessSetCompactCbor c)
      (pscriptSources'fieldPreimageLengthsCbor c)
      (pscriptSources'contextCbor c)
      (pscriptSources'resolvedInputCount c)
      (pscriptSources'resolvedInputsAccumulator c)
      (pscriptSources'signerCount c)
      (pscriptSources'signerFrontierCommitment c)
      (pscriptSources'resolvedItemPeaks c)
      (pdata stage)
      (pscriptSources'sourceCount c)
      (pscriptSources'sourcePeaks c)
      (pscriptSources'redeemerCount c)
      (pscriptSources'redeemerPeaks c)
      (pscriptSources'replayCursor c)
      (pscriptSources'replayAccumulator c)
      (pscriptSources'replayRemainingScheduleHash c)
      (pscriptSources'spendIndex c)
      (pscriptSources'purposeCount c)
      (pscriptSources'purposePeaks c)
      (pdata outputCursor)
      (pscriptSources'outputCount c)
      (pscriptSources'outputPeaks c)
      (pscriptSources'outputTotalCount c)
      (pscriptSources'receiveScan c)
      (pscriptSources'sourceTotalCount c)
      (pscriptSources'redeemerTotalCount c)
      (pscriptSources'observerScan c)
      (pscriptSources'discovery c)
      (pscriptSources'outputProof c)
      (pscriptSources'pendingSourceCbor c)
      (pdata mintFold)
      (pscriptSources'resolutionScheduleHash c)

pverifyScriptSourcesStageSixEmptySemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageSixEmptySemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  pmatch (pverified'txCompact verified) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
  pmatch (pfromData $ pscriptSources'mintFold c) $ \fold ->
  plet
    ( pcon $ PMintFoldControlV1
        (pdata 0)
        (pmintFold'policyCursor fold)
        (pmintFold'previousPolicy fold)
        (pmintFold'activePolicy fold)
        (pmintFold'itemLength fold)
        (pmintFold'itemCommitment fold)
        (pmintFold'itemCursor fold)
        (pmintFold'assetsRemaining fold)
        (pmintFold'policyAssetCursor fold)
        (pmintFold'previousAsset fold)
        (pmintFold'assetCount fold)
        (pmintFold'assetPeaks fold)
    )
    $ \nextFold ->
    pand'List
      [ pverified'version verified #== 1
      , pscriptSourcesEarlyControlIsBound # pre # witness # control # 6
      , pfromData (pmintFold'policyCount fold) #== -1
      , pbodyCompact'mintHash body #== NativeField.pemptyFieldCommitment
      , pmatch auxiliary $ \case
          PNoAuxiliaryWitness -> pconstant True
          _ -> pconstant False
      , pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness
          # ( pencodeScriptSourcesBaseControl
                # ( pscriptSourcesSetStageOutputCursorMintFold
                      # control # 7 # 0 # nextFold
                  )
            )
      ]

pverifyScriptSourcesStageSixFinishSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageSixFinishSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  plet (pfromData $ pscriptSources'mintFold c) $ \fold ->
  pmatch fold $ \f ->
    pand'List
      [ pverified'version verified #== 1
      , pscriptSourcesEarlyControlIsBound # pre # witness # control # 6
      , pfromData (pmintFold'policyCount f) #>= 0
      , pfromData (pmintFold'policyCursor f)
          #== pfromData (pmintFold'policyCount f)
      , pfromData (pmintFold'activePolicy f) #== pconstant ""
      , pmatch auxiliary $ \case
          PNoAuxiliaryWitness -> pconstant True
          _ -> pconstant False
      , pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness
          # ( pencodeScriptSourcesBaseControl
                # ( pscriptSourcesSetStageOutputCursorMintFold
                      # control # 7 # 0 # fold
                  )
            )
      ]

pdecodeCanonicalBytesAt :: forall s.
  Term s (PByteString :--> PInteger :--> PPair PInteger PByteString)
pdecodeCanonicalBytesAt = phoistAcyclic $ plam $ \bytes offset ->
  pmatch (pdecodeDefiniteBytesAt # bytes # offset) $ \(PPair next value) ->
    pif
      ( psliceBS # offset # (next - offset) # bytes
          #== pencodeDefiniteBytes # value
      )
      (pcon $ PPair next value)
      perror

pdecodeCanonicalMapHeaderAt :: forall s.
  Term s (PByteString :--> PInteger :--> PPair PInteger PInteger)
pdecodeCanonicalMapHeaderAt = phoistAcyclic $ plam $ \bytes offset ->
  pmatch (pdecodeDefiniteMapHeaderAt # bytes # offset) $ \(PPair next count) ->
    pif
      ( psliceBS # offset # (next - offset) # bytes
          #== pencodeDefiniteMapHeader # count
      )
      (pcon $ PPair next count)
      perror

pscriptSourcesCanonicalBytesKeyPrecedes :: forall s.
  Term s (PByteString :--> PByteString :--> PBool)
pscriptSourcesCanonicalBytesKeyPrecedes = phoistAcyclic $ plam $ \left right ->
  plet (plengthBS # left) $ \leftLength ->
  plet (plengthBS # right) $ \rightLength ->
    pif (leftLength #< rightLength) (pconstant True) $
      pif (rightLength #< leftLength) (pconstant False) (left #< right)

pscriptSourcesSetPurposeMintFold :: forall s.
  Term s
    ( PScriptSourcesControlV1 :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PMintFoldControlV1 :--> PScriptSourcesControlV1
    )
pscriptSourcesSetPurposeMintFold = phoistAcyclic $ plam $
  \control purposeCount purposePeaks mintFold ->
  pmatch control $ \c ->
    pcon $ PScriptSourcesControlV1
      (pscriptSources'compactCbor c)
      (pscriptSources'witnessSetCompactCbor c)
      (pscriptSources'fieldPreimageLengthsCbor c)
      (pscriptSources'contextCbor c)
      (pscriptSources'resolvedInputCount c)
      (pscriptSources'resolvedInputsAccumulator c)
      (pscriptSources'signerCount c)
      (pscriptSources'signerFrontierCommitment c)
      (pscriptSources'resolvedItemPeaks c)
      (pscriptSources'stage c)
      (pscriptSources'sourceCount c)
      (pscriptSources'sourcePeaks c)
      (pscriptSources'redeemerCount c)
      (pscriptSources'redeemerPeaks c)
      (pscriptSources'replayCursor c)
      (pscriptSources'replayAccumulator c)
      (pscriptSources'replayRemainingScheduleHash c)
      (pscriptSources'spendIndex c)
      (pdata purposeCount)
      (pdata purposePeaks)
      (pscriptSources'outputCursor c)
      (pscriptSources'outputCount c)
      (pscriptSources'outputPeaks c)
      (pscriptSources'outputTotalCount c)
      (pscriptSources'receiveScan c)
      (pscriptSources'sourceTotalCount c)
      (pscriptSources'redeemerTotalCount c)
      (pscriptSources'observerScan c)
      (pscriptSources'discovery c)
      (pscriptSources'outputProof c)
      (pscriptSources'pendingSourceCbor c)
      (pdata mintFold)
      (pscriptSources'resolutionScheduleHash c)

pverifyScriptSourcesStageSixBeginSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PItemProofV1 :--> BoundedItem.PChunkProofV1 :--> PBool
    )
pverifyScriptSourcesStageSixBeginSemanticsV1 = phoistAcyclic $ plam $
  \pre witness collectionProof chunkProof ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  pmatch (pverified'txCompact verified) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
  pmatch collectionProof $ \item ->
  pmatch chunkProof $ \chunk ->
  pmatch (pfromData $ pscriptSources'mintFold c) $ \fold ->
  plet (pfromData $ BoundedItem.pchunkProof'chunk chunk) $ \chunkBytes ->
  pmatch (pdecodeDefiniteArrayHeaderAt # chunkBytes # 0) $ \(PPair policyOffset itemCount) ->
  pif (itemCount #== 2)
    ( pmatch (pdecodeCanonicalBytesAt # chunkBytes # policyOffset) $ \(PPair assetsHeaderOffset policyId) ->
      pmatch (pdecodeCanonicalMapHeaderAt # chunkBytes # assetsHeaderOffset) $ \(PPair assetsOffset assetCount) ->
      plet
        ( pif (pfromData (pmintFold'policyCount fold) #== -1)
            (pfromData $ pitemProof'itemCount item)
            (pfromData $ pmintFold'policyCount fold)
        )
        $ \activePolicyCount ->
      plet
        ( pand'List
            [ pverified'version verified #== 1
            , pscriptSourcesEarlyControlIsBound # pre # witness # control # 6
            , pfromData (pmintFold'activePolicy fold) #== pconstant ""
            , pfromData (pmintFold'policyCursor fold) #< activePolicyCount
            , pfromData (pitemProof'fieldIndex item) #== 5
            , pfromData (pitemProof'itemCount item) #== activePolicyCount
            , pfromData (pitemProof'itemCount item) #<= pmaxTxSizeDerivedItemCount
            , pfromData (pitemProof'itemIndex item)
                #== pfromData (pmintFold'policyCursor fold)
            , pfromData (pitemProof'itemLength item) #> assetsOffset
            , pfromData (pitemProof'itemLength item) #<= pmaxAggregateFieldPreimageBytes
            , pfromData (BoundedItem.pchunkProof'fieldIndex chunk) #== 5
            , pfromData (BoundedItem.pchunkProof'itemIndex chunk)
                #== pfromData (pmintFold'policyCursor fold)
            , pfromData (BoundedItem.pchunkProof'totalLength chunk)
                #== pfromData (pitemProof'itemLength item)
            , pfromData (BoundedItem.pchunkProof'chunkIndex chunk) #== 0
            , pverifyBoundedCollectionItem # pbodyCompact'mintHash body # collectionProof
            , BoundedItem.pverifyChunk
                # pfromData (pitemProof'itemCommitment item) # chunkProof
            , plengthBS # policyId #== 28
            , assetCount #> 0
            , pfromData (pmintFold'policyCursor fold) #== 0
                #|| pfromData (pmintFold'previousPolicy fold) #< policyId
            ]
        )
        $ \commonIsValid ->
      plet
        ( pcon $ PMintFoldControlV1
            (pdata activePolicyCount)
            (pmintFold'policyCursor fold)
            (pmintFold'previousPolicy fold)
            (pdata policyId)
            (pitemProof'itemLength item)
            (pitemProof'itemCommitment item)
            (pdata assetsOffset)
            (pdata assetCount)
            (pdata 0)
            (pdata $ pconstant "")
            (pmintFold'assetCount fold)
            (pmintFold'assetPeaks fold)
        )
        $ \nextFold ->
      pif
        ( commonIsValid
            #&& pfromData (pmintFold'assetCount fold) + assetCount
              #> LedgerOutput.pmaxDistinctAssetCount
        )
        ( prejectedSuccessorIsExact
            # pre # pfromData (poneStep'claimedSuccessor stepWitness)
            # pconstant "E_ASSET_COUNT"
        )
        ( commonIsValid
            #&& pscriptSourcesStageZeroSuccessorWorkIsExact
              # pre # witness
              # ( pencodeScriptSourcesBaseControl
                    # ( pscriptSourcesSetPurposeMintFold
                          # control
                          # (pfromData (pscriptSources'purposeCount c) + 1)
                          # ( pappendLeaf
                                # pfromData (pscriptSources'purposeCount c)
                                # pfromData (pscriptSources'purposePeaks c)
                                # ( ScriptProof.ppurposeLeafHash
                                      # 1 # pfromData (pmintFold'policyCursor fold)
                                      # policyId # policyId
                                  )
                            )
                          # nextFold
                      )
                )
        )
    )
    perror

pscriptSourcesMintChunkWindow :: forall s.
  Term s
    ( PMintFoldControlV1 :--> BoundedItem.PChunkProofV1
        :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PMaybe PMintChunkWindowV1
    )
pscriptSourcesMintChunkWindow = phoistAcyclic $ plam $
  \fold chunkProof nextChunkProof ->
  pmatch fold $ \f ->
  pmatch chunkProof $ \chunk ->
  plet
    (pdiv # pfromData (pmintFold'itemCursor f) # BoundedItem.pchunkBytes)
    $ \expectedChunkIndex ->
  plet (BoundedItem.pchunkCount # pfromData (pmintFold'itemLength f)) $ \chunkCount ->
  plet
    ( pand'List
        [ pfromData (BoundedItem.pchunkProof'fieldIndex chunk) #== 5
        , pfromData (BoundedItem.pchunkProof'itemIndex chunk)
            #== pfromData (pmintFold'policyCursor f)
        , pfromData (BoundedItem.pchunkProof'totalLength chunk)
            #== pfromData (pmintFold'itemLength f)
        , pfromData (BoundedItem.pchunkProof'chunkIndex chunk) #== expectedChunkIndex
        , BoundedItem.pverifyChunk
            # pfromData (pmintFold'itemCommitment f) # chunkProof
        ]
    )
    $ \currentMatches ->
  pif (pnot # currentMatches)
    (pcon PNothing)
    ( pif (expectedChunkIndex + 1 #< chunkCount)
        ( pmatch nextChunkProof $ \case
            PDNothing -> pcon PNothing
            PDJust nextD ->
              plet (pfromData nextD) $ \next ->
              pmatch next $ \nextChunk ->
              pif
                ( pand'List
                    [ pfromData (BoundedItem.pchunkProof'fieldIndex nextChunk) #== 5
                    , pfromData (BoundedItem.pchunkProof'itemIndex nextChunk)
                        #== pfromData (pmintFold'policyCursor f)
                    , pfromData (BoundedItem.pchunkProof'totalLength nextChunk)
                        #== pfromData (pmintFold'itemLength f)
                    , pfromData (BoundedItem.pchunkProof'chunkIndex nextChunk)
                        #== expectedChunkIndex + 1
                    , BoundedItem.pverifyChunk
                        # pfromData (pmintFold'itemCommitment f) # next
                    ]
                )
                ( pcon $ PJust $ pcon $ PMintChunkWindowV1
                    ( pfromData (BoundedItem.pchunkProof'chunk chunk)
                        <> pfromData (BoundedItem.pchunkProof'chunk nextChunk)
                    )
                    ( pfromData (pmintFold'itemCursor f)
                        - expectedChunkIndex * BoundedItem.pchunkBytes
                    )
                    expectedChunkIndex
                )
                (pcon PNothing)
        )
        ( pif (nextChunkProof #== pcon PDNothing)
            ( pcon $ PJust $ pcon $ PMintChunkWindowV1
                (pfromData $ BoundedItem.pchunkProof'chunk chunk)
                ( pfromData (pmintFold'itemCursor f)
                    - expectedChunkIndex * BoundedItem.pchunkBytes
                )
                expectedChunkIndex
            )
            (pcon PNothing)
        )
    )

pverifyScriptSourcesStageSixAssetSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1
        :--> PMaybeData BoundedItem.PChunkProofV1 :--> PBool
    )
pverifyScriptSourcesStageSixAssetSemanticsV1 = phoistAcyclic $ plam $
  \pre witness chunkProof nextChunkProof ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  plet (pfromData $ pscriptSources'mintFold c) $ \fold ->
  pmatch fold $ \f ->
  pmatch (pscriptSourcesMintChunkWindow # fold # chunkProof # nextChunkProof) $ \case
    PNothing -> perror
    PJust window ->
      pmatch window $ \(PMintChunkWindowV1 bytes offset chunkIndex) ->
      pmatch (pdecodeCanonicalBytesAt # bytes # offset) $ \(PPair quantityOffset assetName) ->
      pmatch (pdecodeCanonicalIntAt # bytes # quantityOffset) $ \(PPair nextOffset quantity) ->
      plet (chunkIndex * BoundedItem.pchunkBytes + nextOffset) $ \nextItemCursor ->
      plet (pfromData (pmintFold'assetCount f) + 1) $ \nextAssetCount ->
      plet
        ( pand'List
            [ pverified'version verified #== 1
            , pscriptSourcesEarlyControlIsBound # pre # witness # control # 6
            , plengthBS # pfromData (pmintFold'activePolicy f) #== 28
            , pfromData (pmintFold'assetsRemaining f) #> 0
            , pfromData (pmintFold'policyAssetCursor f) #>= 0
            , plengthBS # assetName #<= 32
            , quantity #/= 0
            , nextAssetCount #<= LedgerOutput.pmaxDistinctAssetCount
            , pfromData (pmintFold'policyAssetCursor f) #== 0
                #|| pscriptSourcesCanonicalBytesKeyPrecedes
                  # pfromData (pmintFold'previousAsset f) # assetName
            , pif (pfromData (pmintFold'assetsRemaining f) #== 1)
                (nextItemCursor #== pfromData (pmintFold'itemLength f))
                (nextItemCursor #< pfromData (pmintFold'itemLength f))
            ]
        )
        $ \commonIsValid ->
      plet
        ( pappendLeaf
            # pfromData (pmintFold'assetCount f)
            # pfromData (pmintFold'assetPeaks f)
            # ( pmintAssetLeafHash
                  # pfromData (pmintFold'activePolicy f) # assetName # quantity
              )
        )
        $ \nextAssetPeaks ->
      plet
        ( pif (pfromData (pmintFold'assetsRemaining f) #== 1)
            ( pcon $ PMintFoldControlV1
                (pmintFold'policyCount f)
                (pdata $ pfromData (pmintFold'policyCursor f) + 1)
                (pmintFold'activePolicy f)
                (pdata $ pconstant "")
                (pdata 0) (pdata $ pconstant "") (pdata 0) (pdata 0)
                (pdata 0) (pdata $ pconstant "")
                (pdata nextAssetCount) (pdata nextAssetPeaks)
            )
            ( pcon $ PMintFoldControlV1
                (pmintFold'policyCount f)
                (pmintFold'policyCursor f)
                (pmintFold'previousPolicy f)
                (pmintFold'activePolicy f)
                (pmintFold'itemLength f)
                (pmintFold'itemCommitment f)
                (pdata nextItemCursor)
                (pdata $ pfromData (pmintFold'assetsRemaining f) - 1)
                (pdata $ pfromData (pmintFold'policyAssetCursor f) + 1)
                (pdata assetName)
                (pdata nextAssetCount) (pdata nextAssetPeaks)
            )
        )
        $ \nextFold ->
        commonIsValid
          #&& pscriptSourcesStageZeroSuccessorWorkIsExact
            # pre # witness
            # ( pencodeScriptSourcesBaseControl
                  # ( pscriptSourcesSetStageOutputCursorMintFold
                        # control # 6
                        # pfromData (pscriptSources'outputCursor c) # nextFold
                    )
              )

pscriptSourcesStageSixBranchV1 :: forall s.
  Term s
    ( PByteString :--> PMintFoldControlV1
        :--> PValidationAuxiliaryWitnessV1 :--> PInteger
    )
pscriptSourcesStageSixBranchV1 = phoistAcyclic $ plam $
  \expectedCommitment fold auxiliary ->
  pmatch fold $ \f ->
  pif
    ( pfromData (pmintFold'policyCount f) #== -1
        #&& expectedCommitment #== NativeField.pemptyFieldCommitment
    )
    ( pmatch auxiliary $ \case
        PNoAuxiliaryWitness -> 1
        _ -> 0
    )
    ( pif
        ( pfromData (pmintFold'policyCount f) #>= 0
            #&& pfromData (pmintFold'policyCursor f)
              #== pfromData (pmintFold'policyCount f)
            #&& pfromData (pmintFold'activePolicy f) #== pconstant ""
        )
        ( pmatch auxiliary $ \case
            PNoAuxiliaryWitness -> 2
            _ -> 0
        )
        ( pif (pfromData (pmintFold'activePolicy f) #== pconstant "") 3 4 )
    )

pverifyScriptSourcesStageSixSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageSixSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  pmatch (pverified'txCompact verified) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
  plet (pfromData $ pscriptSources'mintFold c) $ \fold ->
  plet
    ( pscriptSourcesStageSixBranchV1
        # pbodyCompact'mintHash body # fold # auxiliary
    )
    $ \branch ->
  plet
    ( pverified'version verified #== 1
        #&& pscriptSourcesEarlyControlIsBound # pre # witness # control # 6
    )
    $ \controlIsBound ->
  pif (pnot # controlIsBound) (pconstant False) $
  pif (branch #== 1)
    (pverifyScriptSourcesStageSixEmptySemanticsV1 # pre # witness # auxiliary)
    ( pif (branch #== 2)
        (pverifyScriptSourcesStageSixFinishSemanticsV1 # pre # witness # auxiliary)
        ( pif (branch #== 3)
            ( pmatch auxiliary $ \case
                PTransactionFieldChunkWitness collectionProofD chunkProofD ->
                  pverifyScriptSourcesStageSixBeginSemanticsV1
                    # pre # witness # pfromData collectionProofD
                    # pfromData chunkProofD
                _ -> perror
            )
            ( pif (branch #== 4)
                ( pmatch auxiliary $ \case
                    PMintFoldAssetWitness chunkProofD nextChunkProofD ->
                      pverifyScriptSourcesStageSixAssetSemanticsV1
                        # pre # witness # pfromData chunkProofD
                        # pfromData nextChunkProofD
                    _ -> perror
                )
                (pconstant False)
            )
        )
    )

pscriptSourcesStageSevenControlIsBound :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PBool
    )
pscriptSourcesStageSevenControlIsBound = phoistAcyclic $ plam $
  \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'receiveScan c) $ \receiveScan ->
  pmatch (pfromData $ pscriptSources'observerScan c) $ \observerScan ->
  pmatch (pfromData $ pscriptSources'mintFold c) $ \mintFold ->
    pand'List
      [ pencodeScriptSourcesBaseControl # control
          #== pfromData (poneStep'workWitnessCbor stepWitness)
      , pfromData (pscriptSources'stage c) #== 7
      , NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (pscriptSources'compactCbor c)
          # pfromData (pscriptSources'witnessSetCompactCbor c)
          # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
          #== pfromData (pmachineState'transactionCommitment preState)
      , phashValidationContext # pfromData (pscriptSources'contextCbor c)
          #== pfromData (pmachineState'validationContextHash preState)
      , pfromData (pscriptSources'sourceTotalCount c)
          #== pfromData (pscriptSources'sourceCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'sourceCount c)
          # pfromData (pscriptSources'sourcePeaks c)
      , pfromData (pscriptSources'redeemerTotalCount c)
          #== pfromData (pscriptSources'redeemerCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'redeemerCount c)
          # pfromData (pscriptSources'redeemerPeaks c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'purposeCount c)
          # pfromData (pscriptSources'purposePeaks c)
      , pfromData (pscriptSources'outputTotalCount c)
          #== pfromData (pscriptSources'outputCount c)
      , pfromData (pscriptSources'outputCount c) #<= pmaxTxSizeDerivedItemCount
      , pfromData (pscriptSources'outputCursor c) #>= 0
      , pfromData (pscriptSources'outputCursor c)
          #<= pfromData (preceiveScan'sourceCount receiveScan)
      , pfromData (preceiveScan'sourceCount receiveScan) #>= 0
      , pfromData (preceiveScan'sourceCount receiveScan)
          #<= pfromData (pscriptSources'outputCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'outputCount c)
          # pfromData (pscriptSources'outputPeaks c)
      , pfrontierIsWellFormed
          # pfromData (preceiveScan'sourceCount receiveScan)
          # pfromData (preceiveScan'sourcePeaks receiveScan)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'outputCount c)
          # pfromData (preceiveScan'descriptorPeaks receiveScan)
      , pfromData (preceiveScan'receiveCount receiveScan) #>= 0
      , pfromData (preceiveScan'receiveCount receiveScan)
          #<= pfromData (preceiveScan'sourceCount receiveScan)
      , pif (pfromData (preceiveScan'receiveCount receiveScan) #== 0)
          (pfromData (preceiveScan'previousHash receiveScan) #== pconstant "")
          (plengthBS # pfromData (preceiveScan'previousHash receiveScan) #== 28)
      , pfromData (preceiveScan'candidateHash receiveScan) #== pconstant ""
          #|| ( plengthBS # pfromData (preceiveScan'candidateHash receiveScan) #== 28
                  #&& ( pfromData (preceiveScan'previousHash receiveScan) #== pconstant ""
                          #|| pfromData (preceiveScan'previousHash receiveScan)
                            #< pfromData (preceiveScan'candidateHash receiveScan)
                      )
              )
      , pfromData (pobserverScan'totalCount observerScan) #>= 0
      , pfromData (pobserverScan'totalCount observerScan) #<= pmaxTxSizeDerivedItemCount
      , pfromData (pobserverScan'seen observerScan) #>= 0
      , pfromData (pobserverScan'seen observerScan)
          #<= pfromData (pobserverScan'totalCount observerScan)
      , pif (pfromData (pobserverScan'seen observerScan) #== 0)
          (pfromData (pobserverScan'previousHash observerScan) #== pconstant "")
          (plengthBS # pfromData (pobserverScan'previousHash observerScan) #== 28)
      , pfromData (pmintFold'policyCount mintFold) #>= 0
      , pfromData (pmintFold'policyCount mintFold) #<= pmaxTxSizeDerivedItemCount
      , pfromData (pmintFold'policyCursor mintFold)
          #== pfromData (pmintFold'policyCount mintFold)
      , pscriptSourcesMintFoldActiveIsEmpty # pcon mintFold
      , pfromData (pmintFold'assetCount mintFold) #>= 0
      , pfromData (pmintFold'assetCount mintFold) #<= LedgerOutput.pmaxDistinctAssetCount
      , pfrontierIsWellFormed
          # pfromData (pmintFold'assetCount mintFold)
          # pfromData (pmintFold'assetPeaks mintFold)
      , pif (pfromData (pmintFold'policyCursor mintFold) #== 0)
          (pfromData (pmintFold'previousPolicy mintFold) #== pconstant "")
          (plengthBS # pfromData (pmintFold'previousPolicy mintFold) #== 28)
      ]

pscriptSourcesStageSevenObserverScanIsComplete :: forall s.
  Term s (PScriptSourcesControlV1 :--> PByteString :--> PBool)
pscriptSourcesStageSevenObserverScanIsComplete = phoistAcyclic $ plam $
  \control observerCommitment ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'observerScan c) $ \scan ->
    pif (observerCommitment #== NativeField.pemptyFieldCommitment)
      (pcon scan #== pemptyObserverPurposeScanControl)
      ( pfromData (pobserverScan'totalCount scan) #> 0
          #&& pfromData (pobserverScan'seen scan)
            #== pfromData (pobserverScan'totalCount scan)
      )

pscriptSourcesStageSevenWithState :: forall s.
  Term s
    ( PScriptSourcesControlV1 :--> PInteger :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak) :--> PInteger
        :--> PReceivePurposeScanControlV1 :--> PObserverPurposeScanControlV1
        :--> PScriptSourcesControlV1
    )
pscriptSourcesStageSevenWithState = phoistAcyclic $ plam $
  \control stage purposeCount purposePeaks outputCursor receiveScan observerScan ->
  pmatch control $ \c ->
    pcon $ PScriptSourcesControlV1
      (pscriptSources'compactCbor c)
      (pscriptSources'witnessSetCompactCbor c)
      (pscriptSources'fieldPreimageLengthsCbor c)
      (pscriptSources'contextCbor c)
      (pscriptSources'resolvedInputCount c)
      (pscriptSources'resolvedInputsAccumulator c)
      (pscriptSources'signerCount c)
      (pscriptSources'signerFrontierCommitment c)
      (pscriptSources'resolvedItemPeaks c)
      (pdata stage)
      (pscriptSources'sourceCount c)
      (pscriptSources'sourcePeaks c)
      (pscriptSources'redeemerCount c)
      (pscriptSources'redeemerPeaks c)
      (pscriptSources'replayCursor c)
      (pscriptSources'replayAccumulator c)
      (pscriptSources'replayRemainingScheduleHash c)
      (pscriptSources'spendIndex c)
      (pdata purposeCount)
      (pdata purposePeaks)
      (pdata outputCursor)
      (pscriptSources'outputCount c)
      (pscriptSources'outputPeaks c)
      (pscriptSources'outputTotalCount c)
      (pdata receiveScan)
      (pscriptSources'sourceTotalCount c)
      (pscriptSources'redeemerTotalCount c)
      (pdata observerScan)
      (pdata pemptyScriptDiscoveryControl)
      (pdata $ pcon PDNothing)
      (pdata $ pconstant "")
      (pscriptSources'mintFold c)
      (pscriptSources'resolutionScheduleHash c)

pverifyScriptSourcesStageSevenObserverSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PItemProofV1 :--> BoundedItem.PChunkProofV1 :--> PBool
    )
pverifyScriptSourcesStageSevenObserverSemanticsV1 = phoistAcyclic $ plam $
  \pre witness collectionProof chunkProof ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'receiveScan c) $ \receiveScan ->
  pmatch (pfromData $ pscriptSources'observerScan c) $ \observerScan ->
  pmatch collectionProof $ \collection ->
  pmatch chunkProof $ \chunk ->
  pmatch (NativeCompact.pdecodeNativeTxCompactV1 # pfromData (pscriptSources'compactCbor c)) $
    \compact ->
  pmatch (pcompact'body compact) $ \body ->
  plet (pbodyCompact'requiredObserversHash body) $ \observerCommitment ->
  plet
    ( pif (pfromData (pobserverScan'totalCount observerScan) #== 0)
        (pfromData $ pitemProof'itemCount collection)
        (pfromData $ pobserverScan'totalCount observerScan)
    )
    $ \activeCount ->
  plet (pfromData $ BoundedItem.pchunkProof'chunk chunk) $ \observerHash ->
  plet
    ( pand'List
        [ pscriptSourcesStageSevenControlIsBound # pre # witness # control
        , observerCommitment #/= NativeField.pemptyFieldCommitment
        , pnot # (pscriptSourcesStageSevenObserverScanIsComplete # control # observerCommitment)
        , pfromData (pscriptSources'outputCursor c) #== 0
        , pfromData (preceiveScan'receiveCount receiveScan) #== 0
        , pfromData (preceiveScan'previousHash receiveScan) #== pconstant ""
        , pfromData (preceiveScan'candidateHash receiveScan) #== pconstant ""
        , activeCount #> 0
        , activeCount #<= pmaxTxSizeDerivedItemCount
        , pfromData (pitemProof'fieldIndex collection) #== 3
        , pfromData (pitemProof'itemCount collection) #== activeCount
        , pfromData (pitemProof'itemIndex collection)
            #== pfromData (pobserverScan'seen observerScan)
        , pfromData (pitemProof'itemLength collection) #== 28
        , pfromData (BoundedItem.pchunkProof'fieldIndex chunk) #== 3
        , pfromData (BoundedItem.pchunkProof'itemIndex chunk)
            #== pfromData (pitemProof'itemIndex collection)
        , pfromData (BoundedItem.pchunkProof'totalLength chunk) #== 28
        , pfromData (BoundedItem.pchunkProof'chunkIndex chunk) #== 0
        , plengthBS # observerHash #== 28
        , BoundedItem.pchunkCount
            # pfromData (BoundedItem.pchunkProof'totalLength chunk) #== 1
        , pverifyBoundedCollectionItem # observerCommitment # collectionProof
        , BoundedItem.pverifyChunk
            # pfromData (pitemProof'itemCommitment collection) # chunkProof
        ]
    )
    $ \commonIsValid ->
  pif
    ( commonIsValid
        #&& pfromData (pobserverScan'seen observerScan) #> 0
        #&& pnot
          # ( pfromData (pobserverScan'previousHash observerScan)
                #< observerHash
            )
    )
    ( prejectedSuccessorIsExact
        # pre # pfromData (poneStep'claimedSuccessor stepWitness)
        # pconstant "E_INVALID_FIELD_TYPE"
    )
    ( plet
        ( pappendLeaf
            # pfromData (pscriptSources'purposeCount c)
            # pfromData (pscriptSources'purposePeaks c)
            # ( ScriptProof.ppurposeLeafHash
                  # 2 # pfromData (pobserverScan'seen observerScan)
                  # observerHash # observerHash
              )
        )
        $ \nextPurposePeaks ->
      plet
        ( pcon $ PObserverPurposeScanControlV1
            (pdata activeCount)
            (pdata $ pfromData (pobserverScan'seen observerScan) + 1)
            (pdata observerHash)
        )
        $ \nextObserverScan ->
        commonIsValid
          #&& pscriptSourcesStageZeroSuccessorWorkIsExact
            # pre # witness
            # ( pencodeScriptSourcesBaseControl
                  # ( pscriptSourcesStageSevenWithState
                        # control # 7
                        # (pfromData (pscriptSources'purposeCount c) + 1)
                        # nextPurposePeaks
                        # pfromData (pscriptSources'outputCursor c)
                        # pfromData (pscriptSources'receiveScan c)
                        # nextObserverScan
                    )
              )
    )

pnextReceiveCandidate :: forall s.
  Term s (PReceivePurposeScanControlV1 :--> PByteString :--> PByteString)
pnextReceiveCandidate = phoistAcyclic $ plam $ \scan scriptHash ->
  pmatch scan $ \s ->
    pif
      ( ( pfromData (preceiveScan'previousHash s) #== pconstant ""
            #|| pfromData (preceiveScan'previousHash s) #< scriptHash
        )
          #&& ( pfromData (preceiveScan'candidateHash s) #== pconstant ""
                  #|| scriptHash #< pfromData (preceiveScan'candidateHash s)
              )
      )
      scriptHash
      (pfromData $ preceiveScan'candidateHash s)

pverifyScriptSourcesStageSevenReceiveSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInteger :--> PInteger :--> PByteString :--> PByteString
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pverifyScriptSourcesStageSevenReceiveSemanticsV1 = phoistAcyclic $ plam $
  \pre witness purposeKind purposeIndex scriptHash subject siblings ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'receiveScan c) $ \scan ->
  pmatch (NativeCompact.pdecodeNativeTxCompactV1 # pfromData (pscriptSources'compactCbor c)) $
    \compact ->
  pmatch (pcompact'body compact) $ \body ->
  plet
    ( ScriptProof.ppurposeLeafHash
        # purposeKind # purposeIndex # scriptHash # subject
    )
    $ \purposeLeaf ->
  plet (pnextReceiveCandidate # pcon scan # scriptHash) $ \candidateHash ->
  plet
    ( pcon $ PReceivePurposeScanControlV1
        (preceiveScan'sourceCount scan)
        (preceiveScan'sourcePeaks scan)
        (preceiveScan'receiveCount scan)
        (preceiveScan'previousHash scan)
        (pdata candidateHash)
        (preceiveScan'descriptorPeaks scan)
    )
    $ \nextScan ->
    pand'List
      [ pscriptSourcesStageSevenControlIsBound # pre # witness # control
      , pscriptSourcesStageSevenObserverScanIsComplete
          # control # pbodyCompact'requiredObserversHash body
      , pfromData (pscriptSources'outputCursor c)
          #< pfromData (preceiveScan'sourceCount scan)
      , purposeKind #== 3
      , purposeIndex #== pfromData (pscriptSources'outputCursor c)
      , plengthBS # scriptHash #== 28
      , subject #== scriptHash
      , pverifyMembership
          # pfromData (preceiveScan'sourceCount scan)
          # pfromData (preceiveScan'sourcePeaks scan)
          # purposeIndex # purposeLeaf # siblings
      , pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness
          # ( pencodeScriptSourcesBaseControl
                # ( pscriptSourcesStageSevenWithState
                      # control # 7
                      # pfromData (pscriptSources'purposeCount c)
                      # pfromData (pscriptSources'purposePeaks c)
                      # (pfromData (pscriptSources'outputCursor c) + 1)
                      # nextScan
                      # pfromData (pscriptSources'observerScan c)
                  )
            )
      ]

pverifyScriptSourcesStageSevenFinishSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool )
pverifyScriptSourcesStageSevenFinishSemanticsV1 = phoistAcyclic $ plam $
  \pre witness ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'receiveScan c) $ \scan ->
  pmatch (NativeCompact.pdecodeNativeTxCompactV1 # pfromData (pscriptSources'compactCbor c)) $
    \compact ->
  pmatch (pcompact'body compact) $ \body ->
  plet
    ( pscriptSourcesStageSevenControlIsBound # pre # witness # control
        #&& pscriptSourcesStageSevenObserverScanIsComplete
          # control # pbodyCompact'requiredObserversHash body
        #&& pfromData (pscriptSources'outputCursor c)
          #== pfromData (preceiveScan'sourceCount scan)
    )
    $ \commonIsValid ->
  pif (pfromData (preceiveScan'candidateHash scan) #== pconstant "")
    ( plet
        ( pcon $ PReceivePurposeScanControlV1
            (pdata 0) (pdata pnil) (pdata 0)
            (pdata $ pconstant "") (pdata $ pconstant "")
            (preceiveScan'descriptorPeaks scan)
        )
        $ \nextScan ->
        commonIsValid
          #&& pscriptSourcesStageZeroSuccessorWorkIsExact
            # pre # witness
            # ( pencodeScriptSourcesDiscoveryWitness
                  # ( pscriptSourcesStageSevenWithState
                        # control # 8
                        # pfromData (pscriptSources'purposeCount c)
                        # pfromData (pscriptSources'purposePeaks c)
                        # pfromData (pscriptSources'outputCount c)
                        # nextScan # pemptyObserverPurposeScanControl
                    )
                  # 8 # pemptyScriptDiscoveryControl
              )
    )
    ( plet (pfromData $ preceiveScan'candidateHash scan) $ \scriptHash ->
      plet
        ( pappendLeaf
            # pfromData (pscriptSources'purposeCount c)
            # pfromData (pscriptSources'purposePeaks c)
            # ( ScriptProof.ppurposeLeafHash
                  # 3 # pfromData (preceiveScan'receiveCount scan)
                  # scriptHash # scriptHash
              )
        )
        $ \nextPurposePeaks ->
      plet
        ( pcon $ PReceivePurposeScanControlV1
            (preceiveScan'sourceCount scan)
            (preceiveScan'sourcePeaks scan)
            (pdata $ pfromData (preceiveScan'receiveCount scan) + 1)
            (pdata scriptHash) (pdata $ pconstant "")
            (preceiveScan'descriptorPeaks scan)
        )
        $ \nextScan ->
        commonIsValid
          #&& pscriptSourcesStageZeroSuccessorWorkIsExact
            # pre # witness
            # ( pencodeScriptSourcesBaseControl
                  # ( pscriptSourcesStageSevenWithState
                        # control # 7
                        # (pfromData (pscriptSources'purposeCount c) + 1)
                        # nextPurposePeaks # 0 # nextScan
                        # pfromData (pscriptSources'observerScan c)
                    )
              )
    )

pscriptSourcesStageSevenBranchV1 :: forall s.
  Term s
    ( PByteString :--> PScriptSourcesControlV1
        :--> PValidationAuxiliaryWitnessV1 :--> PInteger
    )
pscriptSourcesStageSevenBranchV1 = phoistAcyclic $ plam $
  \observerCommitment control auxiliary ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'receiveScan c) $ \scan ->
  pif (pscriptSourcesStageSevenObserverScanIsComplete # control # observerCommitment)
    ( pif
        ( pfromData (pscriptSources'outputCursor c)
            #== pfromData (preceiveScan'sourceCount scan)
        )
        ( pmatch auxiliary $ \case
            PNoAuxiliaryWitness -> 1
            _ -> 0
        )
        ( pmatch auxiliary $ \case
            PScriptPurposeScanWitness {} -> 2
            _ -> 0
        )
    )
    ( pmatch auxiliary $ \case
        PTransactionFieldChunkWitness {} -> 3
        _ -> 0
    )

pverifyScriptSourcesStageSevenSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageSevenSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesStageZeroControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  pmatch (pverified'txCompact verified) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
  plet
    ( pscriptSourcesStageSevenBranchV1
        # pbodyCompact'requiredObserversHash body # control # auxiliary
    )
    $ \branch ->
  plet
    ( pverified'version verified #== 1
        #&& pscriptSourcesStageSevenControlIsBound # pre # witness # control
    )
    $ \controlIsBound ->
  pif (pnot # controlIsBound) (pconstant False) $
  pif (branch #== 1)
    (pverifyScriptSourcesStageSevenFinishSemanticsV1 # pre # witness)
    ( pif (branch #== 2)
        ( pmatch auxiliary $ \case
            PScriptPurposeScanWitness purposeKindD purposeIndexD scriptHashD subjectD siblingsD ->
              pverifyScriptSourcesStageSevenReceiveSemanticsV1
                # pre # witness # pfromData purposeKindD # pfromData purposeIndexD
                # pfromData scriptHashD # pfromData subjectD # pfromData siblingsD
            _ -> perror
        )
        ( pif (branch #== 3)
            ( pmatch auxiliary $ \case
                PTransactionFieldChunkWitness collectionProofD chunkProofD ->
                  pverifyScriptSourcesStageSevenObserverSemanticsV1
                    # pre # witness # pfromData collectionProofD # pfromData chunkProofD
                _ -> perror
            )
            (pconstant False)
        )
    )

pscriptDiscoveryBit :: forall s. Term s (PInteger :--> PInteger)
pscriptDiscoveryBit = phoistAcyclic $ pfix $ \self -> plam $ \index ->
  pif (index #< 0) perror $
  pif (index #== 0) 1 (2 * (self # (index - 1)))

pscriptSourcesStageEightControlFromWitness :: forall s.
  Term s (PByteString :--> PScriptSourcesControlV1)
pscriptSourcesStageEightControlFromWitness = phoistAcyclic $ plam $ \workWitnessCbor ->
  pmatch (pdeserialise # workWitnessCbor) $ \case
    PNothing -> perror
    PJust dat ->
      plet (pasList # dat) $ \items ->
      pif (plength # items #== 31)
        ( plet (pscriptSourcesControlFromDataItems # items # pconstant "") $ \base ->
          pmatch base $ \c ->
          pif (pfromData (pscriptSources'stage c) #== 8)
            ( pcon $ PScriptSourcesControlV1
                (pscriptSources'compactCbor c)
                (pscriptSources'witnessSetCompactCbor c)
                (pscriptSources'fieldPreimageLengthsCbor c)
                (pscriptSources'contextCbor c)
                (pscriptSources'resolvedInputCount c)
                (pscriptSources'resolvedInputsAccumulator c)
                (pscriptSources'signerCount c)
                (pscriptSources'signerFrontierCommitment c)
                (pscriptSources'resolvedItemPeaks c)
                (pscriptSources'stage c)
                (pscriptSources'sourceCount c)
                (pscriptSources'sourcePeaks c)
                (pscriptSources'redeemerCount c)
                (pscriptSources'redeemerPeaks c)
                (pscriptSources'replayCursor c)
                (pscriptSources'replayAccumulator c)
                (pscriptSources'replayRemainingScheduleHash c)
                (pscriptSources'spendIndex c)
                (pscriptSources'purposeCount c)
                (pscriptSources'purposePeaks c)
                (pscriptSources'outputCursor c)
                (pscriptSources'outputCount c)
                (pscriptSources'outputPeaks c)
                (pscriptSources'outputTotalCount c)
                (pscriptSources'receiveScan c)
                (pscriptSources'sourceTotalCount c)
                (pscriptSources'redeemerTotalCount c)
                (pscriptSources'observerScan c)
                ( pdata $ pdecodeScriptDiscoveryControl
                    # (pasByteStr # (pelemAt # 30 # items))
                )
                (pscriptSources'outputProof c)
                (pscriptSources'pendingSourceCbor c)
                (pscriptSources'mintFold c)
                (pscriptSources'resolutionScheduleHash c)
            )
            perror
        )
        perror

pscriptSourcesStageEightControlIsBound :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PBool
    )
pscriptSourcesStageEightControlIsBound = phoistAcyclic $ plam $
  \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
    pand'List
      [ pencodeScriptSourcesDiscoveryWitness # control # 8 # pcon discovery
          #== pfromData (poneStep'workWitnessCbor stepWitness)
      , pfromData (pscriptSources'stage c) #== 8
      , NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (pscriptSources'compactCbor c)
          # pfromData (pscriptSources'witnessSetCompactCbor c)
          # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
          #== pfromData (pmachineState'transactionCommitment preState)
      , phashValidationContext # pfromData (pscriptSources'contextCbor c)
          #== pfromData (pmachineState'validationContextHash preState)
      , pfromData (pscriptSources'sourceTotalCount c)
          #== pfromData (pscriptSources'sourceCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'sourceCount c)
          # pfromData (pscriptSources'sourcePeaks c)
      , pfromData (pscriptSources'redeemerTotalCount c)
          #== pfromData (pscriptSources'redeemerCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'redeemerCount c)
          # pfromData (pscriptSources'redeemerPeaks c)
      , pfromData (pscriptDiscovery'purposeCursor discovery) #>= 0
      , pfromData (pscriptDiscovery'purposeCursor discovery)
          #<= pfromData (pscriptSources'purposeCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'purposeCount c)
          # pfromData (pscriptSources'purposePeaks c)
      , pfromData (pscriptDiscovery'sourceCursor discovery) #== 0
      , pfromData (pscriptDiscovery'redeemerCursor discovery) #== 0
      , pfromData (pscriptDiscovery'executionCount discovery)
          #== pfromData (pscriptDiscovery'purposeCursor discovery)
      , pfrontierIsWellFormed
          # pfromData (pscriptDiscovery'executionCount discovery)
          # pfromData (pscriptDiscovery'executionPeaks discovery)
      , pfromData (pscriptDiscovery'currentPurposeKind discovery) #== -1
      , pfromData (pscriptDiscovery'currentPurposeIndex discovery) #== -1
      , pfromData (pscriptDiscovery'currentScriptHash discovery) #== pconstant ""
      , pfromData (pscriptDiscovery'currentSubject discovery) #== pconstant ""
      , pfromData (pscriptDiscovery'matchedSourceIndex discovery) #== -1
      , pfromData (pscriptDiscovery'matchedLanguageTag discovery) #== -1
      , pfromData (pscriptDiscovery'matchedSourceLeaf discovery) #== pconstant ""
      , pfromData (pscriptDiscovery'usedInlineBitmap discovery) #>= 0
      , pfromData (pscriptDiscovery'usedInlineBitmap discovery)
          #< pscriptDiscoveryBit # pfromData (pscriptSources'sourceCount c)
      , pfromData (pscriptDiscovery'usedRedeemerBitmap discovery) #>= 0
      , pfromData (pscriptDiscovery'usedRedeemerBitmap discovery)
          #< pscriptDiscoveryBit # pfromData (pscriptSources'redeemerCount c)
      ]

pscriptSourcesResetDiscoveryCurrent :: forall s.
  Term s (PScriptDiscoveryControlV1 :--> PScriptDiscoveryControlV1)
pscriptSourcesResetDiscoveryCurrent = phoistAcyclic $ plam $ \control ->
  pmatch control $ \c ->
    pcon $ PScriptDiscoveryControlV1
      (pscriptDiscovery'purposeCursor c)
      (pdata 0) (pdata 0) (pdata $ -1) (pdata $ -1)
      (pdata $ pconstant "") (pdata $ pconstant "")
      (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
      (pscriptDiscovery'usedInlineBitmap c)
      (pscriptDiscovery'usedRedeemerBitmap c)
      (pdata $ pconstant "")
      (pscriptDiscovery'executionCount c)
      (pscriptDiscovery'executionPeaks c)

pverifyScriptSourcesStageEightFinishSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool )
pverifyScriptSourcesStageEightFinishSemanticsV1 = phoistAcyclic $ plam $
  \pre witness ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageEightControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
    pand'List
      [ pscriptSourcesStageEightControlIsBound # pre # witness # control
      , pfromData (pscriptDiscovery'purposeCursor discovery)
          #== pfromData (pscriptSources'purposeCount c)
      , pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness
          # ( pencodeScriptSourcesDiscoveryWitness
                # control # 11
                # (pscriptSourcesResetDiscoveryCurrent # pcon discovery)
            )
      ]

pverifyScriptSourcesStageEightPurposeSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInteger :--> PInteger :--> PByteString :--> PByteString
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pverifyScriptSourcesStageEightPurposeSemanticsV1 = phoistAcyclic $ plam $
  \pre witness purposeKind purposeIndex scriptHash subject siblings ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageEightControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  plet
    ( ScriptProof.ppurposeLeafHash
        # purposeKind # purposeIndex # scriptHash # subject
    )
    $ \purposeLeaf ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pscriptDiscovery'purposeCursor discovery)
        (pdata 0) (pdata 0)
        (pdata purposeKind) (pdata purposeIndex)
        (pdata scriptHash) (pdata subject)
        (pdata $ -1) (pdata $ -1) (pdata $ pconstant "")
        (pscriptDiscovery'usedInlineBitmap discovery)
        (pscriptDiscovery'usedRedeemerBitmap discovery)
        (pscriptDiscovery'redeemerItemControlHash discovery)
        (pscriptDiscovery'executionCount discovery)
        (pscriptDiscovery'executionPeaks discovery)
    )
    $ \nextDiscovery ->
    pand'List
      [ pscriptSourcesStageEightControlIsBound # pre # witness # control
      , pfromData (pscriptDiscovery'purposeCursor discovery)
          #< pfromData (pscriptSources'purposeCount c)
      , purposeKind #>= 0
      , purposeKind #<= 3
      , purposeIndex #>= 0
      , plengthBS # scriptHash #== 28
      , subject #/= pconstant ""
      , pverifyMembership
          # pfromData (pscriptSources'purposeCount c)
          # pfromData (pscriptSources'purposePeaks c)
          # pfromData (pscriptDiscovery'purposeCursor discovery)
          # purposeLeaf # siblings
      , pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness
          # ( pencodeScriptSourcesDiscoveryWitness
                # control # 9 # nextDiscovery
            )
      ]

pscriptSourcesStageEightBranchV1 :: forall s.
  Term s
    ( PScriptSourcesControlV1 :--> PValidationAuxiliaryWitnessV1 :--> PInteger )
pscriptSourcesStageEightBranchV1 = phoistAcyclic $ plam $ \control auxiliary ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  pif
    ( pfromData (pscriptDiscovery'purposeCursor discovery)
        #== pfromData (pscriptSources'purposeCount c)
    )
    ( pmatch auxiliary $ \case
        PNoAuxiliaryWitness -> 1
        _ -> 0
    )
    ( pmatch auxiliary $ \case
        PScriptPurposeScanWitness {} -> 2
        _ -> 0
    )

pverifyScriptSourcesStageEightSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageEightSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageEightControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  plet (pscriptSourcesStageEightBranchV1 # control # auxiliary) $ \branch ->
  plet
    ( pverified'version verified #== 1
        #&& pscriptSourcesStageEightControlIsBound # pre # witness # control
    )
    $ \controlIsBound ->
  pif (pnot # controlIsBound) (pconstant False) $
  pif (branch #== 1)
    (pverifyScriptSourcesStageEightFinishSemanticsV1 # pre # witness)
    ( pif (branch #== 2)
        ( pmatch auxiliary $ \case
            PScriptPurposeScanWitness purposeKindD purposeIndexD scriptHashD subjectD siblingsD ->
              pverifyScriptSourcesStageEightPurposeSemanticsV1
                # pre # witness # pfromData purposeKindD # pfromData purposeIndexD
                # pfromData scriptHashD # pfromData subjectD # pfromData siblingsD
            _ -> perror
        )
        (pconstant False)
    )

pscriptSourcesStageNineControlFromWitness :: forall s.
  Term s (PByteString :--> PScriptSourcesControlV1)
pscriptSourcesStageNineControlFromWitness = phoistAcyclic $ plam $ \workWitnessCbor ->
  pmatch (pdeserialise # workWitnessCbor) $ \case
    PNothing -> perror
    PJust dat ->
      plet (pasList # dat) $ \items ->
      pif (plength # items #== 31)
        ( plet (pscriptSourcesControlFromDataItems # items # pconstant "") $ \base ->
          pmatch base $ \c ->
          pif (pfromData (pscriptSources'stage c) #== 9)
            ( pcon $ PScriptSourcesControlV1
                (pscriptSources'compactCbor c)
                (pscriptSources'witnessSetCompactCbor c)
                (pscriptSources'fieldPreimageLengthsCbor c)
                (pscriptSources'contextCbor c)
                (pscriptSources'resolvedInputCount c)
                (pscriptSources'resolvedInputsAccumulator c)
                (pscriptSources'signerCount c)
                (pscriptSources'signerFrontierCommitment c)
                (pscriptSources'resolvedItemPeaks c)
                (pscriptSources'stage c)
                (pscriptSources'sourceCount c)
                (pscriptSources'sourcePeaks c)
                (pscriptSources'redeemerCount c)
                (pscriptSources'redeemerPeaks c)
                (pscriptSources'replayCursor c)
                (pscriptSources'replayAccumulator c)
                (pscriptSources'replayRemainingScheduleHash c)
                (pscriptSources'spendIndex c)
                (pscriptSources'purposeCount c)
                (pscriptSources'purposePeaks c)
                (pscriptSources'outputCursor c)
                (pscriptSources'outputCount c)
                (pscriptSources'outputPeaks c)
                (pscriptSources'outputTotalCount c)
                (pscriptSources'receiveScan c)
                (pscriptSources'sourceTotalCount c)
                (pscriptSources'redeemerTotalCount c)
                (pscriptSources'observerScan c)
                ( pdata $ pdecodeScriptDiscoveryControl
                    # (pasByteStr # (pelemAt # 30 # items))
                )
                (pscriptSources'outputProof c)
                (pscriptSources'pendingSourceCbor c)
                (pscriptSources'mintFold c)
                (pscriptSources'resolutionScheduleHash c)
            )
            perror
        )
        perror

pscriptSourcesStageNineControlIsBound :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PBool
    )
pscriptSourcesStageNineControlIsBound = phoistAcyclic $ plam $
  \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  plet (pscriptSourcesStageZeroPrefixBeforeStage # control # 31) $ \prefix ->
  plet (plengthBS # prefix) $ \stageOffset ->
  plet
    (pencodeDefiniteBytes # (pencodeScriptDiscoveryControl # pcon discovery))
    $ \encodedDiscovery ->
  plet (pfromData $ poneStep'workWitnessCbor stepWitness) $ \workWitnessCbor ->
  plet (plengthBS # workWitnessCbor - plengthBS # encodedDiscovery) $ \discoveryOffset ->
    pand'List
      [ pfromData (pscriptSources'stage c) #== 9
      , NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (pscriptSources'compactCbor c)
          # pfromData (pscriptSources'witnessSetCompactCbor c)
          # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
          #== pfromData (pmachineState'transactionCommitment preState)
      , phashValidationContext # pfromData (pscriptSources'contextCbor c)
          #== pfromData (pmachineState'validationContextHash preState)
      , pfromData (pscriptSources'sourceTotalCount c)
          #== pfromData (pscriptSources'sourceCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'sourceCount c)
          # pfromData (pscriptSources'sourcePeaks c)
      , pfromData (pscriptSources'redeemerTotalCount c)
          #== pfromData (pscriptSources'redeemerCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'redeemerCount c)
          # pfromData (pscriptSources'redeemerPeaks c)
      , pfromData (pscriptSources'purposeCount c)
          #> pfromData (pscriptDiscovery'purposeCursor discovery)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'purposeCount c)
          # pfromData (pscriptSources'purposePeaks c)
      , pfromData (pscriptDiscovery'sourceCursor discovery)
          #<= pfromData (pscriptSources'sourceCount c)
      , pfromData (pscriptDiscovery'redeemerCursor discovery)
          #<= pfromData (pscriptSources'redeemerCount c)
      , pfromData (pscriptDiscovery'executionCount discovery)
          #== pfromData (pscriptDiscovery'purposeCursor discovery)
      , pfrontierIsWellFormed
          # pfromData (pscriptDiscovery'executionCount discovery)
          # pfromData (pscriptDiscovery'executionPeaks discovery)
      , pfromData (pscriptDiscovery'currentPurposeKind discovery) #>= 0
      , pfromData (pscriptDiscovery'currentPurposeKind discovery) #<= 3
      , pfromData (pscriptDiscovery'currentPurposeIndex discovery) #>= 0
      , plengthBS # pfromData (pscriptDiscovery'currentScriptHash discovery) #== 28
      , pfromData (pscriptDiscovery'currentSubject discovery) #/= pconstant ""
      , pfromData (pscriptDiscovery'matchedSourceIndex discovery) #== -1
      , pfromData (pscriptDiscovery'matchedLanguageTag discovery) #== -1
      , pfromData (pscriptDiscovery'matchedSourceLeaf discovery) #== pconstant ""
      , pfromData (pscriptSources'pendingSourceCbor c) #== pconstant ""
      , pfromData (pscriptSources'outputProof c) #== pcon PDNothing
      , discoveryOffset #>= stageOffset + 1
      , psliceBS # 0 # stageOffset # workWitnessCbor #== prefix
      , psliceBS # stageOffset # 1 # workWitnessCbor #== pconstant "\x09"
      , psliceBS # discoveryOffset # (plengthBS # encodedDiscovery) # workWitnessCbor
          #== encodedDiscovery
      ]

pverifyScriptSourcesStageNineMissingSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool )
pverifyScriptSourcesStageNineMissingSemanticsV1 = phoistAcyclic $ plam $
  \pre witness ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageNineControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
    pscriptSourcesStageNineControlIsBound # pre # witness # control
      #&& pfromData (pscriptDiscovery'sourceCursor discovery)
        #== pfromData (pscriptSources'sourceCount c)
      #&& prejectedSuccessorIsExact
        # pre # pfromData (poneStep'claimedSuccessor stepWitness)
        # pconstant "E_MISSING_REQUIRED_WITNESS"

pscriptSourcesStageNineSourceIsAuthenticated :: forall s.
  Term s
    ( PScriptSourcesControlV1 :--> PInteger :--> PInteger :--> PByteString
        :--> PInteger :--> PByteString :--> PInteger :--> PByteString
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pscriptSourcesStageNineSourceIsAuthenticated = phoistAcyclic $ plam $
  \control sourceIndex originKind sourceKey languageTag scriptHash totalLength itemCommitment siblings ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  plet
    ( ScriptProof.psourceDescriptorLeafHash
        # originKind # sourceKey # languageTag # scriptHash
        # totalLength # itemCommitment
    )
    $ \sourceLeaf ->
    sourceIndex #== pfromData (pscriptDiscovery'sourceCursor discovery)
      #&& pverifyMembership
        # pfromData (pscriptSources'sourceCount c)
        # pfromData (pscriptSources'sourcePeaks c)
        # sourceIndex # sourceLeaf # siblings

pverifyScriptSourcesStageNineMismatchSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInteger :--> PInteger :--> PByteString :--> PInteger
        :--> PByteString :--> PInteger :--> PByteString
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pverifyScriptSourcesStageNineMismatchSemanticsV1 = phoistAcyclic $ plam $
  \pre witness sourceIndex originKind sourceKey languageTag scriptHash totalLength itemCommitment siblings ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageNineControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pscriptDiscovery'purposeCursor discovery)
        (pdata $ sourceIndex + 1)
        (pscriptDiscovery'redeemerCursor discovery)
        (pscriptDiscovery'currentPurposeKind discovery)
        (pscriptDiscovery'currentPurposeIndex discovery)
        (pscriptDiscovery'currentScriptHash discovery)
        (pscriptDiscovery'currentSubject discovery)
        (pscriptDiscovery'matchedSourceIndex discovery)
        (pscriptDiscovery'matchedLanguageTag discovery)
        (pscriptDiscovery'matchedSourceLeaf discovery)
        (pscriptDiscovery'usedInlineBitmap discovery)
        (pscriptDiscovery'usedRedeemerBitmap discovery)
        (pscriptDiscovery'redeemerItemControlHash discovery)
        (pscriptDiscovery'executionCount discovery)
        (pscriptDiscovery'executionPeaks discovery)
    )
    $ \nextDiscovery ->
    pand'List
      [ pscriptSourcesStageNineControlIsBound # pre # witness # control
      , pfromData (pscriptDiscovery'sourceCursor discovery)
          #< pfromData (pscriptSources'sourceCount c)
      , pscriptSourcesStageNineSourceIsAuthenticated
          # control # sourceIndex # originKind # sourceKey # languageTag
          # scriptHash # totalLength # itemCommitment # siblings
      , scriptHash #/= pfromData (pscriptDiscovery'currentScriptHash discovery)
      , pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness
          # (pencodeScriptSourcesDiscoveryWitness # control # 9 # nextDiscovery)
      ]

pscriptDiscoveryBitmapInsert :: forall s.
  Term s (PInteger :--> PInteger :--> PInteger)
pscriptDiscoveryBitmapInsert = phoistAcyclic $ plam $ \bitmap index ->
  plet (pscriptDiscoveryBit # index) $ \bit ->
    pif (pmod # (pdiv # bitmap # bit) # 2 #== 1) bitmap (bitmap + bit)

pscriptSourcesCurrentPurposeLeaf :: forall s.
  Term s (PScriptDiscoveryControlV1 :--> PByteString)
pscriptSourcesCurrentPurposeLeaf = phoistAcyclic $ plam $ \discovery ->
  pmatch discovery $ \d ->
    ScriptProof.ppurposeLeafHash
      # pfromData (pscriptDiscovery'currentPurposeKind d)
      # pfromData (pscriptDiscovery'currentPurposeIndex d)
      # pfromData (pscriptDiscovery'currentScriptHash d)
      # pfromData (pscriptDiscovery'currentSubject d)

pverifyScriptSourcesStageNineNativeMatchSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInteger :--> PInteger :--> PByteString :--> PByteString
        :--> PInteger :--> PByteString
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pverifyScriptSourcesStageNineNativeMatchSemanticsV1 = phoistAcyclic $ plam $
  \pre witness sourceIndex originKind sourceKey scriptHash totalLength itemCommitment siblings ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageNineControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  plet
    ( ScriptProof.psourceDescriptorLeafHash
        # originKind # sourceKey # 0 # scriptHash # totalLength # itemCommitment
    )
    $ \sourceLeaf ->
  plet
    ( pif (originKind #== 0)
        ( pscriptDiscoveryBitmapInsert
            # pfromData (pscriptDiscovery'usedInlineBitmap discovery) # sourceIndex
        )
        (pfromData $ pscriptDiscovery'usedInlineBitmap discovery)
    )
    $ \nextUsedInline ->
  plet
    ( ScriptProof.pexecutionLeafHash
        # 0 # (pscriptSourcesCurrentPurposeLeaf # pcon discovery)
        # sourceLeaf # pconstant ""
    )
    $ \executionLeaf ->
  plet
    ( pappendLeaf
        # pfromData (pscriptDiscovery'executionCount discovery)
        # pfromData (pscriptDiscovery'executionPeaks discovery)
        # executionLeaf
    )
    $ \nextExecutionPeaks ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pdata $ pfromData (pscriptDiscovery'purposeCursor discovery) + 1)
        (pscriptDiscovery'sourceCursor discovery)
        (pscriptDiscovery'redeemerCursor discovery)
        (pscriptDiscovery'currentPurposeKind discovery)
        (pscriptDiscovery'currentPurposeIndex discovery)
        (pscriptDiscovery'currentScriptHash discovery)
        (pscriptDiscovery'currentSubject discovery)
        (pscriptDiscovery'matchedSourceIndex discovery)
        (pscriptDiscovery'matchedLanguageTag discovery)
        (pscriptDiscovery'matchedSourceLeaf discovery)
        (pdata nextUsedInline)
        (pscriptDiscovery'usedRedeemerBitmap discovery)
        (pscriptDiscovery'redeemerItemControlHash discovery)
        (pdata $ pfromData (pscriptDiscovery'executionCount discovery) + 1)
        (pdata nextExecutionPeaks)
    )
    $ \advancedDiscovery ->
  plet (pscriptSourcesResetDiscoveryCurrent # advancedDiscovery) $ \completed ->
    pand'List
      [ pscriptSourcesStageNineControlIsBound # pre # witness # control
      , pfromData (pscriptDiscovery'sourceCursor discovery)
          #< pfromData (pscriptSources'sourceCount c)
      , pscriptSourcesStageNineSourceIsAuthenticated
          # control # sourceIndex # originKind # sourceKey # 0
          # scriptHash # totalLength # itemCommitment # siblings
      , scriptHash #== pfromData (pscriptDiscovery'currentScriptHash discovery)
      , pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness
          # (pencodeScriptSourcesDiscoveryWitness # control # 8 # completed)
      ]

pverifyScriptSourcesStageNineEffectfulMatchSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInteger :--> PInteger :--> PByteString :--> PInteger
        :--> PByteString :--> PInteger :--> PByteString
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pverifyScriptSourcesStageNineEffectfulMatchSemanticsV1 = phoistAcyclic $ plam $
  \pre witness sourceIndex originKind sourceKey languageTag scriptHash totalLength itemCommitment siblings ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageNineControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  plet
    ( ScriptProof.psourceDescriptorLeafHash
        # originKind # sourceKey # languageTag # scriptHash
        # totalLength # itemCommitment
    )
    $ \sourceLeaf ->
  plet
    ( pif (originKind #== 0)
        ( pscriptDiscoveryBitmapInsert
            # pfromData (pscriptDiscovery'usedInlineBitmap discovery) # sourceIndex
        )
        (pfromData $ pscriptDiscovery'usedInlineBitmap discovery)
    )
    $ \nextUsedInline ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pscriptDiscovery'purposeCursor discovery)
        (pdata $ sourceIndex + 1) (pdata 0)
        (pscriptDiscovery'currentPurposeKind discovery)
        (pscriptDiscovery'currentPurposeIndex discovery)
        (pscriptDiscovery'currentScriptHash discovery)
        (pscriptDiscovery'currentSubject discovery)
        (pdata sourceIndex) (pdata languageTag) (pdata sourceLeaf)
        (pdata nextUsedInline)
        (pscriptDiscovery'usedRedeemerBitmap discovery)
        (pscriptDiscovery'redeemerItemControlHash discovery)
        (pscriptDiscovery'executionCount discovery)
        (pscriptDiscovery'executionPeaks discovery)
    )
    $ \nextDiscovery ->
    pand'List
      [ languageTag #== 3 #|| languageTag #== 128
      , pscriptSourcesStageNineControlIsBound # pre # witness # control
      , pfromData (pscriptDiscovery'sourceCursor discovery)
          #< pfromData (pscriptSources'sourceCount c)
      , pscriptSourcesStageNineSourceIsAuthenticated
          # control # sourceIndex # originKind # sourceKey # languageTag
          # scriptHash # totalLength # itemCommitment # siblings
      , scriptHash #== pfromData (pscriptDiscovery'currentScriptHash discovery)
      , pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness
          # (pencodeScriptSourcesDiscoveryWitness # control # 10 # nextDiscovery)
      ]

pscriptSourcesStageNineBranchV1 :: forall s.
  Term s
    ( PScriptSourcesControlV1 :--> PValidationAuxiliaryWitnessV1 :--> PInteger )
pscriptSourcesStageNineBranchV1 = phoistAcyclic $ plam $ \control auxiliary ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  pif
    ( pfromData (pscriptDiscovery'sourceCursor discovery)
        #== pfromData (pscriptSources'sourceCount c)
    )
    ( pmatch auxiliary $ \case
        PNoAuxiliaryWitness -> 1
        _ -> 0
    )
    ( pmatch auxiliary $ \case
        PScriptSourceScanWitness _ _ _ languageTagD scriptHashD _ _ _ ->
          pif
            ( pfromData scriptHashD
                #/= pfromData (pscriptDiscovery'currentScriptHash discovery)
            )
            2
            (pif (pfromData languageTagD #== 0) 3 4)
        _ -> 0
    )

pverifyScriptSourcesStageNineSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageNineSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageNineControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  plet (pscriptSourcesStageNineBranchV1 # control # auxiliary) $ \branch ->
  plet
    ( pverified'version verified #== 1
        #&& pscriptSourcesStageNineControlIsBound # pre # witness # control
    )
    $ \controlIsBound ->
  pif (pnot # controlIsBound) (pconstant False) $
  pif (branch #== 1)
    (pverifyScriptSourcesStageNineMissingSemanticsV1 # pre # witness)
    ( pif (branch #== 2)
        ( pmatch auxiliary $ \case
            PScriptSourceScanWitness sourceIndexD originKindD sourceKeyD languageTagD scriptHashD totalLengthD itemCommitmentD siblingsD ->
              pverifyScriptSourcesStageNineMismatchSemanticsV1
                # pre # witness # pfromData sourceIndexD # pfromData originKindD
                # pfromData sourceKeyD # pfromData languageTagD
                # pfromData scriptHashD # pfromData totalLengthD
                # pfromData itemCommitmentD # pfromData siblingsD
            _ -> perror
        )
        ( pif (branch #== 3)
            ( pmatch auxiliary $ \case
                PScriptSourceScanWitness sourceIndexD originKindD sourceKeyD _ scriptHashD totalLengthD itemCommitmentD siblingsD ->
                  pverifyScriptSourcesStageNineNativeMatchSemanticsV1
                    # pre # witness # pfromData sourceIndexD # pfromData originKindD
                    # pfromData sourceKeyD # pfromData scriptHashD
                    # pfromData totalLengthD # pfromData itemCommitmentD
                    # pfromData siblingsD
                _ -> perror
            )
            ( pif (branch #== 4)
                ( pmatch auxiliary $ \case
                    PScriptSourceScanWitness sourceIndexD originKindD sourceKeyD languageTagD scriptHashD totalLengthD itemCommitmentD siblingsD ->
                      pverifyScriptSourcesStageNineEffectfulMatchSemanticsV1
                        # pre # witness # pfromData sourceIndexD
                        # pfromData originKindD # pfromData sourceKeyD
                        # pfromData languageTagD # pfromData scriptHashD
                        # pfromData totalLengthD # pfromData itemCommitmentD
                        # pfromData siblingsD
                    _ -> perror
                )
                (pconstant False)
            )
        )
    )

pscriptSourcesStageTenControlFromWitness :: forall s.
  Term s (PByteString :--> PScriptSourcesControlV1)
pscriptSourcesStageTenControlFromWitness = phoistAcyclic $ plam $ \workWitnessCbor ->
  pmatch (pdeserialise # workWitnessCbor) $ \case
    PNothing -> perror
    PJust dat ->
      plet (pasList # dat) $ \items ->
      pif (plength # items #== 31)
        ( plet (pscriptSourcesControlFromDataItems # items # pconstant "") $ \base ->
          pmatch base $ \c ->
          pif (pfromData (pscriptSources'stage c) #== 10)
            ( pcon $ PScriptSourcesControlV1
                (pscriptSources'compactCbor c)
                (pscriptSources'witnessSetCompactCbor c)
                (pscriptSources'fieldPreimageLengthsCbor c)
                (pscriptSources'contextCbor c)
                (pscriptSources'resolvedInputCount c)
                (pscriptSources'resolvedInputsAccumulator c)
                (pscriptSources'signerCount c)
                (pscriptSources'signerFrontierCommitment c)
                (pscriptSources'resolvedItemPeaks c)
                (pscriptSources'stage c)
                (pscriptSources'sourceCount c)
                (pscriptSources'sourcePeaks c)
                (pscriptSources'redeemerCount c)
                (pscriptSources'redeemerPeaks c)
                (pscriptSources'replayCursor c)
                (pscriptSources'replayAccumulator c)
                (pscriptSources'replayRemainingScheduleHash c)
                (pscriptSources'spendIndex c)
                (pscriptSources'purposeCount c)
                (pscriptSources'purposePeaks c)
                (pscriptSources'outputCursor c)
                (pscriptSources'outputCount c)
                (pscriptSources'outputPeaks c)
                (pscriptSources'outputTotalCount c)
                (pscriptSources'receiveScan c)
                (pscriptSources'sourceTotalCount c)
                (pscriptSources'redeemerTotalCount c)
                (pscriptSources'observerScan c)
                ( pdata $ pdecodeScriptDiscoveryControl
                    # (pasByteStr # (pelemAt # 30 # items))
                )
                (pscriptSources'outputProof c)
                (pscriptSources'pendingSourceCbor c)
                (pscriptSources'mintFold c)
                (pscriptSources'resolutionScheduleHash c)
            )
            perror
        )
        perror

pscriptSourcesStageTenControlIsBound :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PBool
    )
pscriptSourcesStageTenControlIsBound = phoistAcyclic $ plam $
  \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
    pand'List
      [ pfromData (poneStep'workWitnessCbor stepWitness)
          #== pencodeScriptSourcesDiscoveryWitness # control # 10 # pcon discovery
      , pfromData (pscriptSources'stage c) #== 10
      , NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (pscriptSources'compactCbor c)
          # pfromData (pscriptSources'witnessSetCompactCbor c)
          # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
          #== pfromData (pmachineState'transactionCommitment preState)
      , phashValidationContext # pfromData (pscriptSources'contextCbor c)
          #== pfromData (pmachineState'validationContextHash preState)
      , pfromData (pscriptSources'sourceTotalCount c)
          #== pfromData (pscriptSources'sourceCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'sourceCount c)
          # pfromData (pscriptSources'sourcePeaks c)
      , pfromData (pscriptSources'redeemerTotalCount c)
          #== pfromData (pscriptSources'redeemerCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'redeemerCount c)
          # pfromData (pscriptSources'redeemerPeaks c)
      , pfromData (pscriptDiscovery'purposeCursor discovery)
          #< pfromData (pscriptSources'purposeCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'purposeCount c)
          # pfromData (pscriptSources'purposePeaks c)
      , pfromData (pscriptDiscovery'sourceCursor discovery) #>= 0
      , pfromData (pscriptDiscovery'sourceCursor discovery)
          #<= pfromData (pscriptSources'sourceCount c)
      , pfromData (pscriptDiscovery'redeemerCursor discovery) #>= 0
      , pfromData (pscriptDiscovery'redeemerCursor discovery)
          #<= pfromData (pscriptSources'redeemerCount c)
      , pfromData (pscriptDiscovery'executionCount discovery)
          #== pfromData (pscriptDiscovery'purposeCursor discovery)
      , pfrontierIsWellFormed
          # pfromData (pscriptDiscovery'executionCount discovery)
          # pfromData (pscriptDiscovery'executionPeaks discovery)
      , pfromData (pscriptDiscovery'currentPurposeKind discovery) #>= 0
      , pfromData (pscriptDiscovery'currentPurposeKind discovery) #<= 3
      , pfromData (pscriptDiscovery'currentPurposeIndex discovery) #>= 0
      , plengthBS # pfromData (pscriptDiscovery'currentScriptHash discovery) #== 28
      , pfromData (pscriptDiscovery'currentSubject discovery) #/= pconstant ""
      , pfromData (pscriptDiscovery'matchedSourceIndex discovery) #>= 0
      , pfromData (pscriptDiscovery'matchedSourceIndex discovery)
          #< pfromData (pscriptSources'sourceCount c)
      , pfromData (pscriptDiscovery'matchedLanguageTag discovery) #== 3
          #|| pfromData (pscriptDiscovery'matchedLanguageTag discovery) #== 128
      , plengthBS # pfromData (pscriptDiscovery'matchedSourceLeaf discovery) #== 32
      , pfromData (pscriptDiscovery'usedInlineBitmap discovery) #>= 0
      , pfromData (pscriptDiscovery'usedInlineBitmap discovery)
          #< pscriptDiscoveryBit # pfromData (pscriptSources'sourceCount c)
      , pfromData (pscriptDiscovery'usedRedeemerBitmap discovery) #>= 0
      , pfromData (pscriptDiscovery'usedRedeemerBitmap discovery)
          #< pscriptDiscoveryBit # pfromData (pscriptSources'redeemerCount c)
      ]

pscriptSourcesStageTenDescriptorMatches :: forall s.
  Term s
    ( RedeemerItemProof.PRedeemerItemDescriptorV1
        :--> PScriptDiscoveryControlV1 :--> PBool
    )
pscriptSourcesStageTenDescriptorMatches = phoistAcyclic $ plam $ \descriptor discovery ->
  pmatch descriptor $ \d ->
  pmatch discovery $ \current ->
    predeemerPointerMatchesPurposeV1
      # pfromData (pscriptDiscovery'currentPurposeKind current)
      # pfromData (pscriptDiscovery'currentPurposeIndex current)
      # pfromData (RedeemerItemProof.predeemerDescriptor'purposeTag d)
      # pfromData (RedeemerItemProof.predeemerDescriptor'pointerIndex d)

pscriptSourcesStageTenBegin :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PInteger :--> PInteger
        :--> PInteger :--> PByteString
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pscriptSourcesStageTenBegin = phoistAcyclic $ plam $
  \pre witness control itemIndex itemCount totalLength itemCommitment siblings ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  plet
    ( RedeemerItemProof.pinitialControlV1
        # RedeemerItemProof.pmodeDescriptor # itemIndex # itemCount
        # totalLength # itemCommitment # (-1) # (-1)
    )
    $ \itemControl ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pscriptDiscovery'purposeCursor discovery)
        (pscriptDiscovery'sourceCursor discovery)
        (pscriptDiscovery'redeemerCursor discovery)
        (pscriptDiscovery'currentPurposeKind discovery)
        (pscriptDiscovery'currentPurposeIndex discovery)
        (pscriptDiscovery'currentScriptHash discovery)
        (pscriptDiscovery'currentSubject discovery)
        (pscriptDiscovery'matchedSourceIndex discovery)
        (pscriptDiscovery'matchedLanguageTag discovery)
        (pscriptDiscovery'matchedSourceLeaf discovery)
        (pscriptDiscovery'usedInlineBitmap discovery)
        (pscriptDiscovery'usedRedeemerBitmap discovery)
        (pdata $ RedeemerItemProof.phashControlV1 # itemControl)
        (pscriptDiscovery'executionCount discovery)
        (pscriptDiscovery'executionPeaks discovery)
    )
    $ \nextDiscovery ->
    pand'List
      [ itemIndex #== pfromData (pscriptDiscovery'redeemerCursor discovery)
      , itemCount #== pfromData (pscriptSources'redeemerCount c)
      , pverifyMembership
          # pfromData (pscriptSources'redeemerCount c)
          # pfromData (pscriptSources'redeemerPeaks c)
          # itemIndex
          # (ScriptProof.predeemerItemLeafHash # itemIndex # itemCommitment)
          # siblings
      , pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness
          # (pencodeScriptSourcesDiscoveryWitness # control # 10 # nextDiscovery)
      ]

pscriptSourcesStageTenCurrentMatches :: forall s.
  Term s
    ( PScriptSourcesControlV1 :--> RedeemerItemProof.PRedeemerItemProofControlV1
        :--> PBool
    )
pscriptSourcesStageTenCurrentMatches = phoistAcyclic $ plam $ \control itemControl ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  pmatch itemControl $ \item ->
    pand'List
      [ pfromData (RedeemerItemProof.predeemerControl'mode item)
          #== RedeemerItemProof.pmodeDescriptor
      , pfromData (RedeemerItemProof.predeemerControl'itemIndex item)
          #== pfromData (pscriptDiscovery'redeemerCursor discovery)
      , pfromData (RedeemerItemProof.predeemerControl'itemCount item)
          #== pfromData (pscriptSources'redeemerCount c)
      , RedeemerItemProof.phashControlV1 # pcon item
          #== pfromData (pscriptDiscovery'redeemerItemControlHash discovery)
      ]

pscriptSourcesStageTenItemStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1
        :--> RedeemerItemProof.PRedeemerItemProofControlV1
        :--> RedeemerItemProof.PRedeemerItemProofWitnessV1 :--> PBool
    )
pscriptSourcesStageTenItemStep = phoistAcyclic $ plam $
  \pre witness control itemControl itemWitness ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  plet (pscriptSourcesStageTenCurrentMatches # control # itemControl) $ \currentMatches ->
  pmatch (RedeemerItemProof.pstepV1 # itemControl # itemWitness) $ \case
    PNothing -> pconstant False
    PJust result -> pmatch result $ \case
      RedeemerItemProof.PRedeemerItemProofInvalid -> pconstant False
      RedeemerItemProof.PRedeemerItemProofAdvanced nextD ->
        plet (pfromData nextD) $ \next ->
        pmatch next $ \nextControl ->
        pif
          ( pfromData (RedeemerItemProof.predeemerControl'stage nextControl)
              #== RedeemerItemProof.pstageTerminal
          )
          ( pmatch (RedeemerItemProof.pdescriptorV1 # next) $ \case
              PNothing -> pconstant False
              PJust descriptor ->
                pmatch descriptor $ \d ->
                pif
                  ( pscriptSourcesStageTenDescriptorMatches
                      # pcon d # pcon discovery
                  )
                  ( plet
                      ( ScriptProof.predeemerItemLeafHash
                          # pfromData (RedeemerItemProof.predeemerDescriptor'itemIndex d)
                          # pfromData (RedeemerItemProof.predeemerDescriptor'itemCommitment d)
                      )
                      $ \redeemerLeaf ->
                    plet
                      ( ScriptProof.pexecutionLeafHash
                          # pfromData (pscriptDiscovery'matchedLanguageTag discovery)
                          # (pscriptSourcesCurrentPurposeLeaf # pcon discovery)
                          # pfromData (pscriptDiscovery'matchedSourceLeaf discovery)
                          # redeemerLeaf
                      )
                      $ \executionLeaf ->
                    plet
                      ( pappendLeaf
                          # pfromData (pscriptDiscovery'executionCount discovery)
                          # pfromData (pscriptDiscovery'executionPeaks discovery)
                          # executionLeaf
                      )
                      $ \executionPeaks ->
                    plet
                      ( pcon $ PScriptDiscoveryControlV1
                          (pdata $ pfromData (pscriptDiscovery'purposeCursor discovery) + 1)
                          (pscriptDiscovery'sourceCursor discovery)
                          (pscriptDiscovery'redeemerCursor discovery)
                          (pscriptDiscovery'currentPurposeKind discovery)
                          (pscriptDiscovery'currentPurposeIndex discovery)
                          (pscriptDiscovery'currentScriptHash discovery)
                          (pscriptDiscovery'currentSubject discovery)
                          (pscriptDiscovery'matchedSourceIndex discovery)
                          (pscriptDiscovery'matchedLanguageTag discovery)
                          (pscriptDiscovery'matchedSourceLeaf discovery)
                          (pscriptDiscovery'usedInlineBitmap discovery)
                          ( pdata $ pscriptDiscoveryBitmapInsert
                              # pfromData (pscriptDiscovery'usedRedeemerBitmap discovery)
                              # pfromData (RedeemerItemProof.predeemerDescriptor'itemIndex d)
                          )
                          (pscriptDiscovery'redeemerItemControlHash discovery)
                          (pdata $ pfromData (pscriptDiscovery'executionCount discovery) + 1)
                          (pdata executionPeaks)
                      )
                      $ \advanced ->
                    currentMatches
                      #&& pscriptSourcesStageZeroSuccessorWorkIsExact
                        # pre # witness
                        # ( pencodeScriptSourcesDiscoveryWitness
                              # control # 8
                              # (pscriptSourcesResetDiscoveryCurrent # advanced)
                          )
                  )
                  ( plet
                      ( pcon $ PScriptDiscoveryControlV1
                          (pscriptDiscovery'purposeCursor discovery)
                          (pscriptDiscovery'sourceCursor discovery)
                          (pdata $ pfromData (RedeemerItemProof.predeemerDescriptor'itemIndex d) + 1)
                          (pscriptDiscovery'currentPurposeKind discovery)
                          (pscriptDiscovery'currentPurposeIndex discovery)
                          (pscriptDiscovery'currentScriptHash discovery)
                          (pscriptDiscovery'currentSubject discovery)
                          (pscriptDiscovery'matchedSourceIndex discovery)
                          (pscriptDiscovery'matchedLanguageTag discovery)
                          (pscriptDiscovery'matchedSourceLeaf discovery)
                          (pscriptDiscovery'usedInlineBitmap discovery)
                          (pscriptDiscovery'usedRedeemerBitmap discovery)
                          (pdata $ pconstant "")
                          (pscriptDiscovery'executionCount discovery)
                          (pscriptDiscovery'executionPeaks discovery)
                      )
                      $ \nextDiscovery ->
                    currentMatches
                      #&& pscriptSourcesStageZeroSuccessorWorkIsExact
                        # pre # witness
                        # (pencodeScriptSourcesDiscoveryWitness # control # 10 # nextDiscovery)
                  )
          )
          ( plet
              ( pcon $ PScriptDiscoveryControlV1
                  (pscriptDiscovery'purposeCursor discovery)
                  (pscriptDiscovery'sourceCursor discovery)
                  (pscriptDiscovery'redeemerCursor discovery)
                  (pscriptDiscovery'currentPurposeKind discovery)
                  (pscriptDiscovery'currentPurposeIndex discovery)
                  (pscriptDiscovery'currentScriptHash discovery)
                  (pscriptDiscovery'currentSubject discovery)
                  (pscriptDiscovery'matchedSourceIndex discovery)
                  (pscriptDiscovery'matchedLanguageTag discovery)
                  (pscriptDiscovery'matchedSourceLeaf discovery)
                  (pscriptDiscovery'usedInlineBitmap discovery)
                  (pscriptDiscovery'usedRedeemerBitmap discovery)
                  (pdata $ RedeemerItemProof.phashControlV1 # next)
                  (pscriptDiscovery'executionCount discovery)
                  (pscriptDiscovery'executionPeaks discovery)
              )
              $ \nextDiscovery ->
            currentMatches
              #&& pscriptSourcesStageZeroSuccessorWorkIsExact
                # pre # witness
                # (pencodeScriptSourcesDiscoveryWitness # control # 10 # nextDiscovery)
          )

pverifyScriptSourcesStageTenMissingSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool )
pverifyScriptSourcesStageTenMissingSemanticsV1 = phoistAcyclic $ plam $
  \pre witness ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageTenControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
    pscriptSourcesStageTenControlIsBound # pre # witness # control
      #&& pfromData (pscriptDiscovery'redeemerCursor discovery)
        #== pfromData (pscriptSources'redeemerCount c)
      #&& prejectedSuccessorIsExact
        # pre # pfromData (poneStep'claimedSuccessor stepWitness)
        # pconstant "E_MISSING_REQUIRED_WITNESS"

pverifyScriptSourcesStageTenSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageTenSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageTenControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  plet (pscriptSourcesStageTenControlIsBound # pre # witness # control) $ \controlIsBound ->
  pif (pnot # controlIsBound) (pconstant False) $
  pif
    ( pfromData (pscriptDiscovery'redeemerCursor discovery)
        #== pfromData (pscriptSources'redeemerCount c)
    )
    ( pfromData (pscriptDiscovery'redeemerItemControlHash discovery) #== pconstant ""
        #&& pmatch auxiliary (\case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False)
        #&& pverifyScriptSourcesStageTenMissingSemanticsV1 # pre # witness
    )
    ( pif
        (pfromData (pscriptDiscovery'redeemerItemControlHash discovery) #== pconstant "")
        ( pmatch auxiliary $ \case
            PRedeemerScanBeginWitness itemIndexD itemCountD totalLengthD itemCommitmentD siblingsD ->
              pscriptSourcesStageTenBegin
                # pre # witness # control # pfromData itemIndexD
                # pfromData itemCountD # pfromData totalLengthD
                # pfromData itemCommitmentD # pfromData siblingsD
            _ -> pconstant False
        )
        ( pmatch auxiliary $ \case
            PRedeemerItemStepWitness redeemerControlD itemControlD itemWitnessD ->
              pmatch (pfromData redeemerControlD) $ \case
                PDNothing ->
                  pscriptSourcesStageTenItemStep
                    # pre # witness # control # pfromData itemControlD
                    # pfromData itemWitnessD
                PDJust _ -> pconstant False
            _ -> pconstant False
        )
    )

pscriptSourcesStageTenMatchStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1
        :--> RedeemerItemProof.PRedeemerItemProofControlV1
        :--> RedeemerItemProof.PRedeemerItemProofWitnessV1 :--> PBool
    )
pscriptSourcesStageTenMatchStep = phoistAcyclic $ plam $
  \pre witness control itemControl itemWitness ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  pmatch (RedeemerItemProof.pstepV1 # itemControl # itemWitness) $ \case
    PNothing -> pconstant False
    PJust result -> pmatch result $ \case
      RedeemerItemProof.PRedeemerItemProofInvalid -> pconstant False
      RedeemerItemProof.PRedeemerItemProofAdvanced nextD ->
        plet (pfromData nextD) $ \next ->
        pmatch next $ \n ->
        pif
          ( pfromData (RedeemerItemProof.predeemerControl'stage n)
              #== RedeemerItemProof.pstageTerminal
          )
          ( pmatch (RedeemerItemProof.pdescriptorV1 # next) $ \case
              PNothing -> pconstant False
              PJust descriptor -> pmatch descriptor $ \d ->
                pif
                  (pscriptSourcesStageTenDescriptorMatches # pcon d # pcon discovery)
                  ( plet
                      ( ScriptProof.predeemerItemLeafHash
                          # pfromData (RedeemerItemProof.predeemerDescriptor'itemIndex d)
                          # pfromData (RedeemerItemProof.predeemerDescriptor'itemCommitment d)
                      )
                      $ \redeemerLeaf ->
                    plet
                      ( ScriptProof.pexecutionLeafHash
                          # pfromData (pscriptDiscovery'matchedLanguageTag discovery)
                          # (pscriptSourcesCurrentPurposeLeaf # pcon discovery)
                          # pfromData (pscriptDiscovery'matchedSourceLeaf discovery)
                          # redeemerLeaf
                      )
                      $ \executionLeaf ->
                    plet
                      ( pappendLeaf
                          # pfromData (pscriptDiscovery'executionCount discovery)
                          # pfromData (pscriptDiscovery'executionPeaks discovery)
                          # executionLeaf
                      )
                      $ \executionPeaks ->
                    plet
                      ( pcon $ PScriptDiscoveryControlV1
                          (pdata $ pfromData (pscriptDiscovery'purposeCursor discovery) + 1)
                          (pscriptDiscovery'sourceCursor discovery)
                          (pscriptDiscovery'redeemerCursor discovery)
                          (pscriptDiscovery'currentPurposeKind discovery)
                          (pscriptDiscovery'currentPurposeIndex discovery)
                          (pscriptDiscovery'currentScriptHash discovery)
                          (pscriptDiscovery'currentSubject discovery)
                          (pscriptDiscovery'matchedSourceIndex discovery)
                          (pscriptDiscovery'matchedLanguageTag discovery)
                          (pscriptDiscovery'matchedSourceLeaf discovery)
                          (pscriptDiscovery'usedInlineBitmap discovery)
                          ( pdata $ pscriptDiscoveryBitmapInsert
                              # pfromData (pscriptDiscovery'usedRedeemerBitmap discovery)
                              # pfromData (RedeemerItemProof.predeemerDescriptor'itemIndex d)
                          )
                          (pscriptDiscovery'redeemerItemControlHash discovery)
                          (pdata $ pfromData (pscriptDiscovery'executionCount discovery) + 1)
                          (pdata executionPeaks)
                      )
                      $ \advanced ->
                    pscriptSourcesStageTenCurrentMatches # control # itemControl
                      #&& pscriptSourcesStageZeroSuccessorWorkIsExact
                        # pre # witness
                        # ( pencodeScriptSourcesDiscoveryWitness
                              # control # 8
                              # (pscriptSourcesResetDiscoveryCurrent # advanced)
                          )
                  )
                  (pconstant False)
          )
          (pconstant False)

pscriptSourcesStageTenMismatchStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1
        :--> RedeemerItemProof.PRedeemerItemProofControlV1
        :--> RedeemerItemProof.PRedeemerItemProofWitnessV1 :--> PBool
    )
pscriptSourcesStageTenMismatchStep = phoistAcyclic $ plam $
  \pre witness control itemControl itemWitness ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  pmatch (RedeemerItemProof.pstepV1 # itemControl # itemWitness) $ \case
    PNothing -> pconstant False
    PJust result -> pmatch result $ \case
      RedeemerItemProof.PRedeemerItemProofInvalid -> pconstant False
      RedeemerItemProof.PRedeemerItemProofAdvanced nextD ->
        plet (pfromData nextD) $ \next ->
        pmatch next $ \n ->
        pif
          ( pfromData (RedeemerItemProof.predeemerControl'stage n)
              #== RedeemerItemProof.pstageTerminal
          )
          ( pmatch (RedeemerItemProof.pdescriptorV1 # next) $ \case
              PNothing -> pconstant False
              PJust descriptor -> pmatch descriptor $ \d ->
                pif
                  (pscriptSourcesStageTenDescriptorMatches # pcon d # pcon discovery)
                  (pconstant False)
                  ( plet
                      ( pcon $ PScriptDiscoveryControlV1
                          (pscriptDiscovery'purposeCursor discovery)
                          (pscriptDiscovery'sourceCursor discovery)
                          (pdata $ pfromData (RedeemerItemProof.predeemerDescriptor'itemIndex d) + 1)
                          (pscriptDiscovery'currentPurposeKind discovery)
                          (pscriptDiscovery'currentPurposeIndex discovery)
                          (pscriptDiscovery'currentScriptHash discovery)
                          (pscriptDiscovery'currentSubject discovery)
                          (pscriptDiscovery'matchedSourceIndex discovery)
                          (pscriptDiscovery'matchedLanguageTag discovery)
                          (pscriptDiscovery'matchedSourceLeaf discovery)
                          (pscriptDiscovery'usedInlineBitmap discovery)
                          (pscriptDiscovery'usedRedeemerBitmap discovery)
                          (pdata $ pconstant "")
                          (pscriptDiscovery'executionCount discovery)
                          (pscriptDiscovery'executionPeaks discovery)
                      )
                      $ \nextDiscovery ->
                    pscriptSourcesStageTenCurrentMatches # control # itemControl
                      #&& pscriptSourcesStageZeroSuccessorWorkIsExact
                        # pre # witness
                        # (pencodeScriptSourcesDiscoveryWitness # control # 10 # nextDiscovery)
                  )
          )
          ( plet
              ( pcon $ PScriptDiscoveryControlV1
                  (pscriptDiscovery'purposeCursor discovery)
                  (pscriptDiscovery'sourceCursor discovery)
                  (pscriptDiscovery'redeemerCursor discovery)
                  (pscriptDiscovery'currentPurposeKind discovery)
                  (pscriptDiscovery'currentPurposeIndex discovery)
                  (pscriptDiscovery'currentScriptHash discovery)
                  (pscriptDiscovery'currentSubject discovery)
                  (pscriptDiscovery'matchedSourceIndex discovery)
                  (pscriptDiscovery'matchedLanguageTag discovery)
                  (pscriptDiscovery'matchedSourceLeaf discovery)
                  (pscriptDiscovery'usedInlineBitmap discovery)
                  (pscriptDiscovery'usedRedeemerBitmap discovery)
                  (pdata $ RedeemerItemProof.phashControlV1 # next)
                  (pscriptDiscovery'executionCount discovery)
                  (pscriptDiscovery'executionPeaks discovery)
              )
              $ \nextDiscovery ->
            pscriptSourcesStageTenCurrentMatches # control # itemControl
              #&& pscriptSourcesStageZeroSuccessorWorkIsExact
                # pre # witness
                # (pencodeScriptSourcesDiscoveryWitness # control # 10 # nextDiscovery)
          )

pverifyPreparedScriptSourcesStageTenBeginTransitionV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInteger :--> PInteger :--> PInteger :--> PByteString
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pverifyPreparedScriptSourcesStageTenBeginTransitionV1 = phoistAcyclic $ plam $
  \pre witness itemIndex itemCount totalLength itemCommitment siblings ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageTenControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
    pscriptSourcesStageTenControlIsBound # pre # witness # control
      #&& pfromData (pscriptDiscovery'redeemerCursor discovery)
        #< pfromData (pscriptSources'redeemerCount c)
      #&& pfromData (pscriptDiscovery'redeemerItemControlHash discovery)
        #== pconstant ""
      #&& pscriptSourcesStageTenBegin
        # pre # witness # control # itemIndex # itemCount # totalLength
        # itemCommitment # siblings

pverifyPreparedScriptSourcesStageTenMismatchTransitionV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> RedeemerItemProof.PRedeemerItemProofControlV1
        :--> RedeemerItemProof.PRedeemerItemProofWitnessV1 :--> PBool
    )
pverifyPreparedScriptSourcesStageTenMismatchTransitionV1 = phoistAcyclic $ plam $
  \pre witness itemControl itemWitness ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageTenControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
    pscriptSourcesStageTenControlIsBound # pre # witness # control
      #&& pfromData (pscriptDiscovery'redeemerCursor discovery)
        #< pfromData (pscriptSources'redeemerCount c)
      #&& pfromData (pscriptDiscovery'redeemerItemControlHash discovery)
        #/= pconstant ""
      #&& pscriptSourcesStageTenMismatchStep
        # pre # witness # control # itemControl # itemWitness

pverifyPreparedScriptSourcesStageTenAdvanceTransitionV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> RedeemerItemProof.PRedeemerItemProofControlV1
        :--> RedeemerItemProof.PRedeemerItemProofWitnessV1 :--> PBool
    )
pverifyPreparedScriptSourcesStageTenAdvanceTransitionV1 = phoistAcyclic $ plam $
  \pre witness itemControl itemWitness ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageTenControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  pmatch (RedeemerItemProof.pstepV1 # itemControl # itemWitness) $ \case
    PNothing -> pconstant False
    PJust result -> pmatch result $ \case
      RedeemerItemProof.PRedeemerItemProofInvalid -> pconstant False
      RedeemerItemProof.PRedeemerItemProofAdvanced nextD ->
        plet (pfromData nextD) $ \next ->
        pmatch next $ \n ->
        plet
          ( pcon $ PScriptDiscoveryControlV1
              (pscriptDiscovery'purposeCursor discovery)
              (pscriptDiscovery'sourceCursor discovery)
              (pscriptDiscovery'redeemerCursor discovery)
              (pscriptDiscovery'currentPurposeKind discovery)
              (pscriptDiscovery'currentPurposeIndex discovery)
              (pscriptDiscovery'currentScriptHash discovery)
              (pscriptDiscovery'currentSubject discovery)
              (pscriptDiscovery'matchedSourceIndex discovery)
              (pscriptDiscovery'matchedLanguageTag discovery)
              (pscriptDiscovery'matchedSourceLeaf discovery)
              (pscriptDiscovery'usedInlineBitmap discovery)
              (pscriptDiscovery'usedRedeemerBitmap discovery)
              (pdata $ RedeemerItemProof.phashControlV1 # next)
              (pscriptDiscovery'executionCount discovery)
              (pscriptDiscovery'executionPeaks discovery)
          )
          $ \nextDiscovery ->
          pand'List
            [ pscriptSourcesStageTenControlIsBound # pre # witness # control
            , pfromData (pscriptDiscovery'redeemerCursor discovery)
                #< pfromData (pscriptSources'redeemerCount c)
            , pfromData (pscriptDiscovery'redeemerItemControlHash discovery)
                #/= pconstant ""
            , pfromData (RedeemerItemProof.predeemerControl'stage n)
                #/= RedeemerItemProof.pstageTerminal
            , pscriptSourcesStageTenCurrentMatches # control # itemControl
            , pscriptSourcesStageZeroSuccessorWorkIsExact
                # pre # witness
                # (pencodeScriptSourcesDiscoveryWitness # control # 10 # nextDiscovery)
            ]

pverifyPreparedScriptSourcesStageTenTerminalMismatchTransitionV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> RedeemerItemProof.PRedeemerItemProofControlV1
        :--> RedeemerItemProof.PRedeemerItemProofWitnessV1 :--> PBool
    )
pverifyPreparedScriptSourcesStageTenTerminalMismatchTransitionV1 = phoistAcyclic $ plam $
  \pre witness itemControl itemWitness ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageTenControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  pmatch (RedeemerItemProof.pstepV1 # itemControl # itemWitness) $ \case
    PNothing -> pconstant False
    PJust result -> pmatch result $ \case
      RedeemerItemProof.PRedeemerItemProofInvalid -> pconstant False
      RedeemerItemProof.PRedeemerItemProofAdvanced nextD ->
        plet (pfromData nextD) $ \next ->
        pmatch next $ \n ->
        pmatch (RedeemerItemProof.pdescriptorV1 # next) $ \case
          PNothing -> pconstant False
          PJust descriptor -> pmatch descriptor $ \d ->
            plet
              ( pcon $ PScriptDiscoveryControlV1
                  (pscriptDiscovery'purposeCursor discovery)
                  (pscriptDiscovery'sourceCursor discovery)
                  (pdata $ pfromData (RedeemerItemProof.predeemerDescriptor'itemIndex d) + 1)
                  (pscriptDiscovery'currentPurposeKind discovery)
                  (pscriptDiscovery'currentPurposeIndex discovery)
                  (pscriptDiscovery'currentScriptHash discovery)
                  (pscriptDiscovery'currentSubject discovery)
                  (pscriptDiscovery'matchedSourceIndex discovery)
                  (pscriptDiscovery'matchedLanguageTag discovery)
                  (pscriptDiscovery'matchedSourceLeaf discovery)
                  (pscriptDiscovery'usedInlineBitmap discovery)
                  (pscriptDiscovery'usedRedeemerBitmap discovery)
                  (pdata $ pconstant "")
                  (pscriptDiscovery'executionCount discovery)
                  (pscriptDiscovery'executionPeaks discovery)
              )
              $ \nextDiscovery ->
              pand'List
                [ pscriptSourcesStageTenControlIsBound # pre # witness # control
                , pfromData (pscriptDiscovery'redeemerCursor discovery)
                    #< pfromData (pscriptSources'redeemerCount c)
                , pfromData (pscriptDiscovery'redeemerItemControlHash discovery)
                    #/= pconstant ""
                , pfromData (RedeemerItemProof.predeemerControl'stage n)
                    #== RedeemerItemProof.pstageTerminal
                , pnot
                    # (pscriptSourcesStageTenDescriptorMatches # pcon d # pcon discovery)
                , pscriptSourcesStageTenCurrentMatches # control # itemControl
                , pscriptSourcesStageZeroSuccessorWorkIsExact
                    # pre # witness
                    # (pencodeScriptSourcesDiscoveryWitness # control # 10 # nextDiscovery)
                ]

pverifyPreparedScriptSourcesStageTenMatchTransitionV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> RedeemerItemProof.PRedeemerItemProofControlV1
        :--> RedeemerItemProof.PRedeemerItemProofWitnessV1 :--> PBool
    )
pverifyPreparedScriptSourcesStageTenMatchTransitionV1 = phoistAcyclic $ plam $
  \pre witness itemControl itemWitness ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageTenControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
    pscriptSourcesStageTenControlIsBound # pre # witness # control
      #&& pfromData (pscriptDiscovery'redeemerCursor discovery)
        #< pfromData (pscriptSources'redeemerCount c)
      #&& pfromData (pscriptDiscovery'redeemerItemControlHash discovery)
        #/= pconstant ""
      #&& pscriptSourcesStageTenMatchStep
        # pre # witness # control # itemControl # itemWitness

pverifyScriptSourcesStageTenMatchSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageTenMatchSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageTenControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  pif
    ( pfromData (pscriptDiscovery'redeemerCursor discovery)
        #< pfromData (pscriptSources'redeemerCount c)
        #&& pfromData (pscriptDiscovery'redeemerItemControlHash discovery)
          #/= pconstant ""
    )
    ( pmatch auxiliary $ \case
        PRedeemerItemStepWitness redeemerControlD itemControlD itemWitnessD ->
          pmatch (pfromData redeemerControlD) $ \case
            PDNothing ->
              pscriptSourcesStageTenControlIsBound # pre # witness # control
                #&& pscriptSourcesStageTenMatchStep
                  # pre # witness # control # pfromData itemControlD
                  # pfromData itemWitnessD
            PDJust _ -> pconstant False
        _ -> pconstant False
    )
    (pconstant False)

pverifyScriptSourcesStageTenMismatchSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageTenMismatchSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageTenControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  pif
    ( pfromData (pscriptDiscovery'redeemerCursor discovery)
        #== pfromData (pscriptSources'redeemerCount c)
    )
    (pconstant False)
    ( pif
        (pfromData (pscriptDiscovery'redeemerItemControlHash discovery) #== pconstant "")
        ( pmatch auxiliary $ \case
            PRedeemerScanBeginWitness itemIndexD itemCountD totalLengthD itemCommitmentD siblingsD ->
              pscriptSourcesStageTenControlIsBound # pre # witness # control
                #&& pscriptSourcesStageTenBegin
                  # pre # witness # control # pfromData itemIndexD
                  # pfromData itemCountD # pfromData totalLengthD
                  # pfromData itemCommitmentD # pfromData siblingsD
            _ -> pconstant False
        )
        ( pmatch auxiliary $ \case
            PRedeemerItemStepWitness redeemerControlD itemControlD itemWitnessD ->
              pmatch (pfromData redeemerControlD) $ \case
                PDJust _ -> pconstant False
                PDNothing ->
                  pscriptSourcesStageTenControlIsBound # pre # witness # control
                    #&& pscriptSourcesStageTenMismatchStep
                      # pre # witness # control # pfromData itemControlD
                      # pfromData itemWitnessD
            _ -> pconstant False
        )
    )

pscriptSourcesStageElevenControlFromWitness :: forall s.
  Term s (PByteString :--> PScriptSourcesControlV1)
pscriptSourcesStageElevenControlFromWitness = phoistAcyclic $ plam $ \workWitnessCbor ->
  pmatch (pdeserialise # workWitnessCbor) $ \case
    PNothing -> perror
    PJust dat ->
      plet (pasList # dat) $ \items ->
      pif (plength # items #== 31)
        ( plet (pscriptSourcesControlFromDataItems # items # pconstant "") $ \base ->
          pmatch base $ \c ->
          pif (pfromData (pscriptSources'stage c) #== 11)
            ( pcon $ PScriptSourcesControlV1
                (pscriptSources'compactCbor c)
                (pscriptSources'witnessSetCompactCbor c)
                (pscriptSources'fieldPreimageLengthsCbor c)
                (pscriptSources'contextCbor c)
                (pscriptSources'resolvedInputCount c)
                (pscriptSources'resolvedInputsAccumulator c)
                (pscriptSources'signerCount c)
                (pscriptSources'signerFrontierCommitment c)
                (pscriptSources'resolvedItemPeaks c)
                (pscriptSources'stage c)
                (pscriptSources'sourceCount c)
                (pscriptSources'sourcePeaks c)
                (pscriptSources'redeemerCount c)
                (pscriptSources'redeemerPeaks c)
                (pscriptSources'replayCursor c)
                (pscriptSources'replayAccumulator c)
                (pscriptSources'replayRemainingScheduleHash c)
                (pscriptSources'spendIndex c)
                (pscriptSources'purposeCount c)
                (pscriptSources'purposePeaks c)
                (pscriptSources'outputCursor c)
                (pscriptSources'outputCount c)
                (pscriptSources'outputPeaks c)
                (pscriptSources'outputTotalCount c)
                (pscriptSources'receiveScan c)
                (pscriptSources'sourceTotalCount c)
                (pscriptSources'redeemerTotalCount c)
                (pscriptSources'observerScan c)
                ( pdata $ pdecodeScriptDiscoveryControl
                    # (pasByteStr # (pelemAt # 30 # items))
                )
                (pscriptSources'outputProof c)
                (pscriptSources'pendingSourceCbor c)
                (pscriptSources'mintFold c)
                (pscriptSources'resolutionScheduleHash c)
            )
            perror
        )
        perror

pscriptSourcesStageElevenControlIsBound :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PBool
    )
pscriptSourcesStageElevenControlIsBound = phoistAcyclic $ plam $
  \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
    pand'List
      [ pfromData (poneStep'workWitnessCbor stepWitness)
          #== pencodeScriptSourcesDiscoveryWitness # control # 11 # pcon discovery
      , pfromData (pscriptSources'stage c) #== 11
      , NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (pscriptSources'compactCbor c)
          # pfromData (pscriptSources'witnessSetCompactCbor c)
          # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
          #== pfromData (pmachineState'transactionCommitment preState)
      , phashValidationContext # pfromData (pscriptSources'contextCbor c)
          #== pfromData (pmachineState'validationContextHash preState)
      , pfromData (pscriptSources'sourceTotalCount c)
          #== pfromData (pscriptSources'sourceCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'sourceCount c)
          # pfromData (pscriptSources'sourcePeaks c)
      , pfromData (pscriptSources'redeemerTotalCount c)
          #== pfromData (pscriptSources'redeemerCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'redeemerCount c)
          # pfromData (pscriptSources'redeemerPeaks c)
      , pfromData (pscriptSources'purposeCount c)
          #== pfromData (pscriptDiscovery'purposeCursor discovery)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'purposeCount c)
          # pfromData (pscriptSources'purposePeaks c)
      , pfromData (pscriptDiscovery'sourceCursor discovery) #>= 0
      , pfromData (pscriptDiscovery'sourceCursor discovery)
          #<= pfromData (pscriptSources'sourceCount c)
      , pfromData (pscriptDiscovery'redeemerCursor discovery) #>= 0
      , pfromData (pscriptDiscovery'redeemerCursor discovery)
          #<= pfromData (pscriptSources'redeemerCount c)
      , pfromData (pscriptDiscovery'executionCount discovery)
          #== pfromData (pscriptDiscovery'purposeCursor discovery)
      , pfrontierIsWellFormed
          # pfromData (pscriptDiscovery'executionCount discovery)
          # pfromData (pscriptDiscovery'executionPeaks discovery)
      , pfromData (pscriptDiscovery'currentPurposeKind discovery) #== -1
      , pfromData (pscriptDiscovery'currentPurposeIndex discovery) #== -1
      , pfromData (pscriptDiscovery'currentScriptHash discovery) #== pconstant ""
      , pfromData (pscriptDiscovery'currentSubject discovery) #== pconstant ""
      , pfromData (pscriptDiscovery'matchedSourceIndex discovery) #== -1
      , pfromData (pscriptDiscovery'matchedLanguageTag discovery) #== -1
      , pfromData (pscriptDiscovery'matchedSourceLeaf discovery) #== pconstant ""
      , pfromData (pscriptDiscovery'usedInlineBitmap discovery) #>= 0
      , pfromData (pscriptDiscovery'usedInlineBitmap discovery)
          #< pscriptDiscoveryBit # pfromData (pscriptSources'sourceCount c)
      , pfromData (pscriptDiscovery'usedRedeemerBitmap discovery) #>= 0
      , pfromData (pscriptDiscovery'usedRedeemerBitmap discovery)
          #< pscriptDiscoveryBit # pfromData (pscriptSources'redeemerCount c)
      ]

pscriptDiscoveryBitmapHas :: forall s.
  Term s (PInteger :--> PInteger :--> PBool)
pscriptDiscoveryBitmapHas = phoistAcyclic $ plam $ \bitmap index ->
  plet (pscriptDiscoveryBit # index) $ \bit ->
    pmod # (pdiv # bitmap # bit) # 2 #== 1

pverifyScriptSourcesStageElevenFinishSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool )
pverifyScriptSourcesStageElevenFinishSemanticsV1 = phoistAcyclic $ plam $
  \pre witness ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageElevenControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pscriptDiscovery'purposeCursor discovery)
        (pscriptDiscovery'sourceCursor discovery)
        (pdata 0)
        (pscriptDiscovery'currentPurposeKind discovery)
        (pscriptDiscovery'currentPurposeIndex discovery)
        (pscriptDiscovery'currentScriptHash discovery)
        (pscriptDiscovery'currentSubject discovery)
        (pscriptDiscovery'matchedSourceIndex discovery)
        (pscriptDiscovery'matchedLanguageTag discovery)
        (pscriptDiscovery'matchedSourceLeaf discovery)
        (pscriptDiscovery'usedInlineBitmap discovery)
        (pscriptDiscovery'usedRedeemerBitmap discovery)
        (pscriptDiscovery'redeemerItemControlHash discovery)
        (pscriptDiscovery'executionCount discovery)
        (pscriptDiscovery'executionPeaks discovery)
    )
    $ \nextDiscovery ->
    pscriptSourcesStageElevenControlIsBound # pre # witness # control
      #&& pfromData (pscriptDiscovery'sourceCursor discovery)
        #== pfromData (pscriptSources'sourceCount c)
      #&& pscriptSourcesStageZeroSuccessorWorkIsExact
        # pre # witness
        # (pencodeScriptSourcesDiscoveryWitness # control # 12 # nextDiscovery)

pverifyScriptSourcesStageElevenSourceSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInteger :--> PInteger :--> PByteString :--> PInteger
        :--> PByteString :--> PInteger :--> PByteString
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pverifyScriptSourcesStageElevenSourceSemanticsV1 = phoistAcyclic $ plam $
  \pre witness sourceIndex originKind sourceKey languageTag scriptHash totalLength itemCommitment siblings ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageElevenControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pscriptDiscovery'purposeCursor discovery)
        (pdata $ sourceIndex + 1)
        (pscriptDiscovery'redeemerCursor discovery)
        (pscriptDiscovery'currentPurposeKind discovery)
        (pscriptDiscovery'currentPurposeIndex discovery)
        (pscriptDiscovery'currentScriptHash discovery)
        (pscriptDiscovery'currentSubject discovery)
        (pscriptDiscovery'matchedSourceIndex discovery)
        (pscriptDiscovery'matchedLanguageTag discovery)
        (pscriptDiscovery'matchedSourceLeaf discovery)
        (pscriptDiscovery'usedInlineBitmap discovery)
        (pscriptDiscovery'usedRedeemerBitmap discovery)
        (pscriptDiscovery'redeemerItemControlHash discovery)
        (pscriptDiscovery'executionCount discovery)
        (pscriptDiscovery'executionPeaks discovery)
    )
    $ \nextDiscovery ->
    pand'List
      [ pscriptSourcesStageElevenControlIsBound # pre # witness # control
      , pfromData (pscriptDiscovery'sourceCursor discovery)
          #< pfromData (pscriptSources'sourceCount c)
      , pscriptSourcesStageNineSourceIsAuthenticated
          # control # sourceIndex # originKind # sourceKey # languageTag
          # scriptHash # totalLength # itemCommitment # siblings
      , pif
          ( originKind #== 0
              #&& pnot
                # ( pscriptDiscoveryBitmapHas
                      # pfromData (pscriptDiscovery'usedInlineBitmap discovery)
                      # sourceIndex
                  )
          )
          ( prejectedSuccessorIsExact
              # pre # pfromData (poneStep'claimedSuccessor stepWitness)
              # pconstant "E_INVALID_FIELD_TYPE"
          )
          ( pscriptSourcesStageZeroSuccessorWorkIsExact
              # pre # witness
              # (pencodeScriptSourcesDiscoveryWitness # control # 11 # nextDiscovery)
          )
      ]

pverifyScriptSourcesStageElevenSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageElevenSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageElevenControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  pif
    ( pfromData (pscriptDiscovery'sourceCursor discovery)
        #== pfromData (pscriptSources'sourceCount c)
    )
    ( pmatch auxiliary $ \case
        PNoAuxiliaryWitness ->
          pverifyScriptSourcesStageElevenFinishSemanticsV1 # pre # witness
        _ -> pconstant False
    )
    ( pmatch auxiliary $ \case
        PScriptSourceScanWitness sourceIndexD originKindD sourceKeyD languageTagD scriptHashD totalLengthD itemCommitmentD siblingsD ->
          pverifyScriptSourcesStageElevenSourceSemanticsV1
            # pre # witness # pfromData sourceIndexD # pfromData originKindD
            # pfromData sourceKeyD # pfromData languageTagD # pfromData scriptHashD
            # pfromData totalLengthD # pfromData itemCommitmentD # pfromData siblingsD
        _ -> pconstant False
    )

pencodeNativeScriptsWitnessV1 :: forall s.
  Term s (PScriptSourcesControlV1 :--> PInteger :--> PInteger :--> PByteString)
pencodeNativeScriptsWitnessV1 = phoistAcyclic $ plam $
  \control executionCursor languageBitmap ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  pmatch (pfromData $ pscriptSources'receiveScan c) $ \receiveScan ->
  pmatch (pfromData $ pscriptSources'mintFold c) $ \mintFold ->
  pif
    ( executionCursor #>= 0
        #&& executionCursor #<= pfromData (pscriptDiscovery'executionCount discovery)
        #&& languageBitmap #>= 0
        #&& languageBitmap #<= 3
    )
    ( pencodeNativeScriptsControlV1
        # ( pcon $ PNativeScriptsControlV1
              (pscriptSources'compactCbor c)
              (pscriptSources'witnessSetCompactCbor c)
              (pscriptSources'fieldPreimageLengthsCbor c)
              (pscriptSources'contextCbor c)
              (pscriptSources'resolvedInputCount c)
              (pscriptSources'resolvedInputsAccumulator c)
              (pscriptSources'spendIndex c)
              (pscriptSources'resolvedItemPeaks c)
              (pscriptSources'signerCount c)
              (pscriptSources'signerFrontierCommitment c)
              (pscriptSources'sourceCount c)
              (pscriptSources'sourcePeaks c)
              (pscriptSources'redeemerCount c)
              (pscriptSources'redeemerPeaks c)
              (pscriptSources'purposeCount c)
              (pscriptSources'purposePeaks c)
              (pscriptSources'outputCount c)
              (pscriptSources'outputPeaks c)
              (preceiveScan'descriptorPeaks receiveScan)
              (pmintFold'assetCount mintFold)
              (pmintFold'assetPeaks mintFold)
              (pscriptDiscovery'executionCount discovery)
              (pscriptDiscovery'executionPeaks discovery)
              (pdata executionCursor)
              (pdata languageBitmap)
              (pscriptSources'resolutionScheduleHash c)
          )
    )
    perror

pscriptSourcesStageTwelveControlFromWitness :: forall s.
  Term s (PByteString :--> PScriptSourcesControlV1)
pscriptSourcesStageTwelveControlFromWitness = phoistAcyclic $ plam $ \workWitnessCbor ->
  pmatch (pdeserialise # workWitnessCbor) $ \case
    PNothing -> perror
    PJust dat ->
      plet (pasList # dat) $ \items ->
      pif (plength # items #== 31)
        ( plet (pscriptSourcesControlFromDataItems # items # pconstant "") $ \base ->
          pmatch base $ \c ->
          pif (pfromData (pscriptSources'stage c) #== 12)
            ( pcon $ PScriptSourcesControlV1
                (pscriptSources'compactCbor c)
                (pscriptSources'witnessSetCompactCbor c)
                (pscriptSources'fieldPreimageLengthsCbor c)
                (pscriptSources'contextCbor c)
                (pscriptSources'resolvedInputCount c)
                (pscriptSources'resolvedInputsAccumulator c)
                (pscriptSources'signerCount c)
                (pscriptSources'signerFrontierCommitment c)
                (pscriptSources'resolvedItemPeaks c)
                (pscriptSources'stage c)
                (pscriptSources'sourceCount c)
                (pscriptSources'sourcePeaks c)
                (pscriptSources'redeemerCount c)
                (pscriptSources'redeemerPeaks c)
                (pscriptSources'replayCursor c)
                (pscriptSources'replayAccumulator c)
                (pscriptSources'replayRemainingScheduleHash c)
                (pscriptSources'spendIndex c)
                (pscriptSources'purposeCount c)
                (pscriptSources'purposePeaks c)
                (pscriptSources'outputCursor c)
                (pscriptSources'outputCount c)
                (pscriptSources'outputPeaks c)
                (pscriptSources'outputTotalCount c)
                (pscriptSources'receiveScan c)
                (pscriptSources'sourceTotalCount c)
                (pscriptSources'redeemerTotalCount c)
                (pscriptSources'observerScan c)
                ( pdata $ pdecodeScriptDiscoveryControl
                    # (pasByteStr # (pelemAt # 30 # items))
                )
                (pscriptSources'outputProof c)
                (pscriptSources'pendingSourceCbor c)
                (pscriptSources'mintFold c)
                (pscriptSources'resolutionScheduleHash c)
            )
            perror
        )
        perror

pscriptSourcesStageTwelveControlIsBound :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PBool
    )
pscriptSourcesStageTwelveControlIsBound = phoistAcyclic $ plam $
  \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
    pand'List
      [ pfromData (poneStep'workWitnessCbor stepWitness)
          #== pencodeScriptSourcesDiscoveryWitness # control # 12 # pcon discovery
      , pfromData (pscriptSources'stage c) #== 12
      , NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (pscriptSources'compactCbor c)
          # pfromData (pscriptSources'witnessSetCompactCbor c)
          # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
          #== pfromData (pmachineState'transactionCommitment preState)
      , phashValidationContext # pfromData (pscriptSources'contextCbor c)
          #== pfromData (pmachineState'validationContextHash preState)
      , pfromData (pscriptSources'sourceTotalCount c)
          #== pfromData (pscriptSources'sourceCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'sourceCount c)
          # pfromData (pscriptSources'sourcePeaks c)
      , pfromData (pscriptSources'redeemerTotalCount c)
          #== pfromData (pscriptSources'redeemerCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'redeemerCount c)
          # pfromData (pscriptSources'redeemerPeaks c)
      , pfromData (pscriptSources'purposeCount c)
          #== pfromData (pscriptDiscovery'purposeCursor discovery)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'purposeCount c)
          # pfromData (pscriptSources'purposePeaks c)
      , pfromData (pscriptDiscovery'sourceCursor discovery)
          #== pfromData (pscriptSources'sourceCount c)
      , pfromData (pscriptDiscovery'redeemerCursor discovery) #>= 0
      , pfromData (pscriptDiscovery'redeemerCursor discovery)
          #<= pfromData (pscriptSources'redeemerCount c)
      , pfromData (pscriptDiscovery'executionCount discovery)
          #== pfromData (pscriptDiscovery'purposeCursor discovery)
      , pfrontierIsWellFormed
          # pfromData (pscriptDiscovery'executionCount discovery)
          # pfromData (pscriptDiscovery'executionPeaks discovery)
      , pfromData (pscriptDiscovery'currentPurposeKind discovery) #== -1
      , pfromData (pscriptDiscovery'currentPurposeIndex discovery) #== -1
      , pfromData (pscriptDiscovery'currentScriptHash discovery) #== pconstant ""
      , pfromData (pscriptDiscovery'currentSubject discovery) #== pconstant ""
      , pfromData (pscriptDiscovery'matchedSourceIndex discovery) #== -1
      , pfromData (pscriptDiscovery'matchedLanguageTag discovery) #== -1
      , pfromData (pscriptDiscovery'matchedSourceLeaf discovery) #== pconstant ""
      , pfromData (pscriptDiscovery'usedInlineBitmap discovery) #>= 0
      , pfromData (pscriptDiscovery'usedInlineBitmap discovery)
          #< pscriptDiscoveryBit # pfromData (pscriptSources'sourceCount c)
      , pfromData (pscriptDiscovery'usedRedeemerBitmap discovery) #>= 0
      , pfromData (pscriptDiscovery'usedRedeemerBitmap discovery)
          #< pscriptDiscoveryBit # pfromData (pscriptSources'redeemerCount c)
      ]

pscriptSourcesStageTwelveBegin :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PInteger :--> PInteger
        :--> PInteger :--> PByteString
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pscriptSourcesStageTwelveBegin = phoistAcyclic $ plam $
  \pre witness control itemIndex itemCount totalLength itemCommitment siblings ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  plet
    ( RedeemerItemProof.pinitialControlV1
        # RedeemerItemProof.pmodeDescriptor # itemIndex # itemCount
        # totalLength # itemCommitment # (-1) # (-1)
    )
    $ \itemControl ->
  plet
    ( pcon $ PScriptDiscoveryControlV1
        (pscriptDiscovery'purposeCursor discovery)
        (pscriptDiscovery'sourceCursor discovery)
        (pscriptDiscovery'redeemerCursor discovery)
        (pscriptDiscovery'currentPurposeKind discovery)
        (pscriptDiscovery'currentPurposeIndex discovery)
        (pscriptDiscovery'currentScriptHash discovery)
        (pscriptDiscovery'currentSubject discovery)
        (pscriptDiscovery'matchedSourceIndex discovery)
        (pscriptDiscovery'matchedLanguageTag discovery)
        (pscriptDiscovery'matchedSourceLeaf discovery)
        (pscriptDiscovery'usedInlineBitmap discovery)
        (pscriptDiscovery'usedRedeemerBitmap discovery)
        (pdata $ RedeemerItemProof.phashControlV1 # itemControl)
        (pscriptDiscovery'executionCount discovery)
        (pscriptDiscovery'executionPeaks discovery)
    )
    $ \nextDiscovery ->
    pand'List
      [ itemIndex #== pfromData (pscriptDiscovery'redeemerCursor discovery)
      , itemCount #== pfromData (pscriptSources'redeemerCount c)
      , pverifyMembership
          # pfromData (pscriptSources'redeemerCount c)
          # pfromData (pscriptSources'redeemerPeaks c)
          # itemIndex
          # (ScriptProof.predeemerItemLeafHash # itemIndex # itemCommitment)
          # siblings
      , pscriptSourcesStageZeroSuccessorWorkIsExact
          # pre # witness
          # (pencodeScriptSourcesDiscoveryWitness # control # 12 # nextDiscovery)
      ]

pverifyPreparedScriptSourcesStageTwelveBeginTransitionV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInteger :--> PInteger :--> PInteger :--> PByteString
        :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pverifyPreparedScriptSourcesStageTwelveBeginTransitionV1 = phoistAcyclic $ plam $
  \pre witness itemIndex itemCount totalLength itemCommitment siblings ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageTwelveControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
    pscriptSourcesStageTwelveControlIsBound # pre # witness # control
      #&& pfromData (pscriptDiscovery'redeemerCursor discovery)
        #< pfromData (pscriptSources'redeemerCount c)
      #&& pfromData (pscriptDiscovery'redeemerItemControlHash discovery)
        #== pconstant ""
      #&& pscriptSourcesStageTwelveBegin
        # pre # witness # control # itemIndex # itemCount # totalLength
        # itemCommitment # siblings

pscriptSourcesStageTwelveItemStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1
        :--> RedeemerItemProof.PRedeemerItemProofControlV1
        :--> RedeemerItemProof.PRedeemerItemProofWitnessV1 :--> PBool
    )
pscriptSourcesStageTwelveItemStep = phoistAcyclic $ plam $
  \pre witness control itemControl itemWitness ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  plet (pscriptSourcesStageTenCurrentMatches # control # itemControl) $ \currentMatches ->
  pmatch (RedeemerItemProof.pstepV1 # itemControl # itemWitness) $ \case
    PNothing -> pconstant False
    PJust result -> pmatch result $ \case
      RedeemerItemProof.PRedeemerItemProofInvalid -> pconstant False
      RedeemerItemProof.PRedeemerItemProofAdvanced nextD ->
        plet (pfromData nextD) $ \next ->
        pmatch next $ \nextControl ->
        pif
          ( pfromData (RedeemerItemProof.predeemerControl'stage nextControl)
              #== RedeemerItemProof.pstageTerminal
          )
          ( pmatch (RedeemerItemProof.pdescriptorV1 # next) $ \case
              PNothing -> pconstant False
              PJust descriptor -> pmatch descriptor $ \d ->
                plet
                  ( pcon $ PScriptDiscoveryControlV1
                      (pscriptDiscovery'purposeCursor discovery)
                      (pscriptDiscovery'sourceCursor discovery)
                      (pdata $ pfromData (RedeemerItemProof.predeemerDescriptor'itemIndex d) + 1)
                      (pscriptDiscovery'currentPurposeKind discovery)
                      (pscriptDiscovery'currentPurposeIndex discovery)
                      (pscriptDiscovery'currentScriptHash discovery)
                      (pscriptDiscovery'currentSubject discovery)
                      (pscriptDiscovery'matchedSourceIndex discovery)
                      (pscriptDiscovery'matchedLanguageTag discovery)
                      (pscriptDiscovery'matchedSourceLeaf discovery)
                      (pscriptDiscovery'usedInlineBitmap discovery)
                      (pscriptDiscovery'usedRedeemerBitmap discovery)
                      (pdata $ pconstant "")
                      (pscriptDiscovery'executionCount discovery)
                      (pscriptDiscovery'executionPeaks discovery)
                  )
                  $ \advanced ->
                  currentMatches
                    #&& pif
                      ( pnot
                          # ( pscriptDiscoveryBitmapHas
                                # pfromData (pscriptDiscovery'usedRedeemerBitmap discovery)
                                # pfromData (RedeemerItemProof.predeemerDescriptor'itemIndex d)
                            )
                      )
                      ( prejectedSuccessorIsExact
                          # pre # pfromData (poneStep'claimedSuccessor stepWitness)
                          # pconstant "E_INVALID_FIELD_TYPE"
                      )
                      ( pscriptSourcesStageZeroSuccessorWorkIsExact
                          # pre # witness
                          # (pencodeScriptSourcesDiscoveryWitness # control # 12 # advanced)
                      )
          )
          ( plet
              ( pcon $ PScriptDiscoveryControlV1
                  (pscriptDiscovery'purposeCursor discovery)
                  (pscriptDiscovery'sourceCursor discovery)
                  (pscriptDiscovery'redeemerCursor discovery)
                  (pscriptDiscovery'currentPurposeKind discovery)
                  (pscriptDiscovery'currentPurposeIndex discovery)
                  (pscriptDiscovery'currentScriptHash discovery)
                  (pscriptDiscovery'currentSubject discovery)
                  (pscriptDiscovery'matchedSourceIndex discovery)
                  (pscriptDiscovery'matchedLanguageTag discovery)
                  (pscriptDiscovery'matchedSourceLeaf discovery)
                  (pscriptDiscovery'usedInlineBitmap discovery)
                  (pscriptDiscovery'usedRedeemerBitmap discovery)
                  (pdata $ RedeemerItemProof.phashControlV1 # next)
                  (pscriptDiscovery'executionCount discovery)
                  (pscriptDiscovery'executionPeaks discovery)
              )
              $ \advanced ->
              currentMatches
                #&& pscriptSourcesStageZeroSuccessorWorkIsExact
                  # pre # witness
                  # (pencodeScriptSourcesDiscoveryWitness # control # 12 # advanced)
          )

pverifyScriptSourcesStageTwelveFinishSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool )
pverifyScriptSourcesStageTwelveFinishSemanticsV1 = phoistAcyclic $ plam $
  \pre witness ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageTwelveControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  pmatch (pfromData $ poneStep'claimedSuccessor stepWitness) $ \post ->
    pand'List
      [ pscriptSourcesStageTwelveControlIsBound # pre # witness # control
      , pfromData (pscriptDiscovery'redeemerCursor discovery)
          #== pfromData (pscriptSources'redeemerCount c)
      , pfromData (pmachineState'phase post) #== pcon PNativeScripts
      , pfromData (pmachineState'workRoot post)
          #== phashWorkWitness
            # pcon PNativeScripts
            # (pfromData (pmachineState'programCounter preState) + 1)
            # (pencodeNativeScriptsWitnessV1 # control # 0 # 0)
      ]

pscriptSourcesStageTwelveRedeemerAuxiliaryIsFamily :: forall s.
  Term s (PValidationAuxiliaryWitnessV1 :--> PBool)
pscriptSourcesStageTwelveRedeemerAuxiliaryIsFamily = phoistAcyclic $ plam $ \auxiliary ->
  pmatch auxiliary $ \case
    PRedeemerScanBeginWitness {} -> pconstant True
    PRedeemerItemStepWitness redeemerControlD _ _ ->
      pmatch (pfromData redeemerControlD) $ \case
        PDNothing -> pconstant True
        PDJust _ -> pconstant False
    _ -> pconstant False

pverifyScriptSourcesStageTwelveRedeemerSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageTwelveRedeemerSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pif (pnot # (pscriptSourcesStageTwelveRedeemerAuxiliaryIsFamily # auxiliary))
    (pconstant False) $
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageTwelveControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  pif
    ( pfromData (pscriptDiscovery'redeemerCursor discovery)
        #== pfromData (pscriptSources'redeemerCount c)
    )
    (pconstant False)
    ( pif
        (pfromData (pscriptDiscovery'redeemerItemControlHash discovery) #== pconstant "")
        ( pmatch auxiliary $ \case
            PRedeemerScanBeginWitness itemIndexD itemCountD totalLengthD itemCommitmentD siblingsD ->
              pscriptSourcesStageTwelveControlIsBound # pre # witness # control
                #&& pscriptSourcesStageTwelveBegin
                  # pre # witness # control # pfromData itemIndexD
                  # pfromData itemCountD # pfromData totalLengthD
                  # pfromData itemCommitmentD # pfromData siblingsD
            _ -> pconstant False
        )
        ( pmatch auxiliary $ \case
            PRedeemerItemStepWitness redeemerControlD itemControlD itemWitnessD ->
              pmatch (pfromData redeemerControlD) $ \case
                PDJust _ -> pconstant False
                PDNothing ->
                  pscriptSourcesStageTwelveControlIsBound # pre # witness # control
                    #&& pscriptSourcesStageTwelveItemStep
                      # pre # witness # control # pfromData itemControlD
                      # pfromData itemWitnessD
            _ -> pconstant False
        )
    )

pverifyScriptSourcesStageTwelveSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesStageTwelveSemanticsV1 = phoistAcyclic $ plam $
  \pre witness auxiliary ->
  pmatch witness $ \stepWitness ->
  plet
    ( pscriptSourcesStageTwelveControlFromWitness
        # pfromData (poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  pif
    ( pfromData (pscriptDiscovery'redeemerCursor discovery)
        #== pfromData (pscriptSources'redeemerCount c)
    )
    ( pmatch auxiliary $ \case
        PNoAuxiliaryWitness ->
          pfromData (pscriptDiscovery'redeemerItemControlHash discovery) #== pconstant ""
            #&& pverifyScriptSourcesStageTwelveFinishSemanticsV1 # pre # witness
        _ -> pconstant False
    )
    (pverifyScriptSourcesStageTwelveRedeemerSemanticsV1 # pre # witness # auxiliary)

-- | Aiken @script_sources_control_from_witness@.
pscriptSourcesControlFromWitness :: forall s.
  Term s (PByteString :--> PScriptSourcesControlV1)
pscriptSourcesControlFromWitness = phoistAcyclic $ plam $ \workWitnessCbor ->
  pmatch (pdeserialise # workWitnessCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \items ->
      pif (plength # items #== 30)
        (pscriptSourcesControlFromDataItems # items # pconstant "")
        ( pif (plength # items #== 31)
            ( plet (pscriptSourcesControlFromDataItems # items # pconstant "") $ \base ->
              pmatch base $ \c ->
              plet (pfromData $ pscriptSources'stage c) $ \stage ->
                pif (stage #< 8)
                  (pscriptSourcesStageZeroControlFromWitness # workWitnessCbor)
                  ( pscriptSourcesWithStageDiscovery
                      # base # stage
                      # (pdecodeScriptDiscoveryControl # (pasByteStr # (pelemAt # 30 # items)))
                  )
            )
            perror
        )

pscriptSourcesExactControl :: forall s.
  Term s (PScriptSourcesControlV1 :--> PByteString)
pscriptSourcesExactControl = phoistAcyclic $ plam $ \control ->
  pmatch control $ \c ->
  plet (pfromData $ pscriptSources'stage c) $ \stage ->
  plet (pfromData $ pscriptSources'pendingSourceCbor c) $ \pendingCbor ->
    pif (stage #== 0 #&& pendingCbor #/= pconstant "")
      (pencodeScriptSourcesPendingSourceWitness # control # pendingCbor)
      ( pif (stage #== 1 #&& pendingCbor #/= pconstant "")
          (pencodeScriptSourcesRedeemerItemWitness # control # pendingCbor)
          ( pif (stage #== 5)
              ( pmatch (pfromData $ pscriptSources'outputProof c) $ \case
                  PDNothing -> pencodeScriptSourcesBaseControl # control
                  PDJust outputProofD ->
                    pencodeScriptSourcesOutputProofWitness # control # pfromData outputProofD
              )
              ( pif (stage #< 8)
                  (pencodeScriptSourcesBaseControl # control)
                  ( pencodeScriptSourcesDiscoveryWitness
                      # control # stage # pfromData (pscriptSources'discovery c)
                  )
              )
          )
      )

-- | Aiken @script_discovery_control_is_well_formed@.
pscriptDiscoveryControlIsWellFormed :: forall s.
  Term s (PScriptDiscoveryControlV1 :--> PBool)
pscriptDiscoveryControlIsWellFormed = phoistAcyclic $ plam $ \control ->
  pmatch control $ \c ->
    pand'List
      [ pfromData (pscriptDiscovery'purposeCursor c) #>= 0
      , pfromData (pscriptDiscovery'sourceCursor c) #>= 0
      , pfromData (pscriptDiscovery'redeemerCursor c) #>= 0
      , pfromData (pscriptDiscovery'currentPurposeKind c) #>= -1
      , pfromData (pscriptDiscovery'currentPurposeKind c) #<= 3
      , pfromData (pscriptDiscovery'currentPurposeIndex c) #>= -1
      , pfromData (pscriptDiscovery'currentScriptHash c) #== pconstant ""
          #|| plengthBS # pfromData (pscriptDiscovery'currentScriptHash c) #== 28
      , pfromData (pscriptDiscovery'matchedSourceIndex c) #>= -1
      , pfromData (pscriptDiscovery'matchedLanguageTag c) #== -1
          #|| pfromData (pscriptDiscovery'matchedLanguageTag c) #== 0
          #|| pfromData (pscriptDiscovery'matchedLanguageTag c) #== 3
          #|| pfromData (pscriptDiscovery'matchedLanguageTag c) #== 128
      , pfromData (pscriptDiscovery'matchedSourceLeaf c) #== pconstant ""
          #|| plengthBS # pfromData (pscriptDiscovery'matchedSourceLeaf c) #== 32
      , pfromData (pscriptDiscovery'usedInlineBitmap c) #>= 0
      , pfromData (pscriptDiscovery'usedRedeemerBitmap c) #>= 0
      , pfrontierIsWellFormed
          # pfromData (pscriptDiscovery'executionCount c)
          # pfromData (pscriptDiscovery'executionPeaks c)
      , plengthBS # pfromData (pscriptDiscovery'redeemerItemControlHash c) #== 0
          #|| plengthBS # pfromData (pscriptDiscovery'redeemerItemControlHash c) #== 32
      ]

-- | Aiken @observer_purpose_scan_is_well_formed@.
pscriptSourcesObserverScanIsWellFormed :: forall s.
  Term s (PInteger :--> PObserverPurposeScanControlV1 :--> PBool)
pscriptSourcesObserverScanIsWellFormed = phoistAcyclic $ plam $ \stage scan ->
  pmatch scan $ \c ->
    pand'List
      [ pfromData (pobserverScan'totalCount c) #>= 0
      , pfromData (pobserverScan'seen c) #>= 0
      , pfromData (pobserverScan'seen c) #<= pfromData (pobserverScan'totalCount c)
      , pif (pfromData (pobserverScan'seen c) #== 0)
          (pfromData (pobserverScan'previousHash c) #== pconstant "")
          (plengthBS # pfromData (pobserverScan'previousHash c) #== 28)
      , pif (stage #== 7)
          (pconstant True)
          ( pfromData (pobserverScan'totalCount c) #== 0
              #&& pfromData (pobserverScan'seen c) #== 0
              #&& pfromData (pobserverScan'previousHash c) #== pconstant ""
          )
      ]

-- | Full Aiken @receive_purpose_scan_is_well_formed@.
pscriptSourcesReceiveScanIsWellFormedV1 :: forall s.
  Term s (PInteger :--> PInteger :--> PInteger :--> PReceivePurposeScanControlV1 :--> PBool)
pscriptSourcesReceiveScanIsWellFormedV1 = phoistAcyclic $ plam $
  \stage outputCursor outputCount scan ->
  pmatch scan $ \c ->
  plet (pif (stage #< 5) 0 (pif (stage #== 5) outputCursor outputCount)) $ \descriptorCount ->
    pand'List
      [ pfrontierIsWellFormed # descriptorCount # pfromData (preceiveScan'descriptorPeaks c)
      , pfromData (preceiveScan'sourceCount c) #>= 0
      , pfromData (preceiveScan'sourceCount c) #<= outputCount
      , pfrontierIsWellFormed
          # pfromData (preceiveScan'sourceCount c) # pfromData (preceiveScan'sourcePeaks c)
      , pfromData (preceiveScan'receiveCount c) #>= 0
      , pfromData (preceiveScan'receiveCount c) #<= pfromData (preceiveScan'sourceCount c)
      , pif (pfromData (preceiveScan'receiveCount c) #== 0)
          (pfromData (preceiveScan'previousHash c) #== pconstant "")
          (plengthBS # pfromData (preceiveScan'previousHash c) #== 28)
      , pfromData (preceiveScan'candidateHash c) #== pconstant ""
          #|| ( plengthBS # pfromData (preceiveScan'candidateHash c) #== 28
                  #&& ( pfromData (preceiveScan'previousHash c) #== pconstant ""
                          #|| pfromData (preceiveScan'previousHash c)
                            #< pfromData (preceiveScan'candidateHash c)
                      )
              )
      , pif (stage #< 5)
          (scan #== pemptyReceivePurposeScanControl)
          ( pif (stage #< 7)
              ( pfromData (preceiveScan'receiveCount c) #== 0
                  #&& pfromData (preceiveScan'previousHash c) #== pconstant ""
                  #&& pfromData (preceiveScan'candidateHash c) #== pconstant ""
              )
              ( pif (stage #== 7)
                  (pconstant True)
                  ( pfromData (preceiveScan'sourceCount c) #== 0
                      #&& pfromData (preceiveScan'sourcePeaks c) #== pnil
                      #&& pfromData (preceiveScan'receiveCount c) #== 0
                      #&& pfromData (preceiveScan'previousHash c) #== pconstant ""
                      #&& pfromData (preceiveScan'candidateHash c) #== pconstant ""
                  )
              )
          )
      ]

pscriptSourcesDiscoveryStageIsWellFormed :: forall s.
  Term s (PScriptSourcesControlV1 :--> PBool)
pscriptSourcesDiscoveryStageIsWellFormed = phoistAcyclic $ plam $ \control ->
  pmatch control $ \c ->
  pmatch (pfromData $ pscriptSources'discovery c) $ \discovery ->
  plet (pfromData $ pscriptSources'stage c) $ \stage ->
    pif (stage #< 8)
      (pcon discovery #== pemptyScriptDiscoveryControl)
      ( pand'List
          [ stage #<= 12
          , pfromData (pscriptDiscovery'purposeCursor discovery)
              #<= pfromData (pscriptSources'purposeCount c)
          , pfromData (pscriptDiscovery'sourceCursor discovery)
              #<= pfromData (pscriptSources'sourceCount c)
          , pfromData (pscriptDiscovery'redeemerCursor discovery)
              #<= pfromData (pscriptSources'redeemerCount c)
          , pfromData (pscriptDiscovery'executionCount discovery)
              #== pfromData (pscriptDiscovery'purposeCursor discovery)
          , pfromData (pscriptDiscovery'usedInlineBitmap discovery)
              #< pscriptDiscoveryBit # pfromData (pscriptSources'sourceCount c)
          , pfromData (pscriptDiscovery'usedRedeemerBitmap discovery)
              #< pscriptDiscoveryBit # pfromData (pscriptSources'redeemerCount c)
          , pif (stage #== 8)
              ( pand'List
                  [ pfromData (pscriptDiscovery'redeemerItemControlHash discovery) #== pconstant ""
                  , discoveryCurrentIsEmpty discovery
                  ]
              )
              ( pif (stage #== 9)
                  ( pand'List
                      [ pfromData (pscriptDiscovery'redeemerItemControlHash discovery) #== pconstant ""
                      , pfromData (pscriptDiscovery'purposeCursor discovery)
                          #< pfromData (pscriptSources'purposeCount c)
                      , pfromData (pscriptDiscovery'currentPurposeKind discovery) #>= 0
                      , pfromData (pscriptDiscovery'currentPurposeKind discovery) #<= 3
                      , pfromData (pscriptDiscovery'currentPurposeIndex discovery) #>= 0
                      , plengthBS # pfromData (pscriptDiscovery'currentScriptHash discovery) #== 28
                      , pfromData (pscriptDiscovery'currentSubject discovery) #/= pconstant ""
                      , pfromData (pscriptDiscovery'matchedSourceIndex discovery) #== -1
                      , pfromData (pscriptDiscovery'matchedLanguageTag discovery) #== -1
                      , pfromData (pscriptDiscovery'matchedSourceLeaf discovery) #== pconstant ""
                      ]
                  )
                  ( pif (stage #== 10)
                      ( pand'List
                          [ pfromData (pscriptDiscovery'purposeCursor discovery)
                              #< pfromData (pscriptSources'purposeCount c)
                          , pfromData (pscriptDiscovery'currentPurposeKind discovery) #>= 0
                          , pfromData (pscriptDiscovery'currentPurposeKind discovery) #<= 3
                          , pfromData (pscriptDiscovery'currentPurposeIndex discovery) #>= 0
                          , plengthBS # pfromData (pscriptDiscovery'currentScriptHash discovery) #== 28
                          , pfromData (pscriptDiscovery'currentSubject discovery) #/= pconstant ""
                          , pfromData (pscriptDiscovery'matchedSourceIndex discovery) #>= 0
                          , pfromData (pscriptDiscovery'matchedSourceIndex discovery)
                              #< pfromData (pscriptSources'sourceCount c)
                          , pfromData (pscriptDiscovery'matchedLanguageTag discovery) #== 3
                              #|| pfromData (pscriptDiscovery'matchedLanguageTag discovery) #== 128
                          , plengthBS # pfromData (pscriptDiscovery'matchedSourceLeaf discovery) #== 32
                          ]
                      )
                      ( pand'List
                          [ pif (stage #== 11)
                              (pfromData (pscriptDiscovery'redeemerItemControlHash discovery) #== pconstant "")
                              (pconstant True)
                          , pfromData (pscriptDiscovery'purposeCursor discovery)
                              #== pfromData (pscriptSources'purposeCount c)
                          , discoveryCurrentIsEmpty discovery
                          ]
                      )
                  )
              )
          ]
      )
  where
    discoveryCurrentIsEmpty discovery =
      pand'List
        [ pfromData (pscriptDiscovery'currentPurposeKind discovery) #== -1
        , pfromData (pscriptDiscovery'currentPurposeIndex discovery) #== -1
        , pfromData (pscriptDiscovery'currentScriptHash discovery) #== pconstant ""
        , pfromData (pscriptDiscovery'currentSubject discovery) #== pconstant ""
        , pfromData (pscriptDiscovery'matchedSourceIndex discovery) #== -1
        , pfromData (pscriptDiscovery'matchedLanguageTag discovery) #== -1
        , pfromData (pscriptDiscovery'matchedSourceLeaf discovery) #== pconstant ""
        ]

-- | Aiken @script_sources_control_is_bound@.
pscriptSourcesControlIsBound :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PScriptSourcesControlV1 :--> PBool
    )
pscriptSourcesControlIsBound = phoistAcyclic $ plam $ \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  plet (pfromData $ pscriptSources'stage c) $ \stage ->
  plet (pfromData $ pscriptSources'pendingSourceCbor c) $ \pendingCbor ->
    pand'List
      [ NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (pscriptSources'compactCbor c)
          # pfromData (pscriptSources'witnessSetCompactCbor c)
          # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
          #== pfromData (pmachineState'transactionCommitment preState)
      , phashValidationContext # pfromData (pscriptSources'contextCbor c)
          #== pfromData (pmachineState'validationContextHash preState)
      , plengthBS # pfromData (pscriptSources'resolvedInputsAccumulator c) #== 32
      , plengthBS # pfromData (pscriptSources'signerFrontierCommitment c) #== 32
      , pfromData (pscriptSources'resolvedInputCount c) #>= 0
      , pfromData (pscriptSources'signerCount c) #>= 0
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'sourceCount c) # pfromData (pscriptSources'sourcePeaks c)
      , pfromData (pscriptSources'sourceTotalCount c) #>= pfromData (pscriptSources'sourceCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'redeemerCount c) # pfromData (pscriptSources'redeemerPeaks c)
      , pfromData (pscriptSources'redeemerTotalCount c) #>= pfromData (pscriptSources'redeemerCount c)
      , plengthBS # pfromData (pscriptSources'replayAccumulator c) #== 32
      , plengthBS # pfromData (pscriptSources'replayRemainingScheduleHash c) #== 32
      , pfromData (pscriptSources'replayCursor c) #>= 0
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'replayCursor c) # pfromData (pscriptSources'resolvedItemPeaks c)
      , pfromData (pscriptSources'spendIndex c) #>= 0
      , pfromData (pscriptSources'spendIndex c) #<= pfromData (pscriptSources'replayCursor c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'purposeCount c) # pfromData (pscriptSources'purposePeaks c)
      , pfromData (pscriptSources'outputCursor c) #>= 0
      , pfromData (pscriptSources'outputCursor c) #<= pfromData (pscriptSources'outputCount c)
      , pfrontierIsWellFormed
          # pfromData (pscriptSources'outputCount c) # pfromData (pscriptSources'outputPeaks c)
      , pfromData (pscriptSources'outputTotalCount c) #>= pfromData (pscriptSources'outputCount c)
      , pfromData (pscriptSources'outputTotalCount c) #<= pmaxTxSizeDerivedItemCount
      , pif (stage #== 0)
          ( pif (pendingCbor #== pconstant "")
              (pconstant True)
              ( plet (pdecodeInlineSourceHashControlV1 # pendingCbor) $ \pending ->
                pmatch pending $ \active ->
                  pfromData (pinlineSource'sourceIndex active)
                      #== pfromData (pscriptSources'sourceCount c)
                    #&& ( pfromData (pscriptSources'sourceTotalCount c) #== 0
                            #|| pfromData (pinlineSource'sourceTotalCount active)
                              #== pfromData (pscriptSources'sourceTotalCount c)
                        )
              )
          )
          ( pif (stage #== 1)
              (plengthBS # pendingCbor #== 0 #|| plengthBS # pendingCbor #== 32)
              (pendingCbor #== pconstant "")
          )
      , pif (stage #== 5)
          ( pmatch (pfromData $ pscriptSources'outputProof c) $ \case
              PDNothing -> pconstant True
              PDJust outputProofD ->
                pmatch (pfromData outputProofD) $ \proof ->
                  LedgerOutputProof.pcontrolIsWellFormed # pcon proof
                    #&& pfromData (LedgerOutputProof.pproof'outputIndex proof)
                      #== pfromData (pscriptSources'outputCursor c)
                    #&& pfromData (LedgerOutputProof.pproof'outputIndex proof)
                      #< pfromData (pscriptSources'outputCount c)
          )
          (pfromData (pscriptSources'outputProof c) #== pcon PDNothing)
      , pif (stage #< 4)
          ( pfromData (pscriptSources'outputCount c) #== 0
              #&& pfromData (pscriptSources'outputTotalCount c) #== 0
          )
          ( pif (stage #== 4)
              (pfromData (pscriptSources'outputCursor c) #== 0)
              ( pif (stage #== 5 #|| stage #== 7)
                  (pconstant True)
                  (pfromData (pscriptSources'outputCursor c) #== pfromData (pscriptSources'outputCount c))
              )
          )
      , pif (stage #== 4)
          (pconstant True)
          (pfromData (pscriptSources'outputTotalCount c) #== pfromData (pscriptSources'outputCount c))
      , pscriptSourcesReceiveScanIsWellFormedV1
          # stage # pfromData (pscriptSources'outputCursor c)
          # pfromData (pscriptSources'outputCount c) # pfromData (pscriptSources'receiveScan c)
      , pscriptSourcesObserverScanIsWellFormed # stage # pfromData (pscriptSources'observerScan c)
      , pscriptSourcesMintFoldIsWellFormed # stage # pfromData (pscriptSources'mintFold c)
      , pscriptDiscoveryControlIsWellFormed # pfromData (pscriptSources'discovery c)
      , pscriptSourcesDiscoveryStageIsWellFormed # control
      , pfromData (poneStep'workWitnessCbor stepWitness) #== pscriptSourcesExactControl # control
      ]

-- | One plus the authenticated stage, or zero when it is not routable.  The
-- flag excludes output-proof stage five for the dedicated non-output entry.
pscriptSourcesStageBranchV1 :: forall s.
  Term s (PBool :--> PInteger :--> PInteger)
pscriptSourcesStageBranchV1 = phoistAcyclic $ plam $ \allowOutput stage ->
  pif (stage #< 0 #|| stage #> 12 #|| (pnot # allowOutput #&& stage #== 5))
    0 (stage + 1)

pverifyPreparedScriptSourcesStage :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PInteger :--> PBool
    )
pverifyPreparedScriptSourcesStage = phoistAcyclic $ plam $
  \pre witness auxiliary branch ->
    pif (branch #== 1) (pverifyScriptSourcesStageZeroSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 2) (pverifyScriptSourcesStageOneSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 3) (pverifyScriptSourcesStageTwoSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 4) (pverifyScriptSourcesStageThreeSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 5) (pverifyScriptSourcesStageFourSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 6) (pverifyScriptSourcesStageFiveSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 7) (pverifyScriptSourcesStageSixSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 8) (pverifyScriptSourcesStageSevenSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 9) (pverifyScriptSourcesStageEightSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 10) (pverifyScriptSourcesStageNineSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 11) (pverifyScriptSourcesStageTenSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 12) (pverifyScriptSourcesStageElevenSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 13) (pverifyScriptSourcesStageTwelveSemanticsV1 # pre # witness # auxiliary) $
    pconstant False

-- The Aiken non-output entry point excludes stage five before dispatch. Keep
-- that branch out of the Plutarch term entirely instead of carrying it behind
-- a runtime flag.
pverifyPreparedScriptSourcesNonOutputStage :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PInteger :--> PBool
    )
pverifyPreparedScriptSourcesNonOutputStage = phoistAcyclic $ plam $
  \pre witness auxiliary branch ->
    pif (branch #== 1) (pverifyScriptSourcesStageZeroSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 2) (pverifyScriptSourcesStageOneSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 3) (pverifyScriptSourcesStageTwoSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 4) (pverifyScriptSourcesStageThreeSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 5) (pverifyScriptSourcesStageFourSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 7) (pverifyScriptSourcesStageSixSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 8) (pverifyScriptSourcesStageSevenSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 9) (pverifyScriptSourcesStageEightSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 10) (pverifyScriptSourcesStageNineSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 11) (pverifyScriptSourcesStageTenSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 12) (pverifyScriptSourcesStageElevenSemanticsV1 # pre # witness # auxiliary) $
    pif (branch #== 13) (pverifyScriptSourcesStageTwelveSemanticsV1 # pre # witness # auxiliary) $
    pconstant False

pverifyScriptSourcesWithOutput :: forall s.
  Term s
    ( PBool :--> PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSourcesWithOutput = phoistAcyclic $ plam $
  \allowOutput pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  plet (pscriptSourcesStageBranchV1 # allowOutput # pfromData (pscriptSources'stage c)) $ \branch ->
    pverified'version verified #== 1
      #&& pscriptSourcesControlIsBound # pre # witness # control
      #&& pverifyPreparedScriptSourcesStage # pre # witness # auxiliary # branch

-- | Aiken @verify_script_sources_non_output_semantics_v1@.
pverifyScriptSourcesNonOutputSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyScriptSourcesNonOutputSemanticsV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transitionD auxiliaryD) ->
  plet (pfromData transitionD) $ \witness ->
  plet (pdecodeValidationAuxiliaryWitnessV1 auxiliaryD) $ \auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pscriptSourcesControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pscriptSources'compactCbor c)
        # pfromData (pscriptSources'witnessSetCompactCbor c)
        # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
  plet (pfromData $ pscriptSources'stage c) $ \stage ->
  plet (pif (stage #< 0 #|| stage #> 12 #|| stage #== 5) 0 (stage + 1)) $ \branch ->
    pverified'version verified #== 1
      #&& pscriptSourcesControlIsBound # pre # witness # control
      #&& pverifyPreparedScriptSourcesNonOutputStage # pre # witness # auxiliary # branch

-- | Aiken @verify_script_sources@.
pverifyScriptSources :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptSources = phoistAcyclic $ plam $ \pre witness auxiliary ->
  pverifyScriptSourcesWithOutput # pconstant True # pre # witness # auxiliary

-- | The common one-step envelope after ScriptSources semantics have been
-- evaluated.  Splitting this predicate also keeps structural-envelope tests
-- independent from the size of the thirteen-stage semantic dispatcher.
pverifyPreparedScriptSourcesOneStepV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PBool :--> PBool
    )
pverifyPreparedScriptSourcesOneStepV1 = phoistAcyclic $ plam $
  \pre witness semanticsAreValid ->
  pmatch pre $ \preState ->
    pfromData (pmachineState'phase preState) #== pcon PScriptSources
      #&& pstructuralTransitionIsValid # pre # witness
      #&& semanticsAreValid

-- | Aiken @verify_script_sources_one_step_v1@.
pverifyScriptSourcesOneStepV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyScriptSourcesOneStepV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transitionD auxiliaryD) ->
  plet (pfromData transitionD) $ \witness ->
  plet (pverifyScriptSources # pre # witness # pdecodeValidationAuxiliaryWitnessV1 auxiliaryD) $ \semanticsAreValid ->
    pverifyPreparedScriptSourcesOneStepV1 # pre # witness # semanticsAreValid

pledgerDeltaSetPending :: forall s.
  Term s (PLedgerDeltaControlV1 :--> PByteString :--> PLedgerDeltaControlV1)
pledgerDeltaSetPending = phoistAcyclic $ plam $ \control pendingCbor ->
  pmatch control $ \c -> pcon $ PLedgerDeltaControlV1
    (pledgerDelta'resolvedInputCount c)
    (pledgerDelta'resolvedInputsAccumulator c)
    (pledgerDelta'outputCount c)
    (pledgerDelta'outputDescriptorPeaks c)
    (pledgerDelta'stage c)
    (pledgerDelta'replayScheduleHash c)
    (pledgerDelta'replayCursor c)
    (pledgerDelta'replayAccumulator c)
    (pledgerDelta'replayRemainingScheduleHash c)
    (pledgerDelta'currentLedgerRoot c)
    (pledgerDelta'outputCursor c)
    (pledgerDelta'operationCount c)
    (pledgerDelta'operationPeaks c)
    (pdata pendingCbor)

pledgerDeltaSetStage :: forall s.
  Term s (PLedgerDeltaControlV1 :--> PInteger :--> PLedgerDeltaControlV1)
pledgerDeltaSetStage = phoistAcyclic $ plam $ \control stage ->
  pmatch control $ \c -> pcon $ PLedgerDeltaControlV1
    (pledgerDelta'resolvedInputCount c)
    (pledgerDelta'resolvedInputsAccumulator c)
    (pledgerDelta'outputCount c)
    (pledgerDelta'outputDescriptorPeaks c)
    (pdata stage)
    (pledgerDelta'replayScheduleHash c)
    (pledgerDelta'replayCursor c)
    (pledgerDelta'replayAccumulator c)
    (pledgerDelta'replayRemainingScheduleHash c)
    (pledgerDelta'currentLedgerRoot c)
    (pledgerDelta'outputCursor c)
    (pledgerDelta'operationCount c)
    (pledgerDelta'operationPeaks c)
    (pledgerDelta'pendingMutationCbor c)

pledgerDeltaAdvanceReplay :: forall s.
  Term s
    ( PLedgerDeltaControlV1 :--> PByteString :--> PByteString :--> PByteString
        :--> PLedgerDeltaControlV1
    )
pledgerDeltaAdvanceReplay = phoistAcyclic $ plam $ \control nextAccumulator nextScheduleHash pendingCbor ->
  pmatch control $ \c -> pcon $ PLedgerDeltaControlV1
    (pledgerDelta'resolvedInputCount c)
    (pledgerDelta'resolvedInputsAccumulator c)
    (pledgerDelta'outputCount c)
    (pledgerDelta'outputDescriptorPeaks c)
    (pledgerDelta'stage c)
    (pledgerDelta'replayScheduleHash c)
    (pdata $ pfromData (pledgerDelta'replayCursor c) + 1)
    (pdata nextAccumulator)
    (pdata nextScheduleHash)
    (pledgerDelta'currentLedgerRoot c)
    (pledgerDelta'outputCursor c)
    (pledgerDelta'operationCount c)
    (pledgerDelta'operationPeaks c)
    (pdata pendingCbor)

pledgerDeltaAdvanceOutput :: forall s.
  Term s (PLedgerDeltaControlV1 :--> PByteString :--> PLedgerDeltaControlV1)
pledgerDeltaAdvanceOutput = phoistAcyclic $ plam $ \control pendingCbor ->
  pmatch control $ \c -> pcon $ PLedgerDeltaControlV1
    (pledgerDelta'resolvedInputCount c)
    (pledgerDelta'resolvedInputsAccumulator c)
    (pledgerDelta'outputCount c)
    (pledgerDelta'outputDescriptorPeaks c)
    (pledgerDelta'stage c)
    (pledgerDelta'replayScheduleHash c)
    (pledgerDelta'replayCursor c)
    (pledgerDelta'replayAccumulator c)
    (pledgerDelta'replayRemainingScheduleHash c)
    (pledgerDelta'currentLedgerRoot c)
    (pdata $ pfromData (pledgerDelta'outputCursor c) + 1)
    (pledgerDelta'operationCount c)
    (pledgerDelta'operationPeaks c)
    (pdata pendingCbor)

pledgerDeltaCompleteMutation :: forall s.
  Term s
    ( PLedgerDeltaControlV1 :--> PByteString
        :--> PBuiltinList (PAsData PFrontierPeak) :--> PLedgerDeltaControlV1
    )
pledgerDeltaCompleteMutation = phoistAcyclic $ plam $ \control nextRoot nextPeaks ->
  pmatch control $ \c -> pcon $ PLedgerDeltaControlV1
    (pledgerDelta'resolvedInputCount c)
    (pledgerDelta'resolvedInputsAccumulator c)
    (pledgerDelta'outputCount c)
    (pledgerDelta'outputDescriptorPeaks c)
    (pledgerDelta'stage c)
    (pledgerDelta'replayScheduleHash c)
    (pledgerDelta'replayCursor c)
    (pledgerDelta'replayAccumulator c)
    (pledgerDelta'replayRemainingScheduleHash c)
    (pdata nextRoot)
    (pledgerDelta'outputCursor c)
    (pdata $ pfromData (pledgerDelta'operationCount c) + 1)
    (pdata nextPeaks)
    (pdata $ pconstant "")

-- | Aiken @ledger_delta_successor_is_exact@.
pledgerDeltaSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PLedgerDeltaControlV1 :--> PBool
    )
pledgerDeltaSuccessorIsExact = phoistAcyclic $ plam $ \pre witness nextControl ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch (pfromData $ poneStep'claimedSuccessor stepWitness) $ \post ->
    pfromData (pmachineState'phase post) #== pcon PLedgerDelta
      #&& pfromData (pmachineState'workRoot post)
        #== phashWorkWitness
          # pcon PLedgerDelta
          # (pfromData (pmachineState'programCounter preState) + 1)
          # (pencodeLedgerDeltaControlV1 # nextControl)

-- | Aiken @ledger_delta_operation_proof_is_valid@.
pledgerDeltaOperationProofIsValid :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PLedgerDeltaControlV1 :--> PInteger
        :--> PByteString :--> PByteString :--> PLedgerDeltaOperationProofV1 :--> PBool
    )
pledgerDeltaOperationProofIsValid = phoistAcyclic $ plam $ \pre control operationKind key value proof ->
  pmatch pre $ \preState ->
  pmatch control $ \c ->
  pmatch proof $ \p ->
  plet (pfromData $ pledgerOperationProof'operationCount p) $ \operationCount ->
  plet (pfromData $ pledgerOperationProof'operationIndex p) $ \operationIndex ->
  plet (pfromData $ pledgerOperationProof'operationPeaks p) $ \operationPeaks ->
  pif
    ( pand'List
        [ operationCount #>= 0
        , operationCount #<= pmaxTxSizeDerivedItemCount
        , pfrontierIsWellFormed # operationCount # operationPeaks
        , operationIndex #== pfromData (pledgerDelta'operationCount c)
        , operationIndex #>= 0
        , operationIndex #< operationCount
        ]
    )
    ( pfrontierCommitment # operationCount # operationPeaks
        #== pfromData (pmachineState'ledgerDeltaRoot preState)
        #&& pverifyMembership
          # operationCount
          # operationPeaks
          # operationIndex
          # ( pledgerDeltaOperationLeafHash
                # operationKind # key # value
                # pfromData (pledgerOperationProof'descriptor p)
            )
          # pfromData (pledgerOperationProof'operationSiblings p)
    )
    (pconstant False)

-- | Aiken @ledger_delta_operation_step@.
pledgerDeltaOperationStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PLedgerDeltaControlV1 :--> PBool
    )
pledgerDeltaOperationStep = phoistAcyclic $ plam $ \pre witness auxiliary control ->
  pmatch pre $ \preState ->
  pmatch control $ \c ->
  pmatch auxiliary $ \case
    PLedgerDeltaOperationWitness operationKindD keyD valueD operationProofD ->
      plet (pfromData operationKindD) $ \operationKind ->
      plet (pfromData keyD) $ \key ->
      plet (pfromData valueD) $ \value ->
      plet (pfromData operationProofD) $ \operationProof ->
      pmatch operationProof $ \operation ->
      plet
        ( pencodeMidgardTxInput
            # pcon (PMidgardTxInput
                (pmachineState'transactionId preState)
                (pledgerDelta'outputCursor c))
        )
        $ \expectedOutputKey ->
      pif
        ( pfromData (pledgerDelta'pendingMutationCbor c) #== pconstant ""
            #&& pif
              (pfromData (pledgerDelta'stage c) #== 0)
              (operationKind #== 0 #&& value #== pconstant "")
              ( pfromData (pledgerDelta'stage c) #== 1
                  #&& operationKind #== 1
                  #&& key #== expectedOutputKey
              )
        )
        ( pif
            ( pledgerDeltaOperationProofIsValid
                # pre # control # operationKind # key # value # operationProof
            )
            ( plet
                ( pcon $ ProofFold.PProofFoldControlV1
                    (pdata (-1)) (pdata 0) (pdata $ pconstant "") (pdata $ pconstant "")
                )
                $ \emptyFold ->
              plet
                ( pcon $ PLedgerDeltaPendingMutationV1
                    (pdata 0) (pdata operationKind) (pdata key) (pdata value)
                    (pledgerOperationProof'descriptor operation)
                    (pdata emptyFold)
                )
                $ \pending ->
              pledgerDeltaSuccessorIsExact
                # pre # witness
                # (pledgerDeltaSetPending # control # (pencodeLedgerDeltaPendingMutationV1 # pending))
            )
            (pconstant False)
        )
        (pconstant False)
    _ -> perror

-- | Aiken @ledger_delta_pending_step@.
pledgerDeltaPendingStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PLedgerDeltaControlV1 :--> PBool
    )
pledgerDeltaPendingStep = phoistAcyclic $ plam $ \pre witness auxiliary control ->
  pmatch control $ \c ->
  plet (pledgerDeltaPendingMutationFromCbor # pfromData (pledgerDelta'pendingMutationCbor c)) $ \pending ->
  pmatch pending $ \mutation ->
  plet (pfromData $ ppendingMutation'foldControl mutation) $ \foldControl ->
  pif (pfromData (ppendingMutation'authorizationStage mutation) #== 1)
    ( pif (ProofFold.pfoldIsCompleteV1 # foldControl)
        ( pmatch foldControl $ \fold ->
          plet
            ( pif (pfromData (ppendingMutation'operationKind mutation) #== 0)
                ( pcon $ PPair
                    (pfromData $ ProofFold.pfoldControl'includingRoot fold)
                    (pfromData $ ProofFold.pfoldControl'excludingRoot fold)
                )
                ( pcon $ PPair
                    (pfromData $ ProofFold.pfoldControl'excludingRoot fold)
                    (pfromData $ ProofFold.pfoldControl'includingRoot fold)
                )
            )
            $ \roots ->
          pmatch roots $ \(PPair expectedCurrentRoot nextRoot) ->
          plet
            ( pif (pfromData (ppendingMutation'operationKind mutation) #== 0)
                (pconstant "")
                (pfromData $ ppendingMutation'value mutation)
            )
            $ \operationValue ->
          plet
            ( pappendLeaf
                # pfromData (pledgerDelta'operationCount c)
                # pfromData (pledgerDelta'operationPeaks c)
                # ( pledgerDeltaOperationLeafHash
                      # pfromData (ppendingMutation'operationKind mutation)
                      # pfromData (ppendingMutation'key mutation)
                      # operationValue
                      # pfromData (ppendingMutation'descriptor mutation)
                  )
            )
            $ \nextPeaks ->
            pand'List
              [ pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False
              , pfromData (pledgerDelta'currentLedgerRoot c) #== expectedCurrentRoot
              , pledgerDeltaSuccessorIsExact
                  # pre # witness
                  # (pledgerDeltaCompleteMutation # control # nextRoot # nextPeaks)
              ]
        )
        ( pmatch auxiliary $ \case
            PLedgerDeltaProofFrameWitness frameD siblingsD ->
              pmatch
                ( ProofFold.pfoldProofFrameV1
                    # pfromData (ppendingMutation'key mutation)
                    # pfromData (ppendingMutation'descriptor mutation)
                    # foldControl
                    # pfromData frameD
                    # pfromData siblingsD
                )
                $ \case
                  PNothing -> pconstant False
                  PJust nextFoldControl ->
                    plet
                      ( pcon $ PLedgerDeltaPendingMutationV1
                          (ppendingMutation'authorizationStage mutation)
                          (ppendingMutation'operationKind mutation)
                          (ppendingMutation'key mutation)
                          (ppendingMutation'value mutation)
                          (ppendingMutation'descriptor mutation)
                          (pdata nextFoldControl)
                      )
                      $ \nextPending ->
                      pledgerDeltaSuccessorIsExact
                        # pre # witness
                        # ( pledgerDeltaSetPending # control
                              # (pencodeLedgerDeltaPendingMutationV1 # nextPending)
                          )
            _ -> perror
        )
    )
    perror

-- | Aiken @ledger_delta_replay_step@.
pledgerDeltaReplayStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PLedgerDeltaControlV1 :--> PBool
    )
pledgerDeltaReplayStep = phoistAcyclic $ plam $ \pre witness auxiliary control ->
  pmatch control $ \c ->
  pmatch auxiliary $ \case
    PLedgerDeltaReplayWitness sourceKindD keyD nextScheduleHashD valueD ->
      plet (pfromData sourceKindD) $ \sourceKind ->
      plet (pfromData keyD) $ \key ->
      plet (pfromData nextScheduleHashD) $ \nextScheduleHash ->
      plet (pfromData valueD) $ \value ->
      plet (OutputCommitment.pdecodeLedgerOutputCommitment # value) $ \descriptor ->
      pmatch descriptor $ \outputDescriptor ->
      plet (pdecodeMidgardTxInputCbor # key) $ \input ->
      pmatch input $ \txInput ->
      pif
        ( pand'List
            [ sourceKind #== 0 #|| sourceKind #== 1
            , presolutionScheduleNodeHash # sourceKind # key # nextScheduleHash
                #== pfromData (pledgerDelta'replayRemainingScheduleHash c)
            , pfromData (pledgerDelta'replayCursor c)
                #< pfromData (pledgerDelta'resolvedInputCount c)
            , pfromData (ptxInput'outputIndex txInput)
                #== pfromData (OutputCommitment.poutputCommitment'outputIndex outputDescriptor)
            ]
        )
        ( plet
            ( presolvedInputAccumulatorSuccessor
                # pfromData (pledgerDelta'replayAccumulator c)
                # sourceKind # key # value
            )
            $ \nextAccumulator ->
          pif (sourceKind #== 1)
            ( pif (pfromData (pledgerDelta'pendingMutationCbor c) #== pconstant "")
                ( pledgerDeltaSuccessorIsExact
                    # pre # witness
                    # ( pledgerDeltaAdvanceReplay # control # nextAccumulator
                          # nextScheduleHash # pconstant ""
                      )
                )
                (pconstant False)
            )
            ( pif (pfromData (pledgerDelta'pendingMutationCbor c) #/= pconstant "")
                ( plet
                    ( pledgerDeltaPendingMutationFromCbor
                        # pfromData (pledgerDelta'pendingMutationCbor c)
                    )
                    $ \pending ->
                  pmatch pending $ \mutation ->
                  pmatch
                    ( ProofFold.pinitialFoldControlV1
                        # key # value # pfromData (ppendingMutation'descriptor mutation)
                    )
                    $ \case
                      PNothing -> pconstant False
                      PJust foldControl ->
                        pif
                          ( pand'List
                              [ pfromData (ppendingMutation'authorizationStage mutation) #== 0
                              , pfromData (ppendingMutation'operationKind mutation) #== 0
                              , pfromData (ppendingMutation'key mutation) #== key
                              , pfromData (ppendingMutation'value mutation) #== pconstant ""
                              ]
                          )
                          ( plet
                              ( pcon $ PLedgerDeltaPendingMutationV1
                                  (pdata 1)
                                  (ppendingMutation'operationKind mutation)
                                  (ppendingMutation'key mutation)
                                  (pdata value)
                                  (ppendingMutation'descriptor mutation)
                                  (pdata foldControl)
                              )
                              $ \nextPending ->
                              pledgerDeltaSuccessorIsExact
                                # pre # witness
                                # ( pledgerDeltaAdvanceReplay # control # nextAccumulator
                                      # nextScheduleHash
                                      # (pencodeLedgerDeltaPendingMutationV1 # nextPending)
                                  )
                          )
                          (pconstant False)
                )
                perror
            )
        )
        (pconstant False)
    _ -> perror

-- | Aiken @ledger_delta_stage_one@ (stage zero in the encoded control).
pledgerDeltaStageOne :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PLedgerDeltaControlV1 :--> PBool
    )
pledgerDeltaStageOne = phoistAcyclic $ plam $ \pre witness auxiliary control ->
  pmatch control $ \c ->
  pif (pfromData (pledgerDelta'pendingMutationCbor c) #/= pconstant "")
    ( plet
        ( pledgerDeltaPendingMutationFromCbor
            # pfromData (pledgerDelta'pendingMutationCbor c)
        )
        $ \pending ->
      pmatch pending $ \mutation ->
      pif (pfromData (ppendingMutation'authorizationStage mutation) #== 0)
        (pledgerDeltaReplayStep # pre # witness # auxiliary # control)
        (pledgerDeltaPendingStep # pre # witness # auxiliary # control)
    )
    ( pif
        (pfromData (pledgerDelta'replayRemainingScheduleHash c) #== pemptyResolutionScheduleHash)
        ( pand'List
            [ pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False
            , pfromData (pledgerDelta'replayCursor c)
                #== pfromData (pledgerDelta'resolvedInputCount c)
            , pfromData (pledgerDelta'replayAccumulator c)
                #== pfromData (pledgerDelta'resolvedInputsAccumulator c)
            , pledgerDeltaSuccessorIsExact
                # pre # witness # (pledgerDeltaSetStage # control # 1)
            ]
        )
        (pledgerDeltaReplayStep # pre # witness # auxiliary # control)
    )

-- | Aiken @ledger_delta_output_step@.
pledgerDeltaOutputStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PLedgerDeltaControlV1 :--> PBool
    )
pledgerDeltaOutputStep = phoistAcyclic $ plam $ \pre witness auxiliary control ->
  pmatch pre $ \preState ->
  pmatch control $ \c ->
  pmatch auxiliary $ \case
    PLedgerDeltaOutputWitness outputIndexD descriptorCborD siblingsD ->
      plet (pfromData outputIndexD) $ \outputIndex ->
      plet (pfromData descriptorCborD) $ \descriptorCbor ->
      plet
        ( pencodeMidgardTxInput
            # pcon (PMidgardTxInput
                (pmachineState'transactionId preState)
                (pdata outputIndex))
        )
        $ \key ->
      plet (OutputCommitment.pdecodeLedgerOutputCommitment # descriptorCbor) $ \descriptor ->
      pmatch descriptor $ \outputDescriptor ->
      pif (pfromData (pledgerDelta'pendingMutationCbor c) #/= pconstant "")
        ( plet
            ( pledgerDeltaPendingMutationFromCbor
                # pfromData (pledgerDelta'pendingMutationCbor c)
            )
            $ \pending ->
          pmatch pending $ \mutation ->
          pmatch
            ( ProofFold.pinitialFoldControlV1
                # key # descriptorCbor # pfromData (ppendingMutation'descriptor mutation)
            )
            $ \case
              PNothing -> pconstant False
              PJust foldControl ->
                pif
                  ( pand'List
                      [ outputIndex #== pfromData (pledgerDelta'outputCursor c)
                      , pfromData (OutputCommitment.poutputCommitment'outputIndex outputDescriptor)
                          #== outputIndex
                      , pfromData (ppendingMutation'authorizationStage mutation) #== 0
                      , pfromData (ppendingMutation'operationKind mutation) #== 1
                      , pfromData (ppendingMutation'key mutation) #== key
                      , pfromData (ppendingMutation'value mutation) #== descriptorCbor
                      , pverifyMembership
                          # pfromData (pledgerDelta'outputCount c)
                          # pfromData (pledgerDelta'outputDescriptorPeaks c)
                          # outputIndex
                          # (ScriptProof.poutputDescriptorLeafHash # outputIndex # descriptorCbor)
                          # pfromData siblingsD
                      ]
                  )
                  ( plet
                      ( pcon $ PLedgerDeltaPendingMutationV1
                          (pdata 1)
                          (ppendingMutation'operationKind mutation)
                          (ppendingMutation'key mutation)
                          (ppendingMutation'value mutation)
                          (ppendingMutation'descriptor mutation)
                          (pdata foldControl)
                      )
                      $ \nextPending ->
                      pledgerDeltaSuccessorIsExact
                        # pre # witness
                        # ( pledgerDeltaAdvanceOutput # control
                              # (pencodeLedgerDeltaPendingMutationV1 # nextPending)
                          )
                  )
                  (pconstant False)
        )
        perror
    _ -> perror

-- | Aiken @ledger_delta_stage_two@ (stage one in the encoded control).
pledgerDeltaStageTwo :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PLedgerDeltaControlV1 :--> PBool
    )
pledgerDeltaStageTwo = phoistAcyclic $ plam $ \pre witness auxiliary control ->
  pmatch control $ \c ->
  pif (pfromData (pledgerDelta'pendingMutationCbor c) #/= pconstant "")
    ( plet
        ( pledgerDeltaPendingMutationFromCbor
            # pfromData (pledgerDelta'pendingMutationCbor c)
        )
        $ \pending ->
      pmatch pending $ \mutation ->
      pif (pfromData (ppendingMutation'authorizationStage mutation) #== 1)
        (pledgerDeltaPendingStep # pre # witness # auxiliary # control)
        (pledgerDeltaOutputStep # pre # witness # auxiliary # control)
    )
    ( pif (pfromData (pledgerDelta'outputCursor c) #== pfromData (pledgerDelta'outputCount c))
        ( pand'List
            [ pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False
            , pledgerDeltaSuccessorIsExact
                # pre # witness # (pledgerDeltaSetStage # control # 2)
            ]
        )
        (pledgerDeltaOutputStep # pre # witness # auxiliary # control)
    )

-- | Aiken @ledger_delta_stage_three@ (stage two in the encoded control).
pledgerDeltaStageThree :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PLedgerDeltaControlV1 :--> PBool
    )
pledgerDeltaStageThree = phoistAcyclic $ plam $ \pre witness auxiliary control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch (pfromData $ poneStep'claimedSuccessor stepWitness) $ \post ->
  pmatch control $ \c ->
    pand'List
      [ pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False
      , pfromData (pledgerDelta'replayCursor c)
          #== pfromData (pledgerDelta'resolvedInputCount c)
      , pfromData (pledgerDelta'replayAccumulator c)
          #== pfromData (pledgerDelta'resolvedInputsAccumulator c)
      , pfromData (pledgerDelta'replayRemainingScheduleHash c) #== pemptyResolutionScheduleHash
      , pfromData (pledgerDelta'outputCursor c) #== pfromData (pledgerDelta'outputCount c)
      , pfromData (pledgerDelta'pendingMutationCbor c) #== pconstant ""
      , pfrontierCommitment
          # pfromData (pledgerDelta'operationCount c)
          # pfromData (pledgerDelta'operationPeaks c)
          #== pfromData (pmachineState'ledgerDeltaRoot preState)
      , pfromData (pmachineState'phase post) #== pcon PTerminal
      , pfromData (pmachineState'verdict post) #== pcon PAccepted
      , pfromData (pmachineState'rejectionCodeHash post)
          #== preplicateBS # 32 # (pintegerToByte # 0)
      , pfromData (pmachineState'workRoot post)
          #== phashWorkWitness
            # pcon PTerminal
            # (pfromData (pmachineState'programCounter preState) + 1)
            # ( pencodeTerminalAcceptanceWitnessV1
                  # pfromData (pledgerDelta'currentLedgerRoot c)
                  # pfromData (pledgerDelta'operationCount c)
                  # pfromData (pledgerDelta'operationPeaks c)
              )
      ]

-- | Shared well-formedness gate used by the monolithic and split verifiers.
pledgerDeltaControlIsWellFormed :: forall s.
  Term s (PValidationOneStepWitnessV1 :--> PLedgerDeltaControlV1 :--> PBool)
pledgerDeltaControlIsWellFormed = phoistAcyclic $ plam $ \witness control ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  plet
    ( pand'List
        [ pfromData (pledgerDelta'resolvedInputCount c) #>= 0
        , pfromData (pledgerDelta'resolvedInputCount c) #<= pmaxTxSizeDerivedItemCount
        , plengthBS # pfromData (pledgerDelta'resolvedInputsAccumulator c) #== 32
        , pfromData (pledgerDelta'outputCount c) #>= 0
        , pfromData (pledgerDelta'outputCount c) #<= pmaxTxSizeDerivedItemCount
        , pfrontierIsWellFormed
            # pfromData (pledgerDelta'outputCount c)
            # pfromData (pledgerDelta'outputDescriptorPeaks c)
        , plengthBS # pfromData (pledgerDelta'replayScheduleHash c) #== 32
        , plengthBS # pfromData (pledgerDelta'replayAccumulator c) #== 32
        , plengthBS # pfromData (pledgerDelta'replayRemainingScheduleHash c) #== 32
        , plengthBS # pfromData (pledgerDelta'currentLedgerRoot c) #== 32
        , pfromData (pledgerDelta'replayCursor c) #>= 0
        , pfromData (pledgerDelta'replayCursor c)
            #<= pfromData (pledgerDelta'resolvedInputCount c)
        , pfromData (pledgerDelta'outputCursor c) #>= 0
        , pfromData (pledgerDelta'outputCursor c) #<= pfromData (pledgerDelta'outputCount c)
        , pfromData (pledgerDelta'operationCount c) #>= 0
        , pfromData (pledgerDelta'operationCount c) #<= pmaxTxSizeDerivedItemCount
        , pfrontierIsWellFormed
            # pfromData (pledgerDelta'operationCount c)
            # pfromData (pledgerDelta'operationPeaks c)
        , pfromData (pledgerDelta'stage c) #>= 0
        , pfromData (pledgerDelta'stage c) #<= 2
        ]
    )
    $ \shapeIsValid ->
      pif shapeIsValid
        ( pfromData (poneStep'workWitnessCbor stepWitness)
            #== pencodeLedgerDeltaControlV1 # control
        )
        (pconstant False)

-- | Aiken @verify_ledger_delta@.
pverifyLedgerDelta :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyLedgerDelta = phoistAcyclic $ plam $ \pre witness auxiliary ->
  pmatch witness $ \stepWitness ->
  plet (pledgerDeltaControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness)) $ \control ->
  pmatch control $ \c ->
  pif (pledgerDeltaControlIsWellFormed # witness # control)
    ( pmatch auxiliary $ \case
        PLedgerDeltaOperationWitness {} ->
          pledgerDeltaOperationStep # pre # witness # auxiliary # control
        _ ->
          pif (pfromData (pledgerDelta'stage c) #== 0)
            (pledgerDeltaStageOne # pre # witness # auxiliary # control) $
          pif (pfromData (pledgerDelta'stage c) #== 1)
            (pledgerDeltaStageTwo # pre # witness # auxiliary # control)
            (pledgerDeltaStageThree # pre # witness # auxiliary # control)
    )
    (pconstant False)

pverifyLedgerDeltaReplayFinishSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyLedgerDeltaReplayFinishSemanticsV1 = phoistAcyclic $ plam $ \pre witness ->
  pmatch witness $ \stepWitness ->
  plet (pledgerDeltaControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness)) $ \control ->
  pmatch control $ \c ->
    pand'List
      [ pledgerDeltaControlIsWellFormed # witness # control
      , pfromData (pledgerDelta'pendingMutationCbor c) #== pconstant ""
      , pfromData (pledgerDelta'stage c) #== 0
      , pfromData (pledgerDelta'replayRemainingScheduleHash c) #== pemptyResolutionScheduleHash
      , pfromData (pledgerDelta'replayCursor c)
          #== pfromData (pledgerDelta'resolvedInputCount c)
      , pfromData (pledgerDelta'replayAccumulator c)
          #== pfromData (pledgerDelta'resolvedInputsAccumulator c)
      , pledgerDeltaSuccessorIsExact # pre # witness # (pledgerDeltaSetStage # control # 1)
      ]

pverifyLedgerDeltaOutputFinishSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyLedgerDeltaOutputFinishSemanticsV1 = phoistAcyclic $ plam $ \pre witness ->
  pmatch witness $ \stepWitness ->
  plet (pledgerDeltaControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness)) $ \control ->
  pmatch control $ \c ->
    pand'List
      [ pledgerDeltaControlIsWellFormed # witness # control
      , pfromData (pledgerDelta'pendingMutationCbor c) #== pconstant ""
      , pfromData (pledgerDelta'stage c) #== 1
      , pfromData (pledgerDelta'outputCursor c) #== pfromData (pledgerDelta'outputCount c)
      , pledgerDeltaSuccessorIsExact # pre # witness # (pledgerDeltaSetStage # control # 2)
      ]

pverifyLedgerDeltaTerminalSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyLedgerDeltaTerminalSemanticsV1 = phoistAcyclic $ plam $ \pre witness ->
  pmatch witness $ \stepWitness ->
  plet (pledgerDeltaControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness)) $ \control ->
  pmatch control $ \c ->
    pand'List
      [ pledgerDeltaControlIsWellFormed # witness # control
      , pfromData (pledgerDelta'pendingMutationCbor c) #== pconstant ""
      , pfromData (pledgerDelta'stage c) #== 2
      , pledgerDeltaStageThree # pre # witness # pcon PNoAuxiliaryWitness # control
      ]

pverifyLedgerDeltaOperationSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PInteger
        :--> PByteString :--> PByteString :--> PLedgerDeltaOperationProofV1 :--> PBool
    )
pverifyLedgerDeltaOperationSemanticsV1 = phoistAcyclic $ plam $ \pre witness operationKind key value proof ->
  pmatch witness $ \stepWitness ->
  plet (pledgerDeltaControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness)) $ \control ->
  pmatch control $ \c ->
    pand'List
      [ pledgerDeltaControlIsWellFormed # witness # control
      , pfromData (pledgerDelta'pendingMutationCbor c) #== pconstant ""
      , pfromData (pledgerDelta'stage c) #== 0 #|| pfromData (pledgerDelta'stage c) #== 1
      , pledgerDeltaOperationStep # pre # witness
          # pcon (PLedgerDeltaOperationWitness
              (pdata operationKind) (pdata key) (pdata value) (pdata proof))
          # control
      ]

pverifyLedgerDeltaReplaySemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PInteger
        :--> PByteString :--> PByteString :--> PByteString :--> PBool
    )
pverifyLedgerDeltaReplaySemanticsV1 = phoistAcyclic $ plam $ \pre witness sourceKind key nextScheduleHash value ->
  pmatch witness $ \stepWitness ->
  plet (pledgerDeltaControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness)) $ \control ->
  pmatch control $ \c ->
    pand'List
      [ pledgerDeltaControlIsWellFormed # witness # control
      , pfromData (pledgerDelta'stage c) #== 0
      , pfromData (pledgerDelta'replayRemainingScheduleHash c) #/= pemptyResolutionScheduleHash
      , pif (sourceKind #== 0)
          (pfromData (pledgerDelta'pendingMutationCbor c) #/= pconstant "")
          (pfromData (pledgerDelta'pendingMutationCbor c) #== pconstant "")
      , pledgerDeltaReplayStep # pre # witness
          # pcon (PLedgerDeltaReplayWitness
              (pdata sourceKind) (pdata key) (pdata nextScheduleHash) (pdata value))
          # control
      ]

pverifyLedgerDeltaOutputSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PInteger
        :--> PByteString :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pverifyLedgerDeltaOutputSemanticsV1 = phoistAcyclic $ plam $ \pre witness outputIndex descriptorCbor siblings ->
  pmatch witness $ \stepWitness ->
  plet (pledgerDeltaControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness)) $ \control ->
  pmatch control $ \c ->
    pand'List
      [ pledgerDeltaControlIsWellFormed # witness # control
      , pfromData (pledgerDelta'stage c) #== 1
      , pfromData (pledgerDelta'pendingMutationCbor c) #/= pconstant ""
      , pfromData (pledgerDelta'outputCursor c) #< pfromData (pledgerDelta'outputCount c)
      , pledgerDeltaOutputStep # pre # witness
          # pcon (PLedgerDeltaOutputWitness
              (pdata outputIndex) (pdata descriptorCbor) (pdata siblings))
          # control
      ]

pverifyLedgerDeltaProofFrameSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> ProofFold.PProofFrameV1 :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pverifyLedgerDeltaProofFrameSemanticsV1 = phoistAcyclic $ plam $ \pre witness frame siblings ->
  pmatch witness $ \stepWitness ->
  plet (pledgerDeltaControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness)) $ \control ->
  pmatch control $ \c ->
  pif (pfromData (pledgerDelta'pendingMutationCbor c) #/= pconstant "")
    ( plet
        ( pledgerDeltaPendingMutationFromCbor
            # pfromData (pledgerDelta'pendingMutationCbor c)
        )
        $ \pending ->
      pmatch pending $ \mutation ->
        pand'List
          [ pledgerDeltaControlIsWellFormed # witness # control
          , pfromData (pledgerDelta'stage c) #== 0 #|| pfromData (pledgerDelta'stage c) #== 1
          , pfromData (ppendingMutation'authorizationStage mutation) #== 1
          , pnot #$ ProofFold.pfoldIsCompleteV1 # pfromData (ppendingMutation'foldControl mutation)
          , pledgerDeltaPendingStep # pre # witness
              # pcon (PLedgerDeltaProofFrameWitness (pdata frame) (pdata siblings))
              # control
          ]
    )
    perror

pverifyLedgerDeltaFinalizeSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyLedgerDeltaFinalizeSemanticsV1 = phoistAcyclic $ plam $ \pre witness ->
  pmatch witness $ \stepWitness ->
  plet (pledgerDeltaControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness)) $ \control ->
  pmatch control $ \c ->
  pif (pfromData (pledgerDelta'pendingMutationCbor c) #/= pconstant "")
    ( plet
        ( pledgerDeltaPendingMutationFromCbor
            # pfromData (pledgerDelta'pendingMutationCbor c)
        )
        $ \pending ->
      pmatch pending $ \mutation ->
        pand'List
          [ pledgerDeltaControlIsWellFormed # witness # control
          , pfromData (pledgerDelta'stage c) #== 0 #|| pfromData (pledgerDelta'stage c) #== 1
          , pfromData (ppendingMutation'authorizationStage mutation) #== 1
          , ProofFold.pfoldIsCompleteV1 # pfromData (ppendingMutation'foldControl mutation)
          , pledgerDeltaPendingStep # pre # witness # pcon PNoAuxiliaryWitness # control
          ]
    )
    perror

-- | Aiken @verify_ledger_delta_one_step_v1@.
pverifyLedgerDeltaOneStepV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyLedgerDeltaOneStepV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch pre $ \preState ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transition auxiliary) ->
  plet (pfromData transition) $ \witness ->
    pand'List
      [ pfromData (pmachineState'phase preState) #== pcon PLedgerDelta
      , pstructuralTransitionIsValid # pre # witness
      , pverifyLedgerDelta # pre # witness # pdecodeValidationAuxiliaryWitnessV1 auxiliary
      ]

pdecodeValidationContext :: forall s. Term s (PByteString :--> PValidationContextV1)
pdecodeValidationContext = phoistAcyclic $ plam $ \contextCbor ->
  pmatch (pdeserialise # contextCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \items ->
      pif (plength # items #== 7)
        ( plet (pasInt # (pelemAt # 0 # items)) $ \version ->
          plet (pasByteStr # (pelemAt # 1 # items)) $ \profileId ->
          plet (pasInt # (pelemAt # 2 # items)) $ \blockEndTimeMs ->
          plet (pasInt # (pelemAt # 3 # items)) $ \expectedNetworkId ->
          plet (pasInt # (pelemAt # 4 # items)) $ \minFeeA ->
          plet (pasInt # (pelemAt # 5 # items)) $ \minFeeB ->
          plet (pasInt # (pelemAt # 6 # items)) $ \blockSlot ->
            pif
              ( version #== 1
                  #&& profileId #== pconsensusProfileV1Id
                  #&& blockEndTimeMs #>= 0
                  #&& (expectedNetworkId #== 0 #|| expectedNetworkId #== 1)
                  #&& minFeeA #>= 0
                  #&& minFeeB #>= 0
                  #&& blockSlot #>= 0
              )
              ( pcon $ PValidationContextV1
                  (pdata blockEndTimeMs)
                  (pdata expectedNetworkId)
                  (pdata minFeeA)
                  (pdata minFeeB)
                  (pdata blockSlot)
              )
              perror
        )
        perror

pinputSetsControlFromWitness :: forall s. Term s (PByteString :--> PInputSetsControlV1)
pinputSetsControlFromWitness = phoistAcyclic $ plam $ \witnessCbor ->
  pmatch (pdeserialise # witnessCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \items ->
      pif (plength # items #== 10)
        ( pcon $ PInputSetsControlV1
            (pdata $ pasByteStr # (pelemAt # 0 # items))
            (pdata $ pasByteStr # (pelemAt # 1 # items))
            (pdata $ pasByteStr # (pelemAt # 2 # items))
            (pdata $ pasByteStr # (pelemAt # 3 # items))
            (pdata $ pasInt # (pelemAt # 4 # items))
            (pdata $ pasInt # (pelemAt # 5 # items))
            (pdata $ pasInt # (pelemAt # 6 # items))
            (pdata $ pasInt # (pelemAt # 7 # items))
            (pdata $ pasByteStr # (pelemAt # 8 # items))
            (pdata $ pasByteStr # (pelemAt # 9 # items))
        )
        perror

pdecodeFrontierPeakItems :: forall s.
  Term s (PBuiltinList PData :--> PBuiltinList (PAsData PFrontierPeak))
pdecodeFrontierPeakItems = phoistAcyclic $ pfix $ \self -> plam $ \items ->
  pelimList
    (\item rest -> plet (pasList # item) $ \fields ->
      pif (plength # fields #== 2)
        ( pcons
            # pdata
              ( pcon $ PFrontierPeak
                  (pdata $ pasInt # (pelemAt # 0 # fields))
                  (pdata $ pasByteStr # (pelemAt # 1 # fields))
              )
            # (self # rest)
        )
        perror)
    pnil
    items

psignaturesControlFromWitness :: forall s. Term s (PByteString :--> PSignaturesControlV1)
psignaturesControlFromWitness = phoistAcyclic $ plam $ \witnessCbor ->
  pmatch (pdeserialise # witnessCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \items ->
      pif (plength # items #== 15)
        ( pcon $ PSignaturesControlV1
            (pdata $ pasByteStr # (pelemAt # 0 # items))
            (pdata $ pasByteStr # (pelemAt # 1 # items))
            (pdata $ pasByteStr # (pelemAt # 2 # items))
            (pdata $ pasByteStr # (pelemAt # 3 # items))
            (pdata $ pasByteStr # (pelemAt # 4 # items))
            (pdata $ pasInt # (pelemAt # 5 # items))
            (pdata $ pasInt # (pelemAt # 6 # items))
            (pdata $ pasInt # (pelemAt # 7 # items))
            (pdata $ pasInt # (pelemAt # 8 # items))
            (pdata $ pasInt # (pelemAt # 9 # items))
            (pdata $ pasByteStr # (pelemAt # 10 # items))
            (pdata $ pasByteStr # (pelemAt # 11 # items))
            (pdata $ pasInt # (pelemAt # 12 # items))
            (pdata $ pdecodeFrontierPeakItems # (pasList # (pelemAt # 13 # items)))
            (pdata $ pasInt # (pelemAt # 14 # items))
        )
        perror

pphaseANativeControlFromWitness :: forall s.
  Term s (PByteString :--> PPhaseANativeScriptsControlV1)
pphaseANativeControlFromWitness = phoistAcyclic $ plam $ \witnessCbor ->
  pmatch (pdeserialise # witnessCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \items ->
      pif (plength # items #== 18)
        ( plet (pasList # (pelemAt # 17 # items)) $ \signerContinuation ->
          pif (plength # signerContinuation #== 2)
            ( pcon $ PPhaseANativeScriptsControlV1
                (pdata $ pasByteStr # (pelemAt # 0 # items))
                (pdata $ pasByteStr # (pelemAt # 1 # items))
                (pdata $ pasByteStr # (pelemAt # 2 # items))
                (pdata $ pasByteStr # (pelemAt # 3 # items))
                (pdata $ pasByteStr # (pelemAt # 4 # items))
                (pdata $ pasInt # (pelemAt # 5 # items))
                (pdata $ pasInt # (pelemAt # 6 # items))
                (pdata $ pasInt # (pelemAt # 7 # items))
                (pdata $ pasInt # (pelemAt # 8 # items))
                (pdata $ pasInt # (pelemAt # 9 # items))
                (pdata $ pasByteStr # (pelemAt # 10 # items))
                (pdata $ pasInt # (pelemAt # 11 # items))
                (pdata $ pasByteStr # (pelemAt # 12 # items))
                (pdata $ pasInt # (pelemAt # 13 # items))
                (pdata $ pasInt # (pelemAt # 14 # items))
                (pdata $ pasInt # (pelemAt # 15 # items))
                (pdata $ pasInt # (pelemAt # 16 # items))
                ( pdata
                    $ pdecodeFrontierPeakItems
                    # (pasList # (pelemAt # 0 # signerContinuation))
                )
                (pdata $ pasByteStr # (pelemAt # 1 # signerContinuation))
            )
            perror
        )
        perror

pphaseAScriptPreconditionsControlFromWitness :: forall s.
  Term s (PByteString :--> PPhaseAScriptPreconditionsControlV1)
pphaseAScriptPreconditionsControlFromWitness = phoistAcyclic $ plam $ \witnessCbor ->
  pmatch (pdeserialise # witnessCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \items ->
      pif (plength # items #== 11)
        ( pcon $ PPhaseAScriptPreconditionsControlV1
            (pdata $ pasByteStr # (pelemAt # 0 # items))
            (pdata $ pasByteStr # (pelemAt # 1 # items))
            (pdata $ pasByteStr # (pelemAt # 2 # items))
            (pdata $ pasByteStr # (pelemAt # 3 # items))
            (pdata $ pasByteStr # (pelemAt # 4 # items))
            (pdata $ pasInt # (pelemAt # 5 # items))
            (pdata $ pasByteStr # (pelemAt # 6 # items))
            (pdata $ pasInt # (pelemAt # 7 # items))
            (pdata $ pasInt # (pelemAt # 9 # items))
            (pdata $ pasInt # (pelemAt # 10 # items))
            (pdata $ pasByteStr # (pelemAt # 8 # items))
        )
        perror

pdecodeResolveInputOutputProof :: forall s.
  Term s (PByteString :--> PResolveInputOutputProofV1)
pdecodeResolveInputOutputProof = phoistAcyclic $ plam $ \pendingCbor ->
  pmatch (pdeserialise # pendingCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \items ->
      pif (plength # items #== 5)
        ( pcon $ PResolveInputOutputProofV1
            (pdata $ pasInt # (pelemAt # 0 # items))
            (pdata $ pasByteStr # (pelemAt # 1 # items))
            (pdata $ pasByteStr # (pelemAt # 2 # items))
            (pdata $ pasByteStr # (pelemAt # 3 # items))
            ( pdata
                $ LedgerOutputProof.pdecodeControlV1
                # (pasByteStr # (pelemAt # 4 # items))
            )
        )
        perror

presolveInputsControlFromWitness :: forall s.
  Term s (PByteString :--> PResolveInputsControlV1)
presolveInputsControlFromWitness = phoistAcyclic $ plam $ \witnessCbor ->
  pmatch (pdeserialise # witnessCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \items ->
      pif (plength # items #== 11)
        ( plet (pasByteStr # (pelemAt # 9 # items)) $ \pendingCbor ->
          pcon $ PResolveInputsControlV1
            (pdata $ pasByteStr # (pelemAt # 0 # items))
            (pdata $ pasByteStr # (pelemAt # 1 # items))
            (pdata $ pasByteStr # (pelemAt # 2 # items))
            (pdata $ pasByteStr # (pelemAt # 3 # items))
            (pdata $ pasInt # (pelemAt # 4 # items))
            (pdata $ pasByteStr # (pelemAt # 5 # items))
            (pdata $ pasByteStr # (pelemAt # 6 # items))
            (pdata $ pasInt # (pelemAt # 7 # items))
            (pdata $ pasByteStr # (pelemAt # 8 # items))
            ( pdata
                $ pif (pendingCbor #== pconstant "\x00")
                  (pcon PDNothing)
                  (pcon $ PDJust $ pdata $ pdecodeResolveInputOutputProof # pendingCbor)
            )
            (pdata $ pasByteStr # (pelemAt # 10 # items))
        )
        perror

pencodeValueAccumulatorV1 :: forall s. Term s (PValueAccumulatorV1 :--> PByteString)
pencodeValueAccumulatorV1 = phoistAcyclic $ plam $ \accumulator -> pmatch accumulator $ \value ->
  pconstant "\x84"
    <> pcborInt (pfromData $ pvalueAccumulator'lovelaceDelta value)
    <> (pencodeDefiniteBytes # pfromData (pvalueAccumulator'assetRoot value))
    <> pcborInt (pfromData $ pvalueAccumulator'seenAssetCount value)
    <> pcborInt (pfromData $ pvalueAccumulator'nonzeroAssetCount value)

pvalueAccumulatorFromCbor :: forall s. Term s (PByteString :--> PValueAccumulatorV1)
pvalueAccumulatorFromCbor = phoistAcyclic $ plam $ \accumulatorCbor ->
  pmatch (pdeserialise # accumulatorCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \items ->
      pif (plength # items #== 4)
        ( plet
            ( pcon $ PValueAccumulatorV1
                (pdata $ pasInt # (pelemAt # 0 # items))
                (pdata $ pasByteStr # (pelemAt # 1 # items))
                (pdata $ pasInt # (pelemAt # 2 # items))
                (pdata $ pasInt # (pelemAt # 3 # items))
            )
            $ \accumulator -> pmatch accumulator $ \value ->
              plet (pfromData $ pvalueAccumulator'seenAssetCount value) $ \seenCount ->
              plet (pfromData $ pvalueAccumulator'nonzeroAssetCount value) $ \nonzeroCount ->
                pif
                  ( plengthBS # pfromData (pvalueAccumulator'assetRoot value) #== 32
                      #&& seenCount #>= 0
                      #&& seenCount #<= LedgerOutput.pmaxDistinctAssetCount
                      #&& nonzeroCount #>= 0
                      #&& nonzeroCount #<= seenCount
                      #&& pencodeValueAccumulatorV1 # accumulator #== accumulatorCbor
                  )
                  accumulator
                  perror
        )
        perror

pupdateValueAssetDeltaRoot :: forall s.
  Term s
    ( PByteString
        :--> PByteString
        :--> PInteger
        :--> PValueAssetMutationWitnessV1
        :--> PMaybe PByteString
    )
pupdateValueAssetDeltaRoot = phoistAcyclic $ plam $ \root unit quantityDelta mutation ->
  pmatch mutation $ \m ->
    plet (pfromData $ pvalueMutation'oldDelta m) $ \oldDelta ->
    plet (pcon $ PProof $ pfromData $ pvalueMutation'deltaProof m) $ \proof ->
      pif
        (pfromData $ pvalueMutation'deltaWasPresent m)
        ( MpfProof.pupdateRoot
            # root
            # unit
            # pcborInt oldDelta
            # pcborInt (oldDelta + quantityDelta)
            # proof
        )
        ( pif
            (oldDelta #/= 0)
            (pcon PNothing)
            (MpfProof.pinsertRoot # root # unit # pcborInt quantityDelta # proof)
        )

-- | Aiken @apply_value_asset_mutation@.
papplyValueAssetMutation :: forall s.
  Term s
    ( PValueAccumulatorV1
        :--> PByteString
        :--> PInteger
        :--> PValueAssetMutationWitnessV1
        :--> PValueAccumulatorUpdateV1
    )
papplyValueAssetMutation = phoistAcyclic $ plam $ \accumulator unit quantityDelta mutation ->
  pmatch accumulator $ \a ->
  pmatch mutation $ \m ->
    plet (pfromData $ pvalueMutation'deltaWasPresent m) $ \wasPresent ->
    plet (pfromData $ pvalueMutation'oldDelta m) $ \oldDeltaClaim ->
    plet (pcon $ PProof $ pfromData $ pvalueMutation'deltaProof m) $ \proof ->
      pif
        ( plengthBS # unit #< 28
            #|| plengthBS # unit #> 60
            #|| quantityDelta #== 0
            #|| pnot # (MpfProof.pproofHasAtMostSteps # proof # 16)
        )
        (pcon PValueAccumulatorMutationInvalid)
        ( pmatch
            ( pupdateValueAssetDeltaRoot
                # pfromData (pvalueAccumulator'assetRoot a)
                # unit
                # quantityDelta
                # mutation
            )
            $ \case
              PNothing -> pcon PValueAccumulatorMutationInvalid
              PJust nextAssetRoot ->
                plet (pif wasPresent oldDeltaClaim 0) $ \oldDelta ->
                plet (oldDelta + quantityDelta) $ \nextDelta ->
                plet
                  ( pif
                      (oldDelta #== 0 #&& nextDelta #/= 0)
                      (pfromData (pvalueAccumulator'nonzeroAssetCount a) + 1)
                      ( pif
                          (oldDelta #/= 0 #&& nextDelta #== 0)
                          (pfromData (pvalueAccumulator'nonzeroAssetCount a) - 1)
                          (pfromData $ pvalueAccumulator'nonzeroAssetCount a)
                      )
                  )
                  $ \nextNonzeroCount ->
                  plet (pif wasPresent 0 1) $ \newAssetCount ->
                    pif
                      ( pnot # wasPresent
                          #&& pfromData (pvalueAccumulator'seenAssetCount a) #>= LedgerOutput.pmaxDistinctAssetCount
                      )
                      (pcon PValueAccumulatorAssetLimitExceeded)
                      ( pif
                          ( nextNonzeroCount #< 0
                              #|| nextNonzeroCount #> pfromData (pvalueAccumulator'seenAssetCount a) + newAssetCount
                          )
                          (pcon PValueAccumulatorMutationInvalid)
                          ( pcon $ PValueAccumulatorUpdated $ pdata $ pcon $ PValueAccumulatorV1
                              (pvalueAccumulator'lovelaceDelta a)
                              (pdata nextAssetRoot)
                              (pdata $ pfromData (pvalueAccumulator'seenAssetCount a) + newAssetCount)
                              (pdata nextNonzeroCount)
                          )
                      )
        )

pledgerDeltaProofDescriptorFromCbor :: forall s.
  Term s (PByteString :--> ProofFold.PProofDescriptorV1)
pledgerDeltaProofDescriptorFromCbor = phoistAcyclic $ plam $ \descriptorCbor ->
  pmatch (pdeserialise # descriptorCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \items ->
      pif (plength # items #== 4)
        ( plet
            ( pcon $ ProofFold.PProofDescriptorV1
                { ProofFold.pproofDescriptor'version = pdata $ pasInt # (pelemAt # 0 # items)
                , ProofFold.pproofDescriptor'frameCount = pdata $ pasInt # (pelemAt # 1 # items)
                , ProofFold.pproofDescriptor'terminalCursor = pdata $ pasInt # (pelemAt # 2 # items)
                , ProofFold.pproofDescriptor'peaks =
                    pdata $ pdecodeFrontierPeakItems # (pasList # (pelemAt # 3 # items))
                }
            )
            $ \descriptor ->
              pif
                (ProofFold.pencodeProofDescriptorV1 # descriptor #== descriptorCbor)
                descriptor
                perror
        )
        perror

pencodeLedgerDeltaPendingMutationV1 :: forall s.
  Term s (PLedgerDeltaPendingMutationV1 :--> PByteString)
pencodeLedgerDeltaPendingMutationV1 = phoistAcyclic $ plam $ \pending ->
  pmatch pending $ \mutation ->
  pmatch (pfromData $ ppendingMutation'foldControl mutation) $ \fold ->
    (pencodeDefiniteArrayHeader # 10)
      <> pcborInt 1
      <> pcborInt (pfromData $ ppendingMutation'authorizationStage mutation)
      <> pcborInt (pfromData $ ppendingMutation'operationKind mutation)
      <> (pencodeDefiniteBytes # pfromData (ppendingMutation'key mutation))
      <> (pencodeDefiniteBytes # pfromData (ppendingMutation'value mutation))
      <> ( pencodeDefiniteBytes
            # (ProofFold.pencodeProofDescriptorV1 # pfromData (ppendingMutation'descriptor mutation))
         )
      <> pcborInt (pfromData $ ProofFold.pfoldControl'nextFrameIndex fold)
      <> (pencodeDefiniteBytes # pfromData (ProofFold.pfoldControl'includingRoot fold))
      <> (pencodeDefiniteBytes # pfromData (ProofFold.pfoldControl'excludingRoot fold))
      <> pcborInt (pfromData $ ProofFold.pfoldControl'expectedNextCursor fold)

pledgerDeltaPendingMutationFromCbor :: forall s.
  Term s (PByteString :--> PLedgerDeltaPendingMutationV1)
pledgerDeltaPendingMutationFromCbor = phoistAcyclic $ plam $ \pendingCbor ->
  pmatch (pdeserialise # pendingCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \items ->
      pif (plength # items #== 10 #&& pasInt # (pelemAt # 0 # items) #== 1)
        ( plet
            ( pcon $ PLedgerDeltaPendingMutationV1
                (pdata $ pasInt # (pelemAt # 1 # items))
                (pdata $ pasInt # (pelemAt # 2 # items))
                (pdata $ pasByteStr # (pelemAt # 3 # items))
                (pdata $ pasByteStr # (pelemAt # 4 # items))
                (pdata $ pledgerDeltaProofDescriptorFromCbor # (pasByteStr # (pelemAt # 5 # items)))
                ( pdata $ pcon $ ProofFold.PProofFoldControlV1
                    { ProofFold.pfoldControl'nextFrameIndex = pdata $ pasInt # (pelemAt # 6 # items)
                    , ProofFold.pfoldControl'expectedNextCursor = pdata $ pasInt # (pelemAt # 9 # items)
                    , ProofFold.pfoldControl'includingRoot = pdata $ pasByteStr # (pelemAt # 7 # items)
                    , ProofFold.pfoldControl'excludingRoot = pdata $ pasByteStr # (pelemAt # 8 # items)
                    }
                )
            )
            $ \pending -> pmatch pending $ \mutation ->
              plet (pfromData $ ppendingMutation'authorizationStage mutation) $ \authorizationStage ->
              plet (pfromData $ ppendingMutation'operationKind mutation) $ \operationKind ->
              pmatch (pfromData $ ppendingMutation'foldControl mutation) $ \fold ->
                pif
                  ( ( operationKind #== 0 #|| operationKind #== 1 )
                      #&& pif (authorizationStage #== 0)
                        ( pfromData (ProofFold.pfoldControl'nextFrameIndex fold) #== (-1)
                            #&& pfromData (ProofFold.pfoldControl'expectedNextCursor fold) #== 0
                            #&& pfromData (ProofFold.pfoldControl'includingRoot fold) #== pconstant ""
                            #&& pfromData (ProofFold.pfoldControl'excludingRoot fold) #== pconstant ""
                        )
                        (authorizationStage #== 1)
                      #&& pencodeLedgerDeltaPendingMutationV1 # pending #== pendingCbor
                  )
                  pending
                  perror
        )
        perror

pencodeLedgerDeltaControlV1 :: forall s. Term s (PLedgerDeltaControlV1 :--> PByteString)
pencodeLedgerDeltaControlV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \delta ->
  (pencodeDefiniteArrayHeader # 14)
    <> pcborInt (pfromData $ pledgerDelta'resolvedInputCount delta)
    <> (pencodeDefiniteBytes # pfromData (pledgerDelta'resolvedInputsAccumulator delta))
    <> pcborInt (pfromData $ pledgerDelta'outputCount delta)
    <> (pencodeFrontier # pfromData (pledgerDelta'outputDescriptorPeaks delta))
    <> pcborInt (pfromData $ pledgerDelta'stage delta)
    <> (pencodeDefiniteBytes # pfromData (pledgerDelta'replayScheduleHash delta))
    <> pcborInt (pfromData $ pledgerDelta'replayCursor delta)
    <> (pencodeDefiniteBytes # pfromData (pledgerDelta'replayAccumulator delta))
    <> (pencodeDefiniteBytes # pfromData (pledgerDelta'replayRemainingScheduleHash delta))
    <> (pencodeDefiniteBytes # pfromData (pledgerDelta'currentLedgerRoot delta))
    <> pcborInt (pfromData $ pledgerDelta'outputCursor delta)
    <> pcborInt (pfromData $ pledgerDelta'operationCount delta)
    <> (pencodeDefiniteBytes # pfromData (pledgerDelta'pendingMutationCbor delta))
    <> (pencodeFrontier # pfromData (pledgerDelta'operationPeaks delta))

pledgerDeltaControlFromWitness :: forall s. Term s (PByteString :--> PLedgerDeltaControlV1)
pledgerDeltaControlFromWitness = phoistAcyclic $ plam $ \workWitnessCbor ->
  pmatch (pdeserialise # workWitnessCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \items ->
      pif (plength # items #== 14)
        ( pcon $ PLedgerDeltaControlV1
            (pdata $ pasInt # (pelemAt # 0 # items))
            (pdata $ pasByteStr # (pelemAt # 1 # items))
            (pdata $ pasInt # (pelemAt # 2 # items))
            (pdata $ pdecodeFrontierPeakItems # (pasList # (pelemAt # 3 # items)))
            (pdata $ pasInt # (pelemAt # 4 # items))
            (pdata $ pasByteStr # (pelemAt # 5 # items))
            (pdata $ pasInt # (pelemAt # 6 # items))
            (pdata $ pasByteStr # (pelemAt # 7 # items))
            (pdata $ pasByteStr # (pelemAt # 8 # items))
            (pdata $ pasByteStr # (pelemAt # 9 # items))
            (pdata $ pasInt # (pelemAt # 10 # items))
            (pdata $ pasInt # (pelemAt # 11 # items))
            (pdata $ pdecodeFrontierPeakItems # (pasList # (pelemAt # 13 # items)))
            (pdata $ pasByteStr # (pelemAt # 12 # items))
        )
        perror

pencodeTerminalAcceptanceWitnessV1 :: forall s.
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PByteString
    )
pencodeTerminalAcceptanceWitnessV1 = phoistAcyclic $ plam $ \postLedgerRoot operationCount operationPeaks ->
  pif
    ( plengthBS # postLedgerRoot #== 32
        #&& pfrontierIsWellFormed # operationCount # operationPeaks
    )
    ( plet
        (pconstant "\x82" <> pcborInt operationCount <> (pencodeFrontier # operationPeaks))
        $ \frontierCbor ->
          pconstant "\x84"
            <> pcborInt 1
            <> (pencodeDefiniteBytes # pconstant "")
            <> (pencodeDefiniteBytes # postLedgerRoot)
            <> (pencodeDefiniteBytes # frontierCbor)
    )
    perror

pencodeLedgerDeltaWitnessV1 :: forall s.
  Term s (PNativeScriptsControlV1 :--> PByteString :--> PByteString :--> PByteString)
pencodeLedgerDeltaWitnessV1 = phoistAcyclic $ plam $ \nativeControl priorLedgerRoot replayScheduleHash ->
  pif
    (plengthBS # priorLedgerRoot #== 32 #&& plengthBS # replayScheduleHash #== 32)
    ( pmatch nativeControl $ \native ->
      pencodeLedgerDeltaControlV1
        # ( pcon $ PLedgerDeltaControlV1
              (pnativeControl'resolvedInputCount native)
              (pnativeControl'resolvedInputsAccumulator native)
              (pnativeControl'outputCount native)
              (pnativeControl'outputDescriptorPeaks native)
              (pdata 0)
              (pdata replayScheduleHash)
              (pdata 0)
              (pdata pinitialResolutionAccumulator)
              (pdata replayScheduleHash)
              (pdata priorLedgerRoot)
              (pdata 0)
              (pdata 0)
              (pdata pnil)
              (pdata $ pconstant "")
          )
    )
    perror

pencodeNativeScriptsControlV1 :: forall s. Term s (PNativeScriptsControlV1 :--> PByteString)
pencodeNativeScriptsControlV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  (pencodeDefiniteArrayHeader # 26)
    <> (pencodeDefiniteBytes # pfromData (pnativeControl'compactCbor c))
    <> (pencodeDefiniteBytes # pfromData (pnativeControl'witnessSetCompactCbor c))
    <> (pencodeDefiniteBytes # pfromData (pnativeControl'fieldPreimageLengthsCbor c))
    <> (pencodeDefiniteBytes # pfromData (pnativeControl'contextCbor c))
    <> pcborInt (pfromData $ pnativeControl'resolvedInputCount c)
    <> (pencodeDefiniteBytes # pfromData (pnativeControl'resolvedInputsAccumulator c))
    <> pcborInt (pfromData $ pnativeControl'spendInputCount c)
    <> (pencodeFrontier # pfromData (pnativeControl'resolvedItemPeaks c))
    <> pcborInt (pfromData $ pnativeControl'signerCount c)
    <> (pencodeDefiniteBytes # pfromData (pnativeControl'signerFrontierCommitment c))
    <> pcborInt (pfromData $ pnativeControl'sourceCount c)
    <> (pencodeFrontier # pfromData (pnativeControl'sourcePeaks c))
    <> pcborInt (pfromData $ pnativeControl'redeemerCount c)
    <> (pencodeFrontier # pfromData (pnativeControl'redeemerPeaks c))
    <> pcborInt (pfromData $ pnativeControl'purposeCount c)
    <> (pencodeFrontier # pfromData (pnativeControl'purposePeaks c))
    <> pcborInt (pfromData $ pnativeControl'outputCount c)
    <> (pencodeFrontier # pfromData (pnativeControl'outputPeaks c))
    <> (pencodeFrontier # pfromData (pnativeControl'outputDescriptorPeaks c))
    <> pcborInt (pfromData $ pnativeControl'mintCount c)
    <> (pencodeFrontier # pfromData (pnativeControl'mintPeaks c))
    <> pcborInt (pfromData $ pnativeControl'executionCount c)
    <> (pencodeFrontier # pfromData (pnativeControl'executionPeaks c))
    <> pcborInt (pfromData $ pnativeControl'executionCursor c)
    <> pcborInt (pfromData $ pnativeControl'languageBitmap c)
    <> (pencodeDefiniteBytes # pfromData (pnativeControl'resolutionScheduleHash c))

pnativeScriptsControlFromWitness :: forall s. Term s (PByteString :--> PNativeScriptsControlV1)
pnativeScriptsControlFromWitness = phoistAcyclic $ plam $ \workWitnessCbor ->
  pmatch (pdeserialise # workWitnessCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \xs ->
      pif (plength # xs #== 26)
        ( pcon $ PNativeScriptsControlV1
            (db 0 xs) (db 1 xs) (db 2 xs) (db 3 xs)
            (di 4 xs) (db 5 xs) (di 6 xs) (dp 7 xs)
            (di 8 xs) (db 9 xs) (di 10 xs) (dp 11 xs)
            (di 12 xs) (dp 13 xs) (di 14 xs) (dp 15 xs)
            (di 16 xs) (dp 17 xs) (dp 18 xs) (di 19 xs)
            (dp 20 xs) (di 21 xs) (dp 22 xs) (di 23 xs)
            (di 24 xs) (db 25 xs)
        )
        perror
  where
    di index xs = pdata $ pasInt # (pelemAt # index # xs)
    db index xs = pdata $ pasByteStr # (pelemAt # index # xs)
    dp index xs = pdata $ pdecodeFrontierPeakItems # (pasList # (pelemAt # index # xs))

pnativeScriptsControlIsWellFormed :: forall s. Term s (PNativeScriptsControlV1 :--> PBool)
pnativeScriptsControlIsWellFormed = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  plet (pfromData $ pnativeControl'resolvedInputCount c) $ \resolvedCount ->
  plet (pfromData $ pnativeControl'spendInputCount c) $ \spendCount ->
  plet (pfromData $ pnativeControl'purposeCount c) $ \purposeCount ->
  plet (pfromData $ pnativeControl'outputCount c) $ \outputCount ->
  plet (pfromData $ pnativeControl'mintCount c) $ \mintCount ->
  plet (pfromData $ pnativeControl'executionCount c) $ \executionCount ->
  plet (pfromData $ pnativeControl'executionCursor c) $ \executionCursor ->
    pand'List
      [ resolvedCount #>= 0
      , plengthBS # pfromData (pnativeControl'resolvedInputsAccumulator c) #== 32
      , plengthBS # pfromData (pnativeControl'resolutionScheduleHash c) #== 32
      , spendCount #>= 0
      , spendCount #<= resolvedCount
      , pfrontierIsWellFormed # resolvedCount # pfromData (pnativeControl'resolvedItemPeaks c)
      , pfromData (pnativeControl'signerCount c) #>= 0
      , plengthBS # pfromData (pnativeControl'signerFrontierCommitment c) #== 32
      , pfrontierIsWellFormed # pfromData (pnativeControl'sourceCount c) # pfromData (pnativeControl'sourcePeaks c)
      , pfrontierIsWellFormed # pfromData (pnativeControl'redeemerCount c) # pfromData (pnativeControl'redeemerPeaks c)
      , pfrontierIsWellFormed # purposeCount # pfromData (pnativeControl'purposePeaks c)
      , pfrontierIsWellFormed # outputCount # pfromData (pnativeControl'outputPeaks c)
      , pfrontierIsWellFormed # outputCount # pfromData (pnativeControl'outputDescriptorPeaks c)
      , mintCount #>= 0
      , mintCount #<= LedgerOutput.pmaxDistinctAssetCount
      , pfrontierIsWellFormed # mintCount # pfromData (pnativeControl'mintPeaks c)
      , pfrontierIsWellFormed # executionCount # pfromData (pnativeControl'executionPeaks c)
      , executionCount #== purposeCount
      , executionCursor #>= 0
      , executionCursor #<= executionCount
      , pfromData (pnativeControl'languageBitmap c) #>= 0
      , pfromData (pnativeControl'languageBitmap c) #<= 3
      ]

-- | Common binding performed by Aiken @verify_native_scripts@ before either
-- the terminal or descriptor branch is selected.
pnativeScriptsControlIsBound :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PBool
    )
pnativeScriptsControlIsBound = phoistAcyclic $ plam $ \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
    pand'List
      [ NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (pnativeControl'compactCbor c)
          # pfromData (pnativeControl'witnessSetCompactCbor c)
          # pfromData (pnativeControl'fieldPreimageLengthsCbor c)
          #== pfromData (pmachineState'transactionCommitment preState)
      , phashValidationContext # pfromData (pnativeControl'contextCbor c)
          #== pfromData (pmachineState'validationContextHash preState)
      , pnativeScriptsControlIsWellFormed # control
      , pfromData (poneStep'workWitnessCbor stepWitness)
          #== pencodeNativeScriptsControlV1 # control
      ]

pnativeScriptsTerminalSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PBool
    )
pnativeScriptsTerminalSuccessorIsExact = phoistAcyclic $ plam $ \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet (pfromData $ poneStep'claimedSuccessor stepWitness) $ \post ->
  pmatch post $ \postState ->
    pfromData (pmachineState'phase postState) #== pcon PScriptIntegrity
      #&& pfromData (pmachineState'workRoot postState)
        #== phashWorkWitness
          # pcon PScriptIntegrity
          # (pfromData (pmachineState'programCounter preState) + 1)
          # (pencodeScriptIntegrityWitnessV1 # control)

pnativeScriptsWithCursorBitmap :: forall s.
  Term s (PNativeScriptsControlV1 :--> PInteger :--> PNativeScriptsControlV1)
pnativeScriptsWithCursorBitmap = phoistAcyclic $ plam $ \control bitmap ->
  pmatch control $ \c -> pcon $ PNativeScriptsControlV1
    (pnativeControl'compactCbor c)
    (pnativeControl'witnessSetCompactCbor c)
    (pnativeControl'fieldPreimageLengthsCbor c)
    (pnativeControl'contextCbor c)
    (pnativeControl'resolvedInputCount c)
    (pnativeControl'resolvedInputsAccumulator c)
    (pnativeControl'spendInputCount c)
    (pnativeControl'resolvedItemPeaks c)
    (pnativeControl'signerCount c)
    (pnativeControl'signerFrontierCommitment c)
    (pnativeControl'sourceCount c) (pnativeControl'sourcePeaks c)
    (pnativeControl'redeemerCount c) (pnativeControl'redeemerPeaks c)
    (pnativeControl'purposeCount c) (pnativeControl'purposePeaks c)
    (pnativeControl'outputCount c) (pnativeControl'outputPeaks c)
    (pnativeControl'outputDescriptorPeaks c)
    (pnativeControl'mintCount c) (pnativeControl'mintPeaks c)
    (pnativeControl'executionCount c) (pnativeControl'executionPeaks c)
    (pdata $ pfromData (pnativeControl'executionCursor c) + 1)
    (pdata bitmap)
    (pnativeControl'resolutionScheduleHash c)

pnativeScriptsSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PNativeScriptsControlV1 :--> PInteger :--> PBool
    )
pnativeScriptsSuccessorIsExact = phoistAcyclic $ plam $
  \pre witness control nextBitmap ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet (pfromData $ poneStep'claimedSuccessor stepWitness) $ \post ->
  pmatch post $ \postState ->
  plet (pnativeScriptsWithCursorBitmap # control # nextBitmap) $ \nextControl ->
    pfromData (pmachineState'phase postState) #== pcon PNativeScripts
      #&& pfromData (pmachineState'workRoot postState)
        #== phashWorkWitness
          # pcon PNativeScripts
          # (pfromData (pmachineState'programCounter preState) + 1)
          # (pencodeNativeScriptsControlV1 # nextControl)

pnativeScriptsNextLanguageBitmap :: forall s.
  Term s (PInteger :--> PInteger :--> PInteger)
pnativeScriptsNextLanguageBitmap = phoistAcyclic $ plam $ \bitmap languageTag ->
  pif (languageTag #== 3)
    (bitmap + pif (pmod # bitmap # 2 #== 0) 1 0)
    (pif (languageTag #== 128)
      (bitmap + pif (bitmap #< 2) 2 0)
      perror)

pverifyNativeExecutionDescriptorStep :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PNativeScriptsControlV1
        :--> PBool
    )
pverifyNativeExecutionDescriptorStep = phoistAcyclic $ plam $
  \pre witness auxiliary control ->
  pmatch auxiliary $ \case
    PNativeExecutionDescriptorWitness
      executionIndexD languageTagD purposeKindD purposeIndexD
      scriptHashD subjectD purposeSiblingsD sourceIndexD originKindD
      sourceKeyD scriptTotalLengthD scriptItemCommitmentD sourceSiblingsD
      redeemerLeafD executionSiblingsD firstChunkProofD signerPeaksD ->
        pmatch pre $ \preState ->
        pmatch witness $ \stepWitness ->
        pmatch control $ \c ->
        plet (pfromData executionIndexD) $ \executionIndex ->
        plet (pfromData languageTagD) $ \languageTag ->
        plet (pfromData purposeKindD) $ \purposeKind ->
        plet (pfromData purposeIndexD) $ \purposeIndex ->
        plet (pfromData scriptHashD) $ \scriptHash ->
        plet (pfromData subjectD) $ \subject ->
        plet (pfromData sourceIndexD) $ \sourceIndex ->
        plet (pfromData originKindD) $ \originKind ->
        plet (pfromData sourceKeyD) $ \sourceKey ->
        plet (pfromData scriptTotalLengthD) $ \scriptTotalLength ->
        plet (pfromData scriptItemCommitmentD) $ \scriptItemCommitment ->
        plet (pfromData redeemerLeafD) $ \redeemerLeaf ->
        plet
          (ScriptProof.ppurposeLeafHash # purposeKind # purposeIndex # scriptHash # subject)
          $ \purposeLeaf ->
        plet
          ( ScriptProof.psourceDescriptorLeafHash
              # originKind # sourceKey # languageTag # scriptHash
              # scriptTotalLength # scriptItemCommitment
          )
          $ \sourceLeaf ->
        plet
          (ScriptProof.pexecutionLeafHash # languageTag # purposeLeaf # sourceLeaf # redeemerLeaf)
          $ \executionLeaf ->
        pif
          ( pand'List
              [ executionIndex #== pfromData (pnativeControl'executionCursor c)
              , pfromData (pnativeControl'executionCount c)
                  #== pfromData (pnativeControl'purposeCount c)
              , scriptTotalLength #> 0
              , scriptTotalLength #<= pmaxAggregateFieldPreimageBytes
              , plengthBS # scriptItemCommitment #== 32
              , pverifyMembership
                  # pfromData (pnativeControl'purposeCount c)
                  # pfromData (pnativeControl'purposePeaks c)
                  # executionIndex # purposeLeaf # pfromData purposeSiblingsD
              , pverifyMembership
                  # pfromData (pnativeControl'sourceCount c)
                  # pfromData (pnativeControl'sourcePeaks c)
                  # sourceIndex # sourceLeaf # pfromData sourceSiblingsD
              , pverifyMembership
                  # pfromData (pnativeControl'executionCount c)
                  # pfromData (pnativeControl'executionPeaks c)
                  # executionIndex # executionLeaf # pfromData executionSiblingsD
              ]
          )
          ( pif (languageTag #== 0)
              ( pmatch (pfromData firstChunkProofD) $ \case
                  PDNothing -> perror
                  PDJust chunkProofD ->
                    plet (pfromData chunkProofD) $ \chunkProof ->
                    pmatch chunkProof $ \proof ->
                    pmatch
                      ( NativeScriptScan.pversionedScriptHeaderV1
                          # pfromData (BoundedItem.pchunkProof'chunk proof)
                          # scriptTotalLength
                      )
                      $ \case
                        PNothing -> perror
                        PJust header -> pmatch header $ \headerFields ->
                          plet (pencodeNativeScriptsControlV1 # control) $ \continuationCbor ->
                          plet (pfromData signerPeaksD) $ \signerPeaks ->
                          plet (pfromData $ poneStep'claimedSuccessor stepWitness) $ \post ->
                          pmatch post $ \postState ->
                            pand'List
                              [ redeemerLeaf #== pconstant ""
                              , pfromData (BoundedItem.pchunkProof'chunkIndex proof) #== 0
                              , pfromData (BoundedItem.pchunkProof'totalLength proof)
                                  #== scriptTotalLength
                              , BoundedItem.pverifyChunk # scriptItemCommitment # chunkProof
                              , pfromData (NativeScriptScan.pheader'languageTag headerFields) #== 0
                              , pfrontierIsWellFormed
                                  # pfromData (pnativeControl'signerCount c) # signerPeaks
                              , pfrontierCommitment
                                  # pfromData (pnativeControl'signerCount c) # signerPeaks
                                  #== pfromData (pnativeControl'signerFrontierCommitment c)
                              , pfromData (pmachineState'phase postState) #== pcon PPhaseANativeScripts
                              , pfromData (pmachineState'workRoot postState)
                                  #== phashWorkWitness
                                    # pcon PPhaseANativeScripts
                                    # (pfromData (pmachineState'programCounter preState) + 1)
                                    # ( pencodePhaseANativeScriptsScanWitness
                                          # pfromData (pnativeControl'compactCbor c)
                                          # pfromData (pnativeControl'witnessSetCompactCbor c)
                                          # pfromData (pnativeControl'fieldPreimageLengthsCbor c)
                                          # pfromData (pnativeControl'contextCbor c)
                                          # (pblake2b_256 # continuationCbor)
                                          # 1 # 1 # 0 # 0 # scriptTotalLength
                                          # scriptItemCommitment
                                          # pfromData (NativeScriptScan.pheader'payloadOffset headerFields)
                                          # pconstant "" # 0 # 0 # (-1)
                                          # pfromData (pnativeControl'signerCount c)
                                          # signerPeaks # continuationCbor
                                      )
                              ]
              )
              ( pif
                  ( pand'List
                      [ languageTag #== 3 #|| languageTag #== 128
                      , plengthBS # redeemerLeaf #== 32
                      , pfromData firstChunkProofD #== pcon PDNothing
                      , pfromData signerPeaksD #== pnil
                      ]
                  )
                  ( plet (pfromData $ pnativeControl'languageBitmap c) $ \bitmap ->
                    plet (pnativeScriptsNextLanguageBitmap # bitmap # languageTag)
                      $ \nextBitmap ->
                        pnativeScriptsSuccessorIsExact
                          # pre # witness # control # nextBitmap
                  )
                  (pconstant False)
              )
          )
          (pconstant False)
    _ -> pconstant False

-- | Aiken @verify_native_scripts@.
pverifyNativeScripts :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyNativeScripts = phoistAcyclic $ plam $ \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet
    (pnativeScriptsControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pnativeControl'compactCbor c)
        # pfromData (pnativeControl'witnessSetCompactCbor c)
        # pfromData (pnativeControl'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verifiedSource _) ->
  pmatch verifiedSource $ \verified ->
    pverified'version verified #== 1
      #&& pnativeScriptsControlIsBound # pre # witness # control
      #&& pif
        ( pfromData (pnativeControl'executionCursor c)
            #== pfromData (pnativeControl'executionCount c)
        )
        ( pmatch auxiliary $ \case
            PNoAuxiliaryWitness ->
              pnativeScriptsTerminalSuccessorIsExact # pre # witness # control
            _ -> pconstant False
        )
        (pverifyNativeExecutionDescriptorStep # pre # witness # auxiliary # control)

-- | Aiken @verify_native_scripts_terminal_semantics_v1@.
pverifyNativeScriptsTerminalSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyNativeScriptsTerminalSemanticsV1 = phoistAcyclic $ plam $ \pre witness ->
  pverifyNativeScripts # pre # witness # pcon PNoAuxiliaryWitness

-- | Aiken @verify_native_scripts_native_semantics_v1@.
pverifyNativeScriptsNativeSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInteger :--> PInteger :--> PInteger
        :--> PByteString :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PInteger :--> PInteger :--> PByteString :--> PInteger
        :--> PByteString :--> PBuiltinList (PAsData PByteString)
        :--> PByteString :--> PBuiltinList (PAsData PByteString)
        :--> BoundedItem.PChunkProofV1
        :--> PBuiltinList (PAsData PFrontierPeak) :--> PBool
    )
pverifyNativeScriptsNativeSemanticsV1 = phoistAcyclic $ plam $
  \pre witness executionIndex purposeKind purposeIndex scriptHash subject
   purposeSiblings sourceIndex originKind sourceKey scriptTotalLength
   scriptItemCommitment sourceSiblings redeemerLeaf executionSiblings
   firstChunkProof signerPeaks ->
    pverifyNativeScripts # pre # witness
      # ( pcon $ PNativeExecutionDescriptorWitness
            (pdata executionIndex) (pdata 0) (pdata purposeKind) (pdata purposeIndex)
            (pdata scriptHash) (pdata subject) (pdata purposeSiblings)
            (pdata sourceIndex) (pdata originKind) (pdata sourceKey)
            (pdata scriptTotalLength) (pdata scriptItemCommitment)
            (pdata sourceSiblings) (pdata redeemerLeaf) (pdata executionSiblings)
            (pdata $ pcon $ PDJust $ pdata firstChunkProof) (pdata signerPeaks)
        )

-- | Aiken @verify_native_scripts_effectful_semantics_v1@.
pverifyNativeScriptsEffectfulSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PInteger :--> PInteger :--> PInteger :--> PInteger
        :--> PByteString :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PInteger :--> PInteger :--> PByteString :--> PInteger
        :--> PByteString :--> PBuiltinList (PAsData PByteString)
        :--> PByteString :--> PBuiltinList (PAsData PByteString) :--> PBool
    )
pverifyNativeScriptsEffectfulSemanticsV1 = phoistAcyclic $ plam $
  \pre witness executionIndex languageTag purposeKind purposeIndex scriptHash
   subject purposeSiblings sourceIndex originKind sourceKey scriptTotalLength
   scriptItemCommitment sourceSiblings redeemerLeaf executionSiblings ->
    pverifyNativeScripts # pre # witness
      # ( pcon $ PNativeExecutionDescriptorWitness
            (pdata executionIndex) (pdata languageTag) (pdata purposeKind) (pdata purposeIndex)
            (pdata scriptHash) (pdata subject) (pdata purposeSiblings)
            (pdata sourceIndex) (pdata originKind) (pdata sourceKey)
            (pdata scriptTotalLength) (pdata scriptItemCommitment)
            (pdata sourceSiblings) (pdata redeemerLeaf) (pdata executionSiblings)
            (pdata $ pcon PDNothing) (pdata pnil)
        )

-- | Aiken @verify_native_scripts_one_step_v1@.
pverifyNativeScriptsOneStepV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyNativeScriptsOneStepV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch pre $ \preState ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transitionD auxiliaryD) ->
  plet (pfromData transitionD) $ \witness ->
    pfromData (pmachineState'phase preState) #== pcon PNativeScripts
      #&& pstructuralTransitionIsValid # pre # witness
      #&& pverifyNativeScripts # pre # witness # pdecodeValidationAuxiliaryWitnessV1 auxiliaryD

pencodeScriptIntegrityWitnessV1, pencodeScriptIntegrityCompactWitnessV1 ::
  forall s. Term s (PNativeScriptsControlV1 :--> PByteString)
pencodeScriptIntegrityWitnessV1 = phoistAcyclic $ plam $ \control ->
  pconstant "\x82" <> (pencodeDefiniteBytes # (pencodeNativeScriptsControlV1 # control)) <> pcborInt 0
pencodeScriptIntegrityCompactWitnessV1 = phoistAcyclic $ plam $ \control ->
  pconstant "\x82" <> (pencodeDefiniteBytes # (pencodeNativeScriptsControlV1 # control)) <> pcborInt 1

pencodeScriptIntegrityWitnessSetWitnessV1, pencodeScriptIntegrityFinalizeWitnessV1 ::
  forall s. Term s (PNativeScriptsControlV1 :--> PByteString :--> PByteString :--> PByteString)
pencodeScriptIntegrityWitnessSetWitnessV1 = pencodeScriptIntegrityHashWitness 2
pencodeScriptIntegrityFinalizeWitnessV1 = pencodeScriptIntegrityHashWitness 3

pencodeScriptIntegrityHashWitness ::
  Integer ->
  (forall s. Term s (PNativeScriptsControlV1 :--> PByteString :--> PByteString :--> PByteString))
pencodeScriptIntegrityHashWitness stage = phoistAcyclic $ plam $ \control firstHash secondHash ->
  pif
    (plengthBS # firstHash #== 32 #&& plengthBS # secondHash #== 32)
    ( pconstant "\x84"
        <> (pencodeDefiniteBytes # (pencodeNativeScriptsControlV1 # control))
        <> pcborInt (pconstant stage)
        <> (pencodeDefiniteBytes # firstHash)
        <> (pencodeDefiniteBytes # secondHash)
    )
    perror

-- | Aiken @script_integrity_control_and_stage@. The outer witness intentionally
-- only requires its first two fields here; stage-specific branches enforce the
-- canonical encoding before accepting a transition.
pscriptIntegrityControlAndStage :: forall s.
  Term s (PByteString :--> PPair PNativeScriptsControlV1 PInteger)
pscriptIntegrityControlAndStage = phoistAcyclic $ plam $ \workWitnessCbor ->
  pmatch (pdeserialise # workWitnessCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \fields ->
      pcon $ PPair
        (pnativeScriptsControlFromWitness # (pasByteStr # (pelemAt # 0 # fields)))
        (pasInt # (pelemAt # 1 # fields))

-- | Aiken @native_resolved_context_is_well_formed@. ScriptIntegrity stage zero
-- deliberately authenticates only the resolved-input portion of the control;
-- the stronger aggregate predicate belongs to NativeScripts.
pnativeResolvedContextIsWellFormed :: forall s.
  Term s (PNativeScriptsControlV1 :--> PBool)
pnativeResolvedContextIsWellFormed = phoistAcyclic $ plam $ \control ->
  pmatch control $ \c ->
  plet (pfromData $ pnativeControl'resolvedInputCount c) $ \resolvedCount ->
  plet (pfromData $ pnativeControl'spendInputCount c) $ \spendCount ->
    pand'List
      [ spendCount #>= 0
      , spendCount #<= resolvedCount
      , pfrontierIsWellFormed
          # resolvedCount
          # pfromData (pnativeControl'resolvedItemPeaks c)
      ]

-- | Aiken @verify_script_integrity_authentication@.
pverifyScriptIntegrityAuthentication :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptIntegrityAuthentication = phoistAcyclic $ plam $ \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch (pscriptIntegrityControlAndStage # pfromData (poneStep'workWitnessCbor stepWitness)) $
    \(PPair control stage) ->
  pmatch control $ \c ->
  plet (pfromData $ poneStep'claimedSuccessor stepWitness) $ \post ->
  pmatch post $ \postState ->
    pand'List
      [ pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False
      , stage #== 0
      , pfromData (pnativeControl'executionCursor c)
          #== pfromData (pnativeControl'executionCount c)
      , pnativeResolvedContextIsWellFormed # control
      , NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (pnativeControl'compactCbor c)
          # pfromData (pnativeControl'witnessSetCompactCbor c)
          # pfromData (pnativeControl'fieldPreimageLengthsCbor c)
          #== pfromData (pmachineState'transactionCommitment preState)
      , phashValidationContext # pfromData (pnativeControl'contextCbor c)
          #== pfromData (pmachineState'validationContextHash preState)
      , pfromData (poneStep'workWitnessCbor stepWitness)
          #== pencodeScriptIntegrityWitnessV1 # control
      , pfromData (pmachineState'phase postState) #== pcon PScriptIntegrity
      , pfromData (pmachineState'workRoot postState)
          #== phashWorkWitness
            # pcon PScriptIntegrity
            # (pfromData (pmachineState'programCounter preState) + 1)
            # (pencodeScriptIntegrityCompactWitnessV1 # control)
      ]

-- | Aiken @verify_script_integrity_authentication_semantics_v1@.
pverifyScriptIntegrityAuthenticationSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyScriptIntegrityAuthenticationSemanticsV1 = phoistAcyclic $ plam $ \pre witness ->
  pmatch pre $ \preState ->
    pfromData (pmachineState'phase preState) #== pcon PScriptIntegrity
      #&& pstructuralTransitionIsValid # pre # witness
      #&& pverifyScriptIntegrityAuthentication
        # pre # witness # pcon PNoAuxiliaryWitness

-- | Decode the four-field stage-two and stage-three witnesses. Both Aiken
-- branches require an exact four-element list before interpreting its fields.
pscriptIntegrityHashFields :: forall s.
  Term s
    ( PByteString
        :--> PPair
          PNativeScriptsControlV1
          (PPair PInteger (PPair PByteString PByteString))
    )
pscriptIntegrityHashFields = phoistAcyclic $ plam $ \workWitnessCbor ->
  pmatch (pdeserialise # workWitnessCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \fields ->
      pif
        (plength # fields #== 4)
        ( pcon $ PPair
            (pnativeScriptsControlFromWitness # (pasByteStr # (pelemAt # 0 # fields)))
            ( pcon $ PPair
                (pasInt # (pelemAt # 1 # fields))
                ( pcon $ PPair
                    (pasByteStr # (pelemAt # 2 # fields))
                    (pasByteStr # (pelemAt # 3 # fields))
                )
            )
        )
        perror

-- | Aiken @verify_script_integrity_compact@.
pverifyScriptIntegrityCompact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptIntegrityCompact = phoistAcyclic $ plam $ \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch (pscriptIntegrityControlAndStage # pfromData (poneStep'workWitnessCbor stepWitness)) $
    \(PPair control stage) ->
  pmatch control $ \c ->
  plet
    ( NativeCompact.pverifyNativeTxCompactCborV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pnativeControl'compactCbor c)
    )
    $ \verifiedSource ->
  pmatch verifiedSource $ \verified ->
  pmatch (pverified'txCompact verified) $ \compact ->
  pmatch (pcompact'body compact) $ \body ->
  plet (pfromData $ poneStep'claimedSuccessor stepWitness) $ \post ->
  pmatch post $ \postState ->
    pand'List
      [ pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False
      , stage #== 1
      , pfromData (poneStep'workWitnessCbor stepWitness)
          #== pencodeScriptIntegrityCompactWitnessV1 # control
      , pfromData (pmachineState'phase postState) #== pcon PScriptIntegrity
      , pfromData (pmachineState'workRoot postState)
          #== phashWorkWitness
            # pcon PScriptIntegrity
            # (pfromData (pmachineState'programCounter preState) + 1)
            # ( pencodeScriptIntegrityWitnessSetWitnessV1
                  # control
                  # pbodyCompact'scriptIntegrityHash body
                  # pcompact'witnessSetHash compact
              )
      ]

-- | Aiken @verify_script_integrity_witness_set@.
pverifyScriptIntegrityWitnessSet :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptIntegrityWitnessSet = phoistAcyclic $ plam $ \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch (pscriptIntegrityHashFields # pfromData (poneStep'workWitnessCbor stepWitness)) $
    \(PPair control rest) ->
  pmatch rest $ \(PPair stage hashes) ->
  pmatch hashes $ \(PPair scriptIntegrityHash witnessSetHash) ->
  pmatch control $ \c ->
  plet
    (NativeCompact.pdecodeNativeTxWitnessSetCompact # pfromData (pnativeControl'witnessSetCompactCbor c))
    $ \verifiedWitnessSet ->
  pmatch verifiedWitnessSet $ \witnessSet ->
  plet (pfromData $ poneStep'claimedSuccessor stepWitness) $ \post ->
  pmatch post $ \postState ->
    pand'List
      [ pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False
      , stage #== 2
      , plengthBS # scriptIntegrityHash #== 32
      , plengthBS # witnessSetHash #== 32
      , pblake2b_256 # pfromData (pnativeControl'witnessSetCompactCbor c)
          #== witnessSetHash
      , pfromData (poneStep'workWitnessCbor stepWitness)
          #== pencodeScriptIntegrityWitnessSetWitnessV1
            # control # scriptIntegrityHash # witnessSetHash
      , pfromData (pmachineState'phase postState) #== pcon PScriptIntegrity
      , pfromData (pmachineState'workRoot postState)
          #== phashWorkWitness
            # pcon PScriptIntegrity
            # (pfromData (pmachineState'programCounter preState) + 1)
            # ( pencodeScriptIntegrityFinalizeWitnessV1
                  # control
                  # scriptIntegrityHash
                  # pfromData (pwitnessSetCompact'redeemerTxWitsHash witnessSet)
              )
      ]

-- | Aiken @verify_script_integrity_finalize@.
pverifyScriptIntegrityFinalize :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptIntegrityFinalize = phoistAcyclic $ plam $ \pre witness auxiliary ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch (pscriptIntegrityHashFields # pfromData (poneStep'workWitnessCbor stepWitness)) $
    \(PPair control rest) ->
  pmatch rest $ \(PPair stage hashes) ->
  pmatch hashes $ \(PPair scriptIntegrityHash redeemerTxWitsHash) ->
  pmatch control $ \c ->
  plet
    ( ScriptLanguageViews.pexpectedScriptIntegrityHash
        # redeemerTxWitsHash
        # pfromData (pnativeControl'languageBitmap c)
    )
    $ \expectedHash ->
  plet (pfromData $ poneStep'claimedSuccessor stepWitness) $ \post ->
  pmatch post $ \postState ->
    pand'List
      [ pmatch auxiliary $ \case PNoAuxiliaryWitness -> pconstant True; _ -> pconstant False
      , stage #== 3
      , plengthBS # scriptIntegrityHash #== 32
      , plengthBS # redeemerTxWitsHash #== 32
      , pfromData (poneStep'workWitnessCbor stepWitness)
          #== pencodeScriptIntegrityFinalizeWitnessV1
            # control # scriptIntegrityHash # redeemerTxWitsHash
      , pif
          (scriptIntegrityHash #/= expectedHash)
          ( prejectedSuccessorIsExact
              # pre # post # pconstant "E_INVALID_FIELD_TYPE"
          )
          ( pfromData (pmachineState'phase postState) #== pcon PCek
              #&& pfromData (pmachineState'workRoot postState)
                #== phashWorkWitness
                  # pcon PCek
                  # (pfromData (pmachineState'programCounter preState) + 1)
                  # ( pencodeCekWitnessV1
                        # control # pconstant "" # 0 # 0 # 0
                        # pconstant "" # 0 # 0 # pconstant ""
                    )
          )
      ]

-- | Aiken @verify_script_integrity@.
pverifyScriptIntegrity :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyScriptIntegrity = phoistAcyclic $ plam $ \pre witness auxiliary ->
  pmatch witness $ \stepWitness ->
  pmatch (pscriptIntegrityControlAndStage # pfromData (poneStep'workWitnessCbor stepWitness)) $
    \(PPair _ stage) ->
      pif (stage #== 0)
        (pverifyScriptIntegrityAuthentication # pre # witness # auxiliary)
        $ pif (stage #== 1)
          (pverifyScriptIntegrityCompact # pre # witness # auxiliary)
        $ pif (stage #== 2)
          (pverifyScriptIntegrityWitnessSet # pre # witness # auxiliary)
        $ pif (stage #== 3)
          (pverifyScriptIntegrityFinalize # pre # witness # auxiliary)
          (pconstant False)

pscriptIntegritySemanticsEnvelope :: forall s.
  Term s
    ( (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool)
        :--> PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool
    )
pscriptIntegritySemanticsEnvelope = phoistAcyclic $ plam $ \verifyStage pre witness ->
  pmatch pre $ \preState ->
    pfromData (pmachineState'phase preState) #== pcon PScriptIntegrity
      #&& pstructuralTransitionIsValid # pre # witness
      #&& verifyStage # pre # witness # pcon PNoAuxiliaryWitness

-- | Aiken @verify_script_integrity_compact_semantics_v1@.
pverifyScriptIntegrityCompactSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyScriptIntegrityCompactSemanticsV1 =
  pscriptIntegritySemanticsEnvelope # pverifyScriptIntegrityCompact

-- | Aiken @verify_script_integrity_witness_set_semantics_v1@.
pverifyScriptIntegrityWitnessSetSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyScriptIntegrityWitnessSetSemanticsV1 =
  pscriptIntegritySemanticsEnvelope # pverifyScriptIntegrityWitnessSet

-- | Aiken @verify_script_integrity_finalize_semantics_v1@.
pverifyScriptIntegrityFinalizeSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyScriptIntegrityFinalizeSemanticsV1 =
  pscriptIntegritySemanticsEnvelope # pverifyScriptIntegrityFinalize

-- | Aiken @verify_script_integrity_one_step_v1@.
pverifyScriptIntegrityOneStepV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyScriptIntegrityOneStepV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch pre $ \preState ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transitionD auxiliaryD) ->
  plet (pfromData transitionD) $ \witness ->
    pfromData (pmachineState'phase preState) #== pcon PScriptIntegrity
      #&& pstructuralTransitionIsValid # pre # witness
      #&& pverifyScriptIntegrity # pre # witness # pdecodeValidationAuxiliaryWitnessV1 auxiliaryD

-- | Aiken @canonical_decode_control_from_witness@.
pcanonicalDecodeControlFromWitness :: forall s.
  Term s (PByteString :--> PCanonicalDecodeControlV1)
pcanonicalDecodeControlFromWitness = phoistAcyclic $ plam $ \witnessCbor ->
  pmatch (pdeserialise # witnessCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \fields ->
      pif
        (plength # fields #== 9)
        ( pcon $ PCanonicalDecodeControlV1
            (pdata $ pasByteStr # (pelemAt # 0 # fields))
            (pdata $ pasByteStr # (pelemAt # 1 # fields))
            (pdata $ pasByteStr # (pelemAt # 2 # fields))
            (pdata $ pasByteStr # (pelemAt # 3 # fields))
            (pdata $ pasInt # (pelemAt # 4 # fields))
            (pdata $ pasInt # (pelemAt # 5 # fields))
            (pdata $ pasInt # (pelemAt # 6 # fields))
            (pdata $ pasInt # (pelemAt # 7 # fields))
            (pdata $ pasInt # (pelemAt # 8 # fields))
        )
        perror

ptransactionFieldPreimageLength :: forall s.
  Term s (PNativeTxFieldPreimageLengthsV1 :--> PInteger :--> PInteger)
ptransactionFieldPreimageLength = phoistAcyclic $ plam $ \lengths fieldIndex ->
  pmatch lengths $ \fieldLengths ->
    pif (fieldIndex #== 0) (plengths'spendInputs fieldLengths) $
    pif (fieldIndex #== 1) (plengths'referenceInputs fieldLengths) $
    pif (fieldIndex #== 2) (plengths'outputs fieldLengths) $
    pif (fieldIndex #== 3) (plengths'requiredObservers fieldLengths) $
    pif (fieldIndex #== 4) (plengths'requiredSigners fieldLengths) $
    pif (fieldIndex #== 5) (plengths'mint fieldLengths) $
    pif (fieldIndex #== 6) (plengths'scriptWitnesses fieldLengths) $
    pif (fieldIndex #== 7) (plengths'addressWitnesses fieldLengths) $
    pif (fieldIndex #== 8) (plengths'redeemers fieldLengths) perror

-- | Aiken @canonical_scan_successor_is_exact@.
pcanonicalScanSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationMachineStateV1
        :--> PByteString :--> PByteString :--> PByteString :--> PByteString
        :--> PInteger :--> PInteger :--> PInteger :--> PInteger :--> PInteger
        :--> PBool
    )
pcanonicalScanSuccessorIsExact = phoistAcyclic $ plam $
  \pre post compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor contextCbor
   fieldIndex itemIndex chunkIndex itemCount encodedLength ->
  pmatch pre $ \preState ->
  pmatch post $ \postState ->
    pfromData (pmachineState'phase postState) #== pcon PCanonicalDecode
      #&& pfromData (pmachineState'workRoot postState)
        #== phashWorkWitness
          # pcon PCanonicalDecode
          # (pfromData (pmachineState'programCounter preState) + 1)
          # ( pencodeTransactionFieldScanWitness
                # compactCbor # witnessSetCompactCbor # fieldPreimageLengthsCbor
                # contextCbor # fieldIndex # itemIndex # chunkIndex
                # itemCount # encodedLength
            )

-- | Aiken @canonical_scan_field_successor_is_exact@.
pcanonicalScanFieldSuccessorIsExact :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationMachineStateV1
        :--> PByteString :--> PByteString :--> PByteString :--> PByteString
        :--> PInteger :--> PBool
    )
pcanonicalScanFieldSuccessorIsExact = phoistAcyclic $ plam $
  \pre post compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor contextCbor fieldIndex ->
  pmatch pre $ \preState ->
  pmatch post $ \postState ->
    pif
      (fieldIndex #< 8)
      ( pcanonicalScanSuccessorIsExact
          # pre # post # compactCbor # witnessSetCompactCbor
          # fieldPreimageLengthsCbor # contextCbor
          # (fieldIndex + 1) # 0 # 0 # (-1) # 0
      )
      ( pfromData (pmachineState'phase postState) #== pcon PCompactBinding
          #&& pfromData (pmachineState'workRoot postState)
            #== phashWorkWitness
              # pcon PCompactBinding
              # (pfromData (pmachineState'programCounter preState) + 1)
              # ( pencodeCompactBindingWitness
                    # pfromData (pmachineState'transactionId preState)
                    # pfromData (pmachineState'transactionCommitment preState)
                    # compactCbor # witnessSetCompactCbor
                    # fieldPreimageLengthsCbor # contextCbor
                )
      )

-- | Aiken @canonical_decode_control_is_bound@.
pcanonicalDecodeControlIsBound :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PCanonicalDecodeControlV1 :--> PVerifiedMidgardNativeTxCompact
        :--> PBool
    )
pcanonicalDecodeControlIsBound = phoistAcyclic $ plam $ \pre witness control verified ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch verified $ \verifiedSource ->
    pand'List
      [ pverified'version verifiedSource #== 1
      , pfromData (pcanonicalControl'fieldIndex c) #>= 0
      , pfromData (pcanonicalControl'fieldIndex c) #<= 8
      , pfromData (pcanonicalControl'itemIndex c) #>= 0
      , pfromData (pcanonicalControl'chunkIndex c) #>= 0
      , pfromData (pcanonicalControl'itemCount c) #>= -1
      , pfromData (pcanonicalControl'encodedLength c) #>= 0
      , NativeCompact.pnativeTxProofCommitmentV1
          # pfromData (pcanonicalControl'compactCbor c)
          # pfromData (pcanonicalControl'witnessSetCompactCbor c)
          # pfromData (pcanonicalControl'fieldPreimageLengthsCbor c)
          #== pfromData (pmachineState'transactionCommitment preState)
      , phashValidationContext # pfromData (pcanonicalControl'contextCbor c)
          #== pfromData (pmachineState'validationContextHash preState)
      , pfromData (poneStep'workWitnessCbor stepWitness)
          #== pencodeTransactionFieldScanWitness
            # pfromData (pcanonicalControl'compactCbor c)
            # pfromData (pcanonicalControl'witnessSetCompactCbor c)
            # pfromData (pcanonicalControl'fieldPreimageLengthsCbor c)
            # pfromData (pcanonicalControl'contextCbor c)
            # pfromData (pcanonicalControl'fieldIndex c)
            # pfromData (pcanonicalControl'itemIndex c)
            # pfromData (pcanonicalControl'chunkIndex c)
            # pfromData (pcanonicalControl'itemCount c)
            # pfromData (pcanonicalControl'encodedLength c)
      ]

-- | Aiken @verify_canonical_decode_empty@.
pverifyCanonicalDecodeEmpty :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PCanonicalDecodeControlV1 :--> PBool
    )
pverifyCanonicalDecodeEmpty = phoistAcyclic $ plam $ \pre witness control ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pcanonicalControl'compactCbor c)
        # pfromData (pcanonicalControl'witnessSetCompactCbor c)
        # pfromData (pcanonicalControl'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified witnessSet) ->
  pmatch verified $ \verifiedSource ->
  plet
    (NativeCompact.pdecodeNativeTxFieldPreimageLengthsV1 # pfromData (pcanonicalControl'fieldPreimageLengthsCbor c))
    $ \fieldLengths ->
  plet
    ( ptransactionFieldPreimageLength
        # fieldLengths # pfromData (pcanonicalControl'fieldIndex c)
    )
    $ \expectedFieldLength ->
  plet
    ( ptransactionFieldCommitment
        # pverified'txCompact verifiedSource # witnessSet
        # pfromData (pcanonicalControl'fieldIndex c)
    )
    $ \expectedFieldCommitment ->
  plet (pfromData $ poneStep'claimedSuccessor stepWitness) $ \post ->
    pand'List
      [ pcanonicalDecodeControlIsBound # pre # witness # control # verified
      , expectedFieldCommitment #== NativeField.pemptyFieldCommitment
      , pfromData (pcanonicalControl'itemIndex c) #== 0
      , pfromData (pcanonicalControl'chunkIndex c) #== 0
      , pfromData (pcanonicalControl'itemCount c) #== -1
      , pfromData (pcanonicalControl'encodedLength c) #== 0
      , pif
          (expectedFieldLength #/= 1)
          ( prejectedSuccessorIsExact
              # pre # post # pconstant "E_FIELD_PREIMAGE_SIZE"
          )
          ( pcanonicalScanFieldSuccessorIsExact
              # pre # post
              # pfromData (pcanonicalControl'compactCbor c)
              # pfromData (pcanonicalControl'witnessSetCompactCbor c)
              # pfromData (pcanonicalControl'fieldPreimageLengthsCbor c)
              # pfromData (pcanonicalControl'contextCbor c)
              # pfromData (pcanonicalControl'fieldIndex c)
          )
      ]

-- | Aiken @verify_canonical_decode_empty_semantics_v1@.
pverifyCanonicalDecodeEmptySemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyCanonicalDecodeEmptySemanticsV1 = phoistAcyclic $ plam $ \pre witness ->
  pverifyCanonicalDecodeEmpty
    # pre # witness
    # ( pcanonicalDecodeControlFromWitness
          # (pmatch witness $ \stepWitness -> pfromData $ poneStep'workWitnessCbor stepWitness)
      )

-- | Aiken @canonical_argument_header_size@.
pcanonicalArgumentHeaderSize :: forall s. Term s (PInteger :--> PInteger)
pcanonicalArgumentHeaderSize = phoistAcyclic $ plam $ \value ->
  pif (value #>= 0)
    ( pif (value #< 24) 1 $
      pif (value #< 256) 2 $
      pif (value #< 65_536) 3 $
      pif (value #< 4_294_967_296) 5 9
    )
    perror

-- | Aiken @transaction_field_item_encoded_length@.
ptransactionFieldItemEncodedLength :: forall s.
  Term s (PInteger :--> PInteger :--> PMaybe PInteger)
ptransactionFieldItemEncodedLength = phoistAcyclic $ plam $ \fieldIndex itemLength ->
  pif (itemLength #>= 0)
    ( pif
        ( fieldIndex #== 0 #|| fieldIndex #== 1 #|| fieldIndex #== 2
            #|| fieldIndex #== 3 #|| fieldIndex #== 4 #|| fieldIndex #== 7
        )
        (pcon $ PJust $ pcanonicalArgumentHeaderSize # itemLength + itemLength)
        ( pif
            (fieldIndex #== 6 #|| fieldIndex #== 8)
            (pcon $ PJust itemLength)
            ( pif
                (fieldIndex #== 5)
                (pif (itemLength #== 0) (pcon PNothing) (pcon $ PJust $ itemLength - 1))
                perror
            )
        )
    )
    perror

-- | Aiken @verify_canonical_decode_chunk@.
pverifyCanonicalDecodeChunk :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PCanonicalDecodeControlV1 :--> PItemProofV1
        :--> BoundedItem.PChunkProofV1 :--> PBool
    )
pverifyCanonicalDecodeChunk = phoistAcyclic $ plam $
  \pre witness control collectionProof chunkProof ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  pmatch control $ \c ->
  pmatch collectionProof $ \item ->
  pmatch chunkProof $ \chunk ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pcanonicalControl'compactCbor c)
        # pfromData (pcanonicalControl'witnessSetCompactCbor c)
        # pfromData (pcanonicalControl'fieldPreimageLengthsCbor c)
    )
    $ \(PPair verified witnessSet) ->
  pmatch verified $ \verifiedSource ->
  plet
    ( ptransactionFieldCommitment
        # pverified'txCompact verifiedSource # witnessSet
        # pfromData (pcanonicalControl'fieldIndex c)
    )
    $ \expectedFieldCommitment ->
  plet
    (NativeCompact.pdecodeNativeTxFieldPreimageLengthsV1 # pfromData (pcanonicalControl'fieldPreimageLengthsCbor c))
    $ \fieldLengths ->
  plet
    ( ptransactionFieldPreimageLength
        # fieldLengths # pfromData (pcanonicalControl'fieldIndex c)
    )
    $ \expectedFieldLength ->
  plet
    ( pand'List
        [ pfromData (pcanonicalControl'itemIndex c) #== 0
        , pfromData (pcanonicalControl'chunkIndex c) #== 0
        , pfromData (pcanonicalControl'itemCount c) #== -1
        , pfromData (pcanonicalControl'encodedLength c) #== 0
        ]
    )
    $ \firstChunk ->
  plet
    ( pand'List
        [ pfromData (pcanonicalControl'itemCount c) #> 0
        , pfromData (pcanonicalControl'itemIndex c)
            #< pfromData (pcanonicalControl'itemCount c)
        , pfromData (pitemProof'itemCount item)
            #== pfromData (pcanonicalControl'itemCount c)
        ]
    )
    $ \continuingChunk ->
  plet
    ( pif firstChunk
        (pfromData $ pitemProof'itemCount item)
        (pfromData $ pcanonicalControl'itemCount c)
    )
    $ \activeItemCount ->
  plet
    ( pif firstChunk
        (pcanonicalArgumentHeaderSize # activeItemCount)
        (pfromData $ pcanonicalControl'encodedLength c)
    )
    $ \lengthBeforeItem ->
  plet (BoundedItem.pchunkCount # pfromData (BoundedItem.pchunkProof'totalLength chunk)) $
    \chunkCount ->
  plet
    (pfromData (pcanonicalControl'chunkIndex c) + 1 #== chunkCount)
    $ \itemFinished ->
  plet (pfromData $ poneStep'claimedSuccessor stepWitness) $ \post ->
    pand'List
      [ pcanonicalDecodeControlIsBound # pre # witness # control # verified
      , expectedFieldCommitment #/= NativeField.pemptyFieldCommitment
      , firstChunk #|| continuingChunk
      , activeItemCount #> 0
      , pfromData (pitemProof'fieldIndex item)
          #== pfromData (pcanonicalControl'fieldIndex c)
      , pfromData (pitemProof'itemCount item) #== activeItemCount
      , pfromData (pitemProof'itemIndex item)
          #== pfromData (pcanonicalControl'itemIndex c)
      , pfromData (pitemProof'itemLength item)
          #== pfromData (BoundedItem.pchunkProof'totalLength chunk)
      , pfromData (BoundedItem.pchunkProof'fieldIndex chunk)
          #== pfromData (pcanonicalControl'fieldIndex c)
      , pfromData (BoundedItem.pchunkProof'itemIndex chunk)
          #== pfromData (pcanonicalControl'itemIndex c)
      , pfromData (BoundedItem.pchunkProof'chunkIndex chunk)
          #== pfromData (pcanonicalControl'chunkIndex c)
      , pverifyBoundedCollectionItem # expectedFieldCommitment # collectionProof
      , BoundedItem.pverifyChunk
          # pfromData (pitemProof'itemCommitment item) # chunkProof
      , pif
          (pnot # itemFinished)
          ( pcanonicalScanSuccessorIsExact
              # pre # post
              # pfromData (pcanonicalControl'compactCbor c)
              # pfromData (pcanonicalControl'witnessSetCompactCbor c)
              # pfromData (pcanonicalControl'fieldPreimageLengthsCbor c)
              # pfromData (pcanonicalControl'contextCbor c)
              # pfromData (pcanonicalControl'fieldIndex c)
              # pfromData (pcanonicalControl'itemIndex c)
              # (pfromData (pcanonicalControl'chunkIndex c) + 1)
              # activeItemCount # lengthBeforeItem
          )
          ( pmatch
              ( ptransactionFieldItemEncodedLength
                  # pfromData (pcanonicalControl'fieldIndex c)
                  # pfromData (BoundedItem.pchunkProof'totalLength chunk)
              )
              $ \case
                PNothing -> prejectedSuccessorIsExact
                  # pre # post # pconstant "E_INVALID_FIELD_TYPE"
                PJust itemEncodedLength ->
                  plet (lengthBeforeItem + itemEncodedLength) $ \nextEncodedLength ->
                    pif
                      (pfromData (pcanonicalControl'itemIndex c) + 1 #< activeItemCount)
                      ( pcanonicalScanSuccessorIsExact
                          # pre # post
                          # pfromData (pcanonicalControl'compactCbor c)
                          # pfromData (pcanonicalControl'witnessSetCompactCbor c)
                          # pfromData (pcanonicalControl'fieldPreimageLengthsCbor c)
                          # pfromData (pcanonicalControl'contextCbor c)
                          # pfromData (pcanonicalControl'fieldIndex c)
                          # (pfromData (pcanonicalControl'itemIndex c) + 1)
                          # 0 # activeItemCount # nextEncodedLength
                      )
                      ( pif
                          (nextEncodedLength #/= expectedFieldLength)
                          ( prejectedSuccessorIsExact
                              # pre # post # pconstant "E_FIELD_PREIMAGE_SIZE"
                          )
                          ( pcanonicalScanFieldSuccessorIsExact
                              # pre # post
                              # pfromData (pcanonicalControl'compactCbor c)
                              # pfromData (pcanonicalControl'witnessSetCompactCbor c)
                              # pfromData (pcanonicalControl'fieldPreimageLengthsCbor c)
                              # pfromData (pcanonicalControl'contextCbor c)
                              # pfromData (pcanonicalControl'fieldIndex c)
                          )
                      )
          )
      ]

-- | Aiken @verify_canonical_decode_chunk_semantics_v1@.
pverifyCanonicalDecodeChunkSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PItemProofV1 :--> BoundedItem.PChunkProofV1 :--> PBool
    )
pverifyCanonicalDecodeChunkSemanticsV1 = phoistAcyclic $ plam $
  \pre witness collectionProof chunkProof ->
    pverifyCanonicalDecodeChunk
      # pre # witness
      # ( pcanonicalDecodeControlFromWitness
            # (pmatch witness $ \stepWitness -> pfromData $ poneStep'workWitnessCbor stepWitness)
        )
      # collectionProof # chunkProof

pcanonicalItemSourceFieldV1 :: forall s.
  Term s
    ( PByteString :--> PByteString :--> PByteString :--> PByteString
        :--> PInteger :--> PPair PByteString PInteger
    )
pcanonicalItemSourceFieldV1 = phoistAcyclic $ plam $
  \transactionId compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor fieldIndex ->
  pmatch
    ( NativeCompact.pverifyNativeTxProofSourceV1
        # transactionId # compactCbor # witnessSetCompactCbor # fieldPreimageLengthsCbor
    )
    $ \(PPair verified witnessSet) ->
  pmatch verified $ \verifiedSource ->
  plet (NativeCompact.pdecodeNativeTxFieldPreimageLengthsV1 # fieldPreimageLengthsCbor) $
    \fieldLengths ->
    pcon $ PPair
      (ptransactionFieldCommitment # pverified'txCompact verifiedSource # witnessSet # fieldIndex)
      (ptransactionFieldPreimageLength # fieldLengths # fieldIndex)

-- | Aiken @bind_canonical_decode_item_source_v1@.
pbindCanonicalDecodeItemSourceV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PCanonicalDecodeItemSourceV1
    )
pbindCanonicalDecodeItemSourceV1 = phoistAcyclic $ plam $ \pre witness ->
  pmatch pre $ \preState ->
  pmatch witness $ \stepWitness ->
  plet (pfromData $ poneStep'workWitnessCbor stepWitness) $ \workCbor ->
  plet (pcanonicalDecodeControlFromWitness # workCbor) $ \control ->
  pmatch control $ \c ->
  pmatch
    ( pcanonicalItemSourceFieldV1
        # pfromData (pmachineState'transactionId preState)
        # pfromData (pcanonicalControl'compactCbor c)
        # pfromData (pcanonicalControl'witnessSetCompactCbor c)
        # pfromData (pcanonicalControl'fieldPreimageLengthsCbor c)
        # pfromData (pcanonicalControl'fieldIndex c)
    )
    $ \(PPair expectedFieldCommitment expectedFieldLength) ->
  pif
    ( pand'List
        [ pfromData (pcanonicalControl'fieldIndex c) #>= 0
        , pfromData (pcanonicalControl'fieldIndex c) #<= 8
        , pfromData (pcanonicalControl'itemIndex c) #>= 0
        , pfromData (pcanonicalControl'chunkIndex c) #>= 0
        , pfromData (pcanonicalControl'itemCount c) #>= -1
        , pfromData (pcanonicalControl'encodedLength c) #>= 0
        , NativeCompact.pnativeTxProofCommitmentV1
            # pfromData (pcanonicalControl'compactCbor c)
            # pfromData (pcanonicalControl'witnessSetCompactCbor c)
            # pfromData (pcanonicalControl'fieldPreimageLengthsCbor c)
            #== pfromData (pmachineState'transactionCommitment preState)
        , phashValidationContext # pfromData (pcanonicalControl'contextCbor c)
            #== pfromData (pmachineState'validationContextHash preState)
        , workCbor #== pencodeTransactionFieldScanWitness
            # pfromData (pcanonicalControl'compactCbor c)
            # pfromData (pcanonicalControl'witnessSetCompactCbor c)
            # pfromData (pcanonicalControl'fieldPreimageLengthsCbor c)
            # pfromData (pcanonicalControl'contextCbor c)
            # pfromData (pcanonicalControl'fieldIndex c)
            # pfromData (pcanonicalControl'itemIndex c)
            # pfromData (pcanonicalControl'chunkIndex c)
            # pfromData (pcanonicalControl'itemCount c)
            # pfromData (pcanonicalControl'encodedLength c)
        ]
    )
    ( pcon $ PCanonicalDecodeItemSourceV1
        (pdata expectedFieldCommitment) (pdata expectedFieldLength)
    )
    perror

-- | Aiken @observe_canonical_decode_item_v1@.
pobserveCanonicalDecodeItemV1 :: forall s.
  Term s
    ( PValidationOneStepWitnessV1 :--> PItemProofV1 :--> PByteString
        :--> PCanonicalDecodeItemObservationV1
    )
pobserveCanonicalDecodeItemV1 = phoistAcyclic $ plam $ \witness collectionProof itemCbor ->
  pmatch witness $ \stepWitness ->
  plet
    (pcanonicalDecodeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
    pcon $ PCanonicalDecodeItemObservationV1
      (pdata collectionProof)
      (pdata $ plengthBS # itemCbor)
      ( pdata $ BoundedItem.pfromBytes
          # pfromData (pcanonicalControl'fieldIndex c)
          # pfromData (pcanonicalControl'itemIndex c)
          # itemCbor
      )

pcanonicalDecodeItemObservationIsAuthentic :: forall s.
  Term s
    ( PValidationOneStepWitnessV1 :--> PCanonicalDecodeItemSourceV1
        :--> PCanonicalDecodeItemObservationV1 :--> PBool
    )
pcanonicalDecodeItemObservationIsAuthentic = phoistAcyclic $ plam $
  \witness source observation ->
  pmatch witness $ \stepWitness ->
  pmatch source $ \sourceFields ->
  pmatch observation $ \observed ->
  plet (pfromData $ pcanonicalObservation'collectionProof observed) $ \collectionProof ->
  pmatch collectionProof $ \item ->
  plet
    (pcanonicalDecodeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  plet
    ( pand'List
        [ pfromData (pcanonicalControl'itemIndex c) #== 0
        , pfromData (pcanonicalControl'chunkIndex c) #== 0
        , pfromData (pcanonicalControl'itemCount c) #== -1
        , pfromData (pcanonicalControl'encodedLength c) #== 0
        ]
    )
    $ \firstItem ->
  plet
    ( pand'List
        [ pfromData (pcanonicalControl'chunkIndex c) #== 0
        , pfromData (pcanonicalControl'itemCount c) #> 0
        , pfromData (pcanonicalControl'itemIndex c)
            #< pfromData (pcanonicalControl'itemCount c)
        , pfromData (pitemProof'itemCount item)
            #== pfromData (pcanonicalControl'itemCount c)
        ]
    )
    $ \continuingItem ->
  plet
    ( pif firstItem
        (pfromData $ pitemProof'itemCount item)
        (pfromData $ pcanonicalControl'itemCount c)
    )
    $ \activeItemCount ->
    pand'List
      [ pfromData (pcanonicalSource'expectedFieldCommitment sourceFields)
          #/= NativeField.pemptyFieldCommitment
      , firstItem #|| continuingItem
      , activeItemCount #> 0
      , pfromData (pitemProof'fieldIndex item)
          #== pfromData (pcanonicalControl'fieldIndex c)
      , pfromData (pitemProof'itemCount item) #== activeItemCount
      , pfromData (pitemProof'itemIndex item)
          #== pfromData (pcanonicalControl'itemIndex c)
      , pfromData (pitemProof'itemLength item)
          #== pfromData (pcanonicalObservation'itemLength observed)
      , pverifyBoundedCollectionItem
          # pfromData (pcanonicalSource'expectedFieldCommitment sourceFields)
          # collectionProof
      , pfromData (pcanonicalObservation'itemCommitment observed)
          #== pfromData (pitemProof'itemCommitment item)
      ]

-- | Aiken @verify_canonical_decode_item_observation_v1@.
pverifyCanonicalDecodeItemObservationV1 :: forall s.
  Term s
    ( PValidationOneStepWitnessV1 :--> PCanonicalDecodeItemSourceV1
        :--> PCanonicalDecodeItemObservationV1 :--> PCanonicalDecodeItemProofV1
    )
pverifyCanonicalDecodeItemObservationV1 = phoistAcyclic $ plam $
  \witness source observation ->
  pmatch witness $ \stepWitness ->
  pmatch observation $ \observed ->
  plet (pfromData $ pcanonicalObservation'collectionProof observed) $ \collectionProof ->
  pmatch collectionProof $ \item ->
  plet
    (pcanonicalDecodeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  plet
    ( pand'List
        [ pfromData (pcanonicalControl'itemIndex c) #== 0
        , pfromData (pcanonicalControl'chunkIndex c) #== 0
        , pfromData (pcanonicalControl'itemCount c) #== -1
        , pfromData (pcanonicalControl'encodedLength c) #== 0
        ]
    )
    $ \firstItem ->
  plet
    ( pif firstItem
        (pfromData $ pitemProof'itemCount item)
        (pfromData $ pcanonicalControl'itemCount c)
    )
    $ \activeItemCount ->
  plet
    ( pif firstItem
        (pcanonicalArgumentHeaderSize # activeItemCount)
        (pfromData $ pcanonicalControl'encodedLength c)
    )
    $ \lengthBeforeItem ->
  pif
    (pcanonicalDecodeItemObservationIsAuthentic # witness # source # observation)
    ( pmatch
        ( ptransactionFieldItemEncodedLength
            # pfromData (pcanonicalControl'fieldIndex c)
            # pfromData (pcanonicalObservation'itemLength observed)
        )
        $ \case
          PNothing -> pcon $ PCanonicalDecodeItemProofV1
            (pdata activeItemCount) (pdata $ pconstant False) (pdata 0)
          PJust itemEncodedLength -> pcon $ PCanonicalDecodeItemProofV1
            (pdata activeItemCount) (pdata $ pconstant True)
            (pdata $ lengthBeforeItem + itemEncodedLength)
    )
    perror

-- | Aiken @verify_canonical_decode_item_successor_v1@.
pverifyCanonicalDecodeItemSuccessorV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PCanonicalDecodeItemSourceV1 :--> PCanonicalDecodeItemProofV1
        :--> PBool
    )
pverifyCanonicalDecodeItemSuccessorV1 = phoistAcyclic $ plam $
  \pre witness source proof ->
  pmatch witness $ \stepWitness ->
  pmatch source $ \sourceFields ->
  pmatch proof $ \proofFields ->
  plet
    (pcanonicalDecodeControlFromWitness # pfromData (poneStep'workWitnessCbor stepWitness))
    $ \control ->
  pmatch control $ \c ->
  plet (pfromData $ poneStep'claimedSuccessor stepWitness) $ \post ->
    pif
      (pnot # pfromData (pcanonicalProof'itemEncodingIsValid proofFields))
      (prejectedSuccessorIsExact # pre # post # pconstant "E_INVALID_FIELD_TYPE")
      ( pif
          ( pfromData (pcanonicalControl'itemIndex c) + 1
              #< pfromData (pcanonicalProof'activeItemCount proofFields)
          )
          ( pcanonicalScanSuccessorIsExact
              # pre # post
              # pfromData (pcanonicalControl'compactCbor c)
              # pfromData (pcanonicalControl'witnessSetCompactCbor c)
              # pfromData (pcanonicalControl'fieldPreimageLengthsCbor c)
              # pfromData (pcanonicalControl'contextCbor c)
              # pfromData (pcanonicalControl'fieldIndex c)
              # (pfromData (pcanonicalControl'itemIndex c) + 1)
              # 0 # pfromData (pcanonicalProof'activeItemCount proofFields)
              # pfromData (pcanonicalProof'nextEncodedLength proofFields)
          )
          ( pif
              ( pfromData (pcanonicalProof'nextEncodedLength proofFields)
                  #/= pfromData (pcanonicalSource'expectedFieldLength sourceFields)
              )
              (prejectedSuccessorIsExact # pre # post # pconstant "E_FIELD_PREIMAGE_SIZE")
              ( pcanonicalScanFieldSuccessorIsExact
                  # pre # post
                  # pfromData (pcanonicalControl'compactCbor c)
                  # pfromData (pcanonicalControl'witnessSetCompactCbor c)
                  # pfromData (pcanonicalControl'fieldPreimageLengthsCbor c)
                  # pfromData (pcanonicalControl'contextCbor c)
                  # pfromData (pcanonicalControl'fieldIndex c)
              )
          )
      )

-- | Aiken @verify_canonical_decode_item_from_source_v1@.
pverifyCanonicalDecodeItemFromSourceV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PCanonicalDecodeItemSourceV1 :--> PItemProofV1 :--> PByteString
        :--> PBool
    )
pverifyCanonicalDecodeItemFromSourceV1 = phoistAcyclic $ plam $
  \pre witness source collectionProof itemCbor ->
  plet (pobserveCanonicalDecodeItemV1 # witness # collectionProof # itemCbor) $
    \observation ->
    pcanonicalDecodeItemObservationIsAuthentic # witness # source # observation
      #&& pverifyCanonicalDecodeItemSuccessorV1
        # pre # witness # source
        # (pverifyCanonicalDecodeItemObservationV1 # witness # source # observation)

-- | Aiken @verify_canonical_decode_item_proof_v1@.
pverifyCanonicalDecodeItemProofV1 :: forall s.
  Term s
    ( PValidationOneStepWitnessV1 :--> PCanonicalDecodeItemSourceV1
        :--> PItemProofV1 :--> PByteString :--> PCanonicalDecodeItemProofV1
    )
pverifyCanonicalDecodeItemProofV1 = phoistAcyclic $ plam $
  \witness source collectionProof itemCbor ->
    pverifyCanonicalDecodeItemObservationV1
      # witness # source
      # (pobserveCanonicalDecodeItemV1 # witness # collectionProof # itemCbor)

-- | Aiken @verify_canonical_decode_item@.
pverifyCanonicalDecodeItem :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PCanonicalDecodeControlV1 :--> PItemProofV1 :--> PByteString
        :--> PBool
    )
pverifyCanonicalDecodeItem = phoistAcyclic $ plam $
  \pre witness _control collectionProof itemCbor ->
    pverifyCanonicalDecodeItemFromSourceV1
      # pre # witness # (pbindCanonicalDecodeItemSourceV1 # pre # witness)
      # collectionProof # itemCbor

-- | Aiken @verify_canonical_decode_item_semantics_v1@.
pverifyCanonicalDecodeItemSemanticsV1 :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PItemProofV1 :--> PByteString :--> PBool
    )
pverifyCanonicalDecodeItemSemanticsV1 = phoistAcyclic $ plam $
  \pre witness collectionProof itemCbor ->
    pverifyCanonicalDecodeItem
      # pre # witness
      # ( pcanonicalDecodeControlFromWitness
            # (pmatch witness $ \stepWitness -> pfromData $ poneStep'workWitnessCbor stepWitness)
        )
      # collectionProof # itemCbor

-- | Aiken @verify_canonical_decode@.
pverifyCanonicalDecode :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyCanonicalDecode = phoistAcyclic $ plam $ \pre witness auxiliary ->
  plet
    ( pcanonicalDecodeControlFromWitness
        # (pmatch witness $ \stepWitness -> pfromData $ poneStep'workWitnessCbor stepWitness)
    )
    $ \control ->
      pmatch auxiliary $ \case
        PNoAuxiliaryWitness ->
          pverifyCanonicalDecodeEmpty # pre # witness # control
        PTransactionFieldChunkWitness collectionProof chunkProof ->
          pverifyCanonicalDecodeChunk
            # pre # witness # control # pfromData collectionProof # pfromData chunkProof
        PTransactionFieldItemWitness collectionProof itemCbor ->
          pverifyCanonicalDecodeItem
            # pre # witness # control # pfromData collectionProof # pfromData itemCbor
        _ -> pconstant False

-- | Aiken @verify_canonical_decode_one_step_v1@.
pverifyCanonicalDecodeOneStepV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyCanonicalDecodeOneStepV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch pre $ \preState ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transition auxiliary) ->
  plet (pfromData transition) $ \witness ->
    pfromData (pmachineState'phase preState) #== pcon PCanonicalDecode
      #&& pstructuralTransitionIsValid # pre # witness
      #&& pverifyCanonicalDecode # pre # witness # pdecodeValidationAuxiliaryWitnessV1 auxiliary

-- | Aiken @verify_canonical_decode_semantics_v1@.
pverifyCanonicalDecodeSemanticsV1 :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyCanonicalDecodeSemanticsV1 = phoistAcyclic $ plam $ \pre evidence ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transition auxiliary) ->
    pverifyCanonicalDecode # pre # pfromData transition # pdecodeValidationAuxiliaryWitnessV1 auxiliary

-- | Aiken @verify_one_step_with_auxiliary@. Midgard V1 is deliberately
-- closed over the phases implemented here.
pverifyOneStepWithAuxiliary :: forall s.
  Term s
    ( PValidationMachineStateV1 :--> PValidationOneStepWitnessV1
        :--> PValidationAuxiliaryWitnessV1 :--> PBool
    )
pverifyOneStepWithAuxiliary = phoistAcyclic $ plam $ \pre witness auxiliary ->
  pstructuralTransitionIsValid # pre # witness
    #&& pmatch pre (\preState ->
      pmatch (pfromData $ pmachineState'phase preState) $ \case
        PCanonicalDecode ->
          pverifyCanonicalDecode # pre # witness # auxiliary
        PCompactBinding ->
          pmatch auxiliary $ \case
            PNoAuxiliaryWitness -> pverifyCompactBinding # pre # witness
            _ -> pconstant False
        PInputSets ->
          pverifyInputSets # pre # witness # auxiliary
        PStaticLedgerRules ->
          pmatch auxiliary $ \case
            PNoAuxiliaryWitness -> pverifyStaticRules # pre # witness
            _ -> pconstant False
        PResolveInputs ->
          pverifyResolveInputs # pre # witness # auxiliary
        PSignatures ->
          pverifySignatures # pre # witness # auxiliary
        PValueAndMint ->
          pverifyValueAndMint # pre # witness # auxiliary
        PScriptSources ->
          pverifyScriptSources # pre # witness # auxiliary
        PNativeScripts ->
          pverifyNativeScripts # pre # witness # auxiliary
        PPhaseANativeScripts ->
          pverifyPhaseANativeScripts # pre # witness # auxiliary
        PPhaseAScriptPreconditions ->
          pverifyPhaseAScriptPreconditions # pre # witness # auxiliary
        PScriptIntegrity ->
          pverifyScriptIntegrity # pre # witness # auxiliary
        PCek ->
          pverifyCek # pre # witness # auxiliary
        PLedgerDelta ->
          pverifyLedgerDelta # pre # witness # auxiliary
        PTerminal -> pconstant False
    )

-- | Aiken @verify_one_step@.
pverifyOneStep :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyOneStep = phoistAcyclic $ plam $ \pre witness ->
  pverifyOneStepWithAuxiliary # pre # witness # pcon PNoAuxiliaryWitness

-- | Aiken @verify_one_step_evidence@.
pverifyOneStepEvidence :: forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool)
pverifyOneStepEvidence = phoistAcyclic $ plam $ \pre evidence ->
  pmatch evidence $ \(PValidationOneStepEvidenceV1 transition auxiliary) ->
    pverifyOneStepWithAuxiliary # pre # pfromData transition # pdecodeValidationAuxiliaryWitnessV1 auxiliary

pinitialValueAccumulator :: forall s. Term s PValueAccumulatorV1
pinitialValueAccumulator = pcon $ PValueAccumulatorV1
  (pdata 0) (pdata pnull_hash) (pdata 0) (pdata 0)

pencodeValueAndMintControlV1 :: forall s. Term s (PValueAndMintControlV1 :--> PByteString)
pencodeValueAndMintControlV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  (pencodeDefiniteArrayHeader # 12)
    <> ( pencodeDefiniteBytes
          # (pencodeNativeScriptsControlV1 # pfromData (pvalueAndMint'nativeControl c))
       )
    <> pcborInt (pfromData $ pvalueAndMint'stage c)
    <> (pencodeDefiniteBytes # pfromData (pvalueAndMint'replayScheduleHash c))
    <> pcborInt (pfromData $ pvalueAndMint'replayCursor c)
    <> pcborInt (pfromData $ pvalueAndMint'replayAssetCursor c)
    <> (pencodeDefiniteBytes # pfromData (pvalueAndMint'replayValueHash c))
    <> (pencodeDefiniteBytes # pfromData (pvalueAndMint'replayAccumulator c))
    <> (pencodeDefiniteBytes # pfromData (pvalueAndMint'replayRemainingScheduleHash c))
    <> pcborInt (pfromData $ pvalueAndMint'outputCursor c)
    <> pcborInt (pfromData $ pvalueAndMint'outputAssetCursor c)
    <> pcborInt (pfromData $ pvalueAndMint'mintCursor c)
    <> ( pencodeDefiniteBytes
          # (pencodeValueAccumulatorV1 # pfromData (pvalueAndMint'valueAccumulator c))
       )

pvalueAndMintControlFromWitness :: forall s. Term s (PByteString :--> PValueAndMintControlV1)
pvalueAndMintControlFromWitness = phoistAcyclic $ plam $ \workWitnessCbor ->
  pmatch (pdeserialise # workWitnessCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \xs ->
      pif (plength # xs #== 12)
        ( pcon $ PValueAndMintControlV1
            (pdata $ pnativeScriptsControlFromWitness # (pasByteStr # (pelemAt # 0 # xs)))
            (pdata $ pasInt # (pelemAt # 1 # xs))
            (pdata $ pasByteStr # (pelemAt # 2 # xs))
            (pdata $ pasInt # (pelemAt # 3 # xs))
            (pdata $ pasInt # (pelemAt # 4 # xs))
            (pdata $ pasByteStr # (pelemAt # 5 # xs))
            (pdata $ pasByteStr # (pelemAt # 6 # xs))
            (pdata $ pasByteStr # (pelemAt # 7 # xs))
            (pdata $ pasInt # (pelemAt # 8 # xs))
            (pdata $ pasInt # (pelemAt # 9 # xs))
            (pdata $ pasInt # (pelemAt # 10 # xs))
            (pdata $ pvalueAccumulatorFromCbor # (pasByteStr # (pelemAt # 11 # xs)))
        )
        perror

pencodeValueAndMintWitnessV1 :: forall s. Term s (PNativeScriptsControlV1 :--> PByteString)
pencodeValueAndMintWitnessV1 = phoistAcyclic $ plam $ \nativeControl ->
  pencodeValueAndMintControlV1
    # ( pcon $ PValueAndMintControlV1
          (pdata nativeControl)
          (pdata 0)
          (pdata pemptyResolutionScheduleHash)
          (pdata 0)
          (pdata 0)
          (pdata $ preplicateBS # 32 # (pintegerToByte # 0))
          (pdata pinitialResolutionAccumulator)
          (pdata pemptyResolutionScheduleHash)
          (pdata 0)
          (pdata 0)
          (pdata 0)
          (pdata pinitialValueAccumulator)
      )

pimmutableContextMatches ::
  forall (s :: S).
  Term s PValidationMachineStateV1 ->
  Term s PValidationMachineStateV1 ->
  Term s PBool
pimmutableContextMatches pre post =
  pmatch pre $ \p -> pmatch post $ \q -> pand'List
    [ pmachineState'machineVersion p #== pmachineState'machineVersion q
    , pmachineState'eventKeyHash p #== pmachineState'eventKeyHash q
    , pmachineState'transactionId p #== pmachineState'transactionId q
    , pmachineState'transactionCommitment p #== pmachineState'transactionCommitment q
    , pmachineState'validationContextHash p #== pmachineState'validationContextHash q
    , pmachineState'sourceKind p #== pmachineState'sourceKind q
    , pmachineState'priorLedgerRoot p #== pmachineState'priorLedgerRoot q
    , pmachineState'ledgerDeltaRoot p #== pmachineState'ledgerDeltaRoot q
    ]

pphaseSuccessorIsValid ::
  forall (s :: S).
  Term s PValidationMachineStateV1 ->
  Term s PValidationMachineStateV1 ->
  Term s PBool
pphaseSuccessorIsValid pre post = pmatch pre $ \p -> pmatch post $ \q ->
  plet (pfromData $ pmachineState'phase p) $ \prePhase ->
  plet (pfromData $ pmachineState'phase q) $ \postPhase ->
  plet (pfromData $ pmachineState'verdict q) $ \postVerdict ->
    pif (postPhase #== pcon PTerminal)
      (postVerdict #/= pcon PPending #&& prePhase #/= pcon PTerminal)
      (postVerdict #== pcon PPending #&& pmatch prePhase (\case
        PCanonicalDecode -> postPhase #== pcon PCanonicalDecode #|| postPhase #== pcon PCompactBinding
        PCompactBinding -> postPhase #== pcon PStaticLedgerRules
        PStaticLedgerRules -> postPhase #== pcon PInputSets
        PInputSets -> postPhase #== pcon PSignatures
        PSignatures -> postPhase #== pcon PPhaseANativeScripts
        PPhaseANativeScripts -> postPhase #== pcon PPhaseAScriptPreconditions
        PPhaseAScriptPreconditions -> postPhase #== pcon PPhaseAScriptPreconditions #|| postPhase #== pcon PResolveInputs
        PResolveInputs -> postPhase #== pcon PResolveInputs #|| postPhase #== pcon PScriptSources
        PScriptSources -> postPhase #== pcon PScriptSources #|| postPhase #== pcon PNativeScripts
        PNativeScripts -> postPhase #== pcon PNativeScripts #|| postPhase #== pcon PScriptIntegrity
        PScriptIntegrity -> postPhase #== pcon PScriptIntegrity #|| postPhase #== pcon PCek
        PCek -> postPhase #== pcon PCek #|| postPhase #== pcon PValueAndMint
        PValueAndMint -> postPhase #== pcon PValueAndMint #|| postPhase #== pcon PLedgerDelta
        PLedgerDelta -> postPhase #== pcon PLedgerDelta
        PTerminal -> pconstant False))

pbudgetProgressIsValid ::
  forall (s :: S).
  Term s PValidationMachineStateV1 ->
  Term s PValidationMachineStateV1 ->
  Term s PBool
pbudgetProgressIsValid pre post = pmatch pre $ \p -> pmatch post $ \q ->
  pif (pfromData (pmachineState'phase p) #== pcon PCek)
    ( pfromData (pmachineState'executionCpu q) #>= pfromData (pmachineState'executionCpu p)
        #&& pfromData (pmachineState'executionMemory q) #>= pfromData (pmachineState'executionMemory p)
    )
    ( pmachineState'executionCpu q #== pmachineState'executionCpu p
        #&& pmachineState'executionMemory q #== pmachineState'executionMemory p
    )

pstructuralTransitionIsValid ::
  forall (s :: S).
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pstructuralTransitionIsValid = phoistAcyclic $ plam $ \pre witness ->
  pmatch witness $ \PValidationOneStepWitnessV1 {poneStep'workWitnessCbor, poneStep'claimedSuccessor} ->
  plet (pfromData poneStep'claimedSuccessor) $ \post ->
  pmatch pre $ \p -> pmatch post $ \q -> pand'List
    [ pmachineStateIsWellFormed # pre
    , pmachineStateIsWellFormed # post
    , pfromData (pmachineState'verdict p) #== pcon PPending
    , phashWorkWitness
        # pfromData (pmachineState'phase p)
        # pfromData (pmachineState'programCounter p)
        # pfromData poneStep'workWitnessCbor
        #== pfromData (pmachineState'workRoot p)
    , pfromData (pmachineState'programCounter q) #== pfromData (pmachineState'programCounter p) + 1
    , pimmutableContextMatches pre post
    , pphaseSuccessorIsValid pre post
    , pbudgetProgressIsValid pre post
    ]

pencodeCompactBindingWitness ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PByteString :--> PByteString :--> PByteString :--> PByteString :--> PByteString)
pencodeCompactBindingWitness = phoistAcyclic $ plam $
  \transactionId transactionCommitment compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor contextCbor ->
    pconstant "\x86"
      <> (pencodeDefiniteBytes # transactionId)
      <> (pencodeDefiniteBytes # transactionCommitment)
      <> (pencodeDefiniteBytes # compactCbor)
      <> (pencodeDefiniteBytes # witnessSetCompactCbor)
      <> (pencodeDefiniteBytes # fieldPreimageLengthsCbor)
      <> (pencodeDefiniteBytes # contextCbor)

{- | The highest field index the nine-field transaction scan can be positioned
at. Aiken writes the literal @8@ inline; it is named here because the same bound
appears in the field-preimage encoders.
-}
pmaxTransactionFieldIndex :: forall (s :: S). Term s PInteger
pmaxTransactionFieldIndex = 8

-- | Aiken @validation_machine_v1.encode_transaction_field_scan_witness@.
pencodeTransactionFieldScanWitness ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PByteString
    )
pencodeTransactionFieldScanWitness = phoistAcyclic $
  plam $
    \compactCbor
     witnessSetCompactCbor
     fieldPreimageLengthsCbor
     contextCbor
     fieldIndex
     itemIndex
     chunkIndex
     itemCount
     encodedLength ->
        pif
          ( fieldIndex
              #>= 0
              #&& fieldIndex
              #<= pmaxTransactionFieldIndex
              #&& itemIndex
              #>= 0
              #&& chunkIndex
              #>= 0
              #&& itemCount
              #>= -1
              #&& encodedLength
              #>= 0
          )
          ( pconstant "\x89"
              <> (pencodeDefiniteBytes # compactCbor)
              <> (pencodeDefiniteBytes # witnessSetCompactCbor)
              <> (pencodeDefiniteBytes # fieldPreimageLengthsCbor)
              <> (pencodeDefiniteBytes # contextCbor)
              <> pcborInt fieldIndex
              <> pcborInt itemIndex
              <> pcborInt chunkIndex
              <> pcborInt itemCount
              <> pcborInt encodedLength
          )
          perror

pencodeStaticRulesWitness :: forall s.
  Term s (PByteString :--> PByteString :--> PByteString :--> PByteString :--> PByteString)
pencodeStaticRulesWitness = phoistAcyclic $ plam $
  \compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor contextCbor ->
    pconstant "\x84"
      <> (pencodeDefiniteBytes # compactCbor)
      <> (pencodeDefiniteBytes # witnessSetCompactCbor)
      <> (pencodeDefiniteBytes # fieldPreimageLengthsCbor)
      <> (pencodeDefiniteBytes # contextCbor)

pencodeInputSetsScanWitness :: forall s.
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PByteString
    )
pencodeInputSetsScanWitness = phoistAcyclic $ plam $
  \compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor contextCbor
   spendCount referenceCount spendSeen referenceSeen previousKey resolutionScheduleHash ->
    pif
      ( spendCount #>= (-1)
          #&& referenceCount #>= (-1)
          #&& spendSeen #>= 0
          #&& referenceSeen #>= 0
          #&& (plengthBS # previousKey #== 0 #|| plengthBS # previousKey #== 38)
          #&& plengthBS # resolutionScheduleHash #== 32
      )
      ( (pencodeDefiniteArrayHeader # 10)
          <> (pencodeDefiniteBytes # compactCbor)
          <> (pencodeDefiniteBytes # witnessSetCompactCbor)
          <> (pencodeDefiniteBytes # fieldPreimageLengthsCbor)
          <> (pencodeDefiniteBytes # contextCbor)
          <> pcborInt spendCount
          <> pcborInt referenceCount
          <> pcborInt spendSeen
          <> pcborInt referenceSeen
          <> (pencodeDefiniteBytes # previousKey)
          <> (pencodeDefiniteBytes # resolutionScheduleHash)
      )
      perror

pencodePhaseANativeScriptsScanWitness :: forall s.
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PByteString
        :--> PInteger
        :--> PByteString
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PByteString
        :--> PByteString
    )
pencodePhaseANativeScriptsScanWitness = phoistAcyclic $ plam $
  \compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor contextCbor
   resolutionScheduleHash stage scriptCount scriptSeen containsNonNativeScript
   itemLength itemCommitment cursor stackRoot stackDepth nodeCount result signerCount
   signerPeaks continuationCbor ->
    pif
      ( plengthBS # resolutionScheduleHash #== 32
          #&& stage #>= 0
          #&& stage #<= 8
          #&& scriptCount #>= (-1)
          #&& scriptSeen #>= 0
          #&& (containsNonNativeScript #== 0 #|| containsNonNativeScript #== 1)
          #&& itemLength #>= 0
          #&& (plengthBS # itemCommitment #== 0 #|| plengthBS # itemCommitment #== 32)
          #&& cursor #>= 0
          #&& (plengthBS # stackRoot #== 0 #|| plengthBS # stackRoot #== 32)
          #&& stackDepth #>= 0
          #&& nodeCount #>= 0
          #&& result #>= (-1)
          #&& result #<= 1
          #&& signerCount #>= 0
      )
      ( pconstant "\x9f"
          <> (pencodeDefiniteBytes # compactCbor)
          <> (pencodeDefiniteBytes # witnessSetCompactCbor)
          <> (pencodeDefiniteBytes # fieldPreimageLengthsCbor)
          <> (pencodeDefiniteBytes # contextCbor)
          <> (pencodeDefiniteBytes # resolutionScheduleHash)
          <> pcborInt stage
          <> pcborInt scriptCount
          <> pcborInt scriptSeen
          <> pcborInt containsNonNativeScript
          <> pcborInt itemLength
          <> (pencodeDefiniteBytes # itemCommitment)
          <> pcborInt cursor
          <> (pencodeDefiniteBytes # stackRoot)
          <> pcborInt stackDepth
          <> pcborInt nodeCount
          <> pcborInt result
          <> pcborInt signerCount
          <> (pencodeDefiniteArrayHeader # 2)
          <> (pencodeFrontier # signerPeaks)
          <> (pencodeDefiniteBytes # continuationCbor)
          <> pconstant "\xff"
      )
      perror

pencodeSignaturesScanWitness :: forall s.
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PInteger
        :--> PByteString
    )
pencodeSignaturesScanWitness = phoistAcyclic $ plam $
  \compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor contextCbor
   resolutionScheduleHash stage addressCount requiredCount addressSeen requiredSeen
   previousOrderKey previousSignerHash signerCount signerPeaks invalidSignatureSeen ->
    pif
      ( plengthBS # resolutionScheduleHash #== 32
          #&& stage #>= 0
          #&& stage #<= 2
          #&& addressCount #>= (-1)
          #&& requiredCount #>= (-1)
          #&& addressSeen #>= 0
          #&& requiredSeen #>= 0
          #&& (plengthBS # previousSignerHash #== 0 #|| plengthBS # previousSignerHash #== 28)
          #&& signerCount #>= 0
          #&& pfrontierIsWellFormed # signerCount # signerPeaks
          #&& (invalidSignatureSeen #== 0 #|| invalidSignatureSeen #== 1)
      )
      ( (pencodeDefiniteArrayHeader # 15)
          <> (pencodeDefiniteBytes # compactCbor)
          <> (pencodeDefiniteBytes # witnessSetCompactCbor)
          <> (pencodeDefiniteBytes # fieldPreimageLengthsCbor)
          <> (pencodeDefiniteBytes # contextCbor)
          <> (pencodeDefiniteBytes # resolutionScheduleHash)
          <> pcborInt stage
          <> pcborInt addressCount
          <> pcborInt requiredCount
          <> pcborInt addressSeen
          <> pcborInt requiredSeen
          <> (pencodeDefiniteBytes # previousOrderKey)
          <> (pencodeDefiniteBytes # previousSignerHash)
          <> pcborInt signerCount
          <> (pencodeFrontier # signerPeaks)
          <> pcborInt invalidSignatureSeen
      )
      perror

pencodePhaseAScriptPreconditionsWitness :: forall s.
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PInteger
        :--> PByteString
        :--> PBool
        :--> PInteger
        :--> PInteger
        :--> PByteString
        :--> PByteString
    )
pencodePhaseAScriptPreconditionsWitness = phoistAcyclic $ plam $
  \compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor contextCbor
   resolutionScheduleHash signerCount signerFrontierCommitment hasNonNativeScript
   observerCount observerSeen previousObserver ->
    pif
      ( plengthBS # resolutionScheduleHash #== 32
          #&& signerCount #>= 0
          #&& plengthBS # signerFrontierCommitment #== 32
          #&& observerCount #>= 0
          #&& observerSeen #>= 0
          #&& (plengthBS # previousObserver #== 0 #|| plengthBS # previousObserver #== 28)
      )
      ( (pencodeDefiniteArrayHeader # 11)
          <> (pencodeDefiniteBytes # compactCbor)
          <> (pencodeDefiniteBytes # witnessSetCompactCbor)
          <> (pencodeDefiniteBytes # fieldPreimageLengthsCbor)
          <> (pencodeDefiniteBytes # contextCbor)
          <> (pencodeDefiniteBytes # resolutionScheduleHash)
          <> pcborInt signerCount
          <> (pencodeDefiniteBytes # signerFrontierCommitment)
          <> pcborInt (pif hasNonNativeScript 1 0)
          <> (pencodeDefiniteBytes # previousObserver)
          <> pcborInt observerCount
          <> pcborInt observerSeen
      )
      perror

pencodeTerminalRejectionWitness :: forall s.
  Term s (PByteString :--> PByteString :--> PByteString)
pencodeTerminalRejectionWitness = phoistAcyclic $ plam $ \rejectionCode priorLedgerRoot ->
  pconstant "\x84"
    <> pcborInt 2
    <> (pencodeDefiniteBytes # rejectionCode)
    <> (pencodeDefiniteBytes # priorLedgerRoot)
    <> (pencodeDefiniteBytes # pconstant "\x80")

pvalidityIntervalIsMalformed :: forall s.
  Term s (PInteger :--> PInteger :--> PBool)
pvalidityIntervalIsMalformed = phoistAcyclic $ plam $ \start end ->
  start #< (-1)
    #|| end #< (-1)
    #|| (start #>= 0 #&& end #>= 0 #&& start #> end)

predeemerTagForPurposeKindV1 :: forall s.
  Term s (PInteger :--> PMaybe PInteger)
predeemerTagForPurposeKindV1 = phoistAcyclic $ plam $ \kind ->
  pif (kind #== 0) (pcon $ PJust 0) $
  pif (kind #== 1) (pcon $ PJust 1) $
  pif (kind #== 2) (pcon $ PJust 3) $
  pif (kind #== 3) (pcon $ PJust 6) (pcon PNothing)

predeemerPointerMatchesPurposeV1 :: forall s.
  Term s (PInteger :--> PInteger :--> PInteger :--> PInteger :--> PBool)
predeemerPointerMatchesPurposeV1 = phoistAcyclic $ plam $ \kind purposeIndex redeemerTag redeemerIndex ->
  pmatch (predeemerTagForPurposeKindV1 # kind) $ \case
    PNothing -> pconstant False
    PJust expectedTag -> redeemerTag #== expectedTag #&& redeemerIndex #== purposeIndex

pemptyObserverPurposeScanControl :: forall s. Term s PObserverPurposeScanControlV1
pemptyObserverPurposeScanControl = pcon $ PObserverPurposeScanControlV1
  (pdata 0) (pdata 0) (pdata $ pconstant "")

pencodeObserverPurposeScanControl :: forall s.
  Term s (PObserverPurposeScanControlV1 :--> PByteString)
pencodeObserverPurposeScanControl = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  plet (pfromData $ pobserverScan'totalCount c) $ \total ->
  plet (pfromData $ pobserverScan'seen c) $ \seen ->
  plet (pfromData $ pobserverScan'previousHash c) $ \previous ->
    pif
      ( total #>= 0
          #&& seen #>= 0
          #&& seen #<= total
          #&& pif (seen #== 0) (previous #== pconstant "") (plengthBS # previous #== 28)
      )
      (pconstant "\x83" <> pcborInt total <> (pencodeDefiniteBytes # previous) <> pcborInt seen)
      perror

pemptyReceivePurposeScanControl :: forall s. Term s PReceivePurposeScanControlV1
pemptyReceivePurposeScanControl = pcon $ PReceivePurposeScanControlV1
  (pdata 0) (pdata pnil) (pdata 0) (pdata $ pconstant "")
  (pdata $ pconstant "") (pdata pnil)

pencodeReceivePurposeScanControl :: forall s.
  Term s (PReceivePurposeScanControlV1 :--> PByteString)
pencodeReceivePurposeScanControl = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  plet (pfromData $ preceiveScan'sourceCount c) $ \sourceCount ->
  plet (pfromData $ preceiveScan'sourcePeaks c) $ \sourcePeaks ->
  plet (pfromData $ preceiveScan'receiveCount c) $ \receiveCount ->
  plet (pfromData $ preceiveScan'previousHash c) $ \previous ->
  plet (pfromData $ preceiveScan'candidateHash c) $ \candidate ->
  plet (pfromData $ preceiveScan'descriptorPeaks c) $ \descriptorPeaks ->
    pif
      ( sourceCount #>= 0
          #&& pfrontierIsWellFormed # sourceCount # sourcePeaks
          #&& receiveCount #>= 0
          #&& pif (receiveCount #== 0) (previous #== pconstant "") (plengthBS # previous #== 28)
          #&& ( candidate #== pconstant ""
                  #|| ( plengthBS # candidate #== 28
                          #&& (previous #== pconstant "" #|| previous #< candidate)
                      )
              )
      )
      ( pconstant "\x86"
          <> pcborInt sourceCount
          <> (pencodeFrontier # sourcePeaks)
          <> pcborInt receiveCount
          <> (pencodeDefiniteBytes # previous)
          <> (pencodeDefiniteBytes # candidate)
          <> (pencodeFrontier # descriptorPeaks)
      )
      perror

pemptyMintFoldControl :: forall s. Term s PMintFoldControlV1
pemptyMintFoldControl = pcon $ PMintFoldControlV1
  (pdata $ pconstant (-1)) (pdata 0) (pdata $ pconstant "")
  (pdata $ pconstant "") (pdata 0) (pdata $ pconstant "")
  (pdata 0) (pdata 0) (pdata 0) (pdata $ pconstant "")
  (pdata 0) (pdata pnil)

pencodeMintFoldControl :: forall s. Term s (PMintFoldControlV1 :--> PByteString)
pencodeMintFoldControl = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  (pencodeDefiniteArrayHeader # 12)
    <> pcborInt (pfromData $ pmintFold'policyCount c)
    <> pcborInt (pfromData $ pmintFold'policyCursor c)
    <> (pencodeDefiniteBytes # pfromData (pmintFold'previousPolicy c))
    <> (pencodeDefiniteBytes # pfromData (pmintFold'activePolicy c))
    <> pcborInt (pfromData $ pmintFold'itemLength c)
    <> (pencodeDefiniteBytes # pfromData (pmintFold'itemCommitment c))
    <> pcborInt (pfromData $ pmintFold'itemCursor c)
    <> pcborInt (pfromData $ pmintFold'assetsRemaining c)
    <> pcborInt (pfromData $ pmintFold'policyAssetCursor c)
    <> (pencodeDefiniteBytes # pfromData (pmintFold'previousAsset c))
    <> pcborInt (pfromData $ pmintFold'assetCount c)
    <> (pencodeFrontier # pfromData (pmintFold'assetPeaks c))

pencodeScriptSourcesWitness :: forall s.
  Term s
    ( PByteString :--> PByteString :--> PByteString :--> PByteString
        :--> PInteger :--> PByteString :--> PInteger :--> PByteString
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PInteger :--> PInteger :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PInteger :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PInteger :--> PByteString :--> PByteString :--> PInteger
        :--> PInteger :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PInteger :--> PInteger :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PInteger :--> PReceivePurposeScanControlV1
        :--> PInteger :--> PInteger :--> PObserverPurposeScanControlV1
        :--> PMintFoldControlV1 :--> PByteString :--> PByteString
    )
pencodeScriptSourcesWitness = phoistAcyclic $ plam $
  \compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor contextCbor
   resolvedInputCount resolvedInputsAccumulator signerCount signerFrontierCommitment
   resolvedItemPeaks stage sourceCount sourcePeaks redeemerCount redeemerPeaks
   replayCursor replayAccumulator replayRemainingScheduleHash spendIndex purposeCount
   purposePeaks outputCursor outputCount outputPeaks outputTotalCount receiveScan
   sourceTotalCount redeemerTotalCount observerScan mintFold resolutionScheduleHash ->
  pif
    ( pand'List
        [ resolvedInputCount #>= 0
        , plengthBS # resolvedInputsAccumulator #== 32
        , signerCount #>= 0
        , plengthBS # signerFrontierCommitment #== 32
        , pfrontierIsWellFormed # replayCursor # resolvedItemPeaks
        , stage #>= 0
        , sourceCount #>= 0
        , pfrontierIsWellFormed # sourceCount # sourcePeaks
        , redeemerCount #>= 0
        , pfrontierIsWellFormed # redeemerCount # redeemerPeaks
        , replayCursor #>= 0
        , plengthBS # replayAccumulator #== 32
        , plengthBS # replayRemainingScheduleHash #== 32
        , spendIndex #>= 0
        , purposeCount #>= 0
        , pfrontierIsWellFormed # purposeCount # purposePeaks
        , outputCursor #>= 0
        , outputCount #>= 0
        , pfrontierIsWellFormed # outputCount # outputPeaks
        , outputCursor #<= outputCount
        , outputTotalCount #>= outputCount
        , sourceTotalCount #>= sourceCount
        , redeemerTotalCount #>= redeemerCount
        , plengthBS # resolutionScheduleHash #== 32
        ]
    )
    ( (pencodeDefiniteArrayHeader # 30)
        <> (pencodeDefiniteBytes # compactCbor)
        <> (pencodeDefiniteBytes # witnessSetCompactCbor)
        <> (pencodeDefiniteBytes # fieldPreimageLengthsCbor)
        <> (pencodeDefiniteBytes # contextCbor)
        <> pcborInt resolvedInputCount
        <> (pencodeDefiniteBytes # resolvedInputsAccumulator)
        <> pcborInt signerCount
        <> (pencodeDefiniteBytes # signerFrontierCommitment)
        <> (pencodeFrontier # resolvedItemPeaks)
        <> pcborInt stage
        <> pcborInt sourceCount
        <> (pencodeFrontier # sourcePeaks)
        <> pcborInt redeemerCount
        <> (pencodeFrontier # redeemerPeaks)
        <> pcborInt replayCursor
        <> (pencodeDefiniteBytes # replayAccumulator)
        <> (pencodeDefiniteBytes # replayRemainingScheduleHash)
        <> pcborInt spendIndex
        <> pcborInt purposeCount
        <> (pencodeFrontier # purposePeaks)
        <> pcborInt outputCursor
        <> pcborInt outputCount
        <> (pencodeFrontier # outputPeaks)
        <> pcborInt outputTotalCount
        <> (pencodeReceivePurposeScanControl # receiveScan)
        <> pcborInt sourceTotalCount
        <> pcborInt redeemerTotalCount
        <> (pencodeObserverPurposeScanControl # observerScan)
        <> (pencodeMintFoldControl # mintFold)
        <> (pencodeDefiniteBytes # resolutionScheduleHash)
    )
    perror

pinitialResolutionAccumulator :: forall s. Term s PByteString
pinitialResolutionAccumulator = pblake2b_256 # presolutionAccumulatorDomain

pemptyResolutionScheduleHash :: forall s. Term s PByteString
pemptyResolutionScheduleHash = pblake2b_256 # presolutionScheduleDomain

presolutionScheduleNodeHash :: forall s.
  Term s (PInteger :--> PByteString :--> PByteString :--> PByteString)
presolutionScheduleNodeHash = phoistAcyclic $ plam $ \sourceKind key nextHash ->
  pif
    ((sourceKind #== 0 #|| sourceKind #== 1) #&& plengthBS # nextHash #== 32)
    ( pblake2b_256
        #$ presolutionScheduleDomain
          <> pcborInt sourceKind
          <> (pencodeDefiniteBytes # key)
          <> nextHash
    )
    perror

pscheduledInputResolutionHash :: forall s.
  Term s (PBuiltinList (PAsData PScheduledInputV1) :--> PByteString)
pscheduledInputResolutionHash = phoistAcyclic $ pfix $ \self -> plam $ \inputs ->
  pelimList
    (\input rest -> pmatch (pfromData input) $ \scheduled ->
      presolutionScheduleNodeHash
        # pfromData (pscheduledInput'sourceKind scheduled)
        # pfromData (pscheduledInput'key scheduled)
        # (self # rest))
    pemptyResolutionScheduleHash
    inputs

pinsertScheduledInput :: forall s.
  Term
    s
    ( PAsData PScheduledInputV1
        :--> PBuiltinList (PAsData PScheduledInputV1)
        :--> PBuiltinList (PAsData PScheduledInputV1)
    )
pinsertScheduledInput = phoistAcyclic $ pfix $ \self -> plam $ \input inputs ->
  pelimList
    (\headInput rest ->
      pmatch (pfromData input) $ \candidate -> pmatch (pfromData headInput) $ \headScheduled ->
      plet (pfromData $ pscheduledInput'key candidate) $ \candidateKey ->
      plet (pfromData $ pscheduledInput'key headScheduled) $ \headKey ->
        pif
          (candidateKey #< headKey #|| candidateKey #== headKey)
          (pcons # input # inputs)
          (pcons # headInput # (self # input # rest)))
    (pcons # input # pnil)
    inputs

psortScheduledInputs :: forall s.
  Term s (PBuiltinList (PAsData PScheduledInputV1) :--> PBuiltinList (PAsData PScheduledInputV1))
psortScheduledInputs = phoistAcyclic $ pfix $ \self -> plam $ \inputs ->
  pelimList
    (\input rest -> pinsertScheduledInput # input # (self # rest))
    pnil
    inputs

pmapScheduledInputs :: forall s.
  Term
    s
    ( PInteger
        :--> PBuiltinList (PAsData PMidgardTxInput)
        :--> PBuiltinList (PAsData PScheduledInputV1)
    )
pmapScheduledInputs = phoistAcyclic $ pfix $ \self -> plam $ \sourceKind inputs ->
  pelimList
    (\input rest ->
      pcons
        # pdata (pcon $ PScheduledInputV1 (pdata sourceKind) (pdata $ pencodeMidgardTxInput # pfromData input))
        # (self # sourceKind # rest))
    pnil
    inputs

pappendScheduledInputs :: forall s.
  Term
    s
    ( PBuiltinList (PAsData PScheduledInputV1)
        :--> PBuiltinList (PAsData PScheduledInputV1)
        :--> PBuiltinList (PAsData PScheduledInputV1)
    )
pappendScheduledInputs = phoistAcyclic $ pfix $ \self -> plam $ \left right ->
  pelimList (\input rest -> pcons # input # (self # rest # right)) right left

ptransactionResolutionScheduleHash :: forall s.
  Term
    s
    ( PBuiltinList (PAsData PMidgardTxInput)
        :--> PBuiltinList (PAsData PMidgardTxInput)
        :--> PByteString
    )
ptransactionResolutionScheduleHash = phoistAcyclic $ plam $ \spendInputs referenceInputs ->
  pscheduledInputResolutionHash
    # ( psortScheduledInputs
          # ( pappendScheduledInputs
                # (pmapScheduledInputs # 0 # spendInputs)
                # (pmapScheduledInputs # 1 # referenceInputs)
            )
      )

pencodeResolveInputOutputProof :: forall s.
  Term s (PResolveInputOutputProofV1 :--> PByteString)
pencodeResolveInputOutputProof = phoistAcyclic $ plam $ \pending -> pmatch pending $ \proof ->
  pconstant "\x85"
    <> pcborInt (pfromData $ presolveOutputProof'sourceKind proof)
    <> (pencodeDefiniteBytes # pfromData (presolveOutputProof'key proof))
    <> (pencodeDefiniteBytes # pfromData (presolveOutputProof'nextScheduleHash proof))
    <> (pencodeDefiniteBytes # pfromData (presolveOutputProof'descriptorCbor proof))
    <> ( pencodeDefiniteBytes
          # (LedgerOutputProof.pencodeControlV1 # pfromData (presolveOutputProof'outputProof proof))
       )

pencodeOptionalResolveInputOutputProof :: forall s.
  Term s (PMaybeData PResolveInputOutputProofV1 :--> PByteString)
pencodeOptionalResolveInputOutputProof = phoistAcyclic $ plam $ \pending -> pmatch pending $ \case
  PDNothing -> pencodeDefiniteBytes # pconstant "\x00"
  PDJust active -> pencodeDefiniteBytes # (pencodeResolveInputOutputProof # pfromData active)

pencodeResolveInputsWitness :: forall s.
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PByteString
        :--> PByteString
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PInteger
        :--> PByteString
        :--> PMaybeData PResolveInputOutputProofV1
        :--> PByteString
        :--> PByteString
    )
pencodeResolveInputsWitness = phoistAcyclic $ plam $
  \compactCbor witnessSetCompactCbor fieldPreimageLengthsCbor contextCbor cursor
   accumulator remainingScheduleHash signerCount signerFrontierCommitment pending
   resolutionScheduleHash ->
    pif
      ( cursor #>= 0
          #&& plengthBS # accumulator #== 32
          #&& plengthBS # remainingScheduleHash #== 32
          #&& signerCount #>= 0
          #&& plengthBS # signerFrontierCommitment #== 32
          #&& plengthBS # resolutionScheduleHash #== 32
      )
      ( pconstant "\x8b"
          <> (pencodeDefiniteBytes # compactCbor)
          <> (pencodeDefiniteBytes # witnessSetCompactCbor)
          <> (pencodeDefiniteBytes # fieldPreimageLengthsCbor)
          <> (pencodeDefiniteBytes # contextCbor)
          <> pcborInt cursor
          <> (pencodeDefiniteBytes # accumulator)
          <> (pencodeDefiniteBytes # remainingScheduleHash)
          <> pcborInt signerCount
          <> (pencodeDefiniteBytes # signerFrontierCommitment)
          <> (pencodeOptionalResolveInputOutputProof # pending)
          <> (pencodeDefiniteBytes # resolutionScheduleHash)
      )
      perror

presolvedInputAccumulatorSuccessor :: forall s.
  Term s (PByteString :--> PInteger :--> PByteString :--> PByteString :--> PByteString)
presolvedInputAccumulatorSuccessor = phoistAcyclic $ plam $ \accumulator sourceKind key value ->
  pblake2b_256
    #$ presolutionAccumulatorDomain
      <> accumulator
      <> pcborInt sourceKind
      <> (pencodeDefiniteBytes # key)
      <> (pencodeDefiniteBytes # value)

pmintAssetLeafHash :: forall s.
  Term s (PByteString :--> PByteString :--> PInteger :--> PByteString)
pmintAssetLeafHash = phoistAcyclic $ plam $ \policyId assetName quantity ->
  pif
    (plengthBS # policyId #== 28 #&& plengthBS # assetName #<= 32 #&& pnot # (quantity #== 0))
    ( pblake2b_256
        #$ pmintAssetLeafDomain
          <> (pencodeDefiniteBytes # policyId)
          <> (pencodeDefiniteBytes # assetName)
          <> pcborInt quantity
    )
    perror

pledgerDeltaOperationLeafHash :: forall s.
  Term s (PInteger :--> PByteString :--> PByteString :--> ProofFold.PProofDescriptorV1 :--> PByteString)
pledgerDeltaOperationLeafHash = phoistAcyclic $ plam $ \operationKind key value descriptor ->
  pif
    ( (operationKind #== 0 #|| operationKind #== 1)
        #&& ProofFold.pdescriptorIsWellFormedV1 # descriptor
        #&& pif (operationKind #== 0) (value #== pconstant "") (pconstant True)
    )
    ( pblake2b_256
        #$ pledgerDeltaOperationDomain
          <> pcborInt operationKind
          <> (pencodeDefiniteBytes # key)
          <> (pencodeDefiniteBytes # value)
          <> (ProofFold.pencodeProofDescriptorV1 # descriptor)
    )
    perror
