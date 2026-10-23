/** The validation-trace dispute's funding terms come from the signed
 * deployment: the thread moves' terms name the boundary and each one-step
 * resolver a thread visits between the game and the award by the address
 * of the script the manifest applies under that holder's name, and a
 * policy for any other category carries no dispute terms. */
import { VALIDATION_TRACE_ONE_STEP_CONTRACT_NAMES } from "@al-ft/midgard-fault-proofs";
import {
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { watcherValidationDisputeFundingTerms } from "../../src/funding/prover-funding-authority.js";
import { watcherDeploymentAppliedScriptHashes } from "../../src/runtime/deployment-identity.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";

const identity = makeWatcherDeploymentAuthorityFixture().result;

/** The address of the script the signed manifest applies under `name`. */
const manifestAddress = (name: string) => {
  const scriptHash = watcherDeploymentAppliedScriptHashes(identity)[name];
  if (scriptHash === undefined) throw new Error(`no ${name}`);
  return credentialToAddress(
    identity.network,
    scriptHashToCredential(scriptHash),
  );
};

describe("validation-trace dispute funding terms from the signed deployment", () => {
  it("names the boundary and each one-step resolver by the manifest's applied script", () => {
    const { validationDispute } = watcherValidationDisputeFundingTerms(
      identity,
      "validationTraceDispute",
    );
    const oneStep = validationDispute?.moves?.oneStep;
    expect(oneStep?.boundaryAddress).toBe(
      manifestAddress("validationTraceDisputeBoundary"),
    );
    expect(oneStep?.resolverAddresses).toStrictEqual(
      [
        "validationTraceDisputeCekPrepare",
        "validationTraceDisputeCekCoreStepSemantic",
        "validationTraceDisputeCekCoreCompute",
        "validationTraceDisputeCekCoreMachine",
        "validationTraceDisputeCekCoreMapConversion",
        "validationTraceDisputeCekCoreDirectScalarRoots",
        "validationTraceDisputeCekCoreDirectScalarBudget",
        "validationTraceDisputeCekCoreDirectScalar",
        "validationTraceDisputeCekCoreDirectStructuredRoots",
        "validationTraceDisputeCekCoreDirectStructuredBudget",
        "validationTraceDisputeCekCoreDirectStructured",
        "validationTraceDisputeCekCoreSemanticRoots",
        "validationTraceDisputeCekCoreSemanticResult",
        "validationTraceDisputeCekCoreSemanticBudget",
        "validationTraceDisputeCekCoreMapStartRoots",
        "validationTraceDisputeCekCoreMapStartBudget",
        "validationTraceDisputeCekCoreMapStartNodes",
        "validationTraceDisputeCekCoreSemanticFailureRoots",
        "validationTraceDisputeCekCoreSemanticFailureMaterial",
        "validationTraceDisputeCekCoreBlsBudget",
        "validationTraceDisputeCekCoreBlsRoots",
        "validationTraceDisputeCekCoreBlsFinal",
        "validationTraceDisputeCekCoreFailureKnown",
        "validationTraceDisputeCekCoreFailureBudget",
        "validationTraceDisputeCekCoreTypeFailureRoots",
        "validationTraceDisputeCekCoreTypeFailureKinds",
        "validationTraceDisputeCekCoreCaseConstant",
        "validationTraceDisputeCekCoreSemanticPair",
        "validationTraceDisputeCekCoreSemanticListConstruct",
        "validationTraceDisputeCekCoreSemanticListSelect",
        "validationTraceDisputeCekCoreSemanticChoose",
        "validationTraceDisputeCekCoreSemanticDataConstruct",
        "validationTraceDisputeCekCoreSemanticDataScalar",
        "validationTraceDisputeCekCoreSemanticDataMisc",
        "validationTraceDisputeCekCoreStagedBytes",
        "validationTraceDisputeCekCoreStagedInteger",
        "validationTraceDisputeCekCoreSettle",
        "validationTraceDisputeCekExecutionSelectionSemantic",
        "validationTraceDisputeCekContextStepSemantic",
        "validationTraceDisputeCekContextControl",
        "validationTraceDisputeCekContextRedeemerBegin",
        "validationTraceDisputeCekContextItemBind",
        "validationTraceDisputeCekContextReference",
        "validationTraceDisputeCekContextSpend",
        "validationTraceDisputeCekContextOutput",
        "validationTraceDisputeCekContextSigner",
        "validationTraceDisputeCekContextObserverAuthenticate",
        "validationTraceDisputeCekContextMintInit",
        "validationTraceDisputeCekContextRedeemerSelectAuthenticate",
        "validationTraceDisputeCekContextFinalizeAuthenticate",
        "validationTraceDisputeCekContextAssemble",
        "validationTraceDisputeCekContextTxInfo",
        "validationTraceDisputeCekContextSeed",
        "validationTraceDisputeCekContextItemEntry",
        "validationTraceDisputeRedeemerItemTraversalNormalizer",
        "validationTraceDisputeRedeemerItemOuterNormalizer",
        "validationTraceDisputeRedeemerItemSourceAuthenticator",
        "validationTraceDisputeRedeemerItemFoldMapExecutor",
        "validationTraceDisputeRedeemerItemFinalizeFrameExecutor",
        "validationTraceDisputeRedeemerItemOpenHeaderExecutor",
        "validationTraceDisputeRedeemerItemOpenTailExecutor",
        "validationTraceDisputeRedeemerItemHeadScalarExecutor",
        "validationTraceDisputeRedeemerItemHeadSequenceExecutor",
        "validationTraceDisputeRedeemerItemHeadMapExecutor",
        "validationTraceDisputeRedeemerItemHeadLargeConstructorExecutor",
        "validationTraceDisputeRedeemerItemAttachIntegerExecutor",
        "validationTraceDisputeRedeemerItemAttachBytesExecutor",
        "validationTraceDisputeRedeemerItemFoldListExecutor",
        "validationTraceDisputeRedeemerItemAdvanceIntegerExecutor",
        "validationTraceDisputeRedeemerItemAdvanceBytesExecutor",
        "validationTraceDisputeRedeemerItemAdvanceLargeConstructorExecutor",
        "validationTraceDisputeRedeemerItemAdvanceLargeFieldsExecutor",
        "validationTraceDisputeRedeemerItemCloseExecutor",
        "validationTraceDisputeRedeemerItemFinishDataExecutor",
        "validationTraceDisputeCekContextItemSettlement",
        "validationTraceDisputeCekContextItemReturn",
        "validationTraceDisputeCekContextItemSelectionHash",
        "validationTraceDisputeCekContextItemSelectionFinish",
        "validationTraceDisputeCekContextItemDataHash",
        "validationTraceDisputeCekContextItemDataFinishDescriptor",
        "validationTraceDisputeCekContextItemFinalize",
        "validationTraceDisputeCekContextItemSelectionContinue",
        "validationTraceDisputeCekContextItemDataContinue",
        "validationTraceDisputeCekContextItemDataFinishValue",
        "validationTraceDisputeCekContextObserverFold",
        "validationTraceDisputeCekContextRedeemerSelectInitialize",
        "validationTraceDisputeCekContextRedeemerSelectHash",
        "validationTraceDisputeCekContextRedeemerSelectFinish",
        "validationTraceDisputeCekContextFinalizeSpend",
        "validationTraceDisputeCekContextSettle",
        "validationTraceDisputeCanonicalDecodePrepare",
        "validationTraceDisputeCanonicalDecodeEmptySemantic",
        "validationTraceDisputeCanonicalDecodeItemSemantic",
        "validationTraceDisputeCanonicalDecodeItemSource",
        "validationTraceDisputeCanonicalDecodeItemObserve",
        "validationTraceDisputeCanonicalDecodeItemProof",
        "validationTraceDisputeCanonicalDecodeItemSettlement",
        "validationTraceDisputeCompactBindingPrepare",
        "validationTraceDisputeCompactBindingSemantic",
        "validationTraceDisputeStaticLedgerRulesPrepare",
        "validationTraceDisputeStaticLedgerRulesSemantic",
        "validationTraceDisputeInputSetsPrepare",
        "validationTraceDisputeInputSetsEmptySemantic",
        "validationTraceDisputeInputSetsItemSemantic",
        "validationTraceDisputeSignaturesPrepare",
        "validationTraceDisputeSignaturesAdvanceSemantic",
        "validationTraceDisputeSignaturesAddressItemSemantic",
        "validationTraceDisputeSignaturesRequiredItemSemantic",
        "validationTraceDisputeSignaturesHandoffSemantic",
        "validationTraceDisputePhaseANativeScriptsPrepare",
        "validationTraceDisputePhaseANativeScriptsItemSemantic",
        "validationTraceDisputePhaseAScriptPreconditionsPrepare",
        "validationTraceDisputePhaseAScriptPreconditionsFinalizeSemantic",
        "validationTraceDisputeResolveInputsPrepare",
        "validationTraceDisputeResolveInputsInitialSemantic",
        "validationTraceDisputeResolveInputsFinishSemantic",
        "validationTraceDisputeResolveInputsMembershipBeginSemantic",
        "validationTraceDisputeResolveInputsMembershipStepSemantic",
        "validationTraceDisputeResolveInputsMembershipFinalizeSemantic",
        "validationTraceDisputeScriptSourcesPrepare",
        "validationTraceDisputeScriptSourcesNonOutputSemantic",
        "validationTraceDisputeScriptSourcesOutputProofBeginSemantic",
        "validationTraceDisputeScriptSourcesOutputProofStepSemantic",
        "validationTraceDisputeScriptSourcesOutputProofFinalizeSemantic",
        "validationTraceDisputeScriptSourcesOutputProofFinishSemantic",
        "validationTraceDisputeScriptSourcesStageZeroBeginSemantic",
        "validationTraceDisputeScriptSourcesStageZeroFinishSemantic",
        "validationTraceDisputeScriptSourcesStageZeroHashBlockSemantic",
        "validationTraceDisputeScriptSourcesStageZeroHashAdvanceSemantic",
        "validationTraceDisputeScriptSourcesStageZeroHashTerminalSemantic",
        "validationTraceDisputeScriptSourcesStageNineEffectfulMatchSemantic",
        "validationTraceDisputeScriptSourcesStageOneFinishSemantic",
        "validationTraceDisputeScriptSourcesStageOneRedeemerSemantic",
        "validationTraceDisputeScriptSourcesStageElevenFinishSemantic",
        "validationTraceDisputeScriptSourcesStageElevenSourceSemantic",
        "validationTraceDisputeScriptSourcesStageTwelveFinishSemantic",
        "validationTraceDisputeScriptSourcesStageTwelveRedeemerSemantic",
        "validationTraceDisputeScriptSourcesStageTenMismatchSemantic",
        "validationTraceDisputeScriptSourcesStageTenMatchSemantic",
        "validationTraceDisputeScriptSourcesStageEightFinishSemantic",
        "validationTraceDisputeScriptSourcesStageEightPurposeSemantic",
        "validationTraceDisputeScriptSourcesStageSevenFinishSemantic",
        "validationTraceDisputeScriptSourcesRedeemerNormalizationSemantic",
        "validationTraceDisputeRedeemerItemInvalidHeaderExecutor",
        "validationTraceDisputeRedeemerItemInvalidTailExecutor",
        "validationTraceDisputeRedeemerItemInvalidDataExecutor",
        "validationTraceDisputeRedeemerItemInvalidScalarDataExecutor",
        "validationTraceDisputeRedeemerItemSettlement",
      ].map(manifestAddress),
    );
    // Every holder the runner visits past the game is named exactly once.
    expect(
      new Set([oneStep?.boundaryAddress, ...(oneStep?.resolverAddresses ?? [])])
        .size,
    ).toBe(Object.keys(VALIDATION_TRACE_ONE_STEP_CONTRACT_NAMES).length);
  });

  it("carries no dispute terms for another category", () => {
    expect(
      watcherValidationDisputeFundingTerms(identity, "doubleSpend"),
    ).toStrictEqual({});
  });
});
