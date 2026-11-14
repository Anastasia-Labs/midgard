import {
  type FraudProofCatalogueCategoryName,
  PROOF_THREAD_SOURCE_KIND_FORCED,
} from "@al-ft/midgard-sdk";

import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import {
  type ManifestBoundOutputReferenceScriptDecodingConfig,
  type OutputReferenceScriptDecodingAuthenticatedSource,
  type OutputReferenceScriptDecodingRuntimeLoader,
  required,
} from "./authenticated-workflow.create-output-reference-script-decoding-bound-config.js";

export const createOutputReferenceScriptDecodingRawL1StageResolver =
  ({
    config,
    l1,
    source,
  }: {
    readonly config: ManifestBoundOutputReferenceScriptDecodingConfig;
    readonly l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
    readonly source: OutputReferenceScriptDecodingAuthenticatedSource;
  }): OutputReferenceScriptDecodingRuntimeLoader["resolveStage"] =>
  async ({ action, evidence, currentStage }) => {
    const observed = await l1.observe({
      headerHash: config.binding.definition.headerHash,
    });
    const stage = observed.stage;
    if (action === "submitInit") {
      if (stage.kind !== "not_started") {
        throw new Error(
          "outputReferenceScriptDecoding init requires raw-L1 not_started",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    if (action === "removeDescendants") {
      if (stage.kind !== "proof_token") {
        throw new Error(
          "outputReferenceScriptDecoding removal requires raw-L1 proof token",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    const effectiveAction =
      action === "cancel"
        ? currentStage === "step01"
          ? "submitStep01"
          : currentStage === "step02"
            ? "submitStep02"
            : currentStage === "outputScan"
              ? "submitOutputScan"
              : currentStage === "referenceBind"
                ? "submitReferenceBind"
                : currentStage === "scan"
                  ? "submitStructuralScan"
                  : (() => {
                      throw new Error(
                        "outputReferenceScriptDecoding cancel stage is not authenticated",
                      );
                    })()
        : action;
    const expectedStep =
      effectiveAction === "submitStep01"
        ? 1
        : effectiveAction === "submitStep02"
          ? 2
          : effectiveAction === "submitOutputScan"
            ? 3
            : effectiveAction === "submitReferenceBind"
              ? 4
              : effectiveAction === "submitStructuralScan"
                ? 5
                : 6;
    if (stage.kind !== "step" || stage.step !== expectedStep) {
      throw new Error(
        `outputReferenceScriptDecoding ${action} differs from authenticated raw-L1 stage`,
      );
    }
    const thread = await requireLinearFaultThreadUtxo({
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId: config.binding.resolvedContracts.category.categoryId,
      family: "output-reference-script-decoding",
      stepIndex: expectedStep - 1,
      threadOutRef: stage.threadOutRef,
    });
    const common = {
      fraudulentBlockOutRef: stage.stateQueueBlockOutRef,
      threadOutRef: stage.threadOutRef,
      threadUtxo: thread.threadUtxo,
      threadToken: thread.threadToken,
      nativeTxCompactCbor: source.nativeTxCompactCbor,
      witnessSetCompactCbor: source.witnessSetCompactCbor,
    };
    if (action !== "submitStep01") return common;
    if (evidence.subject.source_kind === PROOF_THREAD_SOURCE_KIND_FORCED) {
      return {
        ...common,
        forcedHeader: required(
          source.forcedHeader,
          "authenticated forced header",
        ),
        forcedMembership: required(
          source.forcedMembership,
          "authenticated forced membership",
        ),
        forcedDirection: required(
          source.forcedDirection,
          "authenticated forced direction",
        ),
      };
    }
    return {
      ...common,
      threadUtxo: thread.threadUtxo,
      threadToken: thread.threadToken,
      stateQueueBlockOutRef: stage.stateQueueBlockOutRef,
      acceptedInclusion: required(
        source.acceptedInclusion,
        "authenticated accepted inclusion",
      ),
    };
  };
