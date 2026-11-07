import { MidgardNativeScriptDecodingDirections } from "@al-ft/midgard-core";
import {
  type FraudProofCatalogueCategoryName,
  PROOF_THREAD_SOURCE_KIND_FORCED,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import {
  buildNativeScriptDecodingScanPlan,
  NativeScriptDecodingPlanRoutes,
} from "../native-script-decoding/scan-plan.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import {
  type ManifestBoundOutputReferenceScriptDecodingConfig,
  type OutputReferenceScriptDecodingAuthenticatedSource,
  type OutputReferenceScriptDecodingAuthenticatedStage,
  type OutputReferenceScriptDecodingRuntimeLoader,
  required,
} from "./authenticated-workflow.create-output-reference-script-decoding-bound-config.js";
import {
  outputReferenceScriptControlData,
  type OutputReferenceScriptDecodingEvidence,
  OutputReferenceScriptResultClasses,
} from "./output-reference-script-decoding.js";
import {
  OutputReferenceOutputControlSchema,
  OutputReferenceStep03DatumSchema,
  OutputReferenceStep05DatumSchema,
} from "./schemas.js";

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

export const outputReferenceScriptDecodingOutputScanTarget = ({
  stage,
  evidence,
}: {
  readonly stage: OutputReferenceScriptDecodingAuthenticatedStage;
  readonly evidence: OutputReferenceScriptDecodingEvidence;
}): "outputScan" | "referenceBind" => {
  const threadUtxo = required(stage.threadUtxo, "output-scan thread UTxO");
  if (threadUtxo.datum == null)
    throw new Error(
      "outputReferenceScriptDecoding output-scan thread datum is absent",
    );
  const datum = Data.from(
    threadUtxo.datum,
    OutputReferenceStep03DatumSchema as never,
  ) as {
    data: { control: unknown };
  };
  const encoded = Data.to(
    datum.data.control as never,
    OutputReferenceOutputControlSchema as never,
  );
  return outputReferenceScriptDecodingNextOutputScanStage({
    evidence,
    controlCbor: encoded,
  });
};

export const outputReferenceScriptDecodingNextOutputScanStage = ({
  evidence,
  controlCbor,
}: {
  readonly evidence: OutputReferenceScriptDecodingEvidence;
  readonly controlCbor: string;
}): "outputScan" | "referenceBind" => {
  const index = evidence.outputScanControls.findIndex(
    (control) =>
      Data.to(
        outputReferenceScriptControlData(control) as never,
        OutputReferenceOutputControlSchema as never,
      ) === controlCbor,
  );
  if (index < 0 || evidence.outputScanControls[index + 1] === undefined)
    throw new Error(
      "outputReferenceScriptDecoding authenticated output checkpoint is outside the deterministic trace",
    );
  return index + 1 === evidence.outputScanControls.length - 1
    ? "referenceBind"
    : "outputScan";
};

export const outputReferenceScriptDecodingStructuralTarget = ({
  stage,
  evidence,
}: {
  readonly stage: OutputReferenceScriptDecodingAuthenticatedStage;
  readonly evidence: OutputReferenceScriptDecodingEvidence;
}): "scan" | "step06" => {
  const threadUtxo = required(stage.threadUtxo, "structural-scan thread UTxO");
  if (threadUtxo.datum == null)
    throw new Error(
      "outputReferenceScriptDecoding structural-scan datum is absent",
    );
  const datum = Data.from(
    threadUtxo.datum,
    OutputReferenceStep05DatumSchema as never,
  ) as { data: { control_cbor: string; result_class: bigint } };
  return outputReferenceScriptDecodingNextStructuralStage({
    evidence,
    controlCbor: datum.data.control_cbor,
    resultClass: datum.data.result_class,
  });
};

export const outputReferenceScriptDecodingNextStructuralStage = ({
  evidence,
  controlCbor,
  resultClass,
}: {
  readonly evidence: OutputReferenceScriptDecodingEvidence;
  readonly controlCbor: string;
  readonly resultClass: bigint;
}): "scan" | "step06" => {
  if (resultClass !== BigInt(OutputReferenceScriptResultClasses.Pending))
    return "step06";
  const plan = buildNativeScriptDecodingScanPlan({
    itemBytes: Buffer.from(evidence.referenceScriptItemHex, "hex"),
    direction: Number(evidence.subject.direction) as 0 | 1,
  });
  if (plan.route !== NativeScriptDecodingPlanRoutes.Machine)
    throw new Error(
      "outputReferenceScriptDecoding pending state has no structural plan",
    );
  const segment = plan.segments.find(
    ({ controlBefore }) => controlBefore.cborHex === controlCbor,
  );
  if (segment !== undefined) {
    const last = plan.segments.at(-1) === segment;
    return last &&
      plan.direction === MidgardNativeScriptDecodingDirections.WrongfulRejection
      ? "step06"
      : "scan";
  }
  if (
    plan.verdict.control?.cborHex === controlCbor &&
    plan.verdict.refusalClass !== null
  )
    return "step06";
  throw new Error(
    "outputReferenceScriptDecoding authenticated structural checkpoint is outside the deterministic trace",
  );
};
