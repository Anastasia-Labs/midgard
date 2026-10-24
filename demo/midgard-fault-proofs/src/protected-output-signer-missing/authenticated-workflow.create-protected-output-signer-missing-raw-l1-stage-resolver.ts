import {
  type FraudProofCatalogueCategoryName,
  missingSignatureFieldWalkCheckpoint,
  PROOF_THREAD_SOURCE_KIND_FORCED,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import {
  type ManifestBoundProtectedOutputSignerMissingConfig,
  type ProtectedOutputSignerMissingAuthenticatedSource,
  type ProtectedOutputSignerMissingRuntimeLoader,
  type ProtectedOutputSignerMissingStage,
  required,
} from "./authenticated-workflow.create-protected-output-signer-missing-bound-config.js";
import { planProtectedOutputSignerWitnessOpening } from "./field-plans.js";
import {
  PROTECTED_OUTPUT_SIGNER_SCAN_BATCH,
  type ProtectedOutputSignerMissingEvidence,
} from "./protected-output-signer-missing.js";
import { ProtectedOutputSignerStep04DatumSchema } from "./schemas.js";

export const createProtectedOutputSignerMissingRawL1StageResolver =
  ({
    config,
    l1,
    source,
  }: {
    readonly config: ManifestBoundProtectedOutputSignerMissingConfig;
    readonly l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
    readonly source: ProtectedOutputSignerMissingAuthenticatedSource;
  }): ProtectedOutputSignerMissingRuntimeLoader["resolveStage"] =>
  async ({ action, evidence }) => {
    const observed = await l1.observe({
      headerHash: config.binding.definition.headerHash,
    });
    const stage = observed.stage;
    if (action === "submitInit") {
      if (stage.kind !== "not_started") {
        throw new Error(
          "protectedOutputSignerMissing init requires raw-L1 not_started",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    if (action === "removeDescendants") {
      if (stage.kind !== "proof_token") {
        throw new Error(
          "protectedOutputSignerMissing removal requires raw-L1 proof token",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    const expectedStep =
      action === "submitStep01"
        ? 1
        : action === "submitStep02"
          ? 2
          : action === "submitStep03"
            ? 3
            : action === "submitScan"
              ? 4
              : 5;
    if (stage.kind !== "step" || stage.step !== expectedStep) {
      throw new Error(
        `protectedOutputSignerMissing ${action} differs from authenticated raw-L1 stage`,
      );
    }
    const common = {
      fraudulentBlockOutRef: stage.stateQueueBlockOutRef,
      threadOutRef: stage.threadOutRef,
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
    const thread = await requireLinearFaultThreadUtxo({
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId: config.binding.resolvedContracts.category.categoryId,
      family: "protected-output-signer-missing",
      stepIndex: 0,
      threadOutRef: stage.threadOutRef,
    });
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

export const protectedOutputSignerScanTargetFromAuthenticatedStage = ({
  config,
  stage,
  evidence,
}: {
  readonly config: ManifestBoundProtectedOutputSignerMissingConfig;
  readonly stage: ProtectedOutputSignerMissingStage;
  readonly evidence: ProtectedOutputSignerMissingEvidence;
}): "scanning" | "step05" => {
  const threadUtxo = required(stage.threadUtxo, "scan thread UTxO");
  if (threadUtxo.datum == null)
    throw new Error("protectedOutputSignerMissing scan thread datum is absent");
  const datum = Data.from(
    threadUtxo.datum,
    ProtectedOutputSignerStep04DatumSchema as never,
  ) as {
    data: { checkpoint_hash: string };
  };
  const planned = planProtectedOutputSignerWitnessOpening({
    evidence,
    nativeTxCompactCbor: required(
      stage.nativeTxCompactCbor,
      "native transaction compact CBOR",
    ),
    witnessSetCompactCbor: required(
      stage.witnessSetCompactCbor,
      "witness-set compact CBOR",
    ),
    owner: config.signer.paymentKeyHash,
  });
  const cursors: number[] = [];
  for (
    let cursor = 0;
    cursor < planned.itemCount;
    cursor += PROTECTED_OUTPUT_SIGNER_SCAN_BATCH
  )
    cursors.push(cursor);
  if (planned.itemCount === 0) cursors.push(0);
  const checkpoint = cursors.find(
    (nextItemIndex) =>
      missingSignatureFieldWalkCheckpoint({
        txId: evidence.subject.transaction_id,
        itemCount: planned.itemCount,
        totalLength: planned.preimage.length,
        nextItemIndex,
      }).checkpointHash === datum.data.checkpoint_hash,
  );
  if (checkpoint === undefined)
    throw new Error(
      "protectedOutputSignerMissing authenticated scan checkpoint is not on the deterministic frontier",
    );
  return Math.min(
    planned.itemCount,
    checkpoint + PROTECTED_OUTPUT_SIGNER_SCAN_BATCH,
  ) === planned.itemCount
    ? "step05"
    : "scanning";
};
