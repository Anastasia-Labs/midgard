import {
  type FraudProofCatalogueCategoryName,
  PROOF_THREAD_SOURCE_KIND_FORCED,
} from "@al-ft/midgard-sdk";

import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import {
  type ManifestBoundSpendInputSignerMissingConfig,
  required,
  type SpendInputSignerMissingAuthenticatedSource,
  type SpendInputSignerMissingRuntimeLoader,
} from "./authenticated-workflow.create-spend-input-signer-missing-bound-config.js";

export const createSpendInputSignerMissingRawL1StageResolver =
  ({
    config,
    l1,
    source,
  }: {
    readonly config: ManifestBoundSpendInputSignerMissingConfig;
    readonly l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
    readonly source: SpendInputSignerMissingAuthenticatedSource;
  }): SpendInputSignerMissingRuntimeLoader["resolveStage"] =>
  async ({ action, evidence }) => {
    const observed = await l1.observe({
      headerHash: config.binding.definition.headerHash,
    });
    const stage = observed.stage;
    if (action === "submitInit") {
      if (stage.kind !== "not_started") {
        throw new Error(
          "spendInputSignerMissing init requires raw-L1 not_started",
        );
      }
      return { fraudulentBlockOutRef: stage.stateQueueBlockOutRef };
    }
    if (action === "removeDescendants") {
      if (stage.kind !== "proof_token") {
        throw new Error(
          "spendInputSignerMissing removal requires raw-L1 proof token",
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
        `spendInputSignerMissing ${action} differs from authenticated raw-L1 stage`,
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
      family: "spend-input-signer-missing",
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
