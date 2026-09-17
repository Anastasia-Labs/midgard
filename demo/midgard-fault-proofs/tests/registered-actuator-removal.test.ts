import {
  acceptedVerdictSubject,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
} from "@al-ft/midgard-sdk";
import { afterEach, describe, expect, it, vi } from "vitest";

import { createDistinctAssetAccumulationActuator } from "../src/distinct-asset-accumulation-limit/actuator.js";
import { createExecutionSourceScriptDecodingActuator } from "../src/execution-source-script-decoding/actuator.js";
import { createMissingRedeemerActuator } from "../src/missing-redeemer/actuator.js";
import { createMissingScriptSourceActuator } from "../src/missing-script-source/actuator.js";
import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.js";
import * as runtime from "../src/runtime.js";
import { createUnusedRedeemerActuator } from "../src/unused-redeemer/actuator.js";

// Fixtures intentionally stop before transaction construction; adapt each
// actuator's fuller proof-step inputs to this shared removal-only fixture.
const removalCapture =
  <Config, Input>(
    create: (config: Config) => { capture: (input: Input) => Promise<unknown> },
  ) =>
  (config: unknown, input: unknown) =>
    create(config as Config).capture(input as Input);

const families = [
  [
    "executionSourceScriptDecoding",
    "fraudProofExecutionSourceScriptDecoding",
    removalCapture(createExecutionSourceScriptDecodingActuator),
  ],
  [
    "missingScriptSource",
    "fraudProofMissingScriptSource",
    removalCapture(createMissingScriptSourceActuator),
  ],
  [
    "missingRedeemer",
    "fraudProofMissingRedeemer",
    removalCapture(createMissingRedeemerActuator),
  ],
  [
    "unusedRedeemer",
    "fraudProofUnusedRedeemer",
    removalCapture(createUnusedRedeemerActuator),
  ],
  [
    "distinctAssetAccumulationLimit",
    "fraudProofDistinctAssetAccumulationLimit",
    removalCapture(createDistinctAssetAccumulationActuator),
  ],
] as const;
const headerHash = "ab".repeat(28);
const hash = "cd".repeat(28);
const reward = 400_000_000n;
const action = {
  stage: "remove" as const,
  nextRemovalOutRef: `${"11".repeat(32)}#0`,
  fraudProofOutRef: `${"22".repeat(32)}#0`,
};
const subject = acceptedVerdictSubject("12".repeat(32));
const finding = { subject, coordinate: {} };
// Only removal admission fields are read; no proof-step builder is run.
const artifact = {
  schemaVersion: "midgard-missing-redeemer-production-artifact-v1",
  headerHash,
  header: { validationTracesRoot: "ef".repeat(32), validationTraceCount: 1n },
  finding,
  evidence: {
    finding,
    subject,
    purposeKind: 0,
    purposeIndex: 0,
    redeemerMissing: true,
    purpose: { sourceLeafHashHex: "ef".repeat(32), sourceLanguageTag: 3 },
  },
  authentication: {
    validationTracesRoot: "ef".repeat(32),
    validationTraceCount: 1n,
    machineState: { transaction_id: subject.transaction_id },
    sourceLanguageTag: 3n,
    control: {
      discovery: {
        current_purpose_kind: 0n,
        current_purpose_index: 0n,
        matched_source_leaf: "ef".repeat(32),
      },
    },
  },
  acceptedInclusion: {},
};
afterEach(() => vi.restoreAllMocks());

describe.each(families)("%s production removal", (name, entry, capture) => {
  const categoryId = FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[name];
  const deploymentInfo = {
    referenceScriptAuthPolicy: {},
    contracts: Object.fromEntries(
      ["hubOracleMint", "fraudProofMint", "fraudProofSpend", entry].map(
        (key) => [key, { scriptHash: hash }],
      ),
    ),
    economics: {
      profile: "bounded-acceptance-v1",
      requiredBondLovelace: 900_000_000,
      slashingPenaltyLovelace: 500_000_000,
      inactivitySlashingPenaltyLovelace: 100_000_000,
      fraudProverRewardLovelace: Number(reward),
      proverCollateralFloorLovelace: 5_000_000,
    },
  };
  const common = {
    lucid: {},
    blueprint: {},
    deploymentInfo,
    network: "Custom" as const,
    signer: {},
    categoryId,
    contracts: {
      steps: [{ spendingScriptHash: hash }],
      fraudProof: { policyId: hash, spendingScriptAddress: "unused" },
    },
    references: { steps: [], witnesses: {} },
    stateQueueMutationLeaseCoordinator: { acquire: vi.fn() },
    fraudProofSpendingScriptHash: hash,
    fraudProverRewardLovelace: reward,
  };
  const config = {
    ...common,
    binding: {
      ...common,
      definition: { headerHash },
      resolvedContracts: {
        category: { categoryId },
        contracts: { fraudProof: { spendingScriptHash: hash } },
      },
      releaseEconomics: {
        policy: { fraudProverRewardLovelace: String(reward) },
      },
    },
  };

  it("resolves removal through the canonical deployment catalogue", async () => {
    // Keep actuator, cursor capture, and removal routing real. Stop at the
    // canonical resolver, before signing or any provider operation.
    const resolve = vi
      .spyOn(runtime, "resolveFaultProofDeploymentContracts")
      .mockRejectedValue(new Error("canonical deployment resolution boundary"));
    await expect(capture(config, { action, artifact })).rejects.toThrow(
      "canonical deployment resolution boundary",
    );
    expect(resolve).toHaveBeenCalledExactlyOnceWith({
      blueprint: common.blueprint,
      deploymentInfo,
      network: common.network,
      categoryName: name,
      requireFraudProofSpend: true,
    });
    expect(
      common.stateQueueMutationLeaseCoordinator.acquire,
    ).not.toHaveBeenCalled();
  });

  it("still rejects a registered id passed as an explicit pre-registration category", async () => {
    await expect(
      submitRemoveFraudulentBlock({
        ...common,
        fraudulentHeaderHash: headerHash,
        fraudCategory: {
          name,
          categoryId,
          firstStepDeploymentEntry: entry,
          firstStepScriptHash: hash,
          fraudProof: {
            policyId: hash,
            spendingScriptHash: hash,
            spendingScriptAddress: "unused",
          },
        },
      } as unknown as Parameters<typeof submitRemoveFraudulentBlock>[0]),
    ).rejects.toThrow(`collides with the registered ${name} category`);
  });
});
