import { describe, expect, it } from "vitest";

import { MISSING_REDEEMER_PRODUCTION_CONFIG_KEYS_V1 } from "../src/missing-redeemer/production-v1.js";
import { MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY_V1 } from "../src/workflow/complete-replay-v1.js";
import { isAdmittedProductionWorkflowRunnerV1 } from "../src/workflow/production-runner-admission-v1.js";
import { createMissingRedeemerProductionWorkflowRunnerV1 } from "../src/workflow/production-runtime-v1.js";

describe("missingRedeemer production package registration", () => {
  it("admits the fixed-category runner and complete replay token", () => {
    const runner = createMissingRedeemerProductionWorkflowRunnerV1(async () => {
      throw new Error("registration test must not load infrastructure");
    });
    expect(
      isAdmittedProductionWorkflowRunnerV1({
        category: "missingRedeemer",
        runner,
      }),
    ).toBe(true);
    expect(MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY_V1.launchScope).toEqual([
      "missingRedeemer",
    ]);
  });

  it("keeps runtime configuration infrastructure-only", () => {
    expect([...MISSING_REDEEMER_PRODUCTION_CONFIG_KEYS_V1].sort()).toEqual(
      [
        "blueprintJson",
        "decisionDigest",
        "deploymentInfo",
        "headerHash",
        "lucid",
        "manifest",
        "referenceScripts",
        "signer",
        "source",
        "stateQueueMutationLeaseCoordinator",
      ].sort(),
    );
  });
});
