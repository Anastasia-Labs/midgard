import { describe, expect, it } from "vitest";

import { MISSING_REDEEMER_CONFIG_KEYS } from "../src/missing-redeemer/v1.js";
import { MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import { isAdmittedWorkflowRunner } from "../src/workflow/runner-admission.js";
import { createMissingRedeemerWorkflowRunner } from "../src/workflow/runtime.js";

describe("missingRedeemer production package registration", () => {
  it("admits the fixed-category runner and complete replay token", () => {
    const runner = createMissingRedeemerWorkflowRunner(async () => {
      throw new Error("registration test must not load infrastructure");
    });
    expect(
      isAdmittedWorkflowRunner({
        category: "missingRedeemer",
        runner,
      }),
    ).toBe(true);
    expect(MISSING_REDEEMER_COMPLETE_CANONICAL_REPLAY.launchScope).toEqual([
      "missingRedeemer",
    ]);
  });

  it("keeps runtime configuration infrastructure-only", () => {
    expect([...MISSING_REDEEMER_CONFIG_KEYS].sort()).toEqual(
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
