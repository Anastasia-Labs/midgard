import { describe, expect, it } from "vitest";

import {
  parseReleaseL1FinalityPolicy,
  RELEASE_L1_FINALITY_POLICY_DEEP_ROLLBACK_POLICY,
} from "../src/commands/e2e-release-finality-policy-v1.js";

const policy = () => ({
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: RELEASE_L1_FINALITY_POLICY_DEEP_ROLLBACK_POLICY,
});

describe("release-bound L1 finality policy V1", () => {
  it("accepts the exact source-neutral F04 policy", () => {
    expect(parseReleaseL1FinalityPolicy(policy())).toEqual(policy());
  });

  it("rejects caller-selected depths, aliases, and extra fields", () => {
    for (const candidate of [
      { ...policy(), confirmationDepth: 29 },
      { ...policy(), confirmationDepth: 31 },
      { ...policy(), automaticRecoveryMaxDepth: 30 },
      { ...policy(), deepRollbackPolicy: "manual-repair" },
      { ...policy(), sourceMode: "local_node" },
      null,
    ]) {
      expect(() => parseReleaseL1FinalityPolicy(candidate)).toThrow();
    }
  });
});
