import { describe, expect, it, vi } from "vitest";

import {
  createRedeemerCanonicityWorkflowRunnerSurface,
  type LoadedRedeemerCanonicityWorkflow,
} from "../src/redeemer-canonicity/production-runtime-v1.js";

const invocation = (category: string) =>
  ({
    mode: "run",
    category,
    deploymentFingerprint: "11".repeat(32),
    headerHash: "22".repeat(28),
    journalDirectory: "/tmp/redeemer-canonicity-test-journal",
    runtimeConfigPath: "/tmp/redeemer-canonicity-runtime.json",
    decisionDigest: "33".repeat(32),
    actuationPermit: {},
    fundingReservationPermit: {},
  }) as never;

describe("redeemerCanonicity production runner surface", () => {
  it("refuses another category before loading runtime state", async () => {
    const loadRuntimeConfig = vi.fn();
    const runner = createRedeemerCanonicityWorkflowRunnerSurface({
      loadRuntimeConfig,
    });
    await expect(
      runner.runOrResume(invocation("observerOrderInvalid")),
    ).rejects.toThrow(/category mismatch/u);
    expect(loadRuntimeConfig).not.toHaveBeenCalled();
  });

  it("requires retained public DA and always closes its runtime", async () => {
    const close = vi.fn(async () => undefined);
    const loaded = {
      schemaVersion: "midgard-production-fraud-proof-runtime-config-v1",
      workflow: {
        binding: {
          deploymentFingerprint: "11".repeat(32),
          definition: {
            category: "redeemerCanonicity",
            headerHash: "22".repeat(28),
          },
        },
        decisionDigest: "33".repeat(32),
      },
      retainedDaSources: [],
      close,
    } as unknown as LoadedRedeemerCanonicityWorkflow;
    const runner = createRedeemerCanonicityWorkflowRunnerSurface({
      loadRuntimeConfig: async () => loaded,
    });
    await expect(
      runner.runOrResume(invocation("redeemerCanonicity")),
    ).rejects.toThrow(/no public retained-DA source/u);
    expect(close).toHaveBeenCalledOnce();
  });
});
