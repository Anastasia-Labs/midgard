import { describe, expect, it } from "vitest";

import type { WatcherChainCoordinator } from "../../src/runtime/chain-coordinator.js";
import {
  createWatcherOperationsObservability,
  type WatcherOperationsStatus,
} from "../../src/runtime/operations-observability.js";
import { supervisor } from "./operations-observability.supervisor.js";

describe("coordinator recovery readiness", () => {
  it("serves a persistent actionable hold until the coordinator actually resumes", async () => {
    let coordinator: ReturnType<WatcherChainCoordinator["status"]> = {
      integrityHold: "durable_authority_conflict",
      quarantined: false,
      rollbackPoint: null,
      deliveryHeld: true,
      bufferedBlockCount: 1,
      processedThrough: null,
    };
    let now = 100_000n;
    const observability = createWatcherOperationsObservability({
      deploymentFingerprint: "11".repeat(32),
      supervisor: supervisor().runtime,
      launchScopeStatus: () => ({
        installedCategoryCount: 54,
        requiredCategoryCount: 54,
      }),
      durableProofQueueStatus: () => ({
        queuedJobCount: 0,
        oldestQueuedAtMs: null,
      }),
      retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
      coordinatorStatus: () => coordinator,
      nowMs: () => now,
    });
    const read = async (): Promise<WatcherOperationsStatus> =>
      (
        await observability.handleHttpRequest(
          new Request("http://localhost/v1/status"),
        )
      ).json() as Promise<WatcherOperationsStatus>;
    const held = await read();
    expect(held.readinessReasons).toContain("coordinator_recovery_hold");
    expect(held.coordinator?.integrityHold).toBe("durable_authority_conflict");
    expect(held.liveness).toBe("live");
    now += 10_000_000n;
    expect((await read()).readinessReasons).toContain(
      "coordinator_recovery_hold",
    );
    coordinator = { ...coordinator, integrityHold: null, deliveryHeld: false };
    expect((await read()).readinessReasons).not.toContain(
      "coordinator_recovery_hold",
    );
  });
});
