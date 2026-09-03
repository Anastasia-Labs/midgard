import { describe, expect, it } from "vitest";

import type {
  WatcherProductionFaultProofSupervisorStatusV1,
  WatcherProductionFaultProofSupervisorV1,
} from "../../src/fault-proofs/production-fault-proof-supervisor-v1.js";
import {
  createWatcherProductionOperationsObservabilityV1,
  type WatcherProductionOperationsStatusV1,
} from "../../src/runtime/production-operations-observability-v1.js";

const supervisor = () => {
  let status: WatcherProductionFaultProofSupervisorStatusV1 = Object.freeze({
    phase: "accepting",
    recovered: true,
    queuedJobCount: 1,
    activeJob: null,
    blockedJob: null,
    deadlineHealth: "safe",
    earliestDeadlineJob: null,
    remainingSafeStartMs: "1000000",
  });
  return {
    runtime: Object.freeze({
      status: () => status,
    }) as unknown as WatcherProductionFaultProofSupervisorV1,
    setStatus: (
      next: Partial<WatcherProductionFaultProofSupervisorStatusV1>,
    ) => {
      status = Object.freeze({ ...status, ...next });
    },
  };
};

describe("production operations observability V1", () => {
  it("reports bounded secret-safe W38 status, metrics, and alerts", async () => {
    const proofSupervisor = supervisor();
    let now = 100_000n;
    const observability = createWatcherProductionOperationsObservabilityV1({
      deploymentFingerprint: "11".repeat(32),
      supervisor: proofSupervisor.runtime,
      launchScopeStatus: () => ({
        installedCategoryCount: 32,
        requiredCategoryCount: 32,
      }),
      durableProofQueueStatus: () => ({
        queuedJobCount: 1,
        oldestQueuedAtMs: "97000",
      }),
      nowMs: () => now,
      l1FreshnessMaximumAgeMs: 10_000,
      maximumRetainedDiagnostics: 100,
    });

    observability.sink.recordL1Source({
      sourceIdentityDigest: "22".repeat(32),
      sourceMode: "local_node",
      status: "consistent",
      blockHash: "33".repeat(32),
      blockNo: "50",
      slot: "500",
      observedAtMs: "99000",
    });
    observability.sink.recordVerification({
      subjectDigest: "44".repeat(32),
      queuedAtMs: "90000",
      startedAtMs: "92000",
      completedAtMs: "96000",
      outcome: "fault_detected",
    });
    observability.sink.recordDaFetch({
      subjectDigest: "44".repeat(32),
      startedAtMs: "92000",
      completedAtMs: "95000",
      outcome: "succeeded",
    });
    observability.sink.recordProofStep({
      decisionDigest: "55".repeat(32),
      stage: "proof_step",
      actionIdentityDigest: "57".repeat(32),
      status: "queued",
      updatedAtMs: "97000",
    });
    observability.sink.recordProofStep({
      decisionDigest: "55".repeat(32),
      stage: "proof_step",
      actionIdentityDigest: "58".repeat(32),
      status: "confirmed",
      updatedAtMs: "98000",
    });
    observability.sink.recordEvent({
      eventDigest: "66".repeat(32),
      eventKind: "withdrawal",
      status: "unprocessed",
      inclusionAtMs: "90000",
      updatedAtMs: "99000",
    });

    expect(observability.api.status()).toMatchObject({
      liveness: "live",
      readiness: "ready",
      readinessReasons: [],
      launchScope: {
        installedCategoryCount: "32",
        requiredCategoryCount: "32",
        complete: true,
      },
    } satisfies Partial<WatcherProductionOperationsStatusV1>);
    expect(observability.api.metrics()).toMatchObject({
      queuedProofCount: "1",
      oldestQueuedProofAgeMs: "3000",
      verificationLatencyMs: {
        sampleCount: "1",
        p50: "4000",
        p95: "4000",
        maximum: "4000",
      },
      daLatencyMs: {
        sampleCount: "1",
        p50: "3000",
        p95: "3000",
        maximum: "3000",
      },
      proofSteps: { queued: "1", confirmed: "1" },
      unprocessedEventCount: "1",
      oldestUnprocessedEventAgeMs: "10000",
      l1Sources: {
        configured: "1",
        fresh: "1",
        stale: "0",
        disagreement: "0",
        maximumFreshnessAgeMs: "1000",
      },
      activeAlertCount: "0",
    });

    observability.sink.setAlert({
      code: "provider_disagreement",
      subjectDigest: "22".repeat(32),
      active: true,
      observedAtMs: "100000",
    });
    expect(observability.api.status()).toMatchObject({
      readiness: "not_ready",
      readinessReasons: ["active_alert"],
      activeAlerts: [{ code: "provider_disagreement" }],
    });
    now = 100_001n;
    observability.sink.setAlert({
      code: "provider_disagreement",
      subjectDigest: "22".repeat(32),
      active: false,
      observedAtMs: "100001",
    });
    proofSupervisor.setStatus({ deadlineHealth: "at_risk" });
    expect(observability.api.status()).toMatchObject({
      readiness: "not_ready",
      readinessReasons: ["deadline_at_risk"],
    });

    const firstPage = observability.api.diagnostics({
      kind: "alert",
      limit: 1,
    });
    expect(firstPage.records).toHaveLength(1);
    expect(firstPage.nextCursor).toBe(firstPage.records[0]!.sequence);
    expect(
      observability.api.diagnostics({
        kind: "alert",
        cursor: firstPage.nextCursor!,
        limit: 1,
      }).records,
    ).toHaveLength(1);
    expect(Object.isFrozen(firstPage.records)).toBe(true);

    const metricsResponse = await observability.handleHttpRequest(
      new Request("http://127.0.0.1/v1/metrics"),
    );
    expect(metricsResponse.status).toBe(200);
    await expect(metricsResponse.json()).resolves.toMatchObject({
      queuedProofCount: "1",
      oldestQueuedProofAgeMs: "3001",
    });
    expect(metricsResponse.headers.get("cache-control")).toBe("no-store");
    expect(
      (
        await observability.handleHttpRequest(
          new Request("http://127.0.0.1/v1/diagnostics?kind=alert&limit=1000"),
        )
      ).status,
    ).toBe(400);

    now = 120_001n;
    expect(observability.api.status()).toMatchObject({
      readiness: "not_ready",
      readinessReasons: ["deadline_at_risk", "l1_source_stale"],
    });
  });

  it("rejects unbounded pages and secret-shaped diagnostic labels", () => {
    const proofSupervisor = supervisor();
    const observability = createWatcherProductionOperationsObservabilityV1({
      deploymentFingerprint: "11".repeat(32),
      supervisor: proofSupervisor.runtime,
      launchScopeStatus: () => ({
        installedCategoryCount: 31,
        requiredCategoryCount: 32,
      }),
      durableProofQueueStatus: () => ({
        queuedJobCount: 1,
        oldestQueuedAtMs: "99000",
      }),
      nowMs: () => 100_000n,
    });

    expect(observability.api.status()).toMatchObject({
      readiness: "not_ready",
      readinessReasons: ["launch_scope_incomplete", "l1_source_unavailable"],
    });
    expect(() =>
      observability.api.diagnostics({ kind: "alert", limit: 101 }),
    ).toThrow("page request is invalid");
    expect(() =>
      observability.sink.recordL1Source({
        sourceIdentityDigest: "http://operator-private.example/key",
        sourceMode: "local_node",
        status: "consistent",
        blockHash: "33".repeat(32),
        blockNo: "50",
        slot: "500",
        observedAtMs: "99000",
      }),
    ).toThrow("identity digest is invalid");
    expect(() =>
      observability.sink.recordProofStep({
        decisionDigest: "55".repeat(32),
        stage: "proof/seed phrase" as "proof_step",
        actionIdentityDigest: "56".repeat(32),
        status: "failed",
        updatedAtMs: "99000",
      }),
    ).toThrow("stage is invalid");
    expect(() =>
      observability.sink.recordProofStep({
        decisionDigest: "55".repeat(32),
        stage: "proof_step",
        actionIdentityDigest: "56".repeat(32),
        status: "failed",
        updatedAtMs: "100001",
      }),
    ).toThrow("is in the future");
    expect(() =>
      createWatcherProductionOperationsObservabilityV1({
        deploymentFingerprint: "11".repeat(32),
        supervisor: proofSupervisor.runtime,
        launchScopeStatus: () => ({
          installedCategoryCount: 32,
          requiredCategoryCount: 32,
        }),
        durableProofQueueStatus: () => ({
          queuedJobCount: 2,
          oldestQueuedAtMs: "99000",
        }),
        nowMs: () => 100_000n,
      }).api.metrics(),
    ).toThrow("differs from supervisor");
  });
});
