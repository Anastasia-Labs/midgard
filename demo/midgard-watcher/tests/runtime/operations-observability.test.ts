import { describe, expect, it } from "vitest";

import type {
  WatcherFaultProofSupervisor,
  WatcherFaultProofSupervisorStatus,
} from "../../src/fault-proofs/fault-proof-supervisor.js";
import {
  createWatcherOperationsObservability,
  type WatcherOperationsStatus,
} from "../../src/runtime/operations-observability.js";

const supervisor = () => {
  let status: WatcherFaultProofSupervisorStatus = Object.freeze({
    phase: "accepting",
    recovered: true,
    unfinishedObjectiveCount: 1,
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
    }) as unknown as WatcherFaultProofSupervisor,
    setStatus: (next: Partial<WatcherFaultProofSupervisorStatus>) => {
      status = Object.freeze({ ...status, ...next });
    },
  };
};

describe("production operations observability V1", () => {
  it("ages known observations monotonically and keeps future-origin ages unknown", () => {
    let wall = 100_000n;
    let monotonic = 1000;
    let queuedAt = "99000";
    const observability = createWatcherOperationsObservability({
      deploymentFingerprint: "11".repeat(32),
      supervisor: supervisor().runtime,
      launchScopeStatus: () => ({
        installedCategoryCount: 54,
        requiredCategoryCount: 54,
      }),
      durableProofQueueStatus: () => ({
        queuedJobCount: 1,
        oldestQueuedAtMs: queuedAt,
      }),
      nowMs: () => wall,
      monotonicNowMs: () => monotonic,
      l1FreshnessMaximumAgeMs: 10_000,
    });
    const source = {
      sourceIdentityDigest: "22".repeat(32),
      sourceMode: "local_node" as const,
      status: "consistent" as const,
      blockHash: "33".repeat(32),
      blockNo: "50",
      slot: "500",
      observedAtMs: "99000",
    };
    const event = {
      eventDigest: "44".repeat(32),
      eventKind: "deposit" as const,
      status: "unprocessed" as const,
      inclusionAtMs: "99000",
      updatedAtMs: "100000",
    };
    observability.sink.recordL1Source(source);
    observability.sink.recordEvent(event);
    expect(observability.api.metrics().oldestQueuedProofAgeMs).toBe("1000");
    wall = 90_000n;
    monotonic += 10_000;
    observability.sink.recordEvent({ ...event, updatedAtMs: wall.toString() });
    observability.sink.recordProofStep({
      decisionDigest: "55".repeat(32),
      actionIdentityDigest: "66".repeat(32),
      stage: "prepare",
      status: "queued",
      updatedAtMs: "100000",
    });
    observability.sink.setAlert({
      code: "da_fetch_failure",
      subjectDigest: "44".repeat(32),
      active: false,
      observedAtMs: "100000",
    });
    expect(observability.api.metrics()).toMatchObject({
      oldestQueuedProofAgeMs: "11000",
      oldestUnprocessedEventAgeMs: "11000",
      l1Sources: { stale: "1", fresh: "0", maximumFreshnessAgeMs: "11000" },
    });
    expect(observability.api.status().readinessReasons).toContain(
      "l1_source_stale",
    );
    wall = 10_000_000n;
    monotonic += 500;
    expect(observability.api.metrics()).toMatchObject({
      oldestQueuedProofAgeMs: "11500",
      oldestUnprocessedEventAgeMs: "11500",
      l1Sources: { maximumFreshnessAgeMs: "11500" },
    });
    wall = 80_000n;
    queuedAt = "90000";
    observability.sink.recordL1Source({ ...source, observedAtMs: "90000" });
    observability.sink.recordEvent({ ...event, inclusionAtMs: "90000" });
    expect(observability.api.metrics()).toMatchObject({
      oldestQueuedProofAgeMs: null,
      oldestUnprocessedEventAgeMs: null,
      l1Sources: { stale: "1", fresh: "0", maximumFreshnessAgeMs: null },
    });
    wall = 200_000n;
    monotonic += 100;
    expect(observability.api.metrics().oldestQueuedProofAgeMs).toBeNull();
    expect(
      observability.api.diagnostics({ kind: "event" }).records.at(-1),
    ).toMatchObject({ inclusionAtMs: "90000", updatedAtMs: "100000" });
  });

  it("reports bounded secret-safe W38 status, metrics, and alerts", async () => {
    const proofSupervisor = supervisor();
    let now = 100_000n;
    const observability = createWatcherOperationsObservability({
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
      monotonicNowMs: () => Number(now),
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
      elapsedMs: "4000",
      outcome: "fault_detected",
    });
    observability.sink.recordDaFetch({
      subjectDigest: "44".repeat(32),
      startedAtMs: "92000",
      completedAtMs: "95000",
      elapsedMs: "3000",
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
    } satisfies Partial<WatcherOperationsStatus>);
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
    const observability = createWatcherOperationsObservability({
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
    ).not.toThrow();
    expect(() =>
      createWatcherOperationsObservability({
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
