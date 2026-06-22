import { describe, expect, it } from "vitest";

import { evaluateReadiness } from "@/commands/readiness.js";

const now = 1_000_000;
type ReadinessInput = Parameters<typeof evaluateReadiness>[0];

const readyHeartbeats: ReadinessInput["workerHeartbeats"] = {
  blockCommitment: now - 1_000,
  blockConfirmation: now - 2_000,
  merge: now - 2_000,
  depositFetch: now - 3_000,
  withdrawalFetch: now - 3_000,
  txQueueProcessor: now - 1_000,
};

const readinessInput = (
  overrides: Partial<Omit<ReadinessInput, "workerHeartbeats">> & {
    workerHeartbeats?: Partial<ReadinessInput["workerHeartbeats"]>;
  } = {},
): ReadinessInput => ({
  unresolvedBlockSubmissionAgeMs: 0,
  maxUnresolvedBlockSubmissionAgeMs: 10_000,
  nowMillis: now,
  maxHeartbeatAgeMs: 10_000,
  maxQueueDepth: 100,
  queueDepth: 10,
  localFinalizationPending: false,
  dbHealthy: true,
  ...overrides,
  workerHeartbeats: {
    ...readyHeartbeats,
    ...overrides.workerHeartbeats,
  },
});

describe("evaluateReadiness", () => {
  it("returns ready when heartbeats are fresh and queue depth is under threshold", () => {
    const readiness = evaluateReadiness(readinessInput());

    expect(readiness.ready).toBe(true);
    expect(readiness.reasons).toHaveLength(0);
  });

  it("fails readiness when any worker heartbeat is stale", () => {
    const readiness = evaluateReadiness(
      readinessInput({
        workerHeartbeats: {
          blockCommitment: now - 20_000,
        },
      }),
    );

    expect(readiness.ready).toBe(false);
    expect(readiness.reasons.some((r) => r.includes("blockCommitment"))).toBe(
      true,
    );
  });

  it("fails readiness when queue backlog exceeds threshold", () => {
    const readiness = evaluateReadiness(readinessInput({ queueDepth: 101 }));

    expect(readiness.ready).toBe(false);
    expect(readiness.reasons.some((r) => r.includes("queue_depth"))).toBe(true);
  });

  it("fails readiness when local finalization is pending", () => {
    const readiness = evaluateReadiness(
      readinessInput({
        localFinalizationPending: true,
      }),
    );

    expect(readiness.ready).toBe(false);
    expect(
      readiness.reasons.some((r) => r.includes("local_finalization_pending")),
    ).toBe(true);
  });

  it("fails readiness when database health probe fails", () => {
    const readiness = evaluateReadiness(readinessInput({ dbHealthy: false }));

    expect(readiness.ready).toBe(false);
    expect(readiness.reasons).toContain("db_unhealthy");
  });

  it("reports fresh active state-queue leases without failing readiness", () => {
    const readiness = evaluateReadiness(
      readinessInput({
        stateQueueMutationLease: {
          active: true,
          stale: false,
          remainingMs: 30_000,
          holder: "block_commitment",
        },
      }),
    );

    expect(readiness.ready).toBe(true);
    expect(readiness.reasons).toHaveLength(0);
  });

  it("fails readiness when a state-queue lease is stale", () => {
    const readiness = evaluateReadiness(
      readinessInput({
        stateQueueMutationLease: {
          active: true,
          stale: true,
          remainingMs: -120_000,
          holder: "state_queue_merge",
        },
      }),
    );

    expect(readiness.ready).toBe(false);
    expect(readiness.reasons).toContain(
      "state_queue_lease_stale:state_queue_merge:-120000",
    );
  });
});
