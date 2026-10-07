import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { evaluateReadiness } from "../src/commands/readiness.js";
import { attestationTimeoutCorrectionReadinessBounds } from "../src/fibers/attestation-timeout-correction.js";

const now = 1_000_000;
type ReadinessInput = Parameters<typeof evaluateReadiness>[0];

const readyHeartbeats: ReadinessInput["workerHeartbeats"] = {
  blockCommitment: now - 1_000,
  blockConfirmation: now - 2_000,
  merge: now - 2_000,
  txQueueProcessor: now - 1_000,
};

const readinessInput = (
  overrides: Partial<Omit<ReadinessInput, "workerHeartbeats">> & {
    workerHeartbeats?: Partial<ReadinessInput["workerHeartbeats"]>;
  } = {},
): ReadinessInput => ({
  foreignBaseVerification: {
    status: "verified",
    scope: {
      deploymentIdentity: "fixture",
      ownerToken: "fixture",
      generation: "0",
      baseHeaderHash: null,
    },
    foreignHeaderHash: null,
    reason: null,
  },
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
  it("leaves unauthenticated membership ready, and removal to its liveness reason", () => {
    expect(
      evaluateReadiness(readinessInput({ operatorMembership: "unknown" })),
    ).toEqual({ ready: true, reasons: [] });
    expect(
      evaluateReadiness(readinessInput({ operatorMembership: "removed" })),
    ).toEqual({ ready: true, reasons: [] });
  });
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

  it("keeps readiness healthy for a fully live validation pool", () => {
    const readiness = evaluateReadiness(
      readinessInput({
        validationPool: {
          configuredWorkers: 6,
          liveWorkers: 6,
          restartingWorkers: 0,
          oldestInFlightAgeMs: 29_999,
          jobTimeoutMs: 30_000,
        },
      }),
    );
    expect(readiness.ready).toBe(true);
  });

  it("fails readiness for timed-out validation work", () => {
    const readiness = evaluateReadiness(
      readinessInput({
        validationPool: {
          configuredWorkers: 6,
          liveWorkers: 6,
          restartingWorkers: 0,
          oldestInFlightAgeMs: 30_001,
          jobTimeoutMs: 30_000,
        },
      }),
    );
    expect(readiness.reasons).toContain(
      "validation_worker_job_timeout:30001:30000",
    );
  });

  it("fails readiness while validation workers are restarting", () => {
    const readiness = evaluateReadiness(
      readinessInput({
        validationPool: {
          configuredWorkers: 6,
          liveWorkers: 4,
          restartingWorkers: 2,
          oldestInFlightAgeMs: 0,
          jobTimeoutMs: 30_000,
        },
      }),
    );
    expect(readiness.reasons).toContain(
      "validation_worker_pool_degraded:4:6:2",
    );
  });

  it("treats the explicit inline rollback as a healthy disabled pool", () => {
    const readiness = evaluateReadiness(
      readinessInput({
        validationPool: {
          configuredWorkers: 0,
          liveWorkers: 0,
          restartingWorkers: 0,
          oldestInFlightAgeMs: 0,
          jobTimeoutMs: 30_000,
        },
      }),
    );
    expect(readiness.ready).toBe(true);
  });

  describe("attestation-timeout correction", () => {
    const headerHash = "ab".repeat(32);
    // The bounds a node running the default 10 s tick derives from its profile.
    const bounds = attestationTimeoutCorrectionReadinessBounds(10_000);
    const correction = ({
      failures = 0,
      deadlineMs = null,
      lastProgressAtMs = now,
      lastQueueReadAtMs = now,
    }: {
      failures?: number;
      deadlineMs?: number | null;
      lastProgressAtMs?: number;
      lastQueueReadAtMs?: number;
    }): ReadinessInput["attestationTimeoutCorrection"] => ({
      consecutiveFailures: failures,
      oldestUnattestedHeader:
        deadlineMs === null ? null : { headerHash, deadlineMs },
      lastProgressAtMs,
      lastQueueReadAtMs,
      ...bounds,
    });
    const evaluate = (input: Parameters<typeof correction>[0]) =>
      evaluateReadiness(
        readinessInput({ attestationTimeoutCorrection: correction(input) }),
      );

    it("fails readiness while three failing steps leave a timed-out header uncorrected", () => {
      expect(evaluate({ failures: 3, deadlineMs: now - 5_000 })).toEqual({
        ready: false,
        reasons: [
          `attestation_timeout_correction_failing:${headerHash}:3:5000`,
        ],
      });
    });

    it("counts a header as timed out from the deadline itself", () => {
      expect(evaluate({ failures: 3, deadlineMs: now })).toEqual({
        ready: false,
        reasons: [`attestation_timeout_correction_failing:${headerHash}:3:0`],
      });
    });

    it("fails readiness while a step past the deadline makes no progress, whatever the failure count", () => {
      const sinceProgressMs = bounds.stallBoundMs + 1;
      expect(
        evaluate({
          deadlineMs: now - 5_000,
          lastProgressAtMs: now - sinceProgressMs,
        }),
      ).toEqual({
        ready: false,
        reasons: [
          `attestation_timeout_correction_stalled:${headerHash}:${sinceProgressMs}:${bounds.stallBoundMs}`,
        ],
      });
    });

    it("fails readiness once the queue has gone unread long enough to hide a due header", () => {
      const sinceReadMs = bounds.queueUnknownBoundMs + 1;
      expect(evaluate({ lastQueueReadAtMs: now - sinceReadMs })).toEqual({
        ready: false,
        reasons: [
          `attestation_timeout_queue_unknown:${sinceReadMs}:${bounds.queueUnknownBoundMs}`,
        ],
      });
    });

    it.each([
      ["nothing is unattested", { failures: 13 }],
      [
        "the unattested header is a millisecond before its deadline",
        { failures: 3, deadlineMs: now + 1 },
      ],
      ["two steps have failed", { failures: 2, deadlineMs: now - 5_000 }],
      ["the step last succeeded", { failures: 0, deadlineMs: now - 5_000 }],
      [
        "a removal has awaited confirmation for a full validity range and a tick",
        {
          deadlineMs: now - 5_000,
          lastProgressAtMs:
            now - Number(SDK.MAX_VALIDITY_RANGE_LENGTH_MS) - 10_000,
        },
      ],
      [
        "the step has gone exactly the stall bound without progress",
        {
          deadlineMs: now - 5_000,
          lastProgressAtMs: now - bounds.stallBoundMs,
        },
      ],
      [
        "a stalled step has no timed-out header to correct",
        {
          deadlineMs: now + 1,
          lastProgressAtMs: now - bounds.stallBoundMs - 1,
        },
      ],
      [
        "the queue has been unreadable for a minute",
        { lastQueueReadAtMs: now - 60_000 },
      ],
      [
        "the queue has gone unread exactly the unknown bound",
        { lastQueueReadAtMs: now - bounds.queueUnknownBoundMs },
      ],
    ])("stays ready when %s", (_case, input) => {
      expect(evaluate(input)).toEqual({ ready: true, reasons: [] });
    });
  });
});

it.each(["unobserved", "checking", "missing", "refused"] as const)(
  "does not report ready with %s foreign-base verification",
  (status) => {
    const input = readinessInput();
    const foreignBaseVerification: ReadinessInput["foreignBaseVerification"] =
      status === "unobserved"
        ? { status }
        : {
            status,
            scope: {
              deploymentIdentity: "fixture",
              ownerToken: "fixture",
              generation: "0",
              baseHeaderHash: "foreign-header",
            },
            foreignHeaderHash: "foreign-header",
            reason: "verification_pending",
          };
    const result = evaluateReadiness({ ...input, foreignBaseVerification });
    expect(result.ready).toBe(false);
    expect(
      result.reasons.some((reason) =>
        reason.startsWith(`foreign_base_verification_${status}`),
      ),
    ).toBe(true);
  },
);
