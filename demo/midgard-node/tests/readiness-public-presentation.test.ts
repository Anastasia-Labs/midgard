import { describe, expect, it } from "vitest";

import { publicReadinessPresentation } from "../src/commands/listen-readiness-public-presentation.js";
import { readinessDatabaseError } from "../src/commands/listen-router.get-readiness-handler.inputs.js";
import { evaluateReadiness } from "../src/commands/readiness.js";

describe("public readiness database diagnostics", () => {
  it("preserves only the exact planning authority's lossless canonical natural PGbigint epoch domain", () => {
    const project = (epoch: string) =>
      publicReadinessPresentation(
        {
          commitPlanningProgress: { planned: { authority: { epoch } } },
          unrelated: { epoch },
        },
        "preprod-public",
      );
    for (const epoch of ["0", "9007199254740993", "9223372036854775807"])
      expect(project(epoch)).toEqual({
        commitPlanningProgress: { planned: { authority: { epoch } } },
        unrelated: { epoch: "[redacted]" },
      });
    for (const epoch of [
      "9223372036854775808",
      "-1",
      "01",
      "1.0",
      "1e3",
      "12345678-1234-1234-1234-123456789abc",
    ])
      expect(project(epoch)).toEqual({
        commitPlanningProgress: {
          planned: { authority: { epoch: "[redacted]" } },
        },
        unrelated: { epoch: "[redacted]" },
      });
  });

  it("redacts every free-form nested readiness diagnostic while preserving typed operator and planning authority facts", () => {
    const secret = "operator mounted secret";
    const arbitrary = "upstream nonhex text c2VjcmV0LXJlYWRpbmVzcy1ib2R5";
    const epoch = "9007199254740993";
    const body = {
      ready: false,
      deploymentFingerprint: "ab".repeat(32),
      observedAtMs: 1234,
      reasons: [
        "l1_driver_recompute_pending",
        `settlement_worker_failing:4:${arbitrary}`,
      ],
      details: [
        `settlement_jobs_stuck:1:withdrawal:${"dd".repeat(34)}:conclude:${arbitrary}`,
      ],
      dbError: "E_READINESS_DATABASE_UNAVAILABLE:query_cancelled",
      providerQueryError: arbitrary,
      localLedgerSlot: {
        error: arbitrary,
        mode: "cached_stale",
        evidenceAgeMs: 20,
      },
      nativeMpfOwner: { healthy: false, error: arbitrary },
      settlement: {
        state: "error",
        observedAt: 50,
        detail: arbitrary,
        workerFailures: { count: 4, since: 10, last: arbitrary },
      },
      settlementStuckJobs: {
        count: 1,
        first: {
          kind: "withdrawal",
          eventId: "dd".repeat(34),
          phase: "conclude",
        },
      },
      l1Follower: {
        lastError: arbitrary,
        readiness: [{ reason: "l1_events_orphan_recovery", detail: arbitrary }],
        refused: [
          {
            eventId: "ee".repeat(34),
            reason: "l1_event_identity_conflict",
            detail: arbitrary,
          },
        ],
      },
      livenessReasons: [
        {
          source: "commit",
          reason: "l1_driver_recompute_pending",
          detail: arbitrary,
          since: 1,
          ageMs: 10,
        },
      ],
      stateQueueMutationLease: {
        activeLease: {
          token: secret,
          lastError: arbitrary,
          holder: "commit",
          remainingMs: 500,
        },
      },
      commitProgress: {
        kind: "ok",
        generation: 3,
        slot: 4,
        headerHash: "cd".repeat(28),
        endTimeMs: 100,
        blockTimeMs: null,
        l1NowMs: 150,
        eventHorizonMs: 120,
      },
      commitPlanningProgress: {
        status: "current",
        reason: null,
        planned: {
          mode: "catch_up",
          selectedEndTimeMs: 110,
          selectedOrdinaryTxCount: 0,
          authority: {
            epoch,
            view: { generation: 3, slot: 4, height: 5, hash: "de".repeat(32) },
            baseHeaderHash: "cd".repeat(28),
            baseUtxosRoot: "ef".repeat(32),
            deploymentFingerprint: "ab".repeat(32),
          },
        },
      },
      unrelatedValue: secret,
      unexpectedDiagnosticField: arbitrary,
    };
    const presented = publicReadinessPresentation(body, "preprod-public", {
      L1_OPERATOR_SEED_PHRASE: secret,
    });
    const encoded = JSON.stringify(presented);
    expect(encoded).not.toContain(arbitrary);
    expect(encoded).not.toContain(secret);
    expect(presented.ready).toBe(false);
    expect(presented.dbError).toBe(body.dbError);
    expect(presented.reasons).toEqual([
      "l1_driver_recompute_pending",
      "settlement_worker_failing",
    ]);
    expect(presented.commitProgress).toEqual(body.commitProgress);
    expect(presented.l1Follower).toMatchObject({
      refused: [{ reason: "l1_event_identity_conflict" }],
    });
    for (const reason of [
      "services_unavailable",
      "queue_unavailable",
      "queue_unhealthy",
      "clock_unavailable",
      "source_unavailable",
      "source_timeout",
    ])
      expect(
        publicReadinessPresentation(
          { commitProgress: { kind: "unavailable", reason } },
          "preprod-public",
        ),
      ).toEqual({ commitProgress: { kind: "unavailable", reason } });
    expect(presented.commitPlanningProgress).toEqual(
      body.commitPlanningProgress,
    );
    expect(presented.settlementStuckJobs).toEqual(body.settlementStuckJobs);
    expect(presented.deploymentFingerprint).toBe(body.deploymentFingerprint);
    expect(presented.observedAtMs).toBe(body.observedAtMs);
    expect(publicReadinessPresentation(body, "preprod-testing")).toBe(body);
  });

  it.each(["mainnet", "preprod-public", "preprod-rehearsal"])(
    "redacts arbitrary provider diagnostics in %s while retaining controlled SQL reasons",
    (profile) => {
      const secret =
        "operator-wallet-secret nonhex provider response c2VjcmV0LWNyZWRlbnRpYWw=";
      for (const error of [
        new Error(secret),
        secret,
        { message: secret, code: secret },
      ])
        expect(readinessDatabaseError(error, profile)).toBe(
          "E_READINESS_DATABASE_UNAVAILABLE",
        );
      expect(
        readinessDatabaseError(
          { message: secret, cause: { code: "57014", detail: secret } },
          profile,
        ),
      ).toBe("E_READINESS_DATABASE_UNAVAILABLE:query_cancelled");
      expect(
        readinessDatabaseError({ code: "53300", message: secret }, profile),
      ).toBe("E_READINESS_DATABASE_UNAVAILABLE:capacity_unavailable");
      let accessed = false;
      expect(
        readinessDatabaseError(
          {
            get message() {
              accessed = true;
              throw new Error(secret);
            },
          },
          profile,
        ),
      ).toBe("E_READINESS_DATABASE_UNAVAILABLE");
      expect(accessed).toBe(false);
    },
  );

  it("retains the existing bounded private diagnostic presentation", () => {
    expect(
      readinessDatabaseError(
        new Error("private diagnostic\nsecond line"),
        "preprod-testing",
      ),
    ).toBe("private diagnostic");
  });
});

it("preserves source-defined commit alarms while withholding arbitrary worker stage text", () => {
  const stages = ["phase_a", "phase_b", "transaction_root_derivation"];
  for (const code of ["commit_build_held", "commit_stage_breaker"]) {
    const known = stages.map((stage) => `${code}:${stage}`);
    const source = {
      ready: false,
      reasons: [...known, `${code}:arbitrary_provider_SECRET_base64==`],
    };
    const result = publicReadinessPresentation(source, "preprod-public", {});
    expect(result.ready).toBe(false);
    expect(result.reasons).toEqual([...known, `${code}:unclassified`]);
    expect(JSON.stringify(result)).not.toContain(
      "arbitrary_provider_SECRET_base64",
    );
  }
});

it("retains actual typed readiness refusals and commit coordination phases", () => {
  const facts = evaluateReadiness({
    nowMillis: 2_000,
    maxHeartbeatAgeMs: 1_000,
    maxQueueDepth: 10,
    queueDepth: 11,
    workerHeartbeats: {
      blockCommitment: 1,
      blockConfirmation: 1,
      merge: 1,
      txQueueProcessor: 1,
    },
    localFinalizationPending: false,
    unresolvedBlockSubmissionAgeMs: 1_200,
    maxUnresolvedBlockSubmissionAgeMs: 1_000,
    dbHealthy: true,
    operatorMembership: "awaiting_activation",
    validationPool: {
      configuredWorkers: 4,
      liveWorkers: 2,
      restartingWorkers: 2,
      oldestInFlightAgeMs: 1_200,
      jobTimeoutMs: 1_000,
    },
    stateQueueMutationLease: {
      active: true,
      stale: true,
      remainingMs: -100,
      holder: "commit",
    },
    attestationTimeoutCorrection: {
      consecutiveFailures: 3,
      oldestUnattestedHeader: { headerHash: "ab".repeat(28), deadlineMs: 0 },
      lastProgressAtMs: 0,
      lastQueueReadAtMs: 0,
      stallBoundMs: 1_000,
      queueUnknownBoundMs: 1_000,
    },
  });
  expect(facts.ready).toBe(false);
  expect(facts.reasons).toContain("operator_not_yet_active");
  for (const phase of ["idle", "scheduler_alignment", "mutation_worker"]) {
    const body = {
      ...facts,
      blockCommitmentCoordination: {
        commitWorkerActive: true,
        commitPipelinePhase: phase,
      },
    };
    expect(publicReadinessPresentation(body, "preprod-public", {})).toEqual(
      body,
    );
  }
});

it("preserves only the exact pending block hash field and canonical hash shape", () => {
  const hash = "ab".repeat(32);
  const secret = "upstream-opaque-password/base64==";
  expect(
    publicReadinessPresentation(
      { stateQueue: { unconfirmedSubmittedBlockTxHash: hash } },
      "preprod-public",
      {},
    ),
  ).toEqual({ stateQueue: { unconfirmedSubmittedBlockTxHash: hash } });
  for (const value of [secret, "ab".repeat(28), "AB".repeat(32)])
    expect(
      publicReadinessPresentation(
        { stateQueue: { unconfirmedSubmittedBlockTxHash: value } },
        "preprod-public",
        {},
      ),
    ).toEqual({
      stateQueue: { unconfirmedSubmittedBlockTxHash: "[redacted]" },
    });
  expect(
    publicReadinessPresentation(
      { diagnostic: { unconfirmedSubmittedBlockTxHash: hash } },
      "preprod-public",
      {},
    ),
  ).toEqual({ diagnostic: { unconfirmedSubmittedBlockTxHash: "[redacted]" } });
});

it("shows the landed-block holds that stop the follower as their typed reasons", () => {
  const source = {
    ready: false,
    reasons: ["landed_block_local_fault", "landed_block_origin_after_frontier"],
  };
  expect(publicReadinessPresentation(source, "preprod-public", {})).toEqual(
    source,
  );
});

it.each(["mainnet", "preprod-public", "preprod-rehearsal"])(
  "shows a census reason code by name in %s while its details stay redacted",
  (profile) => {
    const arbitrary = "upstream provider text c2VjcmV0LXJlYWRpbmVzcw==";
    const result = publicReadinessPresentation(
      {
        ready: false,
        reasons: ["state_queue_unavailable"],
        livenessReasons: [
          {
            source: "commit",
            reason: "state_queue_unavailable",
            detail: arbitrary,
            error: arbitrary,
          },
        ],
      },
      profile,
      {},
    );
    expect(result).toEqual({
      ready: false,
      reasons: ["state_queue_unavailable"],
      livenessReasons: [
        {
          source: "commit",
          reason: "state_queue_unavailable",
          detail: "E_READINESS_DIAGNOSTIC_REDACTED",
          error: "E_READINESS_DIAGNOSTIC_REDACTED",
        },
      ],
    });
  },
);

it.each(["mainnet", "preprod-public"])(
  "redacts a reason code outside the census in %s",
  (profile) => {
    const code = "reason_outside_the_census";
    expect(
      publicReadinessPresentation(
        { reasons: [code], details: [code], l1Follower: { reason: code } },
        profile,
        {},
      ),
    ).toEqual({
      reasons: ["E_READINESS_DIAGNOSTIC_REDACTED"],
      details: ["E_READINESS_DIAGNOSTIC_REDACTED"],
      l1Follower: { reason: "E_READINESS_DIAGNOSTIC_REDACTED" },
    });
  },
);
