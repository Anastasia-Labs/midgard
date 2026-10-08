import "./utils.js";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { buildListenRouter } from "../src/commands/listen-router.js";
import { takeCommitWorkerOutput } from "../src/fibers/block-commitment.js";
import { NodeConfig } from "../src/services/config.js";
import {
  Globals,
  nextL1ProviderHealthEvidence,
} from "../src/services/globals.js";
import {
  clearLivenessReason,
  setLivenessReason,
} from "../src/services/globals.liveness-reasons.js";
import {
  type ActiveLivenessReason,
  clearLivenessIncident,
  COMMIT_DA_FRAME_EVENTS_OVERFLOW,
  COMMIT_DA_FRAME_LEDGER_CEILING,
  COMMIT_DA_FRAME_SOURCE,
  HaltSource,
  HISTORY_CORRECTION_REWIND_SOURCE,
  raiseLivenessIncident,
} from "../src/services/liveness-halt.js";
import { Lucid } from "../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../src/services/midgard-contracts.js";
import type { NativeMpfOwnerService } from "../src/services/mpf-native-owner/protocol.js";
import { ValidationPool } from "../src/services/validation-pool.js";
import {
  COMMIT_DA_FRAME_FITS_NOTICE,
  commitDaFrameNoticeForOutcome,
} from "../src/workers/utils/commit-block-planner.commit-da-frame-notice.js";
import { seedCaughtUpL1Follower } from "./readiness-l1-follower.fixture.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

/**
 * A raised liveness reason holds block production while the node keeps
 * serving, so `/readyz` must name it: unready (503) while it is raised, with
 * its source, age and escalation, and ready again once it clears.
 */

// Only the settings /readyz reads. Provider evidence is published before
// every request, so the handler never probes a real provider.
const nodeConfig = {
  READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS: 60_000,
  L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: 1_000,
  READINESS_MAX_HEARTBEAT_AGE_MS: 60_000,
  READINESS_MAX_DURABLE_ADMISSION_BACKLOG: 1_000,
  READINESS_MAX_DURABLE_ADMISSION_AGE_MS: 60_000,
  UNCONFIRMED_BLOCK_MAX_AGE_MS: 60_000,
  VALIDATION_WORKER_JOB_TIMEOUT_MS: 60_000,
  STATE_QUEUE_MUTATION_LEASE_STALE_GRACE_MS: 60_000,
  MIN_QUEUE_LENGTH_FOR_MERGING: 1,
} as unknown as NodeConfig["Type"];

const responsiveOwner = {
  diagnostics: () =>
    Promise.resolve({
      ownerEpoch: Buffer.alloc(16, 1),
      durableRoot: "ab".repeat(32),
      residentNodes: 0,
      residentEdges: 0,
      residentBytes: 0,
      activeGenerations: 0,
      generatedNodes: 0,
      generatedBytes: 0,
      rssBytes: 0,
      peakRssBytes: 0,
      childRestarts: 0,
    }),
} as unknown as NativeMpfOwnerService;

type Readyz = {
  readonly status: number;
  readonly ready: boolean;
  readonly reasons: readonly string[];
  readonly details: readonly string[];
  readonly commitDaFramePressure?: { readonly candidateStagePercent: number };
  readonly livenessReasons?: readonly ActiveLivenessReason[];
};

type Node = {
  readonly globals: Globals;
  readonly readyz: Effect.Effect<Readyz>;
};

/** Runs `scenario` against one node's globals, which every `readyz` of it
 * reads. The database is reset before and after, so a row another file
 * left cannot turn a ready answer unready, and none leaks on. */
const onNode = <A, E>(
  scenario: (node: Node) => Effect.Effect<A, E, SqlClient.SqlClient>,
): Promise<A> =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const clear = resetApplicationTables;
        return yield* Effect.gen(function* () {
          yield* clear;
          const globals = yield* Globals;
          yield* seedCaughtUpL1Follower(globals);
          yield* Ref.set(globals.NATIVE_MPF_OWNER, responsiveOwner);
          const readyz = Effect.gen(function* () {
            yield* Ref.update(globals.L1_PROVIDER_HEALTH, (current) =>
              nextL1ProviderHealthEvidence({
                current,
                healthy: true,
                observedAtMs: Date.now(),
                successKind: "exact",
              }),
            );
            const response = (yield* buildListenRouter().pipe(
              Effect.provideService(
                HttpServerRequest.HttpServerRequest,
                HttpServerRequest.fromWeb(
                  new Request("http://midgard.test/readyz"),
                ),
              ),
            )) as HttpServerResponse.HttpServerResponse;
            const web = HttpServerResponse.toWeb(response);
            const body = (yield* Effect.promise(() => web.json())) as Omit<
              Readyz,
              "status"
            >;
            return { status: web.status, ...body };
          }).pipe(Effect.orDie) as unknown as Effect.Effect<Readyz>;
          return yield* scenario({ globals, readyz });
        }).pipe(Effect.ensuring(Effect.orDie(clear)));
      }).pipe(
        Effect.provideService(NodeConfig, nodeConfig),
        Effect.provideService(ValidationPool, {
          poolSize: 1,
          stats: Effect.succeed({
            oldestInFlightAgeMs: 0,
            liveWorkers: 1,
            restartingWorkers: 0,
          }),
        } as unknown as ValidationPool["Type"]),
        Effect.provideService(Lucid, {} as Lucid),
        Effect.provideService(MidgardContracts, {} as MidgardContracts),
        Effect.provideService(
          ContractDeploymentIdentity,
          ContractDeploymentIdentity.make({
            kind: "derived",
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          }),
        ),
        Effect.provide(Globals.Default),
      ) as unknown as Effect.Effect<A, unknown, never>,
    ),
  );

const SOURCE = HaltSource.stateQueueCorrectionRewind;
const REASON = "state_queue_correction_rewind_conflict";
const ESCALATE_AFTER_MS = 10 * 60_000;

describe("GET /readyz liveness reasons", () => {
  it.each([50, 75, 90])(
    "serves measured stage %i as a readiness detail without failing readiness",
    async (stage) => {
      const response = await onNode(({ globals, readyz }) =>
        Effect.gen(function* () {
          takeCommitWorkerOutput(
            globals,
            commitDaFrameNoticeForOutcome({
              outcome: "fits",
              passes: 1,
              baseEmptyBlockInnerBytes: 1_000,
              maxInnerBytes: 100_000,
              measurement: {
                innerBytesUpperBound: stage * 1_000,
                acceptedTxCount: 1,
                rejectedTxIds: [],
              },
            })!,
            0,
          );
          return yield* readyz;
        }),
      );
      expect(response.status).toBe(200);
      expect(response.reasons).toEqual([]);
      expect(response.commitDaFramePressure).toMatchObject({
        candidateStagePercent: stage,
      });
      expect(response.details).toContain(
        `commit_da_frame_pressure:candidate:${stage.toString()}`,
      );
    },
  );

  it("reports failed commit workers while ticks stay fresh, until actual worker success", async () => {
    const [failed, noOp, cleared] = await onNode(({ globals, readyz }) =>
      Effect.gen(function* () {
        takeCommitWorkerOutput(
          globals,
          {
            type: "FailureOutput",
            error:
              "Commit worker requires a unique parent-generated ledger MPF lease owner",
          },
          0,
        );
        yield* Ref.set(globals.HEARTBEAT_BLOCK_COMMITMENT, Date.now());
        const failed = yield* readyz;
        // A no-op can defer local finalization or exclude events beyond a
        // source-owned end time, so it alone cannot clear a failed worker.
        takeCommitWorkerOutput(globals, { type: "NothingToCommitOutput" }, 0);
        const noOp = yield* readyz;
        takeCommitWorkerOutput(
          globals,
          {
            type: "SuccessfulLocalFinalizationRecoveryOutput",
            finalizedHeaderHash: "ab".repeat(28),
            mempoolTxsCount: 0,
            sizeOfBlocksTxs: 0,
            mempoolLedgerDeletedOutRefHexes: [],
          },
          0,
        );
        return [failed, noOp, yield* readyz] as const;
      }),
    );
    expect(failed.status).toBe(503);
    expect(failed.ready).toBe(false);
    expect(failed.reasons).toEqual(["commit_worker_failed"]);
    expect(failed.livenessReasons).toEqual([
      expect.objectContaining({
        source: "commit_worker",
        reason: "commit_worker_failed",
      }),
    ]);
    expect(noOp.status).toBe(503);
    expect(noOp.reasons).toEqual(["commit_worker_failed"]);
    expect(cleared.status).toBe(200);
    expect(cleared.ready).toBe(true);
    expect(cleared.reasons).toEqual([]);
  });

  it.each([
    [1_000, COMMIT_DA_FRAME_EVENTS_OVERFLOW],
    [100_001, COMMIT_DA_FRAME_LEDGER_CEILING],
  ] as const)(
    "projects the parent's DA frame refusal %i as %s and clears on fitting evidence",
    async (baseEmptyBlockInnerBytes, reason) => {
      const [before, raised, cleared] = await onNode(({ globals, readyz }) =>
        Effect.gen(function* () {
          const before = yield* readyz;
          const notice = commitDaFrameNoticeForOutcome({
            outcome: "no_transactions_to_drop",
            passes: 3,
            baseEmptyBlockInnerBytes,
            maxInnerBytes: 100_000,
          })!;
          expect(takeCommitWorkerOutput(globals, notice, 0)).toBeUndefined();
          const raised = yield* readyz;
          // A subsequent measured block supplies the evidence that clears it.
          takeCommitWorkerOutput(globals, COMMIT_DA_FRAME_FITS_NOTICE, 0);
          return [before, raised, yield* readyz] as const;
        }),
      );
      expect(before.status).toBe(200);
      expect(raised.status).toBe(503);
      expect(raised.ready).toBe(false);
      expect(raised.reasons).toEqual([reason]);
      expect(raised.livenessReasons).toEqual([
        expect.objectContaining({ source: COMMIT_DA_FRAME_SOURCE, reason }),
      ]);
      expect(cleared.status).toBe(200);
      expect(cleared.ready).toBe(true);
      expect(cleared.reasons).toEqual([]);
      expect(cleared.livenessReasons).toEqual([]);
    },
  );
  it("names a raised hold with its age, and is ready again once it clears", async () => {
    const [before, raised, cleared] = await onNode(({ globals, readyz }) =>
      Effect.gen(function* () {
        const before = yield* readyz;
        yield* raiseLivenessIncident(
          globals,
          SOURCE,
          REASON,
          "removal disagrees with L1",
          { escalateAfterMs: ESCALATE_AFTER_MS },
        );
        const raised = yield* readyz;
        yield* clearLivenessIncident(globals, SOURCE);
        return [before, raised, yield* readyz] as const;
      }),
    );
    expect(before.status).toBe(200);
    expect(before.reasons).toEqual([]);

    expect(raised.status).toBe(503);
    expect(raised.ready).toBe(false);
    expect(raised.reasons).toEqual([REASON]);
    expect(raised.livenessReasons).toEqual([
      {
        source: SOURCE,
        reason: REASON,
        ageMs: expect.any(Number),
        escalateAfterMs: ESCALATE_AFTER_MS,
        escalated: false,
      },
    ]);

    expect(cleared.status).toBe(200);
    expect(cleared.ready).toBe(true);
    expect(cleared.reasons).toEqual([]);
    expect(cleared.livenessReasons).toEqual([]);
  });

  it("flags a hold raised past its escalation bound, and stays serving", async () => {
    const escalated = await onNode(({ globals, readyz }) =>
      Effect.gen(function* () {
        yield* raiseLivenessIncident(
          globals,
          HaltSource.stateQueueCorrectionRewind,
          "state_queue_correction_rewind_conflict",
          "removal disagrees with L1",
          { escalateAfterMs: 0 },
        );
        return yield* readyz;
      }),
    );
    expect(escalated.status).toBe(503);
    expect(escalated.reasons).toEqual([
      "state_queue_correction_rewind_conflict",
    ]);
    expect(escalated.livenessReasons).toEqual([
      expect.objectContaining({
        source: HaltSource.stateQueueCorrectionRewind,
        escalateAfterMs: 0,
        escalated: true,
      }),
    ]);
  });

  it("names a reason a fiber sets directly, keeping its age across a changed reason", async () => {
    const source = "user_event_ingestion:deposit";
    const [first, second, cleared] = await onNode(({ globals, readyz }) =>
      Effect.gen(function* () {
        yield* setLivenessReason(globals, source, "stalled:3");
        const first = yield* readyz;
        yield* Effect.sleep("20 millis");
        yield* setLivenessReason(globals, source, "stalled:4");
        const second = yield* readyz;
        yield* clearLivenessReason(globals, source);
        return [first, second, yield* readyz] as const;
      }),
    );
    expect(first.reasons).toEqual(["stalled:3"]);
    expect(second.reasons).toEqual(["stalled:4"]);
    const [entry] = second.livenessReasons ?? [];
    expect(entry).toMatchObject({ source, escalated: false });
    expect(entry?.ageMs).toBeGreaterThanOrEqual(20);
    expect(cleared.status).toBe(200);
    expect(cleared.livenessReasons).toEqual([]);
  });

  it("lists one reason two sources raise once, with both sources", async () => {
    const both = await onNode(({ globals, readyz }) =>
      Effect.gen(function* () {
        yield* raiseLivenessIncident(globals, SOURCE, REASON, "confirmation");
        yield* raiseLivenessIncident(
          globals,
          HISTORY_CORRECTION_REWIND_SOURCE,
          REASON,
          "history",
        );
        return yield* readyz;
      }),
    );
    expect(both.reasons).toEqual([REASON]);
    expect(
      (both.livenessReasons ?? []).map((entry) => entry.source).sort(),
    ).toEqual([SOURCE, HISTORY_CORRECTION_REWIND_SOURCE].sort());
  });
});
