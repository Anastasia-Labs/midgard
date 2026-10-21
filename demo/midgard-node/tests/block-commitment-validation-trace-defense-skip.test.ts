/**
 * The commit fiber's pause for a due validation-trace Defend: while the
 * defender's fresh decision names a Defend it can build, a commit tick takes
 * no lease and runs no worker; the pause is bounded, so once the decision is
 * stale or the Defend has been due past the bound, and with no follower or
 * nothing due, the tick commits as before. Each Defend pauses commits once:
 * one still due after the commits that ran past its bound pauses no more.
 */
import "./utils.js";

import { SqlClient } from "@effect/sql";
import { Effect, Logger, Ref } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import {
  SKIPPED_DUE_VALIDATION_TRACE_DEFENSE,
  VALIDATION_TRACE_DEFENSE_COMMIT_PAUSE_MS,
} from "../src/fibers/block-commitment.validation-trace-defense-pause.js";
import { Globals, NodeConfig } from "../src/services/index.js";
import { Lucid } from "../src/services/lucid.js";
import { MidgardContracts } from "../src/services/midgard-contracts.js";
import {
  initialValidationTraceDefenseHealth,
  type ValidationTraceDefenseHealth,
} from "../src/validation-trace-defense/health.js";
import { recordDefendFirstDue } from "../src/validation-trace-defense/tick.js";
import {
  header,
  journalFixture,
} from "./local-mutation-job-abandonment.journal-fixture.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const buildAndSubmitMock = vi.hoisted(() => vi.fn());

// The commit worker: each run prepares one journal under its lease.
vi.mock(
  "../src/fibers/block-commitment.build-and-submit-commitment-block-action.js",
  () => ({ buildAndSubmitCommitmentBlockAction: buildAndSubmitMock }),
);

// Due work exists and the scheduler needs no alignment: the tick goes
// straight to the lease.
vi.mock(
  "../src/fibers/block-commitment.should-skip-for-detailed-scheduler-due-work.js",
  async (importOriginal) => {
    const { Effect: EffectModule } = await import("effect");
    return {
      ...(await importOriginal<Record<string, unknown>>()),
      shouldSkipIdleCommitPipelineBeforeSchedulerAlignment:
        EffectModule.succeed(false),
    };
  },
);
vi.mock(
  "../src/fibers/block-commitment.should-skip-for-registered-commit-due-work.js",
  async (importOriginal) => {
    const { Effect: EffectModule } = await import("effect");
    return {
      ...(await importOriginal<Record<string, unknown>>()),
      shouldSkipForRegisteredCommitDueWork: EffectModule.succeed(false),
      registerPreLeaseCommitSchedulerDueWorkIfProven:
        EffectModule.succeed(false),
      alignCommitSchedulerBeforeMutationWorkerIfIdle:
        EffectModule.succeed(false),
    };
  },
);

import { blockCommitmentAction } from "../src/fibers/block-commitment.block-commitment-action.js";

const nodeConfig = {
  STATE_QUEUE_MUTATION_LEASE_TTL_MS: 120_000,
  STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS: 30_000,
  COMMIT_EVENT_DEPTH: 0,
} as unknown as NodeConfig["Type"];

const leaseCount = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const [leases] = yield* sql<{ n: number }>`SELECT COUNT(*)::int AS n
    FROM state_queue_mutation_leases`;
  return leases?.n ?? -1;
});

/** One commit tick with the defender's health at `health(now)`. */
const tickWith = (health: (nowMs: number) => ValidationTraceDefenseHealth) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        const globals = yield* Globals;
        yield* Ref.set(
          globals.VALIDATION_TRACE_DEFENSE_HEALTH,
          health(Date.now()),
        );
        const logs: string[] = [];
        yield* blockCommitmentAction.pipe(
          Effect.provide(
            Logger.replace(
              Logger.defaultLogger,
              Logger.make(({ message }) => {
                logs.push(
                  (Array.isArray(message) ? message : [message])
                    .map(String)
                    .join(" "),
                );
              }),
            ),
          ),
        );
        const leases = yield* leaseCount;
        yield* Effect.orDie(resetApplicationTables);
        return {
          leases,
          workerCalls: buildAndSubmitMock.mock.calls.length,
          skipped: logs.some((line) =>
            line.includes(SKIPPED_DUE_VALIDATION_TRACE_DEFENSE),
          ),
        };
      }).pipe(
        Effect.provideService(NodeConfig, nodeConfig),
        Effect.provideService(Lucid, {} as Lucid),
        Effect.provideService(MidgardContracts, {} as MidgardContracts),
        Effect.provide(Globals.Default),
      ),
    ) as Effect.Effect<
      { leases: number; workerCalls: number; skipped: boolean },
      unknown,
      never
    >,
  );

const due = [{ subject: "ab".repeat(28), deadline: Number.MAX_SAFE_INTEGER }];
const bound = VALIDATION_TRACE_DEFENSE_COMMIT_PAUSE_MS;
/** The L1 time the defender decides at: before every deadline here. */
const l1Now = 0;

/** `due` first became due at wall-clock `sinceMs`, with no commit in flight. */
const dueSince = (sinceMs: number) =>
  recordDefendFirstDue(new Map(), due, false, sinceMs, l1Now);

describe("block commitment beside a due validation-trace Defend", () => {
  beforeEach(() => {
    buildAndSubmitMock.mockReset();
    buildAndSubmitMock.mockImplementation(() =>
      Effect.gen(function* () {
        yield* PendingBlockFinalizationsDB.preparePendingSubmission(
          journalFixture(header("defense-pause-next-block")),
        );
        return { type: "NothingToCommitOutput" };
      }),
    );
  });

  it("takes no lease and runs no worker while a fresh decision names a due Defend", async () => {
    const result = await tickWith((now) => ({
      ...initialValidationTraceDefenseHealth(now),
      following: true,
      dueDefends: due,
      defendFirstDue: dueSince(now - 1_000),
      decidedAtMs: now,
    }));
    expect(result).toStrictEqual({ leases: 0, workerCalls: 0, skipped: true });
  });

  it("pauses commits once for a Defend that stays due: the commit that ran past the bound starts no new pause", async () => {
    // Past the bound, a commit went out; the Defend was due while it was in
    // flight and is due still once it has reconciled.
    const result = await tickWith((now) => {
      const inFlight = recordDefendFirstDue(
        dueSince(now - bound - 2_000),
        due,
        true,
        now - 1_000,
        l1Now,
      );
      return {
        ...initialValidationTraceDefenseHealth(now),
        following: true,
        dueDefends: due,
        defendFirstDue: recordDefendFirstDue(inFlight, due, false, now, l1Now),
        decidedAtMs: now,
      };
    });
    expect(result).toStrictEqual({ leases: 1, workerCalls: 1, skipped: false });
  });

  it.each([
    [
      "nothing is due",
      (now: number) => ({
        ...initialValidationTraceDefenseHealth(now),
        following: true,
      }),
    ],
    [
      "no follower runs",
      (now: number) => ({
        ...initialValidationTraceDefenseHealth(now),
        dueDefends: due,
        defendFirstDue: dueSince(now),
      }),
    ],
    [
      "the decision is stale",
      (now: number) => ({
        ...initialValidationTraceDefenseHealth(now),
        following: true,
        dueDefends: due,
        defendFirstDue: dueSince(now - 1_000),
        decidedAtMs: now - bound - 1,
      }),
    ],
    [
      "the Defend has been due past the bound",
      (now: number) => ({
        ...initialValidationTraceDefenseHealth(now),
        following: true,
        dueDefends: due,
        defendFirstDue: dueSince(now - bound - 1),
        decidedAtMs: now,
      }),
    ],
  ])("commits as before when %s", async (_, health) => {
    const result = await tickWith(health);
    expect(result).toStrictEqual({ leases: 1, workerCalls: 1, skipped: false });
  });
});
