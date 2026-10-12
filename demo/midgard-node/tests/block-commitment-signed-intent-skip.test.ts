import "./utils.js";

import { inspect } from "node:util";

import { SqlClient } from "@effect/sql";
import { Effect, Logger } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import { Globals, NodeConfig } from "../src/services/index.js";
import { Lucid } from "../src/services/lucid.js";
import { MidgardContracts } from "../src/services/midgard-contracts.js";
import {
  header,
  journalFixture,
} from "./local-mutation-job-abandonment.journal-fixture.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const prepared = vi.hoisted(() => ({ count: 0 }));
const buildAndSubmitMock = vi.hoisted(() => vi.fn());

// The commit worker itself: each run prepares one fresh journal under the
// lease it was given, as the real worker does before signing.
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

import {
  blockCommitmentAction,
  SKIPPED_ACTIVE_PENDING_FINALIZATION,
} from "../src/fibers/block-commitment.block-commitment-action.js";

const nodeConfig = {
  STATE_QUEUE_MUTATION_LEASE_TTL_MS: 120_000,
  STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS: 30_000,
  COMMIT_EVENT_DEPTH: 0,
} as unknown as NodeConfig["Type"];

const runWithNode = <A, E, R>(program: Effect.Effect<A, E, R>) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        return yield* program.pipe(
          Effect.ensuring(Effect.orDie(resetApplicationTables)),
        );
      }).pipe(
        Effect.provideService(NodeConfig, nodeConfig),
        Effect.provideService(Lucid, {} as Lucid),
        Effect.provideService(MidgardContracts, {} as MidgardContracts),
        Effect.provide(Globals.Default),
      ),
    ) as Effect.Effect<A, unknown, never>,
  );

/** A journal holding a signed commit intent nothing has resolved yet. */
const signedIntentJournal = (headerHash: Buffer) =>
  Effect.gen(function* () {
    const txHash = Buffer.alloc(32, 9);
    yield* PendingBlockFinalizationsDB.preparePendingSubmission({
      ...journalFixture(headerHash),
      preparedTxHash: txHash,
    });
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE pending_block_finalizations
      SET intended_tx_hash = ${txHash}, signed_tx_cbor = ${Buffer.from("84a0a0f5f6", "hex")}
      WHERE header_hash = ${headerHash}`;
  });

const counts = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const [leases] = yield* sql<{ n: number }>`SELECT COUNT(*)::int AS n
    FROM state_queue_mutation_leases`;
  const [pending] = yield* sql<{ n: number }>`SELECT COUNT(*)::int AS n
    FROM pending_block_finalizations WHERE status = 'pending_submission'`;
  return { leases: leases?.n ?? -1, pendingSubmissions: pending?.n ?? -1 };
});

const collectLogs = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  Effect.gen(function* () {
    const logs: string[] = [];
    const value = yield* effect.pipe(
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
    return { value, logs };
  });

describe("block commitment over an unreconciled signed intent", () => {
  beforeEach(() => {
    prepared.count = 0;
    buildAndSubmitMock.mockReset();
    buildAndSubmitMock.mockImplementation(() =>
      Effect.gen(function* () {
        prepared.count += 1;
        yield* PendingBlockFinalizationsDB.preparePendingSubmission(
          journalFixture(header(`next-block-${prepared.count.toString()}`)),
        );
        return { type: "NothingToCommitOutput" };
      }),
    );
  });

  it("skips without a lease or worker while the intent is unresolved, then prepares exactly one block once it clears", async () => {
    const result = await runWithNode(
      Effect.gen(function* () {
        const signed = header("signed-intent");
        yield* signedIntentJournal(signed);

        const skipped = yield* collectLogs(
          Effect.all([
            blockCommitmentAction,
            blockCommitmentAction,
            blockCommitmentAction,
          ]),
        );
        const whileSigned = yield* counts;
        const workerCallsWhileSigned = buildAndSubmitMock.mock.calls.length;

        // The landed-block rebase disposes of the intent's journal.
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE pending_block_finalizations
          SET status = 'abandoned', updated_at = NOW()
          WHERE header_hash = ${signed}`;
        const resumed = yield* collectLogs(blockCommitmentAction);
        return {
          skippedLogs: skipped.logs,
          whileSigned,
          workerCallsWhileSigned,
          resumedLogs: resumed.logs,
          afterClear: yield* counts,
          workerCalls: buildAndSubmitMock.mock.calls.length,
          signedHex: signed.toString("hex"),
        };
      }),
    );

    expect(result.workerCallsWhileSigned).toBe(0);
    expect(result.whileSigned).toEqual({ leases: 0, pendingSubmissions: 1 });
    const skipLines = result.skippedLogs.filter((line) =>
      line.includes(SKIPPED_ACTIVE_PENDING_FINALIZATION),
    );
    expect(skipLines).toHaveLength(1);
    expect(skipLines[0]).toContain(`header=${result.signedHex}`);
    expect(skipLines[0]).toMatch(/pending_finalization_age:\d+/u);
    expect(
      result.skippedLogs.some((line) =>
        line.includes("New block commitment process started"),
      ),
    ).toBe(false);

    expect(result.workerCalls).toBe(1);
    expect(prepared.count).toBe(1);
    expect(result.afterClear).toEqual({ leases: 1, pendingSubmissions: 1 });
    expect(
      result.resumedLogs.some((line) =>
        line.includes(
          `signed commit intent header=${result.signedHex} is reconciled`,
        ),
      ),
    ).toBe(true);
  });

  it("still runs the worker for an active journal without a signed intent", async () => {
    const result = await runWithNode(
      Effect.gen(function* () {
        yield* PendingBlockFinalizationsDB.preparePendingSubmission(
          journalFixture(header("unsigned-pending")),
        );
        const outcome = yield* Effect.either(blockCommitmentAction);
        return {
          outcome: outcome._tag,
          workerCalls: buildAndSubmitMock.mock.calls.length,
          counts: yield* counts,
        };
      }),
    );

    // The worker ran and the prepare guard, unchanged, refused its block.
    expect(result.workerCalls).toBe(1);
    expect(result.outcome).toBe("Left");
    expect(result.counts).toEqual({ leases: 1, pendingSubmissions: 1 });
  });

  it("lets only one of two concurrent prepares create a pending journal", async () => {
    const result = await runWithNode(
      Effect.gen(function* () {
        const outcomes = yield* Effect.all(
          [
            Effect.either(
              PendingBlockFinalizationsDB.preparePendingSubmission(
                journalFixture(header("planner-a")),
              ),
            ),
            Effect.either(
              PendingBlockFinalizationsDB.preparePendingSubmission(
                journalFixture(header("planner-b")),
              ),
            ),
          ],
          { concurrency: "unbounded" },
        );
        return {
          tags: outcomes.map((outcome) => outcome._tag).sort(),
          // The fixture's history gate wraps the guard's refusal as its cause.
          refusal: outcomes.flatMap((outcome) =>
            outcome._tag === "Left"
              ? [inspect(outcome.left, { depth: 12, colors: false })]
              : [],
          ),
          counts: yield* counts,
        };
      }),
    );

    expect(result.tags).toEqual(["Left", "Right"]);
    expect(result.refusal[0]).toContain(
      "Refusing to prepare a new pending block while another active pending-finalization record exists",
    );
    expect(result.counts.pendingSubmissions).toBe(1);
  });
});
