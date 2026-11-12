import { inspect } from "node:util";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Logger } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import { Frontier } from "../src/landed-blocks/store.js";
import { Globals, NodeConfig } from "../src/services/index.js";
import { Lucid } from "../src/services/lucid.js";
import { MidgardContracts } from "../src/services/midgard-contracts.js";
import { simCommitAnchor } from "./helpers/landed-blocks-sim.own.js";
import {
  prepareUnderTestLease,
  withTestMutationLease,
} from "./helpers/mutation-lease.js";
import {
  insertCommitIntent,
  land,
  ownLandedRow,
} from "./helpers/own-block-facts.js";
import { header, journalFixture } from "./helpers/own-journal-fixture.js";
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
import { makeTestNodeConfig } from "./helpers/test-node-config.js";

const nodeConfig = makeTestNodeConfig({
  STATE_QUEUE_MUTATION_LEASE_TTL_MS: 120_000,
  STATE_QUEUE_MUTATION_LEASE_RENEW_INTERVAL_MS: 30_000,
  COMMIT_EVENT_DEPTH: 0,
});

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

/** The journals' base, on the confirmed frontier. */
const BASE = journalFixture(header("base")).metadata.baseTailHeaderHash;

/** Puts the journals' base on the frontier: their blocks can land. */
const baseOnFrontier = Frontier.upsert({
  headerHash: BASE.toString("hex"),
  utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
});

/** A prepare anchored at the follower's tip: on the frontier's base, its
 * journal is the node's live journal, unless another one is. */
const anchored = { beforeJournalInsert: Effect.orDie(simCommitAnchor) };

/** A live journal holding a signed commit intent nothing has resolved yet. */
const signedIntentJournal = (headerHash: Buffer) =>
  Effect.gen(function* () {
    const txHash = Buffer.alloc(32, 9);
    yield* baseOnFrontier;
    yield* prepareUnderTestLease(
      { ...journalFixture(headerHash), preparedTxHash: txHash },
      anchored,
    );
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE pending_block_finalizations
      SET intended_tx_hash = ${txHash} WHERE header_hash = ${headerHash}`;
    yield* insertCommitIntent(
      headerHash,
      txHash,
      Buffer.from("84a0a0f5f6", "hex"),
    );
  });

const counts = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const [leases] = yield* sql<{ n: number }>`SELECT COUNT(*)::int AS n
    FROM state_queue_mutation_leases`;
  const [journals] = yield* sql<{ n: number }>`SELECT COUNT(*)::int AS n
    FROM pending_block_finalizations`;
  return { leases: leases?.n ?? -1, journals: journals?.n ?? -1 };
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
    buildAndSubmitMock.mockImplementation((leaseToken: string) =>
      Effect.gen(function* () {
        prepared.count += 1;
        const journal = journalFixture(
          header(`next-block-${prepared.count.toString()}`),
        );
        yield* PendingBlockFinalizationsDB.preparePendingSubmission({
          ...journal,
          metadata: { ...journal.metadata, stateQueueLeaseToken: leaseToken },
        });
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

        // Another operator's block takes the slot after its base: the
        // journal is dead, and the intent resolved.
        yield* land(
          ownLandedRow(
            header("foreign-block").toString("hex"),
            BASE.toString("hex"),
            {
              kind: "foreign",
              applied: true,
            },
          ),
        );
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
    expect(result.whileSigned).toEqual({ leases: 0, journals: 1 });
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
    // The dead journal stays; the worker prepared the next one.
    expect(result.afterClear).toEqual({ leases: 1, journals: 2 });
    expect(
      result.resumedLogs.some((line) =>
        line.includes(
          `signed commit intent header=${result.signedHex} is reconciled`,
        ),
      ),
    ).toBe(true);
  });

  it("still runs the worker for a live journal without a signed intent", async () => {
    const result = await runWithNode(
      Effect.gen(function* () {
        yield* baseOnFrontier;
        yield* prepareUnderTestLease(
          journalFixture(header("unsigned-pending")),
          anchored,
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
    expect(result.counts).toEqual({ leases: 1, journals: 1 });
  });

  it("lets only one of two concurrent prepares under one lease create a live journal", async () => {
    const result = await runWithNode(
      Effect.gen(function* () {
        yield* baseOnFrontier;
        // Each prepare holds the lease row FOR UPDATE: the second waits for
        // the first to commit, then finds its journal live.
        const outcomes = yield* Effect.all(
          [
            Effect.either(
              PendingBlockFinalizationsDB.preparePendingSubmission(
                journalFixture(header("planner-a")),
                anchored,
              ),
            ),
            Effect.either(
              PendingBlockFinalizationsDB.preparePendingSubmission(
                journalFixture(header("planner-b")),
                anchored,
              ),
            ),
          ],
          { concurrency: "unbounded" },
        ).pipe(
          withTestMutationLease(
            journalFixture(header("planner-a")).metadata.stateQueueLeaseToken,
          ),
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
      PendingBlockFinalizationsDB.LIVE_JOURNAL_REFUSAL,
    );
    expect(result.counts.journals).toBe(1);
  });
});
