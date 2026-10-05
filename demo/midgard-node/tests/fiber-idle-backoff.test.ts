import { SqlClient } from "@effect/sql";
import {
  Effect,
  Fiber,
  Logger,
  LogLevel,
  Option,
  Ref,
  TestClock,
  TestContext,
} from "effect";
import { describe, expect, it } from "vitest";

import {
  CONFIRMATION_IDLE_BACKOFF_KEY,
  recordConfirmationTickIdleness,
  skipIdleConfirmationTick,
} from "../src/fibers/block-confirmation.idle-backoff.js";
import {
  MERGE_IDLE_BACKOFF_KEY,
  recordMergeTickIdleness,
  skipIdleMergeTick,
} from "../src/fibers/merge.idle-backoff.js";
import {
  speculativeCommitBuilderFiber,
  speculativeCommitSubmitterFiber,
} from "../src/fibers/speculative-commit-builder.submit-speculative-candidate-on-confirmation.js";
import { nextIdleBackoffState } from "../src/services/globals.idle-backoff.js";
import { logOnStateChange } from "../src/services/globals.liveness-reasons.js";
import { Globals, NodeConfig } from "../src/services/index.js";

const runWithGlobals = <A, E>(effect: Effect.Effect<A, E, Globals>) =>
  Effect.runPromise(effect.pipe(Effect.provide(Globals.Default)));

const confirmedTip = { utxo: "confirmed" } as never;

describe("idle backoff", () => {
  it("grows exponentially from the base up to the cap", () => {
    const options = { baseMs: 2_000, maxMs: 30_000 };
    const delays: number[] = [];
    let state = undefined as
      | ReturnType<typeof nextIdleBackoffState>
      | undefined;
    for (let i = 0; i < 6; i += 1) {
      state = nextIdleBackoffState(state, options, 0);
      delays.push(state.skipUntilMs);
    }
    expect(delays).toEqual([4_000, 8_000, 16_000, 30_000, 30_000, 30_000]);
  });
});

describe("confirmation idle backoff", () => {
  it("skips refreshes only while provably idle and resets on any sign of work", async () => {
    const outcome = await runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, confirmedTip);
        yield* Ref.set(globals.COMMIT_PIPELINE_IDLE, true);
        const none = Option.none();
        const beforeAnyIdleTick = yield* skipIdleConfirmationTick(
          globals,
          none,
        );
        yield* recordConfirmationTickIdleness(globals, none, 1_000);
        const whileIdle = yield* skipIdleConfirmationTick(globals, none);
        const withPendingJournal = yield* skipIdleConfirmationTick(
          globals,
          Option.some("journal"),
        );
        const resetByJournal = !(yield* Ref.get(globals.IDLE_BACKOFF)).has(
          CONFIRMATION_IDLE_BACKOFF_KEY,
        );
        yield* recordConfirmationTickIdleness(globals, none, 1_000);
        yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "ab");
        const withSubmittedBlock = yield* skipIdleConfirmationTick(
          globals,
          none,
        );
        yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
        yield* recordConfirmationTickIdleness(globals, none, 1_000);
        yield* Ref.set(globals.COMMIT_PIPELINE_IDLE, false);
        const withCommitWork = yield* skipIdleConfirmationTick(globals, none);
        return {
          beforeAnyIdleTick,
          whileIdle,
          withPendingJournal,
          resetByJournal,
          withSubmittedBlock,
          withCommitWork,
        };
      }),
    );
    expect(outcome).toEqual({
      beforeAnyIdleTick: false,
      whileIdle: true,
      withPendingJournal: false,
      resetByJournal: true,
      withSubmittedBlock: false,
      withCommitWork: false,
    });
  });

  it("never backs off before a confirmed tip is published", async () => {
    const skipped = await runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Ref.set(globals.COMMIT_PIPELINE_IDLE, true);
        yield* recordConfirmationTickIdleness(globals, Option.none(), 1_000);
        return yield* skipIdleConfirmationTick(globals, Option.none());
      }),
    );
    expect(skipped).toBe(false);
  });
});

describe("merge idle backoff", () => {
  it("skips scheduled attempts after an empty-queue result, keeps the heartbeat fresh, and resets once a block is queued", async () => {
    const outcome = await runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const first = yield* skipIdleMergeTick(globals);
        yield* recordMergeTickIdleness(
          globals,
          { status: "no_queued_block", reason: "queue_length=0" },
          1_000,
        );
        yield* Ref.set(globals.HEARTBEAT_MERGE, 0);
        const whileIdle = yield* skipIdleMergeTick(globals);
        const heartbeat = yield* Ref.get(globals.HEARTBEAT_MERGE);
        yield* Ref.set(globals.BLOCKS_IN_QUEUE, 1);
        const withQueuedBlock = yield* skipIdleMergeTick(globals);
        const reset = !(yield* Ref.get(globals.IDLE_BACKOFF)).has(
          MERGE_IDLE_BACKOFF_KEY,
        );
        yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
        yield* recordMergeTickIdleness(
          globals,
          {
            status: "skipped_oldest_block_not_mature",
            reason: "not mature",
          },
          1_000,
        );
        const afterOtherSkip = yield* skipIdleMergeTick(globals);
        return {
          first,
          whileIdle,
          heartbeatFresh: heartbeat > 0,
          withQueuedBlock,
          reset,
          afterOtherSkip,
        };
      }),
    );
    expect(outcome).toEqual({
      first: false,
      whileIdle: true,
      heartbeatFresh: true,
      withQueuedBlock: false,
      reset: true,
      afterOtherSkip: false,
    });
  });
});

describe("status logs", () => {
  it("log at info once per state change and at debug while the state repeats", async () => {
    const logs: string[] = [];
    const logger = Logger.make(({ logLevel, message }) => {
      logs.push(`${logLevel.label}:${String(message)}`);
    });
    await runWithGlobals(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* logOnStateChange(globals, "k", "idle", "idle");
        yield* logOnStateChange(globals, "k", "idle", "idle");
        yield* logOnStateChange(globals, "k", "busy", "busy");
        yield* logOnStateChange(globals, "k", "idle", "idle");
      }).pipe(
        Effect.provide(Logger.replace(Logger.defaultLogger, logger)),
        Logger.withMinimumLogLevel(LogLevel.All),
      ),
    );
    expect(logs).toEqual(["INFO:idle", "DEBUG:idle", "INFO:busy", "INFO:idle"]);
  });
});

describe("speculative wake loops", () => {
  it("refresh their heartbeat while no wake arrives", async () => {
    const refreshed = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const fiber = yield* Effect.fork(
          speculativeCommitSubmitterFiber as Effect.Effect<
            void,
            never,
            Globals
          >,
        );
        yield* TestClock.adjust("1 second");
        yield* Ref.set(globals.HEARTBEAT_SPECULATIVE_COMMIT_SUBMITTER, 0);
        yield* TestClock.adjust("31 seconds");
        const heartbeat = yield* Ref.get(
          globals.HEARTBEAT_SPECULATIVE_COMMIT_SUBMITTER,
        );
        yield* Fiber.interrupt(fiber);
        return heartbeat > 0;
      }).pipe(
        Effect.provide(Globals.Default),
        Effect.provide(TestContext.TestContext),
      ),
    );
    expect(refreshed).toBe(true);
  });

  it("refresh the builder heartbeat while no build wake arrives", async () => {
    // No active pending finalization, so the builder goes straight to its
    // wake loop; the config is read only on the pending path.
    const noRowsSql = Object.assign(
      (() => Effect.succeed([])) as unknown as SqlClient.SqlClient,
      { in: (values: readonly unknown[]) => values },
    ) as unknown as SqlClient.SqlClient;
    const refreshed = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const fiber = yield* Effect.fork(
          speculativeCommitBuilderFiber as Effect.Effect<
            void,
            never,
            Globals | SqlClient.SqlClient | NodeConfig
          >,
        );
        yield* TestClock.adjust("1 second");
        yield* Ref.set(globals.HEARTBEAT_SPECULATIVE_COMMIT_BUILDER, 0);
        yield* TestClock.adjust("31 seconds");
        const heartbeat = yield* Ref.get(
          globals.HEARTBEAT_SPECULATIVE_COMMIT_BUILDER,
        );
        yield* Fiber.interrupt(fiber);
        return heartbeat > 0;
      }).pipe(
        Effect.provideService(SqlClient.SqlClient, noRowsSql),
        Effect.provideService(NodeConfig, {} as never),
        Effect.provide(Globals.Default),
        Effect.provide(TestContext.TestContext),
      ),
    );
    expect(refreshed).toBe(true);
  });
});
