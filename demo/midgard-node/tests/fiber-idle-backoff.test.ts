import { Effect, Logger, LogLevel, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  MERGE_IDLE_BACKOFF_KEY,
  recordMergeTickIdleness,
  skipIdleMergeTick,
} from "../src/fibers/merge.idle-backoff.js";
import { nextIdleBackoffState } from "../src/services/globals.idle-backoff.js";
import { logOnStateChange } from "../src/services/globals.liveness-reasons.js";
import { Globals } from "../src/services/index.js";

const runWithGlobals = <A, E>(effect: Effect.Effect<A, E, Globals>) =>
  Effect.runPromise(effect.pipe(Effect.provide(Globals.Default)));

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
