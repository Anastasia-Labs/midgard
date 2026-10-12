/**
 * The retention sweep's journal prune runs under a follower write permit
 * (W5-F): taken at once at the follower-change driver's applied view, or
 * refused while the driver recomputes or has applied no view; a refused or
 * stalled prune deletes nothing more and the sweep goes on.
 */
import "./utils.js";

import { SqlClient } from "@effect/sql";
import {
  Deferred,
  Effect,
  Fiber,
  Option,
  Ref,
  TestClock,
  TestContext,
} from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import {
  RETENTION_HISTORY_PRUNE_TIMEOUT_MS,
  withRetentionHistoryProducer,
} from "../src/fibers/retention-sweeper.history-producer.js";
import { NodeConfig } from "../src/services/config.js";
import { Database } from "../src/services/database.js";
import {
  FollowerWrite,
  FollowerWriteFixture,
} from "../src/services/follower-write-gate.js";
import { Globals } from "../src/services/index.js";
import { openFollowerWriteGate } from "./helpers/follower-write-gate.js";
import { resetApplicationTables } from "./utils.js";

/** The node database without the fixture capability (a runtime process). */
const onDatabase = <A, E>(
  effect: Effect.Effect<A, E, SqlClient.SqlClient | Globals>,
) =>
  Effect.runPromise(
    effect.pipe(
      Effect.provide(Globals.Default),
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );

/** The prune's work: records whether it ran holding a follower write permit. */
const work = (ran: { permit?: boolean }) =>
  Effect.gen(function* () {
    ran.permit = Option.isSome(yield* Effect.serviceOption(FollowerWrite));
    return 3;
  });

/**
 * Runs `effect` with this process's driver as `driver` leaves it: at the
 * gate's applied view ("applied"), recomputing, or with no view applied
 * ("none"); `fixture` adds the database fixture capability.
 */
const runWith = <A, E>(
  effect: Effect.Effect<A, E, Globals | SqlClient.SqlClient>,
  setup: {
    readonly driver: "applied" | "recomputing" | "none";
    readonly fixture?: boolean;
  },
) =>
  onDatabase(
    Effect.gen(function* () {
      const globals = yield* Globals;
      if (setup.driver !== "none") {
        const { epoch } = yield* openFollowerWriteGate;
        yield* Ref.update(globals.FOLLOWER_WRITE_GATE, (local) => ({
          ...local,
          epoch,
          recomputing: setup.driver === "recomputing",
        }));
      }
      const result = yield* setup.fixture === true
        ? effect.pipe(Effect.provideService(FollowerWriteFixture, true))
        : effect;
      const local = yield* Ref.get(globals.FOLLOWER_WRITE_GATE);
      return { result, producers: local.producers.size };
    }),
  );

beforeEach(async () => {
  await onDatabase(resetApplicationTables);
});

describe("retention journal prune under a follower write permit (W5-F)", () => {
  it("runs the prune holding a permit at the driver's applied view", async () => {
    const ran: { permit?: boolean } = {};
    const { result } = await runWith(withRetentionHistoryProducer(work(ran)), {
      driver: "applied",
    });
    expect(result).toBe(3);
    expect(ran.permit).toBe(true);
  });

  it("skips without failing the sweep while the driver recomputes", async () => {
    const ran: { permit?: boolean } = {};
    const { result } = await runWith(withRetentionHistoryProducer(work(ran)), {
      driver: "recomputing",
    });
    expect(result).toBeUndefined();
    expect(ran.permit).toBeUndefined();
  });

  it("skips while no driver applied a view, unless a database fixture runs", async () => {
    const unapplied: { permit?: boolean } = {};
    const refused = await runWith(
      withRetentionHistoryProducer(work(unapplied)),
      { driver: "none" },
    );
    expect(refused.result).toBeUndefined();
    expect(unapplied.permit).toBeUndefined();
    const fixture: { permit?: boolean } = {};
    const direct = await runWith(withRetentionHistoryProducer(work(fixture)), {
      driver: "none",
      fixture: true,
    });
    expect(direct.result).toBe(3);
    expect(fixture.permit).toBe(false);
  });

  it("returns the permit after a bounded wait rather than hold off the driver", async () => {
    let released = false;
    const { result, producers } = await runWith(
      Effect.gen(function* () {
        const started = yield* Deferred.make<void>();
        // A stuck prune batch holding the permit.
        const stuck = Deferred.succeed(started, undefined).pipe(
          Effect.zipRight(Effect.never),
          Effect.onInterrupt(() =>
            Effect.sync(() => {
              released = true;
            }),
          ),
        );
        const fiber = yield* Effect.fork(withRetentionHistoryProducer(stuck));
        yield* Deferred.await(started);
        yield* TestClock.adjust(RETENTION_HISTORY_PRUNE_TIMEOUT_MS - 1);
        const early = yield* Fiber.poll(fiber);
        yield* TestClock.adjust(1);
        return { early, late: yield* Fiber.join(fiber) };
      }).pipe(Effect.provide(TestContext.TestContext)),
      { driver: "applied" },
    );
    expect(Option.isNone(result.early)).toBe(true);
    expect(result.late).toBeUndefined();
    expect(released).toBe(true);
    expect(producers).toBe(0);
  });
});
