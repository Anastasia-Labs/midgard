import { SqlClient } from "@effect/sql";
import { Effect, Fiber, Option, Ref, TestClock, TestContext } from "effect";
import { describe, expect, it } from "vitest";

import {
  RETENTION_HISTORY_PRUNE_TIMEOUT_MS,
  withRetentionHistoryProducer,
} from "../src/fibers/retention-sweeper.js";
import {
  HistoryProducer,
  UnownedHistoryFixture,
} from "../src/services/event-history-producer.js";
import { HistoryRecoverySuperseded } from "../src/services/event-history-recovery.js";
import { Globals } from "../src/services/index.js";

type ProducerBody = (
  token: unknown,
  assertCurrent: Effect.Effect<void>,
  coverage: unknown,
) => Effect.Effect<unknown, unknown, unknown>;

/** A history owner whose producer registration behaves as `register`. */
const owner = (
  register: (body: ProducerBody) => Effect.Effect<unknown, unknown, unknown>,
) => ({ runProducer: register }) as never;

/** The prune's work: records whether it ran holding a producer permit. */
const work = (ran: { permit?: boolean }) =>
  Effect.gen(function* () {
    ran.permit = Option.isSome(yield* Effect.serviceOption(HistoryProducer));
    return 3;
  });

const runWith = <A, E>(
  effect: Effect.Effect<A, E, Globals | SqlClient.SqlClient>,
  setup: { readonly owner?: never; readonly fixture?: boolean },
) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const globals = yield* Globals;
      if (setup.owner !== undefined)
        yield* Ref.set(globals.EVENT_HISTORY_OWNER, setup.owner);
      const provided = effect.pipe(
        Effect.provideService(SqlClient.SqlClient, {} as never),
      );
      return yield* setup.fixture === true
        ? provided.pipe(Effect.provideService(UnownedHistoryFixture, true))
        : provided;
    }).pipe(
      Effect.provide(Globals.Default),
      Effect.provide(TestContext.TestContext),
    ),
  );

describe("retention journal prune under the history producer permit (W5-F)", () => {
  it("runs the prune holding the permit the owner grants", async () => {
    const ran: { permit?: boolean } = {};
    const result = await runWith(withRetentionHistoryProducer(work(ran)), {
      owner: owner((body) => body("token", Effect.void, {})),
    });
    expect(result).toBe(3);
    expect(ran.permit).toBe(true);
  });

  it("skips without failing the sweep when the owner refuses for a recovery", async () => {
    const ran: { permit?: boolean } = {};
    const result = await runWith(withRetentionHistoryProducer(work(ran)), {
      owner: owner(() =>
        Effect.fail(new HistoryRecoverySuperseded({ message: "recovering" })),
      ),
    });
    expect(result).toBeUndefined();
    expect(ran.permit).toBeUndefined();
  });

  it("skips while no history owner is up, unless a database fixture runs unowned", async () => {
    const unowned: { permit?: boolean } = {};
    expect(
      await runWith(withRetentionHistoryProducer(work(unowned)), {}),
    ).toBeUndefined();
    expect(unowned.permit).toBeUndefined();
    const fixture: { permit?: boolean } = {};
    expect(
      await runWith(withRetentionHistoryProducer(work(fixture)), {
        fixture: true,
      }),
    ).toBe(3);
    expect(fixture.permit).toBe(false);
  });

  it("returns the permit after a bounded wait rather than wedge the owner", async () => {
    let released = false;
    const result = await runWith(
      Effect.gen(function* () {
        const fiber = yield* Effect.fork(
          withRetentionHistoryProducer(Effect.succeed(1)),
        );
        yield* TestClock.adjust(RETENTION_HISTORY_PRUNE_TIMEOUT_MS - 1);
        const early = yield* Fiber.poll(fiber);
        yield* TestClock.adjust(1);
        return { early, late: yield* Fiber.join(fiber) };
      }),
      {
        // Registration that never completes: a stuck batch holding the permit.
        owner: owner(() =>
          Effect.never.pipe(
            Effect.onInterrupt(() =>
              Effect.sync(() => {
                released = true;
              }),
            ),
          ),
        ),
      },
    );
    expect(Option.isNone(result.early)).toBe(true);
    expect(result.late).toBeUndefined();
    expect(released).toBe(true);
  });
});
