/**
 * `runHistoryProducer` relabels only what the history gate refused: a producer
 * the owner would not register or keep, and a superseded producer wherever it
 * surfaced. The work's own failures keep their type, so a commit worker's
 * submit refusal is not reported as a missing producer.
 */
import { Effect, Exit, Option, Ref } from "effect";
import { describe, expect, it } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import { DatabaseError } from "../src/database/utils/common.js";
import { isHistoryGateClosedCause } from "../src/fibers/tx-queue-processor.run-phase-afor-batch.js";
import {
  HistoryProducer,
  type HistoryProducerPermit,
  isHistoryProducerGateClosed,
  runHistoryProducer,
} from "../src/services/event-history-producer.js";
import { HistoryRecoverySuperseded } from "../src/services/event-history-recovery.js";
import { Globals } from "../src/services/globals.js";
import { TxSubmitError } from "../src/transactions/utils.js";

const PRODUCER_REQUIRED = "Current authenticated history producer is required";

const permit: HistoryProducerPermit = {
  token: {
    deploymentIdentity: "33".repeat(32),
    ownerToken: "stub-owner",
    generation: "1",
  },
  coverage: {
    bindingDigest: "44".repeat(32),
    checkpointRevision: "1",
    point: { id: "55".repeat(32), slot: 1 },
    snapshotDigest: "66".repeat(32),
    includedThroughMs: 0,
  },
};

const superseded = (message: string) =>
  new HistoryRecoverySuperseded({ message });

type Guard = Effect.Effect<void, HistoryRecoverySuperseded>;

/** The owner's `runProducer` contract: refuse up front, or run the work
 * between two currency checks of the same guard it hands the work. */
const stubOwner = ({
  refuse,
  guard = Effect.void,
}: {
  readonly refuse?: unknown;
  readonly guard?: Guard;
}) => ({
  runProducer: <A, E, R>(
    work: (
      token: HistoryProducerPermit["token"],
      assertCurrent: Guard,
      coverage: HistoryProducerPermit["coverage"],
    ) => Effect.Effect<A, E, R>,
  ) =>
    refuse !== undefined
      ? Effect.fail(refuse)
      : guard.pipe(
          Effect.zipRight(
            Effect.suspend(() => work(permit.token, guard, permit.coverage)),
          ),
          Effect.tap(() => guard),
        ),
});

const runUnder = <A, E>(
  owner: ReturnType<typeof stubOwner>,
  work: Effect.Effect<A, E, HistoryProducerPermit>,
) =>
  Effect.runPromiseExit(
    Effect.gen(function* () {
      const globals = yield* Globals;
      yield* Ref.set(globals.EVENT_HISTORY_OWNER, owner as never);
      return yield* runHistoryProducer(work);
    }).pipe(Effect.provide(Globals.Default)) as Effect.Effect<
      A,
      unknown,
      never
    >,
  );

const failureOf = (exit: Exit.Exit<unknown, unknown>): unknown => {
  if (Exit.isSuccess(exit)) throw new Error("Expected the producer to fail");
  const failure = exit.cause._tag === "Fail" ? exit.cause.error : undefined;
  if (failure === undefined)
    throw new Error(`Expected one typed failure, got ${exit.cause._tag}`);
  return failure;
};

const expectProducerRequired = (error: unknown, cause: unknown) => {
  expect(error).toBeInstanceOf(DatabaseError);
  expect((error as DatabaseError).table).toBe(Authority.tableName);
  expect((error as DatabaseError).message).toBe(PRODUCER_REQUIRED);
  expect((error as DatabaseError).cause).toBe(cause);
};

describe("runHistoryProducer error attribution", () => {
  it("runs the work under its permit and returns its value", async () => {
    const exit = await runUnder(
      stubOwner({}),
      Effect.serviceOption(HistoryProducer).pipe(Effect.map(Option.getOrNull)),
    );
    expect(exit).toEqual(Exit.succeed(permit));
  });

  it("passes the work's own failure through with its original tag", async () => {
    const submitRefusal = new TxSubmitError({
      message: "Commit block submit deferred in no-inline mode",
      cause: "provider_slot_wait",
      txHash: "77".repeat(32),
    });
    const error = failureOf(
      await runUnder(stubOwner({}), Effect.fail(submitRefusal)),
    );
    expect(error).toBe(submitRefusal);
    expect((error as TxSubmitError)._tag).toBe("TxSubmitError");
    expect(isHistoryProducerGateClosed(error)).toBe(false);
  });

  it("passes a DatabaseError the work raised through unchanged", async () => {
    const workError = new DatabaseError({
      table: "pending_block_finalizations",
      message: "row lock timed out",
      cause: "lock_timeout",
    });
    const error = failureOf(
      await runUnder(stubOwner({}), Effect.fail(workError)),
    );
    expect(error).toBe(workError);
  });

  it("maps a refused registration to PRODUCER_REQUIRED", async () => {
    const refusal = superseded("History follower is 9 blocks behind");
    const error = failureOf(
      await runUnder(stubOwner({ refuse: refusal }), Effect.succeed(1)),
    );
    expectProducerRequired(error, refusal);
    expect(isHistoryProducerGateClosed(error)).toBe(true);
  });

  it("maps a registration refused by the authority row to PRODUCER_REQUIRED", async () => {
    const refusal = new DatabaseError({
      table: Authority.tableName,
      message: "History authority is not ready",
      cause: "recovering",
    });
    const error = failureOf(
      await runUnder(stubOwner({ refuse: refusal }), Effect.succeed(1)),
    );
    expectProducerRequired(error, refusal);
    expect(isHistoryProducerGateClosed(error)).toBe(false);
  });

  it("maps a missing owner to PRODUCER_REQUIRED", async () => {
    const exit = await Effect.runPromiseExit(
      runHistoryProducer(Effect.succeed(1)).pipe(
        Effect.provide(Globals.Default),
      ) as Effect.Effect<number, unknown, never>,
    );
    const error = failureOf(exit);
    expect((error as DatabaseError).message).toBe(PRODUCER_REQUIRED);
    expect((error as DatabaseError).cause).toBe(
      "History owner is not initialized",
    );
  });

  it("keeps a supersession the work itself raised gate-closed", async () => {
    const midWork = superseded("History source gate is closed");
    const error = failureOf(
      await runUnder(stubOwner({}), Effect.fail(midWork)),
    );
    expectProducerRequired(error, midWork);
    expect(isHistoryProducerGateClosed(error)).toBe(true);
  });

  it("keeps a supersession the owner's guard detects mid-work gate-closed", async () => {
    let current = true;
    const supersededMidWork = superseded("Recovery superseded the producer");
    const guard: Guard = Effect.suspend(() =>
      current ? Effect.void : Effect.fail(supersededMidWork),
    );
    const error = failureOf(
      await runUnder(
        stubOwner({ guard }),
        Effect.sync(() => {
          current = false;
          return "built";
        }),
      ),
    );
    expectProducerRequired(error, supersededMidWork);
    expect(isHistoryProducerGateClosed(error)).toBe(true);
  });
});

describe("the tx-queue gate-closed classification over runHistoryProducer", () => {
  const causeOf = (exit: Exit.Exit<unknown, unknown>) => {
    if (Exit.isSuccess(exit)) throw new Error("Expected the producer to fail");
    return exit.cause;
  };

  it("treats a supersession raised mid-work as a closed gate", async () => {
    const exit = await runUnder(
      stubOwner({}),
      Effect.fail(superseded("History source gate is closed")),
    );
    expect(isHistoryGateClosedCause(causeOf(exit))).toBe(true);
  });

  it("treats a gate refusal withHistoryWrite already raised as a closed gate", async () => {
    const midWork = superseded("History source gate is closed");
    const refusal = new DatabaseError({
      table: Authority.tableName,
      message: PRODUCER_REQUIRED,
      cause: midWork,
    });
    const exit = await runUnder(stubOwner({}), Effect.fail(refusal));
    expect(failureOf(exit)).toBe(refusal);
    expect(isHistoryGateClosedCause(causeOf(exit))).toBe(true);
  });

  it("does not treat the work's own submit refusal as a closed gate", async () => {
    const exit = await runUnder(
      stubOwner({}),
      Effect.fail(
        new TxSubmitError({
          message: "Commit block submit refused",
          cause: "OutsideValidityInterval",
          txHash: "77".repeat(32),
        }),
      ),
    );
    expect(isHistoryGateClosedCause(causeOf(exit))).toBe(false);
  });
});
