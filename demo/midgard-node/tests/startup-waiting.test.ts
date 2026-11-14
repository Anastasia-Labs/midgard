/**
 * Startup steps that wait out a transient failure under a named reason and
 * a bounded budget (`retryStartupStep`, `startup-waiting.ts`):
 *
 * - while a step's transient failure lasts, the startup keeps running and
 *   `/readyz` names the step's reason; once the step succeeds it is gone;
 * - a transient failure that outlives the budget fails the step with a
 *   `StartupStepFailedError` naming the step, the reason and the last cause;
 * - a failure that is not transient fails the step at once, with no retry.
 *
 * Each polarity runs for the shared primitive, the admission backlog's
 * first read (`refreshAdmissionBacklogGaugeOnStartup`) and a Lucid client's
 * construction (`constructLucidOnStartup`). Time budgets run on the test
 * clock.
 */
import { L1ProviderTransientError } from "@al-ft/midgard-l1-follower/provider";
import { HttpServer } from "@effect/platform";
import { it as effectIt } from "@effect/vitest";
import type * as LE from "@lucid-evolution/lucid";
import { Cause, Effect, Exit, Fiber, Logger, Option, TestClock } from "effect";
import { describe, expect, it } from "vitest";

import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";
import { DatabaseError } from "../src/database/utils/common.js";
import { refreshAdmissionBacklogGaugeOnStartup } from "../src/fibers/admission-backlog-gauge.js";
import { ConfigError } from "../src/services/config.js";
import { constructLucidOnStartup } from "../src/services/lucid.js";
import {
  ADMISSION_BACKLOG_UNREAD,
  findStartupStepFailure,
  LUCID_INITIALIZATION_PENDING,
  retryStartupStep,
  StartupStepFailedError,
  StartupWaitingReporter,
} from "../src/services/startup-waiting.js";

const baseUrl = HttpServer.addressWith((address) =>
  address._tag === "TcpAddress"
    ? Effect.succeed(`http://127.0.0.1:${address.port}`)
    : Effect.die("Expected TCP server"),
);

const readyReasons = (url: string) =>
  Effect.promise(async () => {
    const response = await fetch(`${url}/readyz`);
    expect(response.status).toBe(503);
    return ((await response.json()) as { reasons: string[] }).reasons;
  });

/** Polls `/readyz` every 25 ms until `done` holds of its reasons, for up to
 * 20 s; returns those reasons. */
const untilReasons = (url: string, done: (reasons: string[]) => boolean) =>
  Effect.gen(function* () {
    const deadline = Date.now() + 20_000;
    for (;;) {
      const reasons = yield* readyReasons(url);
      if (done(reasons)) return reasons;
      if (Date.now() > deadline)
        throw new Error(`/readyz reasons stayed ${JSON.stringify(reasons)}`);
      yield* Effect.sleep("25 millis");
    }
  });

/** PostgreSQL refusing the connection: a connection-class failure. */
const connectionRefused = () =>
  new DatabaseError({
    message: "Failed to count the admission backlog",
    cause: Object.assign(new Error("connect ECONNREFUSED 127.0.0.1:5432"), {
      code: "ECONNREFUSED",
    }),
    table: "tx_admissions",
  });

/** A statement the database refuses: no wait clears it. */
const undefinedTable = () =>
  new DatabaseError({
    message: "Failed to count the admission backlog",
    cause: Object.assign(new Error('relation "tx_admissions" does not exist'), {
      code: "42P01",
    }),
    table: "tx_admissions",
  });

const nodeUnreachable = () =>
  new L1ProviderTransientError("transport", "node_unreachable");

const recorder = () => {
  const reported: [string, readonly string[]][] = [];
  return {
    reported,
    report: <A, E, R>(effect: Effect.Effect<A, E, R>) =>
      effect.pipe(
        Effect.locally(StartupWaitingReporter, (key, reasons) =>
          Effect.sync(() => {
            reported.push([key, reasons]);
          }),
        ),
      ),
  };
};

/** The `StartupStepFailedError` an exit failed with, directly or under a
 * wrapping error's `cause`. */
const stepFailureOf = (exit: Exit.Exit<unknown, unknown>) =>
  Exit.isFailure(exit) ? findStartupStepFailure(exit.cause) : undefined;

describe("startup steps waiting under a named reason", () => {
  it("name each waiting step's reason in /readyz while the startup keeps running, and drop it once the step succeeds", async () => {
    const gates = { backlog: false, lucid: false };
    const attempts = { backlog: 0, lucid: 0 };
    const lucid = {} as LE.LucidEvolution;
    await Effect.runPromise(
      withStartupHttpServer(0, (startup) =>
        Effect.gen(function* () {
          const url = yield* baseUrl;
          yield* startup.setStage("runtime_services");
          const backlog = yield* Effect.fork(
            refreshAdmissionBacklogGaugeOnStartup(
              Effect.suspend(() => {
                attempts.backlog += 1;
                return gates.backlog
                  ? Effect.void
                  : Effect.fail(connectionRefused());
              }),
            ),
          );
          const constructed = yield* Effect.fork(
            constructLucidOnStartup(
              "lucid_initialization",
              "An error occurred on lucid initialization",
              "Preprod",
              () => {
                attempts.lucid += 1;
                return gates.lucid
                  ? Promise.resolve(lucid)
                  : Promise.reject(nodeUnreachable());
              },
            ),
          );

          const both = yield* untilReasons(
            url,
            (reasons) =>
              reasons.includes(ADMISSION_BACKLOG_UNREAD) &&
              reasons.includes(LUCID_INITIALIZATION_PENDING),
          );
          expect(both[0]).toBe("startup_incomplete");

          gates.backlog = true;
          yield* Fiber.join(backlog);
          expect(yield* readyReasons(url)).toEqual([
            "startup_incomplete",
            LUCID_INITIALIZATION_PENDING,
          ]);
          // A stage change keeps the steps' waits.
          yield* startup.setStage("database_initialization", ["named_wait"]);
          expect(yield* readyReasons(url)).toEqual([
            "startup_incomplete",
            "named_wait",
            LUCID_INITIALIZATION_PENDING,
          ]);

          gates.lucid = true;
          expect(yield* Fiber.join(constructed)).toBe(lucid);
          expect(yield* readyReasons(url)).toEqual([
            "startup_incomplete",
            "named_wait",
          ]);
          expect(attempts.backlog).toBeGreaterThan(1);
          expect(attempts.lucid).toBeGreaterThan(1);
        }),
      ),
    );
  }, 60_000);

  it("names the failed step and its reason in the startup failure log, never the cause, and holds", async () => {
    const messages: unknown[] = [];
    const logger = Logger.make(({ message }) => {
      messages.push(message);
    });
    const running = Effect.runFork(
      withStartupHttpServer(0, (startup) =>
        Effect.gen(function* () {
          yield* startup.setStage("runtime_services");
          // As the Lucid service fails: a `ConfigError` over the step.
          return yield* constructLucidOnStartup(
            "lucid_initialization",
            "An error occurred on lucid initialization",
            "Preprod",
            () => Promise.reject(new Error("private-provider-cause")),
          );
        }),
      ).pipe(Effect.provide(Logger.replace(Logger.defaultLogger, logger))),
    );
    // Not a failure the step waits out: the node holds, unready, and does
    // not exit.
    await expect
      .poll(() => messages.flat())
      .toContain(
        `node_startup_failed stage=runtime_services step=lucid_initialization reason=${LUCID_INITIALIZATION_PENDING}; the node stays up, unready, until it is restarted`,
      );
    expect(running.unsafePoll()).toBeNull();
    expect(JSON.stringify(messages)).not.toContain("private-provider-cause");
    await Effect.runPromise(Fiber.interrupt(running));
  });
});

describe("retryStartupStep", () => {
  /** A step that fails with `failures` in turn, then succeeds. */
  const scripted = (failures: readonly string[]) => {
    let attempt = 0;
    return {
      attempts: () => attempt,
      step: Effect.suspend(() => {
        const failure = failures[attempt];
        attempt += 1;
        return failure === undefined
          ? Effect.succeed("done")
          : Effect.fail(failure);
      }),
    };
  };
  const options = {
    key: "step",
    reason: (error: string) => `waiting_${error}`,
    retryable: (error: string) => error === "transient",
    initialMs: 0,
  } as const;

  it("waits out a transient failure within its budget, then reports no reason", async () => {
    const { reported, report } = recorder();
    const step = scripted(["transient", "transient"]);
    const result = await Effect.runPromise(
      report(
        retryStartupStep(step.step, { ...options, budget: { maxAttempts: 3 } }),
      ),
    );
    expect(result).toBe("done");
    expect(step.attempts()).toBe(3);
    expect(reported).toEqual([
      ["step", ["waiting_transient"]],
      ["step", ["waiting_transient"]],
      ["step", []],
    ]);
  });

  it("fails with the named terminal error once a transient failure outlives the attempt budget", async () => {
    const { reported, report } = recorder();
    const step = scripted(Array<string>(10).fill("transient"));
    const exit = await Effect.runPromiseExit(
      report(
        retryStartupStep(step.step, { ...options, budget: { maxAttempts: 3 } }),
      ),
    );
    expect(step.attempts()).toBe(3);
    const failure = stepFailureOf(exit);
    expect(failure).toBeInstanceOf(StartupStepFailedError);
    expect(failure).toMatchObject({
      step: "step",
      reason: "waiting_transient",
      exhausted: true,
      attempts: 3,
      cause: "transient",
    });
    expect(failure?.message).toMatch(
      /startup step step failed: reason=waiting_transient; a transient failure outlived/u,
    );
    expect(reported.at(-1)).toEqual(["step", []]);
  });

  effectIt.effect(
    "fails with the named terminal error once a transient failure outlives the time budget",
    () =>
      Effect.gen(function* () {
        const step = scripted(Array<string>(1_000).fill("transient"));
        const fiber = yield* Effect.fork(
          retryStartupStep(step.step, {
            ...options,
            initialMs: 1_000,
            maxMs: 30_000,
            budget: { maxElapsed: "15 minutes" },
          }).pipe(Effect.exit),
        );
        // Short of the budget the step still waits.
        yield* TestClock.adjust("14 minutes");
        expect(Option.isNone(yield* Fiber.poll(fiber))).toBe(true);
        yield* TestClock.adjust("2 minutes");
        const exit = yield* Fiber.join(fiber);
        expect(stepFailureOf(exit)).toMatchObject({
          step: "step",
          reason: "waiting_transient",
          exhausted: true,
        });
        // 1 + 2 + 4 + 8 + 16 s, then every 30 s, up to 15 minutes.
        expect(step.attempts()).toBeGreaterThan(5);
        expect(step.attempts()).toBeLessThan(40);
      }),
  );

  it("fails at once on a failure it does not wait out, and drops its reason", async () => {
    const { reported, report } = recorder();
    const step = scripted(["transient", "terminal", "terminal"]);
    const exit = await Effect.runPromiseExit(
      report(
        retryStartupStep(step.step, {
          ...options,
          budget: { maxAttempts: 100 },
        }),
      ),
    );
    expect(step.attempts()).toBe(2);
    expect(stepFailureOf(exit)).toMatchObject({
      step: "step",
      reason: "waiting_terminal",
      exhausted: false,
      cause: "terminal",
    });
    expect(reported).toEqual([
      ["step", ["waiting_transient"]],
      ["step", []],
    ]);
  });
});

describe("the admission backlog's first read at startup", () => {
  /** A backlog read that fails with `failure` `failures` times, then
   * succeeds. */
  const backlogRead = (failures: number, failure: () => DatabaseError) => {
    let attempts = 0;
    return {
      attempts: () => attempts,
      read: Effect.suspend(() => {
        attempts += 1;
        return attempts <= failures ? Effect.fail(failure()) : Effect.void;
      }),
    };
  };

  effectIt.effect(
    "waits out a dropped connection within the database budget",
    () =>
      Effect.gen(function* () {
        const backlog = backlogRead(3, connectionRefused);
        const fiber = yield* Effect.fork(
          refreshAdmissionBacklogGaugeOnStartup(backlog.read),
        );
        yield* TestClock.adjust("1 minute");
        yield* Fiber.join(fiber);
        expect(backlog.attempts()).toBe(4);
      }),
  );

  effectIt.effect(
    "fails the startup under admission_backlog_unread once the database budget runs out",
    () =>
      Effect.gen(function* () {
        const backlog = backlogRead(Number.MAX_SAFE_INTEGER, connectionRefused);
        const fiber = yield* Effect.fork(
          refreshAdmissionBacklogGaugeOnStartup(backlog.read).pipe(Effect.exit),
        );
        yield* TestClock.adjust("14 minutes");
        expect(Option.isNone(yield* Fiber.poll(fiber))).toBe(true);
        yield* TestClock.adjust("2 minutes");
        const exit = yield* Fiber.join(fiber);
        expect(stepFailureOf(exit)).toMatchObject({
          step: "admission_backlog_refresh",
          reason: ADMISSION_BACKLOG_UNREAD,
          exhausted: true,
        });
        expect(backlog.attempts()).toBeGreaterThan(5);
      }),
  );

  it("fails the startup at once on a failure that is not a connection failure", async () => {
    const backlog = backlogRead(Number.MAX_SAFE_INTEGER, undefinedTable);
    const exit = await Effect.runPromiseExit(
      refreshAdmissionBacklogGaugeOnStartup(backlog.read),
    );
    expect(backlog.attempts()).toBe(1);
    expect(stepFailureOf(exit)).toMatchObject({
      step: "admission_backlog_refresh",
      reason: ADMISSION_BACKLOG_UNREAD,
      exhausted: false,
    });
  });
});

describe("a Lucid client's construction at startup", () => {
  const construct = (failures: number, failure: () => Error) => {
    let attempts = 0;
    const lucid = {} as LE.LucidEvolution;
    return {
      lucid,
      attempts: () => attempts,
      run: () => {
        attempts += 1;
        return attempts <= failures
          ? Promise.reject(failure())
          : Promise.resolve(lucid);
      },
    };
  };

  it("waits out an unreachable node within its budget", async () => {
    const client = construct(1, nodeUnreachable);
    const result = await Effect.runPromise(
      constructLucidOnStartup("lucid", "init failed", "Preprod", client.run, {
        maxAttempts: 2,
      }),
    );
    expect(result).toBe(client.lucid);
    expect(client.attempts()).toBe(2);
  }, 30_000);

  it("fails as a ConfigError over the named step once the budget runs out", async () => {
    const client = construct(10, nodeUnreachable);
    const exit = await Effect.runPromiseExit(
      constructLucidOnStartup("lucid", "init failed", "Preprod", client.run, {
        maxAttempts: 2,
      }),
    );
    expect(client.attempts()).toBe(2);
    expect(
      Exit.isFailure(exit) &&
        Option.getOrUndefined(Cause.failureOption(exit.cause)),
    ).toBeInstanceOf(ConfigError);
    expect(stepFailureOf(exit)).toMatchObject({
      step: "lucid",
      reason: LUCID_INITIALIZATION_PENDING,
      exhausted: true,
    });
  }, 30_000);

  it("fails at once on a failure that is not a provider outage", async () => {
    const client = construct(10, () => new Error("unsupported network"));
    const exit = await Effect.runPromiseExit(
      constructLucidOnStartup("lucid", "init failed", "Preprod", client.run, {
        maxAttempts: 100,
      }),
    );
    expect(client.attempts()).toBe(1);
    expect(stepFailureOf(exit)).toMatchObject({
      step: "lucid",
      exhausted: false,
    });
  });
});
