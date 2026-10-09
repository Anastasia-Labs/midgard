/**
 * Startup steps that wait under a named reason (`retryStartupStep`,
 * `startup-waiting.ts`) as the startup HTTP server reports them: while the
 * first read of the admission backlog (`refreshAdmissionBacklogGaugeOnStartup`)
 * and a Lucid client's construction (`constructLucidOnStartup`) fail, the
 * startup keeps running and `/readyz` names each step's reason; once a step
 * succeeds its reason is gone. A failure the step does not wait out fails it
 * at once, and its reason is gone too.
 */
import { HttpServer } from "@effect/platform";
import type * as LE from "@lucid-evolution/lucid";
import { Effect, Fiber } from "effect";
import { describe, expect, it } from "vitest";

import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";
import { DatabaseError } from "../src/database/utils/common.js";
import { refreshAdmissionBacklogGaugeOnStartup } from "../src/fibers/admission-backlog-gauge.js";
import { constructLucidOnStartup } from "../src/services/lucid.js";
import {
  ADMISSION_BACKLOG_UNREAD,
  LUCID_INITIALIZATION_PENDING,
  retryStartupStep,
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
                  : Effect.fail(
                      new DatabaseError({
                        message: "connection refused",
                        cause: "test",
                        table: "tx_admissions",
                      }),
                    );
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
                  : Promise.reject(new Error("protocol parameters unread"));
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

  it("fails a step at once on a failure it does not wait out, and drops its reason", async () => {
    const reported: [string, readonly string[]][] = [];
    let attempt = 0;
    const result = await Effect.runPromise(
      retryStartupStep(
        Effect.suspend(() => {
          attempt += 1;
          return Effect.fail(attempt === 1 ? "transient" : "terminal");
        }),
        {
          key: "step",
          reason: (error) => `waiting_${error}`,
          retryable: (error) => error === "transient",
          initialMs: 0,
        },
      ).pipe(
        Effect.either,
        Effect.locally(StartupWaitingReporter, (key, reasons) =>
          Effect.sync(() => {
            reported.push([key, reasons]);
          }),
        ),
      ),
    );
    expect(result).toMatchObject({ _tag: "Left", left: "terminal" });
    expect(attempt).toBe(2);
    expect(reported).toEqual([
      ["step", ["waiting_transient"]],
      ["step", []],
    ]);
  });
});
