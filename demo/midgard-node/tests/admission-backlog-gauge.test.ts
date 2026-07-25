import { SqlClient } from "@effect/sql";
import {
  Deferred,
  Duration,
  Effect,
  Exit,
  Fiber,
  Ref,
  Schedule,
} from "effect";
import { describe, expect, it } from "vitest";

import {
  admissionBacklogGaugeFiber,
  beginAdmissionBacklogRefresh,
  commitAdmissionBacklogSlot,
  completeAdmissionBacklogRefresh,
  noteLocalAdmit,
  readAdmissionBacklogGauge,
  releaseAdmissionBacklogSlot,
  reserveAdmissionBacklogSlot,
} from "@/fibers/admission-backlog-gauge.js";
import {
  type AdmissionBacklogGaugeState,
  Globals,
} from "@/services/globals.js";

describe("admission backlog gauge", () => {
  it("preserves the reported value during refresh and retains interleaved admits", () => {
    const before: AdmissionBacklogGaugeState = {
      ADMISSION_BACKLOG_BASE: 8n,
      ADMISSION_BACKLOG_LOCAL_DELTA: 2n,
      ADMISSION_BACKLOG_IN_FLIGHT: 0n,
      ADMISSION_BACKLOG_REFRESHED_AT: 100,
    };
    const refreshing = beginAdmissionBacklogRefresh(before);
    expect(
      refreshing.ADMISSION_BACKLOG_BASE +
        refreshing.ADMISSION_BACKLOG_LOCAL_DELTA,
    ).toBe(10n);

    const withInterleavedAdmit = {
      ...refreshing,
      ADMISSION_BACKLOG_LOCAL_DELTA: 1n,
    };
    const completed = completeAdmissionBacklogRefresh(
      withInterleavedAdmit,
      10n,
      200,
    );
    expect(
      completed.ADMISSION_BACKLOG_BASE +
        completed.ADMISSION_BACKLOG_LOCAL_DELTA,
    ).toBe(11n);
    expect(completed.ADMISSION_BACKLOG_REFRESHED_AT).toBe(200);
  });

  it("reports base plus every local admit", async () => {
    const value = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Ref.set(globals.ADMISSION_BACKLOG_GAUGE, {
          ADMISSION_BACKLOG_BASE: 5n,
          ADMISSION_BACKLOG_LOCAL_DELTA: 0n,
          ADMISSION_BACKLOG_IN_FLIGHT: 0n,
          ADMISSION_BACKLOG_REFRESHED_AT: Date.now(),
        });
        yield* noteLocalAdmit;
        yield* noteLocalAdmit;
        return yield* readAdmissionBacklogGauge;
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(value).toBe(7n);
  });

  it("atomically bounds concurrent reservations and accounts for commit/release", async () => {
    const result = await Effect.runPromise(
      Effect.gen(function* () {
        const reservations = yield* Effect.all(
          Array.from({ length: 12 }, () => reserveAdmissionBacklogSlot(5)),
          { concurrency: "unbounded" },
        );
        const reserved = reservations.filter((entry) => entry.reserved);
        yield* Effect.forEach(
          reserved.slice(0, 3),
          () => commitAdmissionBacklogSlot,
        );
        yield* Effect.forEach(
          reserved.slice(3),
          () => releaseAdmissionBacklogSlot,
        );
        return {
          reserved: reserved.length,
          value: yield* readAdmissionBacklogGauge,
        };
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(result).toEqual({ reserved: 5, value: 3n });
  });

  it("does not erase an in-flight reservation across a live-count refresh", () => {
    const before: AdmissionBacklogGaugeState = {
      ADMISSION_BACKLOG_BASE: 8n,
      ADMISSION_BACKLOG_LOCAL_DELTA: 2n,
      ADMISSION_BACKLOG_IN_FLIGHT: 1n,
      ADMISSION_BACKLOG_REFRESHED_AT: 100,
    };
    const refreshing = beginAdmissionBacklogRefresh(before);
    const completed = completeAdmissionBacklogRefresh(refreshing, 10n, 200);

    expect(completed.ADMISSION_BACKLOG_IN_FLIGHT).toBe(1n);
    expect(
      completed.ADMISSION_BACKLOG_BASE +
        completed.ADMISSION_BACKLOG_LOCAL_DELTA +
        completed.ADMISSION_BACKLOG_IN_FLIGHT,
    ).toBe(11n);
  });

  it("preserves cancellation while a test refresh gate is frozen", async () => {
    const exit = await Effect.runPromise(
      Effect.gen(function* () {
        const frozen = yield* Deferred.make<void>();
        const fiber = yield* Effect.fork(
          admissionBacklogGaugeFiber(
            Schedule.spaced(Duration.millis(10)),
            Deferred.await(frozen),
          ),
        );
        yield* Effect.yieldNow();
        return yield* Fiber.interrupt(fiber);
      }).pipe(
        Effect.provideService(
          SqlClient.SqlClient,
          {} as SqlClient.SqlClient,
        ),
        Effect.provide(Globals.Default),
      ),
    );

    expect(Exit.isInterrupted(exit)).toBe(true);
  });

  it("never under-counts committed or in-flight local growth across refresh schedules", async () => {
    type Step =
      | "reserve"
      | "commit"
      | "release"
      | "local-admit"
      | "begin-refresh"
      | "complete-refresh";
    const schedules: readonly (readonly Step[])[] = [
      [
        "reserve",
        "begin-refresh",
        "local-admit",
        "reserve",
        "complete-refresh",
        "commit",
        "release",
      ],
      [
        "local-admit",
        "reserve",
        "begin-refresh",
        "reserve",
        "commit",
        "complete-refresh",
        "release",
      ],
      [
        "reserve",
        "reserve",
        "begin-refresh",
        "complete-refresh",
        "release",
        "local-admit",
        "commit",
      ],
      [
        "reserve",
        "local-admit",
        "begin-refresh",
        "release",
        "reserve",
        "commit",
        "complete-refresh",
      ],
    ];

    for (const steps of schedules) {
      await Effect.runPromise(
        Effect.gen(function* () {
          const globals = yield* Globals;
          const initialBase = 2n;
          let committedLocal = 0n;
          let inFlight = 0n;
          let committedAtRefreshStart = 0n;
          yield* Ref.set(globals.ADMISSION_BACKLOG_GAUGE, {
            ADMISSION_BACKLOG_BASE: initialBase,
            ADMISSION_BACKLOG_LOCAL_DELTA: 0n,
            ADMISSION_BACKLOG_IN_FLIGHT: 0n,
            ADMISSION_BACKLOG_REFRESHED_AT: 100,
          });

          for (const step of steps) {
            switch (step) {
              case "reserve": {
                const reservation = yield* reserveAdmissionBacklogSlot(100);
                expect(reservation.reserved).toBe(true);
                inFlight += 1n;
                break;
              }
              case "commit":
                yield* commitAdmissionBacklogSlot;
                inFlight -= 1n;
                committedLocal += 1n;
                break;
              case "release":
                yield* releaseAdmissionBacklogSlot;
                inFlight -= 1n;
                break;
              case "local-admit":
                yield* noteLocalAdmit;
                committedLocal += 1n;
                break;
              case "begin-refresh":
                committedAtRefreshStart = committedLocal;
                yield* Ref.update(
                  globals.ADMISSION_BACKLOG_GAUGE,
                  beginAdmissionBacklogRefresh,
                );
                break;
              case "complete-refresh":
                yield* Ref.update(globals.ADMISSION_BACKLOG_GAUGE, (current) =>
                  completeAdmissionBacklogRefresh(
                    current,
                    initialBase + committedAtRefreshStart,
                    200,
                  ),
                );
                break;
            }

            expect(yield* readAdmissionBacklogGauge, steps.join(" -> ")).toBe(
              initialBase + committedLocal + inFlight,
            );
          }
        }).pipe(Effect.provide(Globals.Default)),
      );
    }
  });
});
