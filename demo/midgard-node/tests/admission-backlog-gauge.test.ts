import { SqlClient } from "@effect/sql";
import {
  Cause,
  Deferred,
  Duration,
  Effect,
  Exit,
  Fiber,
  MutableRef,
  Schedule,
} from "effect";
import { describe, expect, it } from "vitest";

import {
  admissionBacklogGaugeFiber,
  admissionFailureDefinitelyDidNotInsert,
  beginAdmissionBacklogRefresh,
  commitAdmissionBacklogSlot,
  completeAdmissionBacklogRefresh,
  noteLocalAdmit,
  readAdmissionBacklogGauge,
  readAdmissionBacklogSize,
  releaseAdmissionBacklogSlot,
  reserveAdmissionBacklogSlot,
} from "../src/fibers/admission-backlog-gauge.js";
import {
  AdmissionWriterFullError,
  AdmissionWriterShutdownError,
} from "../src/services/admission-writer.js";
import {
  type AdmissionBacklogGaugeState,
  Globals,
} from "../src/services/globals.js";

describe("admission backlog gauge", () => {
  it("preserves the reported value during refresh and retains interleaved admits", () => {
    const before: AdmissionBacklogGaugeState = {
      ADMISSION_BACKLOG_BASE: 8n,
      ADMISSION_BACKLOG_LOCAL_DELTA: 2n,
      ADMISSION_BACKLOG_IN_FLIGHT: 0n,
      ADMISSION_BACKLOG_BYTES_BASE: 0n,
      ADMISSION_BACKLOG_BYTES_LOCAL_DELTA: 0n,
      ADMISSION_BACKLOG_BYTES_IN_FLIGHT: 0n,
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
      { rows: 10n, bytes: 0n },
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
        MutableRef.set(globals.ADMISSION_BACKLOG_GAUGE, {
          ADMISSION_BACKLOG_BASE: 5n,
          ADMISSION_BACKLOG_LOCAL_DELTA: 0n,
          ADMISSION_BACKLOG_IN_FLIGHT: 0n,
          ADMISSION_BACKLOG_BYTES_BASE: 0n,
          ADMISSION_BACKLOG_BYTES_LOCAL_DELTA: 0n,
          ADMISSION_BACKLOG_BYTES_IN_FLIGHT: 0n,
          ADMISSION_BACKLOG_REFRESHED_AT: Date.now(),
        });
        yield* noteLocalAdmit(100);
        yield* noteLocalAdmit(100);
        return yield* readAdmissionBacklogGauge;
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(value).toBe(7n);
  });

  it("atomically bounds concurrent reservations and accounts for commit/release", async () => {
    const result = await Effect.runPromise(
      Effect.gen(function* () {
        const reservations = yield* Effect.all(
          Array.from({ length: 12 }, () =>
            reserveAdmissionBacklogSlot({
              maxRows: 5,
              maxBytes: 1_000_000,
              bytes: 100,
            }),
          ),
          { concurrency: "unbounded" },
        );
        const reserved = reservations.filter((entry) => entry.reserved);
        yield* Effect.forEach(reserved.slice(0, 3), commitAdmissionBacklogSlot);
        yield* Effect.forEach(reserved.slice(3), releaseAdmissionBacklogSlot);
        return {
          reserved: reserved.length,
          value: yield* readAdmissionBacklogGauge,
        };
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(result).toEqual({ reserved: 5, value: 3n });
  });

  it("refuses past the byte bound, names the bound that refused, and frees released bytes", async () => {
    const result = await Effect.runPromise(
      Effect.gen(function* () {
        const limits = { maxRows: 100, maxBytes: 1_000 };
        const first = yield* reserveAdmissionBacklogSlot({
          ...limits,
          bytes: 600,
        });
        const overBytes = yield* reserveAdmissionBacklogSlot({
          ...limits,
          bytes: 401,
        });
        const exactFit = yield* reserveAdmissionBacklogSlot({
          ...limits,
          bytes: 400,
        });
        if (!first.reserved || !exactFit.reserved)
          throw new Error("expected both fits to reserve");
        yield* releaseAdmissionBacklogSlot(exactFit);
        yield* commitAdmissionBacklogSlot(first);
        const afterRelease = yield* reserveAdmissionBacklogSlot({
          ...limits,
          bytes: 400,
        });
        const overRows = yield* reserveAdmissionBacklogSlot({
          maxRows: 2,
          maxBytes: 1_000_000,
          bytes: 1,
        });
        return {
          overBytes,
          afterRelease: afterRelease.reserved,
          overRows,
          size: yield* readAdmissionBacklogSize,
        };
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(result).toEqual({
      overBytes: {
        reserved: false,
        unit: "bytes",
        backlog: 600n,
        maxBacklog: 1_000n,
      },
      afterRelease: true,
      overRows: {
        reserved: false,
        unit: "rows",
        backlog: 2n,
        maxBacklog: 2n,
      },
      size: { rows: 2n, bytes: 1_000n },
    });
  });

  it("does not erase an in-flight reservation across a live-count refresh", () => {
    const before: AdmissionBacklogGaugeState = {
      ADMISSION_BACKLOG_BASE: 8n,
      ADMISSION_BACKLOG_LOCAL_DELTA: 2n,
      ADMISSION_BACKLOG_IN_FLIGHT: 1n,
      ADMISSION_BACKLOG_BYTES_BASE: 0n,
      ADMISSION_BACKLOG_BYTES_LOCAL_DELTA: 0n,
      ADMISSION_BACKLOG_BYTES_IN_FLIGHT: 0n,
      ADMISSION_BACKLOG_REFRESHED_AT: 100,
    };
    const refreshing = beginAdmissionBacklogRefresh(before);
    const completed = completeAdmissionBacklogRefresh(
      refreshing,
      { rows: 10n, bytes: 0n },
      200,
    );

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
        Effect.provideService(SqlClient.SqlClient, {} as SqlClient.SqlClient),
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
          MutableRef.set(globals.ADMISSION_BACKLOG_GAUGE, {
            ADMISSION_BACKLOG_BASE: initialBase,
            ADMISSION_BACKLOG_LOCAL_DELTA: 0n,
            ADMISSION_BACKLOG_IN_FLIGHT: 0n,
            ADMISSION_BACKLOG_BYTES_BASE: 10n * initialBase,
            ADMISSION_BACKLOG_BYTES_LOCAL_DELTA: 0n,
            ADMISSION_BACKLOG_BYTES_IN_FLIGHT: 0n,
            ADMISSION_BACKLOG_REFRESHED_AT: 100,
          });

          for (const step of steps) {
            switch (step) {
              case "reserve": {
                const reservation = yield* reserveAdmissionBacklogSlot({
                  maxRows: 100,
                  maxBytes: 1_000_000,
                  bytes: 10,
                });
                expect(reservation.reserved).toBe(true);
                inFlight += 1n;
                break;
              }
              case "commit":
                yield* commitAdmissionBacklogSlot({ bytes: 10n });
                inFlight -= 1n;
                committedLocal += 1n;
                break;
              case "release":
                yield* releaseAdmissionBacklogSlot({ bytes: 10n });
                inFlight -= 1n;
                break;
              case "local-admit":
                yield* noteLocalAdmit(10);
                committedLocal += 1n;
                break;
              case "begin-refresh":
                committedAtRefreshStart = committedLocal;
                MutableRef.update(
                  globals.ADMISSION_BACKLOG_GAUGE,
                  beginAdmissionBacklogRefresh,
                );
                break;
              case "complete-refresh":
                MutableRef.update(globals.ADMISSION_BACKLOG_GAUGE, (current) =>
                  completeAdmissionBacklogRefresh(
                    current,
                    {
                      rows: initialBase + committedAtRefreshStart,
                      bytes: 10n * (initialBase + committedAtRefreshStart),
                    },
                    200,
                  ),
                );
                break;
            }

            expect(yield* readAdmissionBacklogSize, steps.join(" -> ")).toEqual(
              {
                rows: initialBase + committedLocal + inFlight,
                bytes: 10n * (initialBase + committedLocal + inFlight),
              },
            );
          }
        }).pipe(Effect.provide(Globals.Default)),
      );
    }
  });

  it("releases known-unstarted shutdown reservations and retains durability-unknown ones", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const knownUnstarted = new AdmissionWriterShutdownError({
          message: "queued during shutdown",
          mayHaveCommitted: false,
        });
        const durabilityUnknown = new AdmissionWriterShutdownError({
          message: "in flight during shutdown",
          mayHaveCommitted: true,
        });
        expect(admissionFailureDefinitelyDidNotInsert(knownUnstarted)).toBe(
          true,
        );
        expect(admissionFailureDefinitelyDidNotInsert(durabilityUnknown)).toBe(
          false,
        );
        expect(
          admissionFailureDefinitelyDidNotInsert(
            new AdmissionWriterFullError({
              message: "writer full",
              unit: "rows",
              used: 1,
              capacity: 1,
            }),
          ),
        ).toBe(true);

        for (const error of [knownUnstarted, durabilityUnknown]) {
          const limits = { maxRows: 10, maxBytes: 1_000, bytes: 10 };
          expect((yield* reserveAdmissionBacklogSlot(limits)).reserved).toBe(
            true,
          );
          yield* Effect.fail(error).pipe(
            Effect.onExit((exit) => {
              const failure = Exit.isFailure(exit)
                ? Cause.failureOption(exit.cause)
                : undefined;
              const value =
                failure !== undefined && failure._tag === "Some"
                  ? failure.value
                  : undefined;
              return admissionFailureDefinitelyDidNotInsert(value)
                ? releaseAdmissionBacklogSlot({ bytes: 10n })
                : commitAdmissionBacklogSlot({ bytes: 10n });
            }),
            Effect.either,
          );
        }
        expect(MutableRef.get(globals.ADMISSION_BACKLOG_GAUGE)).toMatchObject({
          ADMISSION_BACKLOG_LOCAL_DELTA: 1n,
          ADMISSION_BACKLOG_IN_FLIGHT: 0n,
          ADMISSION_BACKLOG_BYTES_LOCAL_DELTA: 10n,
          ADMISSION_BACKLOG_BYTES_IN_FLIGHT: 0n,
        });
      }).pipe(Effect.provide(Globals.Default)),
    );
  });
});
