import { Cause, Effect, Logger, LogLevel, Ref, Schedule } from "effect";
import { describe, expect, it } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import { DatabaseError } from "../src/database/utils/common.js";
import {
  classifyPlutusEvaluationFailure,
  repeatScheduledWithCauseLogging,
} from "../src/fibers/tx-queue-processor.js";
import { HistoryRecoverySuperseded } from "../src/services/event-history-recovery.js";

/** The exact refusal `runHistoryProducer` returns while the gate is closed. */
const gateClosed = () =>
  new DatabaseError({
    table: Authority.tableName,
    message: "Current authenticated history producer is required",
    cause: new HistoryRecoverySuperseded({
      message: "History source gate is closed",
    }),
  });

/** Runs the loop over scripted iterations and records every log line. */
const runLoggedIterations = async (
  iterations: readonly (() => Effect.Effect<void, unknown>)[],
) => {
  const lines: { level: string; message: string; hasCause: boolean }[] = [];
  const logger = Logger.make(({ logLevel, message, cause }) => {
    lines.push({
      level: logLevel.label,
      message: (Array.isArray(message) ? message : [message])
        .map((part) => (typeof part === "string" ? part : String(part)))
        .join(" "),
      hasCause: !Cause.isEmpty(cause),
    });
  });
  await Effect.runPromise(
    Effect.gen(function* () {
      const index = yield* Ref.make(0);
      yield* repeatScheduledWithCauseLogging(
        Effect.gen(function* () {
          const next = yield* Ref.getAndUpdate(index, (n) => n + 1);
          yield* iterations[next]!();
        }),
        Schedule.recurs(iterations.length - 1),
      );
    }).pipe(
      Logger.withMinimumLogLevel(LogLevel.All),
      Effect.provide(Logger.replace(Logger.defaultLogger, logger)),
    ),
  );
  return lines;
};

describe("tx queue processor plutus evaluation failure classification", () => {
  it("treats infrastructure/network failures as retryable", () => {
    expect(
      classifyPlutusEvaluationFailure(new Error("fetch failed")),
    ).toBeNull();
    expect(
      classifyPlutusEvaluationFailure(
        new Error("Configured Lucid provider does not support evaluateTx"),
      ),
    ).toBeNull();
    expect(
      classifyPlutusEvaluationFailure(
        new Error(
          'Could not evaluate the transaction: {"status_code":500,"message":"backend unavailable"}',
        ),
      ),
    ).toBeNull();
  });

  it("recognizes strong positive evidence of script failure", () => {
    const detail = classifyPlutusEvaluationFailure(
      new Error(
        "TxId: abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd ScriptHash: 00112233445566778899aabbccddeeff00112233445566778899aabb Caused by: The provided Plutus code called 'error'",
      ),
    );

    expect(detail).not.toBeNull();
    expect(detail).toContain(
      "script_hash=00112233445566778899aabbccddeeff00112233445566778899aabb",
    );
  });

  it("treats generic deterministic UPLC failures as script-invalid", () => {
    expect(
      classifyPlutusEvaluationFailure(new Error("UPLC evaluation failed")),
    ).toContain("UPLC evaluation failed");
    expect(
      classifyPlutusEvaluationFailure(
        new Error(
          'Could not evaluate the transaction: {"status_code":400,"message":"The provided Plutus code called error"}',
        ),
      ),
    ).toContain("Could not evaluate the transaction");
  });

  it("keeps the scheduled loop alive after one iteration fails", async () => {
    const attempts = await Effect.runPromise(
      Effect.gen(function* () {
        const counter = yield* Ref.make(0);
        yield* repeatScheduledWithCauseLogging(
          Effect.gen(function* () {
            const next = (yield* Ref.get(counter)) + 1;
            yield* Ref.set(counter, next);
            if (next === 1) {
              return yield* Effect.fail(new Error("transient failure"));
            }
          }),
          Schedule.recurs(1),
        );
        return yield* Ref.get(counter);
      }),
    );

    expect(attempts).toBe(2);
  });

  it("reports a closed history gate once per closure without a stack, and keeps real failures at WARN", async () => {
    const lines = await runLoggedIterations([
      () => Effect.fail(gateClosed()),
      // Concurrent drains fail together; one interrupted sibling is still a
      // closed gate.
      () =>
        Effect.failCause(
          Cause.parallel(Cause.fail(gateClosed()), Cause.interrupt(0 as never)),
        ),
      () => Effect.fail(gateClosed()),
      () => Effect.fail(new Error("transient database outage")),
      () => Effect.void,
      () => Effect.fail(gateClosed()),
      () =>
        Effect.failCause(
          Cause.parallel(
            Cause.fail(gateClosed()),
            Cause.fail(new Error("unrelated failure")),
          ),
        ),
    ]);
    const closed = lines.filter(({ message }) =>
      message.includes("history owner recovers"),
    );
    expect(closed.map(({ level, hasCause }) => [level, hasCause])).toEqual([
      ["INFO", false],
      ["DEBUG", false],
      ["DEBUG", false],
      // After a successful iteration, the next closure is reported again.
      ["INFO", false],
    ]);
    const warnings = lines.filter(({ level }) => level === "WARN");
    expect(warnings).toHaveLength(2);
    expect(warnings.every(({ hasCause }) => hasCause)).toBe(true);
  });

  it("keeps a producer refusal that is not a closed gate at WARN", async () => {
    const lines = await runLoggedIterations([
      () =>
        Effect.fail(
          new DatabaseError({
            table: Authority.tableName,
            message: "Current authenticated history producer is required",
            cause: "History producer coverage changed",
          }),
        ),
    ]);
    expect(lines.map(({ level }) => level)).toEqual(["WARN"]);
  });
});
