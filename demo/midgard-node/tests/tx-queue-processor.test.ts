import type { PhaseAValidatedTx } from "@al-ft/midgard-validation";
import { Cause, Effect, Logger, LogLevel, Ref, Schedule } from "effect";
import { describe, expect, it } from "vitest";

import {
  ADMISSION_REJECT_CODE_PENDING_WITHDRAWAL_INPUT,
  classifyPlutusEvaluationFailure,
  refusePendingWithdrawalInputs,
  repeatScheduledWithCauseLogging,
} from "../src/fibers/tx-queue-processor.js";
import {
  DRIVER_RECOMPUTE_PENDING,
  followerWriteHeld,
  followerWriteUnavailable,
} from "../src/services/follower-write-gate.js";

/** The exact refusal the follower write gate returns while a recompute is pending. */
const gateClosed = () =>
  followerWriteHeld(DRIVER_RECOMPUTE_PENDING, "a landed-block rebase");

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

  it("reports a closed follower write gate once per closure without a stack, and keeps real failures at WARN", async () => {
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
      message.includes("follower-change driver recomputes"),
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
          followerWriteUnavailable("The follower write gate row is missing"),
        ),
    ]);
    expect(lines.map(({ level }) => level)).toEqual(["WARN"]);
  });
});

describe("tx queue processor pending-withdrawal admission refusal", () => {
  /** The fields the refusal reads; the rest of Phase A's output is unused. */
  const candidate = (txIdByte: string, spentOutRefHexes: readonly string[]) =>
    ({
      ledgerTx: { txId: Buffer.from(txIdByte.repeat(32), "hex") },
      graph: { spentOutRefHexes, referenceOutRefHexes: [], produced: [] },
    }) as unknown as PhaseAValidatedTx;

  it("refuses only candidates that spend an outref a pending withdrawal names", () => {
    const untouched = candidate("01", ["a1", "a2"]);
    const spendsWithdrawn = candidate("02", ["a3", "b1"]);
    expect(
      refusePendingWithdrawalInputs(
        [untouched, spendsWithdrawn],
        new Set(["b1"]),
      ),
    ).toEqual({
      accepted: [untouched],
      rejected: [
        {
          txId: Buffer.from("02".repeat(32), "hex"),
          code: ADMISSION_REJECT_CODE_PENDING_WITHDRAWAL_INPUT,
          detail:
            "Transaction spends L2 outref b1, which a pending withdrawal names",
        },
      ],
    });
  });
});
