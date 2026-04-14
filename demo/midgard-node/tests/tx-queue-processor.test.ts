import { describe, expect, it } from "vitest";
import { Effect, Ref, Schedule } from "effect";
import {
  classifyPlutusEvaluationFailure,
  repeatScheduledWithCauseLogging,
} from "@/fibers/tx-queue-processor.js";

describe("tx queue processor plutus evaluation failure classification", () => {
  it("treats infrastructure/network failures as retryable", () => {
    expect(classifyPlutusEvaluationFailure(new Error("fetch failed"))).toBeNull();
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
});
