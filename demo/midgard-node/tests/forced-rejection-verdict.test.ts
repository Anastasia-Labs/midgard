import { ForcedRejectionStopped } from "@al-ft/midgard-fault-proofs";
import { RejectCodes } from "@al-ft/midgard-validation";
import { Effect, Logger } from "effect";
import { describe, expect, it } from "vitest";

import { forcedRejectionVerdict } from "../src/mpf/event-window.forced-verdict-for-rejection.js";

// This boundary requires no ledger or database fixture. Its nearest forced
// transaction suite already exceeds the module size limit.
describe("forced rejection verdict", () => {
  it("writes the machine's located verdict and stops unsupported faults without a DatabaseError", async () => {
    const rejection = (
      code: (typeof RejectCodes)[keyof typeof RejectCodes],
    ) => ({
      txId: Buffer.alloc(32),
      code,
      detail: null,
      consensusPhase: "cek" as const,
    });
    expect(
      await Effect.runPromise(
        forcedRejectionVerdict({
          ...rejection(RejectCodes.PlutusScriptInvalid),
          subject: { arm: "PlutusExecutionFailed", index: 7n },
        }),
      ),
    ).toStrictEqual({
      ForcedTxInvalid: {
        reason: { PlutusExecutionFailed: { execution_index: 7n } },
      },
    });
    const alarms: unknown[] = [];
    const alarmLogger = Logger.make(({ message }) => {
      alarms.push(message);
    });
    for (const code of [
      RejectCodes.PlutusEvaluationUnavailable,
      RejectCodes.AuxDataForbidden,
      RejectCodes.InvalidOutput,
    ]) {
      const result = await Effect.runPromise(
        Effect.either(forcedRejectionVerdict(rejection(code))).pipe(
          Effect.provide(Logger.replace(Logger.defaultLogger, alarmLogger)),
        ),
      );
      expect(result._tag).toBe("Left");
      if (result._tag !== "Left") throw new Error("expected stop");
      expect(result.left).toBeInstanceOf(ForcedRejectionStopped);
      expect(result.left._tag).toBe("ForcedRejectionStopped");
      expect((result.left as ForcedRejectionStopped).retryable).toBe(
        code === RejectCodes.PlutusEvaluationUnavailable,
      );
    }
    expect(alarms).toHaveLength(3);
    expect(
      alarms.every((message) =>
        String(message).includes("Forced transaction block stopped"),
      ),
    ).toBe(true);
  });
});
