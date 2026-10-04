import { describe, expect, it } from "vitest";

import {
  buildForgedOperatorSuccessorValidationDisputeFixture,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

describe("Cardano builtin failure published lifecycle", () => {
  it.each([12, 21, 52, 82, 83] as const)(
    "proves genuine builtin %s failure and removes the forged block",
    async (cekBuiltinFailureTag) => {
      const result = await runForcedValidationDisputeScenario(
        ({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase: "cek",
            plutusSelection: true,
            cekBuiltinFailureTag,
            cekCoreArm: "executeBuiltinFailure",
          }),
      );
      expect(result.awardResult?.txHash).toHaveLength(64);
      expect(result.removal?.transactions.length).toBeGreaterThan(0);
    },
    900_000,
  );
  it.each(["cpu", "memory"] as const)(
    "refuses a forged %s charge against an honest failed execution",
    async (cekBuiltinFailureBudgetForgery) => {
      await expect(
        runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase: "cek",
            plutusSelection: true,
            cekBuiltinFailureTag: 21,
            cekCoreArm: "executeBuiltinFailure",
            dishonestChallenger: true,
            cekBuiltinFailureBudgetForgery,
          }),
        ),
      ).rejects.toThrow(/CEK core failureBudget transaction failed/);
    },
    900_000,
  );
});
