import { expect, it } from "vitest";

import {
  buildForgedOperatorSuccessorValidationDisputeFixture,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

it.each(["initial", "finish", "membershipBegin", "nonMembership"] as const)(
  "proves resolve-inputs %s through permanent proof and removal",
  async (resolveInputsKind) => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "resolveInputs",
          resolveInputsKind,
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
  },
  180_000,
);

it.each(["initial", "finish", "membershipBegin"] as const)(
  "refuses a forged resolve-inputs %s successor against an honest trace",
  async (resolveInputsKind) => {
    await expect(
      runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "resolveInputs",
          resolveInputsKind,
          dishonestChallenger: true,
        }),
      ),
    ).rejects.toThrow(/semantic-resolution/);
  },
  180_000,
);
