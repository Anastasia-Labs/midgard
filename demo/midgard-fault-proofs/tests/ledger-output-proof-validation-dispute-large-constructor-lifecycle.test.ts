import { Data } from "@lucid-evolution/lucid";
import { afterEach, expect, it, vi } from "vitest";

import * as plans from "../src/ledger-output-proof-plan.js";
import {
  buildForgedOperatorSuccessorValidationDisputeFixture,
  expectOnchainRefusal,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

// Canonical constructor 128 with one integer field. Ordinal 14 is an active
// constructor blob hash round (role 20), and ordinal 29 opens its fields
// (role 21), in both shared LOP families. Pin each selected role before using it.
const DATUM = Buffer.from("d8668218809f01ff", "hex");
const CASES = [
  ["resolveInputs", 14, 20],
  ["scriptSources", 14, 20],
  ["resolveInputs", 29, 21],
  ["scriptSources", 29, 21],
] as const;

afterEach(() => vi.restoreAllMocks());

const scenario = (
  phase: "resolveInputs" | "scriptSources",
  ordinal: number,
  role: number,
  dishonestChallenger = false,
  expectedPlanRole = role,
) =>
  runForcedValidationDisputeScenario(async ({ operatorVkey, now }) => {
    const fixture = await buildForgedOperatorSuccessorValidationDisputeFixture({
      operatorVkey,
      now,
      disputedPhase: phase,
      ...(phase === "resolveInputs"
        ? { resolveInputsKind: "membershipStep" as const }
        : { scriptSourcesSemanticIndex: 2 }),
      disputedMatchOrdinal: ordinal,
      outputDatumCbor: DATUM,
      dishonestChallenger,
    });
    expect(
      plans.deriveLedgerOutputProofStepPlan(fixture.evidence.oneStepArgument)
        .roleIndex,
    ).toBe(expectedPlanRole);
    return fixture;
  });

it.each(CASES)(
  "proves %s constructor ordinal %i through role %i with checked control",
  async (phase, ordinal, role) => {
    const result = await scenario(phase, ordinal, role);
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
    expect(result.semanticMeasurement!.redeemerCount).toBeGreaterThanOrEqual(2);
    console.info(
      `${phase} role ${role}: memory ${result.semanticMeasurement!.executionMemory} cpu ${result.semanticMeasurement!.executionSteps}`,
    );
    expect(result.semanticMeasurement!.executionMemory).toBeLessThanOrEqual(
      13_200_000n,
    );
    expect(result.semanticMeasurement!.executionSteps).toBeLessThanOrEqual(
      8_000_000_000n,
    );
  },
  300_000,
);

it.each(CASES)(
  "refuses forged %s constructor ordinal %i successor at role %i",
  async (phase, ordinal, role) => {
    await expectOnchainRefusal(() => scenario(phase, ordinal, role, true));
  },
  300_000,
);

it.each(CASES)(
  "refuses %s constructor ordinal %i role %i substituted by ordinary integer role",
  async (phase, ordinal, role) => {
    const honest = plans.deriveLedgerOutputProofStepPlan;
    let mutated = false;
    vi.spyOn(plans, "deriveLedgerOutputProofStepPlan").mockImplementation(
      (input) => {
        const plan = honest(input);
        if (plan.roleIndex !== role) return plan;
        mutated = true;
        const control = Data.from(plan.controlCbor);
        if (!Array.isArray(control))
          throw new Error("LOP control must be a list");
        return {
          ...plan,
          roleIndex: 7,
          attestationRoles: ["scalarInteger"],
          ...plans.deriveLedgerOutputProofStepClaims({ control, roleIndex: 7 }),
        };
      },
    );
    await expectOnchainRefusal(() => scenario(phase, ordinal, role, false, 7));
    expect(mutated).toBe(true);
  },
  300_000,
);
