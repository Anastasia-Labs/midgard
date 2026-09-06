import { describe, expect, it } from "vitest";

import { deriveLedgerOutputProofStepPlan } from "../src/ledger-output-proof-plan.js";
import { buildForgedOperatorSuccessorValidationDisputeFixture } from "./support/submit-init-emulator-shared.js";

describe("ledger output proof successor carriage", () => {
  it.each(["resolveInputs", "scriptSources"] as const)(
    "retains and authenticates the adjacent %s control",
    async (disputedPhase) => {
      const fixture =
        await buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey: "11".repeat(32),
          now: 1_800_000_000_000,
          disputedPhase,
          ...(disputedPhase === "resolveInputs"
            ? { resolveInputsKind: "membershipStep" as const }
            : { scriptSourcesSemanticIndex: 2 }),
        });
      const argument = fixture.evidence.oneStepArgument;
      const plan = deriveLedgerOutputProofStepPlan(argument);
      expect(plan.roleIndex).toBe(0);
      expect(plan.controlCbor).not.toBe(plan.nextControlCbor);
      expect(plan.nextControlCbor.length).toBeGreaterThan(0);
      expect(() =>
        deriveLedgerOutputProofStepPlan({
          ...argument,
          ledgerOutputProofSuccessorWorkWitnessCbor: undefined,
        }),
      ).toThrow(/successor bytes/);
      expect(() =>
        deriveLedgerOutputProofStepPlan({
          ...argument,
          ledgerOutputProofSuccessorWorkWitnessCbor: Buffer.from("80", "hex"),
        }),
      ).toThrow(/successor bytes/);
    },
    60_000,
  );
});
