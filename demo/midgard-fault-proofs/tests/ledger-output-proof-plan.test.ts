import { Constr, type Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  deriveLedgerOutputProofStepPlan,
  ledgerOutputProofDatumRoleIndex,
} from "../src/ledger-output-proof-plan.js";
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

describe("ledger output proof datum role selection", () => {
  const controlAtTraverseStage = (stage: bigint): Data[] => {
    const control: Data[] = Array.from({ length: 12 }, () => 0n);
    control[7] = new Constr(0, [
      [1n, stage, 0n, 1n, 0n, "", new Constr(1, []), 0n, 0n, 0n],
    ]);
    return control;
  };
  const integerControl = controlAtTraverseStage(1n);
  const bytesControl = controlAtTraverseStage(2n);

  it.each([
    [7, 2],
    [8, 3],
    [1, 4],
    [2, 14],
    [3, 15],
    [4, 16],
    [6, 6],
  ])("routes action %i to its single role", (action, role) => {
    for (const control of [integerControl, bytesControl])
      expect(
        ledgerOutputProofDatumRoleIndex(control, new Constr(action, [])),
      ).toBe(role);
  });

  it.each([
    [5, 5, 17],
    [0, 7, 18],
  ])(
    "splits scalar action %i by the traversal stage",
    (action, integerRole, bytesRole) => {
      expect(
        ledgerOutputProofDatumRoleIndex(integerControl, new Constr(action, [])),
      ).toBe(integerRole);
      expect(
        ledgerOutputProofDatumRoleIndex(bytesControl, new Constr(action, [])),
      ).toBe(bytesRole);
    },
  );

  it("refuses a scalar action outside the scalar traversal stages", () => {
    for (const stage of [0n, 3n, 4n, 5n, 6n, 7n])
      expect(() =>
        ledgerOutputProofDatumRoleIndex(
          controlAtTraverseStage(stage),
          new Constr(5, []),
        ),
      ).toThrow(/integer or bytes traversal stage/);
  });

  it("refuses an unknown datum action", () => {
    expect(() =>
      ledgerOutputProofDatumRoleIndex(integerControl, new Constr(9, [])),
    ).toThrow(/Unknown datum action/);
  });
});
