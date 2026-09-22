import { Constr, type Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  deriveLedgerOutputProofFinalizeClaims,
  deriveLedgerOutputProofStepClaims,
  deriveLedgerOutputProofStepPlan,
  ledgerOutputProofAttestationRoles,
  ledgerOutputProofDatumRoleIndex,
  ledgerOutputProofFactAttachRoles,
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
      expect(plan.claimedScalar).toStrictEqual(new Constr(1, []));
      expect(plan.attestationRoles).toStrictEqual([]);
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

  it("splits the attach action by the scalar traversal stage", () => {
    expect(
      ledgerOutputProofDatumRoleIndex(integerControl, new Constr(5, [])),
    ).toBe(5);
    expect(
      ledgerOutputProofDatumRoleIndex(bytesControl, new Constr(5, [])),
    ).toBe(17);
  });

  it.each([
    [1n, 7],
    [2n, 18],
    [3n, 20],
    [4n, 21],
    [5n, 22],
  ])(
    "routes the advance action at traversal stage %i to role %i",
    (stage, role) => {
      expect(
        ledgerOutputProofDatumRoleIndex(
          controlAtTraverseStage(stage),
          new Constr(0, []),
        ),
      ).toBe(role);
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

  it("refuses the advance action outside an active traversal stage", () => {
    for (const stage of [0n, 6n, 7n])
      expect(() =>
        ledgerOutputProofDatumRoleIndex(
          controlAtTraverseStage(stage),
          new Constr(0, []),
        ),
      ).toThrow(/active traversal stage/);
  });

  it("refuses an unknown datum action", () => {
    expect(() =>
      ledgerOutputProofDatumRoleIndex(integerControl, new Constr(9, [])),
    ).toThrow(/Unknown datum action/);
  });
});

const NONE = new Constr(1, []);
const some = (inner: Data): Data => new Constr(0, [inner]);
const ROOT = "11".repeat(32);

const controlWithTraverse = (slots: Data[]): Data[] => {
  const control: Data[] = Array.from({ length: 17 }, () => 0n);
  control[3] = 40n;
  control[7] = new Constr(0, [slots]);
  return control;
};

describe("ledger output proof step claims", () => {
  it("claims nothing for a role without a scalar attestation", () => {
    for (const roleIndex of [
      0, 1, 2, 3, 4, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 19, 20, 21, 22, 23,
    ]) {
      expect(
        deriveLedgerOutputProofStepClaims({
          control: controlWithTraverse([]),
          roleIndex,
        }),
      ).toStrictEqual({ claimedScalar: NONE });
      expect(ledgerOutputProofAttestationRoles(roleIndex)).toStrictEqual([]);
    }
  });

  it("claims the attach-integer scalar", () => {
    const integerWire = [1n, 2n, 5n, 3n, 5n, NONE];
    const control = controlWithTraverse([
      1n,
      1n,
      3n,
      8n,
      2n,
      ROOT,
      NONE,
      some(integerWire),
      NONE,
      NONE,
    ]);
    const claims = deriveLedgerOutputProofStepClaims({
      control,
      roleIndex: 5,
    });
    expect(claims.claimedScalar).toStrictEqual(
      new Constr(0, [
        3n,
        8n,
        2n,
        ROOT,
        new Constr(0, [1n, 2n, 5n, 3n, 5n, NONE]),
      ]),
    );
    expect(ledgerOutputProofAttestationRoles(5)).toStrictEqual([
      "scalarInteger",
    ]);
  });

  it("transcodes a nested blob and hash trace into the scalar claim", () => {
    const blakeWire = [1n, 0n, 0n, 3n, "22".repeat(64), "", 0n, "", 0n];
    const frontierWire = [1n, 0n, 0n, []];
    const blobWire = [1n, 0n, 5n, 3n, frontierWire, some(blakeWire)];
    const integerWire = [1n, 1n, 5n, 3n, 5n, some(blobWire)];
    const control = controlWithTraverse([
      1n,
      1n,
      3n,
      8n,
      2n,
      ROOT,
      NONE,
      some(integerWire),
      NONE,
      NONE,
    ]);
    const claims = deriveLedgerOutputProofStepClaims({
      control,
      roleIndex: 5,
    });
    expect(claims.claimedScalar).toStrictEqual(
      new Constr(0, [
        3n,
        8n,
        2n,
        ROOT,
        new Constr(0, [
          1n,
          1n,
          5n,
          3n,
          5n,
          some(
            new Constr(0, [
              1n,
              0n,
              5n,
              3n,
              new Constr(0, [0n, 0n, []]),
              some(new Constr(0, [...blakeWire])),
            ]),
          ),
        ]),
      ]),
    );
  });

  it("claims the integer scalar for the advance-integer role", () => {
    const integerWire = [1n, 0n, 5n, 4n, 0n, NONE];
    const control = controlWithTraverse([
      1n,
      1n,
      3n,
      8n,
      2n,
      ROOT,
      NONE,
      some(integerWire),
      NONE,
      NONE,
    ]);
    const claims = deriveLedgerOutputProofStepClaims({
      control,
      roleIndex: 7,
    });
    expect(claims.claimedScalar).toStrictEqual(
      new Constr(0, [
        3n,
        8n,
        2n,
        ROOT,
        new Constr(0, [1n, 0n, 5n, 4n, 0n, NONE]),
      ]),
    );
    expect(ledgerOutputProofAttestationRoles(7)).toStrictEqual([
      "scalarInteger",
    ]);
  });

  it("claims the bytes scalar for the advance-bytes role", () => {
    const bytesWire = [1n, 0n, 5n, 4n, 0n, NONE];
    const control = controlWithTraverse([
      1n,
      2n,
      3n,
      8n,
      2n,
      ROOT,
      NONE,
      NONE,
      some(bytesWire),
      NONE,
    ]);
    const claims = deriveLedgerOutputProofStepClaims({
      control,
      roleIndex: 18,
    });
    expect(claims.claimedScalar).toStrictEqual(
      new Constr(0, [
        3n,
        8n,
        2n,
        ROOT,
        new Constr(0, [1n, 0n, 5n, 4n, 0n, NONE]),
      ]),
    );
    expect(ledgerOutputProofAttestationRoles(18)).toStrictEqual([
      "scalarBytes",
    ]);
  });

  it("refuses a scalar role without its active sub-control", () => {
    const control = controlWithTraverse([
      1n,
      1n,
      3n,
      8n,
      2n,
      ROOT,
      NONE,
      NONE,
      NONE,
      NONE,
    ]);
    expect(() =>
      deriveLedgerOutputProofStepClaims({ control, roleIndex: 5 }),
    ).toThrow(/active integer sub-control/);
  });

  it("refuses an out-of-range role index", () => {
    expect(() =>
      deriveLedgerOutputProofStepClaims({
        control: controlWithTraverse([]),
        roleIndex: 24,
      }),
    ).toThrow(/Unknown ledger output proof stage role/);
    expect(() => ledgerOutputProofAttestationRoles(24)).toThrow(
      /Unknown ledger output proof stage role/,
    );
  });
});

describe("ledger output proof finalize claims", () => {
  const terminalControl = (datumOffset: bigint): Data[] => {
    const scan: Data[] = Array.from({ length: 23 }, () => 0n);
    scan[16] = datumOffset;
    const control: Data[] = Array.from({ length: 17 }, () => 0n);
    control[1] = 6n;
    for (let slot = 12; slot < 17; slot += 1) control[slot] = NONE;
    control[3] = 40n;
    control[5] = scan;
    control[6] = some([1n, 3n, 0n, "", 0n, 0n, some([ROOT, 5n, 7n])]);
    control[7] = some([
      1n,
      7n,
      10n,
      8n,
      8n,
      ROOT,
      NONE,
      NONE,
      NONE,
      some(["22".repeat(32), 3n, 4n]),
    ]);
    return control;
  };

  it("claims the terminal value and datum summaries", () => {
    const claims = deriveLedgerOutputProofFinalizeClaims(terminalControl(10n));
    expect(claims.claimedValueSummary).toStrictEqual(
      new Constr(0, [ROOT, 5n, 7n]),
    );
    expect(claims.claimedDatumSummary).toStrictEqual(
      some(new Constr(0, ["22".repeat(32), 3n, 4n])),
    );
  });

  it("claims no datum summary when the scan pins no datum", () => {
    const claims = deriveLedgerOutputProofFinalizeClaims(terminalControl(-1n));
    expect(claims.claimedDatumSummary).toStrictEqual(NONE);
  });

  it("refuses a non-terminal control", () => {
    const control = terminalControl(10n);
    control[1] = 5n;
    expect(() => deriveLedgerOutputProofFinalizeClaims(control)).toThrow(
      /terminal output proof control/,
    );
  });

  it("attaches fact groups in the canonical order", () => {
    const fact = some("33".repeat(32));
    const control = terminalControl(10n);
    expect(ledgerOutputProofFactAttachRoles(control)).toStrictEqual([2, 3]);
    control[15] = fact;
    control[16] = fact;
    expect(ledgerOutputProofFactAttachRoles(control)).toStrictEqual([0]);
    control[13] = fact;
    expect(ledgerOutputProofFactAttachRoles(control)).toStrictEqual([1]);
    control[14] = fact;
    expect(ledgerOutputProofFactAttachRoles(control)).toStrictEqual([]);
  });

  it("refuses a half-attached fact group", () => {
    const control = terminalControl(10n);
    control[15] = some("33".repeat(32));
    expect(ledgerOutputProofFactAttachRoles(control)).toStrictEqual([0]);
  });
});
