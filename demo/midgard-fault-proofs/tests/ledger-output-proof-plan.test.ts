import { Constr, type Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  deriveLedgerOutputProofFinalizeClaims,
  deriveLedgerOutputProofStepClaims,
  deriveLedgerOutputProofStepPlan,
  ledgerOutputProofAttestationRoles,
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
      expect(plan.claimedSpan).toStrictEqual(new Constr(1, []));
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
const OUTPUT_BYTES = Buffer.from(
  Array.from({ length: 40 }, (_, index) => index),
).toString("hex");

const chunkProofData = (chunkIndex: bigint, chunk: string): Data =>
  new Constr(0, [1n, 2n, 0n, 40n, chunkIndex, chunk, 0n, []]);
const datumWitness = (chunk: string): Constr<Data> =>
  new Constr(3, [
    new Constr(0, []),
    some(chunkProofData(0n, chunk)),
    new Constr(1, []),
  ]);
const noSpanWitness = (): Constr<Data> =>
  new Constr(3, [new Constr(0, []), new Constr(1, []), new Constr(1, [])]);

const controlWithTraverse = (slots: Data[]): Data[] => {
  const control: Data[] = Array.from({ length: 12 }, () => 0n);
  control[3] = 40n;
  control[7] = new Constr(0, [slots]);
  return control;
};

describe("ledger output proof step claims", () => {
  it("claims nothing for a role without attestations", () => {
    for (const roleIndex of [0, 1, 2, 3, 6, 10, 11, 12, 13, 19]) {
      expect(
        deriveLedgerOutputProofStepClaims({
          control: controlWithTraverse([]),
          witness: noSpanWitness(),
          roleIndex,
        }),
      ).toStrictEqual({ claimedSpan: NONE, claimedScalar: NONE });
      expect(ledgerOutputProofAttestationRoles(roleIndex)).toStrictEqual([]);
    }
  });

  it.each([4, 14, 15, 16])(
    "claims the head window content for head role %i",
    (roleIndex) => {
      const control = controlWithTraverse([
        1n,
        0n,
        10n,
        20n,
        0n,
        "",
        NONE,
        NONE,
        NONE,
        NONE,
      ]);
      const claims = deriveLedgerOutputProofStepClaims({
        control,
        witness: datumWitness(OUTPUT_BYTES),
        roleIndex,
      });
      expect(claims.claimedScalar).toStrictEqual(NONE);
      expect(claims.claimedSpan).toStrictEqual(
        new Constr(2, [10n, 14n, OUTPUT_BYTES.slice(20, 48)]),
      );
      expect(ledgerOutputProofAttestationRoles(roleIndex)).toStrictEqual([
        "span",
      ]);
    },
  );

  it("claims the attach-integer scalar without a span", () => {
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
      witness: noSpanWitness(),
      roleIndex: 5,
    });
    expect(claims.claimedSpan).toStrictEqual(NONE);
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
      witness: noSpanWitness(),
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

  it("claims the syntax window and scalar for the advance-integer role", () => {
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
      witness: datumWitness(OUTPUT_BYTES),
      roleIndex: 7,
    });
    expect(claims.claimedSpan).toStrictEqual(
      new Constr(2, [5n, 4n, OUTPUT_BYTES.slice(10, 18)]),
    );
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
      "span",
      "scalarInteger",
    ]);
  });

  it("claims the bytes syntax window and scalar for the advance-bytes role", () => {
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
      witness: datumWitness(OUTPUT_BYTES),
      roleIndex: 18,
    });
    expect(claims.claimedSpan).toStrictEqual(
      new Constr(2, [5n, 2n, OUTPUT_BYTES.slice(10, 14)]),
    );
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
      "span",
      "scalarBytes",
    ]);
  });

  it("claims no span for the large-constructor completion step", () => {
    const blobWire = [1n, 1n, 1n, 3n, [1n, 1n, 3n, [[0n, ROOT, 3n]]], NONE];
    const integerWire = [1n, 2n, 1n, 3n, 5n, some(blobWire)];
    const control = controlWithTraverse([
      1n,
      3n,
      0n,
      10n,
      1n,
      ROOT,
      some(2n),
      some(integerWire),
      NONE,
      NONE,
    ]);
    const claims = deriveLedgerOutputProofStepClaims({
      control,
      witness: noSpanWitness(),
      roleIndex: 20,
    });
    expect(claims).toStrictEqual({ claimedSpan: NONE, claimedScalar: NONE });
  });

  it("claims the single header byte for the large-fields completion", () => {
    const blobWire = [1n, 1n, 1n, 3n, [1n, 1n, 3n, [[0n, ROOT, 3n]]], NONE];
    const integerWire = [1n, 2n, 1n, 3n, 5n, some(blobWire)];
    const control = controlWithTraverse([
      1n,
      4n,
      0n,
      10n,
      4n,
      ROOT,
      some(2n),
      some(integerWire),
      NONE,
      NONE,
    ]);
    const claims = deriveLedgerOutputProofStepClaims({
      control,
      witness: datumWitness(OUTPUT_BYTES),
      roleIndex: 21,
    });
    expect(claims.claimedSpan).toStrictEqual(
      new Constr(2, [4n, 1n, OUTPUT_BYTES.slice(8, 10)]),
    );
  });

  it("claims the break byte for the close step", () => {
    const control = controlWithTraverse([
      1n,
      5n,
      0n,
      10n,
      4n,
      ROOT,
      NONE,
      NONE,
      NONE,
      NONE,
    ]);
    const claims = deriveLedgerOutputProofStepClaims({
      control,
      witness: datumWitness(OUTPUT_BYTES),
      roleIndex: 22,
    });
    expect(claims.claimedSpan).toStrictEqual(
      new Constr(2, [4n, 1n, OUTPUT_BYTES.slice(8, 10)]),
    );
  });

  it("refuses a head step whose control demands no span", () => {
    const control = controlWithTraverse([
      1n,
      6n,
      0n,
      10n,
      4n,
      ROOT,
      NONE,
      NONE,
      NONE,
      NONE,
    ]);
    expect(() =>
      deriveLedgerOutputProofStepClaims({
        control,
        witness: noSpanWitness(),
        roleIndex: 4,
      }),
    ).toThrow(/demands a source span/);
  });

  const scanWith = (entries: Record<number, bigint>): Data[] => {
    const scan: Data[] = Array.from({ length: 23 }, () => 0n);
    for (const [index, value] of Object.entries(entries))
      scan[Number(index)] = value;
    return scan;
  };

  it("claims sliced reference-script chunks by cursor", () => {
    const control: Data[] = Array.from({ length: 12 }, () => 0n);
    control[3] = 5000n;
    control[5] = scanWith({ 20: 5n });
    control[8] = 0n;
    expect(
      deriveLedgerOutputProofStepClaims({
        control,
        witness: noSpanWitness(),
        roleIndex: 8,
      }).claimedSpan,
    ).toStrictEqual(new Constr(0, [5n, 4095n]));
    control[8] = 1n;
    expect(
      deriveLedgerOutputProofStepClaims({
        control,
        witness: noSpanWitness(),
        roleIndex: 8,
      }).claimedSpan,
    ).toStrictEqual(new Constr(0, [4100n, 900n]));
    control[8] = 2n;
    expect(
      deriveLedgerOutputProofStepClaims({
        control,
        witness: noSpanWitness(),
        roleIndex: 8,
      }).claimedSpan,
    ).toStrictEqual(NONE);
  });

  it("claims sliced script-hash blocks in the ready stage only", () => {
    const control: Data[] = Array.from({ length: 12 }, () => 0n);
    control[3] = 5000n;
    control[5] = scanWith({ 21: 6n });
    control[10] = some([1n, 0n, 0n, 100n, "", "", 0n, "", 0n]);
    expect(
      deriveLedgerOutputProofStepClaims({
        control,
        witness: noSpanWitness(),
        roleIndex: 9,
      }).claimedSpan,
    ).toStrictEqual(new Constr(0, [6n, 99n]));
    control[10] = some([1n, 0n, 128n, 200n, "", "", 0n, "", 0n]);
    expect(
      deriveLedgerOutputProofStepClaims({
        control,
        witness: noSpanWitness(),
        roleIndex: 9,
      }).claimedSpan,
    ).toStrictEqual(new Constr(0, [133n, 72n]));
    control[10] = some([1n, 1n, 128n, 200n, "", "", 0n, "", 0n]);
    expect(
      deriveLedgerOutputProofStepClaims({
        control,
        witness: noSpanWitness(),
        roleIndex: 9,
      }).claimedSpan,
    ).toStrictEqual(NONE);
  });

  it("refuses an out-of-range role index", () => {
    expect(() =>
      deriveLedgerOutputProofStepClaims({
        control: controlWithTraverse([]),
        witness: noSpanWitness(),
        roleIndex: 23,
      }),
    ).toThrow(/Unknown ledger output proof stage role/);
    expect(() => ledgerOutputProofAttestationRoles(23)).toThrow(
      /Unknown ledger output proof stage role/,
    );
  });
});

describe("ledger output proof finalize claims", () => {
  const terminalControl = (datumOffset: bigint): Data[] => {
    const scan: Data[] = Array.from({ length: 23 }, () => 0n);
    scan[16] = datumOffset;
    const control: Data[] = Array.from({ length: 12 }, () => 0n);
    control[1] = 6n;
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
});
