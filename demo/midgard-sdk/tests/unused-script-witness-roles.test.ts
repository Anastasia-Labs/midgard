import { deploymentFamilyStepNames } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  checkUnusedScriptWitnessRoleOrder,
  UNUSED_SCRIPT_WITNESS_ROLE_ORDER,
} from "../src/fraud-proof/contracts/families/unused-script-witness.js";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  type VerdictSubject,
} from "../src/fraud-proof/proof-thread-substrate.js";
import {
  unusedScriptWitnessConvicts,
  type UnusedScriptWitnessFacts,
  unusedScriptWitnessIsUnused,
} from "../src/fraud-proof/unused-script-witness.js";
import type { RejectionReason } from "../src/rejection-reason.js";

const manifestSteps = () => [
  ...deploymentFamilyStepNames("unusedScriptWitness"),
];

const swapped = (first: number, second: number) => {
  const steps = manifestSteps();
  [steps[first], steps[second]] = [steps[second]!, steps[first]!];
  return steps;
};

describe("unused-script-witness role order", () => {
  it("names every manifest step in the manifest's order", () => {
    expect(manifestSteps()).toHaveLength(24);
    expect(UNUSED_SCRIPT_WITNESS_ROLE_ORDER).toHaveLength(24);
    expect(() =>
      checkUnusedScriptWitnessRoleOrder(manifestSteps()),
    ).not.toThrow();
  });

  it("refuses a manifest that swaps the witness grammar and witness walk steps", () => {
    const grammar = UNUSED_SCRIPT_WITNESS_ROLE_ORDER.indexOf("witnessGrammar");
    const scan = UNUSED_SCRIPT_WITNESS_ROLE_ORDER.indexOf("witnessScan");
    expect(scan).toBe(grammar + 1);
    expect(() =>
      checkUnusedScriptWitnessRoleOrder(swapped(grammar, scan)),
    ).toThrow(
      `unused-script-witness role witnessGrammar is fraudProofUnusedScriptWitnessWitnessGrammar, but manifest step ${grammar} is fraudProofUnusedScriptWitnessWitnessScan`,
    );
  });

  it("refuses a manifest with a step more or less than the roles", () => {
    const steps = manifestSteps();
    for (const changed of [steps.slice(1), [...steps, steps[0]!]])
      expect(() => checkUnusedScriptWitnessRoleOrder(changed)).toThrow(
        `unused-script-witness has 24 roles; the manifest has ${changed.length} steps`,
      );
  });
});

const TX = "ab".repeat(32);

const accepted = acceptedVerdictSubject(TX);

const forced = (reason: RejectionReason | null): VerdictSubject =>
  forcedVerdictSubject({
    transactionId: TX,
    sourceKey: { transactionId: "cd".repeat(32), outputIndex: 0n },
    rejectionReason: reason,
  });

const context = { body_network_mismatch: false };

describe("unused-script-witness verdict twins", () => {
  it("calls a witness unused unless it is both its hash's source and executed", () => {
    const rows: readonly [UnusedScriptWitnessFacts, boolean][] = [
      [null, false],
      [{ selected: true, reached: true }, false],
      [{ selected: true, reached: false }, true],
      [{ selected: false, reached: true }, true],
      [{ selected: false, reached: false }, true],
    ];
    for (const [facts, unused] of rows)
      expect(unusedScriptWitnessIsUnused(facts)).toBe(unused);
  });

  // The same cases are pinned on-chain in
  // unused-script-witness/terminal.test.ak; change both together.
  it("convicts each direction exactly when its verdict contradicts the audit", () => {
    const witness = (script_index: bigint): RejectionReason => ({
      UnusedScriptWitness: { script_index },
    });
    const rows: readonly [VerdictSubject, bigint, boolean, boolean][] = [
      [accepted, 0n, true, true],
      [accepted, 0n, false, false],
      [forced(null), 0n, true, true],
      [forced(null), 0n, false, false],
      // The recorded witness: refuted when used or absent.
      [forced(witness(0n)), 0n, false, true],
      [forced(witness(0n)), 0n, true, false],
      [forced(witness(2n)), 2n, false, true],
      [forced(witness(-1n)), -1n, false, true],
      // An earlier unused witness refutes a later recorded one.
      [forced(witness(1n)), 0n, true, true],
      [forced(witness(0n)), 1n, true, false],
      // Other reasons by their rank against the phase-8 audit.
      [forced({ UnusedRedeemer: { redeemer_index: 0n } }), 0n, true, true],
      [forced("ScriptIntegrityHashMismatch"), 0n, true, true],
      [forced("ValueNotPreserved"), 0n, true, true],
      [forced("FeeBelowMinimum"), 0n, true, false],
      [
        forced({
          ScriptSourceMissing: { purpose_kind: 2n, purpose_index: 0n },
        }),
        0n,
        true,
        false,
      ],
      [forced("ValueNotPreserved"), 0n, false, false],
    ];
    for (const [subject, scriptIndex, unused, convicts] of rows)
      expect(
        unusedScriptWitnessConvicts(subject, context, scriptIndex, unused),
      ).toBe(convicts);
  });
});
