import { deploymentFamilyStepNames } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  checkScriptIntegrityHashMismatchRoleOrder,
  SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_ORDER,
} from "../src/fraud-proof/contracts/families/script-integrity-hash-mismatch.js";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  type VerdictSubject,
} from "../src/fraud-proof/proof-thread-substrate.js";
import { scriptIntegrityHashMismatchConvicts } from "../src/fraud-proof/script-integrity-hash-mismatch.js";
import type { RejectionReason } from "../src/rejection-reason.js";

const manifestSteps = () => [
  ...deploymentFamilyStepNames("scriptIntegrityHashMismatch"),
];

const swapped = (first: number, second: number) => {
  const steps = manifestSteps();
  [steps[first], steps[second]] = [steps[second]!, steps[first]!];
  return steps;
};

describe("script-integrity-hash-mismatch role order", () => {
  it("names every manifest step in the manifest's order", () => {
    expect(manifestSteps()).toHaveLength(
      SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_ORDER.length,
    );
    expect(() =>
      checkScriptIntegrityHashMismatchRoleOrder(manifestSteps()),
    ).not.toThrow();
  });

  it("refuses a manifest that swaps the mint grammar and mint walk steps", () => {
    const grammar = SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_ORDER.indexOf(
      "allPurposesMintGrammar",
    );
    const mint =
      SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_ORDER.indexOf("allPurposesMint");
    expect(mint).toBe(grammar + 1);
    expect(() =>
      checkScriptIntegrityHashMismatchRoleOrder(swapped(grammar, mint)),
    ).toThrow(
      `script-integrity-hash-mismatch role allPurposesMintGrammar is fraudProofScriptIntegrityHashMismatchAllPurposesMintGrammar, but manifest step ${grammar} is fraudProofScriptIntegrityHashMismatchAllPurposesMint`,
    );
  });

  it("refuses a manifest with a step more or less than the roles", () => {
    const steps = manifestSteps();
    const roles = SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_ORDER.length;
    for (const changed of [steps.slice(1), [...steps, steps[0]!]])
      expect(() => checkScriptIntegrityHashMismatchRoleOrder(changed)).toThrow(
        `script-integrity-hash-mismatch has ${roles} roles; the manifest has ${changed.length} steps`,
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

describe("script-integrity-hash-mismatch conviction twin", () => {
  // The same cases are pinned on-chain in
  // script-integrity-hash-mismatch/terminal.test.ak; change both together.
  it("convicts each direction exactly when its verdict contradicts the hash", () => {
    const rows: readonly [VerdictSubject, boolean, boolean][] = [
      [accepted, true, true],
      [accepted, false, false],
      [forced(null), true, true],
      [forced(null), false, false],
      [forced("ScriptIntegrityHashMismatch"), false, true],
      [forced("ScriptIntegrityHashMismatch"), true, false],
      [forced({ UnusedRedeemer: { redeemer_index: 0n } }), true, false],
      [forced({ UnusedRedeemer: { redeemer_index: 0n } }), false, false],
      [forced("ValueNotPreserved"), true, true],
      [forced("ValueNotPreserved"), false, false],
    ];
    for (const [subject, mismatch, convicts] of rows)
      expect(
        scriptIntegrityHashMismatchConvicts(subject, context, mismatch),
      ).toBe(convicts);
  });
});
