/** Group 7's arm chain: the roots hop goes on to the staged failure hop for
 * a byte-string, string or integer builtin and to the material hop for the
 * rest, as the on-chain roots route them by tag. */
import { DEPLOYMENT_MANIFEST_CONTRACT_NAMES } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { MidgardCekCoreStepWitness } from "@al-ft/midgard-validation";
import { describe, expect, it } from "vitest";

import {
  CEK_CORE_ARM_CONTRACT_NAMES,
  CEK_CORE_BUILTIN_ARM_HOLDERS,
  cekCoreArmHops,
} from "../src/validation-trace-dispute/one-step-core-chains.js";

const ROOT = new Uint8Array(32);

const stagedFailure = (
  tag: bigint,
  stagedOperands: readonly Buffer[] = [],
): MidgardCekCoreStepWitness => ({
  kind: "executeBuiltinStagedFailure",
  tag,
  arguments: [],
  material: { runs: { refs: [], operands: [] }, sources: [] },
  stagedOperands,
});

describe("group 7's arm chain", () => {
  it.each([0n, 1n, 9n, 10n, 14n, 20n, 25n, 71n, 72n, 86n])(
    "takes builtin %s from the roots to the staged failure hop",
    (tag) => {
      const hops = cekCoreArmHops(7, stagedFailure(tag), ROOT);
      expect(hops.holders).toEqual(["semanticFailureRoots", "stagedFailure"]);
      expect(hops.facts).toHaveLength(2);
    },
  );

  it.each([26n, 30n, 70n, 73n, 85n, 87n])(
    "takes builtin %s from the roots to the material hop",
    (tag) => {
      expect(cekCoreArmHops(7, stagedFailure(tag), ROOT).holders).toEqual([
        "semanticFailureRoots",
        "semanticFailureMaterial",
      ]);
    },
  );

  it("does not drive a staged failure whose material names staged operands", () => {
    expect(() =>
      cekCoreArmHops(7, stagedFailure(25n, [Buffer.alloc(5_000)]), ROOT),
    ).toThrow(/staged operands/u);
  });

  it("refuses a witness of another arm", () => {
    expect(() =>
      cekCoreArmHops(
        7,
        { kind: "executeBuiltinFailure", tag: 14n, arguments: [] },
        ROOT,
      ),
    ).toThrow(/another witness/u);
  });

  it("installs the staged failure hop as a builtin arm under its manifest name", () => {
    expect(CEK_CORE_BUILTIN_ARM_HOLDERS).toContain("stagedFailure");
    expect(CEK_CORE_ARM_CONTRACT_NAMES.stagedFailure).toBe(
      "validationTraceDisputeCekCoreStagedFailure",
    );
    expect(DEPLOYMENT_MANIFEST_CONTRACT_NAMES).toContain(
      CEK_CORE_ARM_CONTRACT_NAMES.stagedFailure,
    );
  });
});
