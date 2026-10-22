import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { describe, expect, it } from "vitest";

import { RejectCodes } from "../src/index.js";
import {
  limitRefusal,
  MIN_NATIVE_NODE_BYTES,
  NATIVE_ROWS,
  NATIVE_SCRIPT_LIMIT,
  overLimitNative,
} from "./emission-order.native.rows.js";
import {
  expectControlledRows,
  expectRowVerdicts,
  machineTrace,
  producer,
} from "./emission-order.support.js";

/** The rows of case (e); see `emission-order.native.rows.ts`. */
describe("emission order (e): native-script failures", () => {
  it("every multi-fault row has a single-fault control per fault", () => {
    expectControlledRows(NATIVE_ROWS, [
      "e_false_spend_",
      "e_false_mint_and_integrity_",
      "e_false_mint_and_receive_",
      "e_inline_false_",
    ]);
  });

  it.each(NATIVE_ROWS)("$name", expectRowVerdicts, 180_000);

  it("a native script over a scan limit outgrows the field-6 and output caps", () => {
    const shortest = MIN_NATIVE_NODE_BYTES * (NATIVE_SCRIPT_LIMIT + 1);
    for (const limit of ["node", "depth"] as const) {
      expect(overLimitNative(limit).length).toBeGreaterThanOrEqual(shortest);
    }
    expect(shortest).toBeGreaterThan(
      MIDGARD_CONSENSUS_LIMITS.maxScriptWitnessesPreimageBytes,
    );
    expect(shortest).toBeGreaterThan(
      MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes,
    );
  });

  it.each([
    [limitRefusal("node"), "node count exceeds the V1 maximum"],
    [limitRefusal("depth"), "nesting exceeds the V1 maximum"],
  ])(
    "%#: an over-limit native witness fails canonical decode",
    async (row, detail) => {
      const { rejection } = await producer(row);
      expect(rejection?.code).toBe(RejectCodes.InvalidFieldType);
      expect(rejection?.consensusPhase).toBe("canonicalDecode");
      expect(rejection?.subject).toBeUndefined();
      expect(rejection?.detail).toContain(detail);
      await expect(machineTrace(row)).rejects.toThrow(
        "Canonical rejection E_INVALID_FIELD_TYPE requires its direct proof",
      );
    },
    180_000,
  );
});
