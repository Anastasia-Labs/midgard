import {
  encodeMidgardTxOutput,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
} from "@al-ft/midgard-core";
import { encodeMidgardDefiniteBytes } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { describe, expect, it } from "vitest";

import {
  buildOpaqueNativeReferenceScriptLedgerOutputMaterial,
  RejectCodes,
} from "../src/index.js";
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
  funded,
  KEY_ADDRESS,
  keySpent,
  machineTrace,
  mintedOutput,
  mintOf,
  producer,
} from "./emission-order.support.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  outRefFromByte,
} from "./validation-fixtures.js";

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

  it("a truncated reference native script a whole number of chunks long stops at input resolution", async () => {
    // `all` claiming one child more than it holds: an execution-mode scan
    // would read its next token head at the item's end, past the last chunk.
    const leaves = [
      ...Array.from({ length: 2 }, () => Buffer.from("82041818", "hex")),
      ...Array.from({ length: 1359 }, () => Buffer.from("820400", "hex")),
    ];
    const item = Buffer.concat([
      Buffer.from([0x82, 0x00]),
      encodeMidgardDefiniteBytes(
        Buffer.concat([Buffer.from("8201990552", "hex"), ...leaves]),
      ),
    ]);
    expect(leaves.length + 1).toBe(0x0552);
    expect(item.length).toBe(MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
    const plain = encodeMidgardTxOutput({
      address: KEY_ADDRESS,
      value: { lovelace: FUNDED_OUTPUT_LOVELACE, assets: new Map() },
    });
    const output = Buffer.concat([
      Buffer.from([plain[0]! + 1]),
      plain.subarray(1),
      Buffer.from([0x03]),
      item,
    ]);
    const policy = buildOpaqueNativeReferenceScriptLedgerOutputMaterial({
      outputIndex: 0,
      outputCbor: output,
    }).descriptor.referenceScriptHash.toString("hex");
    const reference = outRefFromByte(0x90);
    const { rejection } = await producer({
      name: "e_refusal_truncated_reference_native_chunk_multiple",
      kind: "refusal",
      guard: true,
      state: [
        [keySpent, funded],
        [reference, output],
      ],
      tx: {
        spendInputs: [keySpent],
        referenceInputs: [reference],
        outputs: [mintedOutput(policy)],
        mintPreimageCbor: mintOf(policy),
      },
      expected: { code: RejectCodes.InvalidOutput, reason: null },
    });
    // The reference input's output does not decode, so no native execution
    // runs; the trace builder refuses to resolve that output at all.
    expect(rejection).toMatchObject({
      code: RejectCodes.InvalidOutput,
      consensusPhase: "resolveInputs",
      subject: { arm: "InputSpentOutputNonCanonical", sourceKind: 1n },
    });
  }, 180_000);
});
