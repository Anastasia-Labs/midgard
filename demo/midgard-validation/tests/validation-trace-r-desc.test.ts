import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import { decodeMidgardNativeTxWitnessSetCompact } from "@al-ft/midgard-core/codec";
import { MIDGARD_EMPTY_FIELD_COMMITMENT } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { describe, expect, it } from "vitest";

import { requiresRealValidationDescriptorV1 } from "../src/index.js";

type RDescVector = {
  readonly name: string;
  readonly sourceKind: "normal" | "forced";
  readonly canonicalTransactionCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly redeemerTxWitsHash: string;
  readonly requiresRealDescriptor: boolean;
};

/**
 * Rule R-DESC golden vectors, shared with the on-chain gate: a normal
 * transaction with an empty field 8, one with a single redeemer, and a forced
 * transaction whose field 8 is non-empty.
 */
const fixture = JSON.parse(
  readFileSync(resolve(import.meta.dirname, "fixtures/r-desc-v1.json"), "utf8"),
) as {
  readonly emptyFieldCommitment: string;
  readonly vectors: readonly RDescVector[];
};

describe("rule R-DESC", () => {
  it("pins the empty field commitment and the three vectors", () => {
    expect(fixture.emptyFieldCommitment).toBe(
      Buffer.from(MIDGARD_EMPTY_FIELD_COMMITMENT).toString("hex"),
    );
    expect(
      fixture.vectors.map((vector) => [
        vector.name,
        vector.sourceKind,
        vector.redeemerTxWitsHash === fixture.emptyFieldCommitment,
        vector.requiresRealDescriptor,
      ]),
    ).toEqual([
      ["normal-field8-empty", "normal", true, false],
      ["normal-field8-one-redeemer", "normal", false, true],
      ["forced-field8-nonempty", "forced", false, false],
    ]);
  });

  it.each(fixture.vectors)("decides $name", (vector) => {
    expect(
      Buffer.from(
        decodeMidgardNativeTxWitnessSetCompact(
          Buffer.from(vector.witnessSetCompactCbor, "hex"),
        ).redeemerTxWitsHash,
      ).toString("hex"),
    ).toBe(vector.redeemerTxWitsHash);
    expect(
      requiresRealValidationDescriptorV1(
        Buffer.from(vector.canonicalTransactionCbor, "hex"),
        vector.sourceKind,
      ),
    ).toBe(vector.requiresRealDescriptor);
  });

  it("keeps the placeholder for bytes that are not a normal transaction", () => {
    expect(
      requiresRealValidationDescriptorV1(Buffer.from("ff", "hex"), "normal"),
    ).toBe(false);
    expect(requiresRealValidationDescriptorV1(Buffer.alloc(0), "normal")).toBe(
      false,
    );
  });
});
