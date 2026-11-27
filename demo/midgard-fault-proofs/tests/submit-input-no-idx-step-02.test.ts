/**
 * `input-no-idx` step-02's carriage route through §8's ladder: the step has
 * one redeemer route, and the preimage travels under a §8 carriage tier
 * chosen by its own length rather than by this builder.
 *
 * Every negative asserts that its mutation genuinely landed before concluding
 * anything from the refusal.
 */
import {
  computeMidgardNativeTxId,
  encodeMidgardNativeTxCompact,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
} from "@al-ft/midgard-core";
import { type MidgardTxInput } from "@al-ft/midgard-sdk/core";
import {
  encodeMidgardTxInputCanonical,
  MIDGARD_FIELD_INDEX,
} from "@al-ft/midgard-sdk/fraud-proof";
import { fieldPreimagePublicationDatumCbor } from "@al-ft/midgard-sdk/state-queue";
import { describe, expect, it } from "vitest";

import { planFaultProofFieldOpening } from "../src/field-opening.plan-fault-proof-field-opening.js";
import { faultProofFieldOpening } from "../src/field-opening.publish-fault-proof-field-carriage.js";
import { h32 } from "./support/emulator/header-fixtures.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";

const OWNER = "cd".repeat(28);

const inputItem = (txIdByte: string, outputIndex: bigint): MidgardTxInput => ({
  tx_id: h32(txIdByte),
  output_index: outputIndex,
});

const fixture = () => {
  const inputs = [inputItem("31", 0n), inputItem("32", 1n)];
  const nativeTx = makeNativeTx({
    spendInputCbors: inputs.map(encodeMidgardTxInputCanonical),
    fee: 11n,
    outputByte: "33",
  });
  return {
    inputs,
    anchorTxId: computeMidgardNativeTxId(nativeTx).toString("hex"),
    compactCbor: encodeMidgardNativeTxCompact(nativeTx.compact).toString("hex"),
    spendInputsHash: Buffer.from(
      nativeTx.compact.transactionBody.spendInputsHash,
    ).toString("hex"),
  };
};

const plan = (
  overrides: Partial<Parameters<typeof planFaultProofFieldOpening>[0]> = {},
) => {
  const { inputs, anchorTxId, compactCbor } = fixture();
  return planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    anchorTxId,
    nativeTxCompactCbor: compactCbor,
    itemCbors: inputs.map(encodeMidgardTxInputCanonical),
    owner: OWNER,
    label: "Input-no-idx step 02 spend-inputs",
    ...overrides,
  });
};

describe("input-no-idx step-02 §8 carriage", () => {
  it("carries a short preimage in the step's own redeemer (tier 1)", () => {
    const { compactCbor, spendInputsHash } = fixture();
    const planned = plan();

    expect(planned.plan.tier).toBe("Inline");
    expect(planned.itemCount).toBe(2);
    // §4: the commitment is the disputed transaction's own field-0 hash.
    expect(planned.commitment).toBe(spendInputsHash);
    // Two 38-byte items and a one-byte §5.1 header — far inside tier 1.
    expect(planned.preimage.length).toBeLessThan(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
    );
    expect(planned.plan.publications).toHaveLength(0);

    expect(faultProofFieldOpening({ planned, label: "t" })).toStrictEqual({
      BodyFieldOpening: {
        native_tx_compact_cbor: compactCbor,
        carriage: { Inline: { preimage: planned.preimage.toString("hex") } },
      },
    });
  });

  it("publishes the same preimage as §8.5 raw carriage when tier 2 is chosen", () => {
    const inline = plan();
    const published = plan({ publish: true });

    // The one tier choice §8 leaves open changes which transaction pays, never
    // what the door authenticates: same bytes, same §4 commitment.
    expect(published.plan.tier).toBe("RawUtxo");
    expect(published.commitment).toBe(inline.commitment);
    expect(published.preimage).toStrictEqual(inline.preimage);

    // §8.5: a nothing-but-bytes inline datum, which anyone can heal (§8.7).
    expect(published.plan.publications).toHaveLength(1);
    const publication = published.plan.publications[0]!;
    expect(publication.bytes).toStrictEqual(published.preimage);
    expect(fieldPreimagePublicationDatumCbor(publication.bytes)).toBe(
      fieldPreimagePublicationDatumCbor(inline.preimage),
    );

    // Tier 2 names a reference input, so it cannot be built without one.
    expect(() =>
      faultProofFieldOpening({ planned: published, label: "t" }),
    ).toThrow(/is not among the transaction's reference inputs/u);
  });

  it("refuses an input list that is not the anchored transaction's field 0", () => {
    const { inputs } = fixture();
    const tampered = [inputs[0]!, { ...inputs[1]!, output_index: 2n }];
    // The mutation landed.
    expect(tampered).not.toStrictEqual(inputs);

    expect(() =>
      plan({ itemCbors: tampered.map(encodeMidgardTxInputCanonical) }),
    ).toThrow(/the disputed transaction commits at §2\.5 field 0/u);
  });

  it("refuses compact bytes for a transaction the thread did not anchor", () => {
    const { inputs, anchorTxId } = fixture();
    const otherTx = makeNativeTx({
      spendInputCbors: inputs.map(encodeMidgardTxInputCanonical),
      fee: 12n,
      outputByte: "33",
    });
    // The mutation landed: same field 0, different transaction.
    expect(computeMidgardNativeTxId(otherTx).toString("hex")).not.toBe(
      anchorTxId,
    );

    expect(() =>
      plan({
        nativeTxCompactCbor: encodeMidgardNativeTxCompact(
          otherTx.compact,
        ).toString("hex"),
      }),
    ).toThrow(/not the anchored transaction id/u);
  });
});
