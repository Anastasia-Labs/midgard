/**
 * `input-no-idx` step-02's carriage route, after #604 replaced the family's
 * bespoke publication with §8's ladder.
 *
 * The file this replaces tested `PublishedSpendInputsV1` internals — fee
 * selection around a typed publication UTxO, and that publication transaction's
 * exact typed output. None of it survives: the step has one redeemer route, and
 * the preimage travels under a §8 carriage tier chosen by its own length rather
 * than by this builder. What is worth pinning now is what replaced it, so that is
 * what this file pins.
 *
 * Every negative asserts that its mutation genuinely landed before concluding
 * anything from the refusal.
 */
import {
  computeMidgardNativeTxId,
  encodeMidgardNativeTxCompact,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
} from "@al-ft/midgard-core";
import {
  encodeMidgardTxInputCanonical,
  fieldPreimagePublicationDatumCbor,
  MIDGARD_FIELD_INDEX,
  type MidgardTxInput,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
} from "../src/field-opening.js";
import { h32, makeNativeTx } from "./support/submit-init-emulator-shared.js";

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

    // §8.5: a nothing-but-bytes inline datum — not the retired
    // `PublishedSpendInputsV1`, which bound the publication to one computation
    // thread and one prover and so could not be healed by anyone else (§8.7).
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
