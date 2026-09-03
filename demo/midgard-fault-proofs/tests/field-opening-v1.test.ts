/**
 * The builder-side §8.8 door seam (`src/field-opening-v1.ts`), pinned against a
 * real native transaction rather than a hand-written shape.
 *
 * Every negative here is one the on-chain door aborts on, and each asserts that
 * its mutation genuinely landed — a differing commitment, a differing id —
 * before concluding anything from the refusal. A mutation that quietly failed to
 * apply would otherwise read as a passing rejection.
 */
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  encodeMidgardNativeTxCompact,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { MIDGARD_FIELD_INDEX } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
} from "../src/field-opening-v1.js";
import { h32, makeNativeTx } from "./support/submit-init-emulator-shared.js";

const OWNER = "ab".repeat(28);

const fixture = () => {
  const nativeTx = makeNativeTx({
    spendInputCbors: [Buffer.from(h32("41"), "hex")],
    fee: 7n,
    referenceByte: "42",
    outputByte: "43",
    witnessByte: "44",
  });
  return {
    nativeTx,
    txId: computeMidgardNativeTxId(nativeTx).toString("hex"),
    compactCbor: encodeMidgardNativeTxCompact(nativeTx.compact).toString("hex"),
    spendInputItems: decodeMidgardNativeByteListPreimage(
      nativeTx.body.spendInputsPreimageCbor,
      "test.spend_inputs",
    ),
    referenceInputItems: decodeMidgardNativeByteListPreimage(
      nativeTx.body.referenceInputsPreimageCbor,
      "test.reference_inputs",
    ),
  };
};

describe("planFaultProofFieldOpeningV1", () => {
  it("plans tier-1 inline carriage for a body field and opens the anchored slot", () => {
    const { txId, compactCbor, spendInputItems, nativeTx } = fixture();
    const planned = planFaultProofFieldOpening({
      fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
      anchorTxId: txId,
      nativeTxCompactCbor: compactCbor,
      itemCbors: spendInputItems,
      owner: OWNER,
      label: "test",
    });

    expect(planned.plan.tier).toBe("Inline");
    expect(planned.itemCount).toBe(1);
    // §4: the preimage the redeemer carries commits to exactly the hash the
    // disputed transaction's own compact body holds at field 0.
    expect(planned.commitment).toBe(
      Buffer.from(nativeTx.compact.transactionBody.spendInputsHash).toString(
        "hex",
      ),
    );
    expect(midgardFieldCommitment(planned.preimage).toString("hex")).toBe(
      planned.commitment,
    );

    expect(faultProofFieldOpening({ planned, label: "test" })).toStrictEqual({
      BodyFieldOpening: {
        native_tx_compact_cbor: compactCbor,
        carriage: { Inline: { preimage: planned.preimage.toString("hex") } },
      },
    });
  });

  it("refuses compact bytes that do not re-derive to the anchored id", () => {
    const { txId, compactCbor, spendInputItems } = fixture();
    const otherTx = makeNativeTx({
      spendInputCbors: [Buffer.from(h32("41"), "hex")],
      fee: 8n,
      referenceByte: "42",
      outputByte: "43",
      witnessByte: "44",
    });
    const otherCompactCbor = encodeMidgardNativeTxCompact(
      otherTx.compact,
    ).toString("hex");
    // The mutation landed: a different transaction, hence a different id.
    expect(otherCompactCbor).not.toBe(compactCbor);
    expect(computeMidgardNativeTxId(otherTx).toString("hex")).not.toBe(txId);

    expect(() =>
      planFaultProofFieldOpening({
        fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
        anchorTxId: txId,
        nativeTxCompactCbor: otherCompactCbor,
        itemCbors: spendInputItems,
        owner: OWNER,
        label: "test",
      }),
    ).toThrow(/not the anchored transaction id/u);
  });

  it("refuses a preimage that opens a different field of the same transaction", () => {
    const { txId, compactCbor, spendInputItems, referenceInputItems } =
      fixture();
    // §4 removed field-index domain separation, so field 0 and field 1 commit
    // identically for identical items. The mutation is only meaningful because
    // these two item lists genuinely differ.
    expect(midgardFieldCommitment(referenceInputItems[0]!)).not.toStrictEqual(
      midgardFieldCommitment(spendInputItems[0]!),
    );

    expect(() =>
      planFaultProofFieldOpening({
        fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
        anchorTxId: txId,
        nativeTxCompactCbor: compactCbor,
        itemCbors: referenceInputItems,
        owner: OWNER,
        label: "test",
      }),
    ).toThrow(/the disputed transaction commits at §2\.5 field 0/u);
  });

  it("refuses a witness-set field with no witness set supplied", () => {
    const { txId, compactCbor } = fixture();
    expect(() =>
      planFaultProofFieldOpening({
        fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
        anchorTxId: txId,
        nativeTxCompactCbor: compactCbor,
        itemCbors: [],
        owner: OWNER,
        label: "test",
      }),
    ).toThrow(/compact witness set must be supplied/u);
  });

  it("refuses a witness set that is not the one the compact transaction names", () => {
    const { txId, compactCbor, nativeTx } = fixture();
    const honest = {
      addr_tx_wits_hash: midgardFieldCommitment(
        nativeTx.witnessSet.addrTxWitsPreimageCbor,
      ).toString("hex"),
      script_tx_wits_hash: midgardFieldCommitment(
        nativeTx.witnessSet.scriptTxWitsPreimageCbor,
      ).toString("hex"),
      redeemer_tx_wits_hash: midgardFieldCommitment(
        nativeTx.witnessSet.redeemerTxWitsPreimageCbor,
      ).toString("hex"),
    };
    const forged = { ...honest, script_tx_wits_hash: h32("99") };
    // The mutation landed.
    expect(forged.script_tx_wits_hash).not.toBe(honest.script_tx_wits_hash);

    expect(() =>
      planFaultProofFieldOpening({
        fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
        anchorTxId: txId,
        nativeTxCompactCbor: compactCbor,
        itemCbors: [],
        owner: OWNER,
        witnessSet: forged,
        label: "test",
      }),
    ).toThrow(/is not the .* the compact transaction commits/u);
  });
});
