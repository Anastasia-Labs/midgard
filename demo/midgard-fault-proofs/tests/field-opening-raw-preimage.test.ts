import {
  computeHash32,
  computeMidgardNativeTxId,
  encodeMidgardFieldArrayHeader,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { encodeMidgardForcedTxCompact } from "@al-ft/midgard-core/codec/forced";
import { fieldPreimagePublicationDatumCbor } from "@al-ft/midgard-sdk";
import { credentialToAddress, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
} from "../src/field-opening.js";
import { h32, makeNativeTx } from "./support/submit-init-emulator-shared.js";

const OWNER = "ab".repeat(28);
const inputItem = Buffer.concat([
  Buffer.from("5826825820", "hex"),
  Buffer.alloc(32, 0x41),
  Buffer.from("190000", "hex"),
]);
const malformedInput = Buffer.concat([
  Buffer.from("ff26", "hex"),
  inputItem.subarray(2),
]);

const source = (preimage: Buffer, sourceKind: 0n | 1n = 0n) => {
  const tx = makeNativeTx({
    spendInputCbors: [Buffer.from(h32("41"), "hex")],
    fee: 7n,
    referenceByte: "42",
    outputByte: "43",
    witnessByte: "44",
  });
  const compact = {
    ...tx.compact,
    transactionBody: {
      ...tx.compact.transactionBody,
      spendInputsHash: midgardFieldCommitment(preimage),
    },
  };
  return {
    tx,
    compact,
    anchorSourceKind: sourceKind,
    fieldIndex: 0,
    anchorTxId: computeMidgardNativeTxId(compact).toString("hex"),
    nativeTxCompactCbor: (sourceKind === 1n
      ? encodeMidgardForcedTxCompact
      : encodeMidgardNativeTxCompact)(compact).toString("hex"),
    fieldPreimage: preimage,
    owner: OWNER,
    label: "T235 exact raw field",
  };
};

describe("T235 exact raw field-opening planner", () => {
  for (const sourceKind of [0n, 1n] as const) {
    it(`preserves a committed malformed fixed wrapper for source ${sourceKind.toString()}`, () => {
      const preimage = Buffer.concat([Buffer.from([0x81]), malformedInput]);
      const input = source(preimage, sourceKind);
      const planned = planFaultProofFieldOpening(input);
      expect(planned.preimage).toEqual(preimage);
      expect(planned.itemCount).toBe(1);
      expect(planned.commitment).toBe(
        midgardFieldCommitment(preimage).toString("hex"),
      );
      expect(faultProofFieldOpening({ planned, label: input.label })).toEqual({
        BodyFieldOpening: {
          native_tx_compact_cbor: input.nativeTxCompactCbor,
          carriage: { Inline: { preimage: preimage.toString("hex") } },
        },
      });
    });
  }

  it("refuses substituted prover bytes under the original source commitment", () => {
    const preimage = Buffer.concat([Buffer.from([0x81]), inputItem]);
    const input = source(preimage);
    const substituted = Buffer.concat([Buffer.from([0x81]), malformedInput]);
    expect(midgardFieldCommitment(substituted)).not.toEqual(
      midgardFieldCommitment(preimage),
    );
    expect(() =>
      planFaultProofFieldOpening({ ...input, fieldPreimage: substituted }),
    ).toThrow(/the disputed transaction commits at §2\.5 field 0/u);
  });

  it("refuses a fixed width/count mismatch even when the source commits it", () => {
    const preimage = Buffer.concat([
      Buffer.from([0x81]),
      inputItem.subarray(0, inputItem.length - 1),
    ]);
    expect(() => planFaultProofFieldOpening(source(preimage))).toThrow(
      /count and stride disagree/u,
    );
  });

  it("refuses a mismatched declared count rather than inventing item extents", () => {
    const preimage = Buffer.concat([Buffer.from([0x82]), inputItem]);
    expect(() => planFaultProofFieldOpening(source(preimage))).toThrow(
      /count and stride disagree/u,
    );
  });

  it("preserves all certified-tier raw chunk bytes", () => {
    const count = 800;
    const preimage = Buffer.concat([
      encodeMidgardFieldArrayHeader(count),
      ...Array.from({ length: count }, () => malformedInput),
    ]);
    const planned = planFaultProofFieldOpening(source(preimage));
    expect(planned.itemCount).toBe(count);
    expect(planned.plan.tier).toBe("Certified");
    expect(
      Buffer.concat(planned.plan.publications.map(({ bytes }) => bytes)),
    ).toEqual(preimage);
    expect(planned.plan.commitment).toEqual(midgardFieldCommitment(preimage));
  });

  it("resolves raw carriage against the actual reference-input positions", () => {
    const preimage = Buffer.concat([Buffer.from([0x81]), malformedInput]);
    const planned = planFaultProofFieldOpening({
      ...source(preimage),
      publish: true,
    });
    const address = credentialToAddress("Preprod", {
      type: "Key",
      hash: OWNER,
    });
    const publication: UTxO = {
      txHash: "11".repeat(32),
      outputIndex: 0,
      address,
      assets: { lovelace: 2_000_000n },
      datum: fieldPreimagePublicationDatumCbor(preimage),
    };
    const unrelated: UTxO = {
      ...publication,
      txHash: "00".repeat(32),
      datum: fieldPreimagePublicationDatumCbor(Buffer.from("80", "hex")),
    };
    expect(
      faultProofFieldOpening({
        planned,
        referenceInputs: [publication, unrelated],
        label: "T235 actual reference positions",
      }),
    ).toEqual({
      BodyFieldOpening: {
        native_tx_compact_cbor: planned.nativeTxCompactCbor,
        carriage: { RawUtxo: { ref_input_index: 1n } },
      },
    });
    expect(() =>
      faultProofFieldOpening({
        planned,
        referenceInputs: [unrelated],
        label: "T235 absent raw publication",
      }),
    ).toThrow();
  });

  it("retains strict variable-field envelope validation", () => {
    const preimage = Buffer.from("81ff00", "hex");
    const input = source(preimage);
    const compact = {
      ...input.compact,
      transactionBody: {
        ...input.compact.transactionBody,
        outputsHash: midgardFieldCommitment(preimage),
      },
    };
    expect(() =>
      planFaultProofFieldOpening({
        ...input,
        fieldIndex: 2,
        anchorTxId: computeMidgardNativeTxId(compact).toString("hex"),
        nativeTxCompactCbor:
          encodeMidgardNativeTxCompact(compact).toString("hex"),
      }),
    ).toThrow(/definite byte-string header/u);
  });

  it("authenticates the witness hash outside the body-only transaction id", () => {
    const witnessItem = Buffer.concat([
      Buffer.from("ff65825820", "hex"),
      Buffer.alloc(32, 0x41),
      Buffer.from("5840", "hex"),
      Buffer.alloc(64, 0x42),
    ]);
    const preimage = Buffer.concat([Buffer.from([0x81]), witnessItem]);
    const input = source(encodeMidgardFieldPreimage([]));
    const witnessSet = {
      addr_tx_wits_hash: midgardFieldCommitment(preimage).toString("hex"),
      script_tx_wits_hash: midgardFieldCommitment(
        input.tx.witnessSet.scriptTxWitsPreimageCbor,
      ).toString("hex"),
      redeemer_tx_wits_hash: midgardFieldCommitment(
        input.tx.witnessSet.redeemerTxWitsPreimageCbor,
      ).toString("hex"),
    };
    const witnessHash = computeHash32(
      encodeMidgardNativeTxWitnessSetCompact({
        addrTxWitsHash: Buffer.from(witnessSet.addr_tx_wits_hash, "hex"),
        scriptTxWitsHash: Buffer.from(witnessSet.script_tx_wits_hash, "hex"),
        redeemerTxWitsHash: Buffer.from(
          witnessSet.redeemer_tx_wits_hash,
          "hex",
        ),
      }),
    );
    const compact = {
      ...input.compact,
      transactionWitnessSetHash: witnessHash,
    };
    expect(computeMidgardNativeTxId(compact).toString("hex")).toBe(
      input.anchorTxId,
    );
    const args = {
      ...input,
      fieldIndex: 7,
      fieldPreimage: preimage,
      nativeTxCompactCbor:
        encodeMidgardNativeTxCompact(compact).toString("hex"),
      witnessSet,
      anchorWitnessSetHash: witnessHash.toString("hex"),
    };
    expect(planFaultProofFieldOpening(args).preimage).toEqual(preimage);
    expect(() =>
      planFaultProofFieldOpening({
        ...args,
        anchorWitnessSetHash: "ff".repeat(32),
      }),
    ).toThrow(/the thread anchored/u);
  });
});
