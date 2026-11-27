import {
  buildMidgardBoundedItem,
  decodeMidgardLedgerOutputCommitment,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
} from "@al-ft/midgard-core";
import {
  encodeMidgardNativeScript,
  encodeMidgardTxOutput,
  type MidgardNativeScript,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  buildCanonicalMidgardLedgerOutputMaterial,
  buildOpaqueNativeReferenceScriptLedgerOutputMaterial,
} from "../src/ledger-output-descriptor.js";
import { commitMidgardScriptContextTxOut } from "../src/script-context-proof.js";
import { outputFixture } from "./ledger-output-descriptor.output-fixture.js";

const withoutScript = (withDatum: boolean): MidgardTxOutput => {
  const { output } = outputFixture();
  return {
    address: output.address,
    value: output.value,
    ...(withDatum ? { datum: output.datum! } : {}),
  };
};

/** The output's bytes with `item` appended as its reference script. */
const withScriptItem = (output: MidgardTxOutput, item: Buffer): Buffer => {
  const bytes = encodeMidgardTxOutput(output);
  return Buffer.concat([
    Buffer.from([bytes[0]! + 1]),
    bytes.subarray(1),
    Buffer.from([0x03]),
    item,
  ]);
};

const nativeScript: MidgardNativeScript = {
  type: "all",
  scripts: [{ type: "sig", keyHash: Buffer.alloc(28, 7) }],
};

describe("opaque native reference script ledger descriptor", () => {
  it.each([true, false])(
    "equals the canonical descriptor for a native script that decodes (datum %s)",
    (withDatum) => {
      const scriptBytes = encodeMidgardNativeScript(nativeScript);
      const outputCbor = encodeMidgardTxOutput({
        ...withoutScript(withDatum),
        script_ref: { language: "NativeCardano", scriptBytes, nativeScript },
      });
      const canonical = buildCanonicalMidgardLedgerOutputMaterial({
        outputIndex: 3,
        outputCbor,
      });
      const opaque = buildOpaqueNativeReferenceScriptLedgerOutputMaterial({
        outputIndex: 3,
        outputCbor,
      });
      expect(opaque.descriptorCbor.toString("hex")).toBe(
        canonical.descriptorCbor.toString("hex"),
      );
    },
  );

  it("derives the descriptor of an output whose native script does not decode", () => {
    // [0, h'820700']: a native-script envelope whose payload is no native script.
    const item = Buffer.from("820043820700", "hex");
    const rest = withoutScript(true);
    const outputCbor = withScriptItem(rest, item);
    expect(() =>
      buildCanonicalMidgardLedgerOutputMaterial({ outputIndex: 1, outputCbor }),
    ).toThrow();
    const descriptor = decodeMidgardLedgerOutputCommitment(
      buildOpaqueNativeReferenceScriptLedgerOutputMaterial({
        outputIndex: 1,
        outputCbor,
      }).descriptorCbor,
    );
    const hash = Buffer.from(
      blake2b(Buffer.from("00820700", "hex"), { dkLen: 28 }),
    );
    const restDescriptor = decodeMidgardLedgerOutputCommitment(
      buildCanonicalMidgardLedgerOutputMaterial({
        outputIndex: 1,
        outputCbor: encodeMidgardTxOutput(rest),
      }).descriptorCbor,
    );
    const summary = (encoding: "cardano" | "midgard") => {
      const { root, cborLength, memory } = commitMidgardScriptContextTxOut(
        rest,
        encoding,
        hash.toString("hex"),
      );
      return { root: Buffer.from(root), cborLength, memory };
    };
    expect(descriptor).toMatchObject({
      outputIndex: 1,
      totalLength: outputCbor.length,
      itemCommitment: buildMidgardBoundedItem({
        fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
        itemIndex: 1,
        bytes: outputCbor,
      }).commitment,
      address: restDescriptor.address,
      lovelace: restDescriptor.lovelace,
      assetCount: restDescriptor.assetCount,
      assetFrontierCommitment: restDescriptor.assetFrontierCommitment,
      cardanoValueSize: restDescriptor.cardanoValueSize,
      cardanoSpendDatum: restDescriptor.cardanoSpendDatum,
      referenceScriptLanguage: 0,
      referenceScriptHash: hash,
      referenceScriptTotalLength: item.length,
      referenceScriptItemCommitment: buildMidgardBoundedItem({
        fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
        itemIndex: 1,
        bytes: item,
      }).commitment,
    });
    expect(descriptor.cardanoTxOut).toEqual(summary("cardano"));
    expect(descriptor.midgardTxOut).toEqual(summary("midgard"));
  });

  it("refuses an output without a native reference script", () => {
    for (const outputCbor of [
      encodeMidgardTxOutput(withoutScript(true)),
      outputFixture().cbor,
    ])
      expect(() =>
        buildOpaqueNativeReferenceScriptLedgerOutputMaterial({
          outputIndex: 0,
          outputCbor,
        }),
      ).toThrow();
  });
});
