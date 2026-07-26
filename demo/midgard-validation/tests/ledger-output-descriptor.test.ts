import {
  buildMidgardBoundedItemChunkProofV1,
  buildMidgardBoundedItemV1,
  decodeMidgardLedgerOutputCommitmentV1,
  encodeMidgardLedgerOutputCommitmentV1,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
  verifyMidgardLedgerOutputChunkV1,
  verifyMidgardLedgerOutputReferenceScriptChunkV1,
} from "@al-ft/midgard-core";
import {
  decodeMidgardDatum,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildCanonicalMidgardLedgerOutputMaterialV1,
} from "../src/ledger-output-descriptor.js";

const protectedScriptAddress = Buffer.concat([
  Buffer.from([0x78]),
  Buffer.alloc(28, 0x11),
]);

const outputFixture = (): {
  readonly output: MidgardTxOutput;
  readonly cbor: Buffer;
} => {
  const output: MidgardTxOutput = {
    address: protectedScriptAddress,
    value: {
      lovelace: 8_000_000n,
      assets: new Map([
        [
          "55".repeat(28),
          new Map([
            ["", 1n],
            ["0102", 42n],
          ]),
        ],
      ]),
    },
    datum: decodeMidgardDatum(
      Buffer.from(Data.to("ab".repeat(5_000)), "hex"),
    ),
    script_ref: {
      language: "PlutusV3",
      scriptBytes: Buffer.alloc(100, 0x6b),
    },
  };
  return { output, cbor: encodeMidgardTxOutput(output) };
};

describe("canonical ledger output descriptor V1", () => {
  it("derives every compact fact from a multi-chunk canonical output", () => {
    const fixture = outputFixture();
    const material = buildCanonicalMidgardLedgerOutputMaterialV1({
      outputIndex: 7,
      outputCbor: fixture.cbor,
    });
    const repeated = buildCanonicalMidgardLedgerOutputMaterialV1({
      outputIndex: 7,
      outputCbor: fixture.cbor,
    });

    expect(material.item.bytes.length).toBeGreaterThan(4_095);
    expect(material.item.bytes.length).toBeLessThan(16_384);
    expect(material.descriptor.cardanoValueSize).toBeLessThanOrEqual(5_000);
    expect(material.descriptorCbor).toStrictEqual(repeated.descriptorCbor);
    expect(material.descriptor.assetCount).toBe(2);
    expect(material.descriptor.address).toStrictEqual(
      protectedScriptAddress,
    );
    expect(material.descriptor.referenceScriptLanguage).toBe(3);
    expect(material.descriptor.referenceScriptHash).toHaveLength(28);
    expect(material.descriptor.referenceScriptItemCommitment).toHaveLength(
      32,
    );
    expect(material.descriptor.cardanoTxOut.root).not.toStrictEqual(
      material.descriptor.midgardTxOut.root,
    );
    expect(
      encodeMidgardLedgerOutputCommitmentV1(
        decodeMidgardLedgerOutputCommitmentV1(material.descriptorCbor),
      ),
    ).toStrictEqual(material.descriptorCbor);

    for (
      let chunkIndex = 0;
      chunkIndex < material.item.frontier.count;
      chunkIndex += 1
    ) {
      expect(
        verifyMidgardLedgerOutputChunkV1({
          descriptor: material.descriptor,
          proof: buildMidgardBoundedItemChunkProofV1(
            material.item,
            chunkIndex,
          ),
        }),
      ).toBe(true);
    }

    const scriptCbor = encodeMidgardVersionedScript(
      fixture.output.script_ref!,
    );
    const scriptItem = buildMidgardBoundedItemV1({
      fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX_V1,
      itemIndex: 7,
      bytes: scriptCbor,
    });
    expect(scriptItem.commitment).toStrictEqual(
      material.descriptor.referenceScriptItemCommitment,
    );
    for (
      let chunkIndex = 0;
      chunkIndex < scriptItem.frontier.count;
      chunkIndex += 1
    ) {
      expect(
        verifyMidgardLedgerOutputReferenceScriptChunkV1({
          descriptor: material.descriptor,
          proof: buildMidgardBoundedItemChunkProofV1(
            scriptItem,
            chunkIndex,
          ),
        }),
      ).toBe(true);
    }
  });
});
