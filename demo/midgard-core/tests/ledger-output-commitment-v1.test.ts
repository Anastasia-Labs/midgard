import { describe, expect, it } from "vitest";

import { buildMidgardBoundedItemChunkProofV1 } from "../src/bounded-item-v1.js";
import {
  buildMidgardLedgerOutputMaterialV1,
  decodeMidgardLedgerOutputCommitmentV1,
  encodeMidgardLedgerOutputCommitmentV1,
  verifyMidgardLedgerOutputChunkV1,
} from "../src/ledger-output-commitment-v1.js";

describe("ledger output commitment V1", () => {
  it("keeps the ledger leaf bounded while authenticating a multi-chunk output", () => {
    const material = buildMidgardLedgerOutputMaterialV1({
      outputIndex: 7,
      outputCbor: Buffer.alloc(5_000, 0x5a),
    });
    expect(material.descriptorCbor.toString("hex")).toBe(
      "840107191388582013e167684e9dc284acc6ebbe972cd2cf0763d03bba558bae463825f3f35990d6",
    );
    expect(material.descriptorCbor).toHaveLength(40);
    expect(
      encodeMidgardLedgerOutputCommitmentV1(
        decodeMidgardLedgerOutputCommitmentV1(material.descriptorCbor),
      ),
    ).toStrictEqual(material.descriptorCbor);
    for (let chunkIndex = 0; chunkIndex < 2; chunkIndex += 1) {
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
  });

  it("fails closed for output-index, length, and item substitution", () => {
    const material = buildMidgardLedgerOutputMaterialV1({
      outputIndex: 1,
      outputCbor: Buffer.alloc(5_000, 0x42),
    });
    const proof = buildMidgardBoundedItemChunkProofV1(material.item, 0);
    for (const descriptor of [
      { ...material.descriptor, outputIndex: 2 },
      { ...material.descriptor, totalLength: 4_999 },
      {
        ...material.descriptor,
        itemCommitment: Buffer.alloc(32, 0xff),
      },
    ]) {
      expect(
        verifyMidgardLedgerOutputChunkV1({ descriptor, proof }),
      ).toBe(false);
    }
  });
});
