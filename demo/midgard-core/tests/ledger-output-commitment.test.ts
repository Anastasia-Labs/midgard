import { describe, expect, it } from "vitest";

import {
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
} from "../src/bounded-item.js";
import {
  buildMidgardLedgerOutputAssetFrontier,
  buildMidgardLedgerOutputMaterial,
  decodeMidgardLedgerOutputCommitment,
  encodeMidgardLedgerOutputCommitment,
  MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
  type MidgardLedgerOutputCommitmentFacts,
  verifyMidgardLedgerOutputChunk,
  verifyMidgardLedgerOutputReferenceScriptChunk,
} from "../src/ledger-output-commitment.js";

const facts = (): MidgardLedgerOutputCommitmentFacts => ({
  address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x11)]),
  lovelace: 5_000_000n,
  assetCount: 0,
  assetFrontierCommitment: buildMidgardLedgerOutputAssetFrontier([]).commitment,
  cardanoValueSize: 5,
  referenceScriptLanguage: -1,
  referenceScriptHash: Buffer.alloc(0),
  referenceScriptTotalLength: 0,
  referenceScriptItemCommitment: Buffer.alloc(0),
  cardanoTxOut: {
    root: Buffer.alloc(32, 0x22),
    cborLength: 101n,
    memory: 202n,
  },
  midgardTxOut: {
    root: Buffer.alloc(32, 0x33),
    cborLength: 103n,
    memory: 204n,
  },
  cardanoSpendDatum: {
    root: Buffer.alloc(32, 0x44),
    cborLength: 3n,
    memory: 4n,
  },
});

describe("ledger output commitment V1", () => {
  it("commits ordered assets without imposing a separate count cap", () => {
    const assets = buildMidgardLedgerOutputAssetFrontier([
      {
        policyId: Buffer.alloc(28, 0x55),
        assetName: Buffer.from("03", "hex"),
        quantity: 7n,
      },
      {
        policyId: Buffer.alloc(28, 0x55),
        assetName: Buffer.from("0102", "hex"),
        quantity: 42n,
      },
    ]);
    expect(assets.leaves.map((leaf) => leaf.toString("hex"))).toStrictEqual([
      "d4fab6956ed316d6c8094e2f8ba31c07f9b8e11d6b9ed1236541a102187a58ce",
      "9ed4bad2bc3c66d009d021f8fa57eb32d6035c71aa6f1c415e8f66bf35c6661c",
    ]);
    expect(assets.commitment.toString("hex")).toBe(
      "1af1c3eeb6379f3bb3dac82d19faffa1318a2a64f739b38c5fc5640b58196cc5",
    );
  });

  it("keeps the ledger leaf bounded while authenticating a multi-chunk output", () => {
    const material = buildMidgardLedgerOutputMaterial({
      outputIndex: 7,
      outputCbor: Buffer.alloc(5_000, 0x5a),
      facts: facts(),
    });
    expect(material.descriptorCbor.toString("hex")).toBe(
      "900107191388582013e167684e9dc284acc6ebbe972cd2cf0763d03bba558bae463825f3f35990d6581d60111111111111111111111111111111111111111111111111111111111a004c4b40005820b6575c6c81264fc5d6802905bc4cb01d26fcca7c75412712fd4d4b7e5a23d6cd05204000408358202222222222222222222222222222222222222222222222222222222222222222186518ca8358203333333333333333333333333333333333333333333333333333333333333333186718cc83582044444444444444444444444444444444444444444444444444444444444444440304",
    );
    expect(material.descriptorCbor.length).toBeLessThan(512);
    expect(
      encodeMidgardLedgerOutputCommitment(
        decodeMidgardLedgerOutputCommitment(material.descriptorCbor),
      ),
    ).toStrictEqual(material.descriptorCbor);
    for (let chunkIndex = 0; chunkIndex < 2; chunkIndex += 1) {
      expect(
        verifyMidgardLedgerOutputChunk({
          descriptor: material.descriptor,
          proof: buildMidgardBoundedItemChunkProof(material.item, chunkIndex),
        }),
      ).toBe(true);
    }
  });

  it("admits the exact Cardano Value byte bound and fails closed above it", () => {
    const atBound = buildMidgardLedgerOutputMaterial({
      outputIndex: 7,
      outputCbor: Buffer.alloc(16_384, 0x5a),
      facts: {
        ...facts(),
        cardanoValueSize: 5_000,
      },
    });
    expect(
      decodeMidgardLedgerOutputCommitment(atBound.descriptorCbor)
        .cardanoValueSize,
    ).toBe(5_000);
    expect(() =>
      buildMidgardLedgerOutputMaterial({
        outputIndex: 7,
        outputCbor: Buffer.alloc(16_384, 0x5a),
        facts: {
          ...facts(),
          cardanoValueSize: 5_001,
        },
      }),
    ).toThrow(/5,000-byte mainnet bound/);
  });

  it("uses a transaction-size-derived asset guardrail rather than 128", () => {
    expect(() =>
      buildMidgardLedgerOutputMaterial({
        outputIndex: 7,
        outputCbor: Buffer.alloc(16_384, 0x5a),
        facts: {
          ...facts(),
          assetCount: 16_385,
        },
      }),
    ).toThrow(/Cardano-size-derived proof envelope/);
  });

  it("authenticates every chunk of a reference script independently", () => {
    const referenceScript = buildMidgardBoundedItem({
      fieldIndex: MIDGARD_LEDGER_OUTPUT_FIELD_INDEX,
      itemIndex: 7,
      bytes: Buffer.alloc(5_000, 0x6b),
    });
    const material = buildMidgardLedgerOutputMaterial({
      outputIndex: 7,
      outputCbor: Buffer.alloc(8_500, 0x5a),
      facts: {
        ...facts(),
        referenceScriptLanguage: 3,
        referenceScriptHash: Buffer.alloc(28, 0x77),
        referenceScriptTotalLength: referenceScript.bytes.length,
        referenceScriptItemCommitment: referenceScript.commitment,
      },
    });
    for (
      let chunkIndex = 0;
      chunkIndex < referenceScript.frontier.count;
      chunkIndex += 1
    ) {
      expect(
        verifyMidgardLedgerOutputReferenceScriptChunk({
          descriptor: material.descriptor,
          proof: buildMidgardBoundedItemChunkProof(referenceScript, chunkIndex),
        }),
      ).toBe(true);
    }
  });

  it("fails closed for output-index, length, and item substitution", () => {
    const material = buildMidgardLedgerOutputMaterial({
      outputIndex: 1,
      outputCbor: Buffer.alloc(5_000, 0x42),
      facts: facts(),
    });
    const proof = buildMidgardBoundedItemChunkProof(material.item, 0);
    for (const descriptor of [
      { ...material.descriptor, outputIndex: 2 },
      { ...material.descriptor, totalLength: 4_999 },
      {
        ...material.descriptor,
        itemCommitment: Buffer.alloc(32, 0xff),
      },
    ]) {
      expect(verifyMidgardLedgerOutputChunk({ descriptor, proof })).toBe(false);
    }
  });
});
