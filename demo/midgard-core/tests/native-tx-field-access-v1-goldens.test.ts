import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  buildMidgardWholeFieldViewV1,
  decodeMidgardFieldArrayHeaderV1,
  decodeMidgardFieldPreimageV1,
  deriveMidgardFieldPreimageCertificateV1,
  encodeMidgardDefiniteBytesV1,
  encodeMidgardFieldArrayHeaderV1,
  encodeMidgardFieldPreimageV1,
  MIDGARD_ADDRESS_WITNESS_ITEM_BYTES_V1,
  MIDGARD_CHUNK_BYTES_K_V1,
  MIDGARD_EMPTY_FIELD_COMMITMENT_V1,
  MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS_V1,
  MIDGARD_FIELD_COUNT_V1,
  MIDGARD_FIELD_VIEW_CONSTRUCTORS_V1,
  MIDGARD_HASH28_ITEM_BYTES_V1,
  MIDGARD_MAX_FIELD_ITEM_COUNT_V1,
  MIDGARD_MAX_SPEND_INPUTS_PREIMAGE_BYTES_V1,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
  MIDGARD_MAX_TIER3_CHUNK_COUNT_V1,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1,
  MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT_V1,
  MIDGARD_SPEND_INPUT_ITEM_BYTES_V1,
  midgardExpectedChunkCountV1,
  midgardFieldCommitmentFromItemsV1,
  midgardFieldCommitmentV1,
  midgardFieldItemAtV1,
  midgardFieldItemExtentV1,
  MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_V1,
  midgardFieldStrideV1,
  splitMidgardFieldPreimageIntoChunksV1,
} from "../src/codec/native-tx-field-access-v1.js";

/**
 * The TypeScript half of the cross-language golden channel. Every value in the
 * generated fixture is recomputed here from the twin, so a drifting encoder
 * fails on this side; the generated Aiken module
 * (`onchain/aiken/lib/midgard/native-tx-field-access-v1-golden.test.ak`)
 * recomputes the same values with the Aiken producers under the fork runner, so
 * a divergence between the two fails on that side. Regenerate both with
 * `node scripts/generate-native-tx-field-access-v1-goldens.mjs`.
 */

type Golden = {
  readonly schema: string;
  readonly version: number;
  readonly specDocument: string;
  readonly generator: string;
  readonly constants: {
    readonly fieldCount: number;
    readonly maxFieldItemCount: number;
    readonly maxTransactionAggregateFieldBytes: number;
    readonly maxSpendInputsPreimageBytes: number;
    readonly maximumCardanoSpendRedeemerCount: number;
    readonly chunkBytesK: number;
    readonly maxTier1RedeemerPreimageBytes: number;
    readonly maxTier3ChunkCount: number;
    readonly spendInputItemBytes: number;
    readonly hash28ItemBytes: number;
    readonly addressWitnessItemBytes: number;
    readonly carriageConstructors: readonly string[];
    readonly viewConstructors: readonly string[];
  };
  readonly emptyFieldCommitmentHex: string;
  readonly strides: readonly number[];
  readonly arrayHeaders: readonly {
    readonly count: number;
    readonly headerHex: string;
    readonly headerLength: number;
  }[];
  readonly itemWrappers: readonly {
    readonly payloadLength: number;
    readonly wrapperHex: string;
  }[];
  readonly preimages: readonly {
    readonly label: string;
    readonly fieldIndex: number;
    readonly stride: number;
    readonly itemCount: number;
    readonly itemsHex: readonly string[];
    readonly preimageHex: string;
    readonly commitmentHex: string;
    readonly itemExtents: readonly {
      readonly offset: number;
      readonly length: number;
    }[];
  }[];
  readonly chunkCounts: readonly {
    readonly totalLength: number;
    readonly chunkCount: number;
  }[];
  readonly tier3Certificate: {
    readonly txIdHex: string;
    readonly ownerHex: string;
    readonly fieldIndex: number;
    readonly totalLength: number;
    readonly preimageBlockHex: string;
    readonly chunkLengths: readonly number[];
    readonly chunkDigestsHex: readonly string[];
    readonly assetNameHex: string;
  };
  readonly certificateAssetNames: readonly {
    readonly fieldIndex: number;
    readonly txIdHex: string;
    readonly assetNameHex: string;
  }[];
};

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "./fixtures/native-tx-field-access-v1.generated.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as Golden;

const hex = (value: Uint8Array): string => Buffer.from(value).toString("hex");
const bytes = (value: string): Buffer => Buffer.from(value, "hex");

/**
 * Expands the tier-3 vector's pinned 256-byte period to `length` bytes. The
 * generated Aiken module runs the same expansion over the same block, so the
 * §8.6 chunk digests are recomputed from rebuilt bytes on both sides rather
 * than pinned on one and length-checked on the other.
 */
const repeatToLength = (block: Buffer, length: number): Buffer => {
  const whole = Math.floor(length / block.length);
  return Buffer.concat([
    ...Array.from({ length: whole }, () => block),
    block.subarray(0, length - whole * block.length),
  ]);
};

describe("native-tx field-access V1 cross-language goldens", () => {
  it("declares the channel it belongs to", () => {
    expect(golden.schema).toBe("midgard-native-tx-field-access-v1-golden");
    expect(golden.version).toBe(1);
    expect(golden.specDocument).toBe("docs/spec/midgard-tx.md");
    expect(golden.generator).toBe(
      "demo/midgard-core/scripts/generate-native-tx-field-access-v1-goldens.mjs",
    );
  });

  it("reproduces the §5.3/§5.4/§8.3 constants and §8.8 constructor order", () => {
    // `toEqual`, not a per-key sweep: an exhaustive match is what makes a
    // twinned constant added to the module but forgotten here fail loudly
    // instead of silently sitting outside the channel.
    expect(golden.constants).toEqual({
      fieldCount: MIDGARD_FIELD_COUNT_V1,
      maxFieldItemCount: MIDGARD_MAX_FIELD_ITEM_COUNT_V1,
      maxTransactionAggregateFieldBytes:
        MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1,
      maxSpendInputsPreimageBytes: MIDGARD_MAX_SPEND_INPUTS_PREIMAGE_BYTES_V1,
      maximumCardanoSpendRedeemerCount:
        MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT_V1,
      chunkBytesK: MIDGARD_CHUNK_BYTES_K_V1,
      maxTier1RedeemerPreimageBytes:
        MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
      maxTier3ChunkCount: MIDGARD_MAX_TIER3_CHUNK_COUNT_V1,
      spendInputItemBytes: MIDGARD_SPEND_INPUT_ITEM_BYTES_V1,
      hash28ItemBytes: MIDGARD_HASH28_ITEM_BYTES_V1,
      addressWitnessItemBytes: MIDGARD_ADDRESS_WITNESS_ITEM_BYTES_V1,
      carriageConstructors: [...MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS_V1],
      viewConstructors: [...MIDGARD_FIELD_VIEW_CONSTRUCTORS_V1],
    });
  });

  it("reproduces the §4 empty-field commitment", () => {
    expect(golden.emptyFieldCommitmentHex).toBe(
      hex(MIDGARD_EMPTY_FIELD_COMMITMENT_V1),
    );
    expect(golden.emptyFieldCommitmentHex).toBe(
      hex(midgardFieldCommitmentV1(bytes("80"))),
    );
  });

  it("reproduces the §5.3 stride table", () => {
    expect(golden.strides).toEqual(
      Array.from({ length: MIDGARD_FIELD_COUNT_V1 }, (_, fieldIndex) =>
        midgardFieldStrideV1(fieldIndex),
      ),
    );
  });

  it("reproduces every §5.1 array header, in both directions", () => {
    for (const entry of golden.arrayHeaders) {
      expect(hex(encodeMidgardFieldArrayHeaderV1(entry.count))).toBe(
        entry.headerHex,
      );
      expect(decodeMidgardFieldArrayHeaderV1(bytes(entry.headerHex))).toEqual({
        nextOffset: entry.headerLength,
        count: entry.count,
      });
    }
  });

  it("reproduces every §5.1 item wrapper prefix", () => {
    for (const entry of golden.itemWrappers) {
      const vector = golden.preimages.find(
        (candidate) =>
          candidate.label === `single_item_payload_${entry.payloadLength}`,
      );
      expect(vector).toBeDefined();
      const payload = bytes(vector!.itemsHex[0]);
      expect(payload).toHaveLength(entry.payloadLength);
      expect(hex(encodeMidgardDefiniteBytesV1(payload))).toBe(
        `${entry.wrapperHex}${vector!.itemsHex[0]}`,
      );
    }
  });

  it("reproduces every §5.1 preimage, its commitment and its item extents", () => {
    for (const vector of golden.preimages) {
      const items = vector.itemsHex.map(bytes);
      expect(items).toHaveLength(vector.itemCount);
      expect(hex(encodeMidgardFieldPreimageV1(items))).toBe(vector.preimageHex);
      expect(hex(midgardFieldCommitmentFromItemsV1(items))).toBe(
        vector.commitmentHex,
      );
      expect(hex(midgardFieldCommitmentV1(bytes(vector.preimageHex)))).toBe(
        vector.commitmentHex,
      );
      expect(midgardFieldStrideV1(vector.fieldIndex)).toBe(vector.stride);
      expect(
        decodeMidgardFieldPreimageV1(bytes(vector.preimageHex)).map(hex),
      ).toEqual([...vector.itemsHex]);

      const view = buildMidgardWholeFieldViewV1({
        fieldIndex: vector.fieldIndex,
        preimage: bytes(vector.preimageHex),
        expectedCommitment: bytes(vector.commitmentHex),
      });
      for (const [index, extent] of vector.itemExtents.entries()) {
        expect(midgardFieldItemExtentV1(view, index)).toEqual(extent);
        expect(hex(midgardFieldItemAtV1(view, index))).toBe(
          vector.itemsHex[index],
        );
      }
    }
  });

  it("reproduces the §8.4 chunk counts", () => {
    for (const entry of golden.chunkCounts) {
      expect(midgardExpectedChunkCountV1(entry.totalLength)).toBe(
        entry.chunkCount,
      );
    }
  });

  it("reproduces the §8.6 constant certificate asset name (#606)", () => {
    expect(golden.certificateAssetName.assetNameHex).toBe(
      hex(MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_V1),
    );
    expect(golden.certificateAssetName.byteLength).toBe(
      MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_V1.length,
    );
    expect(golden.certificateAssetName.ascii).toBe(
      MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_V1.toString("ascii"),
    );
  });

  it("reproduces the tier-3 manifest shape and its welded datum commitment", () => {
    const certificate = golden.tier3Certificate;
    expect(certificate.chunkLengths.reduce((a, b) => a + b, 0)).toBe(
      certificate.totalLength,
    );
    expect(certificate.chunkLengths).toHaveLength(
      midgardExpectedChunkCountV1(certificate.totalLength),
    );
    expect(certificate.chunkDigestsHex).toHaveLength(
      certificate.chunkLengths.length,
    );
    expect(certificate.totalLength).toBeGreaterThan(MIDGARD_CHUNK_BYTES_K_V1);
    // The datum-shape identity that replaced the retired asset-name class
    // (#606): the welded `field_hash` is §4's commitment over the rebuilt
    // payload.
    expect(certificate.fieldHashHex).toBe(
      hex(
        midgardFieldCommitmentV1(
          repeatToLength(
            bytes(certificate.preimageBlockHex),
            certificate.totalLength,
          ),
        ),
      ),
    );
  });

  it("recomputes every §8.6 chunk digest from the rebuilt tier-3 payload", () => {
    const certificate = golden.tier3Certificate;
    const preimage = repeatToLength(
      bytes(certificate.preimageBlockHex),
      certificate.totalLength,
    );
    expect(preimage).toHaveLength(certificate.totalLength);

    // The §8.4 split, run by the twin rather than read out of the fixture.
    const chunks = splitMidgardFieldPreimageIntoChunksV1(preimage);
    expect(chunks.map((chunk) => chunk.length)).toEqual([
      ...certificate.chunkLengths,
    ]);
    expect(chunks.map((chunk) => hex(midgardFieldCommitmentV1(chunk)))).toEqual(
      [...certificate.chunkDigestsHex],
    );

    // …and the same digests out of the publisher-facing derivation, so the
    // pinned manifest is what a real tier-3 publisher would mint.
    const derived = deriveMidgardFieldPreimageCertificateV1({
      owner: bytes(certificate.ownerHex),
      txId: bytes(certificate.txIdHex),
      fieldIndex: certificate.fieldIndex,
      preimage,
    });
    expect(derived.totalLength).toBe(certificate.totalLength);
    expect(hex(derived.owner)).toBe(certificate.ownerHex);
    expect(derived.chunkDigests.map(hex)).toEqual([
      ...certificate.chunkDigestsHex,
    ]);
  });
});
