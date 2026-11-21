import { describe, expect, it } from "vitest";

import * as codec from "../src/codec/native-tx-field-access.js";
import {
  buildMidgardWholeFieldView,
  decodeMidgardFieldArrayHeader,
  decodeMidgardFieldPreimage,
  deriveMidgardFieldPreimageCertificate,
  encodeMidgardDefiniteBytes,
  encodeMidgardFieldArrayHeader,
  encodeMidgardFieldPreimage,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_EMPTY_FIELD_COMMITMENT,
  midgardExpectedChunkCount,
  midgardFieldCommitment,
  midgardFieldCommitmentFromItems,
  midgardFieldItemAt,
  midgardFieldItemExtent,
  midgardFieldStride,
  splitMidgardFieldPreimageIntoChunks,
} from "../src/codec/native-tx-field-access.js";
import {
  buildNativeTxFieldAccessGoldenV1,
  repeatToLength,
} from "./fixtures/native-tx-field-access-v1.vectors.mjs";

/**
 * The TypeScript half of the cross-language golden channel. The vectors are
 * built here through `src/` from the same inputs the generator drives through
 * `dist/`, and each one is checked against a second entry of the twin (a
 * decoder against its encoder, a commitment over items against one over the
 * preimage, the §8.4 split against the certificate derivation); the generated
 * Aiken module (`onchain/aiken/lib/midgard/native-tx-field-access-v1-golden.test.ak`)
 * recomputes the same values with the Aiken producers under the fork runner, so
 * a divergence between the two fails on that side. Regenerate it with
 * `node scripts/generate-native-tx-field-access-v1-goldens.mjs`.
 */

const golden = buildNativeTxFieldAccessGoldenV1(codec);

const hex = (value: Uint8Array): string => Buffer.from(value).toString("hex");
const bytes = (value: string): Buffer => Buffer.from(value, "hex");

describe("native-tx field-access V1 cross-language goldens", () => {
  it("reproduces the §4 empty-field commitment", () => {
    expect(golden.emptyFieldCommitmentHex).toBe(
      hex(MIDGARD_EMPTY_FIELD_COMMITMENT),
    );
    expect(golden.emptyFieldCommitmentHex).toBe(
      hex(midgardFieldCommitment(bytes("80"))),
    );
  });

  it("reproduces every §5.1 array header, in both directions", () => {
    for (const entry of golden.arrayHeaders) {
      expect(hex(encodeMidgardFieldArrayHeader(entry.count))).toBe(
        entry.headerHex,
      );
      expect(decodeMidgardFieldArrayHeader(bytes(entry.headerHex))).toEqual({
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
      expect(hex(encodeMidgardDefiniteBytes(payload))).toBe(
        `${entry.wrapperHex}${vector!.itemsHex[0]}`,
      );
    }
  });

  it("reproduces every §5.1 preimage, its commitment and its item extents", () => {
    for (const vector of golden.preimages) {
      const items = vector.itemsHex.map(bytes);
      expect(items).toHaveLength(vector.itemCount);
      expect(hex(encodeMidgardFieldPreimage(items))).toBe(vector.preimageHex);
      expect(hex(midgardFieldCommitmentFromItems(items))).toBe(
        vector.commitmentHex,
      );
      expect(hex(midgardFieldCommitment(bytes(vector.preimageHex)))).toBe(
        vector.commitmentHex,
      );
      expect(midgardFieldStride(vector.fieldIndex)).toBe(vector.stride);
      expect(
        decodeMidgardFieldPreimage(bytes(vector.preimageHex)).map(hex),
      ).toEqual([...vector.itemsHex]);

      const view = buildMidgardWholeFieldView({
        fieldIndex: vector.fieldIndex,
        preimage: bytes(vector.preimageHex),
        expectedCommitment: bytes(vector.commitmentHex),
      });
      for (const [index, extent] of vector.itemExtents.entries()) {
        expect(midgardFieldItemExtent(view, index)).toEqual(extent);
        expect(hex(midgardFieldItemAt(view, index))).toBe(
          vector.itemsHex[index],
        );
      }
    }
  });

  it("reproduces the tier-3 manifest shape and its welded datum commitment", () => {
    const certificate = golden.tier3Certificate;
    expect(certificate.chunkLengths.reduce((a, b) => a + b, 0)).toBe(
      certificate.totalLength,
    );
    expect(certificate.chunkLengths).toHaveLength(
      midgardExpectedChunkCount(certificate.totalLength),
    );
    expect(certificate.chunkDigestsHex).toHaveLength(
      certificate.chunkLengths.length,
    );
    expect(certificate.totalLength).toBeGreaterThan(MIDGARD_CHUNK_BYTES_K);
    // The datum-shape identity that replaced the retired asset-name class
    // (#606): the welded `field_hash` is §4's commitment over the rebuilt
    // payload.
    expect(certificate.fieldHashHex).toBe(
      hex(
        midgardFieldCommitment(
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
    const chunks = splitMidgardFieldPreimageIntoChunks(preimage);
    expect(chunks.map((chunk) => chunk.length)).toEqual([
      ...certificate.chunkLengths,
    ]);
    expect(chunks.map((chunk) => hex(midgardFieldCommitment(chunk)))).toEqual([
      ...certificate.chunkDigestsHex,
    ]);

    // …and the same digests out of the publisher-facing derivation, so the
    // pinned manifest is what a real tier-3 publisher would mint.
    const derived = deriveMidgardFieldPreimageCertificate({
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
