import "./native-tx-field-items-goldens.5-3-fixed-width-item-assertions.js";

import { describe, expect, it } from "vitest";

import {
  buildMidgardChunkedFieldView,
  deriveMidgardFieldPreimageCertificate,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT,
  midgardFieldItemAt,
  midgardFieldItemExtent,
  selectMidgardFieldCarriageTier,
} from "../src/codec/native-tx-field-access.js";
import {
  encodeMidgardFieldPreimageForField,
  midgardFieldCommitmentForField,
} from "../src/codec/native-tx-field-items.js";
import * as vectors from "./fixtures/native-tx-field-items-v1.vectors.mjs";
import {
  golden,
  hex,
} from "./native-tx-field-items-goldens.5-3-per-field-item-encodings-recompute-from-the-twin.js";

describe("§8.4 tier-3 straddle at the stride fields 0/1 share", () => {
  it("reads an item that crosses the chunk boundary", () => {
    const straddle = golden.straddle;
    // §5.4 reachability. Fields 0 and 1 share an item encoder and stride, so
    // the bytes below would be identical at either — but field 0's cardinality
    // is capped by the Cardano shape bound at 296 spend inputs, whose maximal
    // preimage still selects tier 1. Pinning the straddle at field 0 would pin
    // a carriage the format never admits, so the vector lives at field 1 and
    // this assertion is what keeps it there.
    expect(straddle.fieldIndex).toBe(1);
    expect(vectors.STRADDLE_FIELD_INDEX).toBe(straddle.fieldIndex);
    const maximalField0Preimage =
      Buffer.from(straddle.headerHex, "hex").length +
      straddle.stride * MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT;
    expect(selectMidgardFieldCarriageTier(maximalField0Preimage)).toBe(
      "Inline",
    );
    // …and the cardinality this vector does use is inside field 1's byte bound.
    expect(straddle.itemCount).toBeLessThanOrEqual(
      Math.floor(
        (MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES -
          Buffer.from(straddle.headerHex, "hex").length) /
          straddle.stride,
      ),
    );
    const selector = {
      fieldIndex: vectors.STRADDLE_FIELD_INDEX,
      items: vectors.straddleInputs(),
    };
    const preimage = encodeMidgardFieldPreimageForField(selector);
    const commitment = midgardFieldCommitmentForField(selector);

    expect(preimage.length).toBe(straddle.totalLength);
    expect(hex(commitment)).toBe(straddle.commitmentHex);
    expect(straddle.carriageTier).toBe("Certified");
    expect(selectMidgardFieldCarriageTier(preimage.length)).toBe("Certified");

    // The compaction is lossless: header ‖ block×repeats rebuilds the preimage.
    const rebuilt = Buffer.concat([
      Buffer.from(straddle.headerHex, "hex"),
      ...Array.from({ length: straddle.repeats }, () =>
        Buffer.from(straddle.blockHex, "hex"),
      ),
    ]);
    expect(hex(rebuilt)).toBe(hex(preimage));

    const certificate = deriveMidgardFieldPreimageCertificate({
      owner: vectors.STRADDLE_OWNER,
      txId: vectors.STRADDLE_TX_ID,
      fieldIndex: vectors.STRADDLE_FIELD_INDEX,
      preimage,
    });
    expect(certificate.chunkDigests.map(hex)).toEqual(straddle.chunkDigestsHex);

    const chunks: Buffer[] = [];
    for (
      let start = 0;
      start < preimage.length;
      start += MIDGARD_CHUNK_BYTES_K
    ) {
      chunks.push(
        preimage.subarray(
          start,
          Math.min(start + MIDGARD_CHUNK_BYTES_K, preimage.length),
        ),
      );
    }
    expect(chunks.map((chunk) => chunk.length)).toEqual(straddle.chunkLengths);

    const view = buildMidgardChunkedFieldView({
      fieldIndex: vectors.STRADDLE_FIELD_INDEX,
      txId: vectors.STRADDLE_TX_ID,
      certificate,
      chunks,
      expectedCommitment: commitment,
    });

    // Exactly one of the three reads straddles, and it is the middle one —
    // its neighbours carry different bytes, so an off-by-one is visible.
    expect(straddle.reads.filter((read) => read.straddles).length).toBe(1);
    for (const read of straddle.reads) {
      expect(midgardFieldItemExtent(view, read.itemIndex)).toEqual({
        offset: read.offset,
        length: read.length,
      });
      expect(hex(midgardFieldItemAt(view, read.itemIndex))).toBe(read.itemHex);
      const crosses =
        Math.floor(read.offset / MIDGARD_CHUNK_BYTES_K) !==
        Math.floor((read.offset + read.length - 1) / MIDGARD_CHUNK_BYTES_K);
      expect(crosses).toBe(read.straddles);
    }
    const straddling = straddle.reads.find((read) => read.straddles);
    expect(straddling?.itemHex).not.toBe(
      straddle.reads.find(
        (read) => read.itemIndex === straddling!.itemIndex - 1,
      )?.itemHex,
    );

    // §7.3 again, on the chunked branch.
    expect(() => midgardFieldItemExtent(view, straddle.itemCount)).toThrow();
  });
});
