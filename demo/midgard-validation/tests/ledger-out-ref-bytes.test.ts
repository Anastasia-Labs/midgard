import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { decodeMidgardOutRefBytes } from "../src/ledger-tx/codec.js";

/**
 * `docs/spec/midgard-tx.md` §5.3: an out-ref has exactly one byte form, the
 * field-0/1 item `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a fixed 38 bytes, and
 * that same value is the ledger MPF trie key and the ledger database `outref`
 * column.
 *
 * `decodeMidgardOutRefBytes` is the read side of that single spelling, so what
 * needs proving is not only that it decodes §5.3 bytes but that it *refuses*
 * every other shape. A tolerated second shape would be worse than dead code:
 * the retired 36-byte canonical-Cardano key would decode here and re-encode to
 * different 38 bytes, silently re-keying a stale row instead of failing. A
 * development ledger still holding those bytes must be reset, not migrated.
 */
const TX_ID = Buffer.alloc(32, 0x42);

const fixedIndexOutRef = (index: number): Buffer =>
  Buffer.concat([
    Buffer.from([0x82, 0x58, 0x20]),
    TX_ID,
    Buffer.from([0x19, (index >> 8) & 0xff, index & 0xff]),
  ]);

describe("decodeMidgardOutRefBytes (§5.3 field-0/1 item)", () => {
  it.each([0, 7, 23, 24, 255, 256, 65_535])(
    "decodes the fixed-index form at output index %i",
    (index) => {
      const bytes = fixedIndexOutRef(index);
      expect(bytes.length).toBe(38);
      const decoded = decodeMidgardOutRefBytes(bytes);
      expect(decoded.txId.toString("hex")).toBe(TX_ID.toString("hex"));
      expect(decoded.index).toBe(BigInt(index));
    },
  );

  it("rejects the retired canonical Cardano TransactionInput spelling", () => {
    // The exact bytes a pre-reset development ledger would still hold, taken
    // from CML rather than written down, so this stays the real retired form.
    //
    // Only indices below 256 are listed, and that is the whole reason §5.3
    // needs a fixed index at all: from 256 up, minimal CBOR already spells the
    // index `19 XXXX`, so the two encodings coincide and there is nothing to
    // reject. The divergence — and every stale 36/37-byte ledger key — lives
    // entirely below 256.
    for (const index of [0n, 23n, 24n, 255n]) {
      const hash = CML.TransactionHash.from_raw_bytes(TX_ID);
      const input = CML.TransactionInput.new(hash, index);
      try {
        const bytes = Buffer.from(input.to_cbor_bytes());
        expect(bytes.length).toBeLessThan(38);
        expect(() => decodeMidgardOutRefBytes(bytes)).toThrow();
      } finally {
        input.free();
        hash.free();
      }
    }
  });

  it("agrees with the minimal spelling from index 256 up", () => {
    // The other half of the boundary, stated so the rejection above cannot be
    // misread as "CML bytes never decode": at 256 and beyond minimal CBOR
    // already chooses `19 XXXX`, so the retired producer and this one emit the
    // same 38 bytes and there is exactly one spelling either way.
    for (const index of [256n, 65_535n]) {
      const hash = CML.TransactionHash.from_raw_bytes(TX_ID);
      const input = CML.TransactionInput.new(hash, index);
      try {
        const bytes = Buffer.from(input.to_cbor_bytes());
        expect(bytes).toEqual(fixedIndexOutRef(Number(index)));
        expect(decodeMidgardOutRefBytes(bytes).index).toBe(index);
      } finally {
        input.free();
        hash.free();
      }
    }
  });

  it("rejects a non-minimal byte-string header for tx_id", () => {
    // `59 0020` is a second, wider spelling of "32 bytes"; on-chain
    // `decode_definite_bytes_at` will read it, so the width guard is what makes
    // the two decoders agree that this is not an out-ref.
    const wideHeader = Buffer.concat([
      Buffer.from([0x82, 0x59, 0x00, 0x20]),
      TX_ID,
      Buffer.from([0x19, 0x00, 0x07]),
    ]);
    expect(wideHeader.length).toBe(39);
    expect(() => decodeMidgardOutRefBytes(wideHeader)).toThrow();
  });

  it("rejects wider and non-19 output-index heads", () => {
    const oneByteHead = Buffer.concat([
      Buffer.from([0x82, 0x58, 0x20]),
      TX_ID,
      Buffer.from([0x18, 0x07]),
    ]);
    const fourByteHead = Buffer.concat([
      Buffer.from([0x82, 0x58, 0x20]),
      TX_ID,
      Buffer.from([0x1a, 0x00, 0x00, 0x00, 0x07]),
    ]);
    for (const bytes of [oneByteHead, fourByteHead]) {
      expect(() => decodeMidgardOutRefBytes(bytes)).toThrow();
    }
  });

  it("rejects malformed, truncated, trailing and out-of-shape bytes", () => {
    const canonical = fixedIndexOutRef(7);
    const malformed = [
      Buffer.alloc(0),
      encodeCbor([TX_ID]),
      encodeCbor([TX_ID, 0n, 1n]),
      encodeCbor([Buffer.alloc(31, 0x42), 0n]),
      canonical.subarray(0, canonical.length - 1),
      Buffer.concat([canonical, Buffer.from([0x00])]),
      Buffer.concat([
        Buffer.from([0x9f, 0x58, 0x20]),
        TX_ID,
        Buffer.from([0x19, 0x00, 0x07, 0xff]),
      ]),
      Buffer.concat([
        Buffer.from([0x83, 0x58, 0x20]),
        TX_ID,
        Buffer.from([0x19, 0x00, 0x07]),
      ]),
    ];
    for (const bytes of malformed) {
      expect(() => decodeMidgardOutRefBytes(bytes)).toThrow();
    }
  });
});
