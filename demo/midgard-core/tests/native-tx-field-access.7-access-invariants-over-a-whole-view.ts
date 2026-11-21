import { describe, expect, it } from "vitest";

import { MidgardTxCodecError } from "../src/codec/errors.js";
import {
  buildMidgardWholeFieldView,
  decodeMidgardFieldArrayHeader,
  decodeMidgardFieldPreimage,
  encodeMidgardDefiniteBytes,
  encodeMidgardFieldArrayHeader,
  encodeMidgardFieldPreimage,
  MIDGARD_ADDRESS_WITNESS_STRIDE,
  MIDGARD_EMPTY_FIELD_COMMITMENT,
  MIDGARD_HASH28_STRIDE,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  MIDGARD_SPEND_INPUT_STRIDE,
  midgardFieldCommitment,
  midgardFieldCommitmentFromItems,
  midgardFieldItemAt,
  midgardFieldItemCount,
  midgardFieldItemExtent,
  midgardFieldReadRange,
  midgardFieldStride,
  midgardFieldTotalLength,
} from "../src/codec/native-tx-field-access.js";

export const hex = (value: Uint8Array): string =>
  Buffer.from(value).toString("hex");

const bytes = (value: string): Buffer => Buffer.from(value, "hex");

export const filler = (length: number, seed = 0): Buffer =>
  Buffer.from(
    Array.from({ length }, (_, index) => (index * 7 + 3 + seed * 31) % 256),
  );

const wholeView = (fieldIndex: number, items: readonly Uint8Array[]) => {
  const preimage = encodeMidgardFieldPreimage(items);
  return buildMidgardWholeFieldView({
    fieldIndex,
    preimage,
    expectedCommitment: midgardFieldCommitment(preimage),
  });
};

describe("§5.1 enveloped preimage grammar", () => {
  it("encodes an empty field as exactly `80` for every one of the nine", () => {
    for (let fieldIndex = 0; fieldIndex < 9; fieldIndex += 1) {
      const view = wholeView(fieldIndex, []);
      expect(hex(encodeMidgardFieldPreimage([]))).toBe("80");
      expect(midgardFieldItemCount(view)).toBe(0);
    }
  });

  it("emits minimal array headers at every width boundary and rejects wider", () => {
    expect(hex(encodeMidgardFieldArrayHeader(0))).toBe("80");
    expect(hex(encodeMidgardFieldArrayHeader(23))).toBe("97");
    expect(hex(encodeMidgardFieldArrayHeader(24))).toBe("9818");
    expect(hex(encodeMidgardFieldArrayHeader(255))).toBe("98ff");
    expect(hex(encodeMidgardFieldArrayHeader(256))).toBe("990100");
    expect(hex(encodeMidgardFieldArrayHeader(65535))).toBe("99ffff");
    expect(() => encodeMidgardFieldArrayHeader(65536)).toThrow(
      MidgardTxCodecError,
    );
  });

  it("emits minimal item wrappers at every width boundary", () => {
    expect(hex(encodeMidgardDefiniteBytes(Buffer.alloc(0)))).toBe("40");
    expect(hex(encodeMidgardDefiniteBytes(Buffer.alloc(23)))).toBe(
      `57${"00".repeat(23)}`,
    );
    expect(hex(encodeMidgardDefiniteBytes(Buffer.alloc(24)))).toBe(
      `5818${"00".repeat(24)}`,
    );
    expect(hex(encodeMidgardDefiniteBytes(Buffer.alloc(256))).slice(0, 6)).toBe(
      "590100",
    );
  });

  it("rejects the non-minimal and out-of-grammar array heads §5.1 excludes", () => {
    // `98 17` spells 23 in the two-byte form; `99 00ff` spells 255 in three.
    expect(() => decodeMidgardFieldArrayHeader(bytes("9817"))).toThrow(
      MidgardTxCodecError,
    );
    expect(() => decodeMidgardFieldArrayHeader(bytes("9900ff"))).toThrow(
      MidgardTxCodecError,
    );
    // `9a` is well-formed CBOR and outside the §5.1 acceptance set.
    expect(() => decodeMidgardFieldArrayHeader(bytes("9a00010000"))).toThrow(
      MidgardTxCodecError,
    );
    expect(() => decodeMidgardFieldArrayHeader(bytes("a0"))).toThrow(
      MidgardTxCodecError,
    );
  });

  it("round-trips items and fails closed on every §5.1 deviation", () => {
    const items = [filler(1, 1), filler(24, 2), filler(300, 3)];
    const preimage = encodeMidgardFieldPreimage(items);
    expect(decodeMidgardFieldPreimage(preimage).map(hex)).toEqual(
      items.map(hex),
    );

    // Trailing bytes after item N-1.
    expect(() =>
      decodeMidgardFieldPreimage(Buffer.concat([preimage, bytes("00")])),
    ).toThrow(/trailing bytes/u);
    // A header that over-counts its items.
    const overCounted = Buffer.from(preimage);
    overCounted[0] = 0x84;
    expect(() => decodeMidgardFieldPreimage(overCounted)).toThrow(
      MidgardTxCodecError,
    );
    // A header that under-counts leaves trailing bytes.
    const underCounted = Buffer.from(preimage);
    underCounted[0] = 0x82;
    expect(() => decodeMidgardFieldPreimage(underCounted)).toThrow(
      /trailing bytes/u,
    );
    // A non-minimal item wrapper: `58 01` where `41` is the one spelling.
    expect(() => decodeMidgardFieldPreimage(bytes("8158010f"))).toThrow(
      /non-minimal/u,
    );
  });
});

describe("§4 flat commitment", () => {
  it("pins the field-independent empty-field commitment", () => {
    // The exact cross-language vector: blake2b_256(#"80"). The Aiken twin pins
    // the same 32 bytes as `native_tx_field_access_v1.empty_field_commitment`.
    expect(hex(MIDGARD_EMPTY_FIELD_COMMITMENT)).toBe(
      "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0",
    );
    expect(hex(midgardFieldCommitment(bytes("80")))).toBe(
      hex(MIDGARD_EMPTY_FIELD_COMMITMENT),
    );
    expect(hex(midgardFieldCommitmentFromItems([]))).toBe(
      hex(MIDGARD_EMPTY_FIELD_COMMITMENT),
    );
  });

  it("carries no domain tag, version prefix or field index", () => {
    const preimage = encodeMidgardFieldPreimage([filler(4, 1)]);
    expect(hex(midgardFieldCommitment(preimage))).toBe(
      hex(midgardFieldCommitmentFromItems([filler(4, 1)])),
    );
  });
});

describe("§5.3 stride table", () => {
  it("matches the Aiken table field by field", () => {
    expect(
      Array.from({ length: 9 }, (_, fieldIndex) =>
        midgardFieldStride(fieldIndex),
      ),
    ).toEqual([
      MIDGARD_SPEND_INPUT_STRIDE,
      MIDGARD_SPEND_INPUT_STRIDE,
      0,
      MIDGARD_HASH28_STRIDE,
      MIDGARD_HASH28_STRIDE,
      0,
      0,
      MIDGARD_ADDRESS_WITNESS_STRIDE,
      0,
    ]);
  });

  it("rejects a field index outside 0..8", () => {
    expect(() => midgardFieldStride(9)).toThrow(MidgardTxCodecError);
    expect(() => midgardFieldStride(-1)).toThrow(MidgardTxCodecError);
  });
});

describe("§7 access invariants over a Whole view", () => {
  const items = [filler(28, 1), filler(28, 2), filler(28, 3)];

  it("authenticates once against the committed hash", () => {
    const preimage = encodeMidgardFieldPreimage(items);
    expect(() =>
      buildMidgardWholeFieldView({
        fieldIndex: 3,
        preimage,
        expectedCommitment: Buffer.alloc(32),
      }),
    ).toThrow(/does not match the committed field hash/u);
  });

  it("resolves fixed-stride items arithmetically and reads their wrapper", () => {
    const view = wholeView(3, items);
    expect(view.view).toBe("Whole");
    expect(midgardFieldItemCount(view)).toBe(3);
    expect(midgardFieldTotalLength(view)).toBe(1 + 30 * 3);
    for (const [index, item] of items.entries()) {
      expect(midgardFieldItemExtent(view, index)).toEqual({
        offset: 1 + 30 * index + 2,
        length: 28,
      });
      expect(hex(midgardFieldItemAt(view, index))).toBe(hex(item));
    }
  });

  it("aborts rather than clamps an out-of-range index or read", () => {
    const view = wholeView(3, items);
    expect(() => midgardFieldItemAt(view, 3)).toThrow(/out of range/u);
    expect(() => midgardFieldItemAt(view, -1)).toThrow(/out of range/u);
    expect(() =>
      midgardFieldReadRange(view, midgardFieldTotalLength(view), 1),
    ).toThrow(/leaves the authenticated bytes/u);
    // Two clamped out-of-range reads would be byte-equal; neither is reachable.
    expect(() => midgardFieldReadRange(view, 1_000, 4)).toThrow(
      /leaves the authenticated bytes/u,
    );
    expect(() => midgardFieldReadRange(view, 2_000, 4)).toThrow(
      /leaves the authenticated bytes/u,
    );
  });

  it("refuses a fixed-stride item whose wrapper is not the canonical spelling", () => {
    // The two counterexamples the on-chain door was hardened against: a
    // 28-byte payload opened behind `00 00` or `ff ff` instead of `58 1c`.
    for (const wrapper of ["0000", "ffff"]) {
      const forged = Buffer.concat([
        bytes("81"),
        bytes(wrapper),
        filler(28, 9),
      ]);
      const view = buildMidgardWholeFieldView({
        fieldIndex: 3,
        preimage: forged,
        expectedCommitment: midgardFieldCommitment(forged),
      });
      expect(() => midgardFieldItemAt(view, 0)).toThrow(MidgardTxCodecError);
    }
  });

  it("enforces §7.4 count consistency at view construction", () => {
    // A fixed-stride field whose header count does not reconcile with length.
    const forged = Buffer.concat([bytes("82"), bytes("581c"), filler(28, 1)]);
    expect(() =>
      buildMidgardWholeFieldView({
        fieldIndex: 3,
        preimage: forged,
        expectedCommitment: midgardFieldCommitment(forged),
      }),
    ).toThrow(/count consistency/u);
    // A variable-width field is checked by the full walk instead.
    const walked = Buffer.concat([bytes("83"), bytes("4100"), bytes("4101")]);
    expect(() =>
      buildMidgardWholeFieldView({
        fieldIndex: 2,
        preimage: walked,
        expectedCommitment: midgardFieldCommitment(walked),
      }),
    ).toThrow(MidgardTxCodecError);
  });

  it("walks variable-width items with no offset table", () => {
    const variable = [filler(1, 1), filler(24, 2), filler(300, 3)];
    const view = wholeView(2, variable);
    expect(midgardFieldItemCount(view)).toBe(3);
    expect(midgardFieldItemExtent(view, 0)).toEqual({ offset: 2, length: 1 });
    expect(midgardFieldItemExtent(view, 1)).toEqual({
      offset: 5,
      length: 24,
    });
    expect(midgardFieldItemExtent(view, 2)).toEqual({
      offset: 32,
      length: 300,
    });
    for (const [index, item] of variable.entries()) {
      expect(hex(midgardFieldItemAt(view, index))).toBe(hex(item));
    }
  });

  it("rejects a preimage above the §5.4 aggregate bound", () => {
    const oversized = Buffer.alloc(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES + 1,
    );
    oversized[0] = 0x80;
    expect(() =>
      buildMidgardWholeFieldView({
        fieldIndex: 2,
        preimage: oversized,
        expectedCommitment: midgardFieldCommitment(oversized),
      }),
    ).toThrow(/aggregate bound/u);
  });
});
