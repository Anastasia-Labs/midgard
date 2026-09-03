import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import { decodeMidgardFieldPreimage } from "../src/codec/native-tx-field-access-v1.js";
import {
  decodeMidgardAddressWitnessItem,
  decodeMidgardFieldItems,
  decodeMidgardHash28Item,
  decodeMidgardMintPolicyItem,
  decodeMidgardRedeemerWitnessItem,
  decodeMidgardSpendInputItem,
  midgardRedeemerPurposeFromTag,
} from "../src/codec/native-tx-field-item-decoders-v1.js";
import {
  encodeMidgardFieldItems,
  encodeMidgardFieldPreimageForField,
  MIDGARD_FIELD_NAMES,
  MIDGARD_REDEEMER_PURPOSE_TAGS,
  type MidgardFieldItems,
} from "../src/codec/native-tx-field-items-v1.js";

/**
 * These decoders are the read-back half of §5.3's per-field item grammar, and
 * the property that makes them correct is not "they parse" — it is that they are
 * **exact inverses of the #569 producers on bytes Aiken already agrees with**.
 *
 * So the vectors are not rebuilt here. They are loaded from
 * `tests/fixtures/native-tx-field-items-v1.generated.json`, the same checked-in
 * fixture whose every value the generated Aiken module
 * (`onchain/aiken/lib/midgard/native-tx-field-items-v1-golden.test.ak`)
 * recomputes with the Aiken producers under the fork runner. A decoder that
 * accepts something the Aiken producer would not emit, or that loses a byte on
 * the way back, fails here — and a decoder that silently agreed with a drifting
 * TypeScript producer would still fail on the Aiken side.
 *
 * Round-tripping is checked in both directions per vector:
 *
 *   1. `decode(preimage)` yields items whose `enc_i` bytes equal the fixture's
 *      `itemsHex` — so the split and the item readers agree with the producers;
 *   2. `encode(decode(preimage))` reproduces `preimageHex` byte-for-byte — so
 *      nothing the grammar carries is dropped or normalised away.
 */

type FieldVector = {
  readonly label: string;
  readonly itemCount: number;
  readonly itemsHex: readonly string[];
  readonly preimageHex: string;
};

type FieldEntry = {
  readonly fieldIndex: number;
  readonly fieldName: string;
  readonly vectors: readonly FieldVector[];
};

const fixture = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "./fixtures/native-tx-field-items-v1.generated.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as { readonly fields: readonly FieldEntry[] };

/**
 * The decoded items, re-tagged for {@link encodeMidgardFieldItems}.
 *
 * The producer's `MidgardFieldItems` and the decoder's
 * `MidgardDecodedFieldItems` are separate types on purpose — fields 3/4 decode
 * to `Buffer` where the producer accepts any `Uint8Array`, and field 2 decodes
 * to a fully materialised output — so the re-encode step names the pairing
 * explicitly rather than relying on the two unions happening to be assignable.
 */
const reencode = (fieldIndex: number, preimage: Uint8Array): Buffer => {
  switch (fieldIndex) {
    case 0:
    case 1:
      return encodeMidgardFieldPreimageForField({
        fieldIndex,
        items: decodeMidgardFieldItems(fieldIndex, preimage).items,
      });
    case 2:
      return encodeMidgardFieldPreimageForField({
        fieldIndex: 2,
        items: decodeMidgardFieldItems(2, preimage).items,
      });
    case 3:
    case 4:
      return encodeMidgardFieldPreimageForField({
        fieldIndex,
        items: decodeMidgardFieldItems(fieldIndex, preimage).items,
      });
    case 5:
      return encodeMidgardFieldPreimageForField({
        fieldIndex: 5,
        items: decodeMidgardFieldItems(5, preimage).items,
      });
    case 6:
      return encodeMidgardFieldPreimageForField({
        fieldIndex: 6,
        items: decodeMidgardFieldItems(6, preimage).items,
      });
    case 7:
      return encodeMidgardFieldPreimageForField({
        fieldIndex: 7,
        items: decodeMidgardFieldItems(7, preimage).items,
      });
    default:
      return encodeMidgardFieldPreimageForField({
        fieldIndex: 8,
        items: decodeMidgardFieldItems(8, preimage).items,
      });
  }
};

const itemEncoderFor = (
  fieldIndex: number,
  items: MidgardFieldItems["items"],
): readonly Buffer[] =>
  encodeMidgardFieldItems({ fieldIndex, items } as MidgardFieldItems);

describe("§5.3 field item decoders are the inverse of the #569 producers", () => {
  it("covers all nine fields, in the fixture's positional order", () => {
    expect(fixture.fields).toHaveLength(MIDGARD_FIELD_NAMES.length);
    expect(fixture.fields.map((field) => field.fieldIndex)).toStrictEqual([
      0, 1, 2, 3, 4, 5, 6, 7, 8,
    ]);
    expect(fixture.fields.map((field) => field.fieldName)).toStrictEqual([
      ...MIDGARD_FIELD_NAMES,
    ]);
  });

  for (const field of fixture.fields) {
    describe(`field ${field.fieldIndex.toString()} (${field.fieldName})`, () => {
      for (const vector of field.vectors) {
        it(`round-trips the ${vector.label} vector`, () => {
          const preimage = Buffer.from(vector.preimageHex, "hex");

          // §5.1: the uniform split, and §5.2's "N lives only in the header".
          const rawItems = decodeMidgardFieldPreimage(preimage);
          expect(rawItems).toHaveLength(vector.itemCount);
          expect(rawItems.map((item) => item.toString("hex"))).toStrictEqual([
            ...vector.itemsHex,
          ]);

          // §5.3: each item's reader, then the producer again.
          const decoded = decodeMidgardFieldItems(field.fieldIndex, preimage);
          expect(decoded.fieldIndex).toBe(field.fieldIndex);
          expect(decoded.items).toHaveLength(vector.itemCount);
          expect(
            itemEncoderFor(field.fieldIndex, decoded.items).map((item) =>
              item.toString("hex"),
            ),
          ).toStrictEqual([...vector.itemsHex]);

          expect(reencode(field.fieldIndex, preimage).toString("hex")).toBe(
            vector.preimageHex,
          );
        });
      }
    });
  }
});

describe("§5.3 item readers fail closed", () => {
  const spendInput = Buffer.from(
    fixture.fields[0]!.vectors.find((vector) => vector.itemCount > 0)!
      .itemsHex[0]!,
    "hex",
  );

  it("rejects the minimal one-byte output index the fixed form replaces", () => {
    // `19 0000` -> `00`: the §5.3 canon is deliberately non-minimal, so the
    // spelling minimal CBOR would choose is exactly what must reject.
    const minimal = Buffer.concat([
      spendInput.subarray(0, 35),
      Buffer.of(0x00),
    ]);
    expect(minimal.length).toBe(36);
    expect(() => decodeMidgardSpendInputItem(minimal)).toThrow();
  });

  it("rejects the `18 XX` one-byte-argument output index", () => {
    const wideHead = Buffer.concat([
      spendInput.subarray(0, 35),
      Buffer.of(0x18, 0x2a),
    ]);
    expect(() => decodeMidgardSpendInputItem(wideHead)).toThrow();
  });

  it("rejects an input item that is not the §5.3 fixed width", () => {
    expect(() =>
      decodeMidgardSpendInputItem(spendInput.subarray(0, 37)),
    ).toThrow();
  });

  it("rejects a non-minimal `59 0020` header for the 32-byte tx_id", () => {
    // The one shape the Aiken twin will *read*: `decode_definite_bytes_at`
    // accepts the wide two-byte length header, so a 39-byte item names the same
    // `(tx_id, index)` as the canonical 38-byte one. Two byte strings for one
    // out-ref would mean two ledger trie keys for one UTxO, so both twins have
    // to reject it, and on both sides the exact-38 width is what does it.
    const wideHeader = Buffer.concat([
      Buffer.of(0x82, 0x59, 0x00, 0x20),
      spendInput.subarray(3, 35),
      spendInput.subarray(35),
    ]);
    expect(wideHeader.length).toBe(39);
    // Same out-ref underneath: only the tx_id's length header differs.
    expect(wideHeader.subarray(4, 36)).toEqual(spendInput.subarray(3, 35));
    expect(() => decodeMidgardSpendInputItem(wideHeader)).toThrow();
  });

  it("rejects observer/signer items that are not 28 bytes", () => {
    expect(() => decodeMidgardHash28Item(Buffer.alloc(27))).toThrow();
    expect(() => decodeMidgardHash28Item(Buffer.alloc(29))).toThrow();
  });

  it("rejects address witness items that are not 101 bytes", () => {
    expect(() => decodeMidgardAddressWitnessItem(Buffer.alloc(100))).toThrow();
  });

  it("rejects a mint policy item with no assets", () => {
    // `82 ‖ 58 1C policy(28) ‖ a0` — §5.6 requires at least one asset.
    const emptyPolicy = Buffer.concat([
      Buffer.of(0x82, 0x58, 0x1c),
      Buffer.alloc(28, 0x11),
      Buffer.of(0xa0),
    ]);
    expect(() => decodeMidgardMintPolicyItem(emptyPolicy)).toThrow();
  });

  it("rejects a mint policy item whose asset names are out of canonical order", () => {
    const descending = Buffer.concat([
      Buffer.of(0x82, 0x58, 0x1c),
      Buffer.alloc(28, 0x11),
      Buffer.of(0xa2, 0x41, 0x02, 0x01, 0x41, 0x01, 0x01),
    ]);
    expect(() => decodeMidgardMintPolicyItem(descending)).toThrow();
  });

  it("rejects a mint quantity of zero", () => {
    const zeroQuantity = Buffer.concat([
      Buffer.of(0x82, 0x58, 0x1c),
      Buffer.alloc(28, 0x11),
      Buffer.of(0xa1, 0x41, 0x01, 0x00),
    ]);
    expect(() => decodeMidgardMintPolicyItem(zeroQuantity)).toThrow();
  });

  it("accepts exactly the seven §5.3 redeemer purpose tags and rejects the rest", () => {
    for (const [purpose, tag] of Object.entries(
      MIDGARD_REDEEMER_PURPOSE_TAGS,
    )) {
      expect(midgardRedeemerPurposeFromTag(tag)).toBe(purpose);
    }
    for (const tag of [7, 8, 23, 24, 255]) {
      expect(() => midgardRedeemerPurposeFromTag(tag)).toThrow();
    }
  });

  it("rejects a redeemer item with trailing bytes after its execution units", () => {
    const redeemerItem = Buffer.from(
      fixture.fields[8]!.vectors.find((vector) => vector.itemCount > 0)!
        .itemsHex[0]!,
      "hex",
    );
    expect(() =>
      decodeMidgardRedeemerWitnessItem(
        Buffer.concat([redeemerItem, Buffer.of(0x00)]),
      ),
    ).toThrow();
  });
});
