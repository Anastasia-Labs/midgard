import { describe, expect, it } from "vitest";

import {
  encodeMidgardNativeTxProofFieldLengths,
  midgardNativeTxProofFieldPreimageLengths,
} from "../src/codec/native.js";
import {
  MIDGARD_ADDRESS_WITNESS_ITEM_BYTES,
  MIDGARD_HASH28_ITEM_BYTES,
  MIDGARD_SPEND_INPUT_ITEM_BYTES,
  selectMidgardFieldCarriageTier,
} from "../src/codec/native-tx-field-access.js";
import {
  encodeMidgardAddressWitnessItem,
  encodeMidgardFieldPreimageForField,
  encodeMidgardFixedOutputIndex,
  encodeMidgardHash28Item,
  encodeMidgardMintPolicyItem,
  encodeMidgardRedeemerWitnessItem,
  encodeMidgardSpendInputItem,
  MIDGARD_REDEEMER_PURPOSE_TAGS,
} from "../src/codec/native-tx-field-items.js";
import {
  encodeMidgardVersionedScript,
  type MidgardVersionedScript,
} from "../src/codec/versioned-script.js";
import * as vectors from "./fixtures/native-tx-field-items-v1.vectors.mjs";
import {
  golden,
  hex,
} from "./native-tx-field-items-goldens.5-3-per-field-item-encodings-recompute-from-the-twin.js";

describe("§5.3 fixed-width item assertions", () => {
  it("keeps fields 0/1 at 38 bytes for every §9 boundary index", () => {
    for (const outputIndex of vectors.FIXED_INDEX_BOUNDARIES) {
      const item = encodeMidgardSpendInputItem(vectors.input(1, outputIndex));
      expect(item.length).toBe(MIDGARD_SPEND_INPUT_ITEM_BYTES);
      // The index occupies the last three bytes and is always `19 XXXX`.
      expect(hex(item.subarray(-3))).toBe(
        hex(encodeMidgardFixedOutputIndex(outputIndex)),
      );
      expect(item[35]).toBe(0x19);
    }
  });

  it("pins the §5.3 fixed output index at every boundary value", () => {
    expect(golden.fixedOutputIndexes.map((entry) => entry.outputIndex)).toEqual(
      vectors.FIXED_INDEX_BOUNDARIES,
    );
    for (const entry of golden.fixedOutputIndexes) {
      expect(hex(encodeMidgardFixedOutputIndex(entry.outputIndex))).toBe(
        entry.encodedHex,
      );
      // Three bytes, always — including for 0 and 23, which minimal CBOR
      // would spell in one. This is the format's sole non-minimal encoding.
      expect(entry.encodedHex.length).toBe(6);
      expect(entry.encodedHex.startsWith("19")).toBe(true);
    }
  });

  it("keeps fields 3/4 at 28 bytes and field 7 at 101", () => {
    for (const field of golden.fields) {
      if (field.fieldIndex === 3 || field.fieldIndex === 4) {
        for (const vector of field.vectors) {
          for (const itemHex of vector.itemsHex) {
            expect(itemHex.length / 2).toBe(MIDGARD_HASH28_ITEM_BYTES);
          }
        }
      }
      if (field.fieldIndex === 7) {
        for (const vector of field.vectors) {
          for (const itemHex of vector.itemsHex) {
            expect(itemHex.length / 2).toBe(MIDGARD_ADDRESS_WITNESS_ITEM_BYTES);
          }
        }
      }
    }
  });

  it("rejects an out-of-range output index rather than truncating it", () => {
    expect(() => encodeMidgardFixedOutputIndex(65_536)).toThrow();
    expect(() => encodeMidgardFixedOutputIndex(-1)).toThrow();
  });

  /**
   * The width rules of §5.3 are what fix the strides, so they have to be
   * enforced, not merely satisfied by every vector. A positive-only suite
   * cannot tell an enforced assertion from a deleted one: loosen
   * `encodeMidgardHash28Item` and every vector above still passes.
   */
  it("rejects items that are not exactly the §5.3 width", () => {
    expect(() => encodeMidgardHash28Item(vectors.filler(27, 1))).toThrow();
    expect(() => encodeMidgardHash28Item(vectors.filler(29, 1))).toThrow();
    expect(() =>
      encodeMidgardSpendInputItem({
        txId: vectors.filler(31, 1),
        outputIndex: 0,
      }),
    ).toThrow();
    expect(() =>
      encodeMidgardAddressWitnessItem({
        verificationKey: vectors.filler(31, 1),
        signature: vectors.filler(64, 2),
      }),
    ).toThrow();
    expect(() =>
      encodeMidgardAddressWitnessItem({
        verificationKey: vectors.filler(32, 1),
        signature: vectors.filler(63, 2),
      }),
    ).toThrow();
  });
});

describe("§5.6 mint ordering is enforced, not assumed", () => {
  const policy = (seed: number, nameHex: string) =>
    vectors.mintPolicy(seed, [vectors.asset(nameHex, 1)]);

  it("rejects asset names out of canonical order or repeated", () => {
    // Canonical order is length-first, then byte-lexicographic: "4141" (2 B)
    // sorts after "42" (1 B), so this pair is descending.
    expect(() =>
      encodeMidgardMintPolicyItem(
        vectors.mintPolicy(1, [
          vectors.asset("4141", 1),
          vectors.asset("42", 2),
        ]),
      ),
    ).toThrow();
    expect(() =>
      encodeMidgardMintPolicyItem(
        vectors.mintPolicy(1, [vectors.asset("41", 1), vectors.asset("41", 2)]),
      ),
    ).toThrow();
  });

  it("rejects a zero quantity and an over-long asset name", () => {
    expect(() =>
      encodeMidgardMintPolicyItem(vectors.mintPolicy(1, [])),
    ).toThrow();
    expect(() =>
      encodeMidgardMintPolicyItem(
        vectors.mintPolicy(1, [vectors.asset("41", 0)]),
      ),
    ).toThrow();
    expect(() =>
      encodeMidgardMintPolicyItem(
        vectors.mintPolicy(1, [
          vectors.asset(vectors.filler(33, 1).toString("hex"), 1),
        ]),
      ),
    ).toThrow();
  });

  it("rejects policy ids out of canonical order or repeated across the field", () => {
    // The §5.6 decoder enforces ordering across the whole field, so a producer
    // that let a descending or duplicated policy list past would hand back a
    // preimage no decoder on either side accepts.
    const ascending = [policy(5, "41"), policy(6, "42")].sort((left, right) =>
      Buffer.compare(Buffer.from(left.policyId), Buffer.from(right.policyId)),
    );
    const descending = [...ascending].reverse();

    expect(() =>
      encodeMidgardFieldPreimageForField({ fieldIndex: 5, items: ascending }),
    ).not.toThrow();
    expect(() =>
      encodeMidgardFieldPreimageForField({
        fieldIndex: 5,
        items: descending,
      }),
    ).toThrow();
    expect(() =>
      encodeMidgardFieldPreimageForField({
        fieldIndex: 5,
        items: [ascending[0]!, ascending[0]!],
      }),
    ).toThrow();
  });
});

describe("§5.3 the two value sets", () => {
  it("pins the three script language tags, `MidgardV1` included", () => {
    expect(golden.languageTags.map((entry) => entry.tag)).toEqual([0, 3, 128]);
    const encoders: Record<string, MidgardVersionedScript> = {
      NativeCardano: vectors.nativeCardano(90),
      PlutusV3: vectors.plutusV3(91, 8),
      MidgardV1: vectors.midgardV1(92, 8),
    };
    for (const entry of golden.languageTags) {
      const script = encoders[entry.language];
      expect(script).toBeDefined();
      expect(hex(encodeMidgardVersionedScript(script!))).toBe(entry.itemHex);
    }
    // 128 is the only tag that is not a single byte: `18 80`.
    const midgard = golden.languageTags.find((entry) => entry.tag === 128);
    expect(midgard?.itemHex.slice(2, 6)).toBe("1880");
  });

  it("pins all seven redeemer purpose tags at one byte each", () => {
    expect(golden.purposeTags.map((entry) => entry.purpose)).toEqual(
      Object.keys(MIDGARD_REDEEMER_PURPOSE_TAGS),
    );
    for (const entry of golden.purposeTags) {
      expect(
        MIDGARD_REDEEMER_PURPOSE_TAGS[
          entry.purpose as keyof typeof MIDGARD_REDEEMER_PURPOSE_TAGS
        ],
      ).toBe(entry.tag);
      expect(entry.tag).toBeLessThanOrEqual(23);
      expect(
        hex(
          encodeMidgardRedeemerWitnessItem(
            vectors.redeemer(
              entry.purpose as keyof typeof MIDGARD_REDEEMER_PURPOSE_TAGS,
              1,
              "d87980",
              2,
              3,
            ),
          ),
        ),
      ).toBe(entry.itemHex);
      // Every tag ≤ 23 occupies exactly one byte equal to its value, right
      // after the `84` array head.
      expect(entry.itemHex.slice(2, 4)).toBe(
        entry.tag.toString(16).padStart(2, "0"),
      );
    }
  });
});

describe("§6.2 datum canonicity boundaries", () => {
  it("carries the bignum and high-alternative constructor forms as bytes", () => {
    expect(
      golden.datumCanonicityBoundaries.map((entry) => entry.label),
    ).toEqual(vectors.DATUM_CANONICITY_BOUNDARIES.map(([label]) => label));
    // The point of the vector: these are *canonical* under §6.2's re-pin, and
    // they reach the committed bytes through an output's opaque `datum_cbor`.
    // Canonicity and materialisability are different predicates — nothing here
    // asks either twin to deserialise one.
    const bignum = golden.datumCanonicityBoundaries.find(
      (entry) => entry.label === "bignum_two_to_64",
    );
    expect(bignum?.cborHex).toBe("c249010000000000000000");
    const constr = golden.datumCanonicityBoundaries.find(
      (entry) => entry.label === "constr_alternative_128",
    );
    expect(constr?.cborHex.startsWith("d866")).toBe(true);

    // Each boundary datum really is embedded in the field-2 preimage.
    const field2 = golden.fields.find((field) => field.fieldIndex === 2);
    const boundaryVector = field2?.vectors.find(
      (vector) => vector.label === "datum_canonicity_boundaries",
    );
    expect(boundaryVector?.itemCount).toBe(
      golden.datumCanonicityBoundaries.length,
    );
    for (const entry of golden.datumCanonicityBoundaries) {
      expect(boundaryVector?.preimageHex).toContain(entry.cborHex);
    }
  });
});

describe("§2.4 field-preimage lengths keep the wire-order transposition", () => {
  it("serialises script_witnesses before address_witnesses", () => {
    const source = vectors.FIELD_PREIMAGE_LENGTH_SOURCE;
    // Driven through the function that performs the transposition. Handing a
    // pre-ordered array to the encoder would prove array order only, and would
    // still pass with the two witness slots swapped.
    const derived = midgardNativeTxProofFieldPreimageLengths(source);

    expect(derived).toEqual(golden.fieldPreimageLengths.lengths);
    // Pairwise distinct, or a transposition could not be observed at all.
    expect(new Set(derived).size).toBe(9);

    // The assertion that actually bites: wire slot 6 must carry the *script*
    // witness length and slot 7 the *address* one, while the record declares
    // them the other way round.
    expect(derived[6]).toBe(source.witnessSet.scriptTxWitsPreimageCbor.length);
    expect(derived[7]).toBe(source.witnessSet.addrTxWitsPreimageCbor.length);
    expect(source.witnessSet.scriptTxWitsPreimageCbor.length).not.toBe(
      source.witnessSet.addrTxWitsPreimageCbor.length,
    );

    expect(hex(encodeMidgardNativeTxProofFieldLengths(derived))).toBe(
      golden.fieldPreimageLengths.encodedHex,
    );
    const encoded = Buffer.from(golden.fieldPreimageLengths.encodedHex, "hex");
    expect(encoded[0]).toBe(0x89);
  });
});

describe("§8 carriage tiers", () => {
  it("partitions preimage lengths across the three tiers", () => {
    expect(golden.carriageTiers.map((entry) => entry.preimageLength)).toEqual(
      vectors.CARRIAGE_BOUNDARY_LENGTHS,
    );
    for (const entry of golden.carriageTiers) {
      expect(selectMidgardFieldCarriageTier(entry.preimageLength)).toBe(
        entry.tier,
      );
    }
    expect(golden.carriageTiers.map((entry) => entry.tier)).toEqual([
      "Inline",
      "Inline",
      "RawUtxo",
      "RawUtxo",
      "Certified",
      "Certified",
    ]);
  });
});
