import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  encodeMidgardNativeTxProofFieldLengths,
  midgardNativeTxProofFieldPreimageLengths,
} from "../src/codec/native.js";
import {
  buildMidgardChunkedFieldView,
  buildMidgardWholeFieldView,
  decodeMidgardFieldArrayHeader,
  decodeMidgardFieldPreimage,
  deriveMidgardFieldPreimageCertificate,
  MIDGARD_ADDRESS_WITNESS_ITEM_BYTES,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_HASH28_ITEM_BYTES,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT,
  MIDGARD_SPEND_INPUT_ITEM_BYTES,
  midgardFieldItemAt,
  midgardFieldItemExtent,
  midgardFieldStride,
  selectMidgardFieldCarriageTier,
} from "../src/codec/native-tx-field-access-v1.js";
import {
  encodeMidgardAddressWitnessItem,
  encodeMidgardFieldItems,
  encodeMidgardFieldPreimageForField,
  encodeMidgardFixedOutputIndex,
  encodeMidgardHash28Item,
  encodeMidgardMintPolicyItem,
  encodeMidgardRedeemerWitnessItem,
  encodeMidgardSpendInputItem,
  MIDGARD_FIELD_NAMES,
  MIDGARD_REDEEMER_PURPOSE_TAGS,
  midgardFieldCommitmentForField,
} from "../src/codec/native-tx-field-items-v1.js";
import {
  encodeMidgardVersionedScript,
  type MidgardVersionedScript,
} from "../src/codec/versioned-script.js";
import * as vectors from "./fixtures/native-tx-field-items-v1.vectors.mjs";

/**
 * The TypeScript half of the **per-field** cross-language golden channel.
 *
 * Every value in the generated fixture is recomputed here from the `src/` twins
 * driven by the same structured vector definitions the generator uses, so a
 * drifting item encoder fails on this side. The generated Aiken module
 * (`onchain/aiken/lib/midgard/native-tx-field-items-v1-golden.test.ak`)
 * recomputes the same values with the Aiken producers under the fork runner, so
 * a divergence between the two fails on that side. Regenerate both with
 * `pnpm run fixtures:native-tx-field-items-v1:sync`.
 *
 * The fixture is loaded, never rebuilt-and-trusted: what makes this a golden
 * suite rather than a tautology is that the checked-in bytes are compared
 * against freshly computed ones.
 */

type GoldenVector = {
  readonly label: string;
  readonly itemCount: number;
  readonly headerLength: number;
  readonly itemsHex: readonly string[];
  readonly preimageHex: string;
  readonly preimageLength: number;
  readonly commitmentHex: string;
  readonly carriageTier: string;
  readonly itemExtents: readonly {
    readonly offset: number;
    readonly length: number;
  }[];
};

type GoldenField = {
  readonly fieldIndex: number;
  readonly fieldName: string;
  readonly stride: number;
  readonly aikenProducer: string;
  readonly aikenDecoder: string;
  readonly vectors: readonly GoldenVector[];
};

type Golden = {
  readonly schema: string;
  readonly version: number;
  readonly specDocument: string;
  readonly generator: string;
  readonly fieldCount: number;
  readonly fields: readonly GoldenField[];
  readonly languageTags: readonly {
    readonly language: string;
    readonly tag: number;
    readonly itemHex: string;
  }[];
  readonly purposeTags: readonly {
    readonly purpose: string;
    readonly tag: number;
    readonly itemHex: string;
  }[];
  readonly fixedOutputIndexes: readonly {
    readonly outputIndex: number;
    readonly encodedHex: string;
  }[];
  readonly datumCanonicityBoundaries: readonly {
    readonly label: string;
    readonly cborHex: string;
  }[];
  readonly fieldPreimageLengths: {
    readonly lengths: readonly number[];
    readonly encodedHex: string;
  };
  readonly carriageTiers: readonly {
    readonly preimageLength: number;
    readonly tier: string;
  }[];
  readonly straddle: {
    readonly fieldIndex: number;
    readonly stride: number;
    readonly blockHex: string;
    readonly blockElementCount: number;
    readonly repeats: number;
    readonly itemCount: number;
    readonly headerHex: string;
    readonly totalLength: number;
    readonly commitmentHex: string;
    readonly carriageTier: string;
    readonly chunkLengths: readonly number[];
    readonly chunkDigestsHex: readonly string[];
    readonly reads: readonly {
      readonly itemIndex: number;
      readonly offset: number;
      readonly length: number;
      readonly straddles: boolean;
      readonly itemHex: string;
    }[];
    readonly itemsHex: readonly string[];
  };
};

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "./fixtures/native-tx-field-items-v1.generated.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as Golden;

const hex = (value: Uint8Array): string => Buffer.from(value).toString("hex");

const FIELD_VECTORS = vectors.FIELD_VECTORS;

/**
 * The vector module is authored as plain ESM — the generator loads it with bare
 * `node`, before and without any TypeScript build — and its types live in a
 * hand-written `.d.mts` beside it. This package sets neither `allowJs` nor
 * `checkJs`, so `tsc` reads the declaration and never the module, which leaves
 * exactly one thing able to drift silently: the **export list**. A declared name
 * the module does not export type-checks at every call site and is `undefined`
 * at run time.
 *
 * The literal below is typed `Record<keyof typeof vectors, true>`, so the
 * compiler rejects it unless it names every declared value export and nothing
 * else; the assertion then compares that set against the module's real one. The
 * declared *types* stay unverified by construction, but they cannot manufacture
 * a false green: every value here is driven through the real encoders and
 * compared byte-for-byte against the checked-in fixture below, so a declaration
 * that lies about a shape fails loudly rather than passing quietly.
 */
const DECLARED_VECTOR_EXPORTS: Record<keyof typeof vectors, true> = {
  filler: true,
  input: true,
  addressWitness: true,
  keyAddress: true,
  value: true,
  datum: true,
  plutusV3: true,
  midgardV1: true,
  nativeCardano: true,
  redeemer: true,
  mintPolicy: true,
  asset: true,
  selectorFor: true,
  straddleInputs: true,
  FIXED_INDEX_BOUNDARIES: true,
  DATUM_CANONICITY_BOUNDARIES: true,
  CARRIAGE_BOUNDARY_LENGTHS: true,
  FIELD_PREIMAGE_LENGTHS: true,
  FIELD_PREIMAGE_LENGTH_SOURCE: true,
  FIELD_VECTORS: true,
  STRADDLE_FIELD_INDEX: true,
  STRADDLE_BLOCK_ITEMS: true,
  STRADDLE_REPEATS: true,
  STRADDLE_ITEM_COUNT: true,
  STRADDLE_ITEM_INDEX: true,
  STRADDLE_OWNER: true,
  STRADDLE_TX_ID: true,
};

describe("per-field golden fixture provenance", () => {
  it("keeps the vector module's declared exports and real exports in step", () => {
    expect(Object.keys(vectors).sort()).toEqual(
      Object.keys(DECLARED_VECTOR_EXPORTS).sort(),
    );
    for (const name of Object.keys(DECLARED_VECTOR_EXPORTS)) {
      expect(vectors[name as keyof typeof vectors]).toBeDefined();
    }
  });

  it("declares the schema, spec document and generator it came from", () => {
    expect(golden.schema).toBe("midgard-native-tx-field-items-v1-golden");
    expect(golden.version).toBe(1);
    expect(golden.specDocument).toBe("docs/spec/midgard-tx.md");
    expect(golden.generator).toBe(
      "demo/midgard-core/scripts/generate-native-tx-field-items-v1-goldens.mjs",
    );
  });

  it("covers every one of the nine §2.5 fields exactly once", () => {
    expect(golden.fieldCount).toBe(9);
    expect(golden.fields.map((field) => field.fieldIndex)).toEqual([
      0, 1, 2, 3, 4, 5, 6, 7, 8,
    ]);
    for (const field of golden.fields) {
      expect(field.fieldName).toBe(MIDGARD_FIELD_NAMES[field.fieldIndex]);
      expect(field.stride).toBe(midgardFieldStride(field.fieldIndex));
      // A field with no vectors would pass every assertion below vacuously.
      expect(field.vectors.length).toBeGreaterThan(0);
      // §5.1's empty case is normative for all nine, mint included.
      expect(field.vectors.some((vector) => vector.itemCount === 0)).toBe(true);
    }
  });
});

describe("§5.3 per-field item encodings recompute from the twin", () => {
  for (const fieldDefinition of FIELD_VECTORS) {
    const goldenField = golden.fields.find(
      (field) => field.fieldIndex === fieldDefinition.fieldIndex,
    );

    describe(`field ${fieldDefinition.fieldIndex}`, () => {
      it("is present in the fixture with the same producer wiring", () => {
        expect(goldenField).toBeDefined();
        expect(goldenField?.aikenProducer).toBe(fieldDefinition.aikenProducer);
        expect(goldenField?.aikenDecoder).toBe(fieldDefinition.aikenDecoder);
        expect(goldenField?.vectors.map((vector) => vector.label)).toEqual(
          fieldDefinition.vectors.map((vector) => vector.label),
        );
      });

      for (const vectorDefinition of fieldDefinition.vectors) {
        it(`recomputes \`${vectorDefinition.label}\` byte for byte`, () => {
          const goldenVector = goldenField?.vectors.find(
            (vector) => vector.label === vectorDefinition.label,
          );
          expect(goldenVector).toBeDefined();
          if (goldenVector === undefined) {
            return;
          }
          const selector = vectors.selectorFor(
            fieldDefinition,
            vectorDefinition,
          );

          const itemBytes = encodeMidgardFieldItems(selector);
          const preimage = encodeMidgardFieldPreimageForField(selector);
          const commitment = midgardFieldCommitmentForField(selector);

          // The §5.3 item bytes — the thing this suite exists for.
          expect(itemBytes.map(hex)).toEqual(goldenVector.itemsHex);
          // The §5.1 envelope over them, and the §4 flat commitment.
          expect(hex(preimage)).toBe(goldenVector.preimageHex);
          expect(preimage.length).toBe(goldenVector.preimageLength);
          expect(hex(commitment)).toBe(goldenVector.commitmentHex);
          expect(itemBytes.length).toBe(goldenVector.itemCount);

          const header = decodeMidgardFieldArrayHeader(preimage);
          expect(header.count).toBe(goldenVector.itemCount);
          expect(header.nextOffset).toBe(goldenVector.headerLength);

          // §5.1's fail-closed decoder must recover exactly the items.
          expect(decodeMidgardFieldPreimage(preimage).map(hex)).toEqual(
            goldenVector.itemsHex,
          );

          // §8's tier for this preimage length.
          expect(selectMidgardFieldCarriageTier(preimage.length)).toBe(
            goldenVector.carriageTier,
          );

          // §7.2 extents and reads through a real authenticated view.
          const view = buildMidgardWholeFieldView({
            fieldIndex: fieldDefinition.fieldIndex,
            preimage,
            expectedCommitment: commitment,
          });
          goldenVector.itemExtents.forEach((extent, index) => {
            expect(midgardFieldItemExtent(view, index)).toEqual({
              offset: extent.offset,
              length: extent.length,
            });
            expect(hex(midgardFieldItemAt(view, index))).toBe(
              goldenVector.itemsHex[index],
            );
          });
          // §7.3 abort, never clamp: one past the end must fail, not return
          // the last item again.
          expect(() =>
            midgardFieldItemExtent(view, goldenVector.itemCount),
          ).toThrow();
        });
      }
    });
  }
});

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
