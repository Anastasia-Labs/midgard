import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  buildMidgardWholeFieldView,
  decodeMidgardFieldArrayHeader,
  decodeMidgardFieldPreimage,
  midgardFieldItemAt,
  midgardFieldItemExtent,
  midgardFieldStride,
  selectMidgardFieldCarriageTier,
} from "../src/codec/native-tx-field-access.js";
import {
  encodeMidgardFieldItems,
  encodeMidgardFieldPreimageForField,
  MIDGARD_FIELD_NAMES,
  midgardFieldCommitmentForField,
} from "../src/codec/native-tx-field-items.js";
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

export const golden = JSON.parse(
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

export const hex = (value: Uint8Array): string =>
  Buffer.from(value).toString("hex");

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
