/**
 * Types for `native-tx-field-items-v1.vectors.mjs`.
 *
 * The vector data is authored as plain ESM so the generator can load it without
 * a build step; this file is what lets the vitest suite consume it with the
 * same types the codec exposes, rather than through `any`. In particular
 * {@link selectorFor} is where the correlation between a group's `fieldIndex`
 * and its item type is stated — once, here — because TypeScript cannot infer it
 * from two independent property reads at the call site.
 *
 * **Why a `.d.mts` beside a `.mjs` rather than a `.ts`.** The generator runs on
 * bare `node`, before any TypeScript exists: it loads the built `dist/` twins
 * and these definitions in the same process, and the package carries no TS
 * loader (`tsx`/`ts-node` are not dependencies). Authoring the vectors in `.ts`
 * would therefore mean compiling them — either into `src/`, which puts test
 * fixtures in the package's shipped public API, or as a second build entry that
 * emits test data into `dist/`. Both are worse than a declaration file.
 *
 * The cost is that `tsc` reads this file and never the module beside it (the
 * package sets neither `allowJs` nor `checkJs`), so these declarations are
 * asserted rather than derived. The export *list* is checked against the real
 * module by `tests/native-tx-field-items-goldens.test.ts`; the declared
 * types are not, and rely instead on every value here being driven through the
 * real encoders and compared byte-for-byte against the checked-in fixture,
 * where a wrong shape fails loudly.
 *
 * **Hazard.** The package sets `skipLibCheck: true`, which silences type errors
 * *inside* this file as well as inside third-party declarations. A name here
 * that no longer exists in `src/` therefore does not fail the build: it
 * degrades silently to `any`, and the suite that consumes these declarations
 * quietly stops being typed at all. Both fixture declaration files had drifted
 * that way. `npx tsc --noEmit --skipLibCheck false` from the package root
 * surfaces it — the only first-party errors it reports are in these files.
 */

import type {
  MidgardAddressWitness,
  MidgardFieldItems,
  MidgardMintAsset,
  MidgardMintPolicyItem,
  MidgardRedeemerPurpose,
  MidgardRedeemerWitness,
  MidgardTxInput,
} from "../../src/codec/native-tx-field-items.js";
import type {
  MidgardNativeTxBodyCanonical,
  MidgardNativeTxWitnessSetCanonical,
} from "../../src/codec/native.js";
import type { MidgardTxOutput } from "../../src/codec/output.js";
import type { MidgardDatum } from "../../src/codec/datum.js";
import type { MidgardValue } from "../../src/codec/value.js";
import type { MidgardVersionedScript } from "../../src/codec/versioned-script.js";

export declare const filler: (length: number, seed?: number) => Buffer;

export declare const input: (
  seed: number,
  outputIndex: number,
) => MidgardTxInput;

export declare const addressWitness: (seed: number) => MidgardAddressWitness;

export declare const keyAddress: (seed: number) => Buffer;

export declare const value: (
  lovelace: bigint,
  assets?: MidgardValue["assets"],
) => MidgardValue;

export declare const datum: (cborHex: string) => MidgardDatum;

export declare const plutusV3: (
  seed: number,
  length: number,
) => MidgardVersionedScript;

export declare const midgardV1: (
  seed: number,
  length: number,
) => MidgardVersionedScript;

export declare const nativeCardano: (seed: number) => MidgardVersionedScript;

export declare const redeemer: (
  purpose: MidgardRedeemerPurpose,
  index: number,
  redeemerCborHex: string,
  memory: number,
  steps: number,
) => MidgardRedeemerWitness;

export declare const mintPolicy: (
  seed: number,
  assets: readonly MidgardMintAsset[],
) => MidgardMintPolicyItem;

export declare const asset: (
  nameHex: string,
  quantity: number,
) => MidgardMintAsset;

export declare const FIXED_INDEX_BOUNDARIES: readonly number[];

export declare const DATUM_CANONICITY_BOUNDARIES: readonly (readonly [
  label: string,
  cborHex: string,
])[];

export declare const CARRIAGE_BOUNDARY_LENGTHS: readonly number[];

export declare const FIELD_PREIMAGE_LENGTHS: readonly number[];

export declare const FIELD_PREIMAGE_LENGTH_SOURCE: {
  readonly body: MidgardNativeTxBodyCanonical;
  readonly witnessSet: MidgardNativeTxWitnessSetCanonical;
};

export declare const STRADDLE_FIELD_INDEX: 1;
export declare const STRADDLE_BLOCK_ITEMS: number;
export declare const STRADDLE_REPEATS: number;
export declare const STRADDLE_ITEM_COUNT: number;
export declare const STRADDLE_ITEM_INDEX: number;
export declare const STRADDLE_OWNER: Buffer;
export declare const STRADDLE_TX_ID: Buffer;

export declare const straddleInputs: () => readonly MidgardTxInput[];

/** One labelled vector: a set of structured items for its group's field. */
export type FieldVectorV1 = {
  readonly label: string;
  readonly items: readonly unknown[];
};

/** One field's vectors, with the Aiken producer/decoder pair they pin. */
export type FieldVectorGroupV1 = {
  readonly fieldIndex: MidgardFieldItems["fieldIndex"];
  readonly aikenProducer: string;
  readonly aikenDecoder: string;
  readonly vectors: readonly FieldVectorV1[];
};

export declare const FIELD_VECTORS: readonly FieldVectorGroupV1[];

export declare const selectorFor: (
  group: FieldVectorGroupV1,
  vector: FieldVectorV1,
) => MidgardFieldItems;

export type { MidgardTxOutput };
