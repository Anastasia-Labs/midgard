/**
 * Types for the part of `cek-builtin-cardano-v1.arguments.mjs` that the
 * TypeScript suites read. See `cek-core-step-v1.vectors.d.mts` for why a
 * `.d.mts` sits beside a `.mjs`. A name declared here that the module stops
 * exporting fails at runtime in `tests/fuzz/cek-step-fuzz.test.ts`, which
 * calls every one.
 */

/** The seeded generator every argument builder draws from. */
export type Rng = {
  readonly int: (bound: number) => number;
  readonly chance: (probability: number) => boolean;
  readonly pick: <T>(items: readonly T[]) => T;
};

/** A Plutus Data value in the channel's JSON argument encoding. */
export type CekBuiltinData =
  | { readonly i: string }
  | { readonly b: string }
  | { readonly l: readonly CekBuiltinData[] }
  | { readonly m: readonly (readonly [CekBuiltinData, CekBuiltinData])[] }
  | { readonly c: string; readonly f: readonly CekBuiltinData[] };

export declare const makeRng: (seed: number) => Rng;

export declare const randomData: (
  rng: Rng,
  maxDepth?: number,
) => CekBuiltinData;
