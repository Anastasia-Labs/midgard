/**
 * Types for the part of `cek-builtin-cardano-v1.cases.mjs` that the
 * TypeScript suites read. See `cek-core-step-v1.vectors.d.mts` for why a
 * `.d.mts` sits beside a `.mjs`. A name declared here that the module stops
 * exporting fails at runtime in `tests/fuzz/cek-step-fuzz.test.ts`, which
 * calls every one.
 */

import type {
  CekBuiltinData,
  Rng,
} from "./cek-builtin-cardano-v1.arguments.mjs";

/** An argument type in the channel's JSON argument encoding. */
export type CekBuiltinArgumentType =
  | "int"
  | "bs"
  | "str"
  | "unit"
  | "bool"
  | "data"
  | "g1"
  | "g2"
  | "ml"
  | { readonly list: CekBuiltinArgumentType }
  | {
      readonly pair: readonly [CekBuiltinArgumentType, CekBuiltinArgumentType];
    };

/** A Miller-loop expression: finalVerify's argument form. */
export type CekBuiltinMl =
  | { readonly loop: readonly [string, string] }
  | { readonly mul: readonly [CekBuiltinMl, CekBuiltinMl] };

/** One builtin argument: its type and its JSON-encoded value. */
export type CekBuiltinArgument = {
  readonly ty: CekBuiltinArgumentType;
  readonly v: unknown;
};

export type { CekBuiltinData, Rng };

export declare const randomArguments: (
  tag: number,
  rng: Rng,
) => readonly CekBuiltinArgument[];

export declare const edgeCases: (tag: number) => readonly {
  readonly note: string;
  readonly args: readonly CekBuiltinArgument[];
}[];
