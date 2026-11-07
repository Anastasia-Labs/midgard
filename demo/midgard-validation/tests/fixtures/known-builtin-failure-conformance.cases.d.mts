/**
 * Types for `known-builtin-failure-conformance.cases.mjs`, authored as plain
 * ESM so the generator can load it on bare `node`. See
 * `cek-core-step-v1.vectors.d.mts` for why a `.d.mts` beside a `.mjs`. A name
 * here that no longer exists in the module degrades silently to `any`, so
 * `tests/known-builtin-failure-conformance.golden.test.ts` checks the export
 * list against the real module.
 */

import type { MidgardCekDirectValueWitness } from "../../src/cek-builtin.argument-kinds.js";

/** The tags `known_builtin_failure_v1` can answer true for. */
export declare const KNOWN_BUILTIN_FAILURE_TAGS: readonly number[];

export type KnownBuiltinFailureVerdict = "known" | "not-known" | "refused";

export declare const VERDICTS: readonly KnownBuiltinFailureVerdict[];

export type KnownBuiltinFailureConformanceCase = {
  readonly tag: number;
  readonly name: string;
  readonly arguments: readonly MidgardCekDirectValueWitness[];
};

/** A witness in the fixture's JSON form: hex bytes, decimal integers. */
export type KnownBuiltinFailureConformanceArgument =
  | {
      readonly kind: "constant";
      readonly type: string;
      readonly payload: string;
    }
  | {
      readonly kind: "semanticConstant";
      readonly type: string;
      readonly root: string;
      readonly cborLength: string;
      readonly payloadMemory: string;
      readonly memory: string;
    }
  | { readonly kind: "opaque"; readonly root: string }
  | { readonly kind: "blsMillerLoop"; readonly root: string };

export type KnownBuiltinFailureConformanceRow = {
  readonly case: number;
  readonly tag: number;
  readonly name: string;
  readonly arguments: readonly KnownBuiltinFailureConformanceArgument[];
  readonly verdict: KnownBuiltinFailureVerdict;
};

export declare const knownBuiltinFailureConformanceCases: () => KnownBuiltinFailureConformanceCase[];

export declare const encodeConformanceArgument: (
  value: MidgardCekDirectValueWitness,
) => KnownBuiltinFailureConformanceArgument;

export declare const buildKnownBuiltinFailureConformanceRows: (
  knownFailure: (
    tag: bigint,
    arguments_: readonly MidgardCekDirectValueWitness[],
  ) => boolean,
) => KnownBuiltinFailureConformanceRow[];
