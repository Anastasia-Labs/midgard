/**
 * Types for `native-tx-vector-v1.vectors.mjs`.
 *
 * Same arrangement, and for the same reason, as
 * `native-tx-field-items-v1.vectors.d.mts`: the vector data is authored as plain
 * ESM so the generator can load it on bare `node` before any TypeScript exists,
 * and this declaration file is what lets the vitest suite consume it with real
 * types instead of `any`.
 *
 * The `codec` parameter is deliberately the *module namespace* of whichever build
 * is under test. Both call sites pass their own — the generator passes `dist/`,
 * the suite passes `src/` — which is exactly what makes one definition able to
 * check two builds against each other. Typing it as the `src/` namespace states
 * the contract the `dist/` build must also satisfy.
 */

import type * as codecModule from "../../src/index.js";

type CodecModule = typeof codecModule;

/** The nine-field golden canonical transaction. */
export declare const nativeTxVectorCanonicalV1: (
  codec: CodecModule,
) => Parameters<CodecModule["materializeMidgardNativeTxFromCanonicalV1"]>[0];

/** The same transaction with nine distinct, ascending field lengths. */
export declare const nativeTxVectorOrderedLengthCanonicalV1: (
  codec: CodecModule,
) => Parameters<CodecModule["materializeMidgardNativeTxFromCanonicalV1"]>[0];

export declare const NATIVE_TX_VECTOR_ORDERED_LENGTH_TUPLE_V1: readonly number[];

/**
 * Every value the `n01`–`n09` vectors pin. Hex strings except the two counts, so
 * the JSON fixture, the vitest expectation and the Aiken constants are all the
 * same spelling.
 */
export type NativeTxVectorV1 = {
  readonly bodyCanonical: string;
  readonly bodyCompact: string;
  readonly witnessPreimages: string;
  readonly witnessCompact: string;
  readonly compact: string;
  readonly canonical: string;
  readonly transactionId: string;
  readonly fullHash: string;
  readonly proofCompact: string;
  readonly proofWitnessCompact: string;
  readonly proofLengths: string;
  readonly proofSource: string;
  readonly proofCommitment: string;
  readonly canonicalSize: number;
  readonly orderedLengthTuple: string;
  readonly derivedOrderedLengthTuple: string;
};

export declare const deriveNativeTxVectorV1: (
  codec: CodecModule,
) => NativeTxVectorV1;
