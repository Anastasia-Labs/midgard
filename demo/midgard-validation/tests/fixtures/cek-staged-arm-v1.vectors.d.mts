/**
 * Types for `cek-staged-arm-v1.vectors.mjs`.
 *
 * The rows are authored as plain ESM so the generator can load them on bare
 * `node`; this file lets the vitest suite consume them with the types the
 * validation package exposes. A name here that no longer exists in the
 * `.mjs` degrades silently to `any` under `skipLibCheck`, so
 * `tests/cek-staged-arm-goldens.test.ts` drives every exported value through
 * `src/` and compares it with the checked-in fixture.
 */

import type { MidgardCekStagedReferenceInput } from "@al-ft/midgard-core";

import type { MidgardCekCoreStepWitness } from "../../src/cek-machine.js";
import type { MidgardCekStagedConstant } from "../../src/cek-staged-builtin.js";

export type CekStagedBytesSpec =
  | { readonly pattern: readonly [number, number] }
  | { readonly text: number }
  | { readonly hex: string }
  | { readonly repeat: readonly [number, number] }
  | { readonly concat: readonly CekStagedBytesSpec[] }
  | { readonly flipLast: CekStagedBytesSpec }
  | { readonly integerCbor: CekStagedIntegerSpec };

export type CekStagedIntegerSpec = {
  readonly negative: boolean;
  readonly argument: CekStagedBytesSpec;
};

export type CekStagedSourceSpec =
  | { readonly inline: CekStagedBytesSpec }
  | { readonly staged: number }
  | { readonly bytesLeaf: CekStagedBytesSpec }
  | { readonly integerLeaf: CekStagedIntegerSpec }
  | { readonly small: bigint };

export type CekStagedArmRow = {
  readonly name: string;
  readonly accepts: boolean;
  readonly tag: number;
  readonly arguments: readonly MidgardCekStagedConstant[];
  readonly result: MidgardCekStagedConstant;
  readonly staged: readonly CekStagedBytesSpec[];
  readonly sources: readonly CekStagedSourceSpec[];
};

export type CekStagedArmBuilt = {
  readonly witness: Extract<
    MidgardCekCoreStepWitness,
    { readonly kind: "executeBuiltinStaged" }
  >;
  readonly referenceInputs: readonly MidgardCekStagedReferenceInput[];
  readonly witnessHash: string;
  readonly verdict: boolean;
};

export declare const CEK_STAGED_ARM_ROWS: readonly CekStagedArmRow[];
export declare const bytesOf: (spec: CekStagedBytesSpec) => Buffer;
export declare const valueOf: (integer: CekStagedIntegerSpec) => bigint;
export declare const integerCbor: (value: bigint) => Buffer;
export declare const fixtureReference: (seed: number) => {
  readonly transactionId: Buffer;
  readonly outputIndex: number;
};
export declare const buildCekStagedArmRow: (
  row: CekStagedArmRow,
  api: object,
) => CekStagedArmBuilt;
