/**
 * Types for `cek-serialise-loop-v1.vectors.mjs` (plain ESM so the golden
 * generator loads it on bare `node`).
 */

export declare const head: (major: number, value: number | bigint) => string;
export declare const int: (value: number | bigint) => string;
export declare const filler: (length: number, seed?: number) => string;
export declare const bytesOf: (payload: string) => string;
export declare const bytes: (length: number, seed?: number) => string;
export declare const bignum: (length: number) => string;
export declare const list: (items: readonly string[]) => string;
export declare const constr: (
  index: number,
  fields: readonly string[],
) => string;
export declare const map: (
  pairs: readonly (readonly [string, string])[],
) => string;

export type CekSerialiseLoopPart = string | readonly [string, number];

export declare const expand: (parts: readonly CekSerialiseLoopPart[]) => string;

export type CekSerialiseLoopVector = {
  readonly name: string;
  readonly parts: readonly CekSerialiseLoopPart[];
  readonly hex: string;
  readonly maximum: boolean;
};

export declare const values: readonly CekSerialiseLoopVector[];
