/**
 * Types for `cek-write-bits-loop-v1.vectors.mjs` (plain ESM so the golden
 * generator loads it on bare `node`).
 */

export declare const pattern: (length: number, salt?: number) => Buffer;
export declare const spread: (
  count: number,
  length: number,
  salt?: number,
) => bigint[];

export type CekWriteBitsLoopStep = {
  readonly name: string;
  readonly length: number;
  readonly indices: readonly bigint[];
  readonly set: boolean;
  readonly input: Buffer;
};

export declare const steps: readonly CekWriteBitsLoopStep[];
