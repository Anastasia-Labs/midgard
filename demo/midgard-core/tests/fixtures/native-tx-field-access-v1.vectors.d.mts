/**
 * Types for `native-tx-field-access-v1.vectors.mjs`.
 *
 * Same arrangement as `native-tx-field-items-v1.vectors.d.mts`: the vectors
 * are authored as plain ESM so the generator can load them on bare `node`, and
 * this declaration file lets the vitest suite consume them with real types.
 * The `codec` parameter is the module namespace of whichever build of
 * `codec/native-tx-field-access` is under test.
 */

import type * as codecModule from "../../src/codec/native-tx-field-access.js";

type CodecModule = typeof codecModule;

export type NativeTxFieldAccessGoldenV1 = {
  readonly schema: string;
  readonly version: number;
  readonly specDocument: string;
  readonly generator: string;
  readonly constants: {
    readonly fieldCount: number;
    readonly maxFieldItemCount: number;
    readonly maxTransactionAggregateFieldBytes: number;
    readonly maxSpendInputsPreimageBytes: number;
    readonly maximumCardanoSpendRedeemerCount: number;
    readonly chunkBytesK: number;
    readonly maxTier1RedeemerPreimageBytes: number;
    readonly maxTier3ChunkCount: number;
    readonly spendInputItemBytes: number;
    readonly hash28ItemBytes: number;
    readonly addressWitnessItemBytes: number;
    readonly carriageConstructors: readonly string[];
    readonly viewConstructors: readonly string[];
  };
  readonly emptyFieldCommitmentHex: string;
  readonly strides: readonly number[];
  readonly arrayHeaders: readonly {
    readonly count: number;
    readonly headerHex: string;
    readonly headerLength: number;
  }[];
  readonly itemWrappers: readonly {
    readonly payloadLength: number;
    readonly wrapperHex: string;
  }[];
  readonly preimages: readonly {
    readonly label: string;
    readonly fieldIndex: number;
    readonly stride: number;
    readonly itemCount: number;
    readonly itemsHex: readonly string[];
    readonly preimageHex: string;
    readonly commitmentHex: string;
    readonly itemExtents: readonly {
      readonly offset: number;
      readonly length: number;
    }[];
  }[];
  readonly chunkCounts: readonly {
    readonly totalLength: number;
    readonly chunkCount: number;
  }[];
  readonly tier3Certificate: {
    readonly txIdHex: string;
    readonly ownerHex: string;
    readonly fieldIndex: number;
    readonly totalLength: number;
    readonly preimageBlockHex: string;
    readonly chunkLengths: readonly number[];
    readonly chunkDigestsHex: readonly string[];
    readonly fieldHashHex: string;
  };
  readonly certificateAssetName: {
    readonly assetNameHex: string;
    readonly byteLength: number;
    readonly ascii: string;
  };
};

export declare const repeatToLength: (block: Buffer, length: number) => Buffer;

export declare const TIER3_PREIMAGE_BLOCK: Buffer;

export declare const buildNativeTxFieldAccessGoldenV1: (
  codec: CodecModule,
) => NativeTxFieldAccessGoldenV1;
