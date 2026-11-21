/**
 * Types for `plutus-data-cbor-decoding-v1.vectors.mjs`, authored as plain ESM
 * so the generator can load it on bare `node`. See
 * `cek-core-step-v1.vectors.d.mts` for why a `.d.mts` beside a `.mjs`. A name
 * here that no longer exists in the module degrades silently to `any`, so
 * `tests/plutus-data-cbor-decoding-v1.golden.test.ts` checks the export list
 * against the real module.
 */

import type { Data } from "../../src/uplc-data.js";

export declare const REFUSED: "refused";

/** Input hex and the plutus-core 1.66 `decodeData` outcome, or `REFUSED`. */
export declare const PLUTUS_DATA_CARDANO_EDGE_VECTORS: readonly (readonly [
  string,
  string,
])[];

export declare const ENVELOPE_ONLY_VECTORS: readonly (readonly [
  string,
  string,
])[];

export declare const ENVELOPE_REFUSED_VECTORS: readonly string[];

export declare const ENVELOPE_WRITER_LENGTHS: readonly number[];

export declare const plutusDataCborDecodingValues: () => Data[];

export declare const plutusDataCborDecodingCarried: () => Data;

/** One row: the input, its origin, and each entry's re-serialisation or `null`. */
export type PlutusDataCborDecodingRowV1 = {
  readonly input: string;
  readonly source: "edge" | "written" | "mutated";
  readonly data: string | null;
  readonly envelope: string | null;
};

export declare const buildPlutusDataCborDecodingRows: (implementations: {
  readonly decodeData: (bytes: Uint8Array) => Data;
  readonly decodeEnvelope: (bytes: Uint8Array) => Data;
  readonly encodeData: (value: Data) => Uint8Array;
  readonly encodeCborBytes: (bytes: Uint8Array) => Uint8Array;
  readonly compactCarriageCbor: (canonicalHex: string) => string;
}) => PlutusDataCborDecodingRowV1[];
