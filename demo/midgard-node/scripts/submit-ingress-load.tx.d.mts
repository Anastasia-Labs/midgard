import type { CML } from "@lucid-evolution/lucid";

export declare const LOAD_TX_NETWORK_ID: bigint;
export declare const LOAD_GENESIS_OUTPUT_LOVELACE: bigint;

export type LoadKey = {
  readonly privateKey: CML.PrivateKey;
  readonly keyHash: Buffer;
  readonly addressBytes: Buffer;
};

export type LoadInput = {
  readonly key: LoadKey;
  readonly lovelace: bigint;
  readonly outRef: Buffer;
};

export type LoadTx = {
  readonly txId: Buffer;
  readonly txCbor: Buffer;
  readonly fee: bigint;
  readonly outputs: readonly Buffer[];
};

export declare const makeLoadKey: (seed: string, index: number) => LoadKey;
export declare const makeLoadOutput: (
  addressBytes: Uint8Array,
  lovelace: bigint,
) => Buffer;
export declare const loadOutRef: (
  txId: Uint8Array,
  outputIndex: number,
) => Buffer;
export declare const buildLoadTx: (options: {
  readonly inputs: readonly LoadInput[];
  readonly outputAddresses: readonly Uint8Array[];
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly feeOverride?: bigint;
  readonly signedTxId?: Buffer;
}) => LoadTx;
export declare const buildLoadTxShape: (options: {
  readonly inputCount: number;
  readonly outputCount: number;
  readonly nonce?: number;
  readonly minFeeA?: bigint;
  readonly minFeeB?: bigint;
}) => LoadTx & { readonly inputs: readonly LoadInput[] };
