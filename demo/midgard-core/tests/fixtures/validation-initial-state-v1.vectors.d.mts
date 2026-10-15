/**
 * Types for `validation-initial-state-v1.vectors.mjs`.
 *
 * Same arrangement as `native-tx-vector-v1.vectors.d.mts`: the vectors are
 * authored as plain ESM so the generator can load them on bare `node`, and this
 * declaration file lets the vitest suite consume them with real types. The
 * `codec` parameter is the module namespace of whichever build is under test.
 */

import type * as codecModule from "../../src/index.js";

type CodecModule = typeof codecModule;

export type ValidationInitialStateHeaderV1 = {
  readonly label: string;
  readonly endTimeMs: string;
  readonly blockSlot: string;
  readonly expectedNetworkId: string;
  readonly minFeeA: string;
  readonly minFeeB: string;
  readonly contextCbor: string;
};

export type ValidationInitialStateTransactionV1 = {
  readonly label: string;
  readonly provisional: boolean;
  readonly canonicalTransactionCbor: string;
  readonly transactionId: string;
  readonly redeemersPreimageBytes: number;
  readonly compactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly fieldPreimageLengthsCbor: string;
};

export type ValidationInitialStateVectorV1 = {
  readonly label: string;
  readonly provisional: boolean;
  readonly transaction: string;
  readonly header: string;
  readonly eventKeyCbor: string;
  readonly preUtxosRoot: string;
  readonly stateCbor: string;
  readonly stateHash: string;
};

export type ValidationInitialStateGoldenV1 = {
  readonly schema: string;
  readonly version: number;
  readonly generator: string;
  readonly aikenModule: string;
  readonly profileId: string;
  readonly headers: readonly ValidationInitialStateHeaderV1[];
  readonly transactions: readonly ValidationInitialStateTransactionV1[];
  readonly vectors: readonly ValidationInitialStateVectorV1[];
};

export declare const l2TransactionEventKeyCbor: (
  transactionId: Uint8Array,
) => Buffer;

export declare const deriveValidationInitialStateVectorsV1: (
  codec: CodecModule,
) => ValidationInitialStateGoldenV1;
