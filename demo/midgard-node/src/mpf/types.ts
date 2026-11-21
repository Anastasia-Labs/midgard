/**
 * Shared MPF value, batch-operation, payload-entry, and proof types.
 */

import { Proof } from "@aiken-lang/merkle-patricia-forestry";

export type MpfBatchOp =
  | { readonly type: "insert"; readonly key: Buffer; readonly value: Buffer }
  | { readonly type: "delete"; readonly key: Buffer };

export type MpfInsertBatchOp = Extract<MpfBatchOp, { readonly type: "insert" }>;

export type MpfStoredValue = string | Record<string, unknown>;

export type MpfSerializableValue = { readonly serialise: () => MpfStoredValue };

export type MpfReadableValue = MpfStoredValue | MpfSerializableValue;

export type UtxoPayloadEntry = {
  readonly outref: Buffer;
  readonly output: Buffer;
};

export type MpfProof = {
  readonly key: Buffer;
  readonly proof: Proof;
  readonly cbor: Buffer;
  readonly json: unknown;
  readonly aiken: string;
};
