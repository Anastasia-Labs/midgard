/**
 * `da-hash-preimage` fault-proof family (Goal task `Q44`).
 *
 * **Rule.** Every leaf of a committed block's `transactions_root` is a
 * `(key, value)` pair whose key is the canonical native-V1 transaction id of
 * that value.
 *
 * **Violation `da-hash-preimage`.** A committed leaf whose key is not the
 * hash-preimage commitment of its own value. Such a leaf can never be opened
 * by any other family — every native family runs the codec precondition
 * `native_tx_id_for_version(version, body_cbor) == key`
 * (`onchain/aiken/lib/midgard/fraud-proofs/native-tx/compact.ak:459`) — so the
 * block hides an undisputable transaction. The catalogue records the row's
 * severity as *provability*
 * (`docs/fault-proofs/catalogue-status.md:177`,
 * `docs/fault-proofs/coverage-matrix.md:174`,
 * `technical-spec/1-ledger-state/6-transaction.tex:175`).
 *
 * The evidence is decided without decoding the leaf: the canonical compact
 * encoding is fixed-framed at both ends
 * (`encode_native_tx_compact_for_version`), so an honest leaf's body preimage
 * is exactly `value[2 .. len - 35]`. This module is the strict TypeScript twin
 * of `onchain/aiken/lib/midgard/fraud-proofs/da-hash-preimage/rule.ak`; the
 * cross-language vectors live in
 * `demo/midgard-sdk/tests/da-hash-preimage-v1.test.ts`.
 */
import { computeHash32, encodeCbor } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "@/common.js";

import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
} from "./native.js";

/** Catalogue violation identifier adjudicated by this family. */
export const DA_HASH_PREIMAGE_VIOLATION_ID_V1 = "da-hash-preimage" as const;

/** `0x84` array header plus the single-byte canonical version. */
export const DA_HASH_PREIMAGE_COMPACT_V1_HEAD_BYTE_COUNT = 2;

/**
 * Definite 32-byte witness-set hash (`0x58 0x20` + 32) plus the single-byte
 * canonical validity code (bounded to `0..=5`).
 */
export const DA_HASH_PREIMAGE_COMPACT_V1_TAIL_BYTE_COUNT = 35;

/** Shortest byte length a canonical compact leaf can have. */
export const DA_HASH_PREIMAGE_COMPACT_V1_FRAME_BYTE_COUNT =
  DA_HASH_PREIMAGE_COMPACT_V1_HEAD_BYTE_COUNT +
  DA_HASH_PREIMAGE_COMPACT_V1_TAIL_BYTE_COUNT;

const NATIVE_TX_BODY_V1_DOMAIN = Buffer.from("MidgardNativeTxBodyV1", "ascii");
const NATIVE_TX_VERSION_V1 = 1n;

// ## Rule (byte-for-byte twin of `rule.ak`)

/**
 * The body-preimage slice an honest committed leaf hashes to its key. Total:
 * leaves shorter than the canonical frame clamp to the empty slice and are
 * convicted by {@link committedLeafIsUnderframedV1}.
 */
export const committedLeafBodyCborV1 = (
  committedLeafValue: Uint8Array,
): Buffer => {
  const bodyByteCount =
    committedLeafValue.length - DA_HASH_PREIMAGE_COMPACT_V1_FRAME_BYTE_COUNT;
  if (bodyByteCount <= 0) {
    return Buffer.alloc(0);
  }
  return Buffer.from(
    committedLeafValue.subarray(
      DA_HASH_PREIMAGE_COMPACT_V1_HEAD_BYTE_COUNT,
      DA_HASH_PREIMAGE_COMPACT_V1_HEAD_BYTE_COUNT + bodyByteCount,
    ),
  );
};

/**
 * Canonical native-V1 transaction id the committed leaf value actually
 * commits to. Never throws.
 */
export const deriveCommittedLeafTxIdV1 = (
  committedLeafValue: Uint8Array,
): Buffer =>
  computeHash32(
    Buffer.concat([
      NATIVE_TX_BODY_V1_DOMAIN,
      encodeCbor(NATIVE_TX_VERSION_V1),
      committedLeafBodyCborV1(committedLeafValue),
    ]),
  );

/** A leaf too short to carry the canonical frame cannot be a transaction. */
export const committedLeafIsUnderframedV1 = (
  committedLeafByteCount: number,
): boolean =>
  committedLeafByteCount < DA_HASH_PREIMAGE_COMPACT_V1_FRAME_BYTE_COUNT;

/** The adjudicated violation predicate, over the committed evidence triple. */
export const isDaHashPreimageViolationV1 = ({
  committedTxId,
  derivedTxId,
  committedLeafByteCount,
}: {
  readonly committedTxId: string;
  readonly derivedTxId: string;
  readonly committedLeafByteCount: number;
}): boolean =>
  committedLeafIsUnderframedV1(committedLeafByteCount) ||
  derivedTxId !== committedTxId;

/**
 * Canonical evidence record for one committed leaf: exactly the triple
 * step 01 pins into the computation thread, plus the authenticated bytes it
 * was derived from.
 */
export type DaHashPreimageEvidenceV1 = {
  readonly violationId: typeof DA_HASH_PREIMAGE_VIOLATION_ID_V1;
  readonly committedTxId: string;
  readonly committedLeafValueCbor: string;
  readonly derivedTxId: string;
  readonly committedLeafByteCount: number;
  readonly isViolation: boolean;
};

/**
 * Builds the evidence record for a committed `(key, value)` leaf. The caller
 * must have authenticated the pair against the header's counted
 * `transactions_root`; this function performs no I/O and never throws.
 */
export const daHashPreimageEvidenceFromCommittedLeafV1 = ({
  committedTxId,
  committedLeafValue,
}: {
  readonly committedTxId: string;
  readonly committedLeafValue: Uint8Array;
}): DaHashPreimageEvidenceV1 => {
  const derivedTxId =
    deriveCommittedLeafTxIdV1(committedLeafValue).toString("hex");
  const normalizedCommittedTxId = committedTxId.toLowerCase();
  return Object.freeze({
    violationId: DA_HASH_PREIMAGE_VIOLATION_ID_V1,
    committedTxId: normalizedCommittedTxId,
    committedLeafValueCbor: Buffer.from(committedLeafValue).toString("hex"),
    derivedTxId,
    committedLeafByteCount: committedLeafValue.length,
    isViolation: isDaHashPreimageViolationV1({
      committedTxId: normalizedCommittedTxId,
      derivedTxId,
      committedLeafByteCount: committedLeafValue.length,
    }),
  });
};

// ## On-chain schemas (positional agreement with the Aiken step modules)

export const DaHashPreimageStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type DaHashPreimageStep01Datum = Data.Static<
  typeof DaHashPreimageStep01DatumSchema
>;
export const DaHashPreimageStep01Datum =
  DaHashPreimageStep01DatumSchema as unknown as DaHashPreimageStep01Datum;

export const DaHashPreimageStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeTxInclusionArgsSchema);
export type DaHashPreimageStep01SpendRedeemer = Data.Static<
  typeof DaHashPreimageStep01SpendRedeemerSchema
>;
export const DaHashPreimageStep01SpendRedeemer =
  DaHashPreimageStep01SpendRedeemerSchema as unknown as DaHashPreimageStep01SpendRedeemer;

/** Mirrors `midgard/fraud_proofs/da_hash_preimage/step_02.State`. */
export const DaHashPreimageStep02StateSchema = Data.Object({
  committed_tx_id: H32Schema,
  derived_tx_id: H32Schema,
  committed_leaf_byte_count: Data.Integer(),
});
export type DaHashPreimageStep02State = Data.Static<
  typeof DaHashPreimageStep02StateSchema
>;
export const DaHashPreimageStep02State =
  DaHashPreimageStep02StateSchema as unknown as DaHashPreimageStep02State;

export const DaHashPreimageStep02DatumSchema = faultProofStepDatumSchema(
  DaHashPreimageStep02StateSchema,
);
export type DaHashPreimageStep02Datum = Data.Static<
  typeof DaHashPreimageStep02DatumSchema
>;
export const DaHashPreimageStep02Datum =
  DaHashPreimageStep02DatumSchema as unknown as DaHashPreimageStep02Datum;

export const DaHashPreimageStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type DaHashPreimageStep02Args = Data.Static<
  typeof DaHashPreimageStep02ArgsSchema
>;
export const DaHashPreimageStep02Args =
  DaHashPreimageStep02ArgsSchema as unknown as DaHashPreimageStep02Args;

export const DaHashPreimageStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(DaHashPreimageStep02ArgsSchema);
export type DaHashPreimageStep02SpendRedeemer = Data.Static<
  typeof DaHashPreimageStep02SpendRedeemerSchema
>;
export const DaHashPreimageStep02SpendRedeemer =
  DaHashPreimageStep02SpendRedeemerSchema as unknown as DaHashPreimageStep02SpendRedeemer;

/** Step-01 args are the shared native inclusion args; the key/value pair they
 * carry is deliberately *not* required to satisfy the native codec. */
export const DaHashPreimageTxInclusionArgsSchema = NativeTxInclusionArgsSchema;
export type DaHashPreimageTxInclusionArgs = NativeTxInclusionArgs;
export const DaHashPreimageTxInclusionArgs = NativeTxInclusionArgs;

export const DaHashPreimageStepCancelSchema = FaultProofStepCancelSchema;
export type DaHashPreimageStepCancel = FaultProofStepCancel;
export const DaHashPreimageStepCancel = FaultProofStepCancel;

/**
 * Builds the step-02 state exactly as the on-chain step-01 validator derives
 * it, so an off-chain builder and the L1 verifier cannot drift.
 */
export const daHashPreimageStep02StateFromEvidenceV1 = (
  evidence: DaHashPreimageEvidenceV1,
): DaHashPreimageStep02State => ({
  committed_tx_id: evidence.committedTxId,
  derived_tx_id: evidence.derivedTxId,
  committed_leaf_byte_count: BigInt(evidence.committedLeafByteCount),
});
