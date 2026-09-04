/**
 * `missing-signature` fault-proof family (Goal task `Q16`) — off-chain wire
 * twins.
 *
 * **Rule.** Every required signer of every committed accepted transaction must
 * be witnessed: `∀t ∈ Ledger, ∀h ∈ required_signers(t): ∃(v, s) ∈
 * addr_tx_wits(t): blake2b_224(v) == h`.
 *
 * **Violation.** A block commits an accepted transaction naming a required
 * signer (body field 4) whose witness is absent from the address-witness
 * collection (witness field 7). Single direction — wrongful acceptance only.
 *
 * The proof is a four-step computation thread:
 *
 * 1. bind the bad transaction to the block's counted `transactions_root` and
 *    forward the §2.5 anchor — the transaction id plus the `witness_set_hash`
 *    read off the block-committed compact structure (§3's id preimage is the
 *    body alone, so the id says nothing about the witness set);
 * 2. open body field 4 (`required_signers`) through the §8.8 door and select
 *    the accused signer hash by its fixed-28-byte-stride ordinal;
 * 3. lift the accused hash to its verification-key preimage
 *    (`blake2b_224(vkey) == hash`); and
 * 4. open witness field 7 (`address_witnesses`) through the door against the
 *    thread-anchored `witness_set_hash`, then walk the authenticated preimage
 *    in bounded batches, requiring the vkey to appear in no witness. Each
 *    non-terminal batch commits a canonical checkpoint into the thread datum;
 *    the terminal batch burns the thread and mints the permanent proof.
 *
 * Production catalogue category: `missingSignature` (`0000000e`). The
 * asset-name helper accepts the deployed category id so callers remain bound
 * to the manifest they are submitting against.
 *
 * This module is the strict TypeScript twin of
 * `onchain/aiken/lib/midgard/fraud-proofs/missing-signature/step-0{1..4}.ak`.
 * Field order in every `Data.Object` mirrors the aiken record declarations
 * 1:1 — the PlutusData encoding is positional, so re-ordering here would
 * silently produce redeemers the validators reject.
 */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { H32Schema, VerificationKeyHashSchema } from "../common.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  type MidgardAddressWitness as MidgardAddressWitnessData,
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
} from "./native.js";

/** Catalogue violation identifier adjudicated by this family. */
export const MISSING_SIGNATURE_VIOLATION_ID = "missing-signature" as const;

// ## Thread NFT asset name

/**
 * A missing-signature computation-thread token's asset name: the family's
 * deployed category id (4 bytes) followed by the challenged block's header hash.
 */
export const missingSignatureThreadTokenAssetName = (
  categoryId: string,
  challengedHeaderHash: string,
): string => {
  if (!/^[0-9a-f]{8}$/u.test(categoryId)) {
    throw new Error(
      "missing-signature category id must be 4 bytes of lowercase hex",
    );
  }
  if (!/^[0-9a-f]{56}$/u.test(challengedHeaderHash)) {
    throw new Error("challenged header hash must be 28 bytes of lowercase hex");
  }
  return `${categoryId}${challengedHeaderHash}`;
};

// ## Rule (twin of the on-chain step-03 lift and bounded step-04 walk)

/**
 * `blake2b_224` of a raw verification key, hex in and hex out — the exact
 * twin of `common/utils.get_verification_key_hash` (`utils.ak:783`), which
 * step-03 compares against the accused hash.
 *
 * Deliberately hashes whatever 32-byte value it is handed rather than parsing
 * it as an Ed25519 point: the on-chain twin hashes raw bytes, and a committed
 * garbage key must classify identically on both sides.
 */
export const missingSignatureVkeyHash = (verificationKey: string): string => {
  const bytes = Buffer.from(verificationKey, "hex");
  if (
    bytes.length !== 32 ||
    bytes.toString("hex") !== verificationKey.toLowerCase()
  ) {
    throw new Error(
      "missing-signature verification key must be 32 bytes of hex",
    );
  }
  return Buffer.from(blake2b(bytes, { dkLen: 28 })).toString("hex");
};

/**
 * The step-04 absence predicate: whether any address witness carries the
 * accused verification key. The validator applies the same comparison to one
 * bounded batch at a time. Byte equality on the key, never signature
 * verification — a *present but invalid* witness is `invalid-signature`'s
 * fault (Q15), not this family's.
 */
export const missingSignatureRequiredSignerIsPresent = ({
  verificationKey,
  addrTxWits,
}: {
  readonly verificationKey: string;
  readonly addrTxWits: readonly MidgardAddressWitnessData[];
}): boolean =>
  addrTxWits.some(
    (witness) =>
      witness.verification_key.toLowerCase() === verificationKey.toLowerCase(),
  );

/**
 * First required-signer ordinal (field 4's fixed 28-byte stride) whose hash
 * matches no witness's `blake2b_224(verification_key)`, or `null` when every
 * required signer is witnessed. This is presence-by-hash — the detection-side
 * classification — where the fold above is presence-by-key over an already
 * lifted accusation.
 */
export const findMissingRequiredSignerIndex = ({
  requiredSignerHashes,
  addrTxWits,
}: {
  readonly requiredSignerHashes: readonly string[];
  readonly addrTxWits: readonly MidgardAddressWitnessData[];
}): number | null => {
  const witnessKeyHashes = new Set(
    addrTxWits.map((witness) =>
      missingSignatureVkeyHash(witness.verification_key),
    ),
  );
  const index = requiredSignerHashes.findIndex(
    (hash) => !witnessKeyHashes.has(hash.toLowerCase()),
  );
  return index === -1 ? null : index;
};

/**
 * The adjudicated violation predicate over authenticated evidence: some
 * required signer of the committed transaction has no witness whose key
 * hashes to it.
 */
export const nativeTxHasMissingSignatureViolation = ({
  requiredSignerHashes,
  addrTxWits,
}: {
  readonly requiredSignerHashes: readonly string[];
  readonly addrTxWits: readonly MidgardAddressWitnessData[];
}): boolean =>
  findMissingRequiredSignerIndex({ requiredSignerHashes, addrTxWits }) !== null;

// ## Shared step aliases

export const MissingSignatureStepCancelSchema = FaultProofStepCancelSchema;
export type MissingSignatureStepCancel = FaultProofStepCancel;
export const MissingSignatureStepCancel =
  FaultProofStepCancel as unknown as MissingSignatureStepCancel;

// ## Step 01 — bind the bad transaction and anchor its witness-set hash
//
// The step-01 UTxO is the initialized fraud proof (its `data` is `None`), so
// it is read with the generic computation-thread step datum. `Args` is the
// bare `NativeTxInclusionArgs` — this step has no published-chunk carriage
// arm on-chain (plan §5, D3).

export const MissingSignatureStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type MissingSignatureStep01Datum = Data.Static<
  typeof MissingSignatureStep01DatumSchema
>;
export const MissingSignatureStep01Datum =
  asDataType<MissingSignatureStep01Datum>(MissingSignatureStep01DatumSchema);

export const MissingSignatureStep01ArgsSchema = NativeTxInclusionArgsSchema;
export type MissingSignatureStep01Args = NativeTxInclusionArgs;
export const MissingSignatureStep01Args =
  NativeTxInclusionArgs as unknown as MissingSignatureStep01Args;

export const MissingSignatureStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingSignatureStep01ArgsSchema);
export type MissingSignatureStep01SpendRedeemer = Data.Static<
  typeof MissingSignatureStep01SpendRedeemerSchema
>;
export const MissingSignatureStep01SpendRedeemer =
  asDataType<MissingSignatureStep01SpendRedeemer>(
    MissingSignatureStep01SpendRedeemerSchema,
  );

// ## Step 02 — open field 4 and select the accused required signer

/**
 * Mirrors `midgard/fraud_proofs/missing_signature/step_02.State`: the §2.5
 * anchor, whole. `verified_witness_set_hash` is the anchor's second half —
 * step-01 read it off the compact structure the block's counted
 * `transactions_root` committed, and step-04 needs it to open field 7, since
 * §3's transaction id commits the body alone.
 */
export const MissingSignatureStep02StateSchema = Data.Object({
  verified_tx_id: H32Schema,
  verified_witness_set_hash: H32Schema,
});
export type MissingSignatureStep02State = Data.Static<
  typeof MissingSignatureStep02StateSchema
>;
export const MissingSignatureStep02State =
  asDataType<MissingSignatureStep02State>(MissingSignatureStep02StateSchema);

export const MissingSignatureStep02DatumSchema = faultProofStepDatumSchema(
  MissingSignatureStep02StateSchema,
);
export type MissingSignatureStep02Datum = Data.Static<
  typeof MissingSignatureStep02DatumSchema
>;
export const MissingSignatureStep02Datum =
  asDataType<MissingSignatureStep02Datum>(MissingSignatureStep02DatumSchema);

/**
 * Mirrors `midgard/fraud_proofs/missing_signature/step_02.Args`.
 * `required_signers_opening` must be the `BodyFieldOpening` arm (field 4 is
 * the body's; the arm is derived by `fieldOpeningV1ForField`, never chosen).
 * The ordinal indexes field 4's fixed 28-byte stride; out-of-domain aborts
 * on-chain (`field_item_at`, §7.3 abort-never-clamp).
 */
export const MissingSignatureStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  required_signers_opening: FieldOpeningSchema,
  bad_required_signer_hash_index: Data.Integer(),
});
export type MissingSignatureStep02Args = Data.Static<
  typeof MissingSignatureStep02ArgsSchema
>;
export const MissingSignatureStep02Args =
  asDataType<MissingSignatureStep02Args>(MissingSignatureStep02ArgsSchema);

export const MissingSignatureStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingSignatureStep02ArgsSchema);
export type MissingSignatureStep02SpendRedeemer = Data.Static<
  typeof MissingSignatureStep02SpendRedeemerSchema
>;
export const MissingSignatureStep02SpendRedeemer =
  asDataType<MissingSignatureStep02SpendRedeemer>(
    MissingSignatureStep02SpendRedeemerSchema,
  );

// ## Step 03 — lift the accused hash to its verification-key preimage

/** Mirrors `midgard/fraud_proofs/missing_signature/step_03.State`. */
export const MissingSignatureStep03StateSchema = Data.Object({
  missing_required_signer_hash: VerificationKeyHashSchema,
  verified_tx_id: H32Schema,
  verified_witness_set_hash: H32Schema,
});
export type MissingSignatureStep03State = Data.Static<
  typeof MissingSignatureStep03StateSchema
>;
export const MissingSignatureStep03State =
  asDataType<MissingSignatureStep03State>(MissingSignatureStep03StateSchema);

export const MissingSignatureStep03DatumSchema = faultProofStepDatumSchema(
  MissingSignatureStep03StateSchema,
);
export type MissingSignatureStep03Datum = Data.Static<
  typeof MissingSignatureStep03DatumSchema
>;
export const MissingSignatureStep03Datum =
  asDataType<MissingSignatureStep03Datum>(MissingSignatureStep03DatumSchema);

/** Mirrors `midgard/fraud_proofs/missing_signature/step_03.Args`. */
export const MissingSignatureStep03ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  missing_required_signer_vkey: Data.Bytes({ minLength: 32, maxLength: 32 }),
});
export type MissingSignatureStep03Args = Data.Static<
  typeof MissingSignatureStep03ArgsSchema
>;
export const MissingSignatureStep03Args =
  asDataType<MissingSignatureStep03Args>(MissingSignatureStep03ArgsSchema);

export const MissingSignatureStep03SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingSignatureStep03ArgsSchema);
export type MissingSignatureStep03SpendRedeemer = Data.Static<
  typeof MissingSignatureStep03SpendRedeemerSchema
>;
export const MissingSignatureStep03SpendRedeemer =
  asDataType<MissingSignatureStep03SpendRedeemer>(
    MissingSignatureStep03SpendRedeemerSchema,
  );

// ## Step 04 — open field 7 and prove the witness absent

/** Mirrors `midgard/fraud_proofs/missing_signature/step_04.State`. */
export const MissingSignatureStep04StateSchema = Data.Object({
  missing_required_signer_vkey: Data.Bytes({ minLength: 32, maxLength: 32 }),
  verified_tx_id: H32Schema,
  verified_witness_set_hash: H32Schema,
  // Exactly empty at entry or a 32-byte checkpoint digest thereafter. Lucid's
  // schema language cannot express that disjoint byte-length set; submitters
  // validate it fail-closed before construction.
  field_walk_checkpoint_hash: Data.Bytes(),
});
export type MissingSignatureStep04State = Data.Static<
  typeof MissingSignatureStep04StateSchema
>;
export const MissingSignatureStep04State =
  asDataType<MissingSignatureStep04State>(MissingSignatureStep04StateSchema);

export const MissingSignatureStep04DatumSchema = faultProofStepDatumSchema(
  MissingSignatureStep04StateSchema,
);
export type MissingSignatureStep04Datum = Data.Static<
  typeof MissingSignatureStep04DatumSchema
>;
export const MissingSignatureStep04Datum =
  asDataType<MissingSignatureStep04Datum>(MissingSignatureStep04DatumSchema);

/**
 * Mirrors `midgard/fraud_proofs/missing_signature/step_04.Args`.
 * `addr_tx_wits_opening` must be the `WitnessFieldOpening` arm — it carries
 * the transaction's `NativeTxWitnessSetCompact`, which the door re-derives
 * against the **thread-anchored** `verified_witness_set_hash` before reading
 * anything (the whole security of the step: §3's id commits the body alone,
 * so an invented witness-set tail re-derives to the genuine id).
 */
export const MissingSignatureStep04ArgsSchema = Data.Enum([
  Data.Object({
    Scan: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      addr_tx_wits_opening: FieldOpeningSchema,
      checkpoint_cbor: Data.Nullable(Data.Bytes()),
    }),
  }),
  Data.Object({
    Finalize: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      fraud_proof_mint_redeemer_index: Data.Integer(),
      addr_tx_wits_opening: FieldOpeningSchema,
      checkpoint_cbor: Data.Nullable(Data.Bytes()),
    }),
  }),
]);
export type MissingSignatureStep04Args = Data.Static<
  typeof MissingSignatureStep04ArgsSchema
>;
export const MissingSignatureStep04Args =
  asDataType<MissingSignatureStep04Args>(MissingSignatureStep04ArgsSchema);

export const MissingSignatureStep04SpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingSignatureStep04ArgsSchema);
export type MissingSignatureStep04SpendRedeemer = Data.Static<
  typeof MissingSignatureStep04SpendRedeemerSchema
>;
export const MissingSignatureStep04SpendRedeemer =
  asDataType<MissingSignatureStep04SpendRedeemer>(
    MissingSignatureStep04SpendRedeemerSchema,
  );

// ## Step-04 deterministic field-7 checkpoint

/** Fixed-width §5.3 item size: 32-byte key + 64-byte signature, CBOR-wrapped. */
export const MISSING_SIGNATURE_ADDRESS_WITNESS_STRIDE = 103;

/** Number of authenticated witnesses consumed by every non-terminal scan. */
export const MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE = 32;

/** ASCII `MidgardFieldWalkCheckpointV1`, matching the Aiken walk core. */
const FIELD_WALK_CHECKPOINT_DOMAIN = Buffer.from(
  "MidgardFieldWalkCheckpointV1",
  "ascii",
);

export type MissingSignatureFieldWalkCheckpoint = {
  readonly checkpointCbor: string;
  readonly checkpointHash: string;
  readonly nextItemIndex: number;
  readonly nextOffset: number;
  readonly itemCount: number;
  readonly totalLength: number;
};

const requireU24 = (value: number, label: string): Buffer => {
  if (!Number.isSafeInteger(value) || value < 0 || value > 0xff_ffff) {
    throw new Error(`${label} must fit the checkpoint's unsigned 24-bit word`);
  }
  const encoded = Buffer.alloc(3);
  encoded.writeUIntBE(value, 0, 3);
  return encoded;
};

const fieldArrayHeaderLength = (itemCount: number): number => {
  if (!Number.isSafeInteger(itemCount) || itemCount < 0 || itemCount > 0xffff) {
    throw new Error(
      "missing-signature witness count is outside the §5.1 array domain",
    );
  }
  return itemCount <= 23 ? 1 : itemCount <= 0xff ? 2 : 3;
};

/**
 * Reconstruct the Aiken core's canonical 53-byte checkpoint for field 7.
 * Field 7 has a fixed stride, so its byte offset is an O(1) function of the
 * authenticated count and cursor; no prover-supplied arithmetic is trusted.
 */
export const missingSignatureFieldWalkCheckpoint = ({
  txId,
  itemCount,
  totalLength,
  nextItemIndex,
}: {
  readonly txId: string;
  readonly itemCount: number;
  readonly totalLength: number;
  readonly nextItemIndex: number;
}): MissingSignatureFieldWalkCheckpoint => {
  if (!/^[0-9a-f]{64}$/u.test(txId)) {
    throw new Error(
      "missing-signature checkpoint transaction id must be 32-byte lowercase hex",
    );
  }
  if (
    !Number.isSafeInteger(nextItemIndex) ||
    nextItemIndex < 0 ||
    nextItemIndex > itemCount
  ) {
    throw new Error(
      "missing-signature checkpoint cursor is outside the witness collection",
    );
  }
  const headerLength = fieldArrayHeaderLength(itemCount);
  const expectedLength =
    headerLength + itemCount * MISSING_SIGNATURE_ADDRESS_WITNESS_STRIDE;
  if (
    totalLength !== expectedLength ||
    totalLength <= 0 ||
    totalLength > 32_768
  ) {
    throw new Error(
      `missing-signature field-7 length ${totalLength.toString()} does not match canonical ${expectedLength.toString()}`,
    );
  }
  const nextOffset =
    headerLength + nextItemIndex * MISSING_SIGNATURE_ADDRESS_WITNESS_STRIDE;
  const checkpoint = Buffer.concat([
    Buffer.from([0x86, 0x58, 0x20]),
    Buffer.from(txId, "hex"),
    Buffer.from([0x41, 0x07, 0x43]),
    requireU24(totalLength, "missing-signature field-7 length"),
    Buffer.from([0x43]),
    requireU24(itemCount, "missing-signature witness count"),
    Buffer.from([0x43]),
    requireU24(nextItemIndex, "missing-signature checkpoint cursor"),
    Buffer.from([0x43]),
    requireU24(nextOffset, "missing-signature checkpoint offset"),
  ]);
  if (checkpoint.length !== 53) {
    throw new Error(
      "missing-signature checkpoint encoder produced a non-canonical length",
    );
  }
  return {
    checkpointCbor: checkpoint.toString("hex"),
    checkpointHash: Buffer.from(
      blake2b(Buffer.concat([FIELD_WALK_CHECKPOINT_DOMAIN, checkpoint]), {
        dkLen: 32,
      }),
    ).toString("hex"),
    nextItemIndex,
    nextOffset,
    itemCount,
    totalLength,
  };
};

/**
 * Resolve a thread-carried checkpoint digest to the only cursor the fixed
 * batch schedule can have produced. Empty means the initial position.
 */
export const resolveMissingSignatureFieldWalkCheckpoint = ({
  txId,
  itemCount,
  totalLength,
  committedHash,
}: {
  readonly txId: string;
  readonly itemCount: number;
  readonly totalLength: number;
  readonly committedHash: string;
}): MissingSignatureFieldWalkCheckpoint | null => {
  // Validate the authenticated field shape even at the initial position.
  missingSignatureFieldWalkCheckpoint({
    txId,
    itemCount,
    totalLength,
    nextItemIndex: 0,
  });
  if (committedHash === "") return null;
  if (!/^[0-9a-f]{64}$/u.test(committedHash)) {
    throw new Error(
      "missing-signature checkpoint commitment must be empty or 32-byte lowercase hex",
    );
  }
  for (
    let cursor = MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE;
    cursor < itemCount;
    cursor += MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE
  ) {
    const candidate = missingSignatureFieldWalkCheckpoint({
      txId,
      itemCount,
      totalLength,
      nextItemIndex: cursor,
    });
    if (candidate.checkpointHash === committedHash) return candidate;
  }
  throw new Error(
    "missing-signature checkpoint commitment is not reachable by the deterministic field-7 scan schedule",
  );
};

// ## Step-state builder (twin of the on-chain forwarding rule)

/** Exactly the state `step-01` writes for `step-02`: the §2.5 anchor. */
export const missingSignatureStep02StateFromVerifiedTx = ({
  verifiedTxId,
  verifiedWitnessSetHash,
}: {
  readonly verifiedTxId: string;
  /**
   * The `witness_set_hash` read off the compact structure the block's
   * `transactions_root` committed — **not** field 7's own commitment, and not
   * a value any later redeemer supplies. It is the second half of
   * `WitnessAnchor`, and the only reason step-04 can open field 7 at all.
   */
  readonly verifiedWitnessSetHash: string;
}): MissingSignatureStep02State => ({
  verified_tx_id: verifiedTxId.toLowerCase(),
  verified_witness_set_hash: verifiedWitnessSetHash.toLowerCase(),
});
