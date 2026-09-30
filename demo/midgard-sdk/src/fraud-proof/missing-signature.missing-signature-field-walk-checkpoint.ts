import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  H32Schema,
  OutputReferenceSchema,
  VerificationKeyHashSchema,
} from "../common.js";
import { ForcedInclusionTxV1Schema, HeaderSchema } from "../ledger-state.js";
import { rootMembershipProofSchema } from "../transition-trace.js";
import { FieldOpeningSchema } from "./field-opening.js";
import { MissingSignatureStep02State } from "./missing-signature.find-missing-required-signer-index.js";
import {
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
} from "./native.js";

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

// Forced rejection: exact source/reason binding, signer coordinate, genuine witness.
export const MissingSignatureForcedStepArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  header: HeaderSchema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxV1Schema,
  ),
  direction: Data.Integer(),
});

export const MissingSignatureForcedSignerStateSchema = Data.Object({
  verified_tx_id: H32Schema,
  verified_witness_set_hash: H32Schema,
  forced_source_key: Data.Bytes(),
  signer_index: Data.Integer(),
});

export const MissingSignatureForcedSignerArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  required_signers_opening: FieldOpeningSchema,
});

export const MissingSignatureForcedWitnessStateSchema = Data.Object({
  verified_tx_id: H32Schema,
  verified_witness_set_hash: H32Schema,
  forced_source_key: Data.Bytes(),
  signer_index: Data.Integer(),
  required_signer_hash: Data.Nullable(VerificationKeyHashSchema),
});

export const MissingSignatureForcedWitnessArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  addr_tx_wits_opening: Data.Nullable(FieldOpeningSchema),
  witness_index: Data.Integer(),
});

export const MissingSignatureForcedStepDatumSchema = faultProofStepDatumSchema(
  Data.Integer(),
);

export const MissingSignatureForcedStepSpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingSignatureForcedStepArgsSchema);

export type MissingSignatureForcedStepArgs = Data.Static<
  typeof MissingSignatureForcedStepArgsSchema
>;

export const MissingSignatureForcedStepArgs =
  asDataType<MissingSignatureForcedStepArgs>(
    MissingSignatureForcedStepArgsSchema,
  );

export type MissingSignatureForcedStepDatum = Data.Static<
  typeof MissingSignatureForcedStepDatumSchema
>;

export const MissingSignatureForcedStepDatum =
  asDataType<MissingSignatureForcedStepDatum>(
    MissingSignatureForcedStepDatumSchema,
  );

export type MissingSignatureForcedStepSpendRedeemer = Data.Static<
  typeof MissingSignatureForcedStepSpendRedeemerSchema
>;

export const MissingSignatureForcedStepSpendRedeemer =
  asDataType<MissingSignatureForcedStepSpendRedeemer>(
    MissingSignatureForcedStepSpendRedeemerSchema,
  );

export const MissingSignatureForcedSignerDatumSchema =
  faultProofStepDatumSchema(MissingSignatureForcedSignerStateSchema);

export const MissingSignatureForcedSignerSpendRedeemerSchema =
  faultProofStepRedeemerSchema(MissingSignatureForcedSignerArgsSchema);

export type MissingSignatureForcedSignerArgs = Data.Static<
  typeof MissingSignatureForcedSignerArgsSchema
>;

export const MissingSignatureForcedSignerArgs =
  asDataType<MissingSignatureForcedSignerArgs>(
    MissingSignatureForcedSignerArgsSchema,
  );
