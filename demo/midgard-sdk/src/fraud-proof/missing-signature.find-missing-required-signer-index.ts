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
// bare `NativeTxInclusionArgs` in Continue. A separate ForcedDispatch arm
// routes to the authenticated forced-source binding step.

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

export const MissingSignatureStep01SpendRedeemerSchema = Data.Enum([
  Data.Object({ Cancel: FaultProofStepCancelSchema }),
  Data.Object({ Continue: Data.Tuple([MissingSignatureStep01ArgsSchema]) }),
  Data.Object({
    ForcedDispatch: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
    }),
  }),
]);

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
