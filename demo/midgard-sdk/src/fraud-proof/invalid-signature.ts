/**
 * Signature contradiction proof: bind an accepted or forced transaction, then
 * authenticate field 7 and verify the selected Ed25519 signature. A forced
 * rejection binds its exact typed reason and witness coordinate. The persistent
 * subject carries transaction/source identity and verdict direction; the
 * witness-set hash is separately authenticated because transaction IDs commit
 * the body only. PlutusData field order mirrors the Aiken records.
 */
import {
  computeHash32,
  decodeSingleCbor,
  encodeCbor,
  encodeMidgardNativeTxWitnessSetCompact,
  midgardFieldCommitmentFromItems,
} from "@al-ft/midgard-core";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { CML, Data } from "@lucid-evolution/lucid";

import { OutputReferenceSchema } from "../common.js";
import { H32Schema } from "../common.js";
import { ForcedInclusionTxV1Schema, HeaderSchema } from "../ledger-state.js";
import { RejectionReasonSchema } from "../rejection-reason.js";
import { rootMembershipProofSchema } from "../transition-trace.js";
import { FieldOpeningSchema } from "./field-opening.js";
import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  type MidgardAddressWitness as MidgardAddressWitnessData,
  NativeTxInclusionCarriage,
  NativeTxInclusionCarriageSchema,
  type NativeTxWitnessSetCompact as NativeTxWitnessSetCompactData,
} from "./native.js";
import {
  acceptedVerdictSubject,
  type VerdictSubject,
  verdictSubjectIsCanonical,
} from "./proof-thread-substrate.js";

/** Catalogue violation identifier adjudicated by this family. */
export const INVALID_SIGNATURE_VIOLATION_ID = "invalid-signature" as const;

/**
 * Canonical address-witness field index of a native V1 transaction witness set.
 * The commitment is `bounded_collection_v1.from_items(7, ...)`, so a preimage
 * built for any other field can never open it.
 */
export const INVALID_SIGNATURE_ADDR_TX_WITS_FIELD_INDEX = 7;

// ## Canonical encoders (twins of the on-chain component encoders)

/**
 * One address witness as the collection commits it: the definite CBOR array
 * `[verification_key, signature]`. Exact twin of the on-chain
 * `encode_midgard_address_witness`, including its 32/64-byte length checks.
 */
export const encodeMidgardAddressWitnessCanonical = (
  witness: MidgardAddressWitnessData,
): Buffer => {
  const verificationKey = Buffer.from(witness.verification_key, "hex");
  const signature = Buffer.from(witness.signature, "hex");
  if (verificationKey.length !== 32) {
    throw new Error(
      `Address witness verification key must be 32 bytes, got ${verificationKey.length.toString()}.`,
    );
  }
  if (signature.length !== 64) {
    throw new Error(
      `Address witness signature must be 64 bytes, got ${signature.length.toString()}.`,
    );
  }
  return encodeCbor([verificationKey, signature]);
};

/**
 * The `addr_tx_wits_hash` a witness list produces, derived exactly as
 * `bounded_collection_v1.from_items(7, ...)` does on-chain. Because the
 * commitment fixes the item count as well as each item's content, a preimage
 * matching this hash also fixes every witness's position — which is what makes
 * the accused index unambiguous on-chain.
 */
export const invalidSignatureAddressWitnessesCommitment = (
  witnesses: readonly MidgardAddressWitnessData[],
): string =>
  midgardFieldCommitmentFromItems(
    witnesses.map(encodeMidgardAddressWitnessCanonical),
  ).toString("hex");

/**
 * Re-encode a positional witness list into the byte-list preimage the node
 * stores as `addrTxWitsPreimageCbor`: a CBOR array whose elements are the raw
 * per-witness encodings. Exact inverse of
 * {@link decodeAddressWitnessPreimage}.
 */
export const encodeAddressWitnessPreimage = (
  witnesses: readonly MidgardAddressWitnessData[],
): Buffer => encodeCbor(witnesses.map(encodeMidgardAddressWitnessCanonical));

/**
 * Decode the node's address-witness preimage CBOR into the positional witness
 * list the step-02 redeemer carries.
 */
export const decodeAddressWitnessPreimage = (
  addrTxWitsPreimageCbor: Uint8Array,
): readonly MidgardAddressWitnessData[] => {
  const entries = decodeSingleCbor(addrTxWitsPreimageCbor);
  if (!Array.isArray(entries)) {
    throw new Error("Address witness preimage must decode to a CBOR array");
  }
  return entries.map((entry, index) => {
    if (!(entry instanceof Uint8Array)) {
      throw new Error(
        `Address witness ${index.toString()} must be a CBOR byte string entry`,
      );
    }
    const witness = decodeSingleCbor(entry);
    if (!Array.isArray(witness) || witness.length !== 2) {
      throw new Error(
        `Address witness ${index.toString()} must decode to a 2-element array`,
      );
    }
    const [verificationKey, signature] = witness;
    if (
      !(verificationKey instanceof Uint8Array) ||
      !(signature instanceof Uint8Array)
    ) {
      throw new Error(
        `Address witness ${index.toString()} must hold two byte strings`,
      );
    }
    return {
      verification_key: Buffer.from(verificationKey).toString("hex"),
      signature: Buffer.from(signature).toString("hex"),
    };
  });
};

/**
 * The `witness_set_hash` a native transaction commits, recomputed from the three
 * witness-category hashes. Mirrors the check the on-chain §8.8 field-access door
 * makes: `authenticated_field_view`
 * (`onchain/aiken/lib/midgard/native-tx-field-access-v1.ak`) will not read any of
 * fields 6–8 unless
 * `blake2b_256(encode_native_tx_witness_set_compact(witness_set))` equals the
 * `witness_set_hash` the compact transaction carries. The standalone
 * `verify_native_tx_witness_set` helper this used to mirror was deleted by #575.
 * For a downstream step the hash that check runs against is the anchored one
 * from thread state — `WitnessAnchor` in
 * `onchain/aiken/lib/midgard/fraud-proofs/field-opening-v1.ak` — because §3's
 * transaction-id preimage is the body alone and so does not cover it.
 */
export const invalidSignatureWitnessSetCommitment = (
  witnessSet: NativeTxWitnessSetCompactData,
): string =>
  computeHash32(
    encodeMidgardNativeTxWitnessSetCompact({
      addrTxWitsHash: Buffer.from(witnessSet.addr_tx_wits_hash, "hex"),
      scriptTxWitsHash: Buffer.from(witnessSet.script_tx_wits_hash, "hex"),
      redeemerTxWitsHash: Buffer.from(witnessSet.redeemer_tx_wits_hash, "hex"),
    }),
  ).toString("hex");

// ## Rule

/**
 * Verify one address witness against a transaction id.
 *
 * The signed message is the native transaction id, which is the blake2b-256 of
 * the compact body CBOR — the same value step 01 carries forward and step 02
 * passes to `verify_ed25519_signature`.
 *
 * Malformed keys or signatures are reported as *not verifying* rather than
 * thrown, because a block committing a structurally broken witness is itself
 * the violation this proof targets.
 */
export const verifyAddressWitness = ({
  txId,
  witness,
}: {
  readonly txId: string;
  readonly witness: MidgardAddressWitnessData;
}): boolean => {
  try {
    const publicKey = CML.PublicKey.from_bytes(
      Buffer.from(witness.verification_key, "hex"),
    );
    const signature = CML.Ed25519Signature.from_raw_bytes(
      Buffer.from(witness.signature, "hex"),
    );
    return publicKey.verify(Buffer.from(txId, "hex"), signature);
  } catch {
    return false;
  }
};

/**
 * Index of the first address witness whose signature does not verify against the
 * transaction id, or `null` when every witness verifies.
 */
export const findInvalidAddressWitnessIndex = ({
  txId,
  addrTxWits,
}: {
  readonly txId: string;
  readonly addrTxWits: readonly MidgardAddressWitnessData[];
}): number | null => {
  const index = addrTxWits.findIndex(
    (witness) => !verifyAddressWitness({ txId, witness }),
  );
  return index === -1 ? null : index;
};

/**
 * The adjudicated violation predicate, over evidence that has already been
 * authenticated against the block header. This is exactly the
 * `verify_ed25519_signature(...) == False` requirement in
 * `validators/fraud-proofs/invalid-signature/step-02.ak`, lifted from one
 * accused index to the whole witness list.
 */
export const nativeTxHasInvalidSignatureViolation = ({
  txId,
  addrTxWits,
}: {
  readonly txId: string;
  readonly addrTxWits: readonly MidgardAddressWitnessData[];
}): boolean => findInvalidAddressWitnessIndex({ txId, addrTxWits }) !== null;

// ## Shared step aliases

export const InvalidSignatureTxInclusionArgsSchema =
  NativeTxInclusionCarriageSchema;
export type InvalidSignatureTxInclusionArgs = NativeTxInclusionCarriage;
export const InvalidSignatureTxInclusionArgs =
  NativeTxInclusionCarriage as unknown as InvalidSignatureTxInclusionArgs;

export const InvalidSignatureStepCancelSchema = FaultProofStepCancelSchema;
export type InvalidSignatureStepCancel = FaultProofStepCancel;
export const InvalidSignatureStepCancel =
  FaultProofStepCancel as unknown as InvalidSignatureStepCancel;

// ## Step 01 — bind the bad transaction and open its witness-set commitment
//
// The step-01 UTxO is the initialized fraud proof (its `data` is `None`), so it
// is read with the generic computation-thread step datum. Spending it carries
// the native-tx inclusion args *and* the witness-set compact whose hash the
// committed transaction pins; the produced UTxO carries step-02's state.

export const InvalidSignatureStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type InvalidSignatureStep01Datum = Data.Static<
  typeof InvalidSignatureStep01DatumSchema
>;
export const InvalidSignatureStep01Datum =
  asDataType<InvalidSignatureStep01Datum>(InvalidSignatureStep01DatumSchema);

/**
 * Mirrors `midgard/fraud_proofs/invalid_signature/step_01.Args`.
 *
 * Step 01 selects accepted inclusion carriage or forced-root membership. Step 02 opens field 7 through
 * the shared field-opening door; step 01 forwards the authenticated witness-set
 * hash in its next-state datum because the transaction id alone does not commit
 * witness fields.
 *
 * Any additional wrapper fields change the positional PlutusData shape and are
 * therefore outside this ABI.
 */
export const InvalidSignatureVerdictSubjectSchema = Data.Object({
  version: Data.Integer(),
  direction: Data.Integer(),
  source_kind: Data.Integer(),
  transaction_id: Data.Bytes(),
  source_key: Data.Bytes(),
  rejection_reason: Data.Nullable(RejectionReasonSchema),
});
export const InvalidSignatureStep01SourceSchema = Data.Enum([
  Data.Object({
    AcceptedSource: Data.Object({ inclusion: NativeTxInclusionCarriageSchema }),
  }),
  Data.Object({
    ForcedSource: Data.Object({
      input_index: Data.Integer(),
      output_index: Data.Integer(),
      header: HeaderSchema,
      membership: rootMembershipProofSchema(
        OutputReferenceSchema,
        ForcedInclusionTxV1Schema,
      ),
      direction: Data.Integer(),
    }),
  }),
]);
export const InvalidSignatureForcedSourcePayloadSchema = Data.Object({
  header: HeaderSchema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxV1Schema,
  ),
  direction: Data.Integer(),
});

export const InvalidSignatureStep01ArgsSchema = Data.Object({
  source: InvalidSignatureStep01SourceSchema,
});
export type InvalidSignatureStep01Args = Data.Static<
  typeof InvalidSignatureStep01ArgsSchema
>;
export const InvalidSignatureStep01Args =
  asDataType<InvalidSignatureStep01Args>(InvalidSignatureStep01ArgsSchema);

export const InvalidSignatureStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(InvalidSignatureStep01ArgsSchema);
export type InvalidSignatureStep01SpendRedeemer = Data.Static<
  typeof InvalidSignatureStep01SpendRedeemerSchema
>;
export const InvalidSignatureStep01SpendRedeemer =
  asDataType<InvalidSignatureStep01SpendRedeemer>(
    InvalidSignatureStep01SpendRedeemerSchema,
  );

// ## Step 02 — open the address-witness collection and convict the signature

/**
 * Mirrors `midgard/fraud_proofs/invalid_signature/step_02.State` — and this is
 * the family that shows why `NativeTxAnchorV1` has two arms.
 *
 * Field 7 lives in the **witness set**, and §3's transaction-id preimage is the
 * body alone, so the id does not commit it: a prover may hand over the genuine
 * body followed by any trailing `witness_set_hash` it likes and the bytes still
 * re-derive to the committed id. The retired `bad_addr_tx_wits_hash` (field 7's
 * own collection commitment) is therefore replaced not by nothing but by
 * `bad_tx_witness_set_hash` — the value step-01 read off the compact structure
 * the block committed, which is what `WitnessAnchor` anchors and what the door
 * checks the supplied witness set against.
 */
export const InvalidSignatureStep02StateSchema = Data.Object({
  subject: InvalidSignatureVerdictSubjectSchema,
  bad_tx_witness_set_hash: H32Schema,
});
export type InvalidSignatureStep02State = Data.Static<
  typeof InvalidSignatureStep02StateSchema
>;
export const InvalidSignatureStep02State =
  asDataType<InvalidSignatureStep02State>(InvalidSignatureStep02StateSchema);

export const InvalidSignatureStep02DatumSchema = faultProofStepDatumSchema(
  InvalidSignatureStep02StateSchema,
);
export type InvalidSignatureStep02Datum = Data.Static<
  typeof InvalidSignatureStep02DatumSchema
>;
export const InvalidSignatureStep02Datum =
  asDataType<InvalidSignatureStep02Datum>(InvalidSignatureStep02DatumSchema);

/**
 * Mirrors `midgard/fraud_proofs/invalid_signature/step_02.Args`.
 *
 * `addr_tx_wits_opening` must be the `WitnessFieldOpening` arm — it carries the
 * transaction's `NativeTxWitnessSetCompact` alongside the compact bytes, and
 * the door refuses a body opening at field 7. Tier 3 is admissible here since
 * #606's repair (the certificate's mint-welded `field_hash` must equal the
 * commitment reached through the anchored `witness_set_hash`, so a fabricated
 * certificate fails at the door); §8.3 erratum E2's disposition records the
 * resolution.
 */
export const InvalidSignatureStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  addr_tx_wits_opening: FieldOpeningSchema,
  bad_addr_tx_wit_index: Data.Integer(),
});
export type InvalidSignatureStep02Args = Data.Static<
  typeof InvalidSignatureStep02ArgsSchema
>;
export const InvalidSignatureStep02Args =
  asDataType<InvalidSignatureStep02Args>(InvalidSignatureStep02ArgsSchema);

export const InvalidSignatureStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(InvalidSignatureStep02ArgsSchema);
export type InvalidSignatureStep02SpendRedeemer = Data.Static<
  typeof InvalidSignatureStep02SpendRedeemerSchema
>;
export const InvalidSignatureStep02SpendRedeemer =
  asDataType<InvalidSignatureStep02SpendRedeemer>(
    InvalidSignatureStep02SpendRedeemerSchema,
  );

// ## Step-state builder (twin of the on-chain forwarding rule)

/** Exactly the state `step-01` writes for `step-02`. */
export const invalidSignatureStep02StateFromBadTx = ({
  badTxId,
  badTxWitnessSetHash,
}: {
  readonly badTxId: string;
  /**
   * The `witness_set_hash` read off the compact structure the block's
   * `transactions_root` committed — **not** field 7's own commitment, and not a
   * value any later redeemer supplies. It is the second half of `WitnessAnchor`,
   * and the only reason a witness-set field can be opened at all.
   */
  readonly badTxWitnessSetHash: string;
}): InvalidSignatureStep02State => ({
  subject: acceptedVerdictSubject(badTxId),
  bad_tx_witness_set_hash: badTxWitnessSetHash.toLowerCase(),
});

/** Exact forced coordinate and terminal polarity over an authenticated field. */
export const invalidSignatureTerminalContradiction = ({
  subject,
  witnessIndex,
  addressWitnesses,
}: {
  readonly subject: VerdictSubject;
  readonly witnessIndex: bigint;
  readonly addressWitnesses: readonly MidgardAddressWitnessData[];
}): boolean => {
  if (!verdictSubjectIsCanonical(subject))
    throw new Error("invalidSignature: noncanonical subject");
  if (subject.direction === 1n) {
    const reason = subject.rejection_reason;
    if (
      reason === null ||
      typeof reason !== "object" ||
      !("AddressWitnessSignatureInvalid" in reason) ||
      reason.AddressWitnessSignatureInvalid.witness_index !== witnessIndex
    )
      throw new Error(
        "invalidSignature: authenticated rejection reason/index changed",
      );
  }
  const witness =
    witnessIndex >= 0n && witnessIndex < BigInt(addressWitnesses.length)
      ? addressWitnesses[Number(witnessIndex)]
      : undefined;
  const fault =
    witness !== undefined &&
    !verifyAddressWitness({ txId: subject.transaction_id, witness });
  return subject.direction === 0n ? fault : !fault;
};
