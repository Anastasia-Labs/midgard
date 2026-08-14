/**
 * Re-derived onto the flat field commitments by #604 (the #575 off-chain builder
 * remediation): thread state carries the §2.5 anchor rather than a per-field
 * collection commitment, and a step redeemer carries a `FieldOpeningV1` rather
 * than a reproduced `..._preimage: List<…>`. The rebind is explained once in
 * `docs/fault-proofs/offchain-builder-staleness-575.md`.
 *
 * `invalid-signature` fault-proof family (Goal task `Q15`).
 *
 * **Rule.** Every address witness of every committed transaction must carry an
 * Ed25519 signature that verifies against that transaction's native id:
 * `∀t ∈ Ledger, ∀(v, s) ∈ addr_tx_wits(t): is_valid_signature(v, t.id, s)`.
 *
 * **Violation.** A block commits a transaction one of whose address witnesses
 * does not verify. The rule is unconditional over the block's transactions, so
 * the proof deliberately never consults `validity_code`: a block may not excuse
 * an invalid signature by declaring the offending transaction invalid.
 *
 * The proof is a two-step computation thread:
 *
 * 1. bind the bad transaction to the block's counted `transactions_root`, open
 *    its committed `witness_set_hash` with the witness-set compact, and forward
 *    the transaction id together with the exposed `addr_tx_wits_hash`; and
 * 2. open that canonical address-witness collection with the complete witness
 *    preimage and require the witness at the accused index not to verify.
 *
 * Two preimages are needed to reach one signature, and both openings fit in one
 * thread because the family takes only a single transaction-inclusion proof (in
 * step 01) and is therefore not subject to the one-`plutarch_phas`-per-
 * transaction limit that forces the `input-no-idx` family to spread its
 * openings over four steps.
 *
 * This module is the strict TypeScript twin of
 * `onchain/aiken/lib/midgard/fraud-proofs/invalid-signature/step-0{1,2}.ak`.
 * Field order in every `Data.Object` mirrors the aiken record declarations 1:1 —
 * the PlutusData encoding is positional, so re-ordering here would silently
 * produce redeemers the validators reject.
 */
import {
  computeHash32,
  decodeSingleCbor,
  encodeCbor,
  encodeMidgardNativeTxWitnessSetCompactV1,
  midgardFieldCommitmentFromItemsV1,
} from "@al-ft/midgard-core";
import { CML, Data } from "@lucid-evolution/lucid";

import { H32Schema } from "@/common.js";

import { FieldOpeningV1Schema } from "./field-opening-v1.js";
import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  type MidgardAddressWitness as MidgardAddressWitnessData,
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
  type NativeTxWitnessSetCompact as NativeTxWitnessSetCompactData,
} from "./native.js";

/** Catalogue violation identifier adjudicated by this family. */
export const INVALID_SIGNATURE_VIOLATION_ID_V1 = "invalid-signature" as const;

/**
 * Canonical address-witness field index of a native V1 transaction witness set.
 * The commitment is `bounded_collection_v1.from_items(7, ...)`, so a preimage
 * built for any other field can never open it.
 */
export const INVALID_SIGNATURE_ADDR_TX_WITS_FIELD_INDEX_V1 = 7;

// ## Canonical encoders (twins of the on-chain component encoders)

/**
 * One address witness as the collection commits it: the definite CBOR array
 * `[verification_key, signature]`. Exact twin of the on-chain
 * `encode_midgard_address_witness`, including its 32/64-byte length checks.
 */
export const encodeMidgardAddressWitnessCanonicalV1 = (
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
export const invalidSignatureAddressWitnessesCommitmentV1 = (
  witnesses: readonly MidgardAddressWitnessData[],
): string =>
  midgardFieldCommitmentFromItemsV1(
    witnesses.map(encodeMidgardAddressWitnessCanonicalV1),
  ).toString("hex");

/**
 * Re-encode a positional witness list into the byte-list preimage the node
 * stores as `addrTxWitsPreimageCbor`: a CBOR array whose elements are the raw
 * per-witness encodings. Exact inverse of
 * {@link decodeAddressWitnessPreimage}.
 */
export const encodeAddressWitnessPreimage = (
  witnesses: readonly MidgardAddressWitnessData[],
): Buffer => encodeCbor(witnesses.map(encodeMidgardAddressWitnessCanonicalV1));

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
export const invalidSignatureWitnessSetCommitmentV1 = (
  witnessSet: NativeTxWitnessSetCompactData,
): string =>
  computeHash32(
    encodeMidgardNativeTxWitnessSetCompactV1({
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
  NativeTxInclusionArgsSchema;
export type InvalidSignatureTxInclusionArgs = NativeTxInclusionArgs;
export const InvalidSignatureTxInclusionArgs =
  NativeTxInclusionArgs as unknown as InvalidSignatureTxInclusionArgs;

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
  InvalidSignatureStep01DatumSchema as unknown as InvalidSignatureStep01Datum;

/**
 * Mirrors `midgard/fraud_proofs/invalid_signature/step_01.Args`.
 *
 * **A fourth #575 divergence, found by #604 and named nowhere else.**
 * `docs/fault-proofs/offchain-builder-staleness-575.md` §2 lists three — the
 * thread anchor, the `FieldOpeningV1`, the certificate-policy parameter — and
 * this is not one of them: #575 (`2fec6b0fb`) also *deleted* step-01's
 * `bad_tx_witness_set_compact` argument, collapsing a two-field record to a bare
 * `NativeTxInclusionArgs`.
 *
 * The witness-set preimage no longer travels through step-01, because step-02
 * opens field 7 through the §8.8 door and re-derives it from whatever carriage
 * the prover chose. What step-01 still owes the thread is the witness-set
 * *hash*, which it alone can authenticate — §3's transaction id does not commit
 * it — and that goes into `step_02.State`, not into these arguments.
 *
 * Emitting the retired wrapper produces a redeemer the validator decodes
 * positionally into the wrong fields; the observed signature was
 * `Spend[0] unexpected empty list`, not a clean decode failure.
 */
export const InvalidSignatureStep01ArgsSchema = NativeTxInclusionArgsSchema;
export type InvalidSignatureStep01Args = Data.Static<
  typeof InvalidSignatureStep01ArgsSchema
>;
export const InvalidSignatureStep01Args =
  InvalidSignatureStep01ArgsSchema as unknown as InvalidSignatureStep01Args;

export const InvalidSignatureStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(InvalidSignatureStep01ArgsSchema);
export type InvalidSignatureStep01SpendRedeemer = Data.Static<
  typeof InvalidSignatureStep01SpendRedeemerSchema
>;
export const InvalidSignatureStep01SpendRedeemer =
  InvalidSignatureStep01SpendRedeemerSchema as unknown as InvalidSignatureStep01SpendRedeemer;

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
  bad_tx_id: H32Schema,
  bad_tx_witness_set_hash: H32Schema,
});
export type InvalidSignatureStep02State = Data.Static<
  typeof InvalidSignatureStep02StateSchema
>;
export const InvalidSignatureStep02State =
  InvalidSignatureStep02StateSchema as unknown as InvalidSignatureStep02State;

export const InvalidSignatureStep02DatumSchema = faultProofStepDatumSchema(
  InvalidSignatureStep02StateSchema,
);
export type InvalidSignatureStep02Datum = Data.Static<
  typeof InvalidSignatureStep02DatumSchema
>;
export const InvalidSignatureStep02Datum =
  InvalidSignatureStep02DatumSchema as unknown as InvalidSignatureStep02Datum;

/**
 * Mirrors `midgard/fraud_proofs/invalid_signature/step_02.Args`.
 *
 * `addr_tx_wits_opening` must be the `WitnessFieldOpening` arm — it carries the
 * transaction's `NativeTxWitnessSetCompact` alongside the compact bytes, and the
 * door refuses a body opening at field 7. It also may **not** name tier 3
 * (§8.3 erratum E2 limit 3); `fieldOpeningV1ForField` refuses that off-chain and
 * `carriage_reaches_the_anchor` refuses it on-chain.
 */
export const InvalidSignatureStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  addr_tx_wits_opening: FieldOpeningV1Schema,
  bad_addr_tx_wit_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type InvalidSignatureStep02Args = Data.Static<
  typeof InvalidSignatureStep02ArgsSchema
>;
export const InvalidSignatureStep02Args =
  InvalidSignatureStep02ArgsSchema as unknown as InvalidSignatureStep02Args;

export const InvalidSignatureStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(InvalidSignatureStep02ArgsSchema);
export type InvalidSignatureStep02SpendRedeemer = Data.Static<
  typeof InvalidSignatureStep02SpendRedeemerSchema
>;
export const InvalidSignatureStep02SpendRedeemer =
  InvalidSignatureStep02SpendRedeemerSchema as unknown as InvalidSignatureStep02SpendRedeemer;

// ## Step-state builder (twin of the on-chain forwarding rule)

/** Exactly the state `step-01` writes for `step-02`. */
export const invalidSignatureStep02StateFromBadTxV1 = ({
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
  bad_tx_id: badTxId.toLowerCase(),
  bad_tx_witness_set_hash: badTxWitnessSetHash.toLowerCase(),
});
