import {
  computeHash32,
  decodeSingleCbor,
  encodeCbor,
  encodeNativeTxWitnessSetCompactCbor,
} from "@al-ft/midgard-core";
import { CML, Data } from "@lucid-evolution/lucid";

import { H32Schema } from "@/common.js";

import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  type MidgardAddressWitness as MidgardAddressWitnessData,
  MidgardAddressWitnessListSchema,
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
  type NativeTxWitnessSetCompact as NativeTxWitnessSetCompactData,
  NativeTxWitnessSetCompactSchema,
} from "./native.js";

export const InvalidSignatureStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type InvalidSignatureStep01Datum = Data.Static<
  typeof InvalidSignatureStep01DatumSchema
>;
export const InvalidSignatureStep01Datum =
  InvalidSignatureStep01DatumSchema as unknown as InvalidSignatureStep01Datum;

export const InvalidSignatureStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(NativeTxInclusionArgsSchema);
export type InvalidSignatureStep01SpendRedeemer = Data.Static<
  typeof InvalidSignatureStep01SpendRedeemerSchema
>;
export const InvalidSignatureStep01SpendRedeemer =
  InvalidSignatureStep01SpendRedeemerSchema as unknown as InvalidSignatureStep01SpendRedeemer;

export const InvalidSignatureStep02StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  bad_tx_wits_hash: H32Schema,
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

export const InvalidSignatureStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
  witness_set_preimage: NativeTxWitnessSetCompactSchema,
  addr_tx_wits_preimage: MidgardAddressWitnessListSchema,
  bad_address_witness_index: Data.Integer(),
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

export {
  FaultProofStepCancel as InvalidSignatureStepCancel,
  FaultProofStepCancelSchema as InvalidSignatureStepCancelSchema,
  NativeTxInclusionArgs as InvalidSignatureTxInclusionArgs,
  NativeTxInclusionArgsSchema as InvalidSignatureTxInclusionArgsSchema,
};

/**
 * Decode the address-witness preimage CBOR into the positional witness list the
 * step-02 redeemer carries.
 *
 * The preimage is a definite CBOR array whose elements are the raw per-witness
 * encodings (`[verification_key, signature]`), which is the same shape the
 * on-chain `encode_address_witness_preimage` reproduces from the structured
 * list. Decoding is therefore exactly the inverse of what the validator hashes.
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
        `Address witness ${index} must be a CBOR byte string entry`,
      );
    }
    const witness = decodeSingleCbor(entry);
    if (!Array.isArray(witness) || witness.length !== 2) {
      throw new Error(
        `Address witness ${index} must decode to a 2-element array`,
      );
    }
    const [verificationKey, signature] = witness;
    if (
      !(verificationKey instanceof Uint8Array) ||
      !(signature instanceof Uint8Array)
    ) {
      throw new Error(`Address witness ${index} must hold two byte strings`);
    }
    return {
      verification_key: Buffer.from(verificationKey).toString("hex"),
      signature: Buffer.from(signature).toString("hex"),
    };
  });
};

/**
 * Re-encode a positional witness list into the preimage the node commits to.
 * Exact inverse of {@link decodeAddressWitnessPreimage}, and the offchain twin
 * of the on-chain `encode_address_witness_preimage`.
 */
export const encodeAddressWitnessPreimage = (
  witnesses: readonly MidgardAddressWitnessData[],
): Buffer =>
  encodeCbor(
    witnesses.map((witness) =>
      encodeCbor([
        Buffer.from(witness.verification_key, "hex"),
        Buffer.from(witness.signature, "hex"),
      ]),
    ),
  );

/**
 * The `addr_tx_wits_hash` a witness list produces. Because the encoding is
 * length prefixed, a list matching this hash also fixes its own length and
 * ordering, which is what makes the accused index unambiguous on-chain.
 */
export const computeAddressWitnessesHash = (
  witnesses: readonly MidgardAddressWitnessData[],
): string =>
  computeHash32(encodeAddressWitnessPreimage(witnesses)).toString("hex");

/**
 * Verify one address witness against a transaction ID.
 *
 * The signed message is the native transaction ID, which is the blake2b-256 of
 * the compact body CBOR — the same value the step-01 validator carries forward
 * and the step-02 validator passes to `verify_ed25519_signature`.
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
 * Index of the first address witness whose signature does not verify against
 * the transaction ID, or `null` when every witness verifies.
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
 * A transaction violates the "signatures are valid" ledger rule when any of its
 * address witnesses carries a signature that does not verify against the
 * transaction's ID.
 */
export const nativeTxHasInvalidSignatureViolation = ({
  txId,
  addrTxWits,
}: {
  readonly txId: string;
  readonly addrTxWits: readonly MidgardAddressWitnessData[];
}): boolean => findInvalidAddressWitnessIndex({ txId, addrTxWits }) !== null;

/**
 * The witness-set hash a native transaction commits to, recomputed from the
 * three witness-category hashes. Mirrors the on-chain
 * `encode_native_tx_witness_set_compact |> blake2b_256`, so a preimage that
 * satisfies this check also satisfies the validator.
 */
export const computeWitnessSetHash = (
  witnessSet: NativeTxWitnessSetCompactData,
): string =>
  computeHash32(
    encodeNativeTxWitnessSetCompactCbor({
      addrTxWitsHash: Buffer.from(witnessSet.addr_tx_wits_hash, "hex"),
      scriptTxWitsHash: Buffer.from(witnessSet.script_tx_wits_hash, "hex"),
      redeemerTxWitsHash: Buffer.from(witnessSet.redeemer_tx_wits_hash, "hex"),
    }),
  ).toString("hex");
