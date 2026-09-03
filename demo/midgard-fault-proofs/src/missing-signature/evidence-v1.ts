/**
 * `missing-signature` evidence assembly (offchain plan §4.2, §5).
 *
 * The family's two openings both ride the §8.8 door builders in
 * `src/field-opening-v1.ts` — nothing here re-implements the door, the tier
 * choice, or carriage publication:
 *
 * - **body field 4** (`required_signers`, step-02): `BodyAnchor` — the field
 *   is committed under the transaction id itself, so the opening needs no
 *   witness-set pairing. Items are the raw 28-byte hashes (fixed stride).
 *   First offchain consumer of field 4.
 * - **witness field 7** (`address_witnesses`, step-04): `WitnessAnchor` — §3's
 *   id preimage is the body alone, so the door must be handed the compact
 *   witness set and the `witness_set_hash` **taken from the thread state,
 *   never re-derived locally** (the `submit-invalid-signature-step-02.ts`
 *   shape). Items are the canonical `[vkey32, sig64]` encodings.
 *
 * What this module owns is only what sits between the finding and the doors:
 * accused-ordinal selection over the decoded evidence, and the two thin plan
 * wrappers that pin each opening's field index and anchor discipline so the
 * submitters cannot diverge from them.
 */
import {
  encodeMidgardAddressWitnessCanonical,
  MIDGARD_FIELD_INDEX,
  type MidgardAddressWitness,
  missingSignatureVkeyHash,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";

import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
} from "../field-opening-v1.js";
import { MISSING_SIGNATURE_CATEGORY_LABEL } from "./contracts-v1.js";
import { missingSignatureSubmitError } from "./submit-common-v1.js";

// ## Accused-ordinal selection (§3.4)

/**
 * The accusation detection makes: the first required-signer ordinal whose
 * hash is the blake2b-224 image of no address witness's verification key,
 * with the hash read back out of the list so finding and redeemer can never
 * name different signers. `null` when every required signer is witnessed —
 * an honest commitment.
 *
 * Presence is judged by hash, exactly as the on-chain pair judges it: a
 * witness whose key hashes to the accused signer counts as present even if
 * its signature is garbage — that is `invalid-signature`'s fault (§7.3, D6).
 */
export const selectMissingSignatureAccusation = ({
  requiredSignerHashes,
  addrTxWits,
}: {
  readonly requiredSignerHashes: readonly string[];
  readonly addrTxWits: readonly MidgardAddressWitness[];
}): { readonly index: bigint; readonly hash: string } | null => {
  const witnessKeyHashes = new Set(
    addrTxWits.map((witness) =>
      missingSignatureVkeyHash(witness.verification_key),
    ),
  );
  const index = requiredSignerHashes.findIndex(
    (hash) => !witnessKeyHashes.has(hash.toLowerCase()),
  );
  if (index === -1) {
    return null;
  }
  const hash = requiredSignerHashes[index];
  if (hash === undefined) {
    throw missingSignatureSubmitError(
      "required-signer list mutated during selection.",
    );
  }
  return { index: BigInt(index), hash: hash.toLowerCase() };
};

// ## Field-4 opening plan (step-02, BodyAnchor)

/**
 * Plans the `required_signers_opening` a step-02 redeemer carries. Body
 * field, so no witness set is handed to the door — the anchor is the
 * thread's `verified_tx_id` alone. Item CBOR for field 4 is the raw 28-byte
 * hash (the fixed stride the validator indexes by).
 */
export const planMissingSignatureRequiredSignersOpening = ({
  anchorTxId,
  nativeTxCompactCbor,
  requiredSignerHashes,
  owner,
  publish,
}: {
  readonly anchorTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly requiredSignerHashes: readonly string[];
  readonly owner: string;
  readonly publish?: boolean;
}): FaultProofFieldOpeningPlan =>
  planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.requiredSigners,
    anchorTxId,
    nativeTxCompactCbor,
    itemCbors: requiredSignerHashes.map((hash) => {
      if (!/^[0-9a-f]{56}$/u.test(hash)) {
        throw missingSignatureSubmitError(
          "required-signer items must be 28 bytes of lowercase hex.",
        );
      }
      return Buffer.from(hash, "hex");
    }),
    owner,
    ...(publish === undefined ? {} : { publish }),
    label: `${MISSING_SIGNATURE_CATEGORY_LABEL} required-signers`,
  });

// ## Field-7 opening plan (step-04, WitnessAnchor)

/**
 * Plans the `addr_tx_wits_opening` a step-04 redeemer carries. Witness
 * field: the door must receive the compact witness set plus the
 * `anchorWitnessSetHash` read off the on-chain thread state — passing a
 * locally re-derived hash would let a stale preimage slip past the §2.5
 * anchor, so callers hand this function the datum's value verbatim.
 */
export const planMissingSignatureAddressWitnessesOpening = ({
  anchorTxId,
  nativeTxCompactCbor,
  addrTxWits,
  witnessSet,
  anchorWitnessSetHash,
  owner,
  publish,
}: {
  readonly anchorTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly addrTxWits: readonly MidgardAddressWitness[];
  readonly witnessSet: NativeTxWitnessSetCompact;
  /** The thread state's `verified_witness_set_hash`, verbatim. */
  readonly anchorWitnessSetHash: string;
  readonly owner: string;
  readonly publish?: boolean;
}): FaultProofFieldOpeningPlan =>
  planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
    anchorTxId,
    nativeTxCompactCbor,
    itemCbors: addrTxWits.map(encodeMidgardAddressWitnessCanonical),
    owner,
    witnessSet,
    anchorWitnessSetHash,
    ...(publish === undefined ? {} : { publish }),
    label: `${MISSING_SIGNATURE_CATEGORY_LABEL} address-witnesses`,
  });
