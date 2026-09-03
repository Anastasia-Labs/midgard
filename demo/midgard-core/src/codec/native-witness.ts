import { decodeSingleCbor, encodeCbor } from "./cbor.js";
import { ensureHash32, type Hash32 } from "./hash.js";
import type {
  MidgardNativeTxWitnessSetCanonical,
  MidgardNativeTxWitnessSetCompact,
} from "./native.js";
import { MIDGARD_NATIVE_TX_VERSION } from "./native-constants.js";
import { midgardFieldCommitment } from "./native-tx-field-access-v1.js";
import { asFixedArray, bytesItem, hashItem } from "./native-validation.js";

type NativeTxWitnessSetCompactValue = readonly [Hash32, Hash32, Hash32];
type NativeTxWitnessSetCanonicalValue = readonly [Buffer, Buffer, Buffer];

export const encodeNativeTxWitnessSetCompactValue = (
  witnessSet: MidgardNativeTxWitnessSetCompact,
): NativeTxWitnessSetCompactValue => [
  ensureHash32(
    witnessSet.addrTxWitsHash,
    "transaction_witness_set_compact.addr_tx_wits_hash",
  ),
  ensureHash32(
    witnessSet.scriptTxWitsHash,
    "transaction_witness_set_compact.script_tx_wits_hash",
  ),
  ensureHash32(
    witnessSet.redeemerTxWitsHash,
    "transaction_witness_set_compact.redeemer_tx_wits_hash",
  ),
];

export const decodeNativeTxWitnessSetCompactValue = (
  value: unknown,
  fieldName: string,
  _version: bigint,
): MidgardNativeTxWitnessSetCompact => {
  const v = asFixedArray(value, 3, fieldName);
  return {
    addrTxWitsHash: hashItem(v, 0, fieldName),
    scriptTxWitsHash: hashItem(v, 1, fieldName),
    redeemerTxWitsHash: hashItem(v, 2, fieldName),
  };
};

export const encodeNativeTxWitnessSetCanonicalValue = (
  _version: bigint,
  witnessSet: MidgardNativeTxWitnessSetCanonical,
): NativeTxWitnessSetCanonicalValue => [
  Buffer.from(witnessSet.addrTxWitsPreimageCbor),
  Buffer.from(witnessSet.scriptTxWitsPreimageCbor),
  Buffer.from(witnessSet.redeemerTxWitsPreimageCbor),
];

export const decodeNativeTxWitnessSetCanonicalValue = (
  value: unknown,
  fieldName: string,
  _version: bigint,
): MidgardNativeTxWitnessSetCanonical => {
  const v = asFixedArray(value, 3, fieldName);
  return {
    addrTxWitsPreimageCbor: bytesItem(v, 0, fieldName),
    scriptTxWitsPreimageCbor: bytesItem(v, 1, fieldName),
    redeemerTxWitsPreimageCbor: bytesItem(v, 2, fieldName),
  };
};

/**
 * The three witness-set field commitments — fields 7, 6 and 8 in that tuple
 * order (§2.2's wire order is not the §2.5 index order).
 *
 * Same §4 derivation as {@link deriveNativeTxBodyCompact}'s six: a plain
 * `blake2b_256` over the field's §5.1 preimage bytes, with no re-validation of
 * the grammar here. The note there is the full one.
 */
export const deriveNativeTxWitnessSetCompact = (
  witnessSet: MidgardNativeTxWitnessSetCanonical,
): MidgardNativeTxWitnessSetCompact => ({
  addrTxWitsHash: midgardFieldCommitment(witnessSet.addrTxWitsPreimageCbor),
  scriptTxWitsHash: midgardFieldCommitment(witnessSet.scriptTxWitsPreimageCbor),
  redeemerTxWitsHash: midgardFieldCommitment(
    witnessSet.redeemerTxWitsPreimageCbor,
  ),
});

export const encodeNativeTxWitnessSetCompactCbor = (
  witnessSet: MidgardNativeTxWitnessSetCompact,
): Buffer => encodeCbor(encodeNativeTxWitnessSetCompactValue(witnessSet));

export const decodeNativeTxWitnessSetCompactCbor = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCompact =>
  decodeNativeTxWitnessSetCompactValue(
    decodeSingleCbor(bytes),
    "transaction_witness_set",
    MIDGARD_NATIVE_TX_VERSION,
  );

export const encodeNativeTxWitnessPreimagesCbor = (
  witnessSet: MidgardNativeTxWitnessSetCanonical,
): Buffer =>
  encodeCbor(
    encodeNativeTxWitnessSetCanonicalValue(
      MIDGARD_NATIVE_TX_VERSION,
      witnessSet,
    ),
  );

export const decodeNativeTxWitnessPreimagesCbor = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCanonical =>
  decodeNativeTxWitnessSetCanonicalValue(
    decodeSingleCbor(bytes),
    "transaction_witness_preimages",
    MIDGARD_NATIVE_TX_VERSION,
  );
