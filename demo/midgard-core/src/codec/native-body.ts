import { decodeSingleCbor, encodeCbor } from "./cbor.js";
import { ensureHash32, type Hash32 } from "./hash.js";
import type {
  MidgardNativeTxBodyCanonicalV1,
  MidgardNativeTxBodyCompactV1,
} from "./native.js";
import { MIDGARD_NATIVE_NETWORK_ID_NONE } from "./native-constants.js";
import { deriveMidgardNativeFieldCollectionV1 } from "./native-field-items.js";
import {
  asFixedArray,
  asSigned,
  asUnsigned,
  bytesItem,
  hashItem,
} from "./native-validation.js";

type NativeTxBodyCompactValue = readonly [
  Hash32,
  Hash32,
  Hash32,
  bigint,
  bigint,
  bigint,
  Hash32,
  Hash32,
  Hash32,
  Hash32,
  Hash32,
  bigint,
];

type NativeTxBodyCanonicalValue = readonly [
  Buffer,
  Buffer,
  Buffer,
  bigint,
  bigint,
  bigint,
  Buffer,
  Buffer,
  Buffer,
  Hash32,
  Hash32,
  bigint,
];

const asNativeNetworkId = (value: unknown, fieldName: string): bigint => {
  const networkId = asUnsigned(value, fieldName);
  if (
    networkId !== 0n &&
    networkId !== 1n &&
    networkId !== MIDGARD_NATIVE_NETWORK_ID_NONE
  ) {
    throw new Error(
      `${fieldName} must be 0, 1, or ${MIDGARD_NATIVE_NETWORK_ID_NONE.toString(10)}`,
    );
  }
  return networkId;
};

export const encodeNativeTxBodyCompactValue = (
  body: MidgardNativeTxBodyCompactV1,
): NativeTxBodyCompactValue => [
  ensureHash32(
    body.spendInputsHash,
    "transaction_body_compact.spend_inputs_hash",
  ),
  ensureHash32(
    body.referenceInputsHash,
    "transaction_body_compact.reference_inputs_hash",
  ),
  ensureHash32(body.outputsHash, "transaction_body_compact.outputs_hash"),
  asUnsigned(body.fee, "transaction_body_compact.fee"),
  asSigned(
    body.validityIntervalStart,
    "transaction_body_compact.validity_interval_start",
  ),
  asSigned(
    body.validityIntervalEnd,
    "transaction_body_compact.validity_interval_end",
  ),
  ensureHash32(
    body.requiredObserversHash,
    "transaction_body_compact.required_observers_hash",
  ),
  ensureHash32(
    body.requiredSignersHash,
    "transaction_body_compact.required_signers_hash",
  ),
  ensureHash32(body.mintHash, "transaction_body_compact.mint_hash"),
  ensureHash32(
    body.scriptIntegrityHash,
    "transaction_body_compact.script_integrity_hash",
  ),
  ensureHash32(
    body.auxiliaryDataHash,
    "transaction_body_compact.auxiliary_data_hash",
  ),
  asNativeNetworkId(body.networkId, "transaction_body_compact.network_id"),
];

export const decodeNativeTxBodyCompactValue = (
  value: unknown,
  fieldName: string,
): MidgardNativeTxBodyCompactV1 => {
  const v = asFixedArray(value, 12, fieldName);
  return {
    spendInputsHash: hashItem(v, 0, fieldName),
    referenceInputsHash: hashItem(v, 1, fieldName),
    outputsHash: hashItem(v, 2, fieldName),
    fee: asUnsigned(v[3], `${fieldName}[3]`),
    validityIntervalStart: asSigned(v[4], `${fieldName}[4]`),
    validityIntervalEnd: asSigned(v[5], `${fieldName}[5]`),
    requiredObserversHash: hashItem(v, 6, fieldName),
    requiredSignersHash: hashItem(v, 7, fieldName),
    mintHash: hashItem(v, 8, fieldName),
    scriptIntegrityHash: hashItem(v, 9, fieldName),
    auxiliaryDataHash: hashItem(v, 10, fieldName),
    networkId: asNativeNetworkId(v[11], `${fieldName}[11]`),
  };
};

export const encodeNativeTxBodyCanonicalValue = (
  body: MidgardNativeTxBodyCanonicalV1,
): NativeTxBodyCanonicalValue => [
  Buffer.from(body.spendInputsPreimageCbor),
  Buffer.from(body.referenceInputsPreimageCbor),
  Buffer.from(body.outputsPreimageCbor),
  asUnsigned(body.fee, "transaction_body.fee"),
  asSigned(
    body.validityIntervalStart,
    "transaction_body.validity_interval_start",
  ),
  asSigned(body.validityIntervalEnd, "transaction_body.validity_interval_end"),
  Buffer.from(body.requiredObserversPreimageCbor),
  Buffer.from(body.requiredSignersPreimageCbor),
  Buffer.from(body.mintPreimageCbor),
  ensureHash32(
    body.scriptIntegrityHash,
    "transaction_body.script_integrity_hash",
  ),
  ensureHash32(body.auxiliaryDataHash, "transaction_body.auxiliary_data_hash"),
  asNativeNetworkId(body.networkId, "transaction_body.network_id"),
];

export const decodeNativeTxBodyCanonicalValue = (
  value: unknown,
  fieldName: string,
): MidgardNativeTxBodyCanonicalV1 => {
  const v = asFixedArray(value, 12, fieldName);
  return {
    spendInputsPreimageCbor: bytesItem(v, 0, fieldName),
    referenceInputsPreimageCbor: bytesItem(v, 1, fieldName),
    outputsPreimageCbor: bytesItem(v, 2, fieldName),
    fee: asUnsigned(v[3], `${fieldName}[3]`),
    validityIntervalStart: asSigned(v[4], `${fieldName}[4]`),
    validityIntervalEnd: asSigned(v[5], `${fieldName}[5]`),
    requiredObserversPreimageCbor: bytesItem(v, 6, fieldName),
    requiredSignersPreimageCbor: bytesItem(v, 7, fieldName),
    mintPreimageCbor: bytesItem(v, 8, fieldName),
    scriptIntegrityHash: hashItem(v, 9, fieldName),
    auxiliaryDataHash: hashItem(v, 10, fieldName),
    networkId: asNativeNetworkId(v[11], `${fieldName}[11]`),
  };
};

/**
 * The six body field commitments.
 *
 * **RETIRED counted-scheme derivation, still live here — owner #585.** This is
 * the one place the residual is written out in full; the other sites that carry
 * it (`deriveNativeTxWitnessSetCompact` and `@al-ft/midgard-sdk`'s
 * `EMPTY_SPEND_INPUTS_HASH`) point here.
 *
 * `docs/spec/midgard-tx.md` §4 makes every field commitment a flat
 * `blake2b_256` over the field's §5.1 preimage bytes; the Aiken side has
 * derived them that way since #567. The flat twin that would replace the six
 * calls below is `midgardFieldCommitmentV1`
 * (`native-tx-field-access-v1.ts`), which hashes preimage bytes — the same
 * commitment reached from a field's *items* is
 * `midgardFieldCommitmentForFieldV1` (`native-tx-field-items-v1.ts`), so a note
 * naming either one means this scheme. What is still here instead is the
 * counted bounded-collection Merkle root: each preimage decomposed into items,
 * every item hashed under a domain tag with its field and item index, the
 * leaves folded into a frontier, and *that* committed.
 *
 * It is deliberate, not an oversight, and it is why `EMPTY_SPEND_INPUTS_HASH`
 * still reads `eb25ed4a…` where §4 requires `45b0cfc2…`. The swap cannot be
 * made here alone: it also kills the counted per-item publication receipt chain
 * in `consensus-validation-v1.ts` — whose Aiken twin
 * `verify_midgard_transaction_field_chunk_v1` is already documented as
 * unsatisfiable under §4 — and it requires the §5.3/§5.6 item grammar to be
 * re-pointed in the `midgard-validation` and `lucid-midgard` producers, which
 * changes the bytes of every transaction TypeScript builds. #585 owns all of
 * it, and blocks #579's blueprint regeneration; #578 carries the measurement
 * that decomposed it.
 *
 * Until then this stays counted so that the codec, its fixtures and the
 * publication chain remain mutually consistent; a half-swap is the one state
 * that is worse than either end.
 */
export const deriveNativeTxBodyCompact = (
  body: MidgardNativeTxBodyCanonicalV1,
): MidgardNativeTxBodyCompactV1 => ({
  spendInputsHash: deriveMidgardNativeFieldCollectionV1({
    fieldIndex: 0,
    preimageCbor: body.spendInputsPreimageCbor,
  }).commitment,
  referenceInputsHash: deriveMidgardNativeFieldCollectionV1({
    fieldIndex: 1,
    preimageCbor: body.referenceInputsPreimageCbor,
  }).commitment,
  outputsHash: deriveMidgardNativeFieldCollectionV1({
    fieldIndex: 2,
    preimageCbor: body.outputsPreimageCbor,
  }).commitment,
  fee: body.fee,
  validityIntervalStart: body.validityIntervalStart,
  validityIntervalEnd: body.validityIntervalEnd,
  requiredObserversHash: deriveMidgardNativeFieldCollectionV1({
    fieldIndex: 3,
    preimageCbor: body.requiredObserversPreimageCbor,
  }).commitment,
  requiredSignersHash: deriveMidgardNativeFieldCollectionV1({
    fieldIndex: 4,
    preimageCbor: body.requiredSignersPreimageCbor,
  }).commitment,
  mintHash: deriveMidgardNativeFieldCollectionV1({
    fieldIndex: 5,
    preimageCbor: body.mintPreimageCbor,
  }).commitment,
  scriptIntegrityHash: body.scriptIntegrityHash,
  auxiliaryDataHash: body.auxiliaryDataHash,
  networkId: body.networkId,
});

export const encodeNativeTxBodyCompactCbor = (
  body: MidgardNativeTxBodyCompactV1,
): Buffer => encodeCbor(encodeNativeTxBodyCompactValue(body));

export const decodeNativeTxBodyCompactCbor = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCompactV1 =>
  decodeNativeTxBodyCompactValue(decodeSingleCbor(bytes), "transaction_body");

export const encodeNativeTxBodyCanonicalCbor = (
  body: MidgardNativeTxBodyCanonicalV1,
): Buffer => encodeCbor(encodeNativeTxBodyCanonicalValue(body));

export const decodeNativeTxBodyCanonicalCbor = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCanonicalV1 =>
  decodeNativeTxBodyCanonicalValue(decodeSingleCbor(bytes), "transaction_body");
