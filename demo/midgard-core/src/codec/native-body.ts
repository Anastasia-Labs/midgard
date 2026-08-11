import { decodeSingleCbor, encodeCbor } from "./cbor.js";
import { ensureHash32, type Hash32 } from "./hash.js";
import type {
  MidgardNativeTxBodyCanonicalV1,
  MidgardNativeTxBodyCompactV1,
} from "./native.js";
import { MIDGARD_NATIVE_NETWORK_ID_NONE } from "./native-constants.js";
import { midgardFieldCommitmentV1 } from "./native-tx-field-access-v1.js";
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
 * The six body field commitments, `docs/spec/midgard-tx.md` §4.
 *
 * Each one is a plain `blake2b_256` over the field's §5.1 preimage bytes: no
 * domain tag, no version prefix, no field index in the hash input. That is the
 * whole derivation — {@link midgardFieldCommitmentV1} is `computeHash32`, and a
 * watcher recomputing these needs the raw bytes and nothing else.
 *
 * **The preimage is not re-validated here, deliberately.** §4's commitment is
 * defined over bytes, and the Aiken twins hash first and walk the grammar
 * separately (`verify_canonical_mint_preimage_cbor` is the pattern: one
 * `field_commitment` check, then an in-place walk). Making this function decode
 * would put a §5.1 parse on the honest path of every transaction — the cost the
 * reversion exists to remove — and would give the format a second verdict on
 * canonicality. Callers that need the grammar checked use
 * {@link decodeMidgardFieldPreimageV1} or the per-field readers in
 * `native-tx-field-item-decoders-v1.ts`; producers that build the bytes go
 * through `encodeMidgardFieldPreimageForFieldV1`, which cannot emit a
 * non-canonical preimage.
 *
 * Field identity is positional (§4), so fields 0/1 and 3/4 alias on identical
 * content; the positional-identity invariant is what makes that safe, and it is
 * enforced at the verification entry points, not here.
 */
export const deriveNativeTxBodyCompact = (
  body: MidgardNativeTxBodyCanonicalV1,
): MidgardNativeTxBodyCompactV1 => ({
  spendInputsHash: midgardFieldCommitmentV1(body.spendInputsPreimageCbor),
  referenceInputsHash: midgardFieldCommitmentV1(
    body.referenceInputsPreimageCbor,
  ),
  outputsHash: midgardFieldCommitmentV1(body.outputsPreimageCbor),
  fee: body.fee,
  validityIntervalStart: body.validityIntervalStart,
  validityIntervalEnd: body.validityIntervalEnd,
  requiredObserversHash: midgardFieldCommitmentV1(
    body.requiredObserversPreimageCbor,
  ),
  requiredSignersHash: midgardFieldCommitmentV1(
    body.requiredSignersPreimageCbor,
  ),
  mintHash: midgardFieldCommitmentV1(body.mintPreimageCbor),
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
