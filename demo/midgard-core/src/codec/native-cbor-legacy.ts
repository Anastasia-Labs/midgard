// Decoders for the legacy CBOR wire format used by midgard-core on the
// `tx-validation` branch, with adapters that re-encode the result using the
// current binary format.
//
// The compact representation is byte-compatible across branches (it carries
// only hashes), so legacy-CBOR fixtures' compact bytes feed straight in. The
// canonical representation is *not* byte-compatible: the legacy CBOR carries
// per-field preimages as CBOR-array-of-CBOR-bytes, whereas the current branch
// expects each preimage to be the corresponding binary list encoding. The
// helpers below re-encode each preimage from legacy CBOR to binary.

import { CML } from "@lucid-evolution/lucid";
import { asArray, asBytes, asMap, decodeSingleCbor } from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import { ensureHash32, type Hash32 } from "./hash.js";
import {
  asFixedArray,
  asSigned,
  asUnsigned,
  decodeValidityCode,
  decodeVersion,
} from "./native-validation.js";
import {
  EMPTY_PREIMAGE_LIST,
  encodeMidgardBytesListPreimage,
  encodeMidgardHash28ListPreimage,
  encodeMidgardMintPreimage,
  encodeMidgardOutputReferenceListPreimage,
  encodeMidgardVKeyWitnessListPreimage,
  type MidgardMintAsset,
  type MidgardMintPolicy,
  type MidgardOutputReference,
  type MidgardVKeyWitness,
} from "./native-preimage.js";
import {
  decodeMidgardNativeTxBodyCompact as decodeBinaryBodyCompact,
  decodeMidgardNativeTxCompact as decodeBinaryTxCompact,
  decodeMidgardNativeTxFull as decodeBinaryTxFull,
  decodeMidgardNativeTxWitnessSetCompact as decodeBinaryWitnessCompact,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxFull,
  encodeMidgardNativeTxWitnessSetCompact,
  materializeMidgardNativeTxFromCanonical,
  verifyMidgardNativeTxFullConsistency,
  type MidgardNativeCodecOptions,
  type MidgardNativeTxBodyCanonical,
  type MidgardNativeTxBodyCompact,
  type MidgardNativeTxCanonical,
  type MidgardNativeTxCompact,
  type MidgardNativeTxFull,
  type MidgardNativeTxWitnessSetCanonical,
  type MidgardNativeTxWitnessSetCompact,
} from "./native.js";

const hashItem = (
  arr: readonly unknown[],
  index: number,
  fieldName: string,
): Hash32 => {
  const field = `${fieldName}[${index}]`;
  return ensureHash32(asBytes(arr[index], field), field);
};

const bytesItem = (
  arr: readonly unknown[],
  index: number,
  fieldName: string,
): Buffer => Buffer.from(asBytes(arr[index], `${fieldName}[${index}]`));

const failLegacy = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.SchemaMismatch,
    message,
    detail,
  );
};

// ---------------------------------------------------------------------------
// Per-field legacy-CBOR → binary preimage translators.
// ---------------------------------------------------------------------------

/** Legacy CBOR-array of CBOR-encoded TransactionInputs → binary OutputRef list. */
const legacyCborToOutputRefPreimage = (
  cborBytes: Uint8Array,
  fieldName: string,
): Buffer => {
  const arr = asArray(decodeSingleCbor(cborBytes), fieldName);
  if (arr.length === 0) return Buffer.from(EMPTY_PREIMAGE_LIST);
  const refs: MidgardOutputReference[] = arr.map((item, i) => {
    const input = CML.TransactionInput.from_cbor_bytes(
      asBytes(item, `${fieldName}[${i}]`),
    );
    const idx = input.index();
    if (idx > 0xffffn) {
      failLegacy(`${fieldName}[${i}].index exceeds u16`, idx.toString(10));
    }
    return {
      txId: Buffer.from(input.transaction_id().to_raw_bytes()),
      index: Number(idx),
    };
  });
  return encodeMidgardOutputReferenceListPreimage(refs);
};

/** Legacy CBOR-array of 28-byte bytestrings → binary Hash28 list preimage. */
const legacyCborToHash28Preimage = (
  cborBytes: Uint8Array,
  fieldName: string,
): Buffer => {
  const arr = asArray(decodeSingleCbor(cborBytes), fieldName);
  if (arr.length === 0) return Buffer.from(EMPTY_PREIMAGE_LIST);
  const hashes = arr.map((item, i) =>
    Buffer.from(asBytes(item, `${fieldName}[${i}]`)),
  );
  return encodeMidgardHash28ListPreimage(hashes, fieldName);
};

/**
 * Legacy CBOR-array of opaque CBOR-encoded entries (outputs, scripts) → binary
 * bytes list preimage. Per-entry bytes are preserved verbatim; only the outer
 * framing changes.
 */
const legacyCborToBytesListPreimage = (
  cborBytes: Uint8Array,
  fieldName: string,
): Buffer => {
  const arr = asArray(decodeSingleCbor(cborBytes), fieldName);
  if (arr.length === 0) return Buffer.from(EMPTY_PREIMAGE_LIST);
  const entries = arr.map((item, i) =>
    Buffer.from(asBytes(item, `${fieldName}[${i}]`)),
  );
  return encodeMidgardBytesListPreimage(entries);
};

/** Legacy CBOR-array of CBOR-encoded vkey witnesses → binary VKeyWitness list. */
const legacyCborToVKeyWitnessPreimage = (
  cborBytes: Uint8Array,
  fieldName: string,
): Buffer => {
  const arr = asArray(decodeSingleCbor(cborBytes), fieldName);
  if (arr.length === 0) return Buffer.from(EMPTY_PREIMAGE_LIST);
  const witnesses: MidgardVKeyWitness[] = arr.map((item, i) => {
    const w = CML.Vkeywitness.from_cbor_bytes(
      asBytes(item, `${fieldName}[${i}]`),
    );
    return {
      vkey: Buffer.from(w.vkey().to_raw_bytes()),
      signature: Buffer.from(w.ed25519_signature().to_raw_bytes()),
    };
  });
  return encodeMidgardVKeyWitnessListPreimage(witnesses);
};

/** Legacy CBOR map (policy_id → asset_name → i64) → binary Mint preimage. */
const legacyCborToMintPreimage = (
  cborBytes: Uint8Array,
  fieldName: string,
): Buffer => {
  const decoded = decodeSingleCbor(cborBytes);
  if (Array.isArray(decoded)) {
    if (decoded.length === 0) return Buffer.from(EMPTY_PREIMAGE_LIST);
    failLegacy(
      `${fieldName} legacy mint must be empty array or CBOR map`,
      `length=${decoded.length}`,
    );
  }
  const policies = asMap(decoded, fieldName);
  const out: MidgardMintPolicy[] = [];
  let policyIndex = 0;
  for (const [policyVal, assetsVal] of policies.entries()) {
    const policyId = Buffer.from(
      asBytes(policyVal, `${fieldName}.policy[${policyIndex}]`),
    );
    const assets = asMap(assetsVal, `${fieldName}.assets[${policyIndex}]`);
    const assetEntries: MidgardMintAsset[] = [];
    let assetIndex = 0;
    for (const [nameVal, amountVal] of assets.entries()) {
      const name = Buffer.from(
        asBytes(nameVal, `${fieldName}.asset_name[${policyIndex}.${assetIndex}]`),
      );
      const amount = asSigned(
        amountVal,
        `${fieldName}.amount[${policyIndex}.${assetIndex}]`,
      );
      assetEntries.push({ name, amount });
      assetIndex++;
    }
    out.push({ policyId, assets: assetEntries });
    policyIndex++;
  }
  return encodeMidgardMintPreimage(out);
};

const decodeLegacyBodyCompactValue = (
  value: unknown,
  fieldName: string,
): MidgardNativeTxBodyCompact => {
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
    networkId: asUnsigned(v[11], `${fieldName}[11]`),
  };
};

const decodeLegacyBodyCanonicalValue = (
  value: unknown,
  fieldName: string,
): MidgardNativeTxBodyCanonical => {
  const v = asFixedArray(value, 12, fieldName);
  return {
    spendInputsPreimage: legacyCborToOutputRefPreimage(
      bytesItem(v, 0, fieldName),
      `${fieldName}.spend_inputs`,
    ),
    referenceInputsPreimage: legacyCborToOutputRefPreimage(
      bytesItem(v, 1, fieldName),
      `${fieldName}.reference_inputs`,
    ),
    outputsPreimage: legacyCborToBytesListPreimage(
      bytesItem(v, 2, fieldName),
      `${fieldName}.outputs`,
    ),
    fee: asUnsigned(v[3], `${fieldName}[3]`),
    validityIntervalStart: asSigned(v[4], `${fieldName}[4]`),
    validityIntervalEnd: asSigned(v[5], `${fieldName}[5]`),
    requiredObserversPreimage: legacyCborToHash28Preimage(
      bytesItem(v, 6, fieldName),
      `${fieldName}.required_observers`,
    ),
    requiredSignersPreimage: legacyCborToHash28Preimage(
      bytesItem(v, 7, fieldName),
      `${fieldName}.required_signers`,
    ),
    mintPreimage: legacyCborToMintPreimage(
      bytesItem(v, 8, fieldName),
      `${fieldName}.mint`,
    ),
    scriptIntegrityHash: hashItem(v, 9, fieldName),
    auxiliaryDataHash: hashItem(v, 10, fieldName),
    networkId: asUnsigned(v[11], `${fieldName}[11]`),
  };
};

const decodeLegacyWitnessCompactValue = (
  value: unknown,
  fieldName: string,
): MidgardNativeTxWitnessSetCompact => {
  const v = asFixedArray(value, 3, fieldName);
  return {
    addrTxWitsHash: hashItem(v, 0, fieldName),
    scriptTxWitsHash: hashItem(v, 1, fieldName),
    redeemerTxWitsHash: hashItem(v, 2, fieldName),
  };
};

const decodeLegacyWitnessCanonicalValue = (
  value: unknown,
  fieldName: string,
): MidgardNativeTxWitnessSetCanonical => {
  const v = asFixedArray(value, 3, fieldName);
  return {
    addrTxWitsPreimage: legacyCborToVKeyWitnessPreimage(
      bytesItem(v, 0, fieldName),
      `${fieldName}.addr_tx_wits`,
    ),
    scriptTxWitsPreimage: legacyCborToBytesListPreimage(
      bytesItem(v, 1, fieldName),
      `${fieldName}.script_tx_wits`,
    ),
    // Redeemers were a single opaque CBOR blob in the legacy format too.
    redeemerTxWitsPreimage: bytesItem(v, 2, fieldName),
  };
};

export const decodeLegacyCborMidgardNativeTxBodyCompact = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCompact =>
  decodeLegacyBodyCompactValue(decodeSingleCbor(bytes), "transaction_body");

export const decodeLegacyCborMidgardNativeTxWitnessSetCompact = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCompact =>
  decodeLegacyWitnessCompactValue(
    decodeSingleCbor(bytes),
    "transaction_witness_set",
  );

export const decodeLegacyCborMidgardNativeTxCompact = (
  bytes: Uint8Array,
): MidgardNativeTxCompact => {
  const v = asFixedArray(decodeSingleCbor(bytes), 4, "transaction_compact");
  return {
    version: decodeVersion(v[0], "transaction_compact[0]"),
    transactionBody: decodeLegacyBodyCompactValue(
      v[1],
      "transaction_compact[1]",
    ),
    transactionWitnessSetHash: ensureHash32(
      asBytes(v[2], "transaction_compact[2]"),
      "transaction_compact[2]",
    ),
    validity: decodeValidityCode(v[3], "transaction_compact[3]"),
  };
};

export const decodeLegacyCborMidgardNativeTxFull = (
  bytes: Uint8Array,
  options: MidgardNativeCodecOptions = {},
): MidgardNativeTxFull => {
  const v = asFixedArray(decodeSingleCbor(bytes), 4, "transaction");
  const canonical: MidgardNativeTxCanonical = {
    version: decodeVersion(v[0], "transaction[0]"),
    body: decodeLegacyBodyCanonicalValue(v[1], "transaction[1]"),
    witnessSet: decodeLegacyWitnessCanonicalValue(v[2], "transaction[2]"),
    validity: decodeValidityCode(v[3], "transaction[3]"),
  };
  const tx = materializeMidgardNativeTxFromCanonical(canonical);
  if (options.enforceConsistency !== false) {
    verifyMidgardNativeTxFullConsistency(tx);
  }
  return tx;
};

export const convertLegacyCborToBinaryMidgardNativeTxBodyCompact = (
  legacyCbor: Uint8Array,
): Buffer =>
  encodeMidgardNativeTxBodyCompact(
    decodeLegacyCborMidgardNativeTxBodyCompact(legacyCbor),
  );

export const convertLegacyCborToBinaryMidgardNativeTxWitnessSetCompact = (
  legacyCbor: Uint8Array,
): Buffer =>
  encodeMidgardNativeTxWitnessSetCompact(
    decodeLegacyCborMidgardNativeTxWitnessSetCompact(legacyCbor),
  );

export const convertLegacyCborToBinaryMidgardNativeTxCompact = (
  legacyCbor: Uint8Array,
): Buffer =>
  encodeMidgardNativeTxCompact(
    decodeLegacyCborMidgardNativeTxCompact(legacyCbor),
  );

export const convertLegacyCborToBinaryMidgardNativeTxFull = (
  legacyCbor: Uint8Array,
  options: MidgardNativeCodecOptions = {},
): Buffer =>
  encodeMidgardNativeTxFull(
    decodeLegacyCborMidgardNativeTxFull(legacyCbor, options),
    options,
  );

// Reverse direction is also useful for round-trip tests: decode a binary blob
// produced on this branch and confirm it matches the legacy-CBOR fixture's
// in-memory shape.
export const decodeBinaryMidgardNativeTxCompact = decodeBinaryTxCompact;
export const decodeBinaryMidgardNativeTxBodyCompact = decodeBinaryBodyCompact;
export const decodeBinaryMidgardNativeTxWitnessSetCompact =
  decodeBinaryWitnessCompact;
export const decodeBinaryMidgardNativeTxFull = decodeBinaryTxFull;
