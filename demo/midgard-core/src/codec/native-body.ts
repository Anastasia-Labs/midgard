/**
 * Binary encoding for the MidgardNativeTx body (canonical + compact).
 *
 * Layout (canonical):
 *   Static:
 *     spend_inputs:      u64 len + n × outref_static (40 bytes each)
 *     reference_inputs:  u64 len + n × outref_static
 *     outputs:           u64 len + n × output.static
 *     fee:               u64
 *     validity_start:    i64
 *     validity_end:      i64
 *     required_observers: u64 len + n × var-bytes static (u64 len each)
 *     required_signers:  u64 len + n × hash28
 *     mint.static
 *     script_integrity_hash (32)
 *     auxiliary_data_hash   (32)
 *     network_id (u64)
 *   Dynamic:
 *     outputs[*].dynamic
 *     required_observers[*] bytes + pad
 *     mint.dynamic
 *
 * Compact form is fully static (12 fields, no dynamic section).
 */

import {
  BinaryReader,
  BinaryWriter,
  readBigI64,
  readBigU64,
  writeBigI64,
  writeBigU64,
} from "./binary.js";
import {
  readBytesListDynamic,
  readBytesListStatic,
  readHash28ListStatic,
  readHash32,
  readOutputReferenceListStatic,
  writeBytesListDynamic,
  writeBytesListStatic,
  writeHash28ListStatic,
  writeHash32,
  writeOutputReferenceListStatic,
  type OutputReference,
  type Hash28,
} from "./binary-types.js";
import { computeHash32, ensureHash32, type Hash32 } from "./hash.js";
import type {
  MidgardNativeTxBodyCanonical,
  MidgardNativeTxBodyCompact,
} from "./native.js";
import {
  readMidgardTxOutputListDynamic,
  readMidgardTxOutputListStatic,
  writeMidgardTxOutputListDynamic,
  writeMidgardTxOutputListStatic,
  type MidgardTxOutput,
} from "./output.js";
import {
  encodeMidgardMint,
  readMidgardMintDynamic,
  readMidgardMintStatic,
  writeMidgardMintDynamic,
  writeMidgardMintStatic,
} from "./value.js";

// ===========================================================================
// Canonical body (binary)
// ===========================================================================

export const writeNativeTxBodyCanonicalStatic = (
  w: BinaryWriter,
  body: MidgardNativeTxBodyCanonical,
): void => {
  writeOutputReferenceListStatic(w, body.spendInputs);
  writeOutputReferenceListStatic(w, body.referenceInputs);
  writeMidgardTxOutputListStatic(w, body.outputs);
  writeBigU64(w, body.fee);
  writeBigI64(w, body.validityIntervalStart);
  writeBigI64(w, body.validityIntervalEnd);
  writeBytesListStatic(w, body.requiredObservers);
  writeHash28ListStatic(w, body.requiredSigners);
  writeMidgardMintStatic(w, body.mint);
  writeHash32(w, body.scriptIntegrityHash);
  writeHash32(w, body.auxiliaryDataHash);
  writeBigU64(w, body.networkId);
};

export const writeNativeTxBodyCanonicalDynamic = (
  w: BinaryWriter,
  body: MidgardNativeTxBodyCanonical,
): void => {
  writeMidgardTxOutputListDynamic(w, body.outputs);
  writeBytesListDynamic(w, body.requiredObservers);
  writeMidgardMintDynamic(w, body.mint);
};

type BodyPartial = {
  readonly spendInputs: OutputReference[];
  readonly referenceInputs: OutputReference[];
  readonly outputPartials: ReturnType<typeof readMidgardTxOutputListStatic>;
  readonly fee: bigint;
  readonly validityIntervalStart: bigint;
  readonly validityIntervalEnd: bigint;
  readonly observerLengths: readonly number[];
  readonly requiredSigners: Hash28[];
  readonly mintPartial: ReturnType<typeof readMidgardMintStatic>;
  readonly scriptIntegrityHash: Hash32;
  readonly auxiliaryDataHash: Hash32;
  readonly networkId: bigint;
};

export const readNativeTxBodyCanonicalStatic = (
  r: BinaryReader,
): BodyPartial => {
  const spendInputs = readOutputReferenceListStatic(r);
  const referenceInputs = readOutputReferenceListStatic(r);
  const outputPartials = readMidgardTxOutputListStatic(r);
  const fee = readBigU64(r);
  const validityIntervalStart = readBigI64(r);
  const validityIntervalEnd = readBigI64(r);
  const { lengths: observerLengths } = readBytesListStatic(r);
  const requiredSigners = readHash28ListStatic(r);
  const mintPartial = readMidgardMintStatic(r);
  const scriptIntegrityHash = readHash32(r);
  const auxiliaryDataHash = readHash32(r);
  const networkId = readBigU64(r);
  return {
    spendInputs,
    referenceInputs,
    outputPartials,
    fee,
    validityIntervalStart,
    validityIntervalEnd,
    observerLengths,
    requiredSigners,
    mintPartial,
    scriptIntegrityHash,
    auxiliaryDataHash,
    networkId,
  };
};

export const readNativeTxBodyCanonicalDynamic = (
  r: BinaryReader,
  partial: BodyPartial,
): MidgardNativeTxBodyCanonical => {
  const outputs = readMidgardTxOutputListDynamic(r, partial.outputPartials);
  const requiredObservers = readBytesListDynamic(r, partial.observerLengths);
  const mint = readMidgardMintDynamic(r, partial.mintPartial.partial);
  return {
    spendInputs: partial.spendInputs,
    referenceInputs: partial.referenceInputs,
    outputs,
    fee: partial.fee,
    validityIntervalStart: partial.validityIntervalStart,
    validityIntervalEnd: partial.validityIntervalEnd,
    requiredObservers,
    requiredSigners: partial.requiredSigners,
    mint,
    scriptIntegrityHash: partial.scriptIntegrityHash,
    auxiliaryDataHash: partial.auxiliaryDataHash,
    networkId: partial.networkId,
  };
};

export const encodeNativeTxBodyCanonical = (
  body: MidgardNativeTxBodyCanonical,
): Buffer => {
  const sw = new BinaryWriter();
  writeNativeTxBodyCanonicalStatic(sw, body);
  const dw = new BinaryWriter();
  writeNativeTxBodyCanonicalDynamic(dw, body);
  return Buffer.concat([sw.toBytes(), dw.toBytes()]);
};

export const decodeNativeTxBodyCanonical = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCanonical => {
  const r = new BinaryReader(bytes);
  const partial = readNativeTxBodyCanonicalStatic(r);
  const body = readNativeTxBodyCanonicalDynamic(r, partial);
  r.expectEnd("transaction_body");
  return body;
};

// ===========================================================================
// Compact body (binary, fully static — 12 fields)
// ===========================================================================

export const writeNativeTxBodyCompactStatic = (
  w: BinaryWriter,
  body: MidgardNativeTxBodyCompact,
): void => {
  writeHash32(w, body.spendInputsHash);
  writeHash32(w, body.referenceInputsHash);
  writeHash32(w, body.outputsHash);
  writeBigU64(w, body.fee);
  writeBigI64(w, body.validityIntervalStart);
  writeBigI64(w, body.validityIntervalEnd);
  writeHash32(w, body.requiredObserversHash);
  writeHash32(w, body.requiredSignersHash);
  writeHash32(w, body.mintHash);
  writeHash32(w, body.scriptIntegrityHash);
  writeHash32(w, body.auxiliaryDataHash);
  writeBigU64(w, body.networkId);
};

export const readNativeTxBodyCompactStatic = (
  r: BinaryReader,
): MidgardNativeTxBodyCompact => ({
  spendInputsHash: readHash32(r),
  referenceInputsHash: readHash32(r),
  outputsHash: readHash32(r),
  fee: readBigU64(r),
  validityIntervalStart: readBigI64(r),
  validityIntervalEnd: readBigI64(r),
  requiredObserversHash: readHash32(r),
  requiredSignersHash: readHash32(r),
  mintHash: readHash32(r),
  scriptIntegrityHash: readHash32(r),
  auxiliaryDataHash: readHash32(r),
  networkId: readBigU64(r),
});

export const encodeNativeTxBodyCompact = (
  body: MidgardNativeTxBodyCompact,
): Buffer => {
  const w = new BinaryWriter();
  writeNativeTxBodyCompactStatic(w, body);
  return w.toBytes();
};

export const decodeNativeTxBodyCompact = (
  bytes: Uint8Array,
): MidgardNativeTxBodyCompact => {
  const r = new BinaryReader(bytes);
  const body = readNativeTxBodyCompactStatic(r);
  r.expectEnd("transaction_body_compact");
  return body;
};

// ===========================================================================
// Derivation: per-field hashes from typed preimages.
// ===========================================================================

const writeBytesListAll = (list: readonly Uint8Array[]): Buffer => {
  const sw = new BinaryWriter();
  writeBytesListStatic(sw, list);
  const dw = new BinaryWriter();
  writeBytesListDynamic(dw, list);
  return Buffer.concat([sw.toBytes(), dw.toBytes()]);
};

const writeOutputRefListAll = (
  list: readonly OutputReference[],
): Buffer => {
  const w = new BinaryWriter();
  writeOutputReferenceListStatic(w, list);
  return w.toBytes();
};

const writeOutputsAll = (outputs: readonly MidgardTxOutput[]): Buffer => {
  const sw = new BinaryWriter();
  writeMidgardTxOutputListStatic(sw, outputs);
  const dw = new BinaryWriter();
  writeMidgardTxOutputListDynamic(dw, outputs);
  return Buffer.concat([sw.toBytes(), dw.toBytes()]);
};

const writeHash28ListAll = (list: readonly Hash28[]): Buffer => {
  const w = new BinaryWriter();
  writeHash28ListStatic(w, list);
  return w.toBytes();
};

export const deriveNativeTxBodyCompact = (
  body: MidgardNativeTxBodyCanonical,
): MidgardNativeTxBodyCompact => ({
  spendInputsHash: computeHash32(writeOutputRefListAll(body.spendInputs)),
  referenceInputsHash: computeHash32(writeOutputRefListAll(body.referenceInputs)),
  outputsHash: computeHash32(writeOutputsAll(body.outputs)),
  fee: body.fee,
  validityIntervalStart: body.validityIntervalStart,
  validityIntervalEnd: body.validityIntervalEnd,
  requiredObserversHash: computeHash32(writeBytesListAll(body.requiredObservers)),
  requiredSignersHash: computeHash32(writeHash28ListAll(body.requiredSigners)),
  mintHash: computeHash32(encodeMidgardMint(body.mint)),
  scriptIntegrityHash: ensureHash32(
    body.scriptIntegrityHash,
    "transaction_body.script_integrity_hash",
  ),
  auxiliaryDataHash: ensureHash32(
    body.auxiliaryDataHash,
    "transaction_body.auxiliary_data_hash",
  ),
  networkId: body.networkId,
});

// Re-exports for the field-level encoders (used by phase-a / phase-b /
// midgard-node which previously hashed the preimage CBOR directly).
export const encodeSpendInputsBinary = (
  inputs: readonly OutputReference[],
): Buffer => writeOutputRefListAll(inputs);
export const encodeReferenceInputsBinary = encodeSpendInputsBinary;
export const encodeOutputsBinary = (
  outputs: readonly MidgardTxOutput[],
): Buffer => writeOutputsAll(outputs);
export const encodeRequiredObserversBinary = (
  list: readonly Uint8Array[],
): Buffer => writeBytesListAll(list);
export const encodeRequiredSignersBinary = (
  list: readonly Hash28[],
): Buffer => writeHash28ListAll(list);
