/**
 * Binary codec for the Midgard native transaction witness set
 * (canonical/full and compact representations).
 *
 * Replaces the previous CBOR encoding with the fuel-vm-style static/dynamic
 * binary layout (see ./binary.ts). The three witness fields stay opaque CBOR
 * preimage blobs; only the surrounding witness-set structure is binary.
 */

import {
  BinaryReader,
  BinaryWriter,
  ensureNoTrailingBytes,
  readHash32,
  readVarBytesDynamic,
  readVarBytesLen,
  writeHash32,
  writeVarBytesDynamic,
  writeVarBytesStatic,
} from "./binary.js";
import { computeHash32, ensureHash32 } from "./hash.js";
import type {
  MidgardNativeTxWitnessSetCanonical,
  MidgardNativeTxWitnessSetCompact,
} from "./native.js";

// ===========================================================================
// Canonical (full) witness set — three opaque CBOR preimage blobs.
//
// Static:  addr / script / redeemer blob lengths (u64)
// Dynamic: addr / script / redeemer blobs (bytes + alignment padding)
// ===========================================================================

/** Static-phase decode state: the three blob lengths. */
export interface NativeTxWitnessSetCanonicalPartial {
  addrLen: number;
  scriptLen: number;
  redeemerLen: number;
}

export const writeNativeTxWitnessSetCanonicalStatic = (
  w: BinaryWriter,
  witnessSet: MidgardNativeTxWitnessSetCanonical,
): void => {
  writeVarBytesStatic(w, witnessSet.addrTxWitsPreimageCbor);
  writeVarBytesStatic(w, witnessSet.scriptTxWitsPreimageCbor);
  writeVarBytesStatic(w, witnessSet.redeemerTxWitsPreimageCbor);
};

export const writeNativeTxWitnessSetCanonicalDynamic = (
  w: BinaryWriter,
  witnessSet: MidgardNativeTxWitnessSetCanonical,
): void => {
  writeVarBytesDynamic(w, witnessSet.addrTxWitsPreimageCbor);
  writeVarBytesDynamic(w, witnessSet.scriptTxWitsPreimageCbor);
  writeVarBytesDynamic(w, witnessSet.redeemerTxWitsPreimageCbor);
};

export const readNativeTxWitnessSetCanonicalStatic = (
  r: BinaryReader,
): NativeTxWitnessSetCanonicalPartial => {
  const addrLen = readVarBytesLen(r);
  const scriptLen = readVarBytesLen(r);
  const redeemerLen = readVarBytesLen(r);
  return { addrLen, scriptLen, redeemerLen };
};

export const readNativeTxWitnessSetCanonicalDynamic = (
  r: BinaryReader,
  p: NativeTxWitnessSetCanonicalPartial,
): MidgardNativeTxWitnessSetCanonical => {
  const addrTxWitsPreimageCbor = readVarBytesDynamic(r, p.addrLen);
  const scriptTxWitsPreimageCbor = readVarBytesDynamic(r, p.scriptLen);
  const redeemerTxWitsPreimageCbor = readVarBytesDynamic(r, p.redeemerLen);
  return {
    addrTxWitsPreimageCbor,
    scriptTxWitsPreimageCbor,
    redeemerTxWitsPreimageCbor,
  };
};

export const encodeNativeTxWitnessSetCanonical = (
  witnessSet: MidgardNativeTxWitnessSetCanonical,
): Buffer => {
  const w = new BinaryWriter();
  writeNativeTxWitnessSetCanonicalStatic(w, witnessSet);
  writeNativeTxWitnessSetCanonicalDynamic(w, witnessSet);
  return w.toBytes();
};

export const decodeNativeTxWitnessSetCanonical = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCanonical => {
  const r = new BinaryReader(bytes);
  const partial = readNativeTxWitnessSetCanonicalStatic(r);
  const witnessSet = readNativeTxWitnessSetCanonicalDynamic(r, partial);
  ensureNoTrailingBytes(r, "transaction_witness_set");
  return witnessSet;
};

// ===========================================================================
// Compact witness set — three 32-byte hashes, fully static.
// ===========================================================================

export const writeNativeTxWitnessSetCompact = (
  w: BinaryWriter,
  witnessSet: MidgardNativeTxWitnessSetCompact,
): void => {
  writeHash32(
    w,
    ensureHash32(
      witnessSet.addrTxWitsHash,
      "transaction_witness_set_compact.addr_tx_wits_hash",
    ),
  );
  writeHash32(
    w,
    ensureHash32(
      witnessSet.scriptTxWitsHash,
      "transaction_witness_set_compact.script_tx_wits_hash",
    ),
  );
  writeHash32(
    w,
    ensureHash32(
      witnessSet.redeemerTxWitsHash,
      "transaction_witness_set_compact.redeemer_tx_wits_hash",
    ),
  );
};

export const readNativeTxWitnessSetCompact = (
  r: BinaryReader,
): MidgardNativeTxWitnessSetCompact => {
  const addrTxWitsHash = ensureHash32(
    readHash32(r),
    "transaction_witness_set_compact.addr_tx_wits_hash",
  );
  const scriptTxWitsHash = ensureHash32(
    readHash32(r),
    "transaction_witness_set_compact.script_tx_wits_hash",
  );
  const redeemerTxWitsHash = ensureHash32(
    readHash32(r),
    "transaction_witness_set_compact.redeemer_tx_wits_hash",
  );
  return { addrTxWitsHash, scriptTxWitsHash, redeemerTxWitsHash };
};

export const encodeNativeTxWitnessSetCompact = (
  witnessSet: MidgardNativeTxWitnessSetCompact,
): Buffer => {
  const w = new BinaryWriter();
  writeNativeTxWitnessSetCompact(w, witnessSet);
  return w.toBytes();
};

export const decodeNativeTxWitnessSetCompact = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCompact => {
  const r = new BinaryReader(bytes);
  const witnessSet = readNativeTxWitnessSetCompact(r);
  ensureNoTrailingBytes(r, "transaction_witness_set_compact");
  return witnessSet;
};

// ===========================================================================
// Derivation: compact witness set = canonical witness set with each preimage
// blob replaced by its blake2b hash.
// ===========================================================================

export const deriveNativeTxWitnessSetCompact = (
  witnessSet: MidgardNativeTxWitnessSetCanonical,
): MidgardNativeTxWitnessSetCompact => ({
  addrTxWitsHash: computeHash32(witnessSet.addrTxWitsPreimageCbor),
  scriptTxWitsHash: computeHash32(witnessSet.scriptTxWitsPreimageCbor),
  redeemerTxWitsHash: computeHash32(witnessSet.redeemerTxWitsPreimageCbor),
});
