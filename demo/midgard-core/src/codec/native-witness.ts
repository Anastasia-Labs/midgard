/**
 * Binary encoding for the MidgardNativeTx witness set (canonical + compact).
 *
 * Layout (canonical):
 *   Static:
 *     addr_tx_wits:   u64 len + n × vkeywitness (96 bytes each)
 *     script_tx_wits: u64 len + n × versioned_script.static
 *     redeemer_tx_wits.len (u64)   — opaque CBOR redeemer-set length
 *   Dynamic:
 *     script_tx_wits[*].dynamic (scriptBytes + pad)
 *     redeemer_tx_wits bytes + pad
 *
 * Compact form is fully static (3 hashes).
 */

import {
  BinaryReader,
  BinaryWriter,
  readVarBytesDynamic,
  readVarBytesLen,
  writeVarBytesDynamic,
  writeVarBytesStatic,
} from "./binary.js";
import {
  readHash32,
  readVKeyWitnessListStatic,
  writeHash32,
  writeVKeyWitnessListStatic,
} from "./binary-types.js";
import { computeHash32 } from "./hash.js";
import type {
  MidgardNativeTxWitnessSetCanonical,
  MidgardNativeTxWitnessSetCompact,
} from "./native.js";
import {
  readMidgardVersionedScriptListDynamic,
  readMidgardVersionedScriptListStatic,
  writeMidgardVersionedScriptListDynamic,
  writeMidgardVersionedScriptListStatic,
} from "./versioned-script.js";

// ===========================================================================
// Canonical witness set (binary)
// ===========================================================================

export const writeNativeTxWitnessSetCanonicalStatic = (
  w: BinaryWriter,
  ws: MidgardNativeTxWitnessSetCanonical,
): void => {
  writeVKeyWitnessListStatic(w, ws.addrTxWits);
  writeMidgardVersionedScriptListStatic(w, ws.scriptTxWits);
  writeVarBytesStatic(w, ws.redeemerTxWits);
};

export const writeNativeTxWitnessSetCanonicalDynamic = (
  w: BinaryWriter,
  ws: MidgardNativeTxWitnessSetCanonical,
): void => {
  writeMidgardVersionedScriptListDynamic(w, ws.scriptTxWits);
  writeVarBytesDynamic(w, ws.redeemerTxWits);
};

type WitnessPartial = {
  readonly addrTxWits: ReturnType<typeof readVKeyWitnessListStatic>;
  readonly scriptPartials: ReturnType<typeof readMidgardVersionedScriptListStatic>;
  readonly redeemerLen: number;
};

export const readNativeTxWitnessSetCanonicalStatic = (
  r: BinaryReader,
): WitnessPartial => {
  const addrTxWits = readVKeyWitnessListStatic(r);
  const scriptPartials = readMidgardVersionedScriptListStatic(r);
  const redeemerLen = readVarBytesLen(r);
  return { addrTxWits, scriptPartials, redeemerLen };
};

export const readNativeTxWitnessSetCanonicalDynamic = (
  r: BinaryReader,
  partial: WitnessPartial,
): MidgardNativeTxWitnessSetCanonical => {
  const scriptTxWits = readMidgardVersionedScriptListDynamic(
    r,
    partial.scriptPartials,
  );
  const redeemerTxWits = readVarBytesDynamic(r, partial.redeemerLen);
  return {
    addrTxWits: partial.addrTxWits,
    scriptTxWits,
    redeemerTxWits,
  };
};

export const encodeNativeTxWitnessSetCanonical = (
  ws: MidgardNativeTxWitnessSetCanonical,
): Buffer => {
  const sw = new BinaryWriter();
  writeNativeTxWitnessSetCanonicalStatic(sw, ws);
  const dw = new BinaryWriter();
  writeNativeTxWitnessSetCanonicalDynamic(dw, ws);
  return Buffer.concat([sw.toBytes(), dw.toBytes()]);
};

export const decodeNativeTxWitnessSetCanonical = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCanonical => {
  const r = new BinaryReader(bytes);
  const partial = readNativeTxWitnessSetCanonicalStatic(r);
  const ws = readNativeTxWitnessSetCanonicalDynamic(r, partial);
  r.expectEnd("transaction_witness_set");
  return ws;
};

// ===========================================================================
// Compact witness set (binary, fully static — 3 hashes)
// ===========================================================================

export const writeNativeTxWitnessSetCompactStatic = (
  w: BinaryWriter,
  ws: MidgardNativeTxWitnessSetCompact,
): void => {
  writeHash32(w, ws.addrTxWitsHash);
  writeHash32(w, ws.scriptTxWitsHash);
  writeHash32(w, ws.redeemerTxWitsHash);
};

export const readNativeTxWitnessSetCompactStatic = (
  r: BinaryReader,
): MidgardNativeTxWitnessSetCompact => ({
  addrTxWitsHash: readHash32(r),
  scriptTxWitsHash: readHash32(r),
  redeemerTxWitsHash: readHash32(r),
});

export const encodeNativeTxWitnessSetCompact = (
  ws: MidgardNativeTxWitnessSetCompact,
): Buffer => {
  const w = new BinaryWriter();
  writeNativeTxWitnessSetCompactStatic(w, ws);
  return w.toBytes();
};

export const decodeNativeTxWitnessSetCompact = (
  bytes: Uint8Array,
): MidgardNativeTxWitnessSetCompact => {
  const r = new BinaryReader(bytes);
  const ws = readNativeTxWitnessSetCompactStatic(r);
  r.expectEnd("transaction_witness_set_compact");
  return ws;
};

// ===========================================================================
// Derivation: per-field hashes from typed preimages.
// ===========================================================================

const writeAddrTxWitsAll = (
  ws: MidgardNativeTxWitnessSetCanonical,
): Buffer => {
  const w = new BinaryWriter();
  writeVKeyWitnessListStatic(w, ws.addrTxWits);
  return w.toBytes();
};

const writeScriptTxWitsAll = (
  ws: MidgardNativeTxWitnessSetCanonical,
): Buffer => {
  const sw = new BinaryWriter();
  writeMidgardVersionedScriptListStatic(sw, ws.scriptTxWits);
  const dw = new BinaryWriter();
  writeMidgardVersionedScriptListDynamic(dw, ws.scriptTxWits);
  return Buffer.concat([sw.toBytes(), dw.toBytes()]);
};

const writeRedeemerTxWitsAll = (
  ws: MidgardNativeTxWitnessSetCanonical,
): Buffer => {
  const sw = new BinaryWriter();
  writeVarBytesStatic(sw, ws.redeemerTxWits);
  const dw = new BinaryWriter();
  writeVarBytesDynamic(dw, ws.redeemerTxWits);
  return Buffer.concat([sw.toBytes(), dw.toBytes()]);
};

export const deriveNativeTxWitnessSetCompact = (
  ws: MidgardNativeTxWitnessSetCanonical,
): MidgardNativeTxWitnessSetCompact => ({
  addrTxWitsHash: computeHash32(writeAddrTxWitsAll(ws)),
  scriptTxWitsHash: computeHash32(writeScriptTxWitsAll(ws)),
  redeemerTxWitsHash: computeHash32(writeRedeemerTxWitsAll(ws)),
});

export const encodeAddrTxWitsBinary = (
  ws: MidgardNativeTxWitnessSetCanonical,
): Buffer => writeAddrTxWitsAll(ws);

export const encodeScriptTxWitsBinary = (
  ws: MidgardNativeTxWitnessSetCanonical,
): Buffer => writeScriptTxWitsAll(ws);

export const encodeRedeemerTxWitsBinary = (
  ws: MidgardNativeTxWitnessSetCanonical,
): Buffer => writeRedeemerTxWitsAll(ws);
