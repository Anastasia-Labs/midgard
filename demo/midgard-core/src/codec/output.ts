import { encodeMidgardAddressBytes, type MidgardAddress } from "./address.js";
import {
  BinaryReader,
  BinaryWriter,
  readBool,
  readVarBytesDynamic,
  readVarBytesLen,
  writeBool,
  writeVarBytesDynamic,
  writeVarBytesStatic,
} from "./binary.js";
import {
  decodeMidgardDatum,
  encodeMidgardDatum,
  type MidgardDatum,
} from "./datum.js";
import {
  readMidgardValueDynamic,
  readMidgardValueStatic,
  writeMidgardValueDynamic,
  writeMidgardValueStatic,
  type MidgardValue,
} from "./value.js";
import {
  readMidgardVersionedScriptDynamic,
  readMidgardVersionedScriptStatic,
  writeMidgardVersionedScriptDynamic,
  writeMidgardVersionedScriptStatic,
  type MidgardVersionedScript,
} from "./versioned-script.js";

export type { MidgardDatum } from "./datum.js";
export type { MidgardValue } from "./value.js";
export type { MidgardVersionedScript } from "./versioned-script.js";

export type MidgardTxOutput = {
  readonly address: MidgardAddress;
  readonly value: MidgardValue;
  readonly datum?: MidgardDatum;
  readonly script_ref?: MidgardVersionedScript;
};

/**
 * Binary `transaction_output` encoding (mirrors staging midgard-ts adapted to
 * keep MidgardDatum/MidgardVersionedScript wrappers).
 *
 * Layout:
 *   Static:
 *     address_len  (u64)
 *     value.static
 *     datum_present (u64)        + datum_len (u64) if present
 *     script_ref_present (u64)   + versioned_script.static if present
 *   Dynamic:
 *     address bytes + pad
 *     value.dynamic
 *     datum bytes + pad        (if present, opaque Plutus CBOR)
 *     versioned_script.dynamic (if present, opaque inner script bytes)
 */

type OutputPartial = {
  readonly addrLen: number;
  readonly valuePartial: ReturnType<typeof readMidgardValueStatic>;
  readonly datumLen: number | undefined;
  readonly scriptRefPartial:
    | ReturnType<typeof readMidgardVersionedScriptStatic>
    | undefined;
};

export const writeMidgardTxOutputStatic = (
  w: BinaryWriter,
  output: MidgardTxOutput,
): void => {
  const address = encodeMidgardAddressBytes(output.address);
  writeVarBytesStatic(w, address);
  writeMidgardValueStatic(w, output.value);
  if (output.datum !== undefined) {
    writeBool(w, true);
    writeVarBytesStatic(w, encodeMidgardDatum(output.datum));
  } else {
    writeBool(w, false);
  }
  if (output.script_ref !== undefined) {
    writeBool(w, true);
    writeMidgardVersionedScriptStatic(w, output.script_ref);
  } else {
    writeBool(w, false);
  }
};

export const writeMidgardTxOutputDynamic = (
  w: BinaryWriter,
  output: MidgardTxOutput,
): void => {
  const address = encodeMidgardAddressBytes(output.address);
  writeVarBytesDynamic(w, address);
  writeMidgardValueDynamic(w, output.value);
  if (output.datum !== undefined) {
    writeVarBytesDynamic(w, encodeMidgardDatum(output.datum));
  }
  if (output.script_ref !== undefined) {
    writeMidgardVersionedScriptDynamic(w, output.script_ref);
  }
};

export const readMidgardTxOutputStatic = (r: BinaryReader): OutputPartial => {
  const addrLen = readVarBytesLen(r);
  const valuePartial = readMidgardValueStatic(r);
  const datumPresent = readBool(r);
  const datumLen = datumPresent ? readVarBytesLen(r) : undefined;
  const scriptRefPresent = readBool(r);
  const scriptRefPartial = scriptRefPresent
    ? readMidgardVersionedScriptStatic(r)
    : undefined;
  return { addrLen, valuePartial, datumLen, scriptRefPartial };
};

export const readMidgardTxOutputDynamic = (
  r: BinaryReader,
  partial: OutputPartial,
): MidgardTxOutput => {
  const addressBytes = readVarBytesDynamic(r, partial.addrLen);
  const address = encodeMidgardAddressBytes(addressBytes);
  const value = readMidgardValueDynamic(r, partial.valuePartial);
  const datum =
    partial.datumLen === undefined
      ? undefined
      : decodeMidgardDatum(readVarBytesDynamic(r, partial.datumLen));
  const scriptRef =
    partial.scriptRefPartial === undefined
      ? undefined
      : readMidgardVersionedScriptDynamic(r, partial.scriptRefPartial);
  return {
    address,
    value,
    ...(datum === undefined ? {} : { datum }),
    ...(scriptRef === undefined ? {} : { script_ref: scriptRef }),
  };
};

export const encodeMidgardTxOutput = (output: MidgardTxOutput): Buffer => {
  const sw = new BinaryWriter();
  writeMidgardTxOutputStatic(sw, output);
  const dw = new BinaryWriter();
  writeMidgardTxOutputDynamic(dw, output);
  return Buffer.concat([sw.toBytes(), dw.toBytes()]);
};

export const decodeMidgardTxOutput = (bytes: Uint8Array): MidgardTxOutput => {
  const r = new BinaryReader(bytes);
  const partial = readMidgardTxOutputStatic(r);
  const output = readMidgardTxOutputDynamic(r, partial);
  r.expectEnd("transaction_output");
  return output;
};

// ---------------------------------------------------------------------------
// Vec<MidgardTxOutput> — len (u64) + n × output_static, then n × output_dynamic
// ---------------------------------------------------------------------------

export const writeMidgardTxOutputListStatic = (
  w: BinaryWriter,
  outputs: readonly MidgardTxOutput[],
): void => {
  // length first
  const lengthBuf = Buffer.alloc(8);
  lengthBuf.writeBigUInt64BE(BigInt(outputs.length), 0);
  w.write(lengthBuf);
  for (const o of outputs) writeMidgardTxOutputStatic(w, o);
};

export const writeMidgardTxOutputListDynamic = (
  w: BinaryWriter,
  outputs: readonly MidgardTxOutput[],
): void => {
  for (const o of outputs) writeMidgardTxOutputDynamic(w, o);
};

export const readMidgardTxOutputListStatic = (
  r: BinaryReader,
): OutputPartial[] => {
  const lenBuf = r.read(8);
  const len = Number(lenBuf.readBigUInt64BE(0));
  const partials: OutputPartial[] = [];
  for (let i = 0; i < len; i += 1) {
    partials.push(readMidgardTxOutputStatic(r));
  }
  return partials;
};

export const readMidgardTxOutputListDynamic = (
  r: BinaryReader,
  partials: readonly OutputPartial[],
): MidgardTxOutput[] => {
  const out: MidgardTxOutput[] = [];
  for (const p of partials) {
    out.push(readMidgardTxOutputDynamic(r, p));
  }
  return out;
};

export const encodeMidgardTxOutputList = (
  outputs: readonly MidgardTxOutput[],
): Buffer => {
  const sw = new BinaryWriter();
  writeMidgardTxOutputListStatic(sw, outputs);
  const dw = new BinaryWriter();
  writeMidgardTxOutputListDynamic(dw, outputs);
  return Buffer.concat([sw.toBytes(), dw.toBytes()]);
};

export const decodeMidgardTxOutputList = (
  bytes: Uint8Array,
  fieldName = "transaction_output_list",
): MidgardTxOutput[] => {
  const r = new BinaryReader(bytes);
  const partials = readMidgardTxOutputListStatic(r);
  const outs = readMidgardTxOutputListDynamic(r, partials);
  r.expectEnd(fieldName);
  return outs;
};
