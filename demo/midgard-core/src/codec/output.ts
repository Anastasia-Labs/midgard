import { encodeMidgardAddressBytes, type MidgardAddress } from "./address.js";
import {
  BinaryReader,
  BinaryWriter,
  readU64,
  readU8,
  readVarBytesDynamic,
  readVarBytesLen,
  writeU64,
  writeU8,
  writeVarBytesDynamic,
  writeVarBytesStatic,
} from "./binary.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
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
 * Binary `transaction_output` encoding.
 *
 * Layout:
 *   Static:
 *     address_len   (varuint)
 *     value.static
 *     presence_mask (u8 — bit 0 = datum, bit 1 = script_ref)
 *     datum_len     (varuint)            if datum present
 *     versioned_script.static            if script_ref present
 *   Dynamic:
 *     address bytes
 *     value.dynamic
 *     datum bytes              (if present, opaque Plutus CBOR)
 *     versioned_script.dynamic (if present, opaque inner script bytes)
 */

const PRESENCE_DATUM = 0b01;
const PRESENCE_SCRIPT_REF = 0b10;
const PRESENCE_RESERVED_MASK = ~(PRESENCE_DATUM | PRESENCE_SCRIPT_REF) & 0xff;

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
  const presence =
    (output.datum !== undefined ? PRESENCE_DATUM : 0) |
    (output.script_ref !== undefined ? PRESENCE_SCRIPT_REF : 0);
  writeU8(w, presence);
  if (output.datum !== undefined) {
    writeVarBytesStatic(w, encodeMidgardDatum(output.datum));
  }
  if (output.script_ref !== undefined) {
    writeMidgardVersionedScriptStatic(w, output.script_ref);
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
  const presence = readU8(r);
  if ((presence & PRESENCE_RESERVED_MASK) !== 0) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      "transaction_output: presence mask has reserved bits set",
      presence.toString(2),
    );
  }
  const datumLen =
    (presence & PRESENCE_DATUM) !== 0 ? readVarBytesLen(r) : undefined;
  const scriptRefPartial =
    (presence & PRESENCE_SCRIPT_REF) !== 0
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
  writeU64(w, outputs.length);
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
  const len = readU64(r);
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
