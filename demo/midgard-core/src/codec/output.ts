import {
  compareCborKeyBytes,
  encodeCborBytes,
  encodeCborMapRaw,
  encodeCborUnsigned,
  readCborBytes,
  readCborMapHeader,
  readCborUnsigned,
  skipCborItem,
} from "./cbor.js";
import {
  decodeMidgardAddressBytes,
  encodeMidgardAddressBytes,
  type MidgardAddress,
} from "./address.js";
import {
  decodeMidgardDatum,
  encodeMidgardDatum,
  type MidgardDatum,
} from "./datum.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import {
  decodeMidgardValue,
  encodeMidgardValue,
  type MidgardValue,
} from "./value.js";
import {
  decodeMidgardVersionedScript,
  encodeMidgardVersionedScript,
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

const fail = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    message,
    detail,
  );
};

const readRawValue = (
  bytes: Uint8Array,
  offset: number,
): { readonly raw: Buffer; readonly nextOffset: number } => {
  const span = skipCborItem(bytes, offset);
  return {
    raw: Buffer.from(bytes.subarray(span.start, span.end)),
    nextOffset: span.end,
  };
};

export const encodeMidgardTxOutput = (output: MidgardTxOutput): Buffer => {
  const address = encodeMidgardAddressBytes(output.address);
  const entries: (readonly [Buffer, Buffer])[] = [
    [encodeCborUnsigned(0n), encodeCborBytes(address)],
    [encodeCborUnsigned(1n), encodeMidgardValue(output.value)],
  ];
  if (output.datum !== undefined) {
    entries.push([
      encodeCborUnsigned(2n),
      encodeCborBytes(encodeMidgardDatum(output.datum)),
    ]);
  }
  if (output.script_ref !== undefined) {
    entries.push([
      encodeCborUnsigned(3n),
      encodeMidgardVersionedScript(output.script_ref),
    ]);
  }
  entries.sort(([left], [right]) => compareCborKeyBytes(left, right));
  return encodeCborMapRaw(entries);
};

export const decodeMidgardTxOutput = (bytes: Uint8Array): MidgardTxOutput => {
  const header = readCborMapHeader(bytes, 0, "output");
  let cursor = header.nextOffset;
  let address: Buffer | undefined;
  let value: MidgardValue | undefined;
  let datum: MidgardDatum | undefined;
  let scriptRef: MidgardVersionedScript | undefined;
  let previousKey: bigint | undefined;

  for (let i = 0; i < header.length; i += 1) {
    const key = readCborUnsigned(bytes, cursor, "output.key");
    cursor = key.nextOffset;
    if (previousKey !== undefined && key.value <= previousKey) {
      fail("Output map keys must be unique and sorted", key.value.toString());
    }
    previousKey = key.value;
    switch (key.value) {
      case 0n: {
        if (address !== undefined) {
          fail("Duplicate output address key");
        }
        const decodedAddress = readCborBytes(bytes, cursor, "output.address");
        cursor = decodedAddress.nextOffset;
        address = encodeMidgardAddressBytes(decodedAddress.value);
        break;
      }
      case 1n: {
        if (value !== undefined) {
          fail("Duplicate output value key");
        }
        const raw = readRawValue(bytes, cursor);
        cursor = raw.nextOffset;
        value = decodeMidgardValue(raw.raw);
        break;
      }
      case 2n: {
        if (datum !== undefined) {
          fail("Duplicate output datum key");
        }
        const rawDatum = readCborBytes(bytes, cursor, "output.datum");
        cursor = rawDatum.nextOffset;
        datum = decodeMidgardDatum(rawDatum.value);
        break;
      }
      case 3n: {
        if (scriptRef !== undefined) {
          fail("Duplicate output script_ref key");
        }
        const raw = readRawValue(bytes, cursor);
        cursor = raw.nextOffset;
        scriptRef = decodeMidgardVersionedScript(raw.raw);
        break;
      }
      default:
        fail("Unknown Midgard output field", key.value.toString());
    }
  }

  if (cursor !== bytes.length) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      "Trailing bytes after Midgard output",
      `offset=${cursor}`,
    );
  }
  if (address === undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.MissingRequiredField,
      "Midgard output missing address key 0",
    );
  }
  if (value === undefined) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.MissingRequiredField,
      "Midgard output missing value key 1",
    );
  }

  const output: MidgardTxOutput = {
    address,
    value,
    ...(datum === undefined ? {} : { datum }),
    ...(scriptRef === undefined ? {} : { script_ref: scriptRef }),
  };
  const encoded = encodeMidgardTxOutput(output);
  if (!encoded.equals(Buffer.from(bytes))) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      "Midgard output CBOR is not canonical",
    );
  }
  return output;
};
