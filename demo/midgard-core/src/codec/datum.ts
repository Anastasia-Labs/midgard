import {
  compareCborKeyBytes,
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborInteger,
  encodeCborMapRaw,
  encodeCborTagRaw,
  encodeCborUnsigned,
  readCborArrayHeader,
  readCborBytes,
  readCborInteger,
  readCborMapHeader,
  readCborTag,
  readCborUnsigned,
  skipCborItem,
} from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";

export type MidgardDatum = {
  readonly kind: "inline";
  readonly cbor: Buffer;
};

type PlutusData =
  | { readonly kind: "integer"; readonly value: bigint }
  | { readonly kind: "bytes"; readonly value: Buffer }
  | { readonly kind: "list"; readonly items: readonly PlutusData[] }
  | { readonly kind: "map"; readonly entries: readonly (readonly [PlutusData, PlutusData])[] }
  | { readonly kind: "constructor"; readonly alternative: bigint; readonly fields: readonly PlutusData[] };

const fail = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    message,
    detail,
  );
};

const constrTagToAlternative = (tag: bigint): bigint | undefined => {
  if (tag >= 121n && tag <= 127n) {
    return tag - 121n;
  }
  if (tag >= 1280n && tag <= 1400n) {
    return tag - 1280n + 7n;
  }
  if (tag === 102n) {
    return undefined;
  }
  return fail("Invalid PlutusData constructor tag", tag.toString());
};

const encodePlutusData = (data: PlutusData): Buffer => {
  switch (data.kind) {
    case "integer":
      return encodeCborInteger(data.value);
    case "bytes":
      return encodeCborBytes(data.value);
    case "list":
      return encodeCborArrayRaw(data.items.map(encodePlutusData));
    case "map": {
      const entries = data.entries.map(([key, value]) => [
        encodePlutusData(key),
        encodePlutusData(value),
      ] as const);
      entries.sort(([left], [right]) => compareCborKeyBytes(left, right));
      return encodeCborMapRaw(entries);
    }
    case "constructor": {
      const fields = encodeCborArrayRaw(data.fields.map(encodePlutusData));
      const alt = data.alternative;
      if (alt >= 0n && alt <= 6n) {
        return encodeCborTagRaw(121n + alt, fields);
      }
      if (alt >= 7n && alt <= 127n) {
        return encodeCborTagRaw(1280n + alt - 7n, fields);
      }
      return encodeCborTagRaw(
        102n,
        encodeCborArrayRaw([encodeCborUnsigned(alt), fields]),
      );
    }
  }
};

const decodePlutusDataAt = (
  bytes: Uint8Array,
  offset: number,
): { readonly data: PlutusData; readonly nextOffset: number } => {
  if (offset >= bytes.length) {
    fail("Unexpected end of PlutusData CBOR", `offset=${offset}`);
  }
  const major = bytes[offset] >> 5;
  switch (major) {
    case 0:
    case 1: {
      const int = readCborInteger(bytes, offset, "datum.integer");
      return { data: { kind: "integer", value: int.value }, nextOffset: int.nextOffset };
    }
    case 2: {
      const value = readCborBytes(bytes, offset, "datum.bytes");
      return { data: { kind: "bytes", value: value.value }, nextOffset: value.nextOffset };
    }
    case 4: {
      const header = readCborArrayHeader(bytes, offset, "datum.list");
      let cursor = header.nextOffset;
      const items: PlutusData[] = [];
      for (let i = 0; i < header.length; i += 1) {
        const item = decodePlutusDataAt(bytes, cursor);
        items.push(item.data);
        cursor = item.nextOffset;
      }
      return { data: { kind: "list", items }, nextOffset: cursor };
    }
    case 5: {
      const header = readCborMapHeader(bytes, offset, "datum.map");
      let cursor = header.nextOffset;
      const entries: (readonly [PlutusData, PlutusData])[] = [];
      const seen = new Set<string>();
      let previousKey: Buffer | undefined;
      for (let i = 0; i < header.length; i += 1) {
        const keyStart = cursor;
        const key = decodePlutusDataAt(bytes, cursor);
        const keyBytes = Buffer.from(bytes.subarray(keyStart, key.nextOffset));
        if (seen.has(keyBytes.toString("hex"))) {
          fail("Duplicate PlutusData map key", `index=${i}`);
        }
        seen.add(keyBytes.toString("hex"));
        if (
          previousKey !== undefined &&
          compareCborKeyBytes(previousKey, keyBytes) > 0
        ) {
          fail("Non-canonical PlutusData map key ordering", `index=${i}`);
        }
        previousKey = keyBytes;
        const value = decodePlutusDataAt(bytes, key.nextOffset);
        entries.push([key.data, value.data]);
        cursor = value.nextOffset;
      }
      return { data: { kind: "map", entries }, nextOffset: cursor };
    }
    case 6: {
      const tag = readCborTag(bytes, offset, "datum.constructor_tag");
      const alternative = constrTagToAlternative(tag.value);
      if (alternative === undefined) {
        const general = readCborArrayHeader(bytes, tag.nextOffset, "datum.constructor");
        if (general.length !== 2) {
          fail("General PlutusData constructor must be [alternative, fields]");
        }
        const alt = readCborUnsigned(bytes, general.nextOffset, "datum.constructor.alternative");
        const fieldsHeader = readCborArrayHeader(bytes, alt.nextOffset, "datum.constructor.fields");
        let cursor = fieldsHeader.nextOffset;
        const fields: PlutusData[] = [];
        for (let i = 0; i < fieldsHeader.length; i += 1) {
          const field = decodePlutusDataAt(bytes, cursor);
          fields.push(field.data);
          cursor = field.nextOffset;
        }
        return {
          data: {
            kind: "constructor",
            alternative: alt.value,
            fields,
          },
          nextOffset: cursor,
        };
      }
      const fieldsHeader = readCborArrayHeader(bytes, tag.nextOffset, "datum.constructor.fields");
      let cursor = fieldsHeader.nextOffset;
      const fields: PlutusData[] = [];
      for (let i = 0; i < fieldsHeader.length; i += 1) {
        const field = decodePlutusDataAt(bytes, cursor);
        fields.push(field.data);
        cursor = field.nextOffset;
      }
      return {
        data: { kind: "constructor", alternative, fields },
        nextOffset: cursor,
      };
    }
    default:
      skipCborItem(bytes, offset, { allowTags: true });
      return fail("Unsupported PlutusData CBOR major type", `offset=${offset}`);
  }
};

export const decodeMidgardDatum = (bytes: Uint8Array): MidgardDatum => {
  const decoded = decodePlutusDataAt(bytes, 0);
  if (decoded.nextOffset !== bytes.length) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      "Trailing bytes after PlutusData",
      `offset=${decoded.nextOffset}`,
    );
  }
  const encoded = encodePlutusData(decoded.data);
  if (!encoded.equals(Buffer.from(bytes))) {
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      "PlutusData datum is not canonical",
    );
  }
  return { kind: "inline", cbor: encoded };
};

export const encodeMidgardDatum = (datum: MidgardDatum): Buffer =>
  decodeMidgardDatum(datum.cbor).cbor;
