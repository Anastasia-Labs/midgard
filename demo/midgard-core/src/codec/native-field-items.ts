import {
  buildMidgardBoundedCollectionV1,
  type MidgardBoundedCollectionV1,
} from "../bounded-collection-v1.js";
import {
  decodeSingleCbor,
  encodeCbor,
  encodeCborArrayRaw,
  encodeCborMapRaw,
  readCborArrayHeader,
  readCborMapHeader,
  skipCborItem,
} from "./cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";

export const MIDGARD_NATIVE_DYNAMIC_FIELD_COUNT_V1 = 9 as const;

const BYTE_LIST_FIELDS = new Set([0, 1, 2, 3, 4, 7]);
const RAW_ITEM_LIST_FIELDS = new Set([6, 8]);

const fail = (message: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    message,
  );
};

const exactFieldIndex = (fieldIndex: number): number => {
  if (
    !Number.isSafeInteger(fieldIndex) ||
    fieldIndex < 0 ||
    fieldIndex >= MIDGARD_NATIVE_DYNAMIC_FIELD_COUNT_V1
  ) {
    return fail(`unknown native-V1 dynamic field index ${fieldIndex}`);
  }
  return fieldIndex;
};

const rawArrayItems = (
  preimageCbor: Uint8Array,
  fieldName: string,
): readonly Buffer[] => {
  decodeSingleCbor(preimageCbor);
  const header = readCborArrayHeader(preimageCbor, 0, fieldName);
  const items: Buffer[] = [];
  let cursor = header.nextOffset;
  for (let index = 0; index < header.length; index += 1) {
    const span = skipCborItem(preimageCbor, cursor);
    items.push(Buffer.from(preimageCbor.subarray(span.start, span.end)));
    cursor = span.end;
  }
  if (cursor !== preimageCbor.length) {
    return fail(`${fieldName} has trailing bytes`);
  }
  return items;
};

const byteListItems = (
  preimageCbor: Uint8Array,
  fieldName: string,
): readonly Buffer[] =>
  rawArrayItems(preimageCbor, fieldName).map((rawItem, index) => {
    const decoded = decodeSingleCbor(rawItem);
    if (!(decoded instanceof Uint8Array)) {
      return fail(`${fieldName}[${index}] must be a CBOR byte string`);
    }
    return Buffer.from(decoded);
  });

const mintPolicyItems = (preimageCbor: Uint8Array): readonly Buffer[] => {
  const decoded = decodeSingleCbor(preimageCbor);
  if (Array.isArray(decoded)) {
    if (decoded.length !== 0) {
      return fail("native-V1 mint array sentinel must be empty");
    }
    return [];
  }
  if (!(decoded instanceof Map) || decoded.size === 0) {
    return fail("native-V1 mint must be an empty array or non-empty map");
  }
  const header = readCborMapHeader(preimageCbor, 0, "native.mint");
  const items: Buffer[] = [];
  let cursor = header.nextOffset;
  for (let index = 0; index < header.length; index += 1) {
    const key = skipCborItem(preimageCbor, cursor);
    const value = skipCborItem(preimageCbor, key.end);
    items.push(
      encodeCborArrayRaw([
        preimageCbor.subarray(key.start, key.end),
        preimageCbor.subarray(value.start, value.end),
      ]),
    );
    cursor = value.end;
  }
  if (cursor !== preimageCbor.length) {
    return fail("native.mint has trailing bytes");
  }
  return items;
};

/**
 * Produces the exact semantic item content committed by a native-V1 dynamic
 * field. Byte-list wrappers are removed; raw script/redeemer items are kept;
 * each mint policy is encoded as one canonical [policy, assets] item.
 */
export const deriveMidgardNativeFieldItemBytesV1 = ({
  fieldIndex,
  preimageCbor,
}: {
  readonly fieldIndex: number;
  readonly preimageCbor: Uint8Array;
}): readonly Buffer[] => {
  const exactField = exactFieldIndex(fieldIndex);
  if (BYTE_LIST_FIELDS.has(exactField)) {
    return byteListItems(preimageCbor, `native.field[${exactField}]`);
  }
  if (RAW_ITEM_LIST_FIELDS.has(exactField)) {
    return rawArrayItems(preimageCbor, `native.field[${exactField}]`);
  }
  return mintPolicyItems(preimageCbor);
};

export const deriveMidgardNativeFieldCollectionV1 = ({
  fieldIndex,
  preimageCbor,
}: {
  readonly fieldIndex: number;
  readonly preimageCbor: Uint8Array;
}): MidgardBoundedCollectionV1 =>
  buildMidgardBoundedCollectionV1({
    fieldIndex: exactFieldIndex(fieldIndex),
    items: deriveMidgardNativeFieldItemBytesV1({
      fieldIndex,
      preimageCbor,
    }),
  });

export const reconstructMidgardNativeFieldPreimageV1 = ({
  fieldIndex,
  items,
}: {
  readonly fieldIndex: number;
  readonly items: readonly Uint8Array[];
}): Buffer => {
  const exactField = exactFieldIndex(fieldIndex);
  const exactItems = items.map((item) => Buffer.from(item));
  if (BYTE_LIST_FIELDS.has(exactField)) {
    return encodeCbor(exactItems);
  }
  if (RAW_ITEM_LIST_FIELDS.has(exactField)) {
    return encodeCborArrayRaw(exactItems);
  }
  if (exactItems.length === 0) {
    return encodeCbor([]);
  }
  const entries = exactItems.map((item, itemIndex) => {
    decodeSingleCbor(item);
    const header = readCborArrayHeader(
      item,
      0,
      `native.mint.policy[${itemIndex}]`,
    );
    if (header.length !== 2) {
      return fail("native-V1 mint policy item must be [policy, assets]");
    }
    const key = skipCborItem(item, header.nextOffset);
    const value = skipCborItem(item, key.end);
    if (value.end !== item.length) {
      return fail("native-V1 mint policy item has trailing bytes");
    }
    return [
      item.subarray(key.start, key.end),
      item.subarray(value.start, value.end),
    ] as const;
  });
  return encodeCborMapRaw(entries);
};
