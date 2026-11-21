import { CML } from "@lucid-evolution/lucid";

import {
  constrFieldRanges,
  countPlutusDataNodes,
  encodeCborHeader,
  parseCompletePlutusData,
} from "./plutus-data-cbor.parse-array-item-ranges.js";
import {
  type CborNode,
  parseCborNode,
} from "./plutus-data-cbor.parse-cbor-node.js";

export const encodeCborNodeWithDefiniteMaps = (
  node: CborNode,
  sortMaps: boolean,
  chunkByteStrings = true,
  normalizeData = false,
): Buffer => {
  type EncodeVisit = {
    readonly node: CborNode;
    readonly expanded: boolean;
  };
  const encoded = new Map<CborNode, Buffer>();
  const stack: EncodeVisit[] = [{ node, expanded: false }];

  while (stack.length > 0) {
    const visit = stack.pop()!;
    const current = visit.node;
    if (!visit.expanded) {
      stack.push({ node: current, expanded: true });
      if (current.kind === "array") {
        for (let index = current.items.length - 1; index >= 0; index -= 1) {
          stack.push({
            node: current.items[index]!,
            expanded: false,
          });
        }
      } else if (current.kind === "map") {
        for (let index = current.entries.length - 1; index >= 0; index -= 1) {
          const [key, value] = current.entries[index]!;
          stack.push({ node: value, expanded: false });
          stack.push({ node: key, expanded: false });
        }
      } else if (current.kind === "tag") {
        stack.push({ node: current.value, expanded: false });
      }
      continue;
    }

    switch (current.kind) {
      case "uint":
        encoded.set(current, encodeCborHeader(0, current.value));
        break;
      case "nint":
        encoded.set(current, encodeCborHeader(1, current.value));
        break;
      case "bytes": {
        if (!chunkByteStrings || current.value.length <= 64) {
          encoded.set(
            current,
            Buffer.concat([
              encodeCborHeader(2, BigInt(current.value.length)),
              current.value,
            ]),
          );
          break;
        }
        const chunks: Buffer[] = [];
        for (
          let chunkOffset = 0;
          chunkOffset < current.value.length;
          chunkOffset += 64
        ) {
          const chunk = current.value.subarray(chunkOffset, chunkOffset + 64);
          chunks.push(encodeCborHeader(2, BigInt(chunk.length)), chunk);
        }
        encoded.set(
          current,
          Buffer.concat([Buffer.from([0x5f]), ...chunks, Buffer.from([0xff])]),
        );
        break;
      }
      case "array":
        encoded.set(
          current,
          current.items.length === 0
            ? Buffer.from([0x80])
            : Buffer.concat([
                Buffer.from([0x9f]),
                ...current.items.map((item) => encoded.get(item)!),
                Buffer.from([0xff]),
              ]),
        );
        break;
      case "map": {
        const entries = current.entries.map(([key, value]) => ({
          encodedKey: encoded.get(key)!,
          encodedValue: encoded.get(value)!,
        }));
        if (sortMaps) {
          entries.sort((left, right) =>
            Buffer.compare(left.encodedKey, right.encodedKey),
          );
        }
        encoded.set(
          current,
          Buffer.concat([
            encodeCborHeader(5, BigInt(entries.length)),
            ...entries.flatMap(({ encodedKey, encodedValue }) => [
              encodedKey,
              encodedValue,
            ]),
          ]),
        );
        break;
      }
      case "tag":
        if (
          normalizeData &&
          (current.tag === 2n || current.tag === 3n) &&
          current.value.kind === "bytes"
        ) {
          let first = 0;
          while (
            first < current.value.value.length &&
            current.value.value[first] === 0
          )
            first++;
          const magnitude = current.value.value.subarray(first);
          encoded.set(
            current,
            magnitude.length <= 8
              ? encodeCborHeader(
                  current.tag === 2n ? 0 : 1,
                  magnitude.length === 0
                    ? 0n
                    : BigInt(`0x${magnitude.toString("hex")}`),
                )
              : Buffer.concat([
                  encodeCborHeader(6, current.tag),
                  encodeCborNodeWithDefiniteMaps(
                    { kind: "bytes", value: magnitude },
                    sortMaps,
                    chunkByteStrings,
                  ),
                ]),
          );
          break;
        }
        if (
          normalizeData &&
          current.tag === 102n &&
          current.value.kind === "array"
        ) {
          const index = current.value.items[0]!;
          const fields = current.value.items[1]!;
          if (index.kind !== "uint" || fields.kind !== "array")
            throw new Error("Invalid general PlutusData constructor");
          if (index.value < 128n) {
            const tag =
              index.value < 7n ? 121n + index.value : 1280n + index.value - 7n;
            encoded.set(
              current,
              Buffer.concat([encodeCborHeader(6, tag), encoded.get(fields)!]),
            );
            break;
          }
        }
        encoded.set(
          current,
          current.tag === 102n &&
            current.value.kind === "array" &&
            current.value.items.length === 2
            ? Buffer.concat([
                encodeCborHeader(6, current.tag),
                Buffer.from([0x82]),
                ...current.value.items.map((item) => encoded.get(item)!),
              ])
            : Buffer.concat([
                encodeCborHeader(6, current.tag),
                encoded.get(current.value)!,
              ]),
        );
        break;
    }
  }

  return encoded.get(node)!;
};

/**
 * Mirrors `cbor.serialise(builtin.b_data(bytes))`. Plutus Data byte strings
 * longer than 64 bytes use an indefinite byte string containing definite
 * chunks of at most 64 bytes.
 */
export const aikenSerialisedPlutusDataBytes = (bytes: Uint8Array): Buffer =>
  encodeCborNodeWithDefiniteMaps(
    { kind: "bytes", value: Buffer.from(bytes) },
    true,
  );

const aikenSerialisedPlutusDataCborWithMapOrder = (
  cbor: string,
  sortMaps: boolean,
): string => {
  const node = parseCompletePlutusData(cbor);
  countPlutusDataNodes(node);
  return encodeCborNodeWithDefiniteMaps(node, sortMaps, true, true).toString(
    "hex",
  );
};

export const aikenSerialisedPlutusDataCbor = (cbor: string): string =>
  aikenSerialisedPlutusDataCborWithMapOrder(cbor, true);

/**
 * Mirrors `serialiseData` for an already constructed Data value. Unlike
 * typed SDK encoders, a raw Plutus Data map retains its explicit pair order,
 * and that order is observable through both `unMapData` and serialization.
 */
export const aikenSerialisedPlutusDataCborPreservingMapOrder = (
  cbor: string,
): string => aikenSerialisedPlutusDataCborWithMapOrder(cbor, false);

export const plutusConstrFieldCbor = (
  cbor: string,
  fieldPath: readonly number[],
): string => {
  let current = Buffer.from(cbor, "hex");
  for (const index of fieldPath) {
    const fields = constrFieldRanges(current);
    const field = fields.items[index];
    if (field === undefined) {
      throw new Error(`Constructor field ${index.toString()} is missing`);
    }
    current = current.subarray(field.start, field.end);
  }
  return current.toString("hex");
};

export const aikenSerialisedPlutusConstrFieldCbor = (
  cbor: string,
  fieldPath: readonly number[],
): string =>
  aikenSerialisedPlutusDataCbor(plutusConstrFieldCbor(cbor, fieldPath));

/** Replace one constructor field without decoding arbitrary Data through a JS
 * Map. Parent encodings and all other fields stay byte-for-byte unchanged;
 * repeated map keys and pair order in the replacement remain observable. */
export const replacePlutusConstrFieldCbor = (
  cbor: string,
  fieldPath: readonly number[],
  replacementCbor: string,
): string => {
  const checked = (value: string) => {
    if (!/^(?:[0-9a-fA-F]{2})+$/u.test(value))
      throw new Error("Expected complete PlutusData CBOR hex");
    const bytes = Buffer.from(value, "hex");
    if (parseCborNode(bytes, 0).offset !== bytes.length)
      throw new Error("Unexpected trailing bytes in PlutusData CBOR");
    const decoded = CML.PlutusData.from_cbor_hex(value);
    decoded.free();
    return bytes;
  };
  const original = checked(cbor);
  const replacement = checked(replacementCbor);
  let current = original;
  let offset = 0;
  for (const index of fieldPath) {
    if (!Number.isSafeInteger(index) || index < 0)
      throw new Error("Constructor field index must be a safe natural number");
    const field = constrFieldRanges(current).items[index];
    if (field === undefined)
      throw new Error(`Constructor field ${index.toString()} is missing`);
    offset += field.start;
    current = current.subarray(field.start, field.end);
  }
  return Buffer.concat([
    original.subarray(0, offset),
    replacement,
    original.subarray(offset + current.length),
  ]).toString("hex");
};

export const canonicalPlutusDataCbor = (cbor: string): string =>
  CML.PlutusData.from_cbor_hex(cbor).to_canonical_cbor_hex();

export const MIDGARD_PLUTUS_DATA_MAX_BYTES_CHUNK = 64;

export const isMidgardPlutusDataConstrTag = (tag: bigint): boolean =>
  (tag >= 121n && tag <= 127n) || (tag >= 1_280n && tag <= 1_400n);

export type MidgardPlutusDataHead = {
  readonly major: number;
  readonly value: bigint | null;
  readonly offset: number;
};
