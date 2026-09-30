import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
} from "@harmoniclabs/plutus-data";

import { asByteArray } from "./cek-constant.semantic-data.js";
import { isPlutusDataMap } from "./plutus-data-narrowing.js";

/**
 * Plutus' ExMemory size for an integer. This is the signed CBOR-style byte
 * magnitude used by cardano-node's CEK cost model, not the encoded payload
 * length.
 */
export const midgardCekIntegerMemorySize = (value: bigint): bigint => {
  const doubledMagnitude = value < 0n ? (-value - 1n) << 1n : value << 1n;
  if (doubledMagnitude === 0n) {
    return 1n;
  }
  return BigInt(Math.floor((doubledMagnitude.toString(2).length - 1) / 8) + 1);
};

export const midgardCekByteStringMemorySize = (value: Uint8Array): bigint =>
  BigInt(Math.max(1, value.length));

/**
 * Plutus Data charges four memory words for every node, then the signed
 * integer or byte-string size for leaf payloads. The walk keeps its own stack,
 * so any nesting depth is sized.
 */
export const midgardCekDataMemorySize = (value: Data): bigint => {
  let total = 0n;
  const work: Data[] = [value];
  while (work.length > 0) {
    const next = work.pop()!;
    if (next instanceof DataConstr) {
      total += 4n;
      for (let index = next.fields.length - 1; index >= 0; index -= 1) {
        work.push(next.fields[index]!);
      }
    } else if (isPlutusDataMap(next)) {
      total += 4n;
      for (let index = next.map.length - 1; index >= 0; index -= 1) {
        const entry = next.map[index]!;
        work.push(entry.snd, entry.fst);
      }
    } else if (next instanceof DataList) {
      total += 4n;
      for (let index = next.list.length - 1; index >= 0; index -= 1) {
        work.push(next.list[index]!);
      }
    } else if (next instanceof DataI) {
      total += 4n + midgardCekIntegerMemorySize(next.int);
    } else if (next instanceof DataB) {
      total += 4n + midgardCekByteStringMemorySize(asByteArray(next.bytes));
    } else {
      throw new Error("V1 data constant has an unknown node");
    }
  }
  return total;
};
