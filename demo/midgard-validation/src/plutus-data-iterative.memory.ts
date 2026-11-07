import {
  midgardCekBytesMemoryWords,
  midgardCekIntegerMemoryWords,
} from "@al-ft/midgard-core";

import { isPlutusDataMap } from "./plutus-data-narrowing.js";
import { type Data, DataB, DataConstr, DataI, DataList } from "./uplc-data.js";

/**
 * Plutus' ExMemory size for an integer: 64-bit words of the magnitude, as the
 * cardano-node CEK cost model counts them.
 */
export const midgardCekIntegerMemorySize = (value: bigint): bigint =>
  midgardCekIntegerMemoryWords(value);

export const midgardCekByteStringMemorySize = (value: Uint8Array): bigint =>
  midgardCekBytesMemoryWords(value.length);

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
      total += 4n + midgardCekByteStringMemorySize(next.bytes);
    } else {
      throw new Error("V1 data constant has an unknown node");
    }
  }
  return total;
};
