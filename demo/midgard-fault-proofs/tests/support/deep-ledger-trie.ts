/** Predecessor ledgers whose opened keys sit deep in the ledger trie: every
 * opened out-ref is ground into a path cluster, and padding keys leave the
 * cluster at each of its digits, so each opened key's proof steps through a
 * full branch at every shared digit before the cluster's own branches. */
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec";

import type { LedgerEntry } from "../../src/ledger-at.js";

/** Whether a trie path (the Blake2b-256 of the key) starts with the hex
 * digits of `prefix`. */
export const pathStarts = (key: Uint8Array, prefix: string): boolean => {
  const path = computeHash32(key);
  for (let digit = 0; digit < prefix.length; digit += 1) {
    const nibble = (path[digit >> 1]! >> (digit % 2 === 0 ? 4 : 0)) & 0xf;
    if (nibble !== Number.parseInt(prefix[digit]!, 16)) return false;
  }
  return true;
};

/** The first out-ref at `outputIndex` whose path satisfies `accepts`, its
 * transaction id filled with `fill` but for an eight-byte nonce after the
 * id's CBOR head. The search starts at nonce `from`; a recorded first nonce
 * finds the same out-ref without repeating the search. */
export const groundOutRef = (
  outputIndex: number,
  fill: number,
  accepts: (key: Buffer) => boolean,
  from = 0n,
): Buffer => {
  const outRef = encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, fill),
    outputIndex,
  });
  for (let nonce = from; ; nonce += 1n) {
    outRef.writeBigUInt64BE(nonce, 3);
    if (accepts(outRef)) return outRef;
  }
};

const KEY_LOCKED = encodeMidgardTxOutput({
  address: Buffer.from(`60${"44".repeat(28)}`, "hex"),
  value: { lovelace: 5_000_000n, assets: new Map() },
});

/** Key-locked padding entries leaving `cluster` at each of its digits through
 * every other digit, so each branch on a clustered path is full, as in a
 * populated ledger. The search for the entry at `depth` leaving through the
 * `k`-th other digit starts at `from[depth][k]`. */
export const clusterPadding = (
  cluster: string,
  from: readonly (readonly bigint[])[] = [],
): LedgerEntry[] =>
  Array.from({ length: cluster.length }, (_, depth) =>
    [...Array(16).keys()]
      .filter((digit) => digit !== Number.parseInt(cluster[depth]!, 16))
      .map((digit, k) => ({
        outRef: groundOutRef(
          digit,
          0x50 + depth,
          (key) =>
            pathStarts(key, cluster.slice(0, depth) + digit.toString(16)),
          from[depth]?.[k],
        ),
        output: KEY_LOCKED,
      })),
  ).flat();

/** `entries` with each out-ref re-ground into `cluster`, keeping its output
 * index and output, each search starting at `from[index]`. */
export const intoCluster = (
  entries: readonly LedgerEntry[],
  fill: number,
  cluster: string,
  from: readonly bigint[] = [],
): LedgerEntry[] =>
  entries.map((entry, index) => ({
    ...entry,
    outRef: groundOutRef(
      index,
      fill,
      (key) => pathStarts(key, cluster),
      from[index],
    ),
  }));
