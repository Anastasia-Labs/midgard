/** Path grinding for deep-trie max-shape subjects of the
 * `unusedScriptWitness` tests: out-refs and native scripts whose trie paths
 * follow or leave a chosen cluster, and a bisection for field caps. */
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  hashMidgardVersionedScript,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  computeHash28,
  computeHash32,
  encodeMidgardNativeScript,
} from "@al-ft/midgard-core/codec";

import { nativeScript } from "./script-integrity-hash-mismatch-emulator.subject.js";

/** The largest value in [low, high) for which `fits` holds (`fits(low)`
 * must). */
export const largest = (
  low: number,
  high: number,
  fits: (value: number) => boolean,
): number => {
  let [ok, bad] = [low, high];
  while (bad - ok > 1) {
    const mid = Math.floor((ok + bad) / 2);
    if (fits(mid)) ok = mid;
    else bad = mid;
  }
  return ok;
};

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

/** A path that follows `prefix` for exactly `depth` digits, then leaves it. */
export const leavesAt = (key: Uint8Array, prefix: string, depth: number) =>
  pathStarts(key, prefix.slice(0, depth)) &&
  !pathStarts(key, prefix.slice(0, depth + 1));

/** Digits every clustered key's path shares; one padding key leaves the
 * cluster at each of them, so every clustered key's proof steps through a
 * branch at each shared digit before the cluster's own branches. */
export const CLUSTER_DIGITS = 3;
export const LEDGER_CLUSTER = "a5c";

export const KEY_LOCKED = encodeMidgardTxOutput({
  address: Buffer.from(`60${"44".repeat(28)}`, "hex"),
  value: { lovelace: 5_000_000n, assets: new Map() },
});

/** The first out-ref at `outputIndex`, varying its transaction id, whose path
 * satisfies `accepts`. */
export const groundOutRef = (
  outputIndex: number,
  fill: number,
  accepts: (key: Buffer) => boolean,
): Buffer => {
  const outRef = encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, fill),
    outputIndex,
  });
  for (let nonce = 0; ; nonce += 1) {
    outRef.writeUInt32BE(nonce, 3);
    if (accepts(outRef)) return outRef;
  }
};

/** Native `after` scripts with a four-byte slot, in slot order from `from`,
 * whose hashes' paths satisfy `accepts`. */
export const groundNatives = (
  count: number,
  from: number,
  accepts: (hash: Buffer) => boolean,
): { readonly scripts: MidgardVersionedScript[]; readonly next: number } => {
  const preimage = Buffer.concat([
    Buffer.from([0x00]),
    encodeMidgardNativeScript({ type: "after", slot: 1n << 24n }),
  ]);
  const slotAt = preimage.length - 4;
  const scripts: MidgardVersionedScript[] = [];
  let slot = from;
  for (; scripts.length < count; slot += 1) {
    preimage.writeUInt32BE(slot, slotAt);
    const hash = computeHash28(preimage);
    if (!accepts(hash)) continue;
    const script = nativeScript(BigInt(slot));
    if (hashMidgardVersionedScript(script) !== hash.toString("hex"))
      throw new Error("native script preimage differs");
    scripts.push(script);
  }
  return { scripts, next: slot };
};
