import { createHash, createHmac, timingSafeEqual } from "node:crypto";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import type {
  StoredHead,
  StoredRow,
} from "./watcher-journal-database.types.js";

const MAC_DOMAIN = "midgard-watcher-journal-row-v1";
const ACCUMULATOR_DOMAIN = "midgard-watcher-journal-accumulator-v1";

export const ZERO_DIGEST = "00".repeat(32);
export const MODULUS = 1n << 256n;

export const sha256Hex = (text: string): string =>
  createHash("sha256").update(text).digest("hex");

/** Constant-time equality of two 32-byte hex digests. */
export const sameHex = (left: string, right: string): boolean => {
  if (left.length !== 64 || right.length !== 64) return false;
  const a = Buffer.from(left, "hex");
  const b = Buffer.from(right, "hex");
  return a.byteLength === 32 && b.byteLength === 32 && timingSafeEqual(a, b);
};

export type WatcherJournalCodec = Readonly<{
  /** Names the row MAC key, so a head written under another key is refused. */
  keyId: string;
  rowMac(journal: string, row: Omit<StoredRow, "mac">): string;
  headMac(journal: string, head: StoredHead): string;
  revisionMac(
    journal: string,
    revision: number,
    chain: string,
    delta: string,
  ): string;
  chainOf(
    journal: string,
    revision: number,
    prior: string,
    delta: string,
  ): string;
  /** The keyed accumulator term of one row MAC; the head sums them mod 2^256. */
  element(rowMac: string): bigint;
}>;

/**
 * The journals' MACs and digests under one 32-byte key. Two subkeys keep the
 * row MACs and the accumulator terms apart: without the accumulator key a
 * writer cannot compute the term a forged deletion or addition must cancel.
 */
export const createWatcherJournalCodec = (
  authenticationKey: Uint8Array,
): WatcherJournalCodec => {
  if (authenticationKey.byteLength !== 32)
    throw new Error("watcher journal authentication key is invalid");
  const macKey = createHmac("sha256", authenticationKey)
    .update(MAC_DOMAIN)
    .digest();
  const accumulatorKey = createHmac("sha256", authenticationKey)
    .update(ACCUMULATOR_DOMAIN)
    .digest();
  const keyId = createHash("sha256").update(macKey).digest("hex");
  const mac = (fields: readonly string[]): string =>
    createHmac("sha256", macKey)
      .update(watcherCanonicalJson(fields))
      .digest("hex");
  return Object.freeze({
    keyId,
    rowMac: (journal, row) =>
      mac([
        "row",
        journal,
        row.row_key,
        row.scope,
        row.state,
        row.revision.toString(),
        row.body,
      ]),
    headMac: (journal, head) =>
      mac([
        "head",
        journal,
        head.revision.toString(),
        head.chain,
        head.liveRows.toString(),
        head.accumulator.toString(16).padStart(64, "0"),
        keyId,
      ]),
    revisionMac: (journal, revision, chain, delta) =>
      mac(["revision", journal, revision.toString(), chain, delta]),
    chainOf: (journal, revision, prior, delta) =>
      sha256Hex(
        watcherCanonicalJson([
          "chain",
          journal,
          revision.toString(),
          prior,
          sha256Hex(delta),
        ]),
      ),
    element: (rowMac) =>
      BigInt(
        `0x${createHmac("sha256", accumulatorKey).update(rowMac).digest("hex")}`,
      ),
  });
};
