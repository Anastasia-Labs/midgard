import { createHash, createHmac, timingSafeEqual } from "node:crypto";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import type { StoredRow } from "./watcher-journal-database.types.js";

const MAC_DOMAIN = "midgard-watcher-journal-row-v1";

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
  /** Names the row MAC key, so journals written under another key are told
   * from corruption. */
  keyId: string;
  rowMac(journal: string, row: Omit<StoredRow, "mac">): string;
}>;

/**
 * The rows' MAC under a 32-byte key. SQLite checksums its WAL frames but not
 * the records in its pages, so a bit the disk flips inside a row would be
 * read back as valid data; the MAC refuses it instead.
 */
export const createWatcherJournalCodec = (
  authenticationKey: Uint8Array,
): WatcherJournalCodec => {
  if (authenticationKey.byteLength !== 32)
    throw new Error("watcher journal authentication key is invalid");
  const macKey = createHmac("sha256", authenticationKey)
    .update(MAC_DOMAIN)
    .digest();
  const keyId = createHash("sha256").update(macKey).digest("hex");
  return Object.freeze({
    keyId,
    rowMac: (journal, row) =>
      createHmac("sha256", macKey)
        .update(
          watcherCanonicalJson([
            "row",
            journal,
            row.row_key,
            row.scope,
            row.state,
            row.revision.toString(),
            row.body,
          ]),
        )
        .digest("hex"),
  });
};
