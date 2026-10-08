import type { FactStore } from "@al-ft/midgard-l1-follower";

import {
  WATCHER_PROOF_PIN_UNITS_TABLE,
  WATCHER_PROOF_PINS_TABLE,
} from "./tables.js";

/**
 * Proof retention (E1 ruling, option (a)): while a proof objective over a
 * header is open, the header's L1 history is pinned past the follower's
 * k-deep pruning; once its completion marker is verified past k the pin
 * releases and normal pruning applies. Never a time window.
 *
 * A pin row (header, category) holds the header's queue unit history and
 * its departed-header row (registered row pins), and through the history
 * its txs and checkpoints. The followed units the objective's proof reads
 * (its computation thread and proof token) are held by unit rows, written
 * when a capture names them and only while the header holds a pin; a
 * header's unit rows go with its last pin.
 */

export type WatcherProofRetentionTarget = Readonly<{
  category: string;
  headerHash: string;
}>;

export type WatcherProofRetention = Readonly<{
  /** Holds the target's history; idempotent. */
  pin(target: WatcherProofRetentionTarget): Promise<void>;
  /** Releases the target's hold, and the header's unit holds with its last pin. */
  release(target: WatcherProofRetentionTarget): Promise<void>;
  /** Holds the followed units a capture for `headerHash` reads, while the header is pinned. */
  holdUnits(headerHash: string, units: readonly string[]): Promise<void>;
  /** Every held target, header order. */
  pinned(): Promise<readonly WatcherProofRetentionTarget[]>;
}>;

const HEADER = /^[0-9a-f]{56}$/u;
const UNIT = /^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u;

const headerBytes = (headerHash: string): Buffer => {
  if (!HEADER.test(headerHash))
    throw new Error(`proof retention header ${headerHash} is malformed`);
  return Buffer.from(headerHash, "hex");
};

export const createWatcherProofRetention = (
  store: Pick<FactStore, "transaction">,
): WatcherProofRetention =>
  Object.freeze({
    pin: async ({ category, headerHash }) => {
      const header = headerBytes(headerHash);
      await store.transaction("write", (tx) =>
        tx.query(
          `INSERT INTO ${WATCHER_PROOF_PINS_TABLE} (header_hash, category) VALUES (?, ?) ON CONFLICT DO NOTHING`,
          [header, category],
        ),
      );
    },
    release: async ({ category, headerHash }) => {
      const header = headerBytes(headerHash);
      await store.transaction("write", async (tx) => {
        await tx.query(
          `DELETE FROM ${WATCHER_PROOF_PINS_TABLE} WHERE header_hash = ? AND category = ?`,
          [header, category],
        );
        await tx.query(
          `DELETE FROM ${WATCHER_PROOF_PIN_UNITS_TABLE} WHERE header_hash = ? AND NOT EXISTS (SELECT 1 FROM ${WATCHER_PROOF_PINS_TABLE} p WHERE p.header_hash = ?)`,
          [header, header],
        );
      });
    },
    holdUnits: async (headerHash, units) => {
      if (units.length === 0 || !HEADER.test(headerHash)) return;
      const header = Buffer.from(headerHash, "hex");
      await store.transaction("write", async (tx) => {
        for (const unit of new Set(units)) {
          if (!UNIT.test(unit)) continue;
          await tx.query(
            `INSERT INTO ${WATCHER_PROOF_PIN_UNITS_TABLE} (header_hash, unit) SELECT ?, ? WHERE EXISTS (SELECT 1 FROM ${WATCHER_PROOF_PINS_TABLE} p WHERE p.header_hash = ?) ON CONFLICT DO NOTHING`,
            [header, Buffer.from(unit, "hex"), header],
          );
        }
      });
    },
    pinned: async () =>
      (
        await store.transaction("read", (tx) =>
          tx.query(
            `SELECT header_hash, category FROM ${WATCHER_PROOF_PINS_TABLE} ORDER BY header_hash, category`,
          ),
        )
      ).map((row) => ({
        category: String(row.category),
        headerHash: Buffer.from(row.header_hash as Uint8Array).toString("hex"),
      })),
  });
