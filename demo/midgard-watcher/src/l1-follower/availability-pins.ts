import type { DialectName, FactStore } from "@al-ft/midgard-l1-follower";

import { WATCHER_AVAILABILITY_TX_PINS_TABLE } from "./tables.js";

/**
 * The availability journal's transaction pins. The journal reconciles a
 * confirmed transaction until it is deeper than the recovery depth (and an
 * Open for as long as its challenge workflow is live), reading its inclusion
 * from the follower on every pass. The follower prunes a transaction whose
 * outputs are all spent once it is past its retained depth, which comes
 * first, so each row holds one such transaction (a registered tx pin) while
 * the journal still reconciles it.
 */
export const availabilityTxPinsMigrationSql = (dialect: DialectName): string =>
  `
-- class: B; retention: the watcher deletes a row once the availability journal no longer reconciles the transaction
CREATE TABLE ${WATCHER_AVAILABILITY_TX_PINS_TABLE} (
  tx_hash ${dialect === "postgres" ? "bytea" : "BLOB"} NOT NULL PRIMARY KEY
);
`;

/**
 * Holds exactly `txHashes`: pins the ones not held yet and releases every
 * other pin. A transaction the follower does not store gains nothing from a
 * pin, and one it already pruned stays pruned.
 */
export const watcherAvailabilityTransactionPins =
  (store: Pick<FactStore, "transaction" | "dialect">) =>
  async (txHashes: readonly string[]): Promise<void> => {
    const wanted = new Set(txHashes);
    for (const txHash of wanted)
      if (!/^[0-9a-f]{64}$/u.test(txHash))
        throw new Error(`Invalid availability transaction pin ${txHash}`);
    await store.transaction("write", async (tx) => {
      // A prune step holds the cursor row for its whole transaction. Taking
      // the row first commits these pins before such a step or after it,
      // never between its reads and its deletes, as `pinRetainedIn` does.
      await tx.query(
        `SELECT 1 AS one FROM l1_follower_cursor${store.dialect.lockClause("update")}`,
      );
      const held = new Set(
        (
          await tx.query(
            `SELECT tx_hash FROM ${WATCHER_AVAILABILITY_TX_PINS_TABLE}`,
          )
        ).map((row) => Buffer.from(row.tx_hash as Uint8Array).toString("hex")),
      );
      for (const txHash of held)
        if (!wanted.has(txHash))
          await tx.query(
            `DELETE FROM ${WATCHER_AVAILABILITY_TX_PINS_TABLE} WHERE tx_hash = ?`,
            [Buffer.from(txHash, "hex")],
          );
      for (const txHash of wanted)
        if (!held.has(txHash))
          await tx.query(
            `INSERT INTO ${WATCHER_AVAILABILITY_TX_PINS_TABLE} (tx_hash) VALUES (?) ON CONFLICT DO NOTHING`,
            [Buffer.from(txHash, "hex")],
          );
    });
  };
