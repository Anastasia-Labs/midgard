import type {
  DerivationContext,
  DialectName,
  TemporalTableSpec,
} from "@al-ft/midgard-l1-follower";

import {
  WATCHER_UNIT_CARRIERS_TABLE,
  WATCHER_UNIT_HISTORY_TABLE,
} from "./tables.js";

/**
 * The history of every unit of a followed deployment policy
 * (`watcherUnitHistoryPolicies`): each canonical tx that created or spent
 * an output holding one. Availability reconstructs a published payload by
 * walking a challenge tranche's outputs through this history, and the
 * fault-proof families' raw snapshots read the history of the hub-oracle,
 * computation-thread, proof-token and user-event units from it. A unit's
 * rows stay open, and its txs pinned, until a tx consumes the unit without
 * re-outputting it (a burn); they are pruned once that is k deep.
 *
 * `carriers` holds the live outputs that carry a unit, so a spend can tell
 * which units it moved; it is closed by the spend, as the follower's own
 * output rows are.
 */
export const FOLLOWED_UNITS_TEMPORAL_TABLES: readonly TemporalTableSpec[] = [
  {
    name: WATCHER_UNIT_CARRIERS_TABLE,
    shape: "versioned",
    startColumn: "from_slot",
    endColumn: "to_slot",
    retention: { kind: "closed_k_deep" },
  },
  {
    name: WATCHER_UNIT_HISTORY_TABLE,
    shape: "versioned",
    startColumn: "from_slot",
    endColumn: "to_slot",
    retention: { kind: "closed_k_deep" },
  },
];

export const followedUnitsMigrationSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return `
-- class: D-t; retention: live carriers forever; spent carriers once to_slot (the spend) is k deep
CREATE TABLE ${WATCHER_UNIT_CARRIERS_TABLE} (
  tx_hash ${bytes} NOT NULL,
  output_index integer NOT NULL,
  unit ${bytes} NOT NULL,
  from_slot ${int8} NOT NULL,
  to_slot ${int8},
  PRIMARY KEY (tx_hash, output_index, unit)
);
CREATE INDEX ${WATCHER_UNIT_CARRIERS_TABLE}_from ON ${WATCHER_UNIT_CARRIERS_TABLE} (from_slot);
CREATE INDEX ${WATCHER_UNIT_CARRIERS_TABLE}_to ON ${WATCHER_UNIT_CARRIERS_TABLE} (to_slot);
-- class: D-t; retention: rows of a unit still on chain forever; closed rows once to_slot (the unit's burn) is k deep
CREATE TABLE ${WATCHER_UNIT_HISTORY_TABLE} (
  unit ${bytes} NOT NULL,
  tx_hash ${bytes} NOT NULL,
  block_hash ${bytes} NOT NULL,
  block_height ${int8} NOT NULL,
  from_slot ${int8} NOT NULL,
  to_slot ${int8},
  PRIMARY KEY (unit, tx_hash)
);
CREATE INDEX ${WATCHER_UNIT_HISTORY_TABLE}_from ON ${WATCHER_UNIT_HISTORY_TABLE} (from_slot);
CREATE INDEX ${WATCHER_UNIT_HISTORY_TABLE}_to ON ${WATCHER_UNIT_HISTORY_TABLE} (to_slot);
CREATE INDEX ${WATCHER_UNIT_HISTORY_TABLE}_tx ON ${WATCHER_UNIT_HISTORY_TABLE} (tx_hash);
`;
};

/**
 * Records one tx's moves of followed units: closes the carriers it spends,
 * opens the carriers it creates, appends the tx to the history of every
 * unit it moved, and closes the history of a unit it spent without
 * re-outputting. Only tracked outputs are seen: a protocol unit sits at
 * its script's address, which is followed, so its moves are recorded; a
 * unit sent to an untracked address leaves the history there (the spend
 * reads as a burn).
 */
export const recordTrackedUnits = async (
  context: DerivationContext,
  entry: DerivationContext["qualified"][number],
  policies: ReadonlySet<string>,
): Promise<void> => {
  const { tx, block } = context;
  const slot = block.point.slot;
  const spent = new Set<string>();
  for (const outRef of entry.spent) {
    const rows = await tx.query(
      `SELECT unit FROM ${WATCHER_UNIT_CARRIERS_TABLE} WHERE tx_hash = ? AND output_index = ? AND to_slot IS NULL`,
      [outRef.txHash, outRef.index],
    );
    if (rows.length === 0) continue;
    for (const row of rows)
      spent.add(Buffer.from(row.unit as Uint8Array).toString("hex"));
    await tx.query(
      `UPDATE ${WATCHER_UNIT_CARRIERS_TABLE} SET to_slot = ? WHERE tx_hash = ? AND output_index = ? AND to_slot IS NULL`,
      [slot, outRef.txHash, outRef.index],
    );
  }
  const created = new Set<string>();
  for (const { outRef, output } of entry.created)
    for (const [policy, names] of output.assets) {
      if (!policies.has(policy)) continue;
      for (const [name, quantity] of names) {
        if (quantity <= 0n) continue;
        const unit = policy + name;
        created.add(unit);
        await tx.query(
          `INSERT INTO ${WATCHER_UNIT_CARRIERS_TABLE} (tx_hash, output_index, unit, from_slot, to_slot) VALUES (?, ?, ?, ?, NULL)`,
          [outRef.txHash, outRef.index, Buffer.from(unit, "hex"), slot],
        );
      }
    }
  for (const unit of new Set([...spent, ...created])) {
    await tx.query(
      `INSERT INTO ${WATCHER_UNIT_HISTORY_TABLE} (unit, tx_hash, block_hash, block_height, from_slot, to_slot) VALUES (?, ?, ?, ?, ?, NULL)`,
      [
        Buffer.from(unit, "hex"),
        entry.tx.hash,
        block.point.hash,
        block.height,
        slot,
      ],
    );
    if (!created.has(unit))
      await tx.query(
        `UPDATE ${WATCHER_UNIT_HISTORY_TABLE} SET to_slot = ? WHERE unit = ? AND to_slot IS NULL`,
        [slot, Buffer.from(unit, "hex")],
      );
  }
};
