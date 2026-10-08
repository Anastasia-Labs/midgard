import type {
  Cursor,
  FactStore,
  PinResult,
  SqlTx,
} from "@al-ft/midgard-l1-follower";

import {
  WATCHER_DEPARTED_HEADERS_TABLE,
  WATCHER_PROOF_PIN_UNITS_TABLE,
  WATCHER_PROOF_PINS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
  WATCHER_UNIT_HISTORY_TABLE,
} from "./tables.js";
import type { WatcherL1Degradation } from "./tx-inputs.js";

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
 *
 * Both writes go through the follower's `pinRetained`, under its cursor
 * lock: a prune step either committed first, and the pin reports
 * `already_pruned`, or runs after the pin and keeps the rows.
 *
 * No result is dropped. An objective whose history pruning removed first
 * is the named degradation `l1_proof_history_pruned` (status and metrics)
 * until it is released; its captures refuse to read the history rather than
 * read a partial one. A capture whose header holds no pin reads for no open
 * objective (header classification reads every finalized header before any
 * objective exists; an open objective's header holds a pin or is named
 * pruned), so it writes no hold and reads as before: within k the rows are
 * stored, and past k the raw reads refuse it `beyond_retention`.
 */

/** An open objective whose history pruning removed before a pin held it. */
export const L1_PROOF_HISTORY_PRUNED = "l1_proof_history_pruned";

export type WatcherProofRetentionTarget = Readonly<{
  category: string;
  headerHash: string;
}>;

/**
 * `pinned`: the header's history is held from now on (or already was).
 * `already_pruned`: pruning removed the header's history first (or the
 * store holds none of it and has pruned since its origin, so it cannot
 * tell); nothing is held, and a proof over the header cannot read its L1
 * history from this store. A history the store holds none of before any
 * pruning is empty, not gone, and is pinned.
 */
export type WatcherProofPinResult = PinResult;

/**
 * `held`: every named unit's history is held from now on (or already was).
 * `not_pinned`: the header holds no pin (a read for no open objective), so
 * no unit hold was written.
 * `already_pruned`: pruning removed the history of `units` first; the other
 * units are held. A header whose own pin pruning beat reports every unit.
 */
export type WatcherProofUnitHoldResult =
  | Readonly<{ kind: "held" }>
  | Readonly<{ kind: "not_pinned" }>
  | Readonly<{ kind: "already_pruned"; units: readonly string[] }>;

export type WatcherProofRetention = Readonly<{
  /**
   * The k the follower store rewinds and prunes with, in blocks: a proof
   * completion is final, and its hold may be released, only k deep.
   */
  securityParameter: number;
  /** Holds the target's history under the follower's cursor lock; idempotent. */
  pin(target: WatcherProofRetentionTarget): Promise<WatcherProofPinResult>;
  /** Releases the target's hold, and the header's unit holds with its last pin. */
  release(target: WatcherProofRetentionTarget): Promise<void>;
  /** Holds the followed units a capture for `headerHash` reads, while the header is pinned. */
  holdUnits(
    headerHash: string,
    units: readonly string[],
  ): Promise<WatcherProofUnitHoldResult>;
  /** Every held target, header order. */
  pinned(): Promise<readonly WatcherProofRetentionTarget[]>;
  /** The pins and holds pruning beat, as one status-only degradation. */
  degradations(): readonly WatcherL1Degradation[];
}>;

const HEADER = /^[0-9a-f]{56}$/u;
const UNIT = /^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u;

const headerBytes = (headerHash: string): Buffer => {
  if (!HEADER.test(headerHash))
    throw new Error(`proof retention header ${headerHash} is malformed`);
  return Buffer.from(headerHash, "hex");
};

/**
 * Whether a header's or unit's rows in `table` are complete under the
 * cursor lock: a pin row already holds them, or some exist and none is
 * closed at or before the slot pruning has reached (a row closed there that
 * survives sits in a budget-cut prune step whose siblings may be gone), or
 * none exist and nothing was pruned yet (the history is empty, not gone).
 */
const historyRetained = async (
  tx: SqlTx,
  cursor: Cursor,
  key: Buffer,
  held: Readonly<{ table: string; column: string }>,
  rows: readonly Readonly<{ table: string; column: string }>[],
): Promise<boolean> => {
  const holds = await tx.query(
    `SELECT 1 AS one FROM ${held.table} WHERE ${held.column} = ? LIMIT 1`,
    [key],
  );
  if (holds.length > 0) return true;
  const [first, ...others] = rows;
  if (first === undefined) return false;
  const present = await tx.query(
    `SELECT 1 AS one FROM ${first.table} WHERE ${first.column} = ? LIMIT 1`,
    [key],
  );
  if (present.length === 0)
    return cursor.prunedThroughSlot <= cursor.origin.slot;
  for (const { table, column } of [first, ...others]) {
    const closed = await tx.query(
      `SELECT 1 AS one FROM ${table} WHERE ${column} = ? AND to_slot IS NOT NULL AND to_slot <= ? LIMIT 1`,
      [key, cursor.prunedThroughSlot],
    );
    if (closed.length > 0) return false;
  }
  return true;
};

export const createWatcherProofRetention = (
  store: Pick<
    FactStore,
    "transaction" | "pinRetained" | "securityParameter" | "dialect"
  >,
): WatcherProofRetention => {
  /** `category:header` (or `header#units`) to what pruning removed first. */
  const pruned = new Map<string, string>();
  const prunedHeader = (headerHash: string): boolean =>
    [...pruned.keys()].some((key) => key.endsWith(`:${headerHash}`));
  const unitsKey = (headerHash: string): string => `${headerHash}#units`;
  return Object.freeze({
    securityParameter: store.securityParameter,
    pin: async ({ category, headerHash }) => {
      const header = headerBytes(headerHash);
      const result = await store.pinRetained({
        retained: async (tx, cursor) =>
          cursor !== null &&
          (await historyRetained(
            tx,
            cursor,
            header,
            { table: WATCHER_PROOF_PINS_TABLE, column: "header_hash" },
            [
              {
                table: WATCHER_QUEUE_UNIT_HISTORY_TABLE,
                column: "header_hash",
              },
              { table: WATCHER_DEPARTED_HEADERS_TABLE, column: "header_hash" },
            ],
          )),
        insert: async (tx) => {
          await tx.query(
            `INSERT INTO ${WATCHER_PROOF_PINS_TABLE} (header_hash, category) VALUES (?, ?) ON CONFLICT DO NOTHING`,
            [header, category],
          );
        },
      });
      const key = `${category}:${headerHash}`;
      if (result.kind === "already_pruned")
        pruned.set(key, `the history of header ${headerHash} (${category})`);
      else pruned.delete(key);
      return result;
    },
    release: async ({ category, headerHash }) => {
      const header = headerBytes(headerHash);
      const last = await store.transaction("write", async (tx) => {
        await tx.query(
          `DELETE FROM ${WATCHER_PROOF_PINS_TABLE} WHERE header_hash = ? AND category = ?`,
          [header, category],
        );
        await tx.query(
          `DELETE FROM ${WATCHER_PROOF_PIN_UNITS_TABLE} WHERE header_hash = ? AND NOT EXISTS (SELECT 1 FROM ${WATCHER_PROOF_PINS_TABLE} p WHERE p.header_hash = ?)`,
          [header, header],
        );
        return (
          (
            await tx.query(
              `SELECT 1 AS one FROM ${WATCHER_PROOF_PINS_TABLE} WHERE header_hash = ? LIMIT 1`,
              [header],
            )
          ).length === 0
        );
      });
      pruned.delete(`${category}:${headerHash}`);
      if (last) pruned.delete(unitsKey(headerHash));
    },
    holdUnits: async (headerHash, units) => {
      const header = headerBytes(headerHash);
      if (units.length === 0) return { kind: "held" };
      const gone: string[] = [];
      for (const unit of new Set(units)) {
        if (!UNIT.test(unit)) continue;
        const unitBytes = Buffer.from(unit, "hex");
        let headerPinned = true;
        const result = await store.pinRetained({
          retained: async (tx, cursor) => {
            // Locked so a concurrent release cannot delete the pin between
            // this read and the unit insert (Postgres READ COMMITTED; SQLite
            // serializes every write transaction and its clause is empty).
            headerPinned =
              (
                await tx.query(
                  `SELECT 1 AS one FROM ${WATCHER_PROOF_PINS_TABLE} WHERE header_hash = ? LIMIT 1${store.dialect.lockClause("update")}`,
                  [header],
                )
              ).length > 0;
            if (!headerPinned || cursor === null) return false;
            return historyRetained(
              tx,
              cursor,
              unitBytes,
              { table: WATCHER_PROOF_PIN_UNITS_TABLE, column: "unit" },
              [{ table: WATCHER_UNIT_HISTORY_TABLE, column: "unit" }],
            );
          },
          insert: async (tx) => {
            await tx.query(
              `INSERT INTO ${WATCHER_PROOF_PIN_UNITS_TABLE} (header_hash, unit) VALUES (?, ?) ON CONFLICT DO NOTHING`,
              [header, unitBytes],
            );
          },
        });
        if (!headerPinned) {
          // A header whose own pin pruning beat holds none of its units.
          if (prunedHeader(headerHash))
            return { kind: "already_pruned", units: [...units] };
          return { kind: "not_pinned" };
        }
        if (result.kind === "already_pruned") gone.push(unit);
      }
      if (gone.length === 0) return { kind: "held" };
      pruned.set(
        unitsKey(headerHash),
        `the history of unit(s) ${gone.join(", ")} for header ${headerHash}`,
      );
      return { kind: "already_pruned", units: gone };
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
    degradations: () => {
      const removed = [...pruned.values()];
      return removed.length === 0
        ? []
        : [
            {
              reason: L1_PROOF_HISTORY_PRUNED,
              count: removed.length,
              detail: `${removed.length.toString()} open proof(s) lost history to pruning before a pin held it; first: ${removed[0]!} (its captures refuse; clears when the objective is released)`,
            },
          ];
    },
  });
};
