import { depth, type SqlTx } from "@al-ft/midgard-l1-follower";

import {
  beyondCompletionDepth,
  type WatcherProofRetentionStore,
} from "./proof-retention.block-depth.js";
import {
  WATCHER_PROOF_PIN_EVENTS_TABLE,
  WATCHER_PROOF_PIN_UNITS_TABLE,
  WATCHER_PROOF_PINS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
  WATCHER_UNIT_CARRIERS_TABLE,
  WATCHER_UNIT_HISTORY_TABLE,
} from "./tables.js";

type Target = Readonly<{ category: string; headerHash: string }>;

/**
 * Deletes one pin, and the header's unit and event holds with its last pin;
 * returns whether it was the header's last. The caller holds the cursor row
 * (`lockCursorIn`).
 */
export const releasePinIn = async (
  tx: SqlTx,
  header: Buffer,
  category: string,
): Promise<boolean> => {
  await tx.query(
    `DELETE FROM ${WATCHER_PROOF_PINS_TABLE} WHERE header_hash = ? AND category = ?`,
    [header, category],
  );
  for (const table of [
    WATCHER_PROOF_PIN_UNITS_TABLE,
    WATCHER_PROOF_PIN_EVENTS_TABLE,
  ])
    await tx.query(
      `DELETE FROM ${table} WHERE header_hash = ? AND NOT EXISTS (SELECT 1 FROM ${WATCHER_PROOF_PINS_TABLE} p WHERE p.header_hash = ?)`,
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
};

/**
 * Takes the cursor row, returning the cursor height (null: no cursor). A
 * prune step holds the row for its whole transaction, and its record hooks
 * read the pins before its deletes run, so a release that takes it first
 * commits before such a step or after it, never between its hooks and its
 * deletes, as `pinRetainedIn` does.
 */
export const lockCursorIn = async (
  tx: SqlTx,
  store: Pick<WatcherProofRetentionStore, "dialect">,
): Promise<number | null> => {
  const row = (
    await tx.query(
      `SELECT height FROM l1_follower_cursor${store.dialect.lockClause("update")}`,
    )
  )[0];
  return row === undefined ? null : Number(row.height);
};

export type WatcherUnownedHoldsInput = Readonly<{
  /** Whether an objective (or a hold in flight) owns the target. */
  owned(target: Target): boolean;
  /** The headers the admitted observation's finalized queue holds. */
  queued: ReadonlySet<string>;
}>;

/** The holds no objective owns, after the latest pass that released them. */
export type WatcherUnownedHoldCounts = Readonly<{
  /** Holds no objective owns that the pass kept. */
  unowned: number;
  /** Of those, the holds kept by a live held unit. */
  live: number;
}>;

export type WatcherUnownedHoldRelease = Readonly<{
  release(
    input: WatcherUnownedHoldsInput,
    forget: (target: Target, last: boolean) => void,
  ): Promise<readonly Target[]>;
  counts(): WatcherUnownedHoldCounts;
}>;

const hex = (bytes: unknown): string =>
  Buffer.from(bytes as Uint8Array).toString("hex");

/**
 * Releases the holds no objective owns, in one write transaction under the
 * cursor lock. Called only from a completed admission pass at tip, so every
 * open objective has been re-admitted and pinned in this process, and a hold
 * nothing owns is left over from a released objective, a lost record, or a
 * previous process. A hold is released only when no reader can still need
 * the history it keeps, judged from the store alone:
 *
 * (a) the header has left the queue: it is not in the observation's
 *     finalized queue, no queue unit history row of it is open, and its
 *     last move (its departure) is beyond completion depth
 *     (`beyondCompletionDepth`, k + 1, the depth the inclusion wait uses);
 * (b) no unit held under the header still has a live carrier, and each
 *     unit's last move is beyond completion depth. A live carrier at the
 *     deployment's fraud-proof spend address does not count: units there
 *     (the proof token, a convicted case's NFT) stay there by protocol, and
 *     their open history is retained without any hold (open unit history
 *     rows are never pruned), so a hold over them would only last forever.
 *
 * A header that returns to the queue after a rollback has an open row
 * again and is kept. `owned` is asked again just before each delete: a hold
 * a job run starts meanwhile is owned from before its pin is written.
 */
export const createUnownedHoldRelease = (
  store: Pick<WatcherProofRetentionStore, "transaction" | "dialect">,
  options: Readonly<{ securityParameter: number; fraudProofSpend: string }>,
): WatcherUnownedHoldRelease => {
  const permanent = Buffer.from(options.fraudProofSpend, "hex");
  let counts: WatcherUnownedHoldCounts = { unowned: 0, live: 0 };
  const settled = (tip: number, height: unknown): boolean =>
    height === null ||
    height === undefined ||
    beyondCompletionDepth(
      depth(tip, Number(height)),
      options.securityParameter,
    );
  /**
   * `release`: (a) and (b) hold; `queued`: (a) does not; `live`, `recent`:
   * (b) does not, by a live carrier or a move short of completion depth.
   */
  const standing = async (
    tx: SqlTx,
    tip: number,
    header: Buffer,
    queued: boolean,
  ): Promise<"release" | "queued" | "live" | "recent"> => {
    if (queued) return "queued";
    const open = await tx.query(
      `SELECT 1 AS one FROM ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} WHERE header_hash = ? AND to_slot IS NULL LIMIT 1`,
      [header],
    );
    if (open.length > 0) return "queued";
    const departed = await tx.query(
      `SELECT max(block_height) AS height FROM ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} WHERE header_hash = ?`,
      [header],
    );
    if (!settled(tip, departed[0]?.height)) return "queued";
    const units = await tx.query(
      `SELECT unit FROM ${WATCHER_PROOF_PIN_UNITS_TABLE} WHERE header_hash = ?`,
      [header],
    );
    let recent = false;
    for (const { unit } of units) {
      const live = await tx.query(
        `SELECT 1 AS one FROM ${WATCHER_UNIT_CARRIERS_TABLE} c LEFT JOIN l1_outputs o ON o.tx_hash = c.tx_hash AND o.output_index = c.output_index WHERE c.unit = ? AND c.to_slot IS NULL AND (o.payment_cred IS NULL OR o.payment_cred <> ?) LIMIT 1`,
        [Buffer.from(unit as Uint8Array), permanent],
      );
      if (live.length > 0) return "live";
      const moved = await tx.query(
        `SELECT max(block_height) AS height FROM ${WATCHER_UNIT_HISTORY_TABLE} WHERE unit = ?`,
        [Buffer.from(unit as Uint8Array)],
      );
      if (!settled(tip, moved[0]?.height)) recent = true;
    }
    return recent ? "recent" : "release";
  };
  return Object.freeze({
    counts: () => counts,
    release: async ({ owned, queued }, forget) => {
      const result = await store.transaction("write", async (tx) => {
        const released: { target: Target; last: boolean }[] = [];
        let unowned = 0;
        let live = 0;
        const tip = await lockCursorIn(tx, store);
        if (tip === null) return { released, unowned, live };
        const pins = await tx.query(
          `SELECT header_hash, category FROM ${WATCHER_PROOF_PINS_TABLE} ORDER BY header_hash, category`,
        );
        for (const row of pins) {
          const target = {
            category: String(row.category),
            headerHash: hex(row.header_hash),
          };
          if (owned(target)) continue;
          const header = Buffer.from(target.headerHash, "hex");
          const state = await standing(
            tx,
            tip,
            header,
            queued.has(target.headerHash),
          );
          if (owned(target)) continue;
          if (state !== "release") {
            unowned += 1;
            if (state === "live") live += 1;
            continue;
          }
          released.push({
            target,
            last: await releasePinIn(tx, header, target.category),
          });
        }
        return { released, unowned, live };
      });
      counts = { unowned: result.unowned, live: result.live };
      for (const { target, last } of result.released) forget(target, last);
      return result.released.map(({ target }) => target);
    },
  });
};
