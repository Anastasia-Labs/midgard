import {
  type Cursor,
  type FactStore,
  pinRetainedIn,
  type SqlTx,
} from "@al-ft/midgard-l1-follower";

import {
  COMMITTEE_PINNED_BLOCKS_TABLE,
  COMMITTEE_PINNED_HEADERS_TABLE,
  COMMITTEE_PINNED_TXS_TABLE,
  COMMITTEE_QUEUE_TABLE,
} from "./queue-table.js";

/**
 * The committee's retention pins on its follower (plan §11): the L1 history
 * a committee record will read again stays stored past the follower's
 * k-deep pruning while the record exists. The pins are the committee's own
 * records (class B: a rewind or a reset never touches them), in the
 * follower's database, so the prune statement sees them:
 *
 * - a block pin (by slot) keeps the block row, so its point keeps answering
 *   canonical with its height and depth;
 * - a tx pin (by hash) keeps the tx row, and through it its block;
 * - a header pin keeps the header's state-queue rows (a registered row pin),
 *   and through their slots the blocks that created and spent them: the
 *   header's landing is re-derived from them, and its exit read.
 *
 * Each pin has a holder: `records` (every L1 point and submission the
 * committee store names) or `intents` (the availability responder's own
 * transactions). A holder's pins are replaced as a whole when its source is
 * read again, so a pin goes once the record that needed it is retired or
 * deleted.
 */

/** Who holds a pin. */
export type CommitteePinHolder = "records" | "intents";

/** What one holder keeps: block slots, tx hashes (hex) and headers. */
export type CommitteePinTargets = Readonly<{
  blocks: readonly number[];
  txs: readonly string[];
  /** A header, with the slot its stored record names (its landing or exit). */
  headers: readonly Readonly<{ headerHash: string; slot: number }>[];
}>;

export const NO_PIN_TARGETS: CommitteePinTargets = {
  blocks: [],
  txs: [],
  headers: [],
};

export type CommitteePinWrite = Readonly<{
  holder: CommitteePinHolder;
  /**
   * `replace`: the holder keeps exactly `targets` (stale pins go);
   * `add`: `targets` join the holder's pins.
   */
  mode: "replace" | "add";
  targets: CommitteePinTargets;
}>;

export type CommitteePinWriteResult = Readonly<{
  /**
   * Each target the follower already pruned, named: never inserted, since a
   * pin cannot bring history back. Empty when every target is kept.
   */
  alreadyPruned: readonly string[];
}>;

const HEX_TX = /^[0-9a-f]{64}$/u;

/** Whether `slot` lies above the pruned window (all of it before init). */
const above = (cursor: Cursor | null, slot: number): boolean =>
  cursor === null || slot > cursor.prunedThroughSlot;

const exists = async (
  tx: SqlTx,
  sql: string,
  params: readonly (number | string | Buffer)[],
): Promise<boolean> => (await tx.query(sql, params)).length > 0;

/**
 * Writes one holder's pins, the committee's one pin write. In one follower
 * write transaction each new target is pinned through the follower's
 * `pinRetainedIn`, under the cursor row lock every prune step takes first,
 * and only while the follower still stores it: a target already pruned is
 * returned in `alreadyPruned`, never inserted. A block is still stored when
 * its row exists or it lies above the pruned window; a header while its
 * queue rows exist or its slot lies above the window. A tx not stored may
 * not have landed yet, so it is always pinned.
 */
export const writeCommitteePins = async (
  store: Pick<FactStore, "transaction" | "dialect">,
  write: CommitteePinWrite,
): Promise<CommitteePinWriteResult> =>
  store.transaction("write", async (tx) => {
    const { holder, targets } = write;
    const alreadyPruned: string[] = [];

    const blocks = new Set(targets.blocks);
    const heldBlocks = new Set(
      (
        await tx.query(
          `SELECT slot FROM ${COMMITTEE_PINNED_BLOCKS_TABLE} WHERE holder = ?`,
          [holder],
        )
      ).map((row) => Number(row.slot)),
    );
    for (const slot of [...blocks].sort((a, b) => a - b)) {
      if (heldBlocks.has(slot)) continue;
      const pinned = await pinRetainedIn(tx, store.dialect, {
        retained: async (_, cursor) =>
          above(cursor, slot) ||
          (await exists(tx, "SELECT 1 FROM l1_blocks WHERE slot = ?", [slot])),
        insert: async () => {
          await tx.query(
            `INSERT INTO ${COMMITTEE_PINNED_BLOCKS_TABLE} (slot, holder) VALUES (?, ?)`,
            [slot, holder],
          );
        },
      });
      if (pinned.kind === "already_pruned")
        alreadyPruned.push(`block at slot ${slot.toString()}`);
    }

    const txs = new Set(targets.txs.map((hash) => hash.toLowerCase()));
    for (const hash of txs)
      if (!HEX_TX.test(hash))
        throw new Error(`retention pin tx hash ${hash} is malformed`);
    const heldTxs = new Set(
      (
        await tx.query(
          `SELECT tx_hash FROM ${COMMITTEE_PINNED_TXS_TABLE} WHERE holder = ?`,
          [holder],
        )
      ).map((row) => Buffer.from(row.tx_hash as Uint8Array).toString("hex")),
    );
    for (const hash of [...txs].sort()) {
      if (heldTxs.has(hash)) continue;
      await pinRetainedIn(tx, store.dialect, {
        retained: () => Promise.resolve(true),
        insert: async () => {
          await tx.query(
            `INSERT INTO ${COMMITTEE_PINNED_TXS_TABLE} (tx_hash, holder) VALUES (?, ?)`,
            [Buffer.from(hash, "hex"), holder],
          );
        },
      });
    }

    const headers = new Map(
      targets.headers.map(({ headerHash, slot }) => [headerHash, slot]),
    );
    const heldHeaders = new Set(
      (
        await tx.query(
          `SELECT header_hash FROM ${COMMITTEE_PINNED_HEADERS_TABLE} WHERE holder = ?`,
          [holder],
        )
      ).map((row) => String(row.header_hash)),
    );
    // Code-unit order: a stable insert order, not a digest.
    for (const [headerHash, slot] of [...headers].sort(([a], [b]) =>
      a < b ? -1 : a > b ? 1 : 0,
    )) {
      if (heldHeaders.has(headerHash)) continue;
      const pinned = await pinRetainedIn(tx, store.dialect, {
        retained: async (_, cursor) =>
          above(cursor, slot) ||
          (await exists(
            tx,
            `SELECT 1 FROM ${COMMITTEE_QUEUE_TABLE} WHERE header_hash = ? LIMIT 1`,
            [headerHash],
          )),
        insert: async () => {
          await tx.query(
            `INSERT INTO ${COMMITTEE_PINNED_HEADERS_TABLE} (header_hash, holder) VALUES (?, ?)`,
            [headerHash, holder],
          );
        },
      });
      if (pinned.kind === "already_pruned")
        alreadyPruned.push(
          `state queue rows of header ${headerHash} (slot ${slot.toString()})`,
        );
    }

    if (write.mode === "replace") {
      for (const slot of heldBlocks)
        if (!blocks.has(slot))
          await tx.query(
            `DELETE FROM ${COMMITTEE_PINNED_BLOCKS_TABLE} WHERE slot = ? AND holder = ?`,
            [slot, holder],
          );
      for (const hash of heldTxs)
        if (!txs.has(hash))
          await tx.query(
            `DELETE FROM ${COMMITTEE_PINNED_TXS_TABLE} WHERE tx_hash = ? AND holder = ?`,
            [Buffer.from(hash, "hex"), holder],
          );
      for (const headerHash of heldHeaders)
        if (!headers.has(headerHash))
          await tx.query(
            `DELETE FROM ${COMMITTEE_PINNED_HEADERS_TABLE} WHERE header_hash = ? AND holder = ?`,
            [headerHash, holder],
          );
    }
    return { alreadyPruned };
  });

/** A target list's identity, for skipping a write that changes nothing. */
const targetsKey = (targets: CommitteePinTargets): string =>
  JSON.stringify([
    [...new Set(targets.blocks)].sort((a, b) => a - b),
    [...new Set(targets.txs.map((hash) => hash.toLowerCase()))].sort(),
    [...targets.headers]
      .map(({ headerHash, slot }) => `${headerHash}@${slot.toString()}`)
      .sort(),
  ]);

/**
 * A pinned history the follower had already pruned. Named on `/readyz`: a
 * record that needs it cannot be proven again (its retirement holds with a
 * named reason too).
 */
export const COMMITTEE_RETENTION_PIN_PRUNED = "committee_retention_pin_pruned";

/** The pins could not be written; the follower does not prune meanwhile. */
export const COMMITTEE_RETENTION_PIN_FAILED = "committee_retention_pin_failed";

/**
 * The committee's retention pins over its follower store: each holder's
 * source read again before every prune step (so nothing a record names is
 * pruned before its pin exists), and `add` for an intent pinned before it
 * is submitted. Every write goes through {@link writeCommitteePins}.
 */
export type CommitteeL1Retention = Readonly<{
  /** Binds the source a holder's pins are replaced from on every sync. */
  bind(
    holder: CommitteePinHolder,
    source: () => Promise<CommitteePinTargets> | CommitteePinTargets,
  ): void;
  /** Replaces every bound holder's pins from its source. Throws on a failed write. */
  sync(): Promise<void>;
  /** Adds `targets` to `holder`'s pins now. */
  add(holder: CommitteePinHolder, targets: CommitteePinTargets): Promise<void>;
  /** The named reasons a pin left the committee unready, `reason: detail`. */
  reasons(): readonly string[];
}>;

const FIRST_NAMED = 3;

const named = (holder: string, items: readonly string[]): string =>
  `${COMMITTEE_RETENTION_PIN_PRUNED}: ${holder} need ${items.length.toString()} pruned item(s): ${items.slice(0, FIRST_NAMED).join("; ")}${items.length > FIRST_NAMED ? "; ..." : ""}`;

export const committeeL1Retention = (
  store: Pick<FactStore, "transaction" | "dialect">,
): CommitteeL1Retention => {
  const sources = new Map<
    CommitteePinHolder,
    () => Promise<CommitteePinTargets> | CommitteePinTargets
  >();
  /** Per holder: the targets last written, and what of them was pruned. */
  const written = new Map<
    CommitteePinHolder,
    Readonly<{ key: string; alreadyPruned: readonly string[] }>
  >();
  /** Per holder: pruned targets an `add` met, until the next replace. */
  const added = new Map<CommitteePinHolder, readonly string[]>();
  let failure: string | null = null;
  const sync = async (): Promise<void> => {
    try {
      for (const [holder, source] of sources) {
        const targets = await source();
        const key = targetsKey(targets);
        if (written.get(holder)?.key === key) continue;
        const { alreadyPruned } = await writeCommitteePins(store, {
          holder,
          mode: "replace",
          targets,
        });
        written.set(holder, { key, alreadyPruned });
        added.delete(holder);
      }
      failure = null;
    } catch (error) {
      failure = error instanceof Error ? error.message : String(error);
      throw error;
    }
  };
  return {
    bind: (holder, source) => {
      sources.set(holder, source);
      written.delete(holder);
    },
    sync,
    add: async (holder, targets) => {
      const { alreadyPruned } = await writeCommitteePins(store, {
        holder,
        mode: "add",
        targets,
      });
      // The next sync replaces the holder's pins from its source again.
      written.delete(holder);
      if (alreadyPruned.length > 0)
        added.set(holder, [...(added.get(holder) ?? []), ...alreadyPruned]);
    },
    reasons: () => [
      ...(failure === null
        ? []
        : [`${COMMITTEE_RETENTION_PIN_FAILED}: ${failure}`]),
      ...[...new Set([...written.keys(), ...added.keys()])]
        .sort()
        .flatMap((holder) => {
          const items = [
            ...(written.get(holder)?.alreadyPruned ?? []),
            ...(added.get(holder) ?? []),
          ];
          return items.length === 0 ? [] : [named(holder, items)];
        }),
    ],
  };
};

/** Wraps `store` so every prune step first brings the committee's pins current. */
export const pruneAfterPins = (
  store: FactStore,
  retention: Pick<CommitteeL1Retention, "sync">,
): FactStore => ({
  ...store,
  prune: async (budget) => {
    await retention.sync();
    return store.prune(budget);
  },
});
