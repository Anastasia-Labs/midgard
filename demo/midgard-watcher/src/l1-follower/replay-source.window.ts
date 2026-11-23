/**
 * The follower facts the replayed ledger's retention reads (DS-3): the L1
 * time at the follower's tip, and the state-queue blocks whose ledgers stay
 * readable then, so a block that can still be classified or replayed finds
 * its ledgers. A block is inside the window while the tip's time is below
 * `block_time + ledger_retention_ms`; only blocks with a node row open, or
 * closed since the slot of `tip - ledger_retention_ms` (rows that
 * `replay-retention.ts` keeps from pruning), are read. That is exact for
 * merged blocks: a merge cannot precede `block_time + maturity`.
 * A block removed by a fault proof or pruned as a descendant can leave up to
 * its commit's validity range early, which loses nothing: no one reads the
 * ledger after it, and a rollback that undoes the removal reopens its row,
 * so its ledger is replayed again from the nearest ledger still held (its
 * parent's, while the parent is queued or confirmed).
 */
import {
  type L1SlotConfig,
  l1SlotToPosixTimeMs,
} from "@al-ft/midgard-core/l1-slot-config";
import type { ReplayedLedgerBlockRoots } from "@al-ft/midgard-fault-proofs/replayed-ledger";
import type { FactStore, SqlTx } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk/core";
import { Data } from "@lucid-evolution/lucid";

import { WATCHER_QUEUE_OUTPUTS_TABLE } from "./tables.js";

type WindowBlock = ReplayedLedgerBlockRoots & { readonly blockTimeMs: number };

const hex = (value: unknown) =>
  Buffer.from(value as Uint8Array).toString("hex");

/** The L1 time of `tx`'s follower tip, or `undefined` before its first block. */
const tipTimeIn = async (
  tx: SqlTx,
  slotConfig: L1SlotConfig,
): Promise<number | undefined> => {
  const cursor = (await tx.query("SELECT slot FROM l1_follower_cursor"))[0];
  return cursor === undefined
    ? undefined
    : Number(l1SlotToPosixTimeMs(slotConfig, BigInt(String(cursor.slot))));
};

/** The slot holding the L1 time `timeMs`, rounded down. */
const slotAtOrBefore = (slotConfig: L1SlotConfig, timeMs: number): number =>
  slotConfig.zeroSlot +
  Math.floor((timeMs - slotConfig.zeroTime) / slotConfig.slotLength);

/** The L1 time at the follower's tip, in ms; `undefined` before its first
 * block. */
export const watcherL1TipTimeMs = (
  store: FactStore,
  slotConfig: L1SlotConfig,
): Promise<number | undefined> =>
  store.transaction("read", (tx) => tipTimeIn(tx, slotConfig));

/** The header `headerHash` as its authenticated admission carries it, or
 * `undefined` when the follower holds no node row of it. */
export const watcherAdmittedHeader = async (
  store: FactStore,
  headerHash: string,
): Promise<SDK.Header | undefined> => {
  const row = (
    await store.transaction("read", (tx) =>
      tx.query(
        `SELECT header_cbor FROM ${WATCHER_QUEUE_OUTPUTS_TABLE} WHERE kind = 'node' AND header_hash = ? ORDER BY from_slot LIMIT 1`,
        [Buffer.from(headerHash, "hex")],
      ),
    )
  )[0];
  return row === undefined
    ? undefined
    : Data.from(hex(row.header_cbor), SDK.Header);
};

/**
 * Reads the blocks inside the ledger window, and the root after the queue's
 * newest block (`undefined` when the queue holds none). A block's roots and
 * `block_time` never change between its rows, so each is decoded once.
 */
export const createWatcherLedgerWindowReader = (
  input: Readonly<{
    store: FactStore;
    slotConfig: L1SlotConfig;
    ledgerRetentionMs: number;
  }>,
) => {
  let decoded = new Map<string, WindowBlock>();
  return async (): Promise<{
    readonly blocks: readonly ReplayedLedgerBlockRoots[];
    readonly newestRoot: string | undefined;
  }> =>
    await input.store.transaction("read", async (tx) => {
      const tipMs = await tipTimeIn(tx, input.slotConfig);
      if (tipMs === undefined) return { blocks: [], newestRoot: undefined };
      const rows = await tx.query(
        `SELECT header_hash FROM ${WATCHER_QUEUE_OUTPUTS_TABLE} WHERE kind = 'node' AND header_hash IS NOT NULL AND to_slot IS NULL UNION SELECT header_hash FROM ${WATCHER_QUEUE_OUTPUTS_TABLE} WHERE kind = 'node' AND header_hash IS NOT NULL AND to_slot >= ?`,
        [slotAtOrBefore(input.slotConfig, tipMs - input.ledgerRetentionMs)],
      );
      const next = new Map<string, WindowBlock>();
      for (const row of rows) {
        const headerHash = hex(row.header_hash);
        let block = decoded.get(headerHash);
        if (block === undefined) {
          const first = (
            await tx.query(
              `SELECT state_queue_node_cbor FROM ${WATCHER_QUEUE_OUTPUTS_TABLE} WHERE kind = 'node' AND header_hash = ? LIMIT 1`,
              [Buffer.from(row.header_hash as Uint8Array)],
            )
          )[0]!;
          const node = Data.from(
            hex(first.state_queue_node_cbor),
            SDK.StateQueueNode,
          );
          block = {
            utxosRoot: node.header.utxosRoot,
            prevUtxosRoot: node.header.prevUtxosRoot,
            blockTimeMs: Number(node.block_time),
          };
        }
        next.set(headerHash, block);
      }
      decoded = next;
      const tail = (
        await tx.query(
          `SELECT header_hash FROM ${WATCHER_QUEUE_OUTPUTS_TABLE} WHERE kind = 'node' AND to_slot IS NULL AND next_header_hash IS NULL AND header_hash IS NOT NULL LIMIT 1`,
        )
      )[0];
      return {
        blocks: [...next.values()].filter(
          (block) => tipMs < block.blockTimeMs + input.ledgerRetentionMs,
        ),
        newestRoot:
          tail === undefined
            ? undefined
            : next.get(hex(tail.header_hash))?.utxosRoot,
      };
    });
};
