/**
 * The native MPF ledgers the node keeps: after each landed-block run the
 * hook has the native owner keep the roots below and drop every other
 * ledger. The owner adds its durable root, its `__root__` marker and every
 * leased fork base itself.
 *
 * - Both roots of every queue block inside the retention window, merged or
 *   not, among the queue outputs the follower still holds
 *   (`readQueueHistory`). A block is inside while the L1 time at the run's
 *   view is below its `block_time` plus `ledger_retention_ms`, as the
 *   watcher counts it. Each block's `prev_utxos_root` keeps its parent's
 *   ledger, the oldest block's predecessor included.
 * - The root of the newest processed block (`node_landed_blocks`), and the
 *   confirmed-ledger frontier's: the rebase restores onto one of the
 *   processed chain's roots.
 * - The queue root's confirmed root: the L1-confirmed ledger.
 */
import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/state-queue";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { sqlErrorToDatabaseError } from "../database/utils/common.js";
import type { LandedStateQueue } from "../l1-state-queue/index.js";
import { Lucid } from "../services/lucid.js";
import type { NativeMpfOwnerService } from "../services/mpf-native-owner/protocol.js";
import { decodeHistory } from "./history.js";
import type { LandedBlockPorts } from "./ports.js";
import { frontierTableName, tableName } from "./store.js";

/**
 * The roots of each processed block no processed block builds on, and the
 * frontier's root. The probe for a child uses the unique index on a
 * processed row's parent, so the read is one index probe per processed row.
 */
const processedTipRoots = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ utxos_root: string }>`
    SELECT b.utxos_root FROM ${sql(tableName)} b
    WHERE b.state = 'processed' AND NOT EXISTS (
      SELECT 1 FROM ${sql(tableName)} c
      WHERE c.state = 'processed' AND c.parent_header_hash = b.header_hash)
    UNION SELECT utxos_root FROM ${sql(frontierTableName)}`;
  return rows.map((row) => row.utxos_root);
}).pipe(
  sqlErrorToDatabaseError(tableName, "Failed to read the processed tip roots"),
);

/** The ledger roots the node keeps at `queue`'s view, whose L1 time is
 * `tipMs`. */
export const retainedLedgerRoots = <R>(
  queueHistory: LandedBlockPorts<R>["queueHistory"],
  queue: LandedStateQueue,
  tipMs: number,
) =>
  Effect.gen(function* () {
    const history = decodeHistory(yield* queueHistory(queue.view));
    const roots = new Set(yield* processedTipRoots);
    for (const [headerHash, header] of history.headers) {
      const blockTime = Number(history.blockTimes.get(headerHash));
      if (tipMs < blockTime + MIDGARD_RETENTION_WINDOW.ledgerRetentionMs) {
        roots.add(header.utxosRoot);
        roots.add(header.prevUtxosRoot);
      }
    }
    if (queue.root !== null)
      roots.add(
        (yield* SDK.getConfirmedStateFromStateQueueDatum(
          queue.root.element.datum,
        )).data.utxoRoot,
      );
    return roots;
  });

/**
 * The hook's retention step over the owner `owner` answers (none before the
 * node has started it): it never fails, and a failure keeps every ledger
 * until the next run.
 */
export const retainLandedLedgers =
  <R>(
    queueHistory: LandedBlockPorts<R>["queueHistory"],
    owner: Effect.Effect<Pick<NativeMpfOwnerService, "retain"> | undefined>,
  ) =>
  (queue: LandedStateQueue) =>
    Effect.gen(function* () {
      const current = yield* owner;
      if (current === undefined) return;
      const lucid = yield* Lucid;
      const roots = yield* retainedLedgerRoots(
        queueHistory,
        queue,
        lucid.api.slotToUnixTime(queue.view.point.slot),
      );
      yield* Effect.tryPromise(() => current.retain(roots));
    }).pipe(
      Effect.catchAll((error) =>
        Effect.logWarning(
          `Native MPF ledger retention did not run; every ledger stays until the next run: ${String(error)}`,
        ),
      ),
    );
