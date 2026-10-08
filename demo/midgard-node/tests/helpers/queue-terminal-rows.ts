/**
 * Direct rows for the reads of the queue-terminal projection (N4): a
 * terminal row as the projection derives it for a landed tx that took a
 * header out of the state queue, and a live state-queue node in the
 * follower's facts (a seed output carrying the node token).
 */
import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

const bytes = (label: string, length: number): Buffer =>
  createHash("sha256").update(label).digest().subarray(0, length);

/** One `node_l1_queue_terminals` row for `headerHash`, at `height`. */
export const insertQueueTerminal = (row: {
  readonly headerHash: Buffer;
  readonly outcome: "merged" | "removed";
  readonly height: number;
  readonly transactionHash?: Buffer;
  readonly txIndex?: number;
}): Effect.Effect<void, unknown, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const transactionHash =
      row.transactionHash ??
      Buffer.concat([
        bytes(`terminal-tx:${row.height.toString()}`, 4),
        row.headerHash,
      ]);
    yield* sql`
      INSERT INTO node_l1_queue_terminals (
        header_hash, transaction_hash, terminal_outcome, tx_index,
        block_hash, height, slot
      ) VALUES (
        ${row.headerHash}, ${transactionHash}, ${row.outcome},
        ${row.txIndex ?? 0}, ${bytes(`terminal-block:${row.height.toString()}`, 32)},
        ${row.height}, ${100 + row.height}
      )`;
  });

/**
 * A live (unspent) seed output in the follower's facts carrying the state
 * queue node token of `headerHash`.
 */
export const insertLiveQueueNode = (
  headerHash: Buffer,
): Effect.Effect<void, unknown, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const txHash = bytes(`live-node:${headerHash.toString("hex")}`, 32);
    yield* sql`
      INSERT INTO l1_outputs (
        tx_hash, output_index, address, lovelace, assets, seed_slot
      ) VALUES (${txHash}, 0, ${Buffer.alloc(29, 0x70)}, 2000000, '{}'::jsonb, 1)`;
    yield* sql`
      INSERT INTO l1_output_assets (
        tx_hash, output_index, policy_id, asset_name, quantity
      ) VALUES (
        ${txHash}, 0, ${Buffer.alloc(28, 0x51)},
        ${Buffer.concat([
          Buffer.from(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX, "hex"),
          headerHash,
        ])}, 1
      )`;
  });

/** Spends the live node `insertLiveQueueNode` seeded for `headerHash`. */
export const deleteLiveQueueNode = (
  headerHash: Buffer,
): Effect.Effect<void, unknown, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM l1_outputs
      WHERE tx_hash = ${bytes(`live-node:${headerHash.toString("hex")}`, 32)}`;
  });
