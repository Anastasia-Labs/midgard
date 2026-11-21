/**
 * The facts an own block's derived status is read from (plan §8.2, §8.3),
 * written directly: the commit intent the intent journal holds for a
 * signed journal, the landed row of its block, its retained fold, and the
 * derived statuses read back.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { PendingBlockFinalizationsDB } from "../../src/database/index.js";
import {
  insertRow,
  type LandedBlockRow,
} from "../../src/landed-blocks/store.js";

const ZERO_ROOT = "00".repeat(32);

/**
 * The commit intent of `headerHash`'s signed journal, as recording its
 * signed commit leaves it: no spends and no validity bound, so S6 derives
 * it pending until a landing or a conflict says otherwise.
 */
export const insertCommitIntent = (
  headerHash: Buffer,
  txHash: Buffer,
  txCbor: Buffer,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO l1_intents (tx_hash, family, workflow_key, tx_cbor,
        inputs, reference_inputs, collaterals, own_outputs, valid_from_slot,
        valid_to_slot, depends_on, built_generation, built_slot, built_hash,
        content_ref)
      VALUES (${txHash}, 'commit', ${`commit:${headerHash.toString("hex")}`},
        ${txCbor}, '{}'::bytea[], '{}'::bytea[], '{}'::bytea[], '[]'::jsonb,
        NULL, NULL, '{}'::bytea[], 1, 0, ${Buffer.alloc(32)}, ${headerHash})`;
  });

/** The landed row of own block `headerHash` on `parent` (hex). */
export const ownLandedRow = (
  headerHash: string,
  parent: string,
  options: {
    readonly state?: LandedBlockRow["state"];
    readonly applied?: boolean;
    readonly kind?: LandedBlockRow["kind"];
  } = {},
): LandedBlockRow => ({
  headerHash,
  parentHeaderHash: parent,
  parentUtxosRoot: ZERO_ROOT,
  utxosRoot: ZERO_ROOT,
  kind: options.kind ?? "own",
  state: options.state ?? "processed",
  applied: options.applied ?? false,
  spent: [],
  produced: [],
  depositIds: [],
  withdrawals: [],
  forcedIds: [],
  txIds: [],
});

/** Records `row` as the follower-change driver processes a landing. */
export const land = (row: LandedBlockRow) => insertRow(row);

const hexOf = (value: Buffer | string) =>
  typeof value === "string" ? value : value.toString("hex");

/** Own block `headerHash` on `parent` landed and the rebase applied it. */
export const landApplied = (headerHash: Buffer, parent: Buffer | string) =>
  land(ownLandedRow(hexOf(headerHash), hexOf(parent), { applied: true }));

/**
 * Records the empty own block `headerHash` on `parent` (hex) as a retained
 * fold (its `node_confirmed_merges` row), as its fold at the frontier
 * leaves it, without the ledger: its derived status is folded.
 */
export const retainFold = (headerHash: string, parent: string) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) => sql`INSERT INTO node_confirmed_merges (header_hash,
        parent_header_hash, parent_utxos_root, utxos_root, kind, applied,
        spent, produced_outrefs, produced_outputs, deposit_ids, withdrawals,
        forced_ids, tx_ids, consumed_deposit_ids, finalized_withdrawal_ids,
        finalized_forced_ids, merge_slot, merge_out_ref)
      VALUES (${Buffer.from(headerHash, "hex")}, ${Buffer.from(parent, "hex")},
        ${ZERO_ROOT}, ${ZERO_ROOT}, 'own', TRUE, '{}', '{}', '{}', '{}',
        '[]'::jsonb, '{}', '{}', '{}', '{}', '{}', NULL, NULL)`,
  );

/** Every journal's derived status, by hex header hash. */
export const ownBlockStatuses = Effect.flatMap(SqlClient.SqlClient, (sql) =>
  sql<{
    header_hash: Buffer;
    status: PendingBlockFinalizationsDB.OwnBlockStatus;
  }>`SELECT p.header_hash,
      ${PendingBlockFinalizationsDB.ownBlockStatusSql(sql, "p")} AS status
    FROM pending_block_finalizations p`.pipe(
    Effect.map(
      (rows) =>
        new Map(
          rows.map(
            (row) => [row.header_hash.toString("hex"), row.status] as const,
          ),
        ),
    ),
  ),
);

/** The stored journal row of `headerHash`, column by column. */
export const journalRowOf = (headerHash: Buffer) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) => sql<Record<string, unknown>>`SELECT * FROM
      pending_block_finalizations WHERE header_hash = ${headerHash}`,
  ).pipe(Effect.map((rows) => rows[0]));
