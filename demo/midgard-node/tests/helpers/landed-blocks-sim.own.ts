/**
 * This node's own blocks in the landed-block fork simulator (N3): the block
 * journals the node keeps for blocks it committed, written as the commit
 * leaves them (one active journal at a time, its ledger delta and the
 * mempool transactions it included), and their status moves (finalized on
 * the merge, abandoned when its base leaves the queue, revived when it
 * lands anyway).
 */
import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  MempoolDB,
  PendingBlockFinalizationsDB,
} from "../../src/database/index.js";
import type * as Ledger from "../../src/database/utils/ledger.js";
import { withFollowerWrite } from "../../src/services/follower-write-gate.js";
import type { SimPendingTx } from "./landed-blocks-sim.mempool.js";
import {
  linkedHeader,
  type SimBlockInfo,
  type SimRegistry,
} from "./landed-blocks-sim.traffic.js";
import type { SimUniverse } from "./landed-blocks-sim.universe.js";

const ZERO_ROOT = "00".repeat(32);
const EMPTY_MERKLE_ROOT =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");
const sha256 = (value: Buffer) => createHash("sha256").update(value).digest();

export type SimOwnJournal = Readonly<{
  headerHash: string;
  baseHeaderHash: string;
  baseUtxosRoot: string;
  expectedUtxosRoot: string;
  spent: readonly Buffer[];
  produced: readonly Ledger.MinimalEntry[];
  txIds: readonly Buffer[];
  at: Date;
}>;

export const JournalStatus = PendingBlockFinalizationsDB.Status;

/** Writes the active journal of an own block the node just committed. */
export const insertOwnJournal = (journal: SimOwnJournal) =>
  withFollowerWrite(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const header = Buffer.from(journal.headerHash, "hex");
      yield* sql`INSERT INTO pending_block_finalizations ${sql.insert({
        header_hash: header,
        submitted_tx_hash: null,
        block_end_time: new Date(journal.at.getTime() + 1_000),
        status: JournalStatus.SubmittedUnconfirmed,
        observed_confirmed_at_ms: null,
        created_at: journal.at,
        updated_at: journal.at,
        state_queue_lease_token: `landed-sim:${journal.headerHash}`,
        base_snapshot_id: "landed-sim",
        base_tail_out_ref: "base#0",
        base_tail_header_hash: Buffer.from(journal.baseHeaderHash, "hex"),
        base_tail_datum_cbor: "d87980",
        base_utxos_root: journal.baseUtxosRoot,
        base_transactions_root: ZERO_ROOT,
        base_deposits_root: ZERO_ROOT,
        base_withdrawals_root: ZERO_ROOT,
        block_start_time: journal.at,
        expected_utxos_root: journal.expectedUtxosRoot,
        expected_transactions_root: ZERO_ROOT,
        expected_deposits_root: ZERO_ROOT,
        expected_withdrawals_root: ZERO_ROOT,
        base_forced_transactions_root: ZERO_ROOT,
        expected_forced_transactions_root: ZERO_ROOT,
        header_cbor: Buffer.from("a0", "hex"),
        format_version: 1,
        replay_kind: "ledger_delta_v1",
        deployment_marker_schema_version: "midgard-deployment-marker-v1",
        deployment_manifest_id: "de".repeat(32),
        expected_transition_trace_root: ZERO_ROOT,
        expected_event_to_step_root: ZERO_ROOT,
        expected_withdrawal_count: 0n,
        expected_forced_transaction_count: 0n,
        expected_l2_transaction_count: 0n,
        expected_deposit_count: 0n,
        expected_total_event_count: 0n,
        expected_transition_step_count: 0n,
        consensus_profile_id: "midgard-consensus-v1",
        expected_validation_traces_root: EMPTY_MERKLE_ROOT,
        expected_validation_trace_count: 0n,
        ledger_delta_spent: JSON.stringify(journal.spent.map(hex)),
        ledger_delta_produced: JSON.stringify(
          journal.produced.map((entry) => ({
            outref: hex(entry.outref),
            output: hex(entry.output),
          })),
        ),
      } as never)}`;
      for (const [ordinal, txId] of journal.txIds.entries()) {
        const payload = Buffer.concat([Buffer.from("landed-sim-tx:"), txId]);
        const sidecar = Buffer.from("a0", "hex");
        yield* sql`INSERT INTO pending_block_finalization_txs ${sql.insert({
          header_hash: header,
          member_id: txId,
          ordinal,
          payload_cbor: payload,
          payload_sha256: sha256(payload),
          cek_program_material_sidecar_cbor: sidecar,
          cek_program_material_sidecar_sha256: sha256(sidecar),
          source_table: MempoolDB.tableName,
          source_id: txId,
          source_time_stamp_tz: journal.at,
        } as never)}`;
      }
    }),
  );

export const setJournalStatus = (
  headerHash: string,
  status: PendingBlockFinalizationsDB.Status,
) =>
  withFollowerWrite(
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql`UPDATE pending_block_finalizations SET status = ${status}
        WHERE header_hash = ${Buffer.from(headerHash, "hex")}`,
    ),
  );

/** Every journal's status, by hex header hash. */
export const journalStatuses = Effect.flatMap(SqlClient.SqlClient, (sql) =>
  sql<{ header_hash: Buffer; status: string }>`
    SELECT header_hash, status FROM pending_block_finalizations`.pipe(
    Effect.map(
      (rows) =>
        new Map(rows.map((row) => [hex(row.header_hash), row.status] as const)),
    ),
  ),
);

/** An own block the node committed. */
export type SimOwnBlock = Readonly<{
  headerHash: string;
  parentHash: string;
  header: SDK.Header;
  txIds: readonly Buffer[];
  /** The journal's time: a member it restores to the mempool carries it. */
  at: Date;
}>;

/** The node's own blocks: every one it committed, and its active journal. */
export type SimOwnBook = {
  blocks: Map<string, SimOwnBlock>;
  active: string | undefined;
  committed: number;
};

export const newSimOwnBook = (): SimOwnBook => ({
  blocks: new Map(),
  active: undefined,
  committed: 0,
});

/**
 * The own block the node commits on the processed tip `tip`: the traffic's
 * `candidate` built on it (one that may land), else a fresh one that never
 * does. It spends the tip's `X`, produces the next height's `Y` and `X`,
 * and includes the pending transactions that spend the tip's `X` with
 * every pending transaction that spends their outputs.
 */
export const ownBlockOn = (
  universe: SimUniverse,
  registry: SimRegistry,
  book: SimOwnBook,
  tip: string,
  survivors: readonly SimPendingTx[],
  candidate: string | undefined,
) => {
  book.committed += 1;
  const parent = registry.get(tip)!;
  const planned =
    candidate === undefined ? undefined : registry.get(candidate)!;
  const h = parent.h + 1;
  const b = planned?.b ?? book.committed % 2;
  const header =
    planned?.ownHeader ??
    linkedHeader(
      universe,
      900_000 + book.committed,
      {
        headerHash: tip,
        utxosRoot: universe.root(parent.h, parent.b),
        endTime: parent.endTime,
      },
      h,
      b,
    );
  const headerHash = SDK.stateQueueHeaderHash(header);
  const reached = new Set([hex(universe.x(parent.h, parent.b).outref)]);
  const txIds: Buffer[] = [];
  for (const tx of survivors)
    if (tx.spent.some((outRef) => reached.has(hex(outRef)))) {
      txIds.push(tx.id);
      for (const entry of tx.produced) reached.add(hex(entry.outref));
    }
  const at = new Date(
    Date.parse("2026-10-01T00:00:00.000Z") + book.committed * 60_000,
  );
  const block: SimOwnBlock = {
    headerHash,
    parentHash: tip,
    header,
    txIds,
    at,
  };
  return {
    block,
    info: {
      ...(planned ?? {
        h,
        b,
        bad: false,
        lateDa: false,
        longLate: false,
        prevHeaderHash: tip,
        endTime: header.endTime,
      }),
      own: true,
    } satisfies SimBlockInfo,
    journal: {
      headerHash,
      baseHeaderHash: tip,
      baseUtxosRoot: universe.root(parent.h, parent.b),
      expectedUtxosRoot: universe.root(h, b),
      spent: [
        universe.x(parent.h, parent.b).outref,
        ...[universe.ySpent(h)].flatMap((entry) =>
          entry === undefined ? [] : [entry.outref],
        ),
      ],
      produced: [universe.y(h), universe.x(h, b)],
      txIds,
      at,
    } satisfies SimOwnJournal,
  };
};
