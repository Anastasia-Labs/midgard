/**
 * The node side of the rebase oracle test: one case's database, written
 * through the node's public functions the way the node leaves it (a
 * frontier ledger that `mempool_ledger` matches, admitted transactions,
 * deposits projected outside any block, processed foreign blocks, own
 * journals that live, die or are pruned, a rollback), one rebase run on
 * the plan the node reads (`rebasePlanOf`, then `rebaseSql` in one
 * follower write) and what the node shows after it.
 */
import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk/core";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { ConfirmedLedgerDB, DepositsDB } from "../src/database/index.js";
import { pruneFoldedBeyondChallengeability } from "../src/database/pendingBlockFinalizations.retrieve-applied-missing-da-payloads.js";
import type * as Ledger from "../src/database/utils/ledger.js";
import { ledgerRows } from "../src/landed-blocks/ledger.js";
import { ownJournalDisposition } from "../src/landed-blocks/own-journals.js";
import { rebaseSql } from "../src/landed-blocks/rebase.js";
import { rebasePlanOf } from "../src/landed-blocks/rebase-target.js";
import { processRow, rollBackRows } from "../src/landed-blocks/settlements.js";
import { Frontier, type LandedBlockRow } from "../src/landed-blocks/store.js";
import type { Globals } from "../src/services/globals.globals.js";
import { projectDepositsToMempoolLedger } from "./helpers/deposit-projection.js";
import { testWrite } from "./helpers/driver-recompute.js";
import { insertDeposits } from "./helpers/event-rows.js";
import {
  admitPending,
  type SimPendingTx,
} from "./helpers/landed-blocks-sim.mempool.js";
import {
  insertOwnJournal,
  simCommitTxHash,
} from "./helpers/landed-blocks-sim.own.js";
import {
  SIM_LEDGER_ADDRESS,
  simDigest,
  simOutput,
} from "./helpers/landed-blocks-sim.universe.js";
import type { OracleDeposit } from "./helpers/working-ledger-oracle.js";
import { hex, run, sqlRun } from "./landed-blocks-rebase.fixture.js";
import { resetApplicationTables } from "./utils.js";

export const AT = Date.parse("2026-10-01T00:00:00.000Z");

/** A deposit of the case, with its database row. */
export type CaseDeposit = OracleDeposit & Readonly<{ row: DepositsDB.Entry }>;

export const depositOf = (label: string, lovelace: bigint): CaseDeposit => {
  const l1Tx = simDigest(`equivalence:${label}:l1`);
  const id = Buffer.from(
    Data.to(
      { transactionId: l1Tx.toString("hex"), outputIndex: 0n },
      SDK.OutputReference,
    ),
    "hex",
  );
  const row: DepositsDB.Entry = {
    [DepositsDB.Columns.ID]: id,
    [DepositsDB.Columns.INFO]: simDigest(`equivalence:${label}:info`),
    [DepositsDB.Columns.INCLUSION_TIME]: new Date(AT),
    [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: l1Tx,
    [DepositsDB.Columns.LEDGER_TX_ID]: simDigest(`equivalence:${label}:ledger`),
    [DepositsDB.Columns.LEDGER_OUTPUT]: simOutput(lovelace),
    [DepositsDB.Columns.LEDGER_ADDRESS]: SIM_LEDGER_ADDRESS,
    [DepositsDB.Columns.PROJECTED_HEADER_HASH]: null,
    [DepositsDB.Columns.STATUS]: DepositsDB.Status.Awaiting,
  };
  const entry = Effect.runSync(DepositsDB.toLedgerEntry(row));
  return {
    outref: Buffer.from(entry.outref),
    output: Buffer.from(entry.output),
    ledgerTxId: Buffer.from(row[DepositsDB.Columns.LEDGER_TX_ID]),
    eventId: id,
    row,
  };
};

/** The deposit's follower admission identity: key and origin outref. */
const admissionOf = (deposit: CaseDeposit) => ({
  key: createHash("sha256").update(deposit.eventId).digest(),
  origin: Buffer.concat([
    deposit.row[DepositsDB.Columns.DEPOSIT_L1_TX_HASH],
    Buffer.alloc(2),
  ]),
});

/** A fresh database: the frontier `frontier` with `confirmed` in both ledgers. */
export const seedCase = async (
  globals: Globals,
  frontier: Readonly<{ headerHash: string; utxosRoot: string }>,
  confirmed: readonly Ledger.MinimalEntry[],
) => {
  await run(globals, resetApplicationTables);
  await sqlRun(globals, (sql) =>
    Effect.gen(function* () {
      const rows = yield* ledgerRows(confirmed, new Map());
      yield* ConfirmedLedgerDB.insertMultiple([...rows]);
      for (const row of rows)
        yield* sql`INSERT INTO mempool_ledger ${sql.insert({
          ...row,
          source_event_id: null,
        } as never)}`;
      yield* Frontier.upsert(frontier);
    }),
  );
};

export const admit = (globals: Globals, txs: readonly SimPendingTx[]) =>
  sqlRun(globals, () => admitPending(txs));

/** The deposit, admitted on L1 and projected outside any block. */
export const projectDeposit = (globals: Globals, deposit: CaseDeposit) =>
  sqlRun(globals, (sql) =>
    Effect.gen(function* () {
      const { key, origin } = admissionOf(deposit);
      yield* insertDeposits([deposit.row]);
      yield* sql`INSERT INTO l1_event_keys
        (kind, key, origin_outref, first_canonical_slot)
        VALUES ('deposit', ${key}, ${origin}, 1)`;
      yield* sql`UPDATE deposits_utxos SET l1_event_key = ${key},
          l1_origin_outref = ${origin}
        WHERE event_id = ${deposit.eventId}`;
      yield* projectDepositsToMempoolLedger([deposit.row]);
    }),
  );

export const landBlock = (globals: Globals, row: LandedBlockRow) =>
  sqlRun(globals, () => processRow(row));

export const rollBack = (globals: Globals, row: LandedBlockRow) =>
  sqlRun(globals, () => rollBackRows([row], []));

export type CaseJournal = Readonly<{
  headerHash: string;
  baseHeaderHash: string;
  baseUtxosRoot: string;
  expectedUtxosRoot: string;
  spent: readonly Buffer[];
  produced: readonly Ledger.MinimalEntry[];
  txIds: readonly Buffer[];
  deposits: readonly CaseDeposit[];
}>;

/** The own journal of a block the node committed and signed. */
export const commitJournal = (globals: Globals, journal: CaseJournal) =>
  sqlRun(globals, (sql) =>
    Effect.gen(function* () {
      yield* insertOwnJournal({ ...journal, at: new Date(AT) });
      for (const [ordinal, deposit] of journal.deposits.entries()) {
        const { key, origin } = admissionOf(deposit);
        const payload = Buffer.concat([
          Buffer.from("equivalence:"),
          deposit.eventId,
        ]);
        yield* sql`INSERT INTO pending_block_finalization_deposits ${sql.insert(
          {
            header_hash: Buffer.from(journal.headerHash, "hex"),
            member_id: deposit.eventId,
            ordinal,
            payload_cbor: payload,
            payload_sha256: createHash("sha256").update(payload).digest(),
            source_table: DepositsDB.tableName,
            source_id: deposit.eventId,
            source_time_stamp_tz: new Date(AT),
            l1_event_key: key,
            l1_origin_outref: origin,
          } as never,
        )}`;
      }
    }),
  );

/** The follower prunes the journal's commit intent: the block is dead. */
export const killJournal = (globals: Globals, headerHash: string) =>
  sqlRun(
    globals,
    (sql) =>
      sql`DELETE FROM l1_intents WHERE tx_hash = ${simCommitTxHash(Buffer.from(headerHash, "hex"))}`,
  );

/** The retention prune: dead journals whose intent is gone, no effect pending. */
export const pruneJournals = (globals: Globals) =>
  run(
    globals,
    testWrite(
      pruneFoldedBeyondChallengeability({
        challengeableCutoff: new Date(Date.now() - 24 * 60 * 60_000),
        view: {
          confirmedHeadHash: Buffer.from("0e".repeat(28), "hex"),
          liveQueueHeaderHashes: [],
        },
        hubOraclePolicyId: "07".repeat(28),
      }),
    ),
  );

/** One rebase on the plan the node reads: its rejections, in order. */
export const rebaseOnce = (globals: Globals) =>
  run(
    globals,
    testWrite(
      Effect.gen(function* () {
        const plan = yield* rebasePlanOf(yield* ownJournalDisposition);
        if (plan.kind !== "ready")
          return yield* Effect.fail(
            new Error(`the rebase is not ready: ${JSON.stringify(plan)}`),
          );
        return yield* rebaseSql(plan.target);
      }),
    ),
  ).then((outcome) => outcome.rejectedTxIds.map(hex));

/** What the node shows: its working ledger, stamps and event statuses. */
export const shownCase = (globals: Globals) =>
  run(
    globals,
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const working = yield* sql<{
        outref: Buffer;
        tx_id: Buffer;
        output: Buffer;
        address: string;
        source_event_id: Buffer | null;
        time_stamp_tz: Date;
      }>`SELECT outref, tx_id, output, address, source_event_id, time_stamp_tz
        FROM mempool_ledger`;
      const deposits = yield* sql<{ event_id: Buffer; status: string }>`
        SELECT event_id, status::text AS status FROM deposits_utxos`;
      const rejections = yield* sql<{ tx_id: Buffer; reject_code: string }>`
        SELECT tx_id, reject_code FROM tx_rejections`;
      const causes = yield* sql<{ tx_id: Buffer; cause_tx_id: Buffer }>`
        SELECT tx_id, cause_tx_id FROM tx_rejection_causes`;
      return {
        rows: new Map(
          working.map((row) => [
            hex(row.outref),
            {
              output: hex(row.output),
              txId: hex(row.tx_id),
              address: row.address,
              source: row.source_event_id && hex(row.source_event_id),
            },
          ]),
        ),
        stamps: new Map(
          working.map((row) => [hex(row.outref), row.time_stamp_tz.getTime()]),
        ),
        deposits: new Map(
          deposits.map((row) => [hex(row.event_id), row.status]),
        ),
        rejections: new Map(
          rejections.map((row) => [hex(row.tx_id), row.reject_code]),
        ),
        causes: causes
          .map((row) => `${hex(row.tx_id)}<-${hex(row.cause_tx_id)}`)
          .sort(),
      };
    }),
  );
