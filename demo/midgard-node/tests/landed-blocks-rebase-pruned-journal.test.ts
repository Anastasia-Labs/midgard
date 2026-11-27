/**
 * A deposit-only live own block that dies with no rebase due, its journal
 * pruned (`pruneFoldedBeyondChallengeability`, a dead journal whose intent
 * is gone with no effect pending) before the next rebase runs: the deposit
 * the block carried stays in the working ledger, projected outside any
 * block, and the next block carries it again.
 */
import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk/core";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it } from "vitest";

import { DepositsDB } from "../src/database/index.js";
import { pruneFoldedBeyondChallengeability } from "../src/database/pendingBlockFinalizations.retrieve-applied-missing-da-payloads.js";
import { rebasePlan } from "../src/landed-blocks/rebase-target.js";
import type { Globals } from "../src/services/globals.globals.js";
import { projectDepositsToMempoolLedger } from "./helpers/deposit-projection.js";
import { testWrite } from "./helpers/driver-recompute.js";
import { insertDeposits } from "./helpers/event-rows.js";
import {
  insertOwnJournal,
  simCommitTxHash,
} from "./helpers/landed-blocks-sim.own.js";
import {
  SIM_LEDGER_ADDRESS,
  simDigest,
  simOutput,
} from "./helpers/landed-blocks-sim.universe.js";
import {
  attempt,
  BLOCK,
  E1,
  entry,
  expectRebased,
  freshNative,
  hex,
  land,
  nativeOwner,
  processOf,
  R0,
  R1,
  root,
  run,
  seed,
  sqlRun,
} from "./landed-blocks-rebase.fixture.js";

const OWN = "0d".repeat(28);
const NEXT = "b2".repeat(28);
const R2 = root(0x12);
const R3 = root(0x13);
const E2 = entry("e2", 4_000_000n);
const AT = new Date("2026-10-01T00:00:00.000Z");

const DEPOSIT_L1_TX = simDigest("pruned-journal:deposit-l1");
const DEPOSIT_ID = Buffer.from(
  Data.to(
    { transactionId: DEPOSIT_L1_TX.toString("hex"), outputIndex: 0n },
    SDK.OutputReference,
  ),
  "hex",
);
const DEPOSIT: DepositsDB.Entry = {
  [DepositsDB.Columns.ID]: DEPOSIT_ID,
  [DepositsDB.Columns.INFO]: simDigest("pruned-journal:deposit-info"),
  [DepositsDB.Columns.INCLUSION_TIME]: AT,
  [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: DEPOSIT_L1_TX,
  [DepositsDB.Columns.LEDGER_TX_ID]: simDigest("pruned-journal:deposit-ledger"),
  [DepositsDB.Columns.LEDGER_OUTPUT]: simOutput(7_000_000n),
  [DepositsDB.Columns.LEDGER_ADDRESS]: SIM_LEDGER_ADDRESS,
  [DepositsDB.Columns.PROJECTED_HEADER_HASH]: null,
  [DepositsDB.Columns.STATUS]: DepositsDB.Status.Awaiting,
};
const D = Effect.runSync(DepositsDB.toLedgerEntry(DEPOSIT));
const ADMISSION_KEY = Buffer.alloc(32, 0x6a);
const ADMISSION_ORIGIN = Buffer.concat([DEPOSIT_L1_TX, Buffer.alloc(2)]);

/** The deposit, admitted on L1 and projected into the working ledger outside any block. */
const admitDeposit = (globals: Globals) =>
  sqlRun(globals, (sql) =>
    Effect.gen(function* () {
      yield* insertDeposits([DEPOSIT]);
      yield* sql`INSERT INTO l1_event_keys
        (kind, key, origin_outref, first_canonical_slot)
        VALUES ('deposit', ${ADMISSION_KEY}, ${ADMISSION_ORIGIN}, 1)`;
      yield* sql`UPDATE deposits_utxos SET l1_event_key = ${ADMISSION_KEY},
          l1_origin_outref = ${ADMISSION_ORIGIN}
        WHERE event_id = ${DEPOSIT_ID}`;
      yield* projectDepositsToMempoolLedger([DEPOSIT]);
    }),
  );

/** The live own block on `BLOCK`: it carries the deposit and nothing else. */
const commitOwn = (globals: Globals) =>
  sqlRun(globals, (sql) =>
    Effect.gen(function* () {
      yield* insertOwnJournal({
        headerHash: OWN,
        baseHeaderHash: BLOCK,
        baseUtxosRoot: R1,
        expectedUtxosRoot: R2,
        spent: [],
        produced: [{ outref: D.outref, output: D.output }],
        txIds: [],
        at: AT,
      });
      const payload = Buffer.concat([
        Buffer.from("pruned-journal:"),
        DEPOSIT_ID,
      ]);
      yield* sql`INSERT INTO pending_block_finalization_deposits ${sql.insert({
        header_hash: Buffer.from(OWN, "hex"),
        member_id: DEPOSIT_ID,
        ordinal: 0,
        payload_cbor: payload,
        payload_sha256: createHash("sha256").update(payload).digest(),
        source_table: DepositsDB.tableName,
        source_id: DEPOSIT_ID,
        source_time_stamp_tz: AT,
        l1_event_key: ADMISSION_KEY,
        l1_origin_outref: ADMISSION_ORIGIN,
      } as never)}`;
    }),
  );

/** The follower prunes the own block's commit intent: the block is dead. */
const killOwn = (globals: Globals) =>
  sqlRun(
    globals,
    (sql) =>
      sql`DELETE FROM l1_intents WHERE tx_hash = ${simCommitTxHash(Buffer.from(OWN, "hex"))}`,
  );

const prune = (globals: Globals) =>
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

/** The deposit's working-ledger row and status, and the own journal count. */
const shown = (globals: Globals) =>
  run(
    globals,
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{
        output: Buffer;
        source_event_id: Buffer | null;
      }>`
        SELECT output, source_event_id FROM mempool_ledger WHERE outref = ${D.outref}`;
      const [deposit] = yield* sql<{ status: string; header: Buffer | null }>`
        SELECT status::text AS status, projected_header_hash AS header
        FROM deposits_utxos WHERE event_id = ${DEPOSIT_ID}`;
      const [journals] = yield* sql<{ count: number }>`
        SELECT COUNT(*)::int AS count FROM pending_block_finalizations
        WHERE header_hash = ${Buffer.from(OWN, "hex")}`;
      return {
        row: rows.map((row) => ({
          output: hex(row.output),
          source: row.source_event_id && hex(row.source_event_id),
        })),
        deposit: deposit && {
          status: deposit.status,
          header: deposit.header && hex(deposit.header),
        },
        journals: journals?.count,
      };
    }),
  );

const projectedOutside = {
  row: [{ output: hex(D.output), source: hex(DEPOSIT_ID) }],
  deposit: { status: "projected", header: null },
};

it("keeps a dead deposit-only own block's deposit in the working ledger after its journal is pruned and the next block is rebased", async () => {
  const native = {
    ...freshNative(),
    next: { [R0]: R1, [R1]: R3 },
  };
  native.ledgers[R3] = [E2];
  const globals = await processOf(native);
  await seed(globals);
  expectRebased(await attempt(globals));

  await admitDeposit(globals);
  await commitOwn(globals);
  expect(await shown(globals)).toEqual({ ...projectedOutside, journals: 1 });

  // It dies before the native MPF took its root: no rebase is due.
  await killOwn(globals);
  expect((await run(globals, rebasePlan(nativeOwner(native)))).kind).toBe(
    "none",
  );
  expect(await prune(globals)).toBe(1);
  expect(await shown(globals)).toEqual({ ...projectedOutside, journals: 0 });

  // The next processed block, on BLOCK, is rebased onto.
  await land(globals, {
    headerHash: NEXT,
    parentHeaderHash: BLOCK,
    parentUtxosRoot: R1,
    utxosRoot: R3,
    spent: [E1.outref],
    produced: [E2],
  });
  const after = await attempt(globals);
  expect(after.hold).toBeUndefined();
  expect(after.due).toBe("none");
  expect(after.applied).toEqual([true, true]);
  expect(native.durableRoot).toBe(R3);
  expect(after.working.sort()).toEqual([hex(E2.outref), hex(D.outref)].sort());
  expect(await shown(globals)).toEqual({ ...projectedOutside, journals: 0 });
}, 60_000);
