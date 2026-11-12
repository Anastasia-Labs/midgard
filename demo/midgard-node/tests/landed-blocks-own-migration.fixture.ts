/**
 * This node's own landed block and its journal as block confirmation left
 * them before this release, for the migration 0036 suites
 * (`landed-blocks-own-migration*.test.ts`): `landPreA` seeds them,
 * `migrate` applies the migration as the runner does, and `shown` reads the
 * block's row, journal status, local effects and events.
 */
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";
import { afterAll, beforeAll, vi } from "vitest";

import { seedDaPayloadPublicationOutboxFromEnv } from "../src/da/libp2p-producer.js";
import {
  BlocksDB,
  DaPayloadsDB,
  DepositsDB,
  ImmutableDB,
  MempoolDB,
  PendingBlockFinalizationsDB,
  WithdrawalsDB,
} from "../src/database/index.js";
import * as MempoolInclusionsDB from "../src/database/mempoolInclusions.js";
import { MIGRATIONS } from "../src/database/migrations/index.js";
import { splitSqlStatements } from "../src/database/migrations/runner.js";
import { retrieveRows } from "../src/landed-blocks/store.js";
import type { Globals } from "../src/services/globals.js";
import { buildDaPayloadInsert } from "../src/workers/commit-block-header/da-payload.js";
import {
  manifestFixture,
  PRODUCER_PRIVATE_KEY_SOURCE,
} from "./da-payload-libp2p-producer.manifest-fixture.js";
import {
  makeDepositEntry,
  makeHistoryWithdrawalEntry,
} from "./database.test/fixtures.events.js";
import { testWrite } from "./helpers/driver-recompute.js";
import { insertDeposits, insertWithdrawals } from "./helpers/event-rows.js";
import {
  SIM_LEDGER_ADDRESS,
  simOutput,
} from "./helpers/landed-blocks-sim.universe.js";
import { prepareUnderTestLease } from "./helpers/mutation-lease.js";
import { ownJournal } from "./landed-blocks-own-journal.fixture.js";
import {
  E1,
  FRONTIER,
  type Native,
  processOf,
  run,
  seed,
} from "./landed-blocks-rebase.fixture.js";

const Journals = PendingBlockFinalizationsDB;
export const Status = Journals.Status;
const MIGRATION = MIGRATIONS.find((migration) => migration.version === 36)!;
const SETTLEMENT = Buffer.from("d87980", "hex");
const WITHDRAWAL: WithdrawalsDB.Entry = {
  ...makeHistoryWithdrawalEntry(),
  [WithdrawalsDB.Columns.VALIDITY]: WithdrawalsDB.Validity.WithdrawalIsValid,
  [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]: SETTLEMENT,
};
export const WITHDRAWAL_ID = WITHDRAWAL[WithdrawalsDB.Columns.ID];
const DEPOSIT = makeDepositEntry({
  [DepositsDB.Columns.LEDGER_OUTPUT]: simOutput(4_000_000n),
  [DepositsDB.Columns.LEDGER_ADDRESS]: SIM_LEDGER_ADDRESS,
});
export const DEPOSIT_ID = DEPOSIT[DepositsDB.Columns.ID];
/** The deposit's output, which the block produces (built before the tests). */
export let DEPOSITED: { outref: Buffer; output: Buffer };
/** The own block's journal, with one transaction `TX`, `WITHDRAWAL` and
 * `DEPOSIT` (built before the tests). */
let journal: PendingBlockFinalizationsDB.PrepareInput;
export let TX: Awaited<ReturnType<typeof ownJournal>>["tx"];
export let HEADER: Buffer;
export let OWN: string;
export const NEXT = "c2".repeat(28);
const SUBMITTED = Buffer.alloc(32, 7);
export const FORCED = Buffer.alloc(36, 0x6f);
const ADMISSION_KEY = Buffer.alloc(32, 0x5a);
const ADMISSION_ORIGIN = Buffer.concat([
  Buffer.alloc(32, 0x5b),
  Buffer.alloc(2),
]);
const DEPOSIT_KEY = Buffer.alloc(32, 0x5c);
const DEPOSIT_ORIGIN = Buffer.concat([
  DEPOSIT[DepositsDB.Columns.DEPOSIT_L1_TX_HASH],
  Buffer.alloc(2),
]);
/** The follower block the journal's commit is anchored at. */
const ANCHOR = { hash: Buffer.alloc(32, 0xa1), height: 1, slot: 10 };
/** The withdrawal as the journal classified it, as a landed row holds it. */
export const MEMBERSHIP = {
  id: WITHDRAWAL_ID.toString("hex"),
  validity: WithdrawalsDB.Validity.WithdrawalIsValid,
  detail: {},
  settlement: SETTLEMENT.toString("hex"),
};
export const COMMITTEE_PEERS = (
  manifestFixture().da_committee as { members: { roles: string[] }[] }
).members.filter((member) => member.roles.includes("committee")).length;

// The node's DA manifest: applying an own block seeds its payload's
// publication outbox for the manifest's committee.
let manifestDirectory: string;

/** The block's journal built, and the node's DA manifest stubbed in. */
export const useOwnMigration = () => {
  beforeAll(async () => {
    ({ journal, tx: TX } = await ownJournal(WITHDRAWAL, DEPOSIT));
    const deposited = await Effect.runPromise(
      DepositsDB.toLedgerEntry(DEPOSIT),
    );
    DEPOSITED = { outref: deposited.outref, output: deposited.output };
    HEADER = journal.headerHash;
    OWN = HEADER.toString("hex");
    manifestDirectory = await mkdtemp(join(tmpdir(), "landed-own-migration-"));
    const manifestPath = join(manifestDirectory, "manifest.json");
    await writeFile(manifestPath, JSON.stringify(manifestFixture()));
    vi.stubEnv("MIDGARD_DEPLOYMENT_MANIFEST_PATH", manifestPath);
    vi.stubEnv("DA_LIBP2P_PRIVATE_KEY_SOURCE", PRODUCER_PRIVATE_KEY_SOURCE);
  });
  afterAll(async () => {
    vi.unstubAllEnvs();
    await rm(manifestDirectory, { recursive: true, force: true });
  });
};

/** The block's DA payload stored, and its outbox seeded if `outbox`. */
const storeDaPayload = (outbox: boolean) =>
  Effect.gen(function* () {
    const record = Option.getOrThrow(
      yield* Journals.retrieveByHeaderHash(HEADER),
    );
    const payload = yield* buildDaPayloadInsert({ record });
    yield* DaPayloadsDB.upsertAvailable(payload);
    if (outbox) yield* seedDaPayloadPublicationOutboxFromEnv(payload);
  });

type PreA = Readonly<{
  status: (typeof Status)[keyof typeof Status];
  withdrawal: "awaiting" | "projected" | "finalized";
  /** Block confirmation's local effects already stored: all of them, or
   * those of the old local finalization's SQL transaction only. */
  stored?: "all" | "sql";
  /** A forced transaction the block included, `finalized` once the
   * journal is locally applied, `projected` to the block before. It is
   * projected here, outside the journal fixture, which models no forced
   * members: the journal's `forcedIds` stay empty, and the fold finalizes it
   * as an event projected to the block. */
  forced?: boolean;
  /** A rollback took the block off the landed chain: its row is removed
   * (its inclusion marks cleared), its commit anchored on the follower's
   * chain and built on the frontier. */
  removed?: boolean;
}>;

/**
 * The own block `OWN` (the seeded block's place, spending E0 for E1 and
 * producing `DEPOSIT`'s output) and its journal, as block confirmation left
 * them before this release: `status` the journal's, the row applied unless
 * the journal was abandoned, holding its withdrawal only then, its
 * transaction in the mempool marked by the block. The deposit is projected
 * to the block as the withdrawal is.
 */
export const landPreA = async (native: Native, preA: PreA) => {
  const globals = await processOf(native);
  const abandoned = preA.status === Status.Abandoned;
  const removed = preA.removed === true;
  await seed(globals, {
    headerHash: OWN,
    kind: "own",
    state: removed ? "removed" : "processed",
    applied: !abandoned,
    produced: [E1, DEPOSITED],
    depositIds: [DEPOSIT_ID],
    withdrawals: abandoned ? [MEMBERSHIP] : [],
    forcedIds: preA.forced === true ? [FORCED] : [],
    txIds: [TX.id],
  });
  await run(
    globals,
    testWrite(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* insertWithdrawals([WITHDRAWAL]);
        // Backed by its canonical admission, as the follower ingests it.
        yield* sql`INSERT INTO l1_event_keys
          (kind, key, origin_outref, first_canonical_slot)
          VALUES ('withdrawal', ${ADMISSION_KEY}, ${ADMISSION_ORIGIN}, 1)`;
        yield* sql`UPDATE withdrawal_utxos SET l1_event_key = ${ADMISSION_KEY},
            l1_origin_outref = ${ADMISSION_ORIGIN}
          WHERE event_id = ${WITHDRAWAL_ID}`;
        yield* insertDeposits([DEPOSIT]);
        yield* sql`INSERT INTO l1_event_keys
          (kind, key, origin_outref, first_canonical_slot)
          VALUES ('deposit', ${DEPOSIT_KEY}, ${DEPOSIT_ORIGIN}, 1)`;
        yield* sql`UPDATE deposits_utxos SET l1_event_key = ${DEPOSIT_KEY},
            l1_origin_outref = ${DEPOSIT_ORIGIN}
          WHERE event_id = ${DEPOSIT_ID}`;
        yield* prepareUnderTestLease(journal);
        yield* MempoolDB.restoreJournalEntries(journal.mempoolTxs);
        // The rollback cleared the block's marks; the old local
        // finalization's SQL transaction, committed after it, set them.
        if (!removed || preA.stored === "sql")
          yield* MempoolInclusionsDB.markIncluded(OWN, [TX.id]);
        if (removed) {
          yield* sql`INSERT INTO l1_blocks
            (slot, hash, height, parent_hash, qualifying_tx_count)
            VALUES (${ANCHOR.slot}, ${ANCHOR.hash}, ${ANCHOR.height}, NULL, 0)`;
          yield* sql`UPDATE pending_block_finalizations
            SET commit_anchor_hash = ${ANCHOR.hash},
              commit_anchor_height = ${ANCHOR.height},
              commit_anchor_slot = ${ANCHOR.slot},
              base_tail_header_hash = ${Buffer.from(FRONTIER, "hex")}
            WHERE header_hash = ${HEADER}`;
        }
        if (abandoned) yield* Journals.markUnsubmittedAbandoned(HEADER);
        else if (preA.status !== Status.PendingSubmission)
          yield* sql`UPDATE pending_block_finalizations
            SET status = ${preA.status}, submitted_tx_hash = ${SUBMITTED}
            WHERE header_hash = ${HEADER}`;
        if (preA.withdrawal !== "awaiting") {
          yield* sql`UPDATE withdrawal_utxos SET status = ${preA.withdrawal},
            projected_header_hash = ${HEADER} WHERE event_id = ${WITHDRAWAL_ID}`;
          yield* sql`UPDATE deposits_utxos SET status = 'projected',
            projected_header_hash = ${HEADER} WHERE event_id = ${DEPOSIT_ID}`;
        }
        if (preA.forced === true)
          yield* sql`INSERT INTO forced_transaction_utxos
            (tx_order_id, tx_order_l1_tx_hash, tx_order_l1_output_index,
             asset_name, raw_datum, tx_id, tx_compact, forced_inclusion_value,
             consensus_profile_id, native_tx_cbor, transaction_commitment,
             inclusion_time, projected_header_hash, status)
            VALUES (${FORCED}, ${Buffer.alloc(32, 1)}, 0, ${Buffer.alloc(1)},
              ${Buffer.alloc(1)}, ${Buffer.alloc(32, 2)}, ${Buffer.alloc(1)},
              ${Buffer.alloc(1)}, 'midgard-consensus-v1', ${Buffer.alloc(1)},
              ${Buffer.alloc(32, 3)}, NOW(), ${HEADER},
              ${preA.status === Status.LocallyApplied ? "finalized" : "projected"})`;
        if (preA.stored !== undefined) {
          yield* ImmutableDB.insertTxsValidatedNative([
            { tx_id: TX.id, tx: TX.cbor },
          ]);
          yield* BlocksDB.insert(HEADER, [TX.id]);
          yield* storeDaPayload(preA.stored === "all");
        }
      }),
    ),
  );
  return globals;
};

/** Migration 0036, as the runner applies it. */
export const migrate = (globals: Globals) =>
  run(
    globals,
    Effect.flatMap(SqlClient.SqlClient, (sql) =>
      Effect.forEach(splitSqlStatements(MIGRATION.sql), (statement) =>
        sql.unsafe(statement),
      ),
    ),
  );

/** The block's row, journal status and local effects, and its events. */
export const shown = (globals: Globals) =>
  run(
    globals,
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* retrieveRows;
      const record = yield* Journals.retrieveByHeaderHash(HEADER);
      const withdrawal = Option.getOrThrow(
        yield* WithdrawalsDB.retrieveByEventId(WITHDRAWAL_ID),
      );
      const [deposit] = yield* sql<{
        status: string;
        projected_header_hash: Buffer | null;
      }>`SELECT status, projected_header_hash FROM deposits_utxos
        WHERE event_id = ${DEPOSIT_ID}`;
      const pending = yield* sql<{ included_by: Buffer | null }>`
        SELECT included_by FROM mempool WHERE tx_id = ${TX.id}`;
      const immutable = yield* sql<{ tx: Buffer }>`
        SELECT tx FROM immutable WHERE tx_id = ${TX.id}`;
      const blocks = yield* sql<{ header_hash: Buffer }>`
        SELECT header_hash FROM blocks WHERE tx_id = ${TX.id}`;
      const forced = yield* sql<{ status: string }>`
        SELECT status FROM forced_transaction_utxos WHERE tx_order_id = ${FORCED}`;
      const [outbox] = yield* sql<{ peers: number; announcements: number }>`
        SELECT
          (SELECT COUNT(*)::int FROM da_payload_publications
            WHERE header_hash = ${HEADER}) AS peers,
          (SELECT COUNT(*)::int FROM da_payload_announcements
            WHERE header_hash = ${HEADER}) AS announcements`;
      return {
        row: rows.find((row) => row.headerHash === OWN),
        journal: Option.map(record, (value) => value[Journals.Columns.STATUS]),
        immutable: immutable.map((row) => row.tx.toString("hex")),
        blocks: blocks.map((row) => row.header_hash.toString("hex")),
        da: Option.isSome(yield* DaPayloadsDB.retrieveByHeaderHash(HEADER)),
        outbox,
        withdrawal: {
          status: withdrawal[WithdrawalsDB.Columns.STATUS],
          header:
            withdrawal[WithdrawalsDB.Columns.PROJECTED_HEADER_HASH]?.toString(
              "hex",
            ) ?? null,
        },
        forced: forced.map((row) => row.status),
        deposit: {
          status: deposit!.status,
          header: deposit!.projected_header_hash?.toString("hex") ?? null,
        },
        pending: pending.map((row) => row.included_by?.toString("hex") ?? null),
      };
    }),
  );

/** What the rebase leaves for a block it applied. */
export const appliedOnce = () => ({
  journal: Option.some(Status.LocallyApplied),
  immutable: [TX.cbor.toString("hex")],
  blocks: [OWN],
  da: true,
  outbox: { peers: COMMITTEE_PEERS, announcements: 1 },
  withdrawal: { status: WithdrawalsDB.Status.Projected, header: OWN },
  forced: [],
  deposit: { status: DepositsDB.Status.Projected, header: OWN },
  pending: [OWN],
});
