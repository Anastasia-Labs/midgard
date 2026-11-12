/**
 * This node's own landed block, applied by the working-ledger rebase (the
 * follower-change driver's `rebaseIfDue`, with the modelled native MPF
 * owner of `landed-blocks-rebase.fixture.ts`) as a foreign block is:
 *
 * - before the rebase the processed row is unapplied, its journal pending,
 *   its transaction in neither `immutable` nor `blocks`, no DA payload
 *   stored and its withdrawal unassigned; the rebase moves the working
 *   ledger and the native root onto it, writes its transaction to
 *   `immutable` and `blocks` once, its DA payload and that payload's
 *   publication outbox, and marks its journal locally applied, and
 *   applying it again writes nothing new;
 * - its withdrawal is `projected` to the block from that rebase, as a
 *   foreign block's is, so it is not yet payout-eligible and refuses a new
 *   classification, and `finalized` only once the block folds;
 * - a rebase that fails after the block's transaction is written leaves
 *   nothing of it, and its retry applies the block as one clean rebase;
 * - a block whose journal was abandoned before it landed is revived and
 *   applied by the same rebase;
 * - a merge of it on L1 while the rebase is held defers its fold, and folds
 *   it once the rebase has applied it.
 */
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import type { View } from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { Effect, Either, Option } from "effect";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import {
  DaPayloadsDB,
  PendingBlockFinalizationsDB,
  WithdrawalsDB,
} from "../src/database/index.js";
import { setSettlementInfoForEventIds } from "../src/database/withdrawals.set-settlement-info-for-event-ids.js";
import { foldToRoot } from "../src/landed-blocks/fold.js";
import { applyOwnBlocks } from "../src/landed-blocks/own-blocks.js";
import type { LandedBlockPorts } from "../src/landed-blocks/ports.js";
import {
  Frontier,
  type LandedBlockRow,
  retrieveRows,
} from "../src/landed-blocks/store.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import type { Globals } from "../src/services/globals.js";
import { MPF_CLOSURE_MISSING } from "../src/services/liveness-halt.js";
import { finalizeConfirmedMergeTransaction } from "../src/transactions/state-queue/merge-to-confirmed-state.finalize-confirmed-merge-program.js";
import {
  manifestFixture,
  PRODUCER_PRIVATE_KEY_SOURCE,
} from "./da-payload-libp2p-producer.manifest-fixture.js";
import { makeHistoryWithdrawalEntry } from "./database.test/fixtures.events.js";
import { testWrite } from "./helpers/driver-recompute.js";
import { insertWithdrawals } from "./helpers/event-rows.js";
import { prepareUnderTestLease } from "./helpers/mutation-lease.js";
import { ownJournal } from "./landed-blocks-own-journal.fixture.js";
import {
  attempt,
  E0,
  E1,
  expectHeld,
  expectRebased,
  freshNative,
  type Native,
  processOf,
  R1,
  root,
  run,
  seed,
  sqlRun,
} from "./landed-blocks-rebase.fixture.js";

const Journals = PendingBlockFinalizationsDB;
/** The own block's journal, with one transaction `TX` (built before the tests). */
let journal: PendingBlockFinalizationsDB.PrepareInput;
let TX: Awaited<ReturnType<typeof ownJournal>>["tx"];
let HEADER: Buffer;
let OWN: string;
const WITHDRAWAL = makeHistoryWithdrawalEntry();
const WITHDRAWAL_ID = WITHDRAWAL[WithdrawalsDB.Columns.ID];
const SETTLEMENT = Buffer.from("d87980", "hex");
const COMMITTEE_PEERS = (
  manifestFixture().da_committee as { members: { roles: string[] }[] }
).members.filter((member) => member.roles.includes("committee")).length;
const ADMISSION_KEY = Buffer.alloc(32, 0x5a);
const ADMISSION_ORIGIN = Buffer.concat([
  Buffer.alloc(32, 0x5b),
  Buffer.alloc(2),
]);

// The node's DA manifest: applying an own block seeds its payload's
// publication outbox for the manifest's committee.
let manifestDirectory: string;
beforeAll(async () => {
  ({ journal, tx: TX } = await ownJournal());
  HEADER = journal.headerHash;
  OWN = HEADER.toString("hex");
  manifestDirectory = await mkdtemp(join(tmpdir(), "landed-own-da-"));
  const manifestPath = join(manifestDirectory, "manifest.json");
  await writeFile(manifestPath, JSON.stringify(manifestFixture()));
  vi.stubEnv("MIDGARD_DEPLOYMENT_MANIFEST_PATH", manifestPath);
  vi.stubEnv("DA_LIBP2P_PRIVATE_KEY_SOURCE", PRODUCER_PRIVATE_KEY_SOURCE);
});
afterAll(async () => {
  vi.unstubAllEnvs();
  await rm(manifestDirectory, { recursive: true, force: true });
});

/** The production fold's ports with the view held. */
const foldPorts = {
  confirmView: () => Effect.succeed(true),
  write: <A, E, R>(work: Effect.Effect<A, E, R>) => withFollowerWrite(work),
} as unknown as LandedBlockPorts<never>;

/**
 * The processed own block `OWN` (the seeded block's place, spending E0 for
 * E1, withdrawing `WITHDRAWAL`) and its journal `own`, with the landed
 * row's fields `row`; `abandoned` abandons the journal before the block lands.
 */
const landOwn = async (
  native: Native,
  {
    abandoned = false,
    own = journal,
    row = {},
  }: {
    readonly abandoned?: boolean;
    readonly own?: PendingBlockFinalizationsDB.PrepareInput;
    readonly row?: Partial<LandedBlockRow>;
  } = {},
) => {
  const globals = await processOf(native);
  await seed(globals, {
    ...row,
    headerHash: OWN,
    kind: "own",
    applied: false,
    txIds: [TX.id],
    withdrawals: [
      {
        id: WITHDRAWAL_ID.toString("hex"),
        validity: WithdrawalsDB.Validity.WithdrawalIsValid,
        detail: null,
        settlement: SETTLEMENT.toString("hex"),
      },
    ],
  });
  await run(
    globals,
    testWrite(
      Effect.gen(function* () {
        yield* insertWithdrawals([WITHDRAWAL]);
        // Backed by its canonical admission, as the follower ingests it.
        const sql = yield* SqlClient.SqlClient;
        yield* sql`INSERT INTO l1_event_keys
          (kind, key, origin_outref, first_canonical_slot)
          VALUES ('withdrawal', ${ADMISSION_KEY}, ${ADMISSION_ORIGIN}, 1)`;
        yield* sql`UPDATE withdrawal_utxos SET l1_event_key = ${ADMISSION_KEY},
            l1_origin_outref = ${ADMISSION_ORIGIN}
          WHERE event_id = ${WITHDRAWAL_ID}`;
        yield* prepareUnderTestLease(own);
        if (abandoned) yield* Journals.markUnsubmittedAbandoned(HEADER);
      }),
    ),
  );
  return globals;
};

/**
 * The block's journal status, its transaction's `immutable` and `blocks`
 * rows, DA payload and withdrawal as stored.
 */
const shownOwn = (globals: Globals) =>
  run(
    globals,
    Effect.gen(function* () {
      const journal = yield* Journals.retrieveByHeaderHash(HEADER);
      const withdrawal = Option.getOrThrow(
        yield* WithdrawalsDB.retrieveByEventId(WITHDRAWAL_ID),
      );
      const sql = yield* SqlClient.SqlClient;
      const immutable = yield* sql<{ tx: Buffer }>`
        SELECT tx FROM immutable WHERE tx_id = ${TX.id}`;
      const blocks = yield* sql<{ header_hash: Buffer }>`
        SELECT header_hash FROM blocks WHERE tx_id = ${TX.id}`;
      const [outbox] = yield* sql<{ peers: number; announcements: number }>`
        SELECT
          (SELECT COUNT(*)::int FROM da_payload_publications
            WHERE header_hash = ${HEADER}) AS peers,
          (SELECT COUNT(*)::int FROM da_payload_announcements
            WHERE header_hash = ${HEADER}) AS announcements`;
      return {
        journal: Option.map(
          journal,
          (record) => record[Journals.Columns.STATUS],
        ),
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
      };
    }),
  );

/** What one clean application of the block stores. */
const appliedOwn = () => ({
  journal: Option.some(Journals.Status.LocallyApplied),
  immutable: [TX.cbor.toString("hex")],
  blocks: [OWN],
  da: true,
  outbox: { peers: COMMITTEE_PEERS, announcements: 1 },
  withdrawal: { status: WithdrawalsDB.Status.Projected, header: OWN },
});

/**
 * A classification of the withdrawal at its current revision, as the
 * follower writes one.
 */
const reclassify = (globals: Globals) =>
  run(
    globals,
    testWrite(
      Effect.gen(function* () {
        const current = Option.getOrThrow(
          yield* WithdrawalsDB.retrieveByEventId(WITHDRAWAL_ID),
        );
        return yield* Effect.either(
          setSettlementInfoForEventIds([
            {
              eventId: WITHDRAWAL_ID,
              settlementEventInfo: SETTLEMENT,
              validity: WithdrawalsDB.Validity.WithdrawalIsValid,
              expectedClassificationRevision:
                current[WithdrawalsDB.Columns.CLASSIFICATION_REVISION],
            },
          ]),
        );
      }),
    ),
  );

describe(
  "an own landed block is applied by the rebase",
  { concurrent: false },
  () => {
    it("writes nothing before the rebase, applies the block at it, and nothing again after", async () => {
      const native = freshNative();
      const globals = await landOwn(native);
      expect(
        (await run(globals, retrieveRows)).map((row) => row.applied),
      ).toEqual([false]);
      expect(await shownOwn(globals)).toEqual({
        journal: Option.some(Journals.Status.PendingSubmission),
        immutable: [],
        blocks: [],
        da: false,
        outbox: { peers: 0, announcements: 0 },
        withdrawal: { status: WithdrawalsDB.Status.Awaiting, header: null },
      });

      expectRebased(await attempt(globals));
      expect(native.durableRoot).toBe(R1);
      const applied = await shownOwn(globals);
      expect(applied).toEqual(appliedOwn());

      // Applying the block again writes nothing new.
      const [row] = await run(globals, retrieveRows);
      await run(
        globals,
        testWrite(applyOwnBlocks([{ ...row!, applied: false }])),
      );
      expectRebased(await attempt(globals));
      expect(await shownOwn(globals)).toEqual(applied);
    });

    it("leaves nothing of a rebase that fails after the block's transaction is written, and applies it once at the retry", async () => {
      const globals = await landOwn(freshNative());
      const landed = await shownOwn(globals);
      // The DA payload write, after the `immutable` and `blocks` writes and
      // before the outbox and the journal's mark, fails.
      await sqlRun(globals, (sql) =>
        sql.unsafe(`CREATE FUNCTION refuse_da_payload() RETURNS trigger
          LANGUAGE plpgsql AS $refuse$
          BEGIN RAISE EXCEPTION 'injected DA payload write failure'; END
          $refuse$`),
      );
      await sqlRun(
        globals,
        (sql) => sql`CREATE TRIGGER refuse_da_payload BEFORE INSERT
          ON da_payloads FOR EACH ROW EXECUTE FUNCTION refuse_da_payload()`,
      );
      try {
        expectHeld(await attempt(globals), /injected DA payload write failure/);
      } finally {
        await sqlRun(
          globals,
          (sql) => sql`DROP TRIGGER refuse_da_payload ON da_payloads`,
        );
        await sqlRun(globals, (sql) =>
          sql.unsafe(`DROP FUNCTION refuse_da_payload()`),
        );
      }
      expect(await shownOwn(globals)).toEqual(landed);

      expectRebased(await attempt(globals));
      expect(await shownOwn(globals)).toEqual(appliedOwn());
    });

    it("leaves its withdrawal projected, not payout-eligible, until the block folds, then finalized", async () => {
      const globals = await landOwn(freshNative());
      // Unassigned before the rebase: a classification is taken.
      expect(Either.isRight(await reclassify(globals))).toBe(true);
      expectRebased(await attempt(globals));
      const projected = await shownOwn(globals);
      expect(projected.withdrawal).toEqual({
        status: WithdrawalsDB.Status.Projected,
        header: OWN,
      });
      expect(projected.withdrawal.status).not.toBe(
        WithdrawalsDB.Status.Finalized,
      );
      // Assigned to the landed block: no new classification is taken.
      const refused = await reclassify(globals);
      expect(Either.isLeft(refused) ? refused.left.message : "").toMatch(
        /classification of an assigned withdrawal/,
      );

      await run(
        globals,
        testWrite(
          foldToRoot(
            foldPorts,
            {} as View,
            { headerHash: OWN, utxosRoot: R1 },
            () => Effect.succeed(null),
          ),
        ),
      );
      expect(await run(globals, retrieveRows)).toEqual([]);
      expect((await shownOwn(globals)).withdrawal).toEqual({
        status: WithdrawalsDB.Status.Finalized,
        header: OWN,
      });
    });

    it("revives a block whose journal was abandoned before it landed and applies it at the same rebase", async () => {
      const native = freshNative();
      const globals = await landOwn(native, { abandoned: true });
      expect((await shownOwn(globals)).journal).toEqual(
        Option.some(Journals.Status.Abandoned),
      );
      expectRebased(await attempt(globals));
      expect(native.durableRoot).toBe(R1);
      expect(await shownOwn(globals)).toMatchObject({
        journal: Option.some(Journals.Status.LocallyApplied),
        da: true,
        outbox: { peers: COMMITTEE_PEERS, announcements: 1 },
        withdrawal: { status: WithdrawalsDB.Status.Projected, header: OWN },
      });
    });

    it("defers the fold of a merge of it while the rebase is held, and folds it once the rebase applies it", async () => {
      // The block on its journal's base and root, its journal's delta the
      // landed row's.
      const base = {
        headerHash: journal.metadata.baseTailHeaderHash.toString("hex"),
        utxosRoot: journal.metadata.baseRoots.utxosRoot,
      };
      const reached = journal.metadata.expectedRoots.utxosRoot;
      const native: Native = {
        ...freshNative(),
        durableRoot: root(0x99),
        retainsNothing: true,
      };
      const globals = await landOwn(native, {
        own: {
          ...journal,
          ledgerDelta: { spent: [E0.outref], produced: [E1] },
        },
        row: {
          parentHeaderHash: base.headerHash,
          parentUtxosRoot: base.utxosRoot,
          utxosRoot: reached,
        },
      });
      await run(globals, testWrite(Frontier.upsert(base)));
      expectHeld(
        await attempt(globals),
        /retains no root of the processed landed chain/,
        MPF_CLOSURE_MISSING,
      );
      // The merge fiber's step, on the journal as it reads it.
      const merge = () =>
        run(
          globals,
          testWrite(
            Effect.flatMap(Journals.retrieveByHeaderHash(HEADER), (found) =>
              finalizeConfirmedMergeTransaction({
                journal: Option.getOrThrow(found),
              }),
            ),
          ),
        );
      const folded = () =>
        run(
          globals,
          Effect.all({
            rows: Effect.map(retrieveRows, (rows) =>
              rows.map((row) => [row.headerHash, row.applied]),
            ),
            frontier: Frontier.retrieve,
          }),
        );

      expect(await merge()).toBe("deferred");
      expect(await folded()).toEqual({ rows: [[OWN, false]], frontier: base });

      native.retainsNothing = false;
      expectRebased(await attempt(globals));
      expect(await merge()).toBe("folded");
      expect(await folded()).toEqual({
        rows: [],
        frontier: { headerHash: OWN, utxosRoot: reached },
      });
    });
  },
);
