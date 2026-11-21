/**
 * Migration 0037 on a populated node database at version 36, with and
 * without the follower's tables (installed after the node migrations): each
 * run rebuilds the schema through 36 in a transaction it rolls back, so the
 * shard keeps its schema.
 *
 * - It carries over a locally applied journal a processed landed row
 *   applied, or a rollback removed, or the state queue merged; an abandoned
 *   signed journal with no intent row; an active signed journal the intent
 *   journal holds with its exact bytes, or whose intent row the follower
 *   pruned (dead once derived, or applied if its block landed, and kept by
 *   the commit tick's unsigned-journal discard); and an unfinished local
 *   finalization of a submitted journal. It deletes an abandoned journal
 *   never signed, and drops the status, its columns and the job table.
 * - A journal whose commit was submitted unjournaled (a submitted hash and
 *   no intended one) carries over signed under its submitted hash, whatever
 *   its status: an abandoned one is a dead journal, pruned once its effects
 *   clear; a locally applied one stays applied or folded, and the commit
 *   tick's unsigned-journal discard leaves it.
 * - It refuses, leaving every row as it was, with
 *   `MIGRATION_0037_SIGNED_JOURNAL_WITHOUT_INTENT` a submitted hash with no
 *   intended one, an active signed journal the intent journal holds with
 *   other bytes, and one with no follower tables to hold it; with
 *   `MIGRATION_0037_UNRECOVERABLE_LOCAL_FINALIZATION` an unfinished job of a
 *   journal never submitted; with `MIGRATION_0037_APPLIED_JOURNAL_UNDERIVABLE`
 *   a locally applied journal nothing applied or folded; with
 *   `MIGRATION_0037_SUBMITTED_COMMIT_NOT_PREPARED` a submitted hash with no
 *   intended one that is not the prepared hash.
 */
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";
import { describe, expect, it } from "vitest";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import { pruneFoldedBeyondChallengeability } from "../src/database/pendingBlockFinalizations.retrieve-applied-missing-da-payloads.js";
import { ownBlockStatusSql } from "../src/database/pendingBlockFinalizations.status-sql.js";
import { discardStoppedUnsignedJournals } from "../src/fibers/block-commitment.discard-unsigned-journals.js";
import { simCommitTxHash } from "./helpers/landed-blocks-sim.own.js";
import {
  anchorAndBaseOnChain,
  bytes,
  header,
  insertIntent,
  insertJob,
  insertJournal,
  land,
  migrate,
  migrateCommitted,
} from "./helpers/own-block-status-migration.js";
import { insertQueueTerminal } from "./helpers/queue-terminal-rows.js";

const APPLIED = header(0x01);
const REMOVED = header(0x02);
const MERGED = header(0x03);
const DEAD_SIGNED = header(0x04);
const NEVER_SIGNED = header(0x05);
const SUBMITTED = header(0x06);
const FINALIZING = header(0x07);
const SUBMITTED_UNJOURNALED = header(0x08);

/** A locally applied journal whose commit was submitted unjournaled. */
const UNJOURNALED_APPLIED = {
  status: "locally_applied",
  submitted: "unsigned",
} as const;
/** The submitted hash `insertJournal` gives an unjournaled commit at `at`. */
const submittedAt = (at: number) => Buffer.alloc(32, at).toString("hex");
const commitOf = (headerHash: string) =>
  simCommitTxHash(bytes(headerHash)).toString("hex");

describe("migration 0037 on a populated node database", () => {
  it.each([false, true])(
    "carries over the journals it can derive, with the follower's tables %s",
    async (follower) => {
      const shown = await migrate(
        follower,
        Effect.gen(function* () {
          yield* insertJournal(APPLIED, UNJOURNALED_APPLIED, 1);
          yield* land(APPLIED, "processed");
          yield* insertJournal(REMOVED, UNJOURNALED_APPLIED, 2);
          yield* land(REMOVED, "removed");
          yield* insertJournal(
            DEAD_SIGNED,
            { status: "abandoned", signed: true },
            4,
          );
          yield* insertJournal(NEVER_SIGNED, { status: "abandoned" }, 5);
          yield* insertJournal(
            FINALIZING,
            { status: "submitted_local_finalization_pending" },
            7,
          );
          yield* insertJob(FINALIZING, "running");
          if (!follower) return;
          yield* insertJournal(MERGED, UNJOURNALED_APPLIED, 3);
          yield* insertQueueTerminal({
            headerHash: bytes(MERGED),
            outcome: "merged",
            height: 5,
          });
        }),
      );
      expect(shown).toEqual({
        statuses: {
          [APPLIED]: "applied",
          [REMOVED]: "unlanded",
          [DEAD_SIGNED]: "unlanded",
          [FINALIZING]: "unlanded",
          ...(follower ? { [MERGED]: "folded" } : {}),
        },
        // A submitted hash with no intended one becomes the intended one.
        intended: {
          [APPLIED]: submittedAt(1),
          [REMOVED]: submittedAt(2),
          [DEAD_SIGNED]: commitOf(DEAD_SIGNED),
          [FINALIZING]: null,
          ...(follower ? { [MERGED]: submittedAt(3) } : {}),
        },
        shape: { columns: 0, jobs: false },
      });
    },
  );

  // One active journal at a time before 0037: the carry-over above holds
  // FINALIZING, so this one stands alone.
  it("carries over an active signed journal the intent journal holds with its exact bytes", async () => {
    expect(
      await migrate(
        true,
        Effect.gen(function* () {
          yield* insertJournal(
            SUBMITTED,
            {
              status: "submitted_unconfirmed",
              signed: true,
              submitted: "signed",
            },
            6,
          );
          yield* insertIntent(SUBMITTED);
        }),
      ),
    ).toEqual({
      statuses: { [SUBMITTED]: "unlanded" },
      intended: { [SUBMITTED]: commitOf(SUBMITTED) },
      shape: { columns: 0, jobs: false },
    });
  });

  // The follower prunes an intent row once it was terminal for k blocks: a
  // node stopped longer than that past its commit's settling restarts to a
  // catch-up that prunes the intent before disposition reads the journal.
  it.each([
    { status: "pending_submission" },
    { status: "submitted_unconfirmed", submitted: "signed" },
  ] as const)(
    "carries over an active signed journal ($status) whose intent row the follower pruned as a dead journal the unsigned-journal discard leaves",
    async (journal) => {
      expect(
        await migrateCommitted(
          Effect.gen(function* () {
            yield* insertJournal(SUBMITTED, { ...journal, signed: true }, 6);
            yield* anchorAndBaseOnChain;
          }),
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const [row] = yield* sql<{ status: string; intended: string }>`
              SELECT ${ownBlockStatusSql(sql, "p")} AS status,
                encode(p.intended_tx_hash, 'hex') AS intended
              FROM pending_block_finalizations p`;
            const dead = yield* PendingBlockFinalizationsDB.deadJournals;
            const discarded = yield* discardStoppedUnsignedJournals;
            const [left] = yield* sql<{ journals: number }>`SELECT
              COUNT(*)::int AS journals FROM pending_block_finalizations`;
            return {
              ...row,
              cause: dead.causes.get(SUBMITTED),
              discarded,
              journals: left?.journals,
            };
          }),
        ),
      ).toEqual({
        status: "unlanded",
        intended: commitOf(SUBMITTED),
        cause: `its signed commit ${commitOf(SUBMITTED)} is no longer journaled`,
        discarded: [],
        journals: 1,
      });
    },
  );

  it("carries over an active signed journal whose intent row the follower pruned as applied once its block landed", async () => {
    expect(
      await migrate(
        true,
        Effect.gen(function* () {
          yield* insertJournal(
            SUBMITTED,
            {
              status: "observed_waiting_stability",
              signed: true,
              submitted: "signed",
            },
            6,
          );
          yield* land(SUBMITTED, "processed");
        }),
      ),
    ).toEqual({
      statuses: { [SUBMITTED]: "applied" },
      intended: { [SUBMITTED]: commitOf(SUBMITTED) },
      shape: { columns: 0, jobs: false },
    });
  });

  it("carries over an abandoned journal submitted unjournaled as a dead signed journal, pruned once its effects clear", async () => {
    const headerHash = bytes(SUBMITTED_UNJOURNALED);
    const prune = pruneFoldedBeyondChallengeability({
      challengeableCutoff: new Date(Date.now() - 24 * 60 * 60_000),
      view: {
        confirmedHeadHash: bytes(header(0xf1)),
        liveQueueHeaderHashes: [],
      },
      deploymentIdentityDigest: Buffer.alloc(32, 7),
    });
    expect(
      await migrateCommitted(
        Effect.gen(function* () {
          yield* insertJournal(
            SUBMITTED_UNJOURNALED,
            { status: "abandoned", submitted: "unsigned" },
            8,
          );
          // Its block row: a disposal effect still pending.
          const sql = yield* SqlClient.SqlClient;
          yield* sql`INSERT INTO blocks (height, header_hash, tx_id)
            VALUES (1, ${headerHash}, ${Buffer.alloc(32, 9)})`;
        }),
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const [status] = yield* sql<{ status: string }>`SELECT
            ${ownBlockStatusSql(sql, "p")} AS status
            FROM pending_block_finalizations p`;
          // It decodes, as disposal reads it.
          const journal = Option.getOrThrow(
            yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash),
          );
          const dead = yield* PendingBlockFinalizationsDB.deadJournals;
          const whilePending = yield* prune;
          yield* sql`DELETE FROM blocks WHERE header_hash = ${headerHash}`;
          const onceCleared = yield* prune;
          const [left] = yield* sql<{ journals: number }>`SELECT COUNT(*)::int
            AS journals FROM pending_block_finalizations`;
          return {
            status: status?.status,
            intended:
              journal[
                PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH
              ]?.toString("hex"),
            prepared:
              journal[PendingBlockFinalizationsDB.Columns.PREPARED_TX_HASH],
            dead: dead.causes.has(SUBMITTED_UNJOURNALED),
            pruned: [whilePending, onceCleared],
            journals: left?.journals,
          };
        }),
      ),
    ).toEqual({
      status: "unlanded",
      intended: Buffer.alloc(32, 8).toString("hex"),
      prepared: null,
      dead: true,
      pruned: [0, 1],
      journals: 0,
    });
  });

  it("keeps locally applied journals submitted unjournaled signed, so the commit tick's unsigned-journal discard leaves them", async () => {
    expect(
      await migrateCommitted(
        Effect.gen(function* () {
          yield* insertJournal(APPLIED, UNJOURNALED_APPLIED, 1);
          yield* land(APPLIED, "processed");
          yield* insertJournal(MERGED, UNJOURNALED_APPLIED, 3);
          yield* insertQueueTerminal({
            headerHash: bytes(MERGED),
            outcome: "merged",
            height: 5,
          });
          // A journal never signed: the discard's to delete.
          yield* insertJournal(
            FINALIZING,
            { status: "submitted_local_finalization_pending" },
            7,
          );
        }),
        Effect.gen(function* () {
          const discarded = yield* discardStoppedUnsignedJournals;
          const sql = yield* SqlClient.SqlClient;
          const left = yield* sql<{ header_hash: Buffer; status: string }>`
            SELECT p.header_hash, ${ownBlockStatusSql(sql, "p")} AS status
            FROM pending_block_finalizations p ORDER BY p.header_hash`;
          return {
            discarded,
            left: left.map((row) => [
              row.header_hash.toString("hex"),
              row.status,
            ]),
          };
        }),
      ),
    ).toEqual({
      discarded: [FINALIZING],
      left: [
        [APPLIED, "applied"],
        [MERGED, "folded"],
      ],
    });
  });

  it.each([false, true])(
    "refuses a submitted hash with no intended one, with the follower's tables %s",
    async (follower) => {
      expect(
        await migrate(
          follower,
          insertJournal(
            SUBMITTED,
            { status: "submitted_unconfirmed", submitted: "unsigned" },
            6,
          ),
        ),
      ).toEqual({ refused: "MIGRATION_0037_SIGNED_JOURNAL_WITHOUT_INTENT" });
    },
  );

  it("refuses an active signed journal the intent journal holds with other bytes", async () => {
    expect(
      await migrate(
        true,
        Effect.gen(function* () {
          yield* insertJournal(
            SUBMITTED,
            { status: "observed_waiting_stability", signed: true },
            6,
          );
          yield* insertIntent(SUBMITTED, true);
        }),
      ),
    ).toEqual({ refused: "MIGRATION_0037_SIGNED_JOURNAL_WITHOUT_INTENT" });
  });

  it("refuses an active signed journal with no follower tables to hold it", async () => {
    expect(
      await migrate(
        false,
        insertJournal(
          SUBMITTED,
          { status: "pending_submission", signed: true },
          6,
        ),
      ),
    ).toEqual({ refused: "MIGRATION_0037_SIGNED_JOURNAL_WITHOUT_INTENT" });
  });

  it.each([false, true])(
    "refuses an unfinished local finalization of a journal never submitted, with the follower's tables %s",
    async (follower) => {
      expect(
        await migrate(
          follower,
          Effect.gen(function* () {
            yield* insertJournal(
              FINALIZING,
              { status: "pending_submission" },
              7,
            );
            yield* insertJob(FINALIZING, "failed");
          }),
        ),
      ).toEqual({ refused: "MIGRATION_0037_UNRECOVERABLE_LOCAL_FINALIZATION" });
    },
  );

  it.each([false, true])(
    "refuses a locally applied journal nothing applied or folded, with the follower's tables %s",
    async (follower) => {
      expect(
        await migrate(
          follower,
          insertJournal(APPLIED, { status: "locally_applied" }, 1),
        ),
      ).toEqual({ refused: "MIGRATION_0037_APPLIED_JOURNAL_UNDERIVABLE" });
    },
  );

  it("refuses a submitted hash with no intended one that is not the prepared hash", async () => {
    expect(
      await migrate(
        true,
        Effect.gen(function* () {
          yield* insertJournal(
            APPLIED,
            { ...UNJOURNALED_APPLIED, prepared: true },
            1,
          );
          yield* land(APPLIED, "processed");
        }),
      ),
    ).toEqual({ refused: "MIGRATION_0037_SUBMITTED_COMMIT_NOT_PREPARED" });
  });
});
