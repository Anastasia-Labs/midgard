/**
 * This node's block journals under whichever-lands-wins (plan §8.3, I3), on
 * the node database: their derived status, which unlanded journals are dead
 * and why, and the disposal of a dead journal's pending effects. Nothing
 * is written to a journal: each check below also reads its row back
 * byte-identical.
 *
 * - (a) The old commit lands after it died: it derives landed, and its
 *   unlanded replacement on the same tail is dead and disposed of once.
 * - (b) The replacement lands: the old commit lost its base slot and is
 *   dead; a dead journal with nothing pending keeps no rebase due.
 * - (c) An unlanded commit's commit anchor leaves the chain, or it has
 *   none: it is dead, so the next commit is built from the follower's
 *   chain (every event it includes lies below its anchor).
 * - A commit on a base a correction (or a rollback) removed is dead, and
 *   no live journal is left.
 * - L7: a rollback deeper than cd takes an applied own block off the
 *   landed chain: it derives unlanded with no halt, its effects are
 *   disposed of once it is dead, and it derives landed when it lands again.
 * - The SQL readers take the dead set they are passed: a header missing
 *   from it holds.
 * - Without the follower's block tables the disposition is held under a
 *   named reason, and the rebase plan is blocked under it, rather than read
 *   as "every anchor is canonical".
 *
 * A signed commit S6 derives dead stands here as one whose intent row is
 * gone (`its signed commit … is no longer journaled`): S6 does not run on
 * this database.
 */

import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";
import { describe, expect, it } from "vitest";

import type { CommitAnchor } from "../src/database/commit-anchor.js";
import {
  BlocksDB,
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import * as Leases from "../src/database/stateQueueMutationLeases.columns.js";
import { LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING } from "../src/landed-blocks/holds.js";
import {
  disposeJournals,
  type OwnJournalDisposition,
  ownJournalDisposition,
} from "../src/landed-blocks/own-journals.js";
import { rebasePlan } from "../src/landed-blocks/rebase-target.js";
import { Frontier, setState } from "../src/landed-blocks/store.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import {
  insertOwnJournal,
  simCommitTxHash,
} from "./helpers/landed-blocks-sim.own.js";
import { simDigest } from "./helpers/landed-blocks-sim.universe.js";
import {
  journalRowOf,
  land,
  ownBlockStatuses,
  ownLandedRow,
} from "./helpers/own-block-facts.js";
import { inNode } from "./landed-blocks-own.fixture.js";

const Journals = PendingBlockFinalizationsDB;
const S = Journals.OwnBlockStatus;
const ZERO_ROOT = "00".repeat(32);
const header = (label: string) =>
  simDigest(`own-journals:${label}`).subarray(0, 28).toString("hex");
const G = header("genesis");
const B = header("b");
const OLD = header("old");
const REPLACEMENT = header("replacement");
const C = header("c");
const bytes = (headerHash: string) => Buffer.from(headerHash, "hex");
const commitOf = (headerHash: string) =>
  simCommitTxHash(bytes(headerHash)).toString("hex");

/** A signed journal for own block `headerHash` on `base`, on the frontier G. */
const journal = (
  headerHash: string,
  base: string,
  at = 0,
  anchor?: CommitAnchor,
) =>
  Effect.gen(function* () {
    yield* withFollowerWrite(
      Frontier.upsert({ headerHash: G, utxosRoot: ZERO_ROOT }),
    );
    yield* insertOwnJournal({
      anchor,
      headerHash,
      baseHeaderHash: base,
      baseUtxosRoot: ZERO_ROOT,
      expectedUtxosRoot: ZERO_ROOT,
      spent: [],
      produced: [],
      txIds: [simDigest(`own-journals:tx:${headerHash}`)],
      at: new Date(Date.parse("2026-10-01T00:00:00.000Z") + at * 60_000),
    });
  });

/** The stand-in for S6 deriving `headerHash`'s signed commit dead. */
const commitDies = (headerHash: string) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) =>
      sql`DELETE FROM l1_intents WHERE tx_hash = ${simCommitTxHash(bytes(headerHash))}`,
  );

/** The commit worker's lease `headerHash`'s journal was prepared under. */
const leaseOf = (headerHash: string) => `landed-sim:${headerHash}`;

const holdLease = (headerHash: string) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) => sql`INSERT INTO ${sql(Leases.tableName)} (
        ${sql(Leases.Columns.TOKEN)}, ${sql(Leases.Columns.SCOPE)},
        ${sql(Leases.Columns.HOLDER)}, ${sql(Leases.Columns.STATUS)},
        ${sql(Leases.Columns.EXPIRES_AT)})
      VALUES (${leaseOf(headerHash)}, ${Leases.SCOPE}, 'own-journals',
        ${Leases.Status.Active}, NOW() + INTERVAL '1 hour')`,
  );

const leaseStatus = (headerHash: string) =>
  Effect.flatMap(SqlClient.SqlClient, (sql) =>
    sql<{ status: string }>`SELECT ${sql(Leases.Columns.STATUS)} AS status
      FROM ${sql(Leases.tableName)}
      WHERE ${sql(Leases.Columns.TOKEN)} = ${leaseOf(headerHash)}`.pipe(
      Effect.map((rows) => rows[0]?.status),
    ),
  );

const summary = (disposition: OwnJournalDisposition) => ({
  dispose: disposition.dispose,
  dead: disposition.dead.map((headerHash) => headerHash.toString("hex")),
  deadCommits: [...disposition.deadCommits],
  ...(disposition.held === undefined ? {} : { held: disposition.held }),
});

/** The rebase's journal step: the disposition, then the disposal. */
const disposeDead = withFollowerWrite(
  Effect.gen(function* () {
    const disposition = yield* ownJournalDisposition;
    yield* disposeJournals(disposition.dispose, disposition.dead);
    return summary(disposition);
  }),
);

const statusOf = (headerHash: string) =>
  ownBlockStatuses.pipe(Effect.map((statuses) => statuses.get(headerHash)));

const liveHeader = Journals.retrieveLive().pipe(
  Effect.map(
    Option.map((record) =>
      record[Journals.Columns.HEADER_HASH].toString("hex"),
    ),
  ),
);

/** Follower block `height` at slot 10 * height (+5 on a fork). */
const followerBlock = (height: number, fork = false) => ({
  hash: simDigest(`own-journals:l1-block:${String(height)}:${String(fork)}`),
  height,
  slot: 10 * height + (fork ? 5 : 0),
});

const writeBlock = (block: ReturnType<typeof followerBlock>) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (
      sql,
    ) => sql`INSERT INTO l1_blocks (slot, hash, height, parent_hash, qualifying_tx_count)
      VALUES (${block.slot}, ${block.hash}, ${block.height}, NULL, 0)`,
  );

/** The schema change was rolled back, carrying what ran under it. */
class RolledBack<A> {
  constructor(readonly value: A) {}
}

/** Runs `work` with `table` renamed away, then rolls the rename back. */
const withoutTable = <A, E, R>(table: string, work: Effect.Effect<A, E, R>) =>
  Effect.flatMap(SqlClient.SqlClient, (sql) =>
    sql
      .withTransaction(
        Effect.gen(function* () {
          yield* sql`ALTER TABLE ${sql(table)} RENAME TO ${sql(`${table}_hidden`)}`;
          return yield* Effect.fail(new RolledBack(yield* work));
        }),
      )
      .pipe(
        Effect.catchIf(
          (error): error is RolledBack<A> => error instanceof RolledBack,
          (rolledBack) => Effect.succeed(rolledBack.value),
        ),
      ),
  );

describe("own block journals under whichever-lands-wins", () => {
  it("(a) the old commit lands after it died: it derives landed, and its replacement on the same tail is dead and disposed of once", () =>
    inNode(
      Effect.gen(function* () {
        yield* journal(OLD, G, 0);
        yield* journal(REPLACEMENT, G, 1);
        yield* commitDies(OLD);
        yield* holdLease(REPLACEMENT);
        const replacementRow = yield* journalRowOf(bytes(REPLACEMENT));
        yield* land(ownLandedRow(OLD, G));
        expect(yield* statusOf(OLD)).toBe(S.Landed);
        expect(yield* statusOf(REPLACEMENT)).toBe(S.Unlanded);
        const cause = `block ${OLD} took the slot after its base ${G}`;
        expect(yield* disposeDead).toEqual({
          dispose: [{ headerHash: REPLACEMENT, cause }],
          dead: [REPLACEMENT],
          deadCommits: [commitOf(REPLACEMENT)],
        });
        expect(yield* leaseStatus(REPLACEMENT)).toBe(Leases.Status.Released);
        // Disposed of once: nothing is pending, and it stays dead.
        expect(yield* disposeDead).toEqual({
          dispose: [],
          dead: [REPLACEMENT],
          deadCommits: [commitOf(REPLACEMENT)],
        });
        expect(yield* journalRowOf(bytes(REPLACEMENT))).toEqual(replacementRow);
        expect(Option.isNone(yield* liveHeader)).toBe(true);
      }),
    ));

  it("(a) keeps the newest unlanded commit live while nothing landed on its tail", () =>
    inNode(
      Effect.gen(function* () {
        yield* journal(OLD, G, 0);
        yield* journal(REPLACEMENT, G, 1);
        yield* commitDies(OLD);
        expect(yield* disposeDead).toEqual({
          dispose: [],
          dead: [OLD],
          deadCommits: [commitOf(OLD)],
        });
        expect(yield* liveHeader).toEqual(Option.some(REPLACEMENT));
        expect(yield* statusOf(OLD)).toBe(S.Unlanded);
        expect(yield* statusOf(REPLACEMENT)).toBe(S.Unlanded);
      }),
    ));

  it("(b) the replacement lands: the old commit lost its base slot and is dead, and keeps no rebase due", () =>
    inNode(
      Effect.gen(function* () {
        yield* journal(OLD, G, 0);
        yield* journal(REPLACEMENT, G, 1);
        yield* land(ownLandedRow(REPLACEMENT, G, { applied: true }));
        expect(yield* statusOf(REPLACEMENT)).toBe(S.Applied);
        const dead = {
          dispose: [],
          dead: [OLD],
          deadCommits: [commitOf(OLD)],
        };
        expect(yield* disposeDead).toEqual(dead);
        // Dead, with no effect pending: the landed rows are applied, so no
        // rebase is due.
        expect((yield* rebasePlan()).kind).toBe("none");
        expect(yield* disposeDead).toEqual(dead);
        expect(yield* statusOf(OLD)).toBe(S.Unlanded);
      }),
    ));

  it("(c) an unlanded commit whose commit anchor left the chain is dead, and one whose anchor is canonical is live", () =>
    inNode(
      Effect.gen(function* () {
        const anchor = followerBlock(10);
        yield* writeBlock(anchor);
        yield* journal(C, G, 0, anchor);
        expect(yield* disposeDead).toEqual({
          dispose: [],
          dead: [],
          deadCommits: [],
        });
        expect(yield* liveHeader).toEqual(Option.some(C));
        // A follower rewind below the anchor, and a fork at its height.
        const sql = yield* SqlClient.SqlClient;
        yield* sql`DELETE FROM l1_blocks WHERE height >= ${anchor.height}`;
        yield* writeBlock(followerBlock(anchor.height, true));
        expect((yield* Journals.deadJournals).causes).toEqual(
          new Map([[C, "its commit anchor left the chain"]]),
        );
        // No live journal: the commit path builds a replacement.
        expect(yield* Journals.hasLive).toBe(false);
      }),
    ));

  it("an unlanded commit with no commit anchor is dead", () =>
    inNode(
      Effect.gen(function* () {
        yield* journal(C, G);
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE pending_block_finalizations SET commit_anchor_hash = NULL,
          commit_anchor_height = NULL, commit_anchor_slot = NULL`;
        expect((yield* Journals.deadJournals).causes).toEqual(
          new Map([[C, "it has no commit anchor"]]),
        );
      }),
    ));

  it("a commit on a base a correction removed is dead, leaving no live journal", () =>
    inNode(
      Effect.gen(function* () {
        yield* journal(C, B);
        yield* land(
          ownLandedRow(B, G, {
            kind: "foreign",
            state: "removed",
            applied: true,
          }),
        );
        expect((yield* Journals.deadJournals).causes).toEqual(
          new Map([[C, `its base ${B} left the landed chain`]]),
        );
        expect(yield* Journals.hasLive).toBe(false);
        // A commit on a base that is still the landed tip is live.
        yield* journal(REPLACEMENT, G, 1);
        expect(yield* liveHeader).toEqual(Option.some(REPLACEMENT));
      }),
    ));

  it("L7: a deep rollback takes an applied own block off the landed chain with no halt, its effects are disposed of once it is dead, and it derives landed when it lands again", () =>
    inNode(
      Effect.gen(function* () {
        const anchor = followerBlock(10);
        yield* writeBlock(anchor);
        yield* journal(OLD, G, 0, anchor);
        const txId = simDigest(`own-journals:tx:${OLD}`);
        yield* land(ownLandedRow(OLD, G, { applied: true }));
        yield* withFollowerWrite(BlocksDB.insert(bytes(OLD), [txId]));
        expect(yield* statusOf(OLD)).toBe(S.Applied);
        const row = yield* journalRowOf(bytes(OLD));

        // The rollback removes the landing and reaches below the anchor.
        yield* setState([OLD], "removed");
        const sql = yield* SqlClient.SqlClient;
        yield* sql`DELETE FROM l1_blocks WHERE hash = ${anchor.hash}`;
        expect(yield* statusOf(OLD)).toBe(S.Unlanded);
        expect(yield* disposeDead).toEqual({
          dispose: [
            { headerHash: OLD, cause: "its commit anchor left the chain" },
          ],
          dead: [OLD],
          deadCommits: [commitOf(OLD)],
        });
        expect(
          yield* BlocksDB.retrieveTxHashesByHeaderHash(bytes(OLD)),
        ).toEqual([]);
        expect((yield* disposeDead).dispose).toEqual([]);
        expect(yield* journalRowOf(bytes(OLD))).toEqual(row);

        // Its bytes land again.
        yield* sql`DELETE FROM node_landed_blocks`;
        yield* land(ownLandedRow(OLD, G));
        expect(yield* statusOf(OLD)).toBe(S.Landed);
        expect(yield* journalRowOf(bytes(OLD))).toEqual(row);
      }),
    ));

  it("readers take the dead set they are passed: a header missing from it holds", () =>
    inNode(
      Effect.gen(function* () {
        yield* journal(C, G);
        yield* commitDies(C);
        const dead = Journals.deadHeadersOf(yield* Journals.deadJournals);
        expect(dead).toEqual([bytes(C)]);
        expect(Option.isNone(yield* Journals.retrieveLiveOver(dead))).toBe(
          true,
        );
        const held = yield* Journals.retrieveLiveOver([]);
        expect(
          Option.map(held, (record) =>
            record[Journals.Columns.HEADER_HASH].toString("hex"),
          ),
        ).toEqual(Option.some(C));
      }),
    ));

  it("holds the disposition under a named reason while a follower block table is missing, and reads it once present", () =>
    inNode(
      Effect.gen(function* () {
        yield* journal(C, G);
        for (const table of ["l1_blocks", "l1_follower_cursor"]) {
          const { disposition, plan } = yield* withoutTable(
            table,
            Effect.all({
              disposition: ownJournalDisposition,
              plan: rebasePlan(),
            }),
          );
          const detail = `the own-journal disposition cannot read commit anchors: ${table} missing`;
          expect(summary(disposition)).toEqual({
            dispose: [],
            dead: [],
            deadCommits: [],
            held: { reason: LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING, detail },
          });
          expect(plan).toEqual({
            kind: "blocked",
            reason: LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING,
            detail,
          });
        }
        // Both present: the disposition is read, nothing held or due.
        expect(summary(yield* ownJournalDisposition)).toEqual({
          dispose: [],
          dead: [],
          deadCommits: [],
        });
        expect((yield* rebasePlan()).kind).toBe("none");
        expect(yield* statusOf(C)).toBe(S.Unlanded);
      }),
    ));
});
