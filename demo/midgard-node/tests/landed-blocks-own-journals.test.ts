/**
 * This node's block journals under whichever-lands-wins (plan §8.3, I3), on
 * the node database: the working-ledger rebase's own-journal disposition
 * over the landed rows, and its disposal and revival.
 *
 * - (a) The old commit lands after its journal was disposed of: it is
 *   revived, and its unlanded replacement on the same tail is disposed of.
 * - (b) The replacement lands: the old commit, still unfinished, lost its
 *   base slot and is disposed of; one already disposed of stays so.
 * - (c) A deposit an unfinished commit includes leaves the chain: the
 *   commit is disposed of, so the next commit is built without it.
 * - A commit on a base an admitted correction (or a rollback) removed is
 *   disposed of, and nothing stays held: no unfinished journal is left.
 * - L7: a rollback deeper than cd takes a locally finalized own block off
 *   the landed chain: its journal reverts to abandoned with no halt, and is
 *   revived when the block lands again.
 *
 * - Reviving a journal deletes its members' rejections and every rejection
 *   whose recorded causes are all deleted ones, transitively; a rejection
 *   with another cause, or none, is kept.
 *
 * - Without the follower's admission tables the disposition is held under
 *   a named reason, and the rebase plan is blocked under it, rather than
 *   read as "no event left the chain".
 *
 * Every journal is kept: disposal abandons it under its replacement digest,
 * never deletes it.
 */
import "./utils.js";

import { createHash } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  PendingBlockFinalizationsDB,
  TxRejectionsDB,
} from "../src/database/index.js";
import { LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING } from "../src/landed-blocks/holds.js";
import type { LandedLedger } from "../src/landed-blocks/ledger.js";
import {
  disposeJournals,
  ownJournalDisposition,
  reviveJournals,
} from "../src/landed-blocks/own-journals.js";
import { rebasePlan } from "../src/landed-blocks/rebase-target.js";
import { Frontier, type LandedBlockRow } from "../src/landed-blocks/store.js";
import { withHistoryWrite } from "../src/services/event-history-producer.js";
import {
  insertOwnJournal,
  JournalStatus,
  journalStatuses,
  setJournalStatus,
} from "./helpers/landed-blocks-sim.own.js";
import { simDigest } from "./helpers/landed-blocks-sim.universe.js";
import { inNode } from "./landed-blocks-own.fixture.js";

const ZERO_ROOT = "00".repeat(32);
const header = (label: string) =>
  simDigest(`own-journals:${label}`).subarray(0, 28).toString("hex");
const G = header("genesis");
const B = header("b");
const OLD = header("old");
const REPLACEMENT = header("replacement");
const C = header("c");

/** A canonical journal for own block `headerHash` on `base`, in `status`. */
const journal = (
  headerHash: string,
  base: string,
  status: PendingBlockFinalizationsDB.Status,
  at = 0,
) =>
  Effect.gen(function* () {
    yield* insertOwnJournal({
      headerHash,
      baseHeaderHash: base,
      baseUtxosRoot: ZERO_ROOT,
      expectedUtxosRoot: ZERO_ROOT,
      spent: [],
      produced: [],
      txIds: [simDigest(`own-journals:tx:${headerHash}`)],
      at: new Date(Date.parse("2026-10-01T00:00:00.000Z") + at * 60_000),
    });
    yield* setJournalStatus(headerHash, status);
  });

const row = (
  headerHash: string,
  parent: string,
  kind: "own" | "foreign",
  state: "processed" | "removed",
  applied: boolean,
): LandedBlockRow => ({
  headerHash,
  parentHeaderHash: parent,
  parentUtxosRoot: ZERO_ROOT,
  utxosRoot: ZERO_ROOT,
  kind,
  state,
  applied,
  spent: [],
  produced: [],
  depositIds: [],
  withdrawals: [],
  forcedIds: [],
  txIds: [],
});

/** The landed ledger: the frontier at G, then the processed chain `chain`. */
const landed = (chain: readonly LandedBlockRow[]): LandedLedger => ({
  frontier: { headerHash: G, utxosRoot: ZERO_ROOT },
  confirmed: [],
  chain,
});

/** The rebase's journal steps: disposition, disposal, revival. */
const rebaseJournals = (
  rows: readonly LandedBlockRow[],
  chain: readonly LandedBlockRow[],
) =>
  withHistoryWrite(
    Effect.gen(function* () {
      const disposition = yield* ownJournalDisposition(rows, landed(chain));
      yield* disposeJournals(disposition.dispose);
      yield* reviveJournals(disposition.revive);
      return disposition;
    }),
  );

const statusOf = (headerHash: string) =>
  journalStatuses.pipe(Effect.map((statuses) => statuses.get(headerHash)));

/** A deposit member of `headerHash` admitted under (key, origin). */
const depositMember = (headerHash: string, key: Buffer, origin: Buffer) =>
  withHistoryWrite(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const payload = Buffer.from("deposit-member");
      yield* sql`INSERT INTO pending_block_finalization_deposits ${sql.insert({
        header_hash: Buffer.from(headerHash, "hex"),
        member_id: key,
        ordinal: 0,
        payload_cbor: payload,
        payload_sha256: createHash("sha256").update(payload).digest(),
        source_table: "deposits_utxos",
        source_id: key,
        source_time_stamp_tz: new Date(1_000_000),
        l1_event_key: key,
        l1_origin_outref: origin,
      } as never)}`;
    }),
  );

const admitKey = (key: Buffer, origin: Buffer) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (
      sql,
    ) => sql`INSERT INTO l1_event_keys (kind, key, origin_outref, first_canonical_slot)
      VALUES ('deposit', ${key}, ${origin}, 0)`,
  );

const rewindKey = (key: Buffer) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) =>
      sql`DELETE FROM l1_event_keys WHERE kind = 'deposit' AND key = ${key}`,
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

/** Records a rejection of `txId` after the rejected transactions `causes`. */
const rejection = (txId: Buffer, causes: readonly Buffer[] = []) =>
  withHistoryWrite(
    Effect.gen(function* () {
      yield* TxRejectionsDB.insertMany([
        {
          [TxRejectionsDB.Columns.TX_ID]: txId,
          [TxRejectionsDB.Columns.REJECT_CODE]: "test",
          [TxRejectionsDB.Columns.REJECT_DETAIL]: null,
        },
      ]);
      yield* TxRejectionsDB.insertCauses(
        causes.map((causeTxId) => ({ txId, causeTxId })),
      );
    }),
  );

const rejectedIds = Effect.flatMap(SqlClient.SqlClient, (sql) =>
  sql<{ tx_id: Buffer }>`SELECT tx_id FROM tx_rejections`.pipe(
    Effect.map((rows) => new Set(rows.map((r) => r.tx_id.toString("hex")))),
  ),
);

describe("own block journals under whichever-lands-wins", () => {
  it("(a) revives the old commit that landed after its disposal and disposes of its unlanded replacement", () =>
    inNode(
      Effect.gen(function* () {
        yield* journal(OLD, G, JournalStatus.Abandoned, 0);
        yield* journal(REPLACEMENT, G, JournalStatus.SubmittedUnconfirmed, 1);
        const oldRow = row(OLD, G, "own", "processed", false);
        expect(yield* rebaseJournals([oldRow], [oldRow])).toEqual({
          dispose: [
            {
              headerHash: REPLACEMENT,
              cause: `abandoned own block ${OLD} landed and is revived`,
              active: true,
            },
          ],
          revive: [OLD],
        });
        expect(yield* statusOf(OLD)).toBe(
          JournalStatus.ObservedWaitingStability,
        );
        expect(yield* statusOf(REPLACEMENT)).toBe(JournalStatus.Abandoned);
        expect(yield* PendingBlockFinalizationsDB.hasActive).toBe(true);
      }),
    ));

  it("revival deletes its members' rejections and those traced only to them, and keeps the rest", () =>
    inNode(
      Effect.gen(function* () {
        yield* journal(OLD, G, JournalStatus.Abandoned, 0);
        const member = simDigest(`own-journals:tx:${OLD}`);
        const tx = (label: string) =>
          simDigest(`own-journals:rejected:${label}`);
        const other = tx("other");
        const dependent = tx("dependent");
        const transitive = tx("transitive");
        const mixed = tx("mixed");
        const unrelated = tx("unrelated");
        const otherDependent = tx("other-dependent");
        yield* rejection(member);
        yield* rejection(other);
        yield* rejection(dependent, [member]);
        yield* rejection(transitive, [dependent]);
        yield* rejection(mixed, [member, other]);
        yield* rejection(unrelated);
        yield* rejection(otherDependent, [other]);
        const oldRow = row(OLD, G, "own", "processed", false);
        expect((yield* rebaseJournals([oldRow], [oldRow])).revive).toEqual([
          OLD,
        ]);
        expect(yield* TxRejectionsDB.retrieveByTxId(member)).toEqual([]);
        expect(yield* rejectedIds).toEqual(
          new Set(
            [other, mixed, unrelated, otherDependent].map((id) =>
              id.toString("hex"),
            ),
          ),
        );
        const causes = yield* Effect.flatMap(
          SqlClient.SqlClient,
          (sql) =>
            sql<{ tx_id: Buffer }>`SELECT tx_id FROM tx_rejection_causes`,
        );
        expect(new Set(causes.map((r) => r.tx_id.toString("hex")))).toEqual(
          new Set([mixed, otherDependent].map((id) => id.toString("hex"))),
        );
      }),
    ));

  it("(a) keeps an unlanded replacement while nothing landed on its tail", () =>
    inNode(
      Effect.gen(function* () {
        yield* journal(OLD, G, JournalStatus.Abandoned, 0);
        yield* journal(REPLACEMENT, G, JournalStatus.SubmittedUnconfirmed, 1);
        expect(yield* rebaseJournals([], [])).toEqual({
          dispose: [],
          revive: [],
        });
        expect(yield* statusOf(OLD)).toBe(JournalStatus.Abandoned);
        expect(yield* statusOf(REPLACEMENT)).toBe(
          JournalStatus.SubmittedUnconfirmed,
        );
      }),
    ));

  it("(b) disposes of the old commit when its replacement takes the base slot, and never revives it", () =>
    inNode(
      Effect.gen(function* () {
        yield* journal(REPLACEMENT, G, JournalStatus.LocallyApplied, 1);
        yield* journal(OLD, G, JournalStatus.SubmittedUnconfirmed, 0);
        const replacementRow = row(REPLACEMENT, G, "own", "processed", true);
        expect(
          yield* rebaseJournals([replacementRow], [replacementRow]),
        ).toEqual({
          dispose: [
            {
              headerHash: OLD,
              cause: `block ${REPLACEMENT} took the slot after its base ${G}`,
              active: true,
            },
          ],
          revive: [],
        });
        expect(yield* statusOf(OLD)).toBe(JournalStatus.Abandoned);
        expect(yield* statusOf(REPLACEMENT)).toBe(JournalStatus.LocallyApplied);
        // Settled: the next rebase leaves both as they are.
        expect(
          yield* rebaseJournals([replacementRow], [replacementRow]),
        ).toEqual({ dispose: [], revive: [] });
        expect(yield* statusOf(OLD)).toBe(JournalStatus.Abandoned);
      }),
    ));

  it("(c) disposes of an unfinished commit whose deposit left the chain, and keeps one whose deposit is canonical", () =>
    inNode(
      Effect.gen(function* () {
        const key = simDigest("own-journals:deposit-key");
        const origin = Buffer.concat([key, Buffer.from([0, 0])]);
        yield* admitKey(key, origin);
        yield* journal(C, G, JournalStatus.SubmittedUnconfirmed);
        yield* depositMember(C, key, origin);
        expect(yield* rebaseJournals([], [])).toEqual({
          dispose: [],
          revive: [],
        });
        yield* rewindKey(key);
        expect(yield* rebaseJournals([], [])).toEqual({
          dispose: [
            {
              headerHash: C,
              cause: "it includes an event whose admission left the chain",
              active: true,
            },
          ],
          revive: [],
        });
        expect(yield* statusOf(C)).toBe(JournalStatus.Abandoned);
        // Nothing unfinished: the commit path builds a replacement.
        expect(yield* PendingBlockFinalizationsDB.hasActive).toBe(false);
      }),
    ));

  it("disposes of a commit on a base a correction removed, leaving nothing held", () =>
    inNode(
      Effect.gen(function* () {
        yield* journal(C, B, JournalStatus.SubmittedUnconfirmed);
        const removedBase = row(B, G, "foreign", "removed", true);
        expect(yield* rebaseJournals([removedBase], [])).toEqual({
          dispose: [
            {
              headerHash: C,
              cause: `its base ${B} left the landed chain`,
              active: true,
            },
          ],
          revive: [],
        });
        expect(yield* statusOf(C)).toBe(JournalStatus.Abandoned);
        expect(yield* PendingBlockFinalizationsDB.hasActive).toBe(false);
        // A commit on a base that is still the landed tip is kept.
        yield* journal(REPLACEMENT, G, JournalStatus.SubmittedUnconfirmed, 1);
        expect(yield* rebaseJournals([removedBase], [])).toEqual({
          dispose: [],
          revive: [],
        });
      }),
    ));

  it("L7: reverts a locally finalized own block a deep rollback removed, with no halt, and revives it when it lands again", () =>
    inNode(
      Effect.gen(function* () {
        yield* journal(OLD, G, JournalStatus.LocallyApplied);
        const removed = row(OLD, G, "own", "removed", true);
        expect(yield* rebaseJournals([removed], [])).toEqual({
          dispose: [
            {
              headerHash: OLD,
              cause: "a rollback took its block off the landed chain",
              active: false,
            },
          ],
          revive: [],
        });
        expect(yield* statusOf(OLD)).toBe(JournalStatus.Abandoned);
        const relanded = row(OLD, G, "own", "processed", false);
        expect(yield* rebaseJournals([relanded], [relanded])).toEqual({
          dispose: [],
          revive: [OLD],
        });
        expect(yield* statusOf(OLD)).toBe(
          JournalStatus.ObservedWaitingStability,
        );
      }),
    ));

  it("holds the disposition under a named reason while a follower admission table is missing, and reads it once present", () =>
    inNode(
      Effect.gen(function* () {
        yield* journal(C, G, JournalStatus.SubmittedUnconfirmed);
        yield* withHistoryWrite(
          Frontier.upsert({ headerHash: G, utxosRoot: ZERO_ROOT }),
        );
        for (const table of ["l1_event_keys", "node_l1_forced_order_fields"]) {
          const { disposition, plan } = yield* withoutTable(
            table,
            Effect.all({
              disposition: ownJournalDisposition([], landed([])),
              plan: rebasePlan,
            }),
          );
          expect(disposition).toEqual({
            dispose: [],
            revive: [],
            held: {
              reason: LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING,
              detail: `the own-journal disposition cannot read event admissions: ${table} missing`,
            },
          });
          expect(plan).toEqual({
            kind: "blocked",
            reason: LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING,
            detail: `the own-journal disposition cannot read event admissions: ${table} missing`,
          });
        }
        // Both present: the disposition is read, nothing held or due.
        expect(yield* ownJournalDisposition([], landed([]))).toEqual({
          dispose: [],
          revive: [],
        });
        expect((yield* rebasePlan).kind).toBe("none");
        expect(yield* statusOf(C)).toBe(JournalStatus.SubmittedUnconfirmed);
      }),
    ));
});
