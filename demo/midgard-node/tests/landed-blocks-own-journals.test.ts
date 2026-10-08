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
 * Every journal is kept: disposal abandons it under its replacement digest,
 * never deletes it.
 */
import "./utils.js";

import { createHash } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import type { LandedLedger } from "../src/landed-blocks/ledger.js";
import {
  disposeJournals,
  ownJournalDisposition,
  reviveJournals,
} from "../src/landed-blocks/own-journals.js";
import type { LandedBlockRow } from "../src/landed-blocks/store.js";
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
});
