/**
 * A signed own journal whose commit's intent row is gone (plan §8.3):
 *
 * - it is dead (`deadJournals`) only while the follower's facts hold no
 *   live queue node of its header: a block that landed and is not
 *   processed yet is never read dead because the follower pruned its
 *   intent;
 * - the journal prune (`pruneFoldedBeyondChallengeability`) deletes it,
 *   with its members, once no disposal effect is pending and its header is
 *   not challenge-relevant. A dead journal whose intent row is still there,
 *   or with an effect pending, is kept until neither is.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import { pruneFoldedBeyondChallengeability } from "../src/database/pendingBlockFinalizations.retrieve-applied-missing-da-payloads.js";
import * as Leases from "../src/database/stateQueueMutationLeases.columns.js";
import { Frontier } from "../src/landed-blocks/store.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import {
  insertOwnJournal,
  simCommitTxHash,
} from "./helpers/landed-blocks-sim.own.js";
import { simDigest } from "./helpers/landed-blocks-sim.universe.js";
import {
  deleteLiveQueueNode,
  insertLiveQueueNode,
} from "./helpers/queue-terminal-rows.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const Journals = PendingBlockFinalizationsDB;
const ZERO_ROOT = "00".repeat(32);
const header = (label: string) =>
  simDigest(`intent-gone:${label}`).subarray(0, 28).toString("hex");
const G = header("genesis");
const bytes = (headerHash: string) => Buffer.from(headerHash, "hex");

const run = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        return yield* effect.pipe(
          Effect.ensuring(Effect.orDie(resetApplicationTables)),
        );
      }),
    ) as Effect.Effect<A, unknown, never>,
  );

/** A signed journal of own block `label` on `base` (G by default), with
 * one transaction member. */
const journal = (label: string, at: number, base = G) =>
  Effect.gen(function* () {
    yield* withFollowerWrite(
      Frontier.upsert({ headerHash: G, utxosRoot: ZERO_ROOT }),
    );
    const headerHash = header(label);
    yield* insertOwnJournal({
      headerHash,
      baseHeaderHash: base,
      baseUtxosRoot: ZERO_ROOT,
      expectedUtxosRoot: ZERO_ROOT,
      spent: [],
      produced: [],
      txIds: [simDigest(`intent-gone:tx:${label}`)],
      at: new Date(Date.parse("2026-10-01T00:00:00.000Z") + at * 60_000),
    });
    return headerHash;
  });

/** The follower prunes `headerHash`'s commit intent. */
const pruneIntent = (headerHash: string) =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) =>
      sql`DELETE FROM l1_intents WHERE tx_hash = ${simCommitTxHash(bytes(headerHash))}`,
  );

/** The lease `headerHash`'s journal was prepared under, as `status`. */
const lease = (headerHash: string, status: "active" | "released") =>
  Effect.flatMap(
    SqlClient.SqlClient,
    (sql) => sql`INSERT INTO ${sql(Leases.tableName)} (
        ${sql(Leases.Columns.TOKEN)}, ${sql(Leases.Columns.SCOPE)},
        ${sql(Leases.Columns.HOLDER)}, ${sql(Leases.Columns.STATUS)},
        ${sql(Leases.Columns.EXPIRES_AT)}, released_at)
      VALUES (${`landed-sim:${headerHash}`}, ${Leases.SCOPE}, 'intent-gone',
        ${status}, NOW() + INTERVAL '1 hour',
        ${status === "released" ? sql`NOW()` : null})
      ON CONFLICT (${sql(Leases.Columns.TOKEN)}) DO UPDATE
        SET status = EXCLUDED.status, released_at = EXCLUDED.released_at`,
  );

const deadCauses = Effect.map(
  Journals.deadJournals,
  (dead) => new Map(dead.causes),
);

/** Each of `headers`' journal and transaction member counts. */
const journalRows = (headers: readonly string[]) =>
  Effect.flatMap(SqlClient.SqlClient, (sql) =>
    Effect.forEach(headers, (headerHash) =>
      sql<{ journals: number; members: number }>`SELECT
          (SELECT COUNT(*)::int FROM pending_block_finalizations
            WHERE header_hash = ${bytes(headerHash)}) AS journals,
          (SELECT COUNT(*)::int FROM pending_block_finalization_txs
            WHERE header_hash = ${bytes(headerHash)}) AS members`.pipe(
        Effect.map(([row]) => [headerHash, row] as const),
      ),
    ).pipe(Effect.map((rows) => Object.fromEntries(rows))),
  );

const prune = (liveQueue: readonly string[] = []) =>
  pruneFoldedBeyondChallengeability({
    challengeableCutoff: new Date(Date.now() - 24 * 60 * 60_000),
    view: {
      confirmedHeadHash: bytes(header("not-journaled")),
      liveQueueHeaderHashes: liveQueue.map(bytes),
    },
    deploymentIdentityDigest: Buffer.alloc(32, 7),
  });

describe("a signed own journal whose intent row is gone", () => {
  it("is not dead while its header is a live queue node in the facts", async () => {
    const result = await run(
      Effect.gen(function* () {
        const landed = yield* journal("landed-unprocessed", 0);
        yield* insertLiveQueueNode(bytes(landed));
        yield* pruneIntent(landed);
        const whileQueued = yield* deadCauses;
        yield* deleteLiveQueueNode(bytes(landed));
        return { landed, whileQueued, afterSpent: yield* deadCauses };
      }),
    );
    expect(result.whileQueued.has(result.landed)).toBe(false);
    expect(result.afterSpent.get(result.landed)).toBe(
      `its signed commit ${simCommitTxHash(bytes(result.landed)).toString("hex")} is no longer journaled`,
    );
  });

  it("is pruned with its members once dead with no effect pending, and kept while its intent or an effect is left", async () => {
    const result = await run(
      Effect.gen(function* () {
        const spent = yield* journal("spent", 0);
        // Dead (its base left the landed chain), its intent row retained.
        const retained = yield* journal("retained", 1, header("off-chain"));
        const pending = yield* journal("pending", 2);
        const queued = yield* journal("queued", 3);
        const live = yield* journal("live", 4);
        for (const headerHash of [spent, pending, queued])
          yield* pruneIntent(headerHash);
        yield* lease(pending, "active");
        const all = [spent, retained, pending, queued, live];
        const dead = yield* deadCauses;
        const removed = yield* prune([queued]);
        const after = yield* journalRows(all);
        yield* lease(pending, "released");
        const removedOnceDone = yield* prune([queued]);
        return {
          headers: { spent, retained, pending, queued, live },
          dead: all.filter((headerHash) => dead.has(headerHash)),
          removed,
          after,
          removedOnceDone,
          afterDone: yield* journalRows([pending]),
        };
      }),
    );
    const { spent, retained, pending, queued, live } = result.headers;
    expect(result.dead).toEqual([spent, retained, pending, queued]);
    expect(result.removed).toBe(1);
    expect(result.after).toEqual({
      [spent]: { journals: 0, members: 0 },
      [retained]: { journals: 1, members: 1 },
      [pending]: { journals: 1, members: 1 },
      [queued]: { journals: 1, members: 1 },
      [live]: { journals: 1, members: 1 },
    });
    expect(result.removedOnceDone).toBe(1);
    expect(result.afterDone).toEqual({
      [pending]: { journals: 0, members: 0 },
    });
  });
});
