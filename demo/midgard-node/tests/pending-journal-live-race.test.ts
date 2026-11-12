/**
 * One live journal at a time under a concurrent prepare (plan §8.3). With
 * the single-active index gone, two prepares under one lease are kept
 * apart by row locks taken before the live-journal check: under a
 * producer's permit, the follower write gate row (`FOR UPDATE` in
 * `withFollowerWrite`); in the fixture mode, where no driver applied a view
 * and the gate row is not locked, the lease row (`FOR UPDATE` in
 * `holdMutationLease`). Prepare A parks just before its insert, holding
 * those locks; prepare B must wait on a lock, never reach its own insert,
 * and, once A commits, be refused by the live check.
 */
import "./utils.js";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Deferred, Duration, Effect, Either, Fiber } from "effect";
import { describe, expect, it } from "vitest";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import { Frontier } from "../src/landed-blocks/store.js";
import {
  FollowerWrite,
  type FollowerWritePermit,
} from "../src/services/follower-write-gate.js";
import { writeFollowerTip } from "./helpers/follower-view.js";
import { openFollowerWriteGateAt } from "./helpers/follower-write-gate.js";
import { simCommitAnchor } from "./helpers/landed-blocks-sim.own.js";
import { withTestMutationLease } from "./helpers/mutation-lease.js";
import { header, journalFixture } from "./helpers/own-journal-fixture.js";
import {
  deterministicFixtureBytes,
  provideDatabaseLayers,
  resetApplicationTables,
} from "./utils.js";

const Journals = PendingBlockFinalizationsDB;

type Mode = "fixture" | "permit";

/** `label`'s journal, with a prepared body hash (a permit requires one). */
const journalOf = (label: string) => ({
  ...journalFixture(header(`live-race-${label}`)),
  preparedTxHash: deterministicFixtureBytes(`live-race-tx:${label}`, 32),
});

/** The backends waiting on a lock `pid` holds. */
const waitersOn = (pid: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ pid: number }>`SELECT pid FROM pg_stat_activity
      WHERE wait_event_type = 'Lock' AND ${pid} = ANY(pg_blocking_pids(pid))`;
    return rows.map((row) => row.pid);
  });

const race = (mode: Mode) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    // Both journals are anchored on the follower's tip and built on the
    // frontier, so A's is live once it commits.
    const view = yield* writeFollowerTip(10);
    yield* Frontier.upsert({
      headerHash: header("base-tail").toString("hex"),
      utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    });
    const permit: FollowerWritePermit | undefined =
      mode === "permit" ? yield* openFollowerWriteGateAt(view) : undefined;
    const underMode = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
      permit === undefined
        ? effect
        : Effect.provideService(effect, FollowerWrite, permit);
    const a = journalOf("a");
    const b = journalOf("b");
    const aParked = yield* Deferred.make<number>();
    const releaseA = yield* Deferred.make<void>();
    const bReachedInsert = yield* Deferred.make<void>();
    const fiberA = yield* Journals.preparePendingSubmission(a, {
      beforeJournalInsert: Effect.gen(function* () {
        const [backend] = yield* Effect.orDie(
          sql<{
            pid: number;
          }>`SELECT pg_backend_pid() AS pid`,
        );
        yield* Deferred.succeed(aParked, backend!.pid);
        yield* Deferred.await(releaseA);
        return yield* Effect.orDie(simCommitAnchor);
      }),
    }).pipe(underMode, Effect.either, Effect.fork);
    const pidA = yield* Effect.raceFirst(
      Deferred.await(aParked),
      Fiber.join(fiberA).pipe(
        Effect.flatMap((outcome) =>
          Effect.dieMessage(`A ended before its insert: ${String(outcome)}`),
        ),
      ),
    );
    const fiberB = yield* Journals.preparePendingSubmission(b, {
      beforeJournalInsert: Deferred.succeed(bReachedInsert, undefined).pipe(
        Effect.zipRight(Effect.orDie(simCommitAnchor)),
      ),
    }).pipe(underMode, Effect.either, Effect.fork);
    // B is blocked behind A's locks: its backend waits on a lock A holds.
    let waiting = yield* waitersOn(pidA);
    for (let poll = 0; waiting.length === 0 && poll < 200; poll++) {
      yield* Effect.sleep(Duration.millis(25));
      waiting = yield* waitersOn(pidA);
    }
    const bReachedWhileAHeld = yield* Deferred.isDone(bReachedInsert);
    yield* Deferred.succeed(releaseA, undefined);
    const outcomeA = yield* Fiber.join(fiberA);
    const outcomeB = yield* Fiber.join(fiberB);
    const [journals] = yield* sql<{ n: number }>`SELECT COUNT(*)::int AS n
      FROM pending_block_finalizations`;
    return {
      bWaitingOnALock: waiting.length,
      bReachedWhileAHeld,
      outcomeA: Either.isRight(outcomeA) ? outcomeA.right : outcomeA.left,
      outcomeB: Either.isLeft(outcomeB)
        ? { message: (outcomeB.left as { message?: string }).message }
        : outcomeB.right,
      journals: journals?.n,
    };
  }).pipe(withTestMutationLease(journalOf("a").metadata.stateQueueLeaseToken));

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

describe("one live journal under two concurrent prepares of one lease", () => {
  it.each<Mode>(["fixture", "permit"])(
    "holds the second prepare on a lock until the first commits, then refuses it (%s)",
    async (mode) => {
      const result = await run(race(mode));
      expect(result).toEqual({
        bWaitingOnALock: 1,
        bReachedWhileAHeld: false,
        outcomeA: { kind: "prepared" },
        outcomeB: { message: Journals.LIVE_JOURNAL_REFUSAL },
        journals: 1,
      });
    },
    30_000,
  );
});
