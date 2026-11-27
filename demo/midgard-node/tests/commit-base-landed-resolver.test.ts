/**
 * The commit base on a foreign queue tail: the commit
 * builds on it only once landed-block processing replayed it to its
 * header's root, the rebase applied it and the native root is that root.
 * Otherwise it waits (`LANDED_COMMIT_BASE_PENDING`), whichever of those is
 * missing; built on, the base is the ledger store's ledger at that root,
 * its outputs read by out-ref. It also waits while this node's own block is
 * in flight, even one that leaves the tail's root unchanged. A tail that
 * is the confirmed-ledger frontier is built on at once, the moment a fold
 * makes it the frontier included: the node's own stores are not checked
 * again.
 */
import { SqlClient } from "@effect/sql";
import { Effect, Either, Option } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import { foldMerge } from "../src/landed-blocks/confirmed-merges.js";
import { Frontier, retrieveRows } from "../src/landed-blocks/store.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import {
  E0,
  E1,
  expectPending,
  fixture,
  FRONTIER_HASH,
  hexEntries,
  resolve,
  rootOf,
  run,
  seed,
  tailOf,
} from "./commit-base-resolver.fixture.js";
import { simCommitAnchor } from "./helpers/landed-blocks-sim.own.js";
import { prepareUnderTestLease } from "./helpers/mutation-lease.js";
import {
  header as journalHeader,
  journalFixture,
} from "./helpers/own-journal-fixture.js";
import { simHeader } from "./helpers/state-queue-sim.fixtures.js";
import { resetApplicationTables } from "./utils.js";

beforeEach(async () => {
  await run(resetApplicationTables);
});

describe("commit base on a foreign queue tail", { concurrent: false }, () => {
  it("builds on the landed ledger at a processed, applied tail at the native root", async () => {
    const { tail, headerHash, r1 } = await fixture({
      frontier: true,
      row: {},
    });
    const outcome = await resolve(tail, r1);
    expect(Either.isRight(outcome)).toBe(true);
    const base = Either.getOrThrow(outcome);
    expect(base.source).toBe(`landed:${headerHash}`);
    expect(base.root).toBe(r1);
    expect(base.entries).toEqual(hexEntries([E1]));
  });

  it("waits while no frontier exists", async () => {
    const { tail, headerHash, r1 } = await fixture({
      frontier: false,
      row: {},
    });
    expectPending(await resolve(tail, r1), headerHash);
  });

  it("waits while the tail is not on the processed landed chain", async () => {
    for (const row of [undefined, { state: "removed" as const }]) {
      const { tail, headerHash, r1 } = await fixture({ frontier: true, row });
      expectPending(await resolve(tail, r1), headerHash);
    }
  });

  it("waits while the landed ledger at the tail misses its header's root", async () => {
    const { tail, headerHash, r1 } = await fixture({
      frontier: true,
      row: { utxosRoot: "ab".repeat(32) },
    });
    expectPending(await resolve(tail, r1), headerHash);
  });

  it("waits while the native root is not the tail's root", async () => {
    const { tail, headerHash, r0 } = await fixture({
      frontier: true,
      row: {},
    });
    expectPending(await resolve(tail, r0), headerHash);
  });

  it("waits while the rebase has not applied the tail", async () => {
    const { tail, headerHash, r1 } = await fixture({
      frontier: true,
      row: { applied: false },
    });
    expectPending(await resolve(tail, r1), headerHash);
  });

  it("waits while this node's own block is in flight on the tail, though it leaves the root unchanged, and builds once none is", async () => {
    const { tail, headerHash, r1 } = await fixture({
      frontier: true,
      row: {},
    });
    // An own block on the applied tail that leaves its root at r1: every
    // tail check passes, only the live journal holds the commit. It is
    // anchored on the follower's chain: a journal with no anchor is dead.
    const inFlight = journalHeader("unchanged-root-in-flight");
    const journal = journalFixture(inFlight);
    await run(
      prepareUnderTestLease(
        {
          ...journal,
          metadata: {
            ...journal.metadata,
            baseTailHeaderHash: Buffer.from(headerHash, "hex"),
            baseRoots: { ...journal.metadata.baseRoots, utxosRoot: r1 },
            expectedRoots: { ...journal.metadata.expectedRoots, utxosRoot: r1 },
          },
        },
        { beforeJournalInsert: Effect.orDie(simCommitAnchor) },
      ),
    );
    expectPending(await resolve(tail, r1), inFlight.toString("hex"));

    // The unsigned journal discarded, the same tail is a base.
    await run(PendingBlockFinalizationsDB.discardUnsignedJournal(inFlight));
    const outcome = await resolve(tail, r1);
    expect(Either.isRight(outcome)).toBe(true);
    expect(Either.getOrThrow(outcome).source).toBe(`landed:${headerHash}`);
  });
});

describe("commit base on a frontier tail", { concurrent: false }, () => {
  it("builds at once on a frontier tail at the native root, its confirmed_ledger never verified", async () => {
    const r0 = await rootOf([E0]);
    const { tail, headerHash } = await tailOf({
      ...simHeader(5, FRONTIER_HASH),
      utxosRoot: r0,
    });
    await seed([E0], { headerHash, utxosRoot: r0 });
    const outcome = await resolve(tail, r0);
    expect(Either.getLeft(outcome)).toEqual(Option.none());
    const base = Either.getOrThrow(outcome);
    expect(base.source).toBe(`landed:${headerHash}`);
    expect(base.root).toBe(r0);
    expect(base.entries).toEqual(hexEntries([E0]));
  });

  it("builds at once on a tail the fold just made the frontier", async () => {
    const { tail, headerHash, r1 } = await fixture({
      frontier: true,
      row: {},
    });
    // The merge fiber folds the processed tail: the frontier moves to it.
    await run(
      withFollowerWrite(
        Effect.gen(function* () {
          const [row] = yield* retrieveRows;
          const sql = yield* SqlClient.SqlClient;
          yield* sql.withTransaction(foldMerge(row!, null));
        }),
      ),
    );
    expect((await run(Frontier.retrieve))?.headerHash).toBe(headerHash);
    const outcome = await resolve(tail, r1);
    expect(Either.getLeft(outcome)).toEqual(Option.none());
    const base = Either.getOrThrow(outcome);
    expect(base.source).toBe(`landed:${headerHash}`);
    expect(base.root).toBe(r1);
    expect(base.entries).toEqual(hexEntries([E1]));
  });
});
