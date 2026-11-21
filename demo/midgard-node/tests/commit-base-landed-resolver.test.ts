/**
 * The commit base on a foreign queue tail (plan §7.3, N3): the commit
 * builds on it only once landed-block processing replayed it to its
 * header's root, the rebase applied it and the native root is that root.
 * Otherwise it waits (`LANDED_COMMIT_BASE_PENDING`), whichever of those is
 * missing; built on, the base is the landed ledger at that header. It also
 * waits while this node's own block is in flight, even one that leaves the
 * tail's root unchanged. It builds from the frontier the main thread
 * verified `confirmed_ledger` at, and waits (`CommitBaseUnverifiedError`)
 * while the frontier is another or none was verified; a landed delta after
 * it that does not apply, and `confirmed_ledger` on a confirmed-state tip
 * that does not derive its root, are refused as a local ledger divergence
 * naming its check, never built on.
 */
import type * as SDK from "@al-ft/midgard-sdk/core";
import { Effect, Either } from "effect";
import { beforeEach, describe, expect, it } from "vitest";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import {
  confirmedTipOf,
  E0,
  E1,
  expectDivergence,
  expectPending,
  expectUnverified,
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
    expect(hexEntries(base.entries ?? [])).toEqual(hexEntries([E1]));
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
    const { tail, headerHash, r0, r1 } = await fixture({
      frontier: true,
      row: { utxosRoot: "ab".repeat(32) },
    });
    // A verification run now would diverge at this tail; the commit sees
    // the frontier the last one passed at, before the row landed.
    expectPending(
      await resolve(tail, r1, undefined, {
        headerHash: FRONTIER_HASH,
        utxosRoot: r0,
      }),
      headerHash,
    );
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

describe(
  "commit base whose entries do not derive its stored root",
  { concurrent: false },
  () => {
    it("waits on a frontier tail whose confirmed_ledger no verification passed at, and builds once one does", async () => {
      const r0 = await rootOf([E0]);
      const { tail, headerHash } = await tailOf({
        ...simHeader(5, FRONTIER_HASH),
        utxosRoot: r0,
      });
      // The frontier is the tail at r0, but `confirmed_ledger` holds a row
      // that root does not: the verification diverges and records none.
      await seed([E0, E1], { headerHash, utxosRoot: r0 });
      expectUnverified(await resolve(tail, r0), headerHash);

      await seed([E0], { headerHash, utxosRoot: r0 });
      const outcome = await resolve(tail, r0);
      expect(Either.isRight(outcome)).toBe(true);
      const base = Either.getOrThrow(outcome);
      expect(base.source).toBe(`landed:${headerHash}`);
      expect(hexEntries(base.entries ?? [])).toEqual(hexEntries([E0]));
    });

    it("refuses confirmed_ledger on a confirmed-state tip the frontier is at, and builds once it derives the state's root", async () => {
      const r0 = await rootOf([E0]);
      const state: SDK.ConfirmedState = {
        headerHash: "c0".repeat(28),
        prevHeaderHash: FRONTIER_HASH,
        utxoRoot: r0,
        startTime: 1n,
        endTime: 2n,
        protocolVersion: 1n,
      };
      const tip = await confirmedTipOf(state);
      await seed([E0, E1], { headerHash: state.headerHash, utxosRoot: r0 });
      expectDivergence(await resolve(tip, r0), {
        check: "commit_base_confirmed_ledger",
        headerHash: state.headerHash,
        root: r0,
        derivedRoot: await rootOf([E0, E1]),
      });

      await seed([E0], { headerHash: state.headerHash, utxosRoot: r0 });
      const outcome = await resolve(tip, r0);
      expect(Either.isRight(outcome)).toBe(true);
      const base = Either.getOrThrow(outcome);
      expect(base.source).toBe("confirmed_ledger");
      expect(base.root).toBe(r0);
      expect(hexEntries(base.entries ?? [])).toEqual(hexEntries([E0]));
    });

    it("waits, not refuses, while the frontier is not yet at the confirmed state", async () => {
      const r0 = await rootOf([E0]);
      const state: SDK.ConfirmedState = {
        headerHash: "c0".repeat(28),
        prevHeaderHash: FRONTIER_HASH,
        utxoRoot: await rootOf([E1]),
        startTime: 1n,
        endTime: 2n,
        protocolVersion: 1n,
      };
      await seed([E0], { headerHash: FRONTIER_HASH, utxosRoot: r0 });
      expectPending(
        await resolve(await confirmedTipOf(state), state.utxoRoot),
        state.headerHash,
      );
    });
  },
);

describe(
  "commit base on a tail built from the verified frontier",
  { concurrent: false },
  () => {
    // The commit-path checks of a landed row that landed after the last
    // verification passed at the frontier: a run now would find the same
    // row and diverge, so the commit is given the frontier it passed at.
    const passedAtFrontier = (r0: string) => ({
      headerHash: FRONTIER_HASH,
      utxosRoot: r0,
    });

    it("waits while no verification has passed, and while the last one was at another frontier", async () => {
      const { tail, headerHash, r0, r1 } = await fixture({
        frontier: true,
        row: {},
      });
      expectUnverified(await resolve(tail, r1, undefined, "none"), headerHash);
      expectUnverified(
        await resolve(tail, r1, undefined, {
          headerHash: "e0".repeat(28),
          utxosRoot: r0,
        }),
        headerHash,
      );
      expectUnverified(
        await resolve(tail, r1, undefined, {
          headerHash: FRONTIER_HASH,
          utxosRoot: r1,
        }),
        headerHash,
      );
      const outcome = await resolve(tail, r1, undefined, {
        headerHash: FRONTIER_HASH,
        utxosRoot: r0,
      });
      expect(Either.getOrThrow(outcome).source).toBe(`landed:${headerHash}`);
    });

    it("refuses a landed delta that spends an output the ledger before it does not hold", async () => {
      const { tail, headerHash, r0, r1 } = await fixture({
        frontier: true,
        row: { spent: [E0.outref, E1.outref], produced: [] },
      });
      expectDivergence(
        await resolve(tail, r1, undefined, passedAtFrontier(r0)),
        {
          check: "commit_base_landed",
          headerHash,
          root: r1,
          fault: expect.stringContaining(
            `spends ${E1.outref.toString("hex")}, which the ledger before it does not hold`,
          ) as unknown as string,
        },
      );
    });

    it("refuses a landed delta that produces an output the ledger before it already holds", async () => {
      const { tail, headerHash, r0, r1 } = await fixture({
        frontier: true,
        row: { spent: [], produced: [E0, E1] },
      });
      expectDivergence(
        await resolve(tail, r1, undefined, passedAtFrontier(r0)),
        {
          check: "commit_base_landed",
          headerHash,
          root: r1,
          fault: expect.stringContaining(
            `produces ${E0.outref.toString("hex")}, which the ledger before it already holds`,
          ) as unknown as string,
        },
      );
    });

    it("refuses a landed row that does not build on the verified frontier's root", async () => {
      const { tail, headerHash, r0, r1 } = await fixture({
        frontier: true,
        row: { parentUtxosRoot: "ab".repeat(32) },
      });
      expectDivergence(
        await resolve(tail, r1, undefined, passedAtFrontier(r0)),
        {
          check: "commit_base_landed",
          headerHash,
          root: r1,
          fault: expect.stringContaining(
            `builds on root ${"ab".repeat(32)}, the root before it is ${r0}`,
          ) as unknown as string,
        },
      );
    });
  },
);
