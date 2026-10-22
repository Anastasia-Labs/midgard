import {
  copyFile,
  mkdir,
  mkdtemp,
  readFile,
  rm,
  writeFile,
} from "node:fs/promises";
import { join } from "node:path";

import { afterEach, describe, expect, it, vi } from "vitest";

import { parseWatcherProverFundingReservationRecord } from "../../src/funding/prover-funding-reservation.js";
import {
  warnWatcherLegacyJournalDirectories,
  type WatcherLegacyJournalIgnored,
} from "../../src/runtime/legacy-journal-directories.js";
import {
  cleanupFundingRecoveryFixtures,
  FixtureProcessStop,
  setupFundingRecoveryFixture as setup,
} from "../support/fault-proof-funding-fixture.js";
import { openFixtureFundingStore } from "../support/fault-proof-funding-fixture.workflow-store.js";
import {
  type Fixture,
  prepareThroughPermit,
  replaceAfterExpiry,
  supersedeAtTip,
} from "./superseded-attempt-replacement.js";

// Each funding change commits with the workflow journal entries it implies,
// in one watcher.sqlite transaction. A process that stops before that
// transaction or after its commit resumes without a quarantine and without a
// lease that nothing settles.

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

const eventsOf = async (fixture: Fixture) =>
  (await fixture.journal.load(fixture.initial.workflowId)).map(
    ({ event }) => event,
  );

const countOf = async (
  fixture: Fixture,
  match: (event: Awaited<ReturnType<typeof eventsOf>>[number]) => boolean,
) => (await eventsOf(fixture)).filter(match).length;

describe("a process stop at a funding change", () => {
  it("starts afresh once an unused reservation whose workflow never started is released at startup", async () => {
    const fixture = await setup(true);
    // The reservation committed; the workflow's first entry did not.
    fixture.workflowJournal.forgetObjective(fixture.objective);
    await fixture.restartStore();
    await fixture.releaseUnusedAtStartup();
    expect(await fixture.store.readReservedOutRefs({})).toEqual([]);
    fixture.useUnspentPendingInputs();
    const result = await fixture.run(await fixture.recover());
    // The objective's one execution is the fresh one, which started.
    const execution = fixture.workflowJournal.readExecution(fixture.objective);
    expect(result).toMatchObject({
      kind: "pending",
      workflowId: execution!.workflowId,
    });
    expect(execution!.entries[0]!.event).toEqual({ kind: "started" });
  });

  it.each(["before", "after"] as const)(
    "prepares one attempt when the process stops %s the prepare transaction",
    async (boundary) => {
      const fixture = await setup(true);
      fixture.useUnspentPendingInputs();
      const intents = () =>
        countOf(fixture, ({ kind }) => kind === "submission_intent");
      fixture.stopAt("prepareTransition", boundary);
      await expect(prepareThroughPermit(fixture)).rejects.toBeInstanceOf(
        FixtureProcessStop,
      );
      expect(await intents()).toBe(boundary === "after" ? 1 : 0);
      expect((await fixture.records())[0]!.pendingTransition === null).toBe(
        boundary === "before",
      );
      await fixture.restartStore();
      const attempt =
        boundary === "before"
          ? await prepareThroughPermit(fixture)
          : (await fixture.records())[0]!.pendingTransition!;
      expect(
        (await fixture.records())[0]!.pendingTransition?.transactionHash,
      ).toBe(attempt.transactionHash);
      // The prepared attempt confirms; nothing is submitted again.
      await fixture.run(await fixture.recover());
      const [record] = await fixture.records();
      expect(record).toMatchObject({
        state: "active",
        pendingTransition: null,
      });
      expect(record!.activeInputs.map(({ outRef }) => outRef)).toContain(
        `${attempt.transactionHash}#0`,
      );
      expect(await intents()).toBe(1);
      expect(
        await countOf(
          fixture,
          (event) =>
            event.kind === "confirmed" &&
            event.txHash === attempt.transactionHash,
        ),
      ).toBe(1);
      expect(fixture.adapter.submit).not.toHaveBeenCalled();
    },
    60_000,
  );

  it.each(["before", "after"] as const)(
    "supersedes an expired attempt once when the process stops %s the supersede transaction",
    async (boundary) => {
      const fixture = await setup();
      const superseded = () =>
        countOf(
          fixture,
          (event) =>
            event.kind === "reconciled" &&
            event.outcome === "not_found" &&
            event.txHash === fixture.transactionHash,
        );
      fixture.stopAt("abandonPendingTransition", boundary);
      await expect(supersedeAtTip(fixture)).rejects.toBeInstanceOf(
        FixtureProcessStop,
      );
      expect(await superseded()).toBe(boundary === "after" ? 1 : 0);
      expect((await fixture.records())[0]!.pendingTransition === null).toBe(
        boundary === "after",
      );
      await fixture.restartStore();
      await supersedeAtTip(fixture);
      expect(await superseded()).toBe(1);
      const [record] = await fixture.records();
      expect(record).toMatchObject({
        state: "active",
        pendingTransition: null,
      });
      // The superseded attempt can still land until it is retired, so a
      // later attempt must exclude it.
      expect(
        await fixture.store.readRecordedSubmissions!({
          reservationId: fixture.plan.reservationId,
        }),
      ).toEqual([
        expect.objectContaining({
          transition: expect.objectContaining({
            transactionHash: fixture.transactionHash,
          }),
        }),
      ]);
      expect(fixture.adapter.submit).not.toHaveBeenCalled();
    },
    60_000,
  );

  it.each(["before", "after"] as const)(
    "adopts a late landing once when the process stops %s the adoption transaction",
    async (boundary) => {
      const fixture = await setup(false, true);
      const { journal, replacement } = await replaceAfterExpiry(fixture);
      fixture.chain.mockImplementation(async (txHash) =>
        txHash === fixture.transactionHash
          ? { kind: "confirmed", txHash }
          : { kind: "not_found" },
      );
      fixture.stopAt("reobserveTransition", boundary);
      const runs = async (bound: typeof journal, from: number) => {
        for (let run = from; run < from + 3; run += 1)
          await fixture.run(bound, () => new Date(Date.now() + run * 60_000));
      };
      await expect(runs(journal, 0)).rejects.toBeInstanceOf(FixtureProcessStop);
      const adoptions = () =>
        countOf(
          fixture,
          (event) =>
            event.kind === "submission_intent" &&
            event.txHash === fixture.transactionHash,
        );
      expect(await adoptions()).toBe(boundary === "after" ? 2 : 1);
      await fixture.restartStore();
      await runs(await fixture.recover(), 3);
      const events = await eventsOf(fixture);
      expect(events.at(-1)).toEqual({
        kind: "confirmed",
        actionId: "init",
        txHash: fixture.transactionHash,
      });
      expect(await adoptions()).toBe(2);
      expect(events.some(({ kind }) => kind === "stalled")).toBe(false);
      expect((await fixture.records())[0]!.pendingTransition).toBeNull();
      expect(await fixture.store.readReservedOutRefs({})).toEqual(
        expect.arrayContaining([
          `${fixture.transactionHash}#0`,
          `${replacement.transactionHash}#0`,
        ]),
      );
      expect(fixture.adapter.submit).not.toHaveBeenCalled();
    },
    60_000,
  );

  it.each(["before", "after"] as const)(
    "holds a conflicting attempt once when the process stops %s the conflict transaction",
    async (boundary) => {
      const fixture = await setup();
      fixture.chain.mockResolvedValue({
        kind: "conflict",
        reason: "canonical transaction consumes different inputs",
      });
      const stalls = () => countOf(fixture, ({ kind }) => kind === "stalled");
      fixture.stopAt("markConflict", boundary);
      await expect(fixture.run(await fixture.recover())).rejects.toBeInstanceOf(
        FixtureProcessStop,
      );
      expect(await stalls()).toBe(boundary === "after" ? 1 : 0);
      expect((await fixture.records())[0]!.state).toBe(
        boundary === "after" ? "conflict" : "active",
      );
      await fixture.restartStore();
      if (boundary === "before")
        expect(await fixture.run(await fixture.recover())).toMatchObject({
          kind: "stalled",
        });
      // A conflicted reservation is held as it is, never re-executed.
      await expect(fixture.recover()).rejects.toThrow(
        "unique non-conflicted original funding reservation",
      );
      expect(await stalls()).toBe(1);
      expect((await fixture.records())[0]).toMatchObject({
        state: "conflict",
        activeInputs: fixture.pending.activeInputs,
      });
      expect(fixture.adapter.submit).not.toHaveBeenCalled();
    },
    60_000,
  );
});

it("keeps a funding change and its journal entries together when a power loss drops any tail of the log", async () => {
  const fixture = await setup();
  // Reopening checkpoints the log, so it holds only what follows.
  await fixture.restartStore();
  fixture.stopAt("confirmTransition", "after");
  await expect(fixture.run(await fixture.recover())).rejects.toBeInstanceOf(
    FixtureProcessStop,
  );
  // The process is gone with the store still open: nothing was checkpointed.
  const source = join(fixture.journalRoot, "watcher.sqlite");
  const log = await readFile(`${source}-wal`);
  const pageSize = log.readUInt32BE(8);
  const frame = 24 + pageSize;
  const frames = Math.floor((log.length - 32) / frame);
  expect(frames).toBeGreaterThan(0);
  const crash = await mkdtemp(join(fixture.journalRoot, "power-loss-"));
  const seen = new Set<string>();
  try {
    // Every frame boundary, and a torn frame after each.
    const lengths = Array.from({ length: frames + 1 }, (_, kept) => [
      32 + kept * frame,
      ...(kept < frames ? [32 + kept * frame + Math.floor(frame / 2)] : []),
    ]).flat();
    for (const [index, length] of lengths.entries()) {
      const path = join(crash, `${index.toString()}.sqlite`);
      await copyFile(source, path);
      await writeFile(`${path}-wal`, log.subarray(0, length));
      const database = await openFixtureFundingStore(path);
      try {
        const [record] = (await database.store.readAll()).map(
          parseWatcherProverFundingReservationRecord,
        );
        const confirmations = database.journal
          .readExecution(fixture.objective)!
          .entries.filter(
            ({ event }) =>
              event.kind === "confirmed" ||
              (event.kind === "reconciled" && event.outcome === "confirmed"),
          ).length;
        const state = `${record!.pendingTransition === null ? "confirmed" : "pending"}:${confirmations.toString()}`;
        expect(["pending:0", "confirmed:2"]).toContain(state);
        seen.add(state);
      } finally {
        database.close();
      }
    }
  } finally {
    await rm(crash, { recursive: true, force: true });
  }
  expect([...seen].sort()).toEqual(["confirmed:2", "pending:0"]);
});

it("recovers an execution a file journal of an earlier layout left with only a partial first entry, warning once", async () => {
  const fixture = await setup();
  const leftover = join(
    fixture.journalRoot,
    "fault-proofs",
    "doubleSpend",
    fixture.old.headerHash,
    fixture.initial.workflowId,
  );
  await mkdir(leftover, { recursive: true });
  await writeFile(join(leftover, ".00000000.persisted.tmp"), "partial");
  const warnings: WatcherLegacyJournalIgnored[] = [];
  await warnWatcherLegacyJournalDirectories(fixture.journalRoot, (warning) =>
    warnings.push(warning),
  );
  expect(warnings).toEqual([
    expect.objectContaining({
      event: "legacy_journal_ignored",
      path: join(fixture.journalRoot, "fault-proofs"),
    }),
  ]);
  expect(await fixture.run(await fixture.recover())).toMatchObject({
    kind: "pending",
    workflowId: fixture.initial.workflowId,
  });
  expect((await fixture.records())[0]!.pendingTransition).toBeNull();
});
