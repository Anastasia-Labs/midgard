import { mkdir, readdir, rm } from "node:fs/promises";
import { join } from "node:path";

import { beginWorkflowFundingReservationAction } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import { fundingTerminal } from "./funding-handoff-fixture.js";
import { expiredNotFound } from "./prover-funding-recovery.retirement-fixture.js";
import {
  type Fixture,
  recordReplacement,
  signReplacement,
} from "./superseded-attempt-replacement.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

/** A restarted process admits the objective through the watcher's
 * production funding authority: under its original decision, or under a
 * fresh one classified at a deeper confirmation depth, as a restart does. */
const restart = async (fixture: Fixture, fresh: boolean) =>
  fresh
    ? await fixture.recover()
    : fixture.bind(fixture.old, await fixture.createPermit(fixture.old, "2"));

/** Every row runs under both decisions a restarted process can carry. */
const DECISIONS = [false, true] as const;

const STEP = { actionId: "step", actionKind: "proof.step" } as const;

/** Loses the workflow journal. With `interrupted`, the first process to
 * rebuild it stops once the journal is recreated, before it recovers anything
 * from the store, so the next one is admitted over that recreated journal.
 * Returns the journal of the process that runs on. */
const loseJournal = async (
  fixture: Fixture,
  interrupted: boolean,
  fresh: boolean,
) => {
  await rm(fixture.journalDirectory, { recursive: true, force: true });
  if (interrupted) {
    const stopped = await restart(fixture, fresh);
    const append = stopped.append.bind(stopped);
    const stop = new Error("process stopped while rebuilding");
    const spy = vi
      .spyOn(stopped, "append")
      .mockImplementation(async (entry, at) => {
        const appended = await append(entry, at);
        if (entry.event.kind === "prepared") throw stop;
        return appended;
      });
    await expect(fixture.run(stopped)).rejects.toBe(stop);
    spy.mockRestore();
    expect(
      (await stopped.load(fixture.initial.workflowId)).map(
        ({ event }) => event.kind,
      ),
    ).toEqual(["started", "prepared"]);
  }
  return await restart(fixture, fresh);
};

/** The init confirmed and the next action's signed attempt is in flight: its
 * handoff is anchored past the first records of any journal recreated for
 * this workflow. */
const stepInFlight = async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  const journal = await fixture.recover();
  await fixture.run(journal);
  expect(
    (await journal.load(fixture.initial.workflowId)).at(-1)!.event.kind,
  ).toBe("confirmed");
  await beginWorkflowFundingReservationAction({
    journal,
    action: { actionId: STEP.actionId, input: { actionKind: STEP.actionKind } },
  });
  const [record] = await fixture.records();
  const funding = record!.activeInputs.find(({ role }) => role === "funding")!;
  const step = signReplacement(
    funding.outRef,
    BigInt(funding.lovelace),
    300n,
    record!.activeInputs
      .filter(({ role }) => role === "collateral")
      .map(({ outRef }) => outRef),
  );
  await recordReplacement(fixture, journal, step, 1, STEP);
  return { fixture, step };
};

// The reservation store survives the loss of an objective's workflow journal.
// The objective proceeds from what the store and the chain prove, never from
// anything they do not.
describe("an objective whose workflow journal was lost proceeds", () => {
  it.each(DECISIONS)(
    "re-records the pending attempt on the next run after one that ended before re-recording it (fresh decision: %s)",
    async (fresh) => {
      const { fixture, step } = await stepInFlight();
      await rm(fixture.journalDirectory, { recursive: true, force: true });
      const stopped = await restart(fixture, fresh);
      // The run recreates the journal, then its chain query for the attempt's
      // inputs fails and the process stops before the attempt is in it.
      const stop = new Error("L1 source unavailable");
      fixture.resolveInputs.mockRejectedValueOnce(stop);
      await expect(fixture.run(stopped)).rejects.toBe(stop);
      expect(
        (await stopped.load(fixture.initial.workflowId)).map(
          ({ event }) => event.kind,
        ),
      ).toEqual(["started", "prepared"]);
      // The next process is admitted over that recreated journal and proceeds.
      const journal = await restart(fixture, fresh);
      expect(await fixture.run(journal)).toMatchObject({
        kind: "pending",
        workflowId: fixture.initial.workflowId,
      });
      const events = (await journal.load(fixture.initial.workflowId)).map(
        ({ event }) => event,
      );
      expect(events.slice(2)).toEqual([
        expect.objectContaining({
          kind: "preflight_passed",
          actionId: STEP.actionId,
          txHash: step.transactionHash,
        }),
        expect.objectContaining({
          kind: "submission_intent",
          actionId: STEP.actionId,
          attempt: 1,
          txHash: step.transactionHash,
        }),
        {
          kind: "reconciled",
          actionId: STEP.actionId,
          outcome: "confirmed",
          txHash: step.transactionHash,
        },
        {
          kind: "confirmed",
          actionId: STEP.actionId,
          txHash: step.transactionHash,
        },
      ]);
      // The original reservation carried it: no other one was taken.
      const records = await fixture.records();
      expect(records).toHaveLength(1);
      expect(records[0]!.pendingTransition).toBeNull();
      expect(fixture.adapter.preflight).not.toHaveBeenCalled();
      expect(fixture.adapter.submit).not.toHaveBeenCalled();
    },
  );

  it("resumes the in-flight attempt after a crash between creating the journal directory and its first record", async () => {
    const { fixture, step } = await stepInFlight();
    await rm(fixture.journalDirectory, { recursive: true, force: true });
    const stopped = await restart(fixture, true);
    // The process stops once the journal directory exists, before any record.
    const directory = join(
      fixture.journalDirectory,
      fixture.initial.workflowId,
    );
    const crash = new Error("crash before the first journal record");
    vi.spyOn(stopped, "append").mockImplementation(async () => {
      await mkdir(directory, { recursive: true });
      throw crash;
    });
    await expect(fixture.run(stopped)).rejects.toBe(crash);
    expect(await readdir(directory)).toEqual([]);
    const journal = await restart(fixture, true);
    expect(await fixture.run(journal)).toMatchObject({
      kind: "pending",
      workflowId: fixture.initial.workflowId,
    });
    expect(
      (await journal.load(fixture.initial.workflowId))
        .slice(2)
        .map(({ event }) => [event.kind, "txHash" in event && event.txHash]),
    ).toEqual(
      ["preflight_passed", "submission_intent", "reconciled", "confirmed"].map(
        (kind) => [kind, step.transactionHash],
      ),
    );
    const records = await fixture.records();
    expect(records).toHaveLength(1);
    expect(records[0]!.pendingTransition).toBeNull();
    expect(fixture.adapter.preflight).not.toHaveBeenCalled();
    expect(fixture.adapter.submit).not.toHaveBeenCalled();
  });

  it.each(
    (["before", "after"] as const).flatMap((boundary) =>
      [false, true].flatMap((interrupted) =>
        DECISIONS.map((fresh) => [boundary, interrupted, fresh] as const),
      ),
    ),
  )(
    "re-records an attempt abandoned %s its abandonment was acknowledged and proceeds (rebuild interrupted: %s, fresh decision: %s)",
    async (boundary, interrupted, fresh) => {
      const fixture = await setupFundingRecoveryFixture();
      vi.mocked(fixture.adapter.reconcile).mockResolvedValue(
        expiredNotFound(fixture.transactionHash),
      );
      const journal = await fixture.recover();
      if (boundary === "before") {
        // The process stops after the store abandons the attempt, before the
        // journal records its outcome.
        const append = journal.append.bind(journal);
        const crash = new Error("crash before recording the abandonment");
        const spy = vi
          .spyOn(journal, "append")
          .mockImplementation(async (entry, at) => {
            if (entry.event.kind === "reconciled") throw crash;
            return append(entry, at);
          });
        await expect(fixture.run(journal)).rejects.toBe(crash);
        spy.mockRestore();
      } else
        expect(await fixture.run(journal)).toMatchObject({ kind: "pending" });
      expect(await fixture.store.readAbandonmentHandoff(fixture.plan)).toEqual(
        boundary === "before" ? expect.anything() : null,
      );
      vi.mocked(fixture.adapter.observe).mockClear();
      const restarted = await loseJournal(fixture, interrupted, fresh);
      expect(await fixture.run(restarted)).toMatchObject({
        kind: "pending",
        workflowId: fixture.initial.workflowId,
      });
      expect(
        (await restarted.load(fixture.initial.workflowId))
          .slice(2)
          .map(({ event }) => event),
      ).toEqual([
        fixture.handoff.preflight,
        fixture.handoff.submissionIntent,
        {
          kind: "reconciled",
          actionId: "init",
          outcome: "not_found",
          txHash: fixture.transactionHash,
          retirement: expect.anything(),
        },
      ]);
      expect(
        await fixture.store.readAbandonmentHandoff(fixture.plan),
      ).toBeNull();
      expect(await fixture.records()).toEqual([
        expect.objectContaining({ state: "active", pendingTransition: null }),
      ]);
      expect(fixture.adapter.observe).toHaveBeenCalledTimes(1);
      expect(fixture.adapter.preflight).not.toHaveBeenCalled();
      expect(fixture.adapter.submit).not.toHaveBeenCalled();
    },
  );

  it.each(
    [false, true].flatMap((interrupted) =>
      DECISIONS.map((fresh) => [interrupted, fresh] as const),
    ),
  )(
    "reports a completed objective done without acting again (rebuild interrupted: %s, fresh decision: %s)",
    async (interrupted, fresh) => {
      const fixture = await setupFundingRecoveryFixture();
      await fixture.run(await fixture.recover());
      const terminal = fundingTerminal(
        fixture.old.headerHash,
        fixture.transactionHash,
        "ee".repeat(32),
      );
      vi.mocked(fixture.adapter.observe).mockResolvedValue({
        kind: "completed",
        terminal,
      });
      expect(await fixture.run(await fixture.recover())).toMatchObject({
        kind: "completed",
        terminal,
      });
      vi.mocked(fixture.adapter.observe).mockClear();
      fixture.terminalVerify.mockClear();
      vi.mocked(fixture.adapter.reconcile).mockClear();
      const journal = await loseJournal(fixture, interrupted, fresh);
      expect(await fixture.run(journal)).toMatchObject({
        kind: "completed",
        workflowId: fixture.initial.workflowId,
        terminal,
      });
      expect(
        (await journal.load(fixture.initial.workflowId))
          .slice(2)
          .map(({ event }) => event),
      ).toEqual([
        fixture.handoff.preflight,
        fixture.handoff.submissionIntent,
        {
          kind: "reconciled",
          actionId: "init",
          outcome: "confirmed",
          txHash: fixture.transactionHash,
        },
        {
          kind: "confirmed",
          actionId: "init",
          txHash: fixture.transactionHash,
        },
        { kind: "completed", terminal, terminalDigest: expect.any(String) },
      ]);
      expect(await fixture.records()).toEqual([
        expect.objectContaining({ state: "released", activeInputs: [] }),
      ]);
      expect(fixture.terminalVerify).toHaveBeenCalledTimes(1);
      expect(fixture.adapter.observe).not.toHaveBeenCalled();
      expect(fixture.adapter.reconcile).not.toHaveBeenCalled();
      expect(fixture.adapter.preflight).not.toHaveBeenCalled();
      expect(fixture.adapter.submit).not.toHaveBeenCalled();
    },
  );
});
