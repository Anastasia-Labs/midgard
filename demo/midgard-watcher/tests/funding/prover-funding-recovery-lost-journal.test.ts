import { rm } from "node:fs/promises";

import { beginWorkflowFundingReservationAction } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import {
  type Fixture,
  recordReplacement,
  signReplacement,
} from "./superseded-attempt-replacement.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

/** A restarted process admits the objective under its original decision,
 * through the watcher's production funding authority. */
const restart = async (fixture: Fixture) =>
  fixture.bind(fixture.old, await fixture.createPermit(fixture.old, "2"));

const STEP = { actionId: "step", actionKind: "proof.step" } as const;

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
  it("re-records the pending attempt on the next run after one that ended before re-recording it", async () => {
    const { fixture, step } = await stepInFlight();
    await rm(fixture.journalDirectory, { recursive: true, force: true });
    const stopped = await restart(fixture);
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
    const journal = await restart(fixture);
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
    expect((await fixture.records())[0]!.pendingTransition).toBeNull();
    expect(fixture.adapter.preflight).not.toHaveBeenCalled();
    expect(fixture.adapter.submit).not.toHaveBeenCalled();
  });
});
