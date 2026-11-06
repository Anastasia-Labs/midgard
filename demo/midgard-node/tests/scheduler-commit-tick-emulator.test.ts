/**
 * The node's real block-commitment tick around an expired scheduler shift,
 * against the real compiled scheduler validator: a node with no work submits
 * nothing, the same node with work pushes the turn on, and a node whose
 * scheduler alignment keeps failing while it holds work says so on readiness.
 */
import "./utils.js";

import { describe, expect, it } from "vitest";

import { COMMIT_WORKER_FAILED_ESCALATION_MS } from "../src/fibers/block-commitment.worker-readiness.js";
import {
  appointFirstSchedulerOperator,
  fetchInactivityDirectorySnapshot,
} from "./helpers/operator-inactivity.js";
import {
  activeOperator,
  emptyWallet,
  initScenario,
  letShiftEnd,
  runCommitmentTicks,
  type TickObservation,
  walletLovelace,
} from "./helpers/scheduler-successor-scenario.js";

/** Two operators; the tail holds the first turn, and it has ended. */
const expiredTail = async () => {
  const scenario = await initScenario(2);
  const { fixture } = scenario;
  const [head, tail] = fixture.operators;
  if (head === undefined || tail === undefined) {
    throw new Error("Expected two fixture operators");
  }
  await appointFirstSchedulerOperator(fixture);
  await letShiftEnd(fixture);
  return { scenario, head, tail };
};

const schedulerReason = (observation: TickObservation | undefined) =>
  observation?.livenessReasons.find(
    ({ source }) => source === "commit_scheduler",
  );

describe("the commitment tick of an operator whose scheduler shift ended", () => {
  it("with no work, and not next, submits nothing", async () => {
    const { scenario, head, tail } = await expiredTail();
    const { fixture } = scenario;
    const schedulerBefore = (await fetchInactivityDirectorySnapshot(fixture))
      .scheduler;
    const tailLovelace = await walletLovelace(scenario, tail.keyHash);
    const headLovelace = await walletLovelace(scenario, head.keyHash);

    await runCommitmentTicks(scenario, tail.keyHash, [{ work: false }]);

    const schedulerAfter = (await fetchInactivityDirectorySnapshot(fixture))
      .scheduler;
    expect(schedulerAfter.utxo.txHash).toBe(schedulerBefore.utxo.txHash);
    expect(schedulerAfter.datum).toEqual(schedulerBefore.datum);
    expect(await walletLovelace(scenario, tail.keyHash)).toBe(tailLovelace);
    expect(await walletLovelace(scenario, head.keyHash)).toBe(headLovelace);
  });

  it("with work, and not next, pushes the turn to its successor and pays for it", async () => {
    const { scenario, head, tail } = await expiredTail();
    const { fixture } = scenario;
    const tailLovelace = await walletLovelace(scenario, tail.keyHash);
    const headLovelace = await walletLovelace(scenario, head.keyHash);

    const [tick] = await runCommitmentTicks(scenario, tail.keyHash, [
      { work: true },
    ]);

    const scheduler = (await fetchInactivityDirectorySnapshot(fixture))
      .scheduler;
    expect(activeOperator(scheduler.datum).operator).toBe(head.keyHash);
    expect(await walletLovelace(scenario, tail.keyHash)).toBeLessThan(
      tailLovelace,
    );
    expect(await walletLovelace(scenario, head.keyHash)).toBe(headLovelace);
    expect(schedulerReason(tick)).toBeUndefined();
  });

  it("with work, and a scheduler alignment that keeps failing, raises a readiness reason that escalates after the commit worker's window and clears once idle", async () => {
    const { scenario, tail } = await expiredTail();
    // The tail cannot pay for the push.
    await emptyWallet(scenario, tail.keyHash);

    const [failing, stillFailing, idle] = await runCommitmentTicks(
      scenario,
      tail.keyHash,
      [
        { work: true },
        // A minute past the window, which runs from the first failure.
        { work: true, advanceMs: COMMIT_WORKER_FAILED_ESCALATION_MS + 60_000 },
        { work: false },
      ],
    );

    expect(schedulerReason(failing)).toMatchObject({
      reason: "commit_scheduler_alignment_failed",
      escalateAfterMs: COMMIT_WORKER_FAILED_ESCALATION_MS,
      escalated: false,
    });
    expect(schedulerReason(failing)?.detail).toBeTruthy();
    expect(schedulerReason(stillFailing)?.escalated).toBe(true);
    // The age the alert rule reads.
    expect(
      stillFailing?.reasonAgeMs("commit_scheduler"),
    ).toBeGreaterThanOrEqual(COMMIT_WORKER_FAILED_ESCALATION_MS);
    expect(schedulerReason(idle)).toBeUndefined();
  });
});
