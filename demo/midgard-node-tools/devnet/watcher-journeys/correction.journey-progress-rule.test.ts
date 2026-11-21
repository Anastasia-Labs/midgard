import { createHash } from "node:crypto";

import { WORKFLOW_CONTINUATION_QUIET_BOUND_MS } from "@al-ft/midgard-fault-proofs/workflow/pending-continuation";
import { describe, expect, it } from "vitest";

import {
  chainCorrection,
  type ChainObservation,
  holding,
  pollChainCorrection,
} from "./correction.chain-progress.test-support.js";

const quietBoundMs = WORKFLOW_CONTINUATION_QUIET_BOUND_MS;
const startAt = 1_800_000_000_000;
const stepMs = 60_000;
const tx = (name: string) => createHash("sha256").update(name).digest("hex");

/**
 * The correction wait reads progress from the chain and the watcher's own
 * verdicts from its `/v1/status`: the watcher keeps no record of its
 * workflow. A family may land any number of transactions; the wait ends at
 * the correction's action depth, at a hold by which the watcher gave up on
 * the objective, or at a pausing hold that outlasts the watcher's quiet bound
 * with nothing of ours landing.
 */
describe("the correction wait's progress rule", () => {
  it("accepts a family that lands more steps than the old 32-transaction bound", async () => {
    const landed: string[] = [];
    const observations: ChainObservation[] = Array.from(
      { length: 40 },
      (_, index) => {
        landed.push(tx(`step-${index.toString()}`));
        return { at: startAt + (index + 1) * stepMs, landed: [...landed] };
      },
    );
    observations.push({
      at: startAt + 41 * stepMs,
      landed,
      correction: chainCorrection(1),
    });
    const { completion, alreadyCorrected, read } =
      await pollChainCorrection(observations);
    expect(completion).toEqual(chainCorrection(1));
    expect(alreadyCorrected).toBe(false);
    expect(read).toBe(41);
  });

  it("waits until the correction reaches the action depth", async () => {
    const landed = [tx("removal")];
    const { completion, read } = await pollChainCorrection(
      [
        { at: startAt, landed, correction: chainCorrection(1) },
        { at: startAt + stepMs, landed, correction: chainCorrection(2) },
        { at: startAt + 2 * stepMs, landed, correction: chainCorrection(3) },
      ],
      { actionDepth: 3 },
    );
    expect(completion.observedAt.confirmationDepth).toBe(3);
    expect(read).toBe(3);
  });

  it("reports a correction the chain already held at its first read", async () => {
    const { alreadyCorrected } = await pollChainCorrection([
      { at: startAt, landed: [tx("removal")], correction: chainCorrection(5) },
    ]);
    expect(alreadyCorrected).toBe(true);
  });

  it.each([
    "fault_proof_continuation_stalled",
    "fault_proof_counterparty_deadline_passed",
    "fault_proof_resubmission_required",
  ])("fails at once, naming it, when the watcher holds %s", async (reason) => {
    await expect(
      pollChainCorrection([
        { at: startAt, landed: [tx("step-0")] },
        {
          at: startAt + stepMs,
          landed: [tx("step-0")],
          watcher: holding({ reason }),
        },
      ]),
    ).rejects.toThrow(
      new RegExp(
        `^at observation 2 of 2: The watcher gave up on .*${reason}`,
        "u",
      ),
    );
  });

  it.each([
    "prover_funding_unavailable",
    "validation_trace_replay_held",
    "fault_proof_objective_quarantined",
    "fault_proof_l1_refused:provider_unavailable",
  ])(
    "fails a %s hold that outlasts the quiet bound with nothing of ours landing",
    async (reason) => {
      const landed = [tx("step-0")];
      const watcher = holding({ reason });
      await expect(
        pollChainCorrection([
          { at: startAt, landed, watcher },
          { at: startAt + quietBoundMs, landed, watcher },
          { at: startAt + quietBoundMs + 1, landed, watcher },
        ]),
      ).rejects.toThrow(
        `at observation 3 of 3: The watcher held validationTraceDispute/${"34".repeat(28)} under ${reason} for over ${quietBoundMs.toString()} ms with nothing of ours landing`,
      );
    },
  );

  it("restarts the pause's clock when one of ours lands or the hold clears", async () => {
    const watcher = holding({ reason: "prover_funding_unavailable" });
    const half = quietBoundMs / 2 + stepMs;
    await expect(
      pollChainCorrection([
        { at: startAt, landed: [], watcher },
        { at: startAt + half, landed: [tx("top-up")], watcher },
        { at: startAt + 2 * half, landed: [tx("top-up")], watcher },
        { at: startAt + 2 * half + 1, landed: [tx("top-up")] },
        { at: startAt + 3 * half, landed: [tx("top-up")], watcher },
        {
          at: startAt + 4 * half,
          landed: [tx("top-up")],
          watcher,
          correction: chainCorrection(1),
        },
      ]),
    ).resolves.toMatchObject({ read: 6 });
  });

  it("ignores a started-late alert and another objective's holds", async () => {
    const landed = [tx("step-0")];
    const watcher = holding(
      { reason: "fault_proof_started_late" },
      {
        reason: "fault_proof_continuation_stalled",
        headerHash: "9a".repeat(28),
      },
      { reason: "prover_funding_unavailable", headerHash: "9a".repeat(28) },
    );
    await expect(
      pollChainCorrection([
        { at: startAt, landed, watcher },
        { at: startAt + 2 * quietBoundMs, landed, watcher },
      ]),
    ).rejects.toThrow("at observation 2 of 2: observations ended");
  });

  it("keeps waiting through a long silence the watcher does not call a stall", async () => {
    // A defense window: the watcher awaits its counterparty and holds nothing.
    const landed = [tx("step-0")];
    await expect(
      pollChainCorrection([
        { at: startAt, landed },
        { at: startAt + 4 * quietBoundMs, landed },
        {
          at: startAt + 4 * quietBoundMs + stepMs,
          landed,
          correction: chainCorrection(1),
        },
      ]),
    ).resolves.toMatchObject({ read: 3 });
  });
});
