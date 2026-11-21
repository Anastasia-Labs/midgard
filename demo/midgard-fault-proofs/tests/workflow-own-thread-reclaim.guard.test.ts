/** The edges of reclaiming our thread on a removed header. The cancel waits
 * until the removal itself is past rollback recovery, so a rollback that
 * drops the removal leaves the run as it was. And the orchestrator itself refuses to resolve against another prover's record
 * while the adapter still sees a thread of ours, wrapped or not. */
import { afterEach, describe, expect, it, vi } from "vitest";

import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.js";
import { FraudProofResolvedByAnotherRecordError } from "../src/workflow/resolved-by-another-record.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";
import {
  canonicalEvidence,
  makeAdapter,
  PROOF_TX_HASH,
  run,
  terminal,
} from "./workflow.make-adapter.js";
import {
  chain,
  decorate,
  expectReclaimedThenResolved,
  FINAL_DEPTH,
  type Ledger,
  OUR_THREAD,
  reclaimPort,
  removedByAnotherProver,
  resolution,
} from "./workflow-own-thread-reclaim.fixtures.js";

afterEach(() => {
  vi.restoreAllMocks();
});

/** `makeAdapter`'s family on a header removed against our key's record,
 * its removal at depth `depth()`. */
const convictedOnOurKey = (
  depth: () => number = () => FINAL_DEPTH,
): FraudProofFamilyWorkflowAdapter => {
  const base = makeAdapter();
  return {
    ...base,
    observe: async (context) => {
      const record = terminal(String(context.artifact.headerHash));
      return {
        kind: "completed",
        terminal: {
          ...record,
          observedAt: { ...record.observedAt, confirmationDepth: depth() },
        },
      };
    },
  };
};

/** An adapter with no cancel in front of it that still reports our threads,
 * as a family's own L1 view does. */
const unwrappedSeeing = (
  base: FraudProofFamilyWorkflowAdapter,
  ledger: Ledger,
): FraudProofFamilyWorkflowAdapter => ({
  ...base,
  observeOwnThreads: async () => ledger.threads(),
});

describe("workflow: reclaim edges on a header another record removed", () => {
  it("sends no cancel until the removal itself is past rollback recovery", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    const ledger = await chain();
    const port = reclaimPort(ledger);
    let depth = 1;
    const adapter = decorate(
      removedByAnotherProver(
        ledger,
        () => true,
        () => depth,
      ),
      port,
    );
    for (depth of [1, FINAL_DEPTH - 1])
      expect(await run({ evidence, adapter, process })).toMatchObject({
        kind: "pending",
        reason: expect.stringContaining(
          `header removal awaits release finality before our thread ${OUR_THREAD} is reclaimed`,
        ),
      });
    expect(port.preflight).not.toHaveBeenCalled();
    expect(port.submit).not.toHaveBeenCalled();
    depth = FINAL_DEPTH;
    expect((await run({ evidence, adapter, process })).kind).toBe("pending");
    expect(port.submit).toHaveBeenCalledTimes(1);
    ledger.land(FINAL_DEPTH);
    expectReclaimedThenResolved(
      await run({ evidence, adapter, process }),
      { ...resolution, confirmationDepth: FINAL_DEPTH },
      ledger,
    );
  });

  it("sends no cancel while the removal against our key's record is one block short of release finality, then completes", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    const ledger = await chain();
    const port = reclaimPort(ledger);
    let depth = FINAL_DEPTH - 1;
    const adapter = decorate(
      convictedOnOurKey(() => depth),
      port,
    );
    expect(await run({ evidence, adapter, process })).toMatchObject({
      kind: "pending",
      reason: expect.stringContaining(
        `header removal awaits release finality before our thread ${OUR_THREAD} is reclaimed`,
      ),
    });
    expect(port.preflight).not.toHaveBeenCalled();
    expect(port.submit).not.toHaveBeenCalled();
    depth = FINAL_DEPTH;
    expect((await run({ evidence, adapter, process })).kind).toBe("pending");
    expect(port.submit).toHaveBeenCalledTimes(1);
    ledger.land(FINAL_DEPTH);
    expect((await run({ evidence, adapter, process })).kind).toBe("completed");
    expect(ledger.threads()).toEqual({
      spends: [expect.objectContaining({ txHash: ledger.reclaimTx })],
    });
  });

  it("carries on with the run when a rollback drops the removal before release finality", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    const ledger = await chain();
    const port = reclaimPort(ledger);
    let removed = true;
    // Our own next step stays in flight: the run holds on it, as it would
    // on a real chain, before anything else of ours could follow it.
    const base = removedByAnotherProver(
      ledger,
      () => removed,
      () => 1,
      false,
    );
    const adapter = decorate(base, port);
    expect((await run({ evidence, adapter, process })).kind).toBe("pending");
    // The removal is rolled back and never re-lands: the header is live
    // again, our thread still stands, and the run takes its own next step.
    removed = false;
    const resumed = await run({
      evidence,
      adapter,
      process,
    });
    expect(port.preflight).not.toHaveBeenCalled();
    expect(port.submit).not.toHaveBeenCalled();
    expect(ledger.threads().standing?.threadOutRef).toBe(OUR_THREAD);
    expect(base.submit).toHaveBeenCalled();
    expect(process.sent().map(({ txHash }) => txHash)).toContain(PROOF_TX_HASH);
    expect(resumed.kind).not.toBe("stalled");
    expect(resumed.kind).not.toBe("resolved_by_another_record");
  });

  /** Another prover's record removed the header; the family's stage
   * derivation does not see our thread. */
  const removedByAnother = (): FraudProofFamilyWorkflowAdapter => ({
    ...makeAdapter(),
    observe: async () => {
      throw new FraudProofResolvedByAnotherRecordError(resolution);
    },
  });

  it("holds under the operator's alarm, never resolving, while an unwrapped adapter still sees our thread", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    const ledger = await chain();
    const adapter = unwrappedSeeing(removedByAnother(), ledger);
    for (let pass = 0; pass < 2; pass += 1)
      expect(await run({ evidence, adapter, process })).toMatchObject({
        kind: "pending",
        resumeOnObservation: true,
        namedState: {
          name: "fraud-proof-thread-reclaim-unavailable",
          alarm: true,
        },
        reason: expect.stringContaining(
          `our thread ${OUR_THREAD} still holds funds on the removed header and cannot be reclaimed: no reclaim is wired in front of this family's workflow`,
        ),
      });
    // Once our thread is gone, the resolution goes through.
    ledger.land(FINAL_DEPTH);
    expect((await run({ evidence, adapter, process })).kind).toBe(
      "resolved_by_another_record",
    );
  });

  it("holds under the operator's alarm, never resolving, while the adapter cannot list our threads", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    // No view of our threads at all: nothing says none of ours stands.
    const { observeOwnThreads, ...blind } = removedByAnother();
    void observeOwnThreads;
    for (let pass = 0; pass < 3; pass += 1)
      expect(await run({ evidence, adapter: blind, process })).toMatchObject({
        kind: "pending",
        resumeOnObservation: true,
        namedState: {
          name: "fraud-proof-own-threads-unobservable",
          alarm: true,
        },
        reason: expect.stringContaining(
          "cannot list our threads on the removed header, so none of them can be shown to be spent: this family's workflow has no view of our threads",
        ),
      });
  });
});
