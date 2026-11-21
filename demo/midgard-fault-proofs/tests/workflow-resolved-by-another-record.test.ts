/** A header removed against another prover's fraud-proof record ends the
 * run resolved by another record once the removal is past rollback
 * recovery. That is never our conviction: there is no terminal and
 * no reward is claimed. Another prover's record reaches the orchestrator as
 * the stage derivation's typed error. */
import { describe, expect, it } from "vitest";

import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.js";
import { FraudProofResolvedByAnotherRecordError } from "../src/workflow/resolved-by-another-record.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";
import {
  canonicalEvidence,
  makeAdapter,
  RELEASE_FINALITY_POLICY,
  run,
} from "./workflow.make-adapter.js";

const OTHER_RECORD = `${"c4".repeat(32)}#0`;

/** Past rollback recovery: the depth a completed terminal needs. */
const FINAL_DEPTH = RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 2;

const resolution = {
  removalTxHash: "c5".repeat(32),
  referencedProofTokenOutRef: OTHER_RECORD,
  confirmationDepth: FINAL_DEPTH,
};

/** `makeAdapter`'s family, whose header is gone through another record's
 * removal while `removed()` says so. */
const removedWhile = (
  removed: () => boolean,
  controls: Parameters<typeof makeAdapter>[0] = {},
  depth: () => number = () => FINAL_DEPTH,
): FraudProofFamilyWorkflowAdapter => {
  const adapter = makeAdapter(controls);
  const observe = adapter.observe;
  adapter.observe = async (context) => {
    if (removed())
      throw new FraudProofResolvedByAnotherRecordError({
        ...resolution,
        confirmationDepth: depth(),
      });
    return await observe(context);
  };
  return adapter;
};

const expectResolved = (
  outcome: Awaited<ReturnType<typeof run>>,
  expected: typeof resolution,
) => {
  expect(outcome.kind).toBe("resolved_by_another_record");
  if (outcome.kind !== "resolved_by_another_record")
    throw new Error("not resolved");
  expect(outcome.resolution).toEqual(expected);
  expect("terminal" in outcome).toBe(false);
  return outcome;
};

describe("workflow: header removed against another record", () => {
  it("ends resolved by another record, submitting nothing, when the header is gone before any attempt", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    const adapter = removedWhile(() => true);
    const outcome = await run({ evidence, adapter, process });
    expectResolved(outcome, resolution);
    expect(adapter.submit).not.toHaveBeenCalled();
  });

  it("waits while the removal is still within rollback recovery, and resolves once it is past it", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    let depth = FINAL_DEPTH - 1;
    const adapter = removedWhile(
      () => true,
      {},
      () => depth,
    );
    const waiting = await run({ evidence, adapter, process });
    expect(waiting).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
      reason: expect.stringMatching(/awaits release finality/u),
    });
    depth = FINAL_DEPTH;
    expectResolved(await run({ evidence, adapter, process }), resolution);
  });
});
