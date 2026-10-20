/** A script re-execution that ends in an error rather than a verdict or a
 * trace holds only its own event: the block's replay still decides, naming
 * the held event as an unprovable gap, instead of failing the whole block.
 * The evaluator's own fault (no verdict, or a trace build that fails for a
 * reason other than a stop) is injected at the validation package; with no
 * fault, the same block is the ordinary validation-trace fault. */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { ScriptEvaluationIndeterminateError } from "@al-ft/midgard-validation";
import { afterEach, describe, expect, it, vi } from "vitest";

import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import {
  createValidationTraceDisputeCompleteCanonicalReplay,
  requireCompleteCanonicalReplayDecision,
} from "../src/workflow/complete-replay.js";
import { plutusExecutionBlockFixture } from "./helpers/plutus-execution-block-fixture.js";

/** The evaluator fault the next replay of the fixture's transaction meets. */
const fault = vi.hoisted(() => ({
  verdict: undefined as Error | undefined,
  trace: undefined as Error | undefined,
}));

vi.mock("@al-ft/midgard-validation", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@al-ft/midgard-validation")>();
  const { Effect } = await import("effect");
  return {
    ...actual,
    replayValidationVerdictEvent: (
      input: Parameters<typeof actual.replayValidationVerdictEvent>[0],
    ) =>
      fault.verdict === undefined
        ? actual.replayValidationVerdictEvent(input)
        : Effect.fail(fault.verdict),
    commitValidationTraceV1: (
      input: Parameters<typeof actual.commitValidationTraceV1>[0],
    ) =>
      fault.trace === undefined
        ? actual.commitValidationTraceV1(input)
        : Effect.fail(fault.trace),
  };
});

const HOLD = "validation-trace-replay-unavailable";
const l1SlotConfig = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;
const replayer =
  createValidationTraceDisputeCompleteCanonicalReplay(l1SlotConfig);

const decide = async () => {
  const fixture = await plutusExecutionBlockFixture("failing");
  const decision = await replayer.replay(fixture.evidence, fixture.context);
  const detections = requireCompleteCanonicalReplayDecision({
    evidence: fixture.evidence,
    replayer,
    decision,
    context: fixture.context,
  });
  const classification = await classifyCanonicalBlockViolations({
    evidence: fixture.evidence,
    detections,
  });
  return { fixture, detections, classification };
};

describe("validation-trace dispute replay: an evaluator error holds its event", () => {
  afterEach(() => {
    fault.verdict = undefined;
    fault.trace = undefined;
  });

  it("detects the failed script run when the evaluator decides", async () => {
    const { detections, classification } = await decide();
    expect(detections.map(({ violationId }) => violationId)).toStrictEqual([
      "validation-trace",
    ]);
    expect(classification.decision).toBe("fault_detected");
  });

  it("holds the event as a gap when the verdict replay has no verdict", async () => {
    const { fixture } = await decide();
    fault.verdict = new ScriptEvaluationIndeterminateError(
      Buffer.from(fixture.tx.txId, "hex"),
      "spend: Plutus evaluation stopped without a verdict",
      { cause: new RangeError("Array buffer allocation failed") },
    );
    const { detections, classification } = await decide();
    expect(detections).toStrictEqual([
      expect.objectContaining({
        violationId: HOLD,
        position: 0n,
        detectionId: `${HOLD}:0:${fixture.tx.txId}`,
        diagnostic: `script re-execution of accepted transaction ${fixture.tx.txId} has no result: ${fault.verdict.message}`,
      }),
    ]);
    expect(classification).toMatchObject({
      decision: "unprovable_gap",
      selected: { violationId: HOLD },
    });
  });

  it("holds the event as a gap when the trace build fails without stopping", async () => {
    const { fixture } = await decide();
    fault.trace = new RangeError("Array buffer allocation failed");
    const { detections, classification } = await decide();
    expect(detections).toStrictEqual([
      expect.objectContaining({
        violationId: HOLD,
        position: 0n,
        diagnostic: `script re-execution of accepted transaction ${fixture.tx.txId} has no result: Array buffer allocation failed`,
      }),
    ]);
    expect(classification).toMatchObject({ decision: "unprovable_gap" });
  });
});
