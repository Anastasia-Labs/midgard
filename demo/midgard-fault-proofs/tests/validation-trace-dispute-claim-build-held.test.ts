/** A validation-trace claim whose pre-state must be read again (its block
 * was not replayed by this process's detector) and whose read then fails:
 * a read that fails without a result holds the claim build, while a refusal
 * of the block's own committed steps still decides it; and a claim whose
 * trace replay deterministically disagrees is refused, not held. Read
 * failures are injected at the event-ledger walk, trace outcomes at the
 * validation package. */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { RejectCodes, ValidationTraceStopped } from "@al-ft/midgard-validation";
import { beforeAll, describe, expect, it, vi } from "vitest";

import { EventLedgerReplayRefusal } from "../src/input-existence/event-ledger.js";
import { VALIDATION_TRACE_CLAIM_BUILD_HELD } from "../src/validation-trace-dispute/claim-material.js";
import { prepareValidationTraceArtifact } from "../src/validation-trace-dispute/detected-claim.js";
import {
  type CanonicalBlockClassification,
  classifyCanonicalBlockViolations,
} from "../src/workflow/classification.js";
import {
  createValidationTraceDisputeCompleteCanonicalReplay,
  requireCompleteCanonicalReplayDecision,
} from "../src/workflow/complete-replay.js";
import { WorkflowArtifactPendingError } from "../src/workflow/workflow-artifact-pending.js";
import { plutusExecutionEventsBlockFixture } from "./helpers/plutus-execution-events-fixture.js";

const injected = vi.hoisted(() => ({
  next: undefined as Error | undefined,
  open: undefined as Error | undefined,
}));

vi.mock("@al-ft/midgard-validation", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@al-ft/midgard-validation")>();
  const { Effect } = await import("effect");
  return {
    ...actual,
    openValidationTraceV1: (
      ...input: Parameters<typeof actual.openValidationTraceV1>
    ) => {
      const fault = injected.open;
      injected.open = undefined;
      return fault === undefined
        ? actual.openValidationTraceV1(...input)
        : Effect.fail(fault);
    },
  };
});

vi.mock("../src/input-existence/event-ledger.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/input-existence/event-ledger.js")
    >();
  return {
    ...actual,
    ledgerBeforeTransactionEvent: async (
      input: Parameters<typeof actual.ledgerBeforeTransactionEvent>[0],
    ) => {
      const fault = injected.next;
      injected.next = undefined;
      if (fault !== undefined) throw fault;
      return await actual.ledgerBeforeTransactionEvent(input);
    },
  };
});

const l1SlotConfig = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;

type Block = Awaited<ReturnType<typeof plutusExecutionEventsBlockFixture>>;
type Fault = Extract<
  CanonicalBlockClassification,
  { readonly decision: "fault_detected" }
>;

let block: Block;
let classification: Fault;

/** The claim prepared for a copy of the block's evidence, which the
 * detector never replayed, so its pre-state is read again. */
const prepareAgain = async () =>
  await prepareValidationTraceArtifact({
    evidence: { ...block.evidence },
    context: block.context,
    classification,
    l1SlotConfig,
  });

describe("validation-trace claim build: held only when it ends without a result", () => {
  beforeAll(async () => {
    block = await plutusExecutionEventsBlockFixture(["failing"]);
    const replayer =
      createValidationTraceDisputeCompleteCanonicalReplay(l1SlotConfig);
    const decided = await classifyCanonicalBlockViolations({
      evidence: block.evidence,
      detections: requireCompleteCanonicalReplayDecision({
        evidence: block.evidence,
        replayer,
        decision: await replayer.replay(block.evidence, block.context),
        context: block.context,
      }),
    });
    if (decided.decision !== "fault_detected")
      throw new Error(`the block decided ${decided.decision}`);
    classification = decided;
  });

  it("builds the same claim when the read succeeds", async () => {
    expect(await prepareAgain()).toStrictEqual(
      await prepareValidationTraceArtifact({
        evidence: block.evidence,
        context: block.context,
        classification,
        l1SlotConfig,
      }),
    );
  });

  it("holds the claim build when the read fails without a result", async () => {
    injected.next = new RangeError("Array buffer allocation failed");
    const error: unknown = await prepareAgain().then(
      () => {
        throw new Error("the claim was built");
      },
      (rejection: unknown) => rejection,
    );
    expect(injected.next).toBeUndefined();
    expect(error).toBeInstanceOf(WorkflowArtifactPendingError);
    expect(error).toMatchObject({
      namedState: { name: VALIDATION_TRACE_CLAIM_BUILD_HELD, alarm: false },
    });
  });

  it("keeps a refusal of the block's own steps as the build's outcome", async () => {
    const refusal = new EventLedgerReplayRefusal("transition_link_changed");
    injected.next = refusal;
    await expect(prepareAgain()).rejects.toBe(refusal);
  });

  it("refuses a claim whose trace replay deterministically disagrees", async () => {
    injected.open = new ValidationTraceStopped(
      "disagreement",
      {
        expectedVerdict: "rejected",
        expectedRejectionCode: RejectCodes.PlutusScriptInvalid,
      },
      "the trace replay ended Accepted",
    );
    const error: unknown = await prepareAgain().then(
      () => {
        throw new Error("the claim was built");
      },
      (rejection: unknown) => rejection,
    );
    expect(injected.open).toBeUndefined();
    expect(error).toBeInstanceOf(Error);
    expect(error).not.toBeInstanceOf(WorkflowArtifactPendingError);
  });
});
