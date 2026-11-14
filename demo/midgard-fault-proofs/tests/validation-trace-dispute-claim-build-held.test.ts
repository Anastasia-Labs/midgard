/** A validation-trace claim whose pre-state read fails: a read that fails
 * without a result holds the claim build, while a refusal of the block's own
 * committed steps still decides it; and a claim whose trace replay
 * deterministically disagrees is refused, not held. Read failures are
 * injected at the event-ledger walk, trace outcomes at the validation
 * package. */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { RejectCodes, ValidationTraceStopped } from "@al-ft/midgard-validation";
import { beforeAll, describe, expect, it, vi } from "vitest";

import { EventLedgerReplayRefusal } from "../src/event-ledger.js";
import { encodeValidationTraceDisputeArtifact } from "../src/validation-trace-dispute/artifact.js";
import {
  buildValidationTraceClaimMaterial,
  VALIDATION_TRACE_CLAIM_BUILD_HELD,
} from "../src/validation-trace-dispute/claim-material.js";
import { prepareValidationTraceArtifact } from "../src/validation-trace-dispute/detected-claim.js";
import {
  type CanonicalBlockClassification,
  classifyCanonicalBlockViolations,
} from "../src/workflow/classification.js";
import {
  completeCanonicalReplayPredecessorLedger,
  createValidationTraceDisputeCompleteCanonicalReplay,
  requireCompleteCanonicalReplayDecision,
} from "../src/workflow/complete-replay.js";
import { WorkflowArtifactPendingError } from "../src/workflow/workflow-artifact-pending.js";
import { referenceLedgerBefore } from "./helpers/event-ledger-reference.js";
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

vi.mock("../src/event-ledger.js", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("../src/event-ledger.js")>();
  return {
    ...actual,
    eventLedgerBefore: async (
      input: Parameters<typeof actual.eventLedgerBefore>[0],
    ) => {
      const fault = injected.next;
      injected.next = undefined;
      if (fault !== undefined) throw fault;
      return await actual.eventLedgerBefore(input);
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

/** The claim prepared for the block's failing event, after detection. */
const prepareClaim = async () =>
  await prepareValidationTraceArtifact({
    evidence: block.evidence,
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

  it("builds the claim of the event's own pre-state when the read succeeds", async () => {
    const event = block.evidence.reconstruction.sourceEvents.find(
      ({ fingerprint }) =>
        fingerprint === classification.selected.subjectEventKeyCbors[0],
    );
    if (event?.phase !== "L2Transaction") throw new Error("not a transaction");
    const ledger = await referenceLedgerBefore({
      block: block.evidence,
      predecessor: completeCanonicalReplayPredecessorLedger(block),
      eventKey: event.eventKey,
      stepIndex: block.evidence.reconstruction.eventToStepByFingerprint.get(
        event.fingerprint,
      )!.value.step_index,
    });
    expect(await prepareClaim()).toStrictEqual(
      encodeValidationTraceDisputeArtifact({
        headerHash: block.evidence.headerHash,
        material: await buildValidationTraceClaimMaterial({
          evidence: block.evidence,
          event,
          ledger,
          l1SlotConfig,
        }),
      }),
    );
  });

  it("holds the claim build when the read fails without a result", async () => {
    injected.next = new RangeError("Array buffer allocation failed");
    const error: unknown = await prepareClaim().then(
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
    const refusal = new EventLedgerReplayRefusal("transition_link_changed", 0n);
    injected.next = refusal;
    await expect(prepareClaim()).rejects.toBe(refusal);
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
    const error: unknown = await prepareClaim().then(
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
