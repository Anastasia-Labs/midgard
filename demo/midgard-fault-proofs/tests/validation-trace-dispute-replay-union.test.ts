/** The validation-trace replayer inside a replay union: a held script
 * re-execution never throws away the findings that already decide the block.
 * A finding strictly earlier than every held event decides it, the union's
 * other members' and the replayer's own alike; one at or after a held event
 * leaves the block waiting on the hold. Evaluator failures are injected at
 * the validation package, per transaction. */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  RejectCodes,
  ScriptEvaluationIndeterminateError,
  ValidationTraceStopped,
} from "@al-ft/midgard-validation";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  VALIDATION_TRACE_REPLAY_HELD,
  ValidationTraceReplayHeldError,
} from "../src/validation-trace-dispute/replay-hold.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import {
  type CompleteCanonicalReplay,
  createCompleteCanonicalReplayUnion,
  createValidationTraceDisputeCompleteCanonicalReplay,
  MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY,
  requireCompleteCanonicalReplayDecision,
} from "../src/workflow/complete-replay.js";
import {
  type EventSpend,
  plutusExecutionEventsBlockFixture,
} from "./helpers/plutus-execution-events-fixture.js";

type Fault = { stage: "verdict" | "trace"; error: Error };

/** Per transaction (canonical CBOR hex): the injected failure. */
const injected = vi.hoisted(() => ({ faults: new Map<string, Fault>() }));

vi.mock("@al-ft/midgard-validation", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@al-ft/midgard-validation")>();
  const { Effect } = await import("effect");
  const failure = (stage: Fault["stage"], cbor: Uint8Array) => {
    const fault = injected.faults.get(Buffer.from(cbor).toString("hex"));
    return fault?.stage === stage ? fault.error : undefined;
  };
  return {
    ...actual,
    replayValidationVerdictEvent: (
      input: Parameters<typeof actual.replayValidationVerdictEvent>[0],
    ) => {
      const error = failure("verdict", input.canonicalTransactionCbor);
      return error === undefined
        ? actual.replayValidationVerdictEvent(input)
        : Effect.fail(error);
    },
    commitValidationTraceV1: (
      input: Parameters<typeof actual.commitValidationTraceV1>[0],
    ) => {
      const error = failure("trace", input.canonicalTransactionCbor);
      return error === undefined
        ? actual.commitValidationTraceV1(input)
        : Effect.fail(error);
    },
  };
});

const l1SlotConfig = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;

type Block = Awaited<ReturnType<typeof plutusExecutionEventsBlockFixture>>;

const blockOf = async (spends: readonly EventSpend[]) =>
  await plutusExecutionEventsBlockFixture(spends);

const cborAt = (block: Block, position: number) => {
  const event = block.evidence.reconstruction.sourceEvents.find(
    (source) =>
      source.phase === "L2Transaction" &&
      source.entry.txId === block.txs[position]!.txId,
  );
  if (event?.phase !== "L2Transaction")
    throw new Error(`block commits no transaction at ${position.toString()}`);
  return Buffer.from(event.entry.fullTransactionCbor).toString("hex");
};

/** The `position`th transaction's verdict replay ends without a verdict. */
const holdAt = (block: Block, position: number) => {
  injected.faults.set(cborAt(block, position), {
    stage: "verdict",
    error: new ScriptEvaluationIndeterminateError(
      Buffer.from(block.txs[position]!.txId, "hex"),
      "spend: Plutus evaluation stopped without a verdict",
      { cause: new RangeError("Array buffer allocation failed") },
    ),
  });
};

/** The `position`th transaction's trace replay deterministically disagrees
 * with its failed verdict. */
const disagreeAt = (block: Block, position: number) => {
  injected.faults.set(cborAt(block, position), {
    stage: "trace",
    error: new ValidationTraceStopped(
      "disagreement",
      {
        expectedVerdict: "rejected",
        expectedRejectionCode: RejectCodes.PlutusScriptInvalid,
      },
      "the trace replay ended Accepted",
    ),
  });
};

/** A union of the validation-trace replayer and `others`, on a clock the
 * test moves. */
const unionOf = (...others: readonly CompleteCanonicalReplay[]) => {
  const clock = { now: 0 };
  return {
    clock,
    union: createCompleteCanonicalReplayUnion([
      createValidationTraceDisputeCompleteCanonicalReplay(l1SlotConfig, {
        now: () => clock.now,
      }),
      ...others,
    ]),
  };
};

const decide = async (union: CompleteCanonicalReplay, block: Block) => {
  const decision = await union.replay(block.evidence, block.context);
  return await classifyCanonicalBlockViolations({
    evidence: block.evidence,
    detections: requireCompleteCanonicalReplayDecision({
      evidence: block.evidence,
      replayer: union,
      decision,
      context: block.context,
    }),
  });
};

const heldBy = async (union: CompleteCanonicalReplay, block: Block) => {
  const error: unknown = await decide(union, block).then(
    (classification) => {
      throw new Error(`replay decided ${classification.decision}`);
    },
    (rejection: unknown) => rejection,
  );
  expect(error).toBeInstanceOf(ValidationTraceReplayHeldError);
  return error as ValidationTraceReplayHeldError;
};

const txIdOf = ({ detectionId }: { readonly detectionId: string }) =>
  detectionId.split(":").at(-1);

describe("validation-trace replay union: a held re-execution keeps the findings that decide the block", () => {
  afterEach(() => {
    injected.faults.clear();
  });

  it("decides the block on another family's finding strictly before the held event", async () => {
    const block = await blockOf(["unsigned_required_signer", "failing"]);
    const { union } = unionOf(MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY);
    holdAt(block, 1);
    const decided = await decide(union, block);
    expect(decided).toMatchObject({
      decision: "fault_detected",
      category: "missingSignature",
    });
    if (decided.decision !== "fault_detected") return;
    // The first transaction's finding; the held second adds none.
    expect(decided.detections).toHaveLength(1);
    expect(decided.selected.detectionId).toContain(block.txs[0]!.txId);
  });

  it("waits on the hold when the held event comes first, then decides on it", async () => {
    const block = await blockOf(["failing", "unsigned_required_signer"]);
    const { clock, union } = unionOf(
      MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY,
    );
    holdAt(block, 0);
    const held = await heldBy(union, block);
    expect(held).toMatchObject({
      readiness: VALIDATION_TRACE_REPLAY_HELD,
      headerHash: block.evidence.headerHash,
      holds: [{ txId: block.txs[0]!.txId, attempts: 1 }],
    });
    injected.faults.clear();
    clock.now = held.retryAtMs;
    const decided = await decide(union, block);
    expect(decided).toMatchObject({
      decision: "fault_detected",
      category: "validationTraceDispute",
    });
    if (decided.decision !== "fault_detected") return;
    expect(txIdOf(decided.selected)).toBe(block.txs[0]!.txId);
  });

  it("waits on the hold when another family's finding is on the held event itself", async () => {
    const block = await blockOf(["unsigned_required_signer"]);
    const { union } = unionOf(MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY);
    holdAt(block, 0);
    const held = await heldBy(union, block);
    expect(held.holds).toStrictEqual([
      expect.objectContaining({ txId: block.txs[0]!.txId }),
    ]);
  });

  it("decides the block on the replayer's own earlier finding while a later event is held", async () => {
    const block = await blockOf(["failing", "failing"]);
    const { union } = unionOf();
    disagreeAt(block, 0);
    holdAt(block, 1);
    const decided = await decide(union, block);
    expect(decided).toMatchObject({ decision: "unprovable_gap" });
    expect(decided.unprovableGaps.map(txIdOf)).toStrictEqual([
      block.txs[0]!.txId,
    ]);
  });

  it("waits on the hold, never a gap, when a later disagreement and a still later failure follow the held event", async () => {
    const block = await blockOf(["failing", "failing", "failing"]);
    const { union } = unionOf();
    holdAt(block, 0);
    disagreeAt(block, 1);
    const held = await heldBy(union, block);
    expect(held.holds).toStrictEqual([
      expect.objectContaining({ txId: block.txs[0]!.txId }),
    ]);
    // The findings the pass did decide travel with the hold.
    expect(
      held.detections.map((detection) => [
        detection.violationId,
        txIdOf(detection),
      ]),
    ).toStrictEqual([
      ["validation-trace-replay-disagreement", block.txs[1]!.txId],
      ["validation-trace", block.txs[2]!.txId],
    ]);
  });

  it("waits on the hold when the replayer's own finding comes after the held event", async () => {
    const block = await blockOf(["failing", "failing"]);
    const { union } = unionOf();
    holdAt(block, 0);
    disagreeAt(block, 1);
    const held = await heldBy(union, block);
    expect(held.holds).toStrictEqual([
      expect.objectContaining({ txId: block.txs[0]!.txId }),
    ]);
  });
});
