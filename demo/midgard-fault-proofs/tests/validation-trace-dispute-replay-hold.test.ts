/** A script re-execution that ends in an error rather than a verdict or a
 * trace (the evaluator's own failure, such as an exhausted heap) says nothing
 * about the script: it holds only its own event, retried after a doubling
 * backoff, and never classifies the block. Only a deterministic outcome
 * without a verdict, a trace replay that disagrees, is an unprovable gap.
 * Evaluator failures are injected at the validation package, per
 * transaction; with none, each block replays as it really does. */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import {
  DirectValidationTraceUnavailable,
  RejectCodes,
  ScriptEvaluationIndeterminateError,
  ValidationTraceStopped,
} from "@al-ft/midgard-validation";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import { EventLedgerReplayRefusal } from "../src/input-existence/event-ledger.js";
import {
  prepareValidationTraceArtifact,
  validateValidationTraceArtifact,
} from "../src/validation-trace-dispute/detected-claim.js";
import {
  VALIDATION_TRACE_REPLAY_HELD,
  ValidationTraceReplayHeldError,
} from "../src/validation-trace-dispute/replay-hold.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import { EVENT_PRE_STATE_UNAVAILABLE_VIOLATION_ID } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import {
  type CompleteCanonicalReplayContext,
  createValidationTraceDisputeCompleteCanonicalReplay,
  requireCompleteCanonicalReplayDecision,
} from "../src/workflow/complete-replay.js";
import { plutusExecutionBlockFixture } from "./helpers/plutus-execution-block-fixture.js";
import { plutusExecutionEventsBlockFixture } from "./helpers/plutus-execution-events-fixture.js";

type Fault = { stage: "verdict" | "trace"; error: Error; remaining: number };

/** Per transaction (canonical CBOR hex): the injected failure, and how often
 * each replay stage ran. */
const injected = vi.hoisted(() => ({
  faults: new Map<string, Fault>(),
  verdictRuns: new Map<string, number>(),
  /** Per txId: the failure the event's pre-state read meets. */
  ledgerFaults: new Map<string, Error>(),
}));

vi.mock("@al-ft/midgard-validation", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@al-ft/midgard-validation")>();
  const { Effect } = await import("effect");
  const failure = (stage: Fault["stage"], cbor: Uint8Array) => {
    const fault = injected.faults.get(Buffer.from(cbor).toString("hex"));
    if (fault?.stage !== stage || fault.remaining <= 0) return undefined;
    fault.remaining -= 1;
    return fault.error;
  };
  return {
    ...actual,
    replayValidationVerdictEvent: (
      input: Parameters<typeof actual.replayValidationVerdictEvent>[0],
    ) => {
      const key = Buffer.from(input.canonicalTransactionCbor).toString("hex");
      injected.verdictRuns.set(key, (injected.verdictRuns.get(key) ?? 0) + 1);
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

vi.mock("../src/input-existence/event-ledger.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/input-existence/event-ledger.js")
    >();
  return {
    ...actual,
    createEventLedgerWalk: (
      input: Parameters<typeof actual.createEventLedgerWalk>[0],
    ) => {
      const walk = actual.createEventLedgerWalk(input);
      return {
        before: async (
          ...[eventKey, stepIndex]: Parameters<typeof walk.before>
        ) => {
          const { L2TransactionEventKey: key } = eventKey as {
            readonly L2TransactionEventKey?: { readonly tx_id: string };
          };
          const fault =
            key === undefined
              ? undefined
              : injected.ledgerFaults.get(key.tx_id);
          if (fault !== undefined) throw fault;
          return await walk.before(eventKey, stepIndex);
        },
      };
    },
  };
});

const l1SlotConfig = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;
const DISAGREEMENT = "validation-trace-replay-disagreement";

/** A committed block and its L2 transactions in the order the block
 * applies them. */
type Block = Readonly<{
  evidence: CanonicalBlockEvidence;
  context: CompleteCanonicalReplayContext;
  txs: readonly Readonly<{ txId: string }>[];
}>;

const oneEventBlock = async () => {
  const { evidence, context, tx } =
    await plutusExecutionBlockFixture("failing");
  return { evidence, context, txs: [tx] };
};

/** The block's transactions in applied order, by txId and committed CBOR. */
const transactions = (block: Block) =>
  block.txs.map(({ txId }) => {
    const event = block.evidence.reconstruction.sourceEvents.find(
      (source) =>
        source.phase === "L2Transaction" && source.entry.txId === txId,
    );
    if (event?.phase !== "L2Transaction")
      throw new Error(`block commits no transaction ${txId}`);
    return {
      txId,
      cbor: Buffer.from(event.entry.fullTransactionCbor).toString("hex"),
    };
  });

/** The transaction a detection names. */
const txIdOf = ({ detectionId }: { readonly detectionId: string }) =>
  detectionId.split(":").at(-1);

const inject = (
  block: Block,
  position: number,
  stage: Fault["stage"],
  error: Error,
  remaining = Number.POSITIVE_INFINITY,
) => {
  injected.faults.set(transactions(block)[position]!.cbor, {
    stage,
    error,
    remaining,
  });
};
const verdictRuns = (block: Block, position: number) =>
  injected.verdictRuns.get(transactions(block)[position]!.cbor) ?? 0;

/** A replayer on a clock the test moves. */
const clockedReplayer = () => {
  const clock = { now: 0 };
  return {
    clock,
    replayer: createValidationTraceDisputeCompleteCanonicalReplay(
      l1SlotConfig,
      { now: () => clock.now },
    ),
  };
};
type Replayer = ReturnType<typeof clockedReplayer>["replayer"];

const decide = async (replayer: Replayer, block: Block) => {
  const decision = await replayer.replay(block.evidence, block.context);
  const detections = requireCompleteCanonicalReplayDecision({
    evidence: block.evidence,
    replayer,
    decision,
    context: block.context,
  });
  return await classifyCanonicalBlockViolations({
    evidence: block.evidence,
    detections,
  });
};

/** The replay's rejection, which must be the hold. */
const heldBy = async (replayer: Replayer, block: Block) => {
  const error: unknown = await decide(replayer, block).then(
    (classification) => {
      throw new Error(`replay decided ${classification.decision}`);
    },
    (rejection: unknown) => rejection,
  );
  expect(error).toBeInstanceOf(ValidationTraceReplayHeldError);
  return error as ValidationTraceReplayHeldError;
};

const indeterminate = (block: Block, position: number) =>
  new ScriptEvaluationIndeterminateError(
    Buffer.from(transactions(block)[position]!.txId, "hex"),
    "spend: Plutus evaluation stopped without a verdict",
    { cause: new RangeError("Array buffer allocation failed") },
  );

const rejectedStop = (
  reason: "disagreement" | "unavailable",
  message: string,
  cause?: unknown,
) =>
  new ValidationTraceStopped(
    reason,
    {
      expectedVerdict: "rejected",
      expectedRejectionCode: RejectCodes.PlutusScriptInvalid,
    },
    message,
    cause === undefined ? undefined : { cause },
  );

describe("validation-trace dispute replay: an evaluator failure holds only its event", () => {
  afterEach(() => {
    injected.faults.clear();
    injected.verdictRuns.clear();
    injected.ledgerFaults.clear();
  });

  it("detects the failed script run when the evaluator decides", async () => {
    const block = await oneEventBlock();
    const classification = await decide(clockedReplayer().replayer, block);
    expect(classification).toMatchObject({
      decision: "fault_detected",
      selected: { violationId: "validation-trace", position: 0n },
    });
  });

  it("holds the event, never a gap, when the verdict replay has no verdict", async () => {
    const block = await oneEventBlock();
    const { clock, replayer } = clockedReplayer();
    clock.now = 5_000;
    const error = indeterminate(block, 0);
    inject(block, 0, "verdict", error);
    const held = await heldBy(replayer, block);
    expect(held).toMatchObject({
      readiness: VALIDATION_TRACE_REPLAY_HELD,
      category: "validationTraceDispute",
      headerHash: block.evidence.headerHash,
      retryAtMs: 6_000,
      holds: [
        {
          headerHash: block.evidence.headerHash,
          position: 0,
          txId: block.txs[0]!.txId,
          attempts: 1,
          retryAtMs: 6_000,
          detail: error.message,
        },
      ],
    });
  });

  it("holds the event when the trace build stops as unavailable", async () => {
    const block = await oneEventBlock();
    const { replayer } = clockedReplayer();
    // What the trace builder makes of any throw, an exhausted heap included.
    inject(
      block,
      0,
      "trace",
      rejectedStop(
        "unavailable",
        "Array buffer allocation failed",
        new RangeError("Array buffer allocation failed"),
      ),
    );
    const held = await heldBy(replayer, block);
    expect(held.holds).toStrictEqual([
      expect.objectContaining({ position: 0, attempts: 1 }),
    ]);
  });

  it("holds the event when its pre-state read fails, then proves it once the read succeeds", async () => {
    const block = await oneEventBlock();
    const { clock, replayer } = clockedReplayer();
    injected.ledgerFaults.set(
      block.txs[0]!.txId,
      new Error("ledger store read failed: connection reset"),
    );
    const held = await heldBy(replayer, block);
    expect(held.holds).toStrictEqual([
      expect.objectContaining({
        txId: block.txs[0]!.txId,
        attempts: 1,
        detail: "ledger store read failed: connection reset",
      }),
    ]);
    expect(verdictRuns(block, 0)).toBe(0);
    injected.ledgerFaults.clear();
    clock.now = held.retryAtMs;
    expect(await decide(replayer, block)).toMatchObject({
      decision: "fault_detected",
      selected: { violationId: "validation-trace" },
    });
  });

  it("classifies a refused pre-state as an unprovable gap", async () => {
    const block = await oneEventBlock();
    injected.ledgerFaults.set(
      block.txs[0]!.txId,
      new EventLedgerReplayRefusal("transaction_input_absent"),
    );
    expect(await decide(clockedReplayer().replayer, block)).toMatchObject({
      decision: "unprovable_gap",
      selected: { violationId: EVENT_PRE_STATE_UNAVAILABLE_VIOLATION_ID },
    });
  });

  it.each([
    [
      "a trace that disagrees with the verdict",
      rejectedStop("disagreement", "trace replay ended Accepted"),
    ],
    [
      "a rejection only a direct proof shows",
      new DirectValidationTraceUnavailable(RejectCodes.InvalidFieldType),
    ],
  ])("classifies %s as an unprovable gap", async (_, stop) => {
    const block = await oneEventBlock();
    inject(block, 0, "trace", stop);
    const classification = await decide(clockedReplayer().replayer, block);
    expect(classification).toMatchObject({
      decision: "unprovable_gap",
      selected: { violationId: DISAGREEMENT, position: 0n },
    });
  });

  it("retries a held event only after a doubling, capped backoff, then proves it", async () => {
    const block = await oneEventBlock();
    const { clock, replayer } = clockedReplayer();
    inject(block, 0, "verdict", indeterminate(block, 0));
    const delays: number[] = [];
    for (let attempt = 1; attempt <= 8; attempt += 1) {
      const held = await heldBy(replayer, block);
      expect(held.holds[0]).toMatchObject({ attempts: attempt });
      expect(verdictRuns(block, 0)).toBe(attempt);
      delays.push(held.retryAtMs - clock.now);
      // Before the retry time the event is not re-executed.
      clock.now = held.retryAtMs - 1;
      expect((await heldBy(replayer, block)).holds[0]).toMatchObject({
        attempts: attempt,
      });
      expect(verdictRuns(block, 0)).toBe(attempt);
      clock.now = held.retryAtMs;
    }
    expect(delays).toStrictEqual([
      1_000, 2_000, 4_000, 8_000, 16_000, 32_000, 60_000, 60_000,
    ]);
    injected.faults.clear();
    expect(await decide(replayer, block)).toMatchObject({
      decision: "fault_detected",
      selected: { violationId: "validation-trace", position: 0n },
    });
    // Decided, so a fresh failure starts the backoff over.
    inject(block, 0, "verdict", indeterminate(block, 0), 1);
    expect((await heldBy(replayer, block)).holds[0]).toMatchObject({
      attempts: 1,
      retryAtMs: clock.now + 1_000,
    });
  });

  it("proves a second failing event while the first is held, and keeps its claim after the first resolves", async () => {
    const block = await plutusExecutionEventsBlockFixture([
      "failing",
      "failing",
    ]);
    const undisturbed = await decide(clockedReplayer().replayer, block);
    expect(undisturbed).toMatchObject({ decision: "fault_detected" });
    if (undisturbed.decision !== "fault_detected") return;
    // Ordered as the block applies its events.
    expect(
      undisturbed.detections.map((detection) => [
        detection.violationId,
        txIdOf(detection),
      ]),
    ).toStrictEqual([
      ["validation-trace", block.txs[0]!.txId],
      ["validation-trace", block.txs[1]!.txId],
    ]);

    const { clock, replayer } = clockedReplayer();
    inject(block, 0, "verdict", indeterminate(block, 0), 1);
    const whileHeld = await decide(replayer, block);
    expect(whileHeld).toMatchObject({ decision: "fault_detected" });
    if (whileHeld.decision !== "fault_detected") return;
    expect(whileHeld.detections).toStrictEqual([undisturbed.detections[1]]);
    expect(whileHeld.selected).toStrictEqual(undisturbed.detections[1]);
    const claim = {
      evidence: block.evidence,
      context: block.context,
      l1SlotConfig,
    };
    const artifact = await prepareValidationTraceArtifact({
      ...claim,
      classification: whileHeld,
    });
    expect(artifact).toStrictEqual(
      await prepareValidationTraceArtifact({
        ...claim,
        classification: {
          ...undisturbed,
          selected: undisturbed.detections[1]!,
        },
      }),
    );

    // The first event resolves: it is now the selected fault, and the claim
    // already started on the second still stands.
    clock.now = 1_000;
    const resolved = await decide(replayer, block);
    expect(resolved).toStrictEqual(undisturbed);
    if (resolved.decision !== "fault_detected") return;
    await expect(
      validateValidationTraceArtifact({
        ...claim,
        classification: resolved,
        artifact,
      }),
    ).resolves.toBeUndefined();
    // A claim on an event this replay does not detect is refused.
    await expect(
      validateValidationTraceArtifact({
        ...claim,
        classification: {
          ...resolved,
          detections: [resolved.detections[0]!],
        },
        artifact,
      }),
    ).rejects.toThrow(
      "validation-trace artifact differs from this evidence's replay",
    );
  });

  it("still re-executes a passing second event while the first is held, and waits on the first", async () => {
    const block = await plutusExecutionEventsBlockFixture([
      "failing",
      "passing",
    ]);
    const { clock, replayer } = clockedReplayer();
    inject(block, 0, "verdict", indeterminate(block, 0), 1);
    const held = await heldBy(replayer, block);
    expect(held.holds).toStrictEqual([
      expect.objectContaining({ txId: block.txs[0]!.txId, attempts: 1 }),
    ]);
    expect(verdictRuns(block, 1)).toBe(1);
    clock.now = held.retryAtMs;
    const decided = await decide(replayer, block);
    expect(decided).toMatchObject({ decision: "fault_detected" });
    if (decided.decision !== "fault_detected") return;
    expect(decided.detections.map(txIdOf)).toStrictEqual([block.txs[0]!.txId]);
  });
});
