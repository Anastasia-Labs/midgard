/** The work held validation-trace re-executions cost is bounded: one replay
 * pass re-executes at most one held event, carrying the rest to the next
 * pass; a hold forgotten past the replayer's capacity keeps its backoff; and
 * an event that fails the same way five times in a row is stuck, a readiness
 * reason of its own raised with exactly one alert, still retried at the
 * capped backoff and never a verdict, until it re-executes to an outcome.
 * Evaluator failures are injected at the validation package, per
 * transaction. */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { ScriptEvaluationIndeterminateError } from "@al-ft/midgard-validation";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import {
  VALIDATION_TRACE_REPLAY_HELD,
  VALIDATION_TRACE_REPLAY_STUCK,
  ValidationTraceReplayHeldError,
  type ValidationTraceReplayHold,
} from "../src/validation-trace-dispute/replay-hold.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import {
  type CompleteCanonicalReplayContext,
  createValidationTraceDisputeCompleteCanonicalReplay,
  requireCompleteCanonicalReplayDecision,
} from "../src/workflow/complete-replay.js";
import { plutusExecutionBlockFixture } from "./helpers/plutus-execution-block-fixture.js";
import { plutusExecutionEventsBlockFixture } from "./helpers/plutus-execution-events-fixture.js";

/** Per transaction (canonical CBOR hex): the injected verdict failure, and
 * how often the verdict replay ran. */
const injected = vi.hoisted(() => ({
  faults: new Map<string, Error>(),
  verdictRuns: new Map<string, number>(),
}));

vi.mock("@al-ft/midgard-validation", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@al-ft/midgard-validation")>();
  const { Effect } = await import("effect");
  return {
    ...actual,
    replayValidationVerdictEvent: (
      input: Parameters<typeof actual.replayValidationVerdictEvent>[0],
    ) => {
      const key = Buffer.from(input.canonicalTransactionCbor).toString("hex");
      injected.verdictRuns.set(key, (injected.verdictRuns.get(key) ?? 0) + 1);
      const fault = injected.faults.get(key);
      return fault === undefined
        ? actual.replayValidationVerdictEvent(input)
        : Effect.fail(fault);
    },
  };
});

const l1SlotConfig = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;

type Block = Readonly<{
  evidence: CanonicalBlockEvidence;
  context: CompleteCanonicalReplayContext;
  txs: readonly Readonly<{ txId: string }>[];
}>;

const oneEventBlock = async (): Promise<Block> => {
  const { evidence, context, tx } =
    await plutusExecutionBlockFixture("failing");
  return { evidence, context, txs: [tx] };
};

/** The committed CBOR of the block's `position`th L2 transaction. */
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

const inject = (block: Block, position: number, error: Error) => {
  injected.faults.set(cborAt(block, position), error);
};
const verdictRuns = (block: Block, position: number) =>
  injected.verdictRuns.get(cborAt(block, position)) ?? 0;

const indeterminate = (block: Block, position: number, message?: string) =>
  new ScriptEvaluationIndeterminateError(
    Buffer.from(block.txs[position]!.txId, "hex"),
    message ?? "spend: Plutus evaluation stopped without a verdict",
    { cause: new RangeError("Array buffer allocation failed") },
  );

/** A replayer on a clock the test moves, its alerts recorded. */
const clockedReplayer = (capacity?: number) => {
  const clock = { now: 0 };
  const alerts: ValidationTraceReplayHold[] = [];
  return {
    clock,
    alerts,
    replayer: createValidationTraceDisputeCompleteCanonicalReplay(
      l1SlotConfig,
      {
        now: () => clock.now,
        onStuck: (hold) => alerts.push(hold),
        ...(capacity === undefined ? {} : { capacity }),
      },
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

describe("validation-trace dispute replay: the work held events cost is bounded", () => {
  afterEach(() => {
    injected.faults.clear();
    injected.verdictRuns.clear();
  });

  it("re-executes at most one held event per pass and carries the rest to the next", async () => {
    const block = await plutusExecutionEventsBlockFixture([
      "failing",
      "failing",
    ]);
    const { clock, replayer } = clockedReplayer();
    inject(block, 0, indeterminate(block, 0));
    inject(block, 1, indeterminate(block, 1));
    // A block's first replay runs every event once.
    const first = await heldBy(replayer, block);
    expect([verdictRuns(block, 0), verdictRuns(block, 1)]).toStrictEqual([
      1, 1,
    ]);
    clock.now = Math.max(...first.holds.map(({ retryAtMs }) => retryAtMs));
    // Both are due; one re-executes, the other is carried, still held.
    const second = await heldBy(replayer, block);
    expect([verdictRuns(block, 0), verdictRuns(block, 1)]).toStrictEqual([
      2, 1,
    ]);
    expect(second.holds).toStrictEqual([
      expect.objectContaining({ txId: block.txs[0]!.txId, attempts: 2 }),
      expect.objectContaining({ txId: block.txs[1]!.txId, attempts: 1 }),
    ]);
    // The carried event is due at once, on the next pass.
    expect(second.retryAtMs).toBeLessThanOrEqual(clock.now);
    await heldBy(replayer, block);
    expect([verdictRuns(block, 0), verdictRuns(block, 1)]).toStrictEqual([
      2, 2,
    ]);
  });

  it("keeps a forgotten hold's retry time and attempt count past the replayer's capacity", async () => {
    const first = await oneEventBlock();
    const second = await plutusExecutionEventsBlockFixture([
      "passing",
      "failing",
    ]);
    const { clock, replayer } = clockedReplayer(1);
    inject(first, 0, indeterminate(first, 0));
    inject(second, 1, indeterminate(second, 1));
    expect((await heldBy(replayer, first)).holds[0]).toMatchObject({
      attempts: 1,
      retryAtMs: 1_000,
    });
    // The second block's hold takes the only place.
    await heldBy(replayer, second);
    clock.now = 500;
    expect((await heldBy(replayer, first)).holds[0]).toMatchObject({
      txId: first.txs[0]!.txId,
      attempts: 1,
      retryAtMs: 1_000,
    });
    expect(verdictRuns(first, 0)).toBe(1);
    clock.now = 1_000;
    expect((await heldBy(replayer, first)).holds[0]).toMatchObject({
      attempts: 2,
      retryAtMs: 3_000,
    });
    expect(verdictRuns(first, 0)).toBe(2);
  });

  it("names an event stuck after five identical failures, alerts once, keeps retrying at the cap, and clears on a verdict", async () => {
    const block = await oneEventBlock();
    const { clock, alerts, replayer } = clockedReplayer();
    inject(block, 0, indeterminate(block, 0));
    const delays: number[] = [];
    for (let attempt = 1; attempt <= 9; attempt += 1) {
      const held = await heldBy(replayer, block);
      expect(verdictRuns(block, 0)).toBe(attempt);
      expect(held.holds[0]).toMatchObject({ attempts: attempt });
      expect(held.readiness).toBe(
        attempt >= 5
          ? VALIDATION_TRACE_REPLAY_STUCK
          : VALIDATION_TRACE_REPLAY_HELD,
      );
      expect(alerts).toHaveLength(attempt >= 5 ? 1 : 0);
      delays.push(held.retryAtMs - clock.now);
      clock.now = held.retryAtMs;
    }
    expect(delays.slice(-3)).toStrictEqual([60_000, 60_000, 60_000]);
    expect(alerts[0]).toMatchObject({
      txId: block.txs[0]!.txId,
      attempts: 5,
    });
    injected.faults.clear();
    expect(await decide(replayer, block)).toMatchObject({
      decision: "fault_detected",
      selected: { violationId: "validation-trace", position: 0n },
    });
    // Cleared: a fresh failure is held again, not stuck.
    inject(block, 0, indeterminate(block, 0));
    const again = await heldBy(replayer, block);
    expect(again.readiness).toBe(VALIDATION_TRACE_REPLAY_HELD);
    expect(again.holds[0]).toMatchObject({ attempts: 1 });
    expect(alerts).toHaveLength(1);
  });

  it("never names an event stuck while its failures differ", async () => {
    const block = await oneEventBlock();
    const { clock, alerts, replayer } = clockedReplayer();
    for (let attempt = 1; attempt <= 8; attempt += 1) {
      inject(
        block,
        0,
        indeterminate(
          block,
          0,
          attempt % 2 === 0
            ? "spend: Plutus evaluation stopped without a verdict"
            : "spend: Plutus evaluation exhausted its heap",
        ),
      );
      const held = await heldBy(replayer, block);
      expect(held.holds[0]).toMatchObject({ attempts: attempt });
      expect(held.readiness).toBe(VALIDATION_TRACE_REPLAY_HELD);
      clock.now = held.retryAtMs;
    }
    expect(alerts).toStrictEqual([]);
  });
});
