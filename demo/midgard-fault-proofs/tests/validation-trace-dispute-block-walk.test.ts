/** The validation-trace replay of a block with several Plutus events: one
 * walk of the block's committed steps serves every event's pre-state. Each
 * event replays from exactly the ledger a one-off reconstruction gives it, in
 * any request order, and a step whose post-state root its ledger does not
 * open refuses every later event while the earlier ones still decide. */
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { describe, expect, it } from "vitest";

import {
  createEventLedgerWalk,
  EventLedgerReplayRefusal,
  ledgerBeforeTransactionEvent,
} from "../src/input-existence/event-ledger.js";
import { prepareValidationTraceArtifact } from "../src/validation-trace-dispute/detected-claim.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import {
  completeCanonicalReplayPredecessorLedger,
  EVENT_PRE_STATE_UNAVAILABLE_VIOLATION_ID,
} from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import {
  createValidationTraceDisputeCompleteCanonicalReplay,
  requireCompleteCanonicalReplayDecision,
} from "../src/workflow/complete-replay.js";
import {
  type EventSpend,
  plutusExecutionEventsBlockFixture,
} from "./helpers/plutus-execution-events-fixture.js";

const l1SlotConfig = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;

type Block = Awaited<ReturnType<typeof plutusExecutionEventsBlockFixture>>;

const detect = async (block: Block) => {
  const replayer =
    createValidationTraceDisputeCompleteCanonicalReplay(l1SlotConfig);
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

/** The transaction a detection names. */
const txIdOf = ({ detectionId }: { readonly detectionId: string }) =>
  detectionId.split(":").at(-1);

const eventKey = (block: Block, step: number) => ({
  L2TransactionEventKey: { tx_id: block.txs[step]!.txId },
});

const predecessorOf = (block: Block) =>
  completeCanonicalReplayPredecessorLedger({
    evidence: block.evidence,
    context: block.context,
  })!;

const oneOff = async (block: Block, step: number) =>
  await ledgerBeforeTransactionEvent({
    block: block.evidence,
    eventKey: eventKey(block, step),
    stepIndex: BigInt(step),
    predecessor: predecessorOf(block),
  });

const SPENDS: readonly EventSpend[] = [
  "failing",
  "passing",
  "failing",
  "failing",
];

describe("validation-trace dispute replay: one walk per block", () => {
  it("detects every failing event of the block, each from its own pre-state", async () => {
    const block = await plutusExecutionEventsBlockFixture(SPENDS);
    const classification = await detect(block);
    expect(classification).toMatchObject({ decision: "fault_detected" });
    if (classification.decision !== "fault_detected") return;
    expect(
      classification.detections.map((detection) => [
        detection.violationId,
        txIdOf(detection),
      ]),
    ).toStrictEqual(
      [0, 2, 3].map((step) => ["validation-trace", block.txs[step]!.txId]),
    );
    // Each detection's claim rebuilds from a one-off pre-state of its own.
    for (const selected of classification.detections)
      await expect(
        prepareValidationTraceArtifact({
          evidence: block.evidence,
          context: block.context,
          classification: { ...classification, selected },
          l1SlotConfig,
        }),
      ).resolves.toMatchObject({ headerHash: block.evidence.headerHash });
  });

  it("gives each event the one-off pre-state, in any request order", async () => {
    const block = await plutusExecutionEventsBlockFixture(SPENDS);
    const walk = createEventLedgerWalk({
      block: block.evidence,
      predecessor: predecessorOf(block),
    });
    for (const step of [1, 3, 0, 2, 2, 3]) {
      const [walked, expected] = [
        await walk.before(eventKey(block, step), BigInt(step)),
        await oneOff(block, step),
      ];
      expect(walked.utxosRoot).toBe(expected.utxosRoot);
      expect(walked.entries).toStrictEqual(expected.entries);
    }
  });

  it("refuses every event after a step whose committed root its ledger does not open, and decides the earlier ones", async () => {
    const block = await plutusExecutionEventsBlockFixture(
      ["failing", "failing", "failing"],
      { foreignPostRootAt: 1 },
    );
    const classification = await detect(block);
    expect(
      classification.detections.map((detection) => [
        detection.violationId,
        txIdOf(detection),
      ]),
    ).toStrictEqual([
      ["validation-trace", block.txs[0]!.txId],
      ["validation-trace", block.txs[1]!.txId],
      [EVENT_PRE_STATE_UNAVAILABLE_VIOLATION_ID, block.txs[2]!.txId],
    ]);
    expect(classification.detections[2]!.diagnostic).toContain(
      "post_root_changed",
    );
    expect(classification).toMatchObject({
      decision: "fault_detected",
      selected: { violationId: "validation-trace" },
    });

    const walk = createEventLedgerWalk({
      block: block.evidence,
      predecessor: predecessorOf(block),
    });
    for (let attempt = 0; attempt < 2; attempt += 1)
      await expect(walk.before(eventKey(block, 2), 2n)).rejects.toMatchObject({
        reason: "post_root_changed",
      });
    await expect(walk.before(eventKey(block, 2), 2n)).rejects.toBeInstanceOf(
      EventLedgerReplayRefusal,
    );
    expect((await walk.before(eventKey(block, 1), 1n)).utxosRoot).toBe(
      (await oneOff(block, 1)).utxosRoot,
    );
  });
});
