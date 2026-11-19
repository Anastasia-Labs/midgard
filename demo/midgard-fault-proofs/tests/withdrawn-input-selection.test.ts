/**
 * Which family the watcher proves when a transaction spends or references
 * an L2 output a valid withdrawal of the same block withdrew (decision of
 * 2026-11-17, by the precedent of decisions 0009 and 0010).
 *
 * The withdrawal removes the output before the transaction's event, so the
 * ledger-absence family also finds it absent from the event's pre-state
 * ledger at the same event. The classifier must select the withdrawn
 * family, whose artifact the watcher then prepares. An input an earlier
 * transaction spent has no withdrawn finding and stays with the
 * ledger-absence family.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  type CanonicalBlockClassification,
  classifyCanonicalBlockViolations,
} from "../src/workflow/classification.classify-canonical-block-violations.js";
import {
  WITHDRAWN_INPUT_COMPLETE_CANONICAL_REPLAY,
  WITHDRAWN_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.create-complete-canonical-replay-union.js";
import {
  NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
  NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.detect-double-withdraws.js";
import { admitWithdrawnInputArtifact } from "../src/workflow/withdrawn-input.admit-withdrawn-input-artifact.js";
import { prepareWithdrawnInputArtifact } from "../src/workflow/withdrawn-input.prepare-withdrawn-input-artifact.js";
import {
  admitWithdrawnReferenceInputArtifact,
  prepareWithdrawnReferenceInputArtifact,
} from "../src/workflow/withdrawn-reference-input.admit-withdrawn-reference-input-artifact.js";
import { withdrawalOf } from "./helpers/event-ledger-seeded-blocks.js";
import { inputExistenceChronologyFixture } from "./helpers/input-existence-chronology-fixture.js";

const REPLAYERS = {
  0: [
    NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
    WITHDRAWN_INPUT_COMPLETE_CANONICAL_REPLAY,
  ],
  1: [
    NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
    WITHDRAWN_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
  ],
} as const;

const WITHDRAWN_CATEGORY = {
  0: "withdrawnInput",
  1: "withdrawnReferenceInput",
} as const;

const LEDGER_ABSENCE_CATEGORY = {
  0: "nonExistentInput",
  1: "noReferenceInput",
} as const;

const VIOLATION_IDS = {
  0: [SDK.WITHDRAWN_INPUT_VIOLATION_ID, "non-existent-input"],
  1: [SDK.WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID, "no-reference-input"],
} as const;

const classifyWithLedger = async (
  block: Awaited<ReturnType<typeof inputExistenceChronologyFixture>>,
  field: 0 | 1,
) => {
  const detections = [];
  for (const replayer of REPLAYERS[field])
    detections.push(
      ...(await replayer.replay(block.evidence, block.context)).detections,
    );
  return {
    violationIds: [...new Set(detections.map((d) => d.violationId))].sort(),
    classification: await classifyCanonicalBlockViolations({
      evidence: block.evidence,
      detections,
    }),
  };
};

/** The decision and what it selected, so a wrong verdict reads plainly. */
const verdict = (classification: CanonicalBlockClassification) => ({
  decision: classification.decision,
  selected:
    classification.decision === "fault_detected"
      ? classification.category
      : classification.decision === "unprovable_gap"
        ? classification.selected
        : null,
});

const fault = (classification: CanonicalBlockClassification) => {
  if (classification.decision !== "fault_detected")
    throw new Error("classification found no fault");
  return classification;
};

describe("an input a same-block withdrawal withdrew", () => {
  for (const field of [0, 1] as const) {
    it(`selects ${WITHDRAWN_CATEGORY[field]} over ${LEDGER_ABSENCE_CATEGORY[field]} and prepares its artifact`, async () => {
      // The seed output 0x51…51#0, withdrawn by a valid withdrawal.
      const block = await inputExistenceChronologyFixture(
        "withdrawal_consumed",
        field,
        undefined,
        withdrawalOf(0x51, true).committed,
      );
      const { violationIds, classification } = await classifyWithLedger(
        block,
        field,
      );
      // Both families find the input at the same event.
      expect(violationIds).toEqual([...VIOLATION_IDS[field]].sort());
      expect(verdict(classification)).toEqual({
        decision: "fault_detected",
        selected: WITHDRAWN_CATEGORY[field],
      });
      const input = {
        evidence: block.evidence,
        classification: fault(classification),
      };
      const withdrawn =
        field === 0
          ? await admitWithdrawnInputArtifact(
              await prepareWithdrawnInputArtifact(input),
            )
          : await admitWithdrawnReferenceInputArtifact(
              await prepareWithdrawnReferenceInputArtifact(input),
            );
      expect(withdrawn.withdrawalMembership.value).toMatchObject({
        validity: "WithdrawalIsValid",
        body: {
          l2_outref: { transactionId: "51".repeat(32), outputIndex: 0n },
        },
      });
    });

    it(`keeps ${LEDGER_ABSENCE_CATEGORY[field]} for an absent input no withdrawal names`, async () => {
      // An earlier transaction of the block spent the input.
      const block = await inputExistenceChronologyFixture(
        "normal_spend",
        field,
      );
      const { violationIds, classification } = await classifyWithLedger(
        block,
        field,
      );
      expect(violationIds).toEqual([VIOLATION_IDS[field][1]]);
      expect(verdict(classification)).toEqual({
        decision: "fault_detected",
        selected: LEDGER_ABSENCE_CATEGORY[field],
      });
    });
  }
});
