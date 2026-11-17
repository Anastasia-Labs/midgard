/** A finding the classifier drops never decides a block. A finding of a
 * family that opens only accepted subjects at validity code 0 is dropped when
 * it names a normal leaf the operator committed `TxIsInvalid`, so such a
 * finding neither releases a block a held re-execution holds nor discharges a
 * replay prerequisite: the replay fails closed in both cases. */
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { ValidationTraceReplayHeldError } from "../src/validation-trace-dispute/replay-hold.js";
import { DOUBLE_SPEND_VIOLATION_ID } from "../src/workflow/classification.js";
import { completeReplayer } from "../src/workflow/complete-replay.detect-double-withdraws.js";
import { createCompleteCanonicalReplayUnion } from "../src/workflow/complete-replay.js";
import {
  acceptedTransactionSubject,
  eventOrder,
} from "../src/workflow/detection-subject.js";
import {
  CanonicalReplayPrerequisiteError,
  replayPrerequisiteFailure,
} from "../src/workflow/replay-prerequisite.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

/** A block of normal leaves, every one committed `TxIsInvalid`. */
const invalidTaggedBlock = async (leaves: number) => {
  const transactions = Array.from({ length: leaves }, (_, index) =>
    buildFixtureTransaction({
      validity: "TxIsInvalid",
      spendInputs: [outRefCbor(4 + index, 0n)],
      fee: 1n,
    }),
  );
  const fixture = await buildCanonicalBlockFixture({ transactions });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "test/invalid-tagged-normal-leaf.hold-and-prerequisite",
      grade: "security",
    },
  });
  // The block's source events in event order, each with its leaf's tx id.
  const events = evidence.reconstruction.sourceEvents
    .map((event) => {
      if (!("L2TransactionEventKey" in event.eventKey))
        throw new Error("fixture block holds only normal leaves");
      return {
        eventKey: event.eventKey,
        order: eventOrder(evidence.reconstruction, event.fingerprint),
        txId: event.eventKey.L2TransactionEventKey.tx_id,
      };
    })
    .sort((left, right) => left.order - right.order);
  expect(events).toHaveLength(leaves);
  return { evidence, events };
};

/** A double-spend member whose one finding names `txId`'s normal leaf. */
const doubleSpendFindingAt = (headerHash: string, txId: string) =>
  completeReplayer(["doubleSpend"], async () => [
    {
      ...acceptedTransactionSubject(txId),
      detectionId: `${DOUBLE_SPEND_VIOLATION_ID}:0`,
      headerHash,
      violationId: DOUBLE_SPEND_VIOLATION_ID,
      position: 0n,
    },
  ]);

describe("a finding on a normal leaf committed TxIsInvalid", () => {
  it("does not release a block held at a later event", async () => {
    const { evidence, events } = await invalidTaggedBlock(2);
    const [first, second] = events;
    const held = new ValidationTraceReplayHeldError(
      evidence.headerHash,
      [],
      second!.order,
      [],
    );
    await expect(
      createCompleteCanonicalReplayUnion([
        doubleSpendFindingAt(evidence.headerHash, first!.txId),
        completeReplayer(["validationTraceDispute"], async () => {
          throw held;
        }),
      ]).replay(evidence, {}),
    ).rejects.toBe(held);
  });

  it("does not discharge a replay prerequisite at its own event", async () => {
    const { evidence, events } = await invalidTaggedBlock(1);
    const [only] = events;
    await expect(
      createCompleteCanonicalReplayUnion([
        doubleSpendFindingAt(evidence.headerHash, only!.txId),
        completeReplayer(["transitionTrace"], async () => {
          throw replayPrerequisiteFailure(
            evidence.headerHash,
            only!.eventKey,
            "present_spend_input",
          );
        }),
      ]).replay(evidence, {}),
    ).rejects.toBeInstanceOf(CanonicalReplayPrerequisiteError);
  });
});
