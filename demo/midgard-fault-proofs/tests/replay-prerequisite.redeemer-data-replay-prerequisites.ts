import { encodeMidgardRedeemerWitnessItem } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
  requireCompleteCanonicalReplayDecision,
} from "../src/workflow/complete-replay.js";
import {
  assertReplayPrerequisiteCovered,
  CanonicalReplayPrerequisiteError,
  replayPrerequisiteFailure,
} from "../src/workflow/replay-prerequisite.js";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { ADVISORY_VIOLATION_ID } from "./replay-prerequisite.complete-replay-proof-prerequisites.js";
import { evidenceFor } from "./replay-prerequisite.complete-replay-proof-prerequisites.js";

/** Two normal transactions, the one at `carrier` carrying `redeemerData` in a
 * spend redeemer and the other a canonical one. */
const redeemerDataEvidence = async (redeemerData: string, carrier = 0) => {
  const redeemer = (data: string) =>
    encodeMidgardRedeemerWitnessItem({
      purpose: "Spend",
      index: 0n,
      redeemerCbor: Buffer.from(data, "hex"),
      executionUnits: { memory: 1n, steps: 2n },
    });
  const block = await buildCanonicalBlockFixture({
    transactions: [0, 1].map((index) =>
      buildFixtureTransaction({
        spendInputs: [outRefCbor(0x31 + index, 0n)],
        fee: BigInt(index + 1),
        redeemerWitnesses: [
          redeemer(index === carrier ? redeemerData : "d87980"),
        ],
      }),
    ),
  });
  const evidence = await evidenceFor(block);
  const decision =
    await REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY.replay(evidence);
  const detections = requireCompleteCanonicalReplayDecision({
    evidence,
    replayer: REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
    decision,
  });
  const fieldShape = (position: number) =>
    replayPrerequisiteFailure(
      evidence.headerHash,
      {
        L2TransactionEventKey: {
          tx_id: evidence.transactions[position]!.nodeTxId,
        },
      },
      "representable_field_shape",
    ).failures[0]!;
  return { evidence, detections, fieldShape };
};

describe("redeemer data replay prerequisites", () => {
  it("lets the accepted redeemer-malformed finding cover its own transaction's field shape", async () => {
    const { evidence, detections, fieldShape } =
      await redeemerDataEvidence("d8798101");
    expect(
      detections.map(({ violationId, position, frontier }) => [
        violationId,
        position,
        frontier,
      ]),
    ).toEqual([["redeemer-malformed", 0n, "accepted"]]);
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, fieldShape(0), detections),
    ).not.toThrow();
    // The finding precedes the second transaction, so it covers that too.
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, fieldShape(1), detections),
    ).not.toThrow();
  });

  it("orders a redeemer-malformed finding by its declared subject only", async () => {
    const { evidence, detections, fieldShape } = await redeemerDataEvidence(
      "d8798101",
      1,
    );
    const [finding] = detections;
    expect(finding).toMatchObject({ position: 1n, frontier: "accepted" });
    // A fault at the second transaction cannot speak for the first.
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, fieldShape(0), detections),
    ).toThrow(CanonicalReplayPrerequisiteError);
    // Neither the reported position nor the detection id moves the finding.
    for (const relabelled of [
      { ...finding!, position: 0n },
      {
        ...finding!,
        detectionId: finding!.detectionId
          .replace(":accepted:", ":forced:")
          .replace(
            evidence.transactions[1]!.nodeTxId,
            evidence.transactions[0]!.nodeTxId,
          ),
      },
    ]) {
      expect(() =>
        assertReplayPrerequisiteCovered(evidence, fieldShape(0), [relabelled]),
      ).toThrow(CanonicalReplayPrerequisiteError);
      expect(() =>
        assertReplayPrerequisiteCovered(evidence, fieldShape(1), [relabelled]),
      ).not.toThrow();
    }
    for (const unrelated of [
      { ...finding!, headerHash: "99".repeat(32) },
      { ...finding!, violationId: ADVISORY_VIOLATION_ID },
    ])
      expect(() =>
        assertReplayPrerequisiteCovered(evidence, fieldShape(1), [unrelated]),
      ).toThrow(CanonicalReplayPrerequisiteError);
  });

  it("finds nothing to cover when the redeemer data is canonical", async () => {
    const { evidence, detections, fieldShape } =
      await redeemerDataEvidence("d87980");
    expect(detections).toEqual([]);
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, fieldShape(0), detections),
    ).toThrow(CanonicalReplayPrerequisiteError);
  });
});
