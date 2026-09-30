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
import { evidenceFor } from "./replay-prerequisite.complete-replay-proof-prerequisites.js";

/** Normal transactions, the first carrying `redeemerData` in a spend redeemer. */
const redeemerDataEvidence = async (redeemerData: string) => {
  const redeemer = (data: string) =>
    encodeMidgardRedeemerWitnessItem({
      purpose: "Spend",
      index: 0n,
      redeemerCbor: Buffer.from(data, "hex"),
      executionUnits: { memory: 1n, steps: 2n },
    });
  const block = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({
        spendInputs: [outRefCbor(0x31, 0n)],
        fee: 1n,
        redeemerWitnesses: [redeemer(redeemerData)],
      }),
      buildFixtureTransaction({
        spendInputs: [outRefCbor(0x32, 0n)],
        fee: 2n,
        redeemerWitnesses: [redeemer("d87980")],
      }),
    ],
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
      detections.map(({ violationId, position }) => [violationId, position]),
    ).toEqual([["redeemer-malformed", 0n]]);
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, fieldShape(0), detections),
    ).not.toThrow();
  });

  it("refuses a redeemer-malformed finding for another transaction, header or frontier", async () => {
    const { evidence, detections, fieldShape } =
      await redeemerDataEvidence("d8798101");
    const [finding] = detections;
    // Another transaction of the same block.
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, fieldShape(1), detections),
    ).toThrow(CanonicalReplayPrerequisiteError);
    for (const unrelated of [
      { ...finding!, headerHash: "99".repeat(32) },
      { ...finding!, position: 1n },
      // The same family's forced-frontier finding at the same ordinal.
      {
        ...finding!,
        detectionId: finding!.detectionId.replace(":accepted:", ":forced:"),
      },
      // A finding naming another transaction at this ordinal.
      {
        ...finding!,
        detectionId: finding!.detectionId.replace(
          evidence.transactions[0]!.nodeTxId,
          evidence.transactions[1]!.nodeTxId,
        ),
      },
      // An unrelated direct family at the exact transaction.
      {
        ...finding!,
        detectionId: "invalid-signature:0",
        violationId: "invalid-signature",
      },
    ])
      expect(() =>
        assertReplayPrerequisiteCovered(evidence, fieldShape(0), [unrelated]),
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
