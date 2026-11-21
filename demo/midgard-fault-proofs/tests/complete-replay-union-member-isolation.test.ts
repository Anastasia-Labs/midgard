/** A union member that fails with an error its caller does not retry has its
 * result withheld: the failure is reported with its family, and the other
 * members' findings still decide the block when they select a provable
 * fault. A failure the caller retries still fails the replay, and a replay
 * whose remaining findings decide nothing provable (none, or only an
 * unprovable gap) fails rather than reading as healthy or as a gap. A held
 * re-execution and an uncovered replay prerequisite keep their own rules
 * beside a withheld member. */
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { ValidationTraceReplayHeldError } from "../src/validation-trace-dispute/replay-hold.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.classify-canonical-block-violations.js";
import { EVENT_PRE_STATE_UNAVAILABLE_VIOLATION_ID } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { createCompleteCanonicalReplayUnion } from "../src/workflow/complete-replay.create-complete-canonical-replay-union.js";
import { completeReplayer } from "../src/workflow/complete-replay.detect-double-withdraws.js";
import { L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.resolved-output-non-canonical-complete-canonical-replay.js";
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

const blockEvidence = async (validity: "TxIsValid" | "TxIsInvalid") => {
  const transaction = buildFixtureTransaction({
    validity,
    spendInputs: [outRefCbor(4, 0n)],
    fee: 1n,
  });
  const fixture = await buildCanonicalBlockFixture({
    transactions: [transaction],
  });
  return {
    txId: transaction.txId,
    evidence: await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "test/complete-replay-union-member-isolation",
        grade: "security",
      },
    }),
  };
};

class RetriedFailure extends Error {}

const failing = (
  family: "doubleSpend" | "mintItemNonCanonical",
  error: Error,
) =>
  completeReplayer([family], async () => {
    throw error;
  });

const isolation = () => {
  const withheld: unknown[] = [];
  return {
    withheld,
    isolation: {
      failsPass: (error: unknown) => error instanceof RetriedFailure,
      withheld: (failure: unknown) => withheld.push(failure),
    },
  };
};

const withheldRecord = (
  family: "doubleSpend" | "mintItemNonCanonical",
  headerHash: string,
  error: Error,
) => ({ launchScope: [family], headerHash, error });

describe("complete replay union member isolation", () => {
  it.each(["doubleSpend", "mintItemNonCanonical"] as const)(
    "decides l2TxMistag while the %s member fails",
    async (family) => {
      const { evidence } = await blockEvidence("TxIsInvalid");
      const error = new Error(`${family} detector failed`);
      const members =
        family === "doubleSpend"
          ? [failing(family, error), L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY]
          : [L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY, failing(family, error)];
      const recorded = isolation();
      const replay = await createCompleteCanonicalReplayUnion(
        members,
        recorded.isolation,
      ).replay(evidence, {});
      await expect(
        classifyCanonicalBlockViolations({
          evidence,
          detections: replay.detections,
        }),
      ).resolves.toMatchObject({
        decision: "fault_detected",
        category: "l2TxMistag",
      });
      expect(recorded.withheld).toEqual([
        withheldRecord(family, evidence.headerHash, error),
      ]);
    },
  );

  it("fails the replay on a failure its caller retries", async () => {
    const { evidence } = await blockEvidence("TxIsInvalid");
    const error = new RetriedFailure("provider did not answer");
    const recorded = isolation();
    await expect(
      createCompleteCanonicalReplayUnion(
        [
          L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY,
          failing("mintItemNonCanonical", error),
        ],
        recorded.isolation,
      ).replay(evidence, {}),
    ).rejects.toBe(error);
    expect(recorded.withheld).toEqual([]);
  });

  it("fails the replay when only withheld results could decide it", async () => {
    const { evidence } = await blockEvidence("TxIsValid");
    const error = new Error("detector failed");
    const recorded = isolation();
    await expect(
      createCompleteCanonicalReplayUnion(
        [
          L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY,
          failing("mintItemNonCanonical", error),
        ],
        recorded.isolation,
      ).replay(evidence, {}),
    ).rejects.toBe(error);
    expect(recorded.withheld).toEqual([
      withheldRecord("mintItemNonCanonical", evidence.headerHash, error),
    ]);
  });

  it("fails the replay when the remaining findings are only an unprovable gap", async () => {
    const { evidence, txId } = await blockEvidence("TxIsValid");
    const gap = completeReplayer(["doubleSpend"], async () => [
      {
        ...acceptedTransactionSubject(txId),
        detectionId: `${EVENT_PRE_STATE_UNAVAILABLE_VIOLATION_ID}:0`,
        headerHash: evidence.headerHash,
        violationId: EVENT_PRE_STATE_UNAVAILABLE_VIOLATION_ID,
        position: 0n,
      },
    ]);
    const error = new Error("detector failed");
    const recorded = isolation();
    await expect(
      createCompleteCanonicalReplayUnion(
        [gap, failing("mintItemNonCanonical", error)],
        recorded.isolation,
      ).replay(evidence, {}),
    ).rejects.toBe(error);
    expect(recorded.withheld).toEqual([
      withheldRecord("mintItemNonCanonical", evidence.headerHash, error),
    ]);
  });

  it("keeps a held re-execution at the decided event failing the replay beside a withheld member", async () => {
    const { evidence } = await blockEvidence("TxIsInvalid");
    const held = new ValidationTraceReplayHeldError(
      evidence.headerHash,
      [],
      eventOrder(
        evidence.reconstruction,
        evidence.reconstruction.sourceEvents[0]!.fingerprint,
      ),
      [],
    );
    const error = new Error("detector failed");
    const recorded = isolation();
    await expect(
      createCompleteCanonicalReplayUnion(
        [
          failing("doubleSpend", error),
          completeReplayer(["validationTraceDispute"], async () => {
            throw held;
          }),
          L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY,
        ],
        recorded.isolation,
      ).replay(evidence, {}),
    ).rejects.toBe(held);
    expect(recorded.withheld).toEqual([
      withheldRecord("doubleSpend", evidence.headerHash, error),
    ]);
  });

  it("decides on a finding that covers a replay prerequisite beside a withheld member", async () => {
    const { evidence } = await blockEvidence("TxIsInvalid");
    const error = new Error("detector failed");
    const recorded = isolation();
    const replay = await createCompleteCanonicalReplayUnion(
      [
        completeReplayer(["transitionTrace"], async () => {
          throw replayPrerequisiteFailure(
            evidence.headerHash,
            evidence.reconstruction.sourceEvents[0]!.eventKey,
            "representable_validity_flag",
          );
        }),
        L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY,
        failing("mintItemNonCanonical", error),
      ],
      recorded.isolation,
    ).replay(evidence, {});
    await expect(
      classifyCanonicalBlockViolations({
        evidence,
        detections: replay.detections,
      }),
    ).resolves.toMatchObject({
      decision: "fault_detected",
      category: "l2TxMistag",
    });
    expect(recorded.withheld).toEqual([
      withheldRecord("mintItemNonCanonical", evidence.headerHash, error),
    ]);
  });

  it("fails the replay on an uncovered replay prerequisite beside a withheld member", async () => {
    const { evidence } = await blockEvidence("TxIsValid");
    const error = new Error("detector failed");
    const recorded = isolation();
    await expect(
      createCompleteCanonicalReplayUnion(
        [
          completeReplayer(["transitionTrace"], async () => {
            throw replayPrerequisiteFailure(
              evidence.headerHash,
              evidence.reconstruction.sourceEvents[0]!.eventKey,
              "present_spend_input",
            );
          }),
          L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY,
          failing("mintItemNonCanonical", error),
        ],
        recorded.isolation,
      ).replay(evidence, {}),
    ).rejects.toBeInstanceOf(CanonicalReplayPrerequisiteError);
    expect(recorded.withheld).toEqual([
      withheldRecord("mintItemNonCanonical", evidence.headerHash, error),
    ]);
  });
});
