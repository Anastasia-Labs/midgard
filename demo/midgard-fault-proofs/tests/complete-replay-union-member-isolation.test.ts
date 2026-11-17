/** A union member that fails with an error its caller does not retry has its
 * result withheld: the failure is reported with its family, and the other
 * members' findings still decide the block. A failure the caller retries
 * still fails the replay, and a replay with nothing but withheld results
 * fails rather than reading as healthy. */
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import { completeReplayer } from "../src/workflow/complete-replay.detect-double-withdraws.js";
import {
  createCompleteCanonicalReplayUnion,
  L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

const blockEvidence = async (validity: "TxIsValid" | "TxIsInvalid") => {
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({
        validity,
        spendInputs: [outRefCbor(4, 0n)],
        fee: 1n,
      }),
    ],
  });
  return await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "test/complete-replay-union-member-isolation",
      grade: "security",
    },
  });
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
  const replayed: unknown[] = [];
  return {
    withheld,
    replayed,
    isolation: {
      failsPass: (error: unknown) => error instanceof RetriedFailure,
      withheld: (failure: unknown) => withheld.push(failure),
      replayed: (launchScope: unknown) => replayed.push(launchScope),
    },
  };
};

describe("complete replay union member isolation", () => {
  it.each(["doubleSpend", "mintItemNonCanonical"] as const)(
    "decides l2TxMistag while the %s member fails",
    async (family) => {
      const evidence = await blockEvidence("TxIsInvalid");
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
        { launchScope: [family], headerHash: evidence.headerHash, error },
      ]);
      expect(recorded.replayed).toEqual([["l2TxMistag"]]);
    },
  );

  it("fails the replay on a failure its caller retries", async () => {
    const evidence = await blockEvidence("TxIsInvalid");
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
    const evidence = await blockEvidence("TxIsValid");
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
      {
        launchScope: ["mintItemNonCanonical"],
        headerHash: evidence.headerHash,
        error,
      },
    ]);
  });
});
