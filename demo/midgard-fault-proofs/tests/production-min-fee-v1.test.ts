import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence-v1.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification-v1.js";
import { MIN_FEE_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay-v1.js";
import {
  admitMinFeeArtifact,
  prepareMinFeeArtifact,
} from "../src/workflow/production-min-fee-v1.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const CATEGORY_ID = "00000013";

const fixtureEvidence = async () => {
  const fixture = await buildCanonicalBlockFixture({
    minFeeA: 0n,
    minFeeB: 1_000n,
    transactions: [
      buildFixtureTransaction({
        spendInputs: [outRefCbor(0x61, 0n)],
        fee: 1_000n,
      }),
      buildFixtureTransaction({
        spendInputs: [outRefCbor(0x62, 0n)],
        fee: 1n,
      }),
    ],
  });
  return await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/production-min-fee-test",
      grade: "security",
    },
  });
};

const preparedArtifact = async () => {
  const evidence = await fixtureEvidence();
  const replay = await MIN_FEE_COMPLETE_CANONICAL_REPLAY.replay(evidence);
  const classification = await classifyCanonicalBlockViolations({
    evidence,
    detections: replay.detections,
  });
  if (
    classification.decision !== "fault_detected" ||
    classification.category !== "minFee"
  ) {
    throw new Error("fixture did not classify as minFee");
  }
  return {
    classification,
    artifact: await prepareMinFeeArtifact({
      evidence,
      classification,
      categoryId: CATEGORY_ID,
    }),
  };
};

describe("production min-fee public-evidence workflow V1", () => {
  it("replays the exact selected transaction, nine field openings, and strict fee boundary", async () => {
    const { artifact, classification } = await preparedArtifact();
    const admitted = admitMinFeeArtifact(artifact, "11".repeat(28));
    expect(artifact).toMatchObject({
      position: Number(classification.selected.position),
      fee: "1",
      minimumFee: "1000",
      shortfall: "999",
      detectionId: `min-fee:${classification.selected.position.toString()}:${artifact.nativeTxId}:1:1000`,
    });
    expect(admitted.fieldPlans).toHaveLength(9);
    expect(admitted.fieldPlans.map((plan) => plan.fieldIndex)).toEqual([
      0, 1, 2, 3, 4, 5, 6, 7, 8,
    ]);
  });

  it("rejects substituted roots, proofs, field bytes, economics, and detection identity", async () => {
    const { artifact } = await preparedArtifact();
    expect(() =>
      admitMinFeeArtifact({
        ...artifact,
        transactionsPhasRoot: "ff".repeat(32),
      }),
    ).toThrow("does not open its PHAS root");
    expect(() =>
      admitMinFeeArtifact({
        ...artifact,
        txMembershipProofCbor: "d87980",
      }),
    ).toThrow();
    const firstField = artifact.fieldItemCbors[0];
    if (firstField === undefined || firstField[0] === undefined) {
      throw new Error("fixture unexpectedly omitted its first field item");
    }
    expect(() =>
      admitMinFeeArtifact({
        ...artifact,
        fieldItemCbors: [["00"], ...artifact.fieldItemCbors.slice(1)],
      }),
    ).toThrow();
    expect(() => admitMinFeeArtifact({ ...artifact, minFeeB: "999" })).toThrow(
      "does not re-derive its exact violation",
    );
    expect(() =>
      admitMinFeeArtifact({
        ...artifact,
        detectionId: `${artifact.detectionId}-substituted`,
      }),
    ).toThrow("does not re-derive its exact violation");
  });
});
