import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayloadV1 } from "../src/evidence/canonical-block-evidence-v1.js";
import { classifyCanonicalBlockViolationsV1 } from "../src/workflow/classification-v1.js";
import { MIN_FEE_COMPLETE_CANONICAL_REPLAY_V1 } from "../src/workflow/complete-replay-v1.js";
import {
  admitProductionMinFeeArtifactV1,
  prepareProductionMinFeeArtifactV1,
} from "../src/workflow/production-min-fee-v1.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const CATEGORY_ID = "00000013";

const fixtureEvidence = async () => {
  const fixture = await buildCanonicalBlockFixtureV1({
    minFeeA: 0n,
    minFeeB: 1_000n,
    transactions: [
      buildFixtureTransactionV1({
        spendInputs: [outRefCbor(0x61, 0n)],
        fee: 1_000n,
      }),
      buildFixtureTransactionV1({
        spendInputs: [outRefCbor(0x62, 0n)],
        fee: 1n,
      }),
    ],
  });
  return await canonicalBlockEvidenceFromVerifiedPayloadV1({
    observation: authenticatedHeaderObservationV1(fixture),
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
  const replay = await MIN_FEE_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
  const classification = await classifyCanonicalBlockViolationsV1({
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
    artifact: await prepareProductionMinFeeArtifactV1({
      evidence,
      classification,
      categoryId: CATEGORY_ID,
    }),
  };
};

describe("production min-fee public-evidence workflow V1", () => {
  it("replays the exact selected transaction, nine field openings, and strict fee boundary", async () => {
    const { artifact, classification } = await preparedArtifact();
    const admitted = admitProductionMinFeeArtifactV1(artifact, "11".repeat(28));
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
      admitProductionMinFeeArtifactV1({
        ...artifact,
        transactionsPhasRoot: "ff".repeat(32),
      }),
    ).toThrow("does not open its PHAS root");
    expect(() =>
      admitProductionMinFeeArtifactV1({
        ...artifact,
        txMembershipProofCbor: "d87980",
      }),
    ).toThrow();
    const firstField = artifact.fieldItemCbors[0];
    if (firstField === undefined || firstField[0] === undefined) {
      throw new Error("fixture unexpectedly omitted its first field item");
    }
    expect(() =>
      admitProductionMinFeeArtifactV1({
        ...artifact,
        fieldItemCbors: [["00"], ...artifact.fieldItemCbors.slice(1)],
      }),
    ).toThrow();
    expect(() =>
      admitProductionMinFeeArtifactV1({ ...artifact, minFeeB: "999" }),
    ).toThrow("does not re-derive its exact violation");
    expect(() =>
      admitProductionMinFeeArtifactV1({
        ...artifact,
        detectionId: `${artifact.detectionId}-substituted`,
      }),
    ).toThrow("does not re-derive its exact violation");
  });
});
