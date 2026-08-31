import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayloadV1 } from "../src/evidence/canonical-block-evidence-v1.js";
import { classifyCanonicalBlockViolationsV1 } from "../src/workflow/classification-v1.js";
import {
  INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1,
  ZERO_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
} from "../src/workflow/complete-replay-v1.js";
import {
  admitProductionNativeInclusionTwoStepArtifactV1,
  prepareProductionNativeInclusionTwoStepArtifactV1,
} from "../src/workflow/production-native-inclusion-two-step-v1.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const evidenceFor = async () => {
  const fixture = await buildCanonicalBlockFixtureV1({
    transactions: [
      buildFixtureTransactionV1({
        spendInputs: [outRefCbor(12, 0n)],
        fee: 1n,
      }),
      buildFixtureTransactionV1({
        spendInputs: [outRefCbor(13, 0n)],
        fee: 2n,
        validityIntervalStart: 1n,
      }),
      buildFixtureTransactionV1({ spendInputs: [], fee: 3n }),
    ],
  });
  return await canonicalBlockEvidenceFromVerifiedPayloadV1({
    observation: authenticatedHeaderObservationV1(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/production-native-inclusion-test",
      grade: "security",
    },
  });
};

describe("production invalid-range/zero-input public-evidence artifacts V1", () => {
  it("prepares the exact classified invalid-range transaction and replays its MPF proof", async () => {
    const evidence = await evidenceFor();
    const replay =
      await INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
    const classification = await classifyCanonicalBlockViolationsV1({
      evidence,
      detections: replay.detections,
    });
    if (
      classification.decision !== "fault_detected" ||
      classification.category !== "invalidRange"
    ) {
      throw new Error("fixture did not classify as invalidRange");
    }
    const artifact = await prepareProductionNativeInclusionTwoStepArtifactV1({
      category: "invalidRange",
      evidence,
      classification,
    });
    expect(artifact).toMatchObject({
      category: "invalidRange",
      position: Number(classification.selected.position),
      blockSlot: "0",
      violationReason: "starts-after-block-slot",
    });
    expect(admitProductionNativeInclusionTwoStepArtifactV1(artifact)).toEqual(
      expect.objectContaining({ artifact }),
    );
  });

  it("prepares the exact classified zero-input transaction", async () => {
    const evidence = await evidenceFor();
    const replay =
      await ZERO_INPUT_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
    const classification = await classifyCanonicalBlockViolationsV1({
      evidence,
      detections: replay.detections,
    });
    if (
      classification.decision !== "fault_detected" ||
      classification.category !== "zeroInput"
    ) {
      throw new Error("fixture did not classify as zeroInput");
    }
    const artifact = await prepareProductionNativeInclusionTwoStepArtifactV1({
      category: "zeroInput",
      evidence,
      classification,
    });
    expect(artifact).toMatchObject({
      category: "zeroInput",
      position: Number(classification.selected.position),
      blockSlot: null,
      violationReason: null,
    });
    expect(admitProductionNativeInclusionTwoStepArtifactV1(artifact)).toEqual(
      expect.objectContaining({ artifact }),
    );
  });

  it("rejects substituted roots, proofs, detection identities, and family fields", async () => {
    const evidence = await evidenceFor();
    const replay =
      await INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
    const classification = await classifyCanonicalBlockViolationsV1({
      evidence,
      detections: replay.detections,
    });
    if (
      classification.decision !== "fault_detected" ||
      classification.category !== "invalidRange"
    ) {
      throw new Error("fixture did not classify as invalidRange");
    }
    const artifact = await prepareProductionNativeInclusionTwoStepArtifactV1({
      category: "invalidRange",
      evidence,
      classification,
    });
    expect(() =>
      admitProductionNativeInclusionTwoStepArtifactV1({
        ...artifact,
        transactionsPhasRoot: "ff".repeat(32),
      }),
    ).toThrow("does not open its PHAS root");
    expect(() =>
      admitProductionNativeInclusionTwoStepArtifactV1({
        ...artifact,
        txMembershipProofCbor: "d87980",
      }),
    ).toThrow();
    expect(() =>
      admitProductionNativeInclusionTwoStepArtifactV1({
        ...artifact,
        detectionId: `${artifact.detectionId}-substituted`,
      }),
    ).toThrow("does not re-derive its selected violation");
    expect(() =>
      admitProductionNativeInclusionTwoStepArtifactV1({
        ...artifact,
        category: "zeroInput",
        blockSlot: null,
        violationReason: null,
      }),
    ).toThrow("zero-input artifact does not re-derive");
  });
});
