import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayloadV1 } from "../src/evidence/canonical-block-evidence-v1.js";
import {
  type CanonicalBlockClassificationV1,
  classifyCanonicalBlockViolationsV1,
} from "../src/workflow/classification-v1.js";
import {
  INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY_V1,
  requireCompleteCanonicalReplayDecisionV1,
} from "../src/workflow/complete-replay-v1.js";
import {
  admitProductionInputSetUniquenessArtifactV1,
  prepareProductionInputSetUniquenessArtifactV1,
} from "../src/workflow/production-input-set-uniqueness-v1.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const evidenceWithInputs = async ({
  spendInputs,
  referenceInputs = [],
}: {
  readonly spendInputs: readonly Buffer[];
  readonly referenceInputs?: readonly Buffer[];
}) => {
  const fixture = await buildCanonicalBlockFixtureV1({
    transactions: [
      buildFixtureTransactionV1({
        spendInputs,
        referenceInputs,
        fee: 1n,
      }),
    ],
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayloadV1({
    observation: authenticatedHeaderObservationV1(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/peer-a",
      grade: "security",
    },
  });
  const replay =
    await INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
  const detections = requireCompleteCanonicalReplayDecisionV1({
    evidence,
    replayer: INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY_V1,
    decision: replay,
  });
  const classification = await classifyCanonicalBlockViolationsV1({
    evidence,
    detections,
  });
  return Object.freeze({ evidence, replay, classification });
};

const fault = (
  classification: CanonicalBlockClassificationV1,
): Extract<
  CanonicalBlockClassificationV1,
  { readonly decision: "fault_detected" }
> => {
  if (classification.decision !== "fault_detected") {
    throw new Error("input-set-uniqueness fixture did not classify a fault");
  }
  return classification;
};

describe("production input-set-uniqueness workflow V1", () => {
  it("selects the canonical first claim and builds an independently re-admitted artifact", async () => {
    const repeated = outRefCbor(41, 0n);
    const fixture = await evidenceWithInputs({
      spendInputs: [repeated, repeated],
      referenceInputs: [repeated, repeated],
    });
    expect(fixture.replay.detections).toHaveLength(1);
    expect(fixture.replay.detections[0]).toMatchObject({
      violationId: "input-set-uniqueness",
      position: 0n,
    });
    expect(fixture.replay.detections[0]?.detectionId).toContain(
      ":duplicateSpendInputs:0:1",
    );
    const artifact = await prepareProductionInputSetUniquenessArtifactV1({
      evidence: fixture.evidence,
      classification: fault(fixture.classification),
    });
    const admitted = admitProductionInputSetUniquenessArtifactV1(artifact);
    expect(admitted.claim).toEqual({
      kind: "duplicateSpendInputs",
      firstIndex: 0n,
      secondIndex: 1n,
    });
    expect(admitted.inclusion.nativeTx.validity_code).toBe(0n);
  });

  it("rejects a substituted claim and unsafe numeric detection position", async () => {
    const repeated = outRefCbor(42, 0n);
    const fixture = await evidenceWithInputs({
      spendInputs: [repeated, repeated],
    });
    const classification = fault(fixture.classification);
    const artifact = await prepareProductionInputSetUniquenessArtifactV1({
      evidence: fixture.evidence,
      classification,
    });
    expect(() =>
      admitProductionInputSetUniquenessArtifactV1({
        ...artifact,
        claim: {
          kind: "duplicateSpendInputs",
          firstIndex: "0",
          secondIndex: "0",
        },
      }),
    ).toThrow("does not re-derive its claim");

    const huge = "9007199254740993";
    await expect(
      prepareProductionInputSetUniquenessArtifactV1({
        evidence: fixture.evidence,
        classification: {
          ...classification,
          selected: {
            ...classification.selected,
            position: BigInt(huge),
            detectionId: classification.selected.detectionId.replace(
              /^input-set-uniqueness:0:/u,
              `input-set-uniqueness:${huge}:`,
            ),
          },
        },
      }),
    ).rejects.toThrow("classification is malformed");
  });

  it("detects spend/reference overlap when neither list has an internal duplicate", async () => {
    const repeated = outRefCbor(43, 0n);
    const fixture = await evidenceWithInputs({
      spendInputs: [repeated],
      referenceInputs: [repeated],
    });
    expect(fixture.replay.detections).toHaveLength(1);
    expect(fixture.replay.detections[0]?.detectionId).toContain(
      ":spendReferenceOverlap:0:0",
    );
  });
});
