import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence-v1.js";
import {
  type CanonicalBlockClassification,
  classifyCanonicalBlockViolations,
} from "../src/workflow/classification-v1.js";
import {
  INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY,
  requireCompleteCanonicalReplayDecision,
} from "../src/workflow/complete-replay-v1.js";
import {
  admitInputSetUniquenessArtifact,
  prepareInputSetUniquenessArtifact,
} from "../src/workflow/production-input-set-uniqueness-v1.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const evidenceWithInputs = async ({
  spendInputs,
  referenceInputs = [],
}: {
  readonly spendInputs: readonly Buffer[];
  readonly referenceInputs?: readonly Buffer[];
}) => {
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({
        spendInputs,
        referenceInputs,
        fee: 1n,
      }),
    ],
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/peer-a",
      grade: "security",
    },
  });
  const replay =
    await INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY.replay(evidence);
  const detections = requireCompleteCanonicalReplayDecision({
    evidence,
    replayer: INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY,
    decision: replay,
  });
  const classification = await classifyCanonicalBlockViolations({
    evidence,
    detections,
  });
  return Object.freeze({ evidence, replay, classification });
};

const fault = (
  classification: CanonicalBlockClassification,
): Extract<
  CanonicalBlockClassification,
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
    const artifact = await prepareInputSetUniquenessArtifact({
      evidence: fixture.evidence,
      classification: fault(fixture.classification),
    });
    const admitted = admitInputSetUniquenessArtifact(artifact);
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
    const artifact = await prepareInputSetUniquenessArtifact({
      evidence: fixture.evidence,
      classification,
    });
    expect(() =>
      admitInputSetUniquenessArtifact({
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
      prepareInputSetUniquenessArtifact({
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
