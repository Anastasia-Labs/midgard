import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence-v1.js";
import { prepareReferenceInputNoIdxFromCanonicalEvidence } from "../src/prepare-reference-input-no-idx.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification-v1.js";
import { REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay-v1.js";
import {
  admitReferenceInputNoIdxArtifact,
  prepareReferenceInputNoIdxArtifact,
} from "../src/workflow/production-reference-input-no-idx-v1.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const evidenceFor = async ({ violating }: { readonly violating: boolean }) => {
  const producer = buildFixtureTransaction({
    spendInputs: [],
    outputs: violating ? [] : [Buffer.from("80", "hex")],
    fee: 1n,
  });
  const consumer = buildFixtureTransaction({
    spendInputs: [],
    referenceInputs: [
      encodeMidgardSpendInputItem({
        txId: Buffer.from(producer.txId, "hex"),
        outputIndex: 0,
      }),
    ],
    fee: 2n,
  });
  const fixture = await buildCanonicalBlockFixture({
    transactions: [producer, consumer],
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/production-reference-input-no-idx-test",
      grade: "security",
    },
  });
  return { evidence, producerId: producer.txId, consumerId: consumer.txId };
};

describe("production reference-input-no-idx V1", () => {
  it("replays, prepares, and strictly re-admits the same-block violation", async () => {
    const { evidence, producerId, consumerId } = await evidenceFor({
      violating: true,
    });
    const replay =
      await REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY.replay(evidence);
    expect(replay.detections).toHaveLength(1);
    expect(replay.detections[0]).toMatchObject({
      violationId: "reference-input-no-idx",
    });
    const classification = await classifyCanonicalBlockViolations({
      evidence,
      detections: replay.detections,
    });
    expect(classification).toMatchObject({
      decision: "fault_detected",
      category: "referenceInputNoIdx",
    });
    const prepared = await prepareReferenceInputNoIdxFromCanonicalEvidence({
      evidence,
      badTxId: consumerId,
      badReferenceInputIndex: 0,
    });
    expect(prepared).toMatchObject({
      badReferenceInputIndex: 0,
      producingTxId: producerId,
      producingTxOutputCount: 0,
    });
    if (
      classification.decision !== "fault_detected" ||
      classification.category !== "referenceInputNoIdx"
    ) {
      throw new Error("fixture did not select referenceInputNoIdx");
    }
    const artifact = await prepareReferenceInputNoIdxArtifact({
      evidence,
      classification,
    });
    expect(admitReferenceInputNoIdxArtifact(artifact)).toMatchObject({
      artifact,
    });
    expect(() =>
      admitReferenceInputNoIdxArtifact({
        ...artifact,
        badReferenceInputOutputIndex: "1",
      }),
    ).toThrow("does not re-derive its violation");

    const fields = classification.selected.detectionId.split(":");
    const unsafeIndexClassification = {
      ...classification,
      selected: {
        ...classification.selected,
        detectionId: [
          fields[0],
          fields[1],
          "9007199254740993",
          ...fields.slice(3),
        ].join(":"),
      },
    };
    await expect(
      prepareReferenceInputNoIdxArtifact({
        evidence,
        classification: unsafeIndexClassification,
      }),
    ).rejects.toThrow("classification identity is malformed");
  });

  it("does not classify an in-range producer output", async () => {
    const { evidence } = await evidenceFor({ violating: false });
    expect(
      (await REFERENCE_INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY.replay(evidence))
        .detections,
    ).toEqual([]);
  });
});
