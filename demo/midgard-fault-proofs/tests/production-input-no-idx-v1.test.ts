import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence-v1.js";
import { prepareInputNoIdxFromCanonicalEvidence } from "../src/prepare-input-no-idx.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification-v1.js";
import { INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay-v1.js";
import {
  admitInputNoIdxArtifact,
  prepareInputNoIdxArtifact,
} from "../src/workflow/production-input-no-idx-v1.js";
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
    spendInputs: [
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
  return await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/production-input-no-idx-test",
      grade: "security",
    },
  });
};

describe("production input-no-idx complete replay V1", () => {
  it("selects and prepares an out-of-range same-block producer reference", async () => {
    const evidence = await evidenceFor({ violating: true });
    const replay =
      await INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY.replay(evidence);
    expect(replay.detections).toHaveLength(1);
    expect(replay.detections[0]).toMatchObject({
      violationId: "input-no-idx",
      position: 1n,
    });
    const classification = await classifyCanonicalBlockViolations({
      evidence,
      detections: replay.detections,
    });
    expect(classification).toMatchObject({
      decision: "fault_detected",
      category: "nonExistentInputNoIndex",
    });
    const prepared = await prepareInputNoIdxFromCanonicalEvidence({
      evidence,
      badTxId: evidence.transactions[1]!.nodeTxId,
      badInputsIndex: 0,
    });
    expect(prepared.step02.badInputsIndex).toBe(0);
    expect(prepared.step04).toMatchObject({
      producingTxId: evidence.transactions[0]!.nodeTxId,
      badInputOutputIndex: "0",
      outputsPreimageCbor: [],
    });
    if (
      classification.decision !== "fault_detected" ||
      classification.category !== "nonExistentInputNoIndex"
    ) {
      throw new Error("fixture did not select nonExistentInputNoIndex");
    }
    const artifact = await prepareInputNoIdxArtifact({
      evidence,
      classification,
    });
    expect(admitInputNoIdxArtifact(artifact)).toMatchObject({
      artifact,
    });
    expect(() =>
      admitInputNoIdxArtifact({
        ...artifact,
        badInputOutputIndex: "1",
      }),
    ).toThrow("does not re-derive its violation");
    expect(() =>
      admitInputNoIdxArtifact({
        ...artifact,
        producingTx: {
          ...artifact.producingTx,
          transactionsPhasRoot: "ff".repeat(32),
        },
      }),
    ).toThrow("does not open its PHAS root");

    const fields = classification.selected.detectionId.split(":");
    await expect(
      prepareInputNoIdxArtifact({
        evidence,
        classification: {
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
        },
      }),
    ).rejects.toThrow("classification identity is malformed");
  });

  it("does not classify an in-range producer output", async () => {
    const evidence = await evidenceFor({ violating: false });
    expect(
      (await INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY.replay(evidence))
        .detections,
    ).toEqual([]);
  });
});
