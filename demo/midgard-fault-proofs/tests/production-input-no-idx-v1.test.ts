import { encodeMidgardSpendInputItemV1 } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayloadV1 } from "../src/evidence/canonical-block-evidence-v1.js";
import { prepareInputNoIdxFromCanonicalEvidenceV1 } from "../src/prepare-input-no-idx.js";
import { classifyCanonicalBlockViolationsV1 } from "../src/workflow/classification-v1.js";
import { INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY_V1 } from "../src/workflow/complete-replay-v1.js";
import {
  admitProductionInputNoIdxArtifactV1,
  prepareProductionInputNoIdxArtifactV1,
} from "../src/workflow/production-input-no-idx-v1.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const evidenceFor = async ({ violating }: { readonly violating: boolean }) => {
  const producer = buildFixtureTransactionV1({
    spendInputs: [],
    outputs: violating ? [] : [Buffer.from("80", "hex")],
    fee: 1n,
  });
  const consumer = buildFixtureTransactionV1({
    spendInputs: [
      encodeMidgardSpendInputItemV1({
        txId: Buffer.from(producer.txId, "hex"),
        outputIndex: 0,
      }),
    ],
    fee: 2n,
  });
  const fixture = await buildCanonicalBlockFixtureV1({
    transactions: [producer, consumer],
  });
  return await canonicalBlockEvidenceFromVerifiedPayloadV1({
    observation: authenticatedHeaderObservationV1(fixture),
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
      await INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
    expect(replay.detections).toHaveLength(1);
    expect(replay.detections[0]).toMatchObject({
      violationId: "input-no-idx",
      position: 1n,
    });
    const classification = await classifyCanonicalBlockViolationsV1({
      evidence,
      detections: replay.detections,
    });
    expect(classification).toMatchObject({
      decision: "fault_detected",
      category: "nonExistentInputNoIndex",
    });
    const prepared = await prepareInputNoIdxFromCanonicalEvidenceV1({
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
    const artifact = await prepareProductionInputNoIdxArtifactV1({
      evidence,
      classification,
    });
    expect(admitProductionInputNoIdxArtifactV1(artifact)).toMatchObject({
      artifact,
    });
    expect(() =>
      admitProductionInputNoIdxArtifactV1({
        ...artifact,
        badInputOutputIndex: "1",
      }),
    ).toThrow("does not re-derive its violation");
    expect(() =>
      admitProductionInputNoIdxArtifactV1({
        ...artifact,
        producingTx: {
          ...artifact.producingTx,
          transactionsPhasRoot: "ff".repeat(32),
        },
      }),
    ).toThrow("does not open its PHAS root");

    const fields = classification.selected.detectionId.split(":");
    await expect(
      prepareProductionInputNoIdxArtifactV1({
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
      (await INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence))
        .detections,
    ).toEqual([]);
  });
});
