import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence-v1.js";
import { prepareInvalidSignatureFromCanonicalEvidence } from "../src/evidence/prepare-from-evidence-v1.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification-v1.js";
import { INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay-v1.js";
import {
  admitInvalidSignatureArtifact,
  prepareInvalidSignatureArtifact,
} from "../src/workflow/production-invalid-signature-v1.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const fixtureEvidence = async () => {
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({
        spendInputs: [outRefCbor(61, 0n)],
        fee: 2n,
        addressWitnesses: [
          {
            verification_key: "11".repeat(32),
            signature: "00".repeat(64),
          },
        ],
      }),
    ],
  });
  return await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/production-invalid-signature-test",
      grade: "security",
    },
  });
};

describe("production invalid-signature public evidence V1", () => {
  it("replays every address witness and prepares the exact selected proof", async () => {
    const evidence = await fixtureEvidence();
    const replay =
      await INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY.replay(evidence);
    expect(replay.detections).toHaveLength(1);
    expect(replay.detections[0]).toMatchObject({
      violationId: "invalid-signature",
      position: 0n,
    });
    const classification = await classifyCanonicalBlockViolations({
      evidence,
      detections: replay.detections,
    });
    expect(classification).toMatchObject({
      decision: "fault_detected",
      category: "invalidSignature",
    });
    const prepared = await prepareInvalidSignatureFromCanonicalEvidence({
      evidence,
      txId: evidence.transactions[0]!.nodeTxId,
    });
    expect(prepared.tx).toMatchObject({
      nodeTxId: evidence.transactions[0]!.nodeTxId,
      badAddrTxWitIndex: 0,
      badAddrTxWitVerificationKey: "11".repeat(32),
    });
    expect(prepared.expectedTransactionsRoot.matches).toBe(true);
    if (
      classification.decision !== "fault_detected" ||
      classification.category !== "invalidSignature"
    ) {
      throw new Error("fixture did not select invalidSignature");
    }
    const artifact = await prepareInvalidSignatureArtifact({
      evidence,
      classification,
    });
    expect(admitInvalidSignatureArtifact(artifact)).toMatchObject({
      artifact,
    });
    expect(() =>
      admitInvalidSignatureArtifact({
        ...artifact,
        badWitnessIndex: 1,
      }),
    ).toThrow("does not re-derive its selected violation");
    expect(() =>
      admitInvalidSignatureArtifact({
        ...artifact,
        transactionsPhasRoot: "ff".repeat(32),
      }),
    ).toThrow("does not open its PHAS root");
  });

  it("does not report an empty address-witness set as a fault", async () => {
    const fixture = await buildCanonicalBlockFixture({
      transactions: [
        buildFixtureTransaction({
          spendInputs: [outRefCbor(62, 0n)],
          fee: 2n,
        }),
      ],
    });
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "libp2p/production-invalid-signature-honest-test",
        grade: "security",
      },
    });
    expect(
      (await INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY.replay(evidence))
        .detections,
    ).toEqual([]);
  });
});
