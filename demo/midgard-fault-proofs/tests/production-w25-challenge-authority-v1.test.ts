import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayloadV1 } from "../src/evidence/canonical-block-evidence-v1.js";
import {
  admitProductionTransitionTraceChallengeV1,
  PRODUCTION_W25_CHALLENGE_COORDINATE_V1,
  productionTransitionTraceProofV1,
  requireProductionTransitionTraceChallengeV1,
} from "../src/workflow/production-w25-challenge-authority-v1.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const evidence = async () => {
  const fixture = await buildCanonicalBlockFixtureV1({
    transactions: [buildFixtureTransactionV1({ spendInputs: [], fee: 2n })],
  });
  return await canonicalBlockEvidenceFromVerifiedPayloadV1({
    observation: authenticatedHeaderObservationV1(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "w25-challenge-authority-test",
      grade: "security",
    },
  });
};

describe("production W25 challenge authority", () => {
  it("replays and admits a transition proof bound to the fresh transcript", async () => {
    const block = await evidence();
    const challenge = await admitProductionTransitionTraceChallengeV1({
      coordinate: {
        schemaVersion: PRODUCTION_W25_CHALLENGE_COORDINATE_V1,
        deploymentFingerprint: "11".repeat(32),
        stateQueueObservationDigest: "22".repeat(32),
        headerHash: block.headerHash,
        payloadEnvelopeSha256: block.payloadEnvelopeSha256,
        payloadSha256: block.payloadSha256,
        transcriptDigest: "33".repeat(32),
        blockReplayResultDigest: "44".repeat(32),
        coordinate: { domain: "block", index: "0" },
      },
      evidence: block,
      completeEvidence: {},
      detectionIndex: 0,
      exactL1ReferenceOutRefs: [],
    });

    expect(requireProductionTransitionTraceChallengeV1(challenge)).toBe(
      challenge,
    );
    expect(productionTransitionTraceProofV1(challenge)).toBeDefined();
    expect(challenge.proofCbor).toMatch(/^[0-9a-f]+$/u);
    expect(challenge.challengeDigest).toMatch(/^[0-9a-f]{64}$/u);
  });

  it("rejects structural copies, changed transcripts, and duplicate L1 refs", async () => {
    const block = await evidence();
    const coordinate = {
      schemaVersion: PRODUCTION_W25_CHALLENGE_COORDINATE_V1,
      deploymentFingerprint: "11".repeat(32),
      stateQueueObservationDigest: "22".repeat(32),
      headerHash: block.headerHash,
      payloadEnvelopeSha256: block.payloadEnvelopeSha256,
      payloadSha256: block.payloadSha256,
      transcriptDigest: "33".repeat(32),
      blockReplayResultDigest: "44".repeat(32),
      coordinate: { domain: "block" as const, index: "0" },
    } as const;
    const challenge = await admitProductionTransitionTraceChallengeV1({
      coordinate,
      evidence: block,
      completeEvidence: {},
      detectionIndex: 0,
      exactL1ReferenceOutRefs: [],
    });
    expect(() =>
      requireProductionTransitionTraceChallengeV1({ ...challenge }),
    ).toThrow(/not admitted/u);
    await expect(
      admitProductionTransitionTraceChallengeV1({
        coordinate: { ...coordinate, transcriptDigest: "x".repeat(64) },
        evidence: block,
        completeEvidence: {},
        detectionIndex: 0,
        exactL1ReferenceOutRefs: [],
      }),
    ).rejects.toThrow(/differs/u);
    await expect(
      admitProductionTransitionTraceChallengeV1({
        coordinate,
        evidence: block,
        completeEvidence: {},
        detectionIndex: 0,
        exactL1ReferenceOutRefs: [
          `${"55".repeat(32)}#0`,
          `${"55".repeat(32)}#0`,
        ],
      }),
    ).rejects.toThrow(/unique canonical out-refs/u);
  });
});
