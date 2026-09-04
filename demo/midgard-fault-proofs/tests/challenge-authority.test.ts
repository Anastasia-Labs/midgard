import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import {
  admitTransitionTraceChallenge,
  requireTransitionTraceChallenge,
  transitionTraceProof,
  W25_CHALLENGE_COORDINATE,
} from "../src/workflow/challenge-authority.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
} from "./helpers/canonical-block-evidence-fixture.js";

const evidence = async () => {
  const fixture = await buildCanonicalBlockFixture({
    transactions: [buildFixtureTransaction({ spendInputs: [], fee: 2n })],
  });
  return await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
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
    const challenge = await admitTransitionTraceChallenge({
      coordinate: {
        schemaVersion: W25_CHALLENGE_COORDINATE,
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

    expect(requireTransitionTraceChallenge(challenge)).toBe(challenge);
    expect(transitionTraceProof(challenge)).toBeDefined();
    expect(challenge.proofCbor).toMatch(/^[0-9a-f]+$/u);
    expect(challenge.challengeDigest).toMatch(/^[0-9a-f]{64}$/u);
  });

  it("rejects structural copies, changed transcripts, and duplicate L1 refs", async () => {
    const block = await evidence();
    const coordinate = {
      schemaVersion: W25_CHALLENGE_COORDINATE,
      deploymentFingerprint: "11".repeat(32),
      stateQueueObservationDigest: "22".repeat(32),
      headerHash: block.headerHash,
      payloadEnvelopeSha256: block.payloadEnvelopeSha256,
      payloadSha256: block.payloadSha256,
      transcriptDigest: "33".repeat(32),
      blockReplayResultDigest: "44".repeat(32),
      coordinate: { domain: "block" as const, index: "0" },
    } as const;
    const challenge = await admitTransitionTraceChallenge({
      coordinate,
      evidence: block,
      completeEvidence: {},
      detectionIndex: 0,
      exactL1ReferenceOutRefs: [],
    });
    expect(() => requireTransitionTraceChallenge({ ...challenge })).toThrow(
      /not admitted/u,
    );
    await expect(
      admitTransitionTraceChallenge({
        coordinate: { ...coordinate, transcriptDigest: "x".repeat(64) },
        evidence: block,
        completeEvidence: {},
        detectionIndex: 0,
        exactL1ReferenceOutRefs: [],
      }),
    ).rejects.toThrow(/differs/u);
    await expect(
      admitTransitionTraceChallenge({
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
