import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayloadV1 } from "../src/evidence/canonical-block-evidence-v1.js";
import {
  COMPLETE_CANONICAL_REPLAY_PREDECESSOR_V1,
  NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
  NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
} from "../src/workflow/complete-replay-v1.js";
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
        referenceInputs: [outRefCbor(13, 1n)],
        fee: 1n,
      }),
    ],
  });
  return await canonicalBlockEvidenceFromVerifiedPayloadV1({
    observation: authenticatedHeaderObservationV1(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/predecessor-replay-test",
      grade: "security",
    },
  });
};

describe("production predecessor-relative complete replay V1", () => {
  it("detects absent spend and reference inputs against the committed empty genesis ledger", async () => {
    const evidence = await evidenceFor();
    await expect(
      NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence),
    ).resolves.toMatchObject({
      context: null,
      detections: [{ violationId: "non-existent-input", position: 0n }],
    });
    await expect(
      NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence),
    ).resolves.toMatchObject({
      context: null,
      detections: [{ violationId: "no-reference-input", position: 0n }],
    });
  });

  it("emits an explicit unprovable gap when a non-empty predecessor is unavailable", async () => {
    const base = await evidenceFor();
    const evidence = {
      ...base,
      header: { ...base.header, prevUtxosRoot: "ee".repeat(32) },
    };
    const decision =
      await NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
    expect(decision.detections).toEqual([
      expect.objectContaining({
        violationId: "authenticated-predecessor-context-unavailable",
        position: 0n,
      }),
    ]);
  });

  it("rejects a structural predecessor clone that did not cross raw L1/DA admission", async () => {
    const base = await evidenceFor();
    const evidence = {
      ...base,
      header: { ...base.header, prevUtxosRoot: "ee".repeat(32) },
    };
    await expect(
      NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence, {
        predecessor: {
          schemaVersion: COMPLETE_CANONICAL_REPLAY_PREDECESSOR_V1,
          challengedHeaderHash: evidence.headerHash,
          headerHash: evidence.header.prevHeaderHash,
          payloadEnvelopeSha256: "11".repeat(32),
          payloadSha256: "22".repeat(32),
        },
      }),
    ).rejects.toThrow("was not admitted for this challenged header");
  });
});
