import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence-v1.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification-v1.js";
import {
  NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY,
  NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay-v1.js";
import {
  admitLedgerAbsenceArtifact,
  type LedgerAbsenceCategory,
  prepareLedgerAbsenceArtifact,
} from "../src/workflow/production-ledger-absence-artifact-v1.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const OWNER = "91".repeat(28);

const context = async () => {
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({
        spendInputs: [outRefCbor(12, 0n)],
        referenceInputs: [outRefCbor(13, 1n)],
        fee: 1n,
      }),
    ],
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/ledger-absence-artifact-test",
      grade: "security",
    },
  });
  return { evidence };
};

const prepare = async (category: LedgerAbsenceCategory) => {
  const { evidence } = await context();
  const replayer =
    category === "nonExistentInput"
      ? NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY
      : NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY;
  const decision = await replayer.replay(evidence);
  const classification = await classifyCanonicalBlockViolations({
    evidence,
    detections: decision.detections,
    minimumConfirmationDepth: 1,
  });
  if (classification.decision !== "fault_detected") {
    throw new Error("fixture did not classify a ledger-absence fault");
  }
  return await prepareLedgerAbsenceArtifact({
    category,
    evidence,
    classification,
    owner: OWNER,
  });
};

describe("production ledger-absence artifacts V1", () => {
  it.each(["nonExistentInput", "noReferenceInput"] as const)(
    "replays %s inclusion, field opening, and both nonmembership roots",
    async (category) => {
      const artifact = await prepare(category);
      const admitted = admitLedgerAbsenceArtifact(artifact, OWNER);

      expect(admitted.artifact.category).toBe(category);
      expect(admitted.selectedInput).toEqual(
        category === "nonExistentInput"
          ? { tx_id: "0c".repeat(32), output_index: 0n }
          : { tx_id: "0d".repeat(32), output_index: 1n },
      );
      expect(admitted.fieldPlan.commitment).toMatch(/^[0-9a-f]{64}$/u);
    },
  );

  it("rejects a substituted predecessor nonmembership proof", async () => {
    const artifact = await prepare("nonExistentInput");

    expect(() =>
      admitLedgerAbsenceArtifact(
        {
          ...artifact,
          ledgerNonMembershipProofCbor: artifact.txsNonMembershipProofCbor,
        },
        OWNER,
      ),
    ).toThrow(/proof/u);
  });

  it("rejects a detection identity whose selected input differs", async () => {
    const artifact = await prepare("nonExistentInput");
    expect(() =>
      admitLedgerAbsenceArtifact(
        {
          ...artifact,
          detectionId: artifact.detectionId.replace(":0:", ":1:"),
        },
        OWNER,
      ),
    ).toThrow(/detection identity/u);
  });
});
