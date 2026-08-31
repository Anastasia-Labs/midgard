import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayloadV1 } from "../src/evidence/canonical-block-evidence-v1.js";
import { classifyCanonicalBlockViolationsV1 } from "../src/workflow/classification-v1.js";
import {
  NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
  NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
} from "../src/workflow/complete-replay-v1.js";
import {
  admitProductionLedgerAbsenceArtifactV1,
  prepareProductionLedgerAbsenceArtifactV1,
  type ProductionLedgerAbsenceCategoryV1,
} from "../src/workflow/production-ledger-absence-artifact-v1.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const OWNER = "91".repeat(28);

const context = async () => {
  const fixture = await buildCanonicalBlockFixtureV1({
    transactions: [
      buildFixtureTransactionV1({
        spendInputs: [outRefCbor(12, 0n)],
        referenceInputs: [outRefCbor(13, 1n)],
        fee: 1n,
      }),
    ],
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayloadV1({
    observation: authenticatedHeaderObservationV1(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/ledger-absence-artifact-test",
      grade: "security",
    },
  });
  return { evidence };
};

const prepare = async (category: ProductionLedgerAbsenceCategoryV1) => {
  const { evidence } = await context();
  const replayer =
    category === "nonExistentInput"
      ? NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY_V1
      : NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1;
  const decision = await replayer.replay(evidence);
  const classification = await classifyCanonicalBlockViolationsV1({
    evidence,
    detections: decision.detections,
    minimumConfirmationDepth: 1,
  });
  if (classification.decision !== "fault_detected") {
    throw new Error("fixture did not classify a ledger-absence fault");
  }
  return await prepareProductionLedgerAbsenceArtifactV1({
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
      const admitted = admitProductionLedgerAbsenceArtifactV1(artifact, OWNER);

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
      admitProductionLedgerAbsenceArtifactV1(
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
      admitProductionLedgerAbsenceArtifactV1(
        {
          ...artifact,
          detectionId: artifact.detectionId.replace(":0:", ":1:"),
        },
        OWNER,
      ),
    ).toThrow(/detection identity/u);
  });
});
