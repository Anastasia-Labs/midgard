import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyCanonicalBlockViolations,
  FRAUD_PROOF_CLASSIFICATION_RULES,
} from "../src/workflow/classification.js";
import { acceptedTransactionSubject } from "../src/workflow/detection-subject.js";
import { h32 } from "./helpers/canonical-block-evidence-fixture.js";
import {
  canonicalEvidence,
  canonicalEvidenceWithEvents,
  detection,
} from "./workflow.make-adapter.js";

describe("Q55/W-O6 deterministic violation classification", () => {
  it("covers every registered family exactly once in catalogue order", () => {
    expect(
      FRAUD_PROOF_CLASSIFICATION_RULES.map((rule) => rule.category),
    ).toEqual(SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER);
    const identifiers = FRAUD_PROOF_CLASSIFICATION_RULES.flatMap((rule) => [
      ...rule.violationIds,
    ]);
    expect(new Set(identifiers).size).toBe(identifiers.length);
  });

  it("selects the earliest event, then stable family order", async () => {
    const [evidence, txIds] = await canonicalEvidenceWithEvents(3);
    const classification = await classifyCanonicalBlockViolations({
      evidence,
      detections: [
        // A lower reported position never outranks an earlier event.
        detection(evidence, "mint-authorization", {
          ...acceptedTransactionSubject(txIds[2]!),
          detectionId: "mint-late",
          position: 0n,
        }),
        detection(evidence, "invalid-range", {
          ...acceptedTransactionSubject(txIds[1]!),
          detectionId: "range-first",
          position: 9n,
        }),
        detection(evidence, "double-spend", {
          ...acceptedTransactionSubject(txIds[0]!, txIds[1]!),
          detectionId: "double-first",
          position: 9n,
        }),
      ],
    });
    expect(classification).toMatchObject({
      decision: "fault_detected",
      category: "doubleSpend",
      selected: { detectionId: "double-first" },
    });
  });

  it("maps unknown earliest violations to unprovable_gap, never verified", async () => {
    const [evidence, txIds] = await canonicalEvidenceWithEvents(2);
    const classification = await classifyCanonicalBlockViolations({
      evidence,
      detections: [
        detection(evidence, "unknown-launch-fault", {
          ...acceptedTransactionSubject(txIds[0]!),
          detectionId: "gap",
          position: 0n,
        }),
        detection(evidence, "double-spend", {
          ...acceptedTransactionSubject(txIds[1]!),
          detectionId: "later-proof",
          position: 1n,
        }),
      ],
    });
    expect(classification).toMatchObject({
      decision: "unprovable_gap",
      selected: {
        detectionId: "gap",
        reason: "unregistered_violation",
      },
    });
  });

  it("does not promote an empty partial detector result to verified", async () => {
    const evidence = await canonicalEvidence();
    await expect(
      classifyCanonicalBlockViolations({ evidence, detections: [] }),
    ).resolves.toMatchObject({ decision: "no_fault_detected" });
  });

  it("rejects duplicate detector identities and cross-header detections", async () => {
    const evidence = await canonicalEvidence();
    const duplicate = detection(evidence);
    await expect(
      classifyCanonicalBlockViolations({
        evidence,
        detections: [duplicate, duplicate],
      }),
    ).rejects.toThrow("duplicate canonical violation detectionId");
    await expect(
      classifyCanonicalBlockViolations({
        evidence,
        detections: [
          detection(evidence, "double-spend", { headerHash: h32(9) }),
        ],
      }),
    ).rejects.toThrow("targets header");
  });
});
