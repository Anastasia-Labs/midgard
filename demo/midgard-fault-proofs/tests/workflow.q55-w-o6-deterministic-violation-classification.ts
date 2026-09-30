import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyCanonicalBlockViolations,
  FRAUD_PROOF_CLASSIFICATION_FAMILY_PRECEDENCE,
  FRAUD_PROOF_CLASSIFICATION_RULES,
} from "../src/workflow/classification.js";
import { h32 } from "./helpers/canonical-block-evidence-fixture.js";
import { canonicalEvidence, detection } from "./workflow.make-adapter.js";

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

  it("ranks crossBlockDuplicateEvent ahead of doubleWithdraw per decision 0008", () => {
    expect([...FRAUD_PROOF_CLASSIFICATION_FAMILY_PRECEDENCE].sort()).toEqual(
      [...SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER].sort(),
    );
    const promoted = FRAUD_PROOF_CLASSIFICATION_FAMILY_PRECEDENCE.indexOf(
      "crossBlockDuplicateEvent",
    );
    expect(promoted).toBe(
      FRAUD_PROOF_CLASSIFICATION_FAMILY_PRECEDENCE.indexOf("doubleWithdraw") -
        1,
    );
    expect(
      FRAUD_PROOF_CLASSIFICATION_FAMILY_PRECEDENCE.slice(0, promoted),
    ).toEqual(SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.slice(0, promoted));
  });

  it("selects the earliest position, then stable family order", async () => {
    const evidence = await canonicalEvidence();
    const classification = await classifyCanonicalBlockViolations({
      evidence,
      detections: [
        detection(evidence, "mint-authorization", {
          detectionId: "mint-late",
          position: 9n,
        }),
        detection(evidence, "invalid-range", {
          detectionId: "range-first",
          position: 2n,
        }),
        detection(evidence, "double-spend", {
          detectionId: "double-first",
          position: 2n,
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
    const evidence = await canonicalEvidence();
    const classification = await classifyCanonicalBlockViolations({
      evidence,
      detections: [
        detection(evidence, "unknown-launch-fault", {
          detectionId: "gap",
          position: 0n,
        }),
        detection(evidence, "double-spend", {
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
