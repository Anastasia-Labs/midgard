import type { EvidenceProvenance } from "@al-ft/midgard-sdk/fraud-proof";
import { describe, expect, it } from "vitest";

import type { FraudProofWorkflowTerminal } from "../src/workflow/identity.js";
import { missingSignatureObservation } from "../src/workflow/missing-signature-state.js";

const hash = (byte: string): string => byte.repeat(32);
const headerHash = "ab".repeat(28);
const outRef = (byte: string, index = 0): string => `${hash(byte)}#${index}`;
const provenance: EvidenceProvenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "local-kupmios/kupo+ogmios",
  grade: "security",
};

const terminal = (
  removalTxHash = hash("77"),
  proofOutRef = outRef("66"),
): FraudProofWorkflowTerminal => ({
  schemaVersion: "midgard-fraud-proof-workflow-terminal-v1",
  category: "missingSignature",
  headerHash,
  proofToken: {
    unit: "11".repeat(28) + "22".repeat(28),
    outRef: proofOutRef,
    createdByTxHash: hash("66"),
    retainedAtFinalState: true,
  },
  correction: {
    removalTxHash,
    removedStateQueueOutRef: outRef("55"),
    fraudulentHeaderAbsent: true,
    referencedProofTokenOutRef: proofOutRef,
  },
  economics: {
    operatorCredential: "33".repeat(28),
    proverCredential: "44".repeat(28),
    operatorBondInputOutRef: outRef("88"),
    operatorBondInputLovelace: "1000000000",
    slashedLovelace: "500000000",
    proverRewardOutputOutRef: outRef("99"),
    proverRewardLovelace: "250000000",
    removalFeeLovelace: "500000000",
    duplicateRewardAbsent: true,
  },
  observedAt: {
    slot: "1000",
    blockHash: hash("aa"),
    confirmationDepth: 30,
  },
});

const step04 = (threadOutRef: string) => ({
  kind: "step" as const,
  step: 4 as const,
  threadOutRef,
  stateQueueBlockOutRef: outRef("10"),
});

describe("production missing-signature authenticated cursor V1", () => {
  it("content-addresses every step-04 scan batch by its current thread outref", () => {
    const first = missingSignatureObservation({
      headerHash,
      provenance,
      stage: step04(outRef("41")),
    });
    const second = missingSignatureObservation({
      headerHash,
      provenance,
      stage: step04(outRef("42")),
    });
    expect(first).toMatchObject({
      kind: "action_required",
      action: {
        actionId: `step_04:${outRef("41")}:${outRef("10")}`,
      },
    });
    expect(second).toMatchObject({
      kind: "action_required",
      action: {
        actionId: `step_04:${outRef("42")}:${outRef("10")}`,
      },
    });
  });

  it("rejects skipped/reordered steps", () => {
    expect(() =>
      missingSignatureObservation({
        headerHash,
        provenance,
        stage: {
          kind: "step",
          step: 8,
          threadOutRef: outRef("51"),
          stateQueueBlockOutRef: outRef("10"),
        },
      }),
    ).toThrow("outside its exact production chain");
  });

  it("fails closed on private provenance and terminal substitution", () => {
    expect(() =>
      missingSignatureObservation({
        headerHash,
        provenance: { ...provenance, trustClass: "operator_private_file" },
        stage: step04(outRef("41")),
      }),
    ).toThrow("prohibited_trust_class");
    expect(() =>
      missingSignatureObservation({
        headerHash,
        provenance,
        stage: {
          kind: "removed",
          terminal: { ...terminal(), category: "invalidSignature" },
        },
      }),
    ).toThrow("changed its category or target header");
  });
});
