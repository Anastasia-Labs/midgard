import type { EvidenceProvenanceV1 } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import type { FraudProofWorkflowTerminalV1 } from "../src/workflow/journal-v1.js";
import {
  productionMissingSignatureObservationV1,
  reconcileProductionMissingSignatureActionV1,
} from "../src/workflow/production-missing-signature-state-v1.js";

const hash = (byte: string): string => byte.repeat(32);
const headerHash = "ab".repeat(28);
const outRef = (byte: string, index = 0): string => `${hash(byte)}#${index}`;
const provenance: EvidenceProvenanceV1 = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "local-kupmios/kupo+ogmios",
  grade: "security",
};

const terminal = (
  removalTxHash = hash("77"),
  proofOutRef = outRef("66"),
): FraudProofWorkflowTerminalV1 => ({
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
    const first = productionMissingSignatureObservationV1({
      headerHash,
      provenance,
      stage: step04(outRef("41")),
    });
    const second = productionMissingSignatureObservationV1({
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

  it("accepts multiple transaction-created step-04 successors before finalization", async () => {
    const first = productionMissingSignatureObservationV1({
      headerHash,
      provenance,
      stage: step04(outRef("41")),
    });
    if (first.kind !== "action_required") throw new Error("missing action");
    const firstTx = hash("42");
    await expect(
      reconcileProductionMissingSignatureActionV1({
        headerHash,
        action: first.action,
        txHash: firstTx,
        provenance,
        stage: step04(`${firstTx}#0`),
        transactionConfirmed: async () => true,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: firstTx });

    const second = productionMissingSignatureObservationV1({
      headerHash,
      provenance,
      stage: step04(`${firstTx}#0`),
    });
    if (second.kind !== "action_required") throw new Error("missing action");
    const secondTx = hash("43");
    await expect(
      reconcileProductionMissingSignatureActionV1({
        headerHash,
        action: second.action,
        txHash: secondTx,
        provenance,
        stage: step04(`${secondTx}#1`),
        transactionConfirmed: async () => true,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: secondTx });

    const final = productionMissingSignatureObservationV1({
      headerHash,
      provenance,
      stage: step04(`${secondTx}#1`),
    });
    if (final.kind !== "action_required") throw new Error("missing action");
    const finalTx = hash("66");
    await expect(
      reconcileProductionMissingSignatureActionV1({
        headerHash,
        action: final.action,
        txHash: finalTx,
        provenance,
        stage: {
          kind: "proof_token",
          fraudProofOutRef: `${finalTx}#0`,
          stateQueueBlockOutRef: outRef("10"),
          nextRemovalOutRef: outRef("55"),
        },
        transactionConfirmed: async () => true,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: finalTx });
  });

  it("rejects skipped/reordered steps and substituted step-04 successors", async () => {
    expect(() =>
      productionMissingSignatureObservationV1({
        headerHash,
        provenance,
        stage: {
          kind: "step",
          step: 5,
          threadOutRef: outRef("51"),
          stateQueueBlockOutRef: outRef("10"),
        },
      }),
    ).toThrow("outside its exact production chain");

    const required = productionMissingSignatureObservationV1({
      headerHash,
      provenance,
      stage: step04(outRef("41")),
    });
    if (required.kind !== "action_required") throw new Error("missing action");
    await expect(
      reconcileProductionMissingSignatureActionV1({
        headerHash,
        action: required.action,
        txHash: hash("42"),
        provenance,
        stage: step04(outRef("99")),
        transactionConfirmed: async () => true,
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
  });

  it("fails closed on private provenance and terminal substitution", () => {
    expect(() =>
      productionMissingSignatureObservationV1({
        headerHash,
        provenance: { ...provenance, trustClass: "operator_private_file" },
        stage: step04(outRef("41")),
      }),
    ).toThrow("prohibited_trust_class");
    expect(() =>
      productionMissingSignatureObservationV1({
        headerHash,
        provenance,
        stage: {
          kind: "removed",
          terminal: { ...terminal(), category: "invalidSignature" },
        },
      }),
    ).toThrow("changed its category or target header");
  });

  it("binds correction to the exact removal transaction and proof outref", async () => {
    const proofOutRef = outRef("66");
    const required = productionMissingSignatureObservationV1({
      headerHash,
      provenance,
      stage: {
        kind: "proof_token",
        fraudProofOutRef: proofOutRef,
        stateQueueBlockOutRef: outRef("10"),
        nextRemovalOutRef: outRef("55"),
      },
    });
    if (required.kind !== "action_required") throw new Error("missing action");
    const removalTx = hash("77");
    await expect(
      reconcileProductionMissingSignatureActionV1({
        headerHash,
        action: required.action,
        txHash: removalTx,
        provenance,
        stage: { kind: "removed", terminal: terminal(removalTx, proofOutRef) },
        transactionConfirmed: async () => true,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: removalTx });

    await expect(
      reconcileProductionMissingSignatureActionV1({
        headerHash,
        action: required.action,
        txHash: removalTx,
        provenance,
        stage: {
          kind: "removed",
          terminal: terminal(removalTx, outRef("67")),
        },
        transactionConfirmed: async () => true,
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
  });
});
