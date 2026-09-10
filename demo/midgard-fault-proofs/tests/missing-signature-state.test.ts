import type { EvidenceProvenance } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import type { FraudProofWorkflowTerminal } from "../src/workflow/journal.js";
import {
  missingSignatureObservation,
  reconcileMissingSignatureAction,
} from "../src/workflow/missing-signature-state.js";

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
  it("keeps known transactions pending while finalized state is unchanged", async () => {
    for (const stage of [
      { kind: "not_started" as const, stateQueueBlockOutRef: outRef("10") },
      {
        kind: "step" as const,
        step: 1 as const,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      },
      {
        kind: "proof_token" as const,
        fraudProofOutRef: outRef("44"),
        stateQueueBlockOutRef: outRef("10"),
        nextRemovalOutRef: outRef("33"),
      },
    ]) {
      const observed = missingSignatureObservation({
        headerHash,
        provenance,
        stage,
      });
      if (observed.kind !== "action_required")
        throw new Error("missing action");
      const input = {
        headerHash,
        provenance,
        stage,
        action: observed.action,
        transactionConfirmed: async () => false,
      } as const;
      await expect(
        reconcileMissingSignatureAction({ ...input, txHash: hash("55") }),
      ).resolves.toEqual({ kind: "pending", txHash: hash("55") });
      await expect(reconcileMissingSignatureAction(input)).resolves.toEqual({
        kind: "not_found",
      });
      if (stage.kind === "proof_token") {
        await expect(
          reconcileMissingSignatureAction({
            ...input,
            txHash: hash("55"),
            stage: { ...stage, nextRemovalOutRef: outRef("34") },
          }),
        ).resolves.toEqual({ kind: "pending", txHash: hash("55") });
      }
    }
  });

  it("rejects changed successors when transaction history does not confirm the intent", async () => {
    const observed = missingSignatureObservation({
      headerHash,
      provenance,
      stage: { kind: "not_started", stateQueueBlockOutRef: outRef("10") },
    });
    if (observed.kind !== "action_required") throw new Error("missing action");
    for (const threadOutRef of [outRef("55"), outRef("99")]) {
      await expect(
        reconcileMissingSignatureAction({
          headerHash,
          provenance,
          action: observed.action,
          txHash: hash("55"),
          stage: {
            kind: "step",
            step: 1,
            threadOutRef,
            stateQueueBlockOutRef: outRef("10"),
          },
          transactionConfirmed: async () => false,
        }),
      ).resolves.toMatchObject({ kind: "conflict" });
    }
  });

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

  it("accepts multiple transaction-created step-04 successors before finalization", async () => {
    const first = missingSignatureObservation({
      headerHash,
      provenance,
      stage: step04(outRef("41")),
    });
    if (first.kind !== "action_required") throw new Error("missing action");
    const firstTx = hash("42");
    await expect(
      reconcileMissingSignatureAction({
        headerHash,
        action: first.action,
        txHash: firstTx,
        provenance,
        stage: step04(`${firstTx}#0`),
        transactionConfirmed: async () => true,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: firstTx });

    const second = missingSignatureObservation({
      headerHash,
      provenance,
      stage: step04(`${firstTx}#0`),
    });
    if (second.kind !== "action_required") throw new Error("missing action");
    const secondTx = hash("43");
    await expect(
      reconcileMissingSignatureAction({
        headerHash,
        action: second.action,
        txHash: secondTx,
        provenance,
        stage: step04(`${secondTx}#1`),
        transactionConfirmed: async () => true,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: secondTx });

    const final = missingSignatureObservation({
      headerHash,
      provenance,
      stage: step04(`${secondTx}#1`),
    });
    if (final.kind !== "action_required") throw new Error("missing action");
    const finalTx = hash("66");
    await expect(
      reconcileMissingSignatureAction({
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

    const required = missingSignatureObservation({
      headerHash,
      provenance,
      stage: step04(outRef("41")),
    });
    if (required.kind !== "action_required") throw new Error("missing action");
    await expect(
      reconcileMissingSignatureAction({
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

  it("binds correction to the exact removal transaction and proof outref", async () => {
    const proofOutRef = outRef("66");
    const required = missingSignatureObservation({
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
      reconcileMissingSignatureAction({
        headerHash,
        action: required.action,
        txHash: removalTx,
        provenance,
        stage: { kind: "removed", terminal: terminal(removalTx, proofOutRef) },
        transactionConfirmed: async () => true,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash: removalTx });

    await expect(
      reconcileMissingSignatureAction({
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
