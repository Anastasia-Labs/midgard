import type { EvidenceProvenance } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import type { FraudProofWorkflowTerminal } from "../src/workflow/journal.js";
import {
  LINEAR_FAMILY_CATEGORIES,
  LINEAR_FAMILY_SPECS,
  linearFamilySpec,
} from "../src/workflow/linear-family-spec.js";
import {
  linearFamilyObservation,
  reconcileLinearFamilyAction,
} from "../src/workflow/linear-family-state.js";

const hash = (byte: string): string => byte.repeat(32);
const headerHash = "ab".repeat(28);
const outRef = (byte: string, index = 0): string => `${hash(byte)}#${index}`;
const provenance: EvidenceProvenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "local-kupmios/kupo+ogmios",
  grade: "security",
};

const terminal = (
  category: (typeof LINEAR_FAMILY_CATEGORIES)[number],
  removalTxHash = hash("55"),
  proofOutRef = outRef("44"),
): FraudProofWorkflowTerminal => ({
  schemaVersion: "midgard-fraud-proof-workflow-terminal-v1",
  category,
  headerHash,
  proofToken: {
    unit: "11".repeat(28) + "22".repeat(28),
    outRef: proofOutRef,
    createdByTxHash: hash("44"),
    retainedAtFinalState: true,
  },
  correction: {
    removalTxHash,
    removedStateQueueOutRef: outRef("33"),
    fraudulentHeaderAbsent: true,
    referencedProofTokenOutRef: proofOutRef,
  },
  economics: {
    operatorCredential: "66".repeat(28),
    proverCredential: "77".repeat(28),
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

describe("production linear family authenticated state machine V1", () => {
  it("recovers a changed header reference only while the exact computation step remains current", async () => {
    const initial = linearFamilyObservation({
      category: "committedFieldShape",
      headerHash,
      provenance,
      stage: {
        kind: "step",
        step: 1,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      },
    });
    if (initial.kind !== "action_required") throw new Error("missing step");
    const input = {
      category: "committedFieldShape",
      headerHash,
      provenance,
      action: initial.action,
      txHash: hash("55"),
      stage: {
        kind: "step",
        step: 1,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("20"),
      },
      transactionConfirmed: async () => false,
    } as const;
    await expect(reconcileLinearFamilyAction(input)).resolves.toEqual({
      kind: "pending",
      txHash: hash("55"),
    });
    const recovery = {
      ...input,
      recoverUnconfirmedTransaction: async () =>
        ({ kind: "not_found" }) as const,
    };
    await expect(reconcileLinearFamilyAction(recovery)).resolves.toEqual({
      kind: "not_found",
    });
    await expect(
      reconcileLinearFamilyAction({
        ...recovery,
        stage: { ...input.stage, threadOutRef: outRef("12") },
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
    await expect(
      reconcileLinearFamilyAction({
        ...recovery,
        stage: { ...input.stage, step: 2 },
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
    await expect(
      reconcileLinearFamilyAction({
        ...recovery,
        transactionConfirmed: async () => true,
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
  });

  it("reconciles signed init after DA apply recreates the same header at another outref", async () => {
    const initial = linearFamilyObservation({
      category: "committedFieldShape",
      headerHash,
      provenance,
      stage: { kind: "not_started", stateQueueBlockOutRef: outRef("10") },
    });
    if (initial.kind !== "action_required") throw new Error("missing init");
    const input = {
      category: "committedFieldShape",
      headerHash,
      provenance,
      action: initial.action,
      txHash: hash("55"),
      stage: { kind: "not_started", stateQueueBlockOutRef: outRef("20") },
      transactionConfirmed: async () => false,
    } as const;
    await expect(reconcileLinearFamilyAction(input)).resolves.toEqual({
      kind: "pending",
      txHash: hash("55"),
    });
    await expect(
      reconcileLinearFamilyAction({
        ...input,
        recoverUnconfirmedTransaction: async () => ({ kind: "not_found" }),
      }),
    ).resolves.toEqual({ kind: "not_found" });
    await expect(
      reconcileLinearFamilyAction({
        ...input,
        transactionConfirmed: async () => true,
        recoverUnconfirmedTransaction: async () => ({ kind: "not_found" }),
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
  });

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
      const observed = linearFamilyObservation({
        category: "daHashPreimage",
        headerHash,
        provenance,
        stage,
      });
      if (observed.kind !== "action_required")
        throw new Error("missing action");
      const input = {
        category: "daHashPreimage",
        headerHash,
        provenance,
        stage,
        action: observed.action,
        transactionConfirmed: async () => false,
      } as const;
      await expect(
        reconcileLinearFamilyAction({ ...input, txHash: hash("55") }),
      ).resolves.toEqual({ kind: "pending", txHash: hash("55") });
      await expect(reconcileLinearFamilyAction(input)).resolves.toEqual({
        kind: "not_found",
      });
      if (stage.kind === "proof_token") {
        await expect(
          reconcileLinearFamilyAction({
            ...input,
            txHash: hash("55"),
            stage: { ...stage, nextRemovalOutRef: outRef("34") },
          }),
        ).resolves.toEqual({ kind: "pending", txHash: hash("55") });
      }
    }
  });

  it("rejects changed successors when transaction history does not confirm the intent", async () => {
    const observed = linearFamilyObservation({
      category: "daHashPreimage",
      headerHash,
      provenance,
      stage: { kind: "not_started", stateQueueBlockOutRef: outRef("10") },
    });
    if (observed.kind !== "action_required") throw new Error("missing action");
    for (const threadOutRef of [outRef("55"), outRef("99")]) {
      await expect(
        reconcileLinearFamilyAction({
          category: "daHashPreimage",
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

  it("defines only fixed-terminal categories in exact closed order", () => {
    expect(LINEAR_FAMILY_SPECS.map((row) => row.category)).toEqual(
      LINEAR_FAMILY_CATEGORIES,
    );
    for (const row of LINEAR_FAMILY_SPECS) {
      expect(row.steps.length).toBeGreaterThanOrEqual(1);
      expect(row.steps.length).toBeLessThanOrEqual(4);
      expect(row.steps.map((step) => step.ordinal)).toEqual(
        Array.from({ length: row.steps.length }, (_, index) => index + 1),
      );
      expect(row.steps.map((step) => step.terminalStep)).toEqual(
        row.steps.map((step) => step.successors.includes("proof_token")),
      );
      expect(row.steps.at(-1)?.terminalStep).toBe(true);
      for (const step of row.steps) {
        expect(step.successors.length).toBeGreaterThanOrEqual(1);
        for (const successor of step.successors) {
          if (successor === "proof_token") continue;
          expect(successor).toBeGreaterThan(step.ordinal);
          expect(successor).toBeLessThanOrEqual(row.steps.length);
        }
      }
      if (row.category !== "inputSetUniqueness") {
        expect(row.steps.map((step) => step.successors)).toEqual(
          row.steps.map((step, index) =>
            index === row.steps.length - 1
              ? ["proof_token"]
              : [step.ordinal + 1],
          ),
        );
      }
    }
  });

  it("declares the accepted and forced input-set-uniqueness branches", () => {
    expect(
      linearFamilySpec("inputSetUniqueness").steps.map(
        (step) => step.successors,
      ),
    ).toEqual([[2, 3], ["proof_token"], [4], ["proof_token"]]);
  });

  it("refuses missing-signature because step-04 is a cursor-driven self-loop", () => {
    expect(LINEAR_FAMILY_CATEGORIES).not.toContain("missingSignature");
    expect(() =>
      linearFamilySpec("missingSignature" as "daHashPreimage"),
    ).toThrow("no production linear family spec");
  });

  it("content-addresses init, every exact step, and removal", () => {
    const init = linearFamilyObservation({
      category: "daHashPreimage",
      headerHash,
      provenance,
      stage: { kind: "not_started", stateQueueBlockOutRef: outRef("11") },
    });
    expect(init).toMatchObject({
      kind: "action_required",
      action: { actionId: `init:${outRef("11")}` },
    });
    const step = linearFamilyObservation({
      category: "daHashPreimage",
      headerHash,
      provenance,
      stage: {
        kind: "step",
        step: 2,
        threadOutRef: outRef("22"),
        stateQueueBlockOutRef: outRef("11"),
      },
    });
    expect(step).toMatchObject({
      kind: "action_required",
      action: {
        actionId: `step_02:${outRef("22")}:${outRef("11")}`,
      },
    });
    const removal = linearFamilyObservation({
      category: "daHashPreimage",
      headerHash,
      provenance,
      stage: {
        kind: "proof_token",
        fraudProofOutRef: outRef("44"),
        stateQueueBlockOutRef: outRef("11"),
        nextRemovalOutRef: outRef("33"),
      },
    });
    expect(removal).toMatchObject({
      kind: "action_required",
      action: {
        actionId: `remove:${outRef("33")}:${outRef("44")}:${outRef("11")}`,
      },
    });
  });

  it("accepts only the immediate transaction-created successor", async () => {
    const current = linearFamilyObservation({
      category: "daHashPreimage",
      headerHash,
      provenance,
      stage: {
        kind: "step",
        step: 1,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      },
    });
    if (current.kind !== "action_required") throw new Error("missing action");
    const txHash = hash("22");
    await expect(
      reconcileLinearFamilyAction({
        category: "daHashPreimage",
        headerHash,
        action: current.action,
        txHash,
        provenance,
        stage: {
          kind: "step",
          step: 2,
          threadOutRef: `${txHash}#0`,
          stateQueueBlockOutRef: outRef("10"),
        },
        transactionConfirmed: async () => true,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash });

    await expect(
      reconcileLinearFamilyAction({
        category: "daHashPreimage",
        headerHash,
        action: current.action,
        txHash,
        provenance,
        stage: {
          kind: "proof_token",
          fraudProofOutRef: `${txHash}#0`,
          stateQueueBlockOutRef: outRef("10"),
          nextRemovalOutRef: outRef("33"),
        },
        transactionConfirmed: async () => true,
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
  });

  it("accepts every declared branch successor and nothing else", async () => {
    const at = (step: 1 | 2 | 3 | 4, threadOutRef: string) =>
      linearFamilyObservation({
        category: "inputSetUniqueness",
        headerHash,
        provenance,
        stage: {
          kind: "step",
          step,
          threadOutRef,
          stateQueueBlockOutRef: outRef("10"),
        },
      });
    const reconcile = (
      action: ReturnType<typeof at>,
      txHash: string,
      stage: Parameters<typeof reconcileLinearFamilyAction>[0]["stage"],
    ) => {
      if (action.kind !== "action_required") throw new Error("missing action");
      return reconcileLinearFamilyAction({
        category: "inputSetUniqueness",
        headerHash,
        action: action.action,
        txHash,
        provenance,
        stage,
        transactionConfirmed: async () => true,
      });
    };
    const step = (
      ordinal: 1 | 2 | 3 | 4,
      txHash: string,
    ): Parameters<typeof reconcileLinearFamilyAction>[0]["stage"] => ({
      kind: "step",
      step: ordinal,
      threadOutRef: `${txHash}#0`,
      stateQueueBlockOutRef: outRef("10"),
    });
    const proof = (
      txHash: string,
    ): Parameters<typeof reconcileLinearFamilyAction>[0]["stage"] => ({
      kind: "proof_token",
      fraudProofOutRef: `${txHash}#0`,
      stateQueueBlockOutRef: outRef("10"),
      nextRemovalOutRef: outRef("33"),
    });
    const txHash = hash("22");
    // Accepted evidence: step-01 -> step-02 -> proof token.
    await expect(
      reconcile(at(1, outRef("11")), txHash, step(2, txHash)),
    ).resolves.toEqual({ kind: "confirmed", txHash });
    await expect(
      reconcile(at(2, outRef("11")), txHash, proof(txHash)),
    ).resolves.toEqual({ kind: "confirmed", txHash });
    // Forced evidence: step-01 -> step-03 -> step-04 -> proof token.
    await expect(
      reconcile(at(1, outRef("11")), txHash, step(3, txHash)),
    ).resolves.toEqual({ kind: "confirmed", txHash });
    await expect(
      reconcile(at(3, outRef("11")), txHash, step(4, txHash)),
    ).resolves.toEqual({ kind: "confirmed", txHash });
    await expect(
      reconcile(at(4, outRef("11")), txHash, proof(txHash)),
    ).resolves.toEqual({ kind: "confirmed", txHash });
    // Undeclared edges stay conflicts.
    await expect(
      reconcile(at(2, outRef("11")), txHash, step(3, txHash)),
    ).resolves.toMatchObject({ kind: "conflict" });
    await expect(
      reconcile(at(1, outRef("11")), txHash, step(4, txHash)),
    ).resolves.toMatchObject({ kind: "conflict" });
    await expect(
      reconcile(at(1, outRef("11")), txHash, proof(txHash)),
    ).resolves.toMatchObject({ kind: "conflict" });
    await expect(
      reconcile(at(3, outRef("11")), txHash, proof(txHash)),
    ).resolves.toMatchObject({ kind: "conflict" });
    // A straight four-step chain still refuses to skip a step.
    const skip = linearFamilyObservation({
      category: "nonExistentInput",
      headerHash,
      provenance,
      stage: {
        kind: "step",
        step: 1,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      },
    });
    if (skip.kind !== "action_required") throw new Error("missing action");
    await expect(
      reconcileLinearFamilyAction({
        category: "nonExistentInput",
        headerHash,
        action: skip.action,
        txHash,
        provenance,
        stage: step(3, txHash),
        transactionConfirmed: async () => true,
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
  });

  it("rejects hostile category, step-order, provenance, and terminal substitution", () => {
    expect(() =>
      linearFamilyObservation({
        category: "daHashPreimage",
        headerHash,
        provenance,
        stage: {
          kind: "step",
          step: 3,
          threadOutRef: outRef("22"),
          stateQueueBlockOutRef: outRef("11"),
        },
      }),
    ).toThrow("outside its exact production chain");
    expect(() =>
      linearFamilyObservation({
        category: "daHashPreimage",
        headerHash,
        provenance: { ...provenance, trustClass: "operator_private_file" },
        stage: { kind: "not_started", stateQueueBlockOutRef: outRef("11") },
      }),
    ).toThrow("prohibited_trust_class");
    expect(() =>
      linearFamilyObservation({
        category: "daHashPreimage",
        headerHash,
        provenance,
        stage: {
          kind: "removed",
          terminal: terminal("minFee"),
        },
      }),
    ).toThrow("changed its category or target header");
  });

  it("fails closed on reordered states and retains an unconfirmed stale removal", async () => {
    const proof = linearFamilyObservation({
      category: "daHashPreimage",
      headerHash,
      provenance,
      stage: {
        kind: "proof_token",
        fraudProofOutRef: outRef("44"),
        stateQueueBlockOutRef: outRef("11"),
        nextRemovalOutRef: outRef("33"),
      },
    });
    if (proof.kind !== "action_required") throw new Error("missing action");
    await expect(
      reconcileLinearFamilyAction({
        category: "daHashPreimage",
        headerHash,
        action: proof.action,
        txHash: hash("55"),
        provenance,
        stage: {
          kind: "proof_token",
          fraudProofOutRef: outRef("44"),
          stateQueueBlockOutRef: outRef("11"),
          nextRemovalOutRef: outRef("34"),
        },
        transactionConfirmed: async () => false,
      }),
    ).resolves.toEqual({ kind: "pending", txHash: hash("55") });

    await expect(
      reconcileLinearFamilyAction({
        category: "daHashPreimage",
        headerHash,
        action: proof.action,
        txHash: hash("55"),
        provenance,
        stage: { kind: "removed", terminal: terminal("daHashPreimage") },
        transactionConfirmed: async () => false,
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
  });

  it("binds terminal removal to the intended transaction and retained proof outref", async () => {
    const proofOutRef = outRef("44");
    const proof = linearFamilyObservation({
      category: "daHashPreimage",
      headerHash,
      provenance,
      stage: {
        kind: "proof_token",
        fraudProofOutRef: proofOutRef,
        stateQueueBlockOutRef: outRef("11"),
        nextRemovalOutRef: outRef("33"),
      },
    });
    if (proof.kind !== "action_required") throw new Error("missing action");
    const txHash = hash("55");
    await expect(
      reconcileLinearFamilyAction({
        category: "daHashPreimage",
        headerHash,
        action: proof.action,
        txHash,
        provenance,
        stage: {
          kind: "removed",
          terminal: terminal("daHashPreimage", txHash, proofOutRef),
        },
        transactionConfirmed: async () => true,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash });

    await expect(
      reconcileLinearFamilyAction({
        category: "daHashPreimage",
        headerHash,
        action: proof.action,
        txHash,
        provenance,
        stage: {
          kind: "removed",
          terminal: terminal("daHashPreimage", txHash, outRef("45")),
        },
        transactionConfirmed: async () => true,
      }),
    ).resolves.toMatchObject({ kind: "conflict" });

    await expect(
      reconcileLinearFamilyAction({
        category: "daHashPreimage",
        headerHash,
        action: proof.action,
        txHash,
        provenance,
        stage: {
          kind: "removed",
          terminal: {
            ...terminal("daHashPreimage", txHash, proofOutRef),
            correction: {
              ...terminal("daHashPreimage", txHash, proofOutRef).correction,
              removedStateQueueOutRef: outRef("34"),
            },
          },
        },
        transactionConfirmed: async () => true,
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
  });

  it("keeps every family lookup exact and category-bound", () => {
    for (const category of LINEAR_FAMILY_CATEGORIES) {
      expect(linearFamilySpec(category).category).toBe(category);
    }
    expect(() => linearFamilySpec("networkId" as "daHashPreimage")).toThrow(
      "no production linear family spec",
    );
  });
});
