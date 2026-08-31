import type { EvidenceProvenanceV1 } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import type { FraudProofWorkflowTerminalV1 } from "../src/workflow/journal-v1.js";
import {
  PRODUCTION_LINEAR_FAMILY_CATEGORIES_V1,
  PRODUCTION_LINEAR_FAMILY_SPECS_V1,
  productionLinearFamilySpecV1,
} from "../src/workflow/production-linear-family-spec-v1.js";
import {
  productionLinearFamilyObservationV1,
  reconcileProductionLinearFamilyActionV1,
} from "../src/workflow/production-linear-family-state-v1.js";

const hash = (byte: string): string => byte.repeat(32);
const headerHash = "ab".repeat(28);
const outRef = (byte: string, index = 0): string => `${hash(byte)}#${index}`;
const provenance: EvidenceProvenanceV1 = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "local-kupmios/kupo+ogmios",
  grade: "security",
};

const terminal = (
  category: (typeof PRODUCTION_LINEAR_FAMILY_CATEGORIES_V1)[number],
  removalTxHash = hash("55"),
  proofOutRef = outRef("44"),
): FraudProofWorkflowTerminalV1 => ({
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
  it("defines only fixed-terminal categories in exact closed order", () => {
    expect(
      PRODUCTION_LINEAR_FAMILY_SPECS_V1.map((row) => row.category),
    ).toEqual(PRODUCTION_LINEAR_FAMILY_CATEGORIES_V1);
    for (const row of PRODUCTION_LINEAR_FAMILY_SPECS_V1) {
      expect(row.steps.length).toBeGreaterThanOrEqual(1);
      expect(row.steps.length).toBeLessThanOrEqual(4);
      expect(row.steps.map((step) => step.ordinal)).toEqual(
        Array.from({ length: row.steps.length }, (_, index) => index + 1),
      );
      expect(row.steps.map((step) => step.terminalStep)).toEqual(
        row.steps.map((_, index) => index === row.steps.length - 1),
      );
    }
  });

  it("refuses missing-signature because step-04 is a cursor-driven self-loop", () => {
    expect(PRODUCTION_LINEAR_FAMILY_CATEGORIES_V1).not.toContain(
      "missingSignature",
    );
    expect(() =>
      productionLinearFamilySpecV1("missingSignature" as "daHashPreimage"),
    ).toThrow("no production linear family spec");
  });

  it("content-addresses init, every exact step, and removal", () => {
    const init = productionLinearFamilyObservationV1({
      category: "daHashPreimage",
      headerHash,
      provenance,
      stage: { kind: "not_started", stateQueueBlockOutRef: outRef("11") },
    });
    expect(init).toMatchObject({
      kind: "action_required",
      action: { actionId: `init:${outRef("11")}` },
    });
    const step = productionLinearFamilyObservationV1({
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
    const removal = productionLinearFamilyObservationV1({
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
    const current = productionLinearFamilyObservationV1({
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
      reconcileProductionLinearFamilyActionV1({
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
      reconcileProductionLinearFamilyActionV1({
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

  it("rejects hostile category, step-order, provenance, and terminal substitution", () => {
    expect(() =>
      productionLinearFamilyObservationV1({
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
      productionLinearFamilyObservationV1({
        category: "daHashPreimage",
        headerHash,
        provenance: { ...provenance, trustClass: "operator_private_file" },
        stage: { kind: "not_started", stateQueueBlockOutRef: outRef("11") },
      }),
    ).toThrow("prohibited_trust_class");
    expect(() =>
      productionLinearFamilyObservationV1({
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

  it("fails closed on stale/reordered states but permits an unincluded stale removal to rebuild", async () => {
    const proof = productionLinearFamilyObservationV1({
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
      reconcileProductionLinearFamilyActionV1({
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
    ).resolves.toEqual({ kind: "not_found" });

    await expect(
      reconcileProductionLinearFamilyActionV1({
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
    const proof = productionLinearFamilyObservationV1({
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
      reconcileProductionLinearFamilyActionV1({
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
      reconcileProductionLinearFamilyActionV1({
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
      reconcileProductionLinearFamilyActionV1({
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
    for (const category of PRODUCTION_LINEAR_FAMILY_CATEGORIES_V1) {
      expect(productionLinearFamilySpecV1(category).category).toBe(category);
    }
    expect(() =>
      productionLinearFamilySpecV1("networkId" as "daHashPreimage"),
    ).toThrow("no production linear family spec");
  });
});
