import { describe, expect, it } from "vitest";

import {
  type ManifestBoundMintDeclaredAssetLimitWorkflowConfig,
  mintDeclaredAssetLimitActionId,
  mintDeclaredAssetLimitSubmissionPrelude,
  reconcileMintDeclaredAssetLimitSubmissionIntent,
} from "../src/mint-declared-asset-limit/v1.js";

describe("mintDeclaredAssetLimit package-owned production V1", () => {
  it("exposes infrastructure-only config without evidence, journal, or submit callbacks", () => {
    const exactKeys = [
      "manifest",
      "blueprintJson",
      "deploymentInfo",
      "headerHash",
      "lucid",
      "signer",
      "source",
      "decisionDigest",
      "stateQueueMutationLeaseCoordinator",
      "referenceScripts",
    ] as const satisfies readonly (keyof ManifestBoundMintDeclaredAssetLimitWorkflowConfig)[];
    expect(exactKeys).not.toContain("evidence" as never);
    expect(exactKeys).not.toContain("loadJournal" as never);
    expect(exactKeys).not.toContain("appendJournal" as never);
    expect(exactKeys).not.toContain("submit" as never);
    expect(exactKeys).not.toContain("observeStage" as never);
  });

  it("journals the exact locally evaluated transaction intent before submission", () => {
    const txHash = "ab".repeat(32);
    const events = mintDeclaredAssetLimitSubmissionPrelude({
      actionId: "mintDeclaredAssetLimit:grammar:1",
      actionInput: {
        schemaVersion: "midgard-production-cursor-family-action-v1",
        category: "mintDeclaredAssetLimit",
        stage: "step_02",
        threadOutRef: `${"11".repeat(32)}#0`,
      },
      txHash,
      referenceScripts: [
        {
          role: "step_02",
          outRef: `${"22".repeat(32)}#0`,
          scriptHash: "33".repeat(28),
        },
      ],
      durableRecovery: {
        stateQueueMutationLease: { token: "lease-1", source: "emulator" },
      },
    });
    expect(events.map((event) => event.kind)).toEqual([
      "preflight_passed",
      "submission_intent",
    ]);
    expect(events[0].txHash).toBe(txHash);
    expect(events[1]).toMatchObject({
      actionId: "mintDeclaredAssetLimit:grammar:1",
      attempt: 1,
      txHash,
    });
  });

  it("resumes an identical self-loop cursor and refuses tx substitution", () => {
    const intended = {
      stage: "step_02" as const,
      threadOutRef: `${"11".repeat(32)}#0`,
      action: { kind: "grammar_resume" as const, nextOrdinal: 1 },
    };
    const intendedActionId = mintDeclaredAssetLimitActionId(intended);
    expect(
      reconcileMintDeclaredAssetLimitSubmissionIntent({
        intendedActionId,
        txHash: "aa".repeat(32),
        transactionConfirmed: false,
        observedAction: intended,
      }),
    ).toEqual({ kind: "pending", txHash: "aa".repeat(32) });
    expect(
      reconcileMintDeclaredAssetLimitSubmissionIntent({
        intendedActionId,
        txHash: "aa".repeat(32),
        transactionConfirmed: false,
        observedAction: {
          ...intended,
          threadOutRef: `${"22".repeat(32)}#0`,
        },
      }),
    ).toEqual({ kind: "conflict", txHash: "aa".repeat(32) });
  });
});
