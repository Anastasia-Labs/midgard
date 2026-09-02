import { describe, expect, it } from "vitest";

import type { ManifestBoundObserversForbiddenWorkflowConfigV1 } from "../src/observers-forbidden-on-untagged-network/production-v1.js";
import {
  observersForbiddenActionIdV1,
  observersForbiddenSubmissionPreludeV1,
  reconcileObserversForbiddenSubmissionIntentV1,
} from "../src/observers-forbidden-on-untagged-network/production-v1.js";

describe("observersForbiddenOnUntaggedNetwork durable production boundary", () => {
  it("exposes infrastructure-only config without core callback authority", () => {
    const keys = [
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
    ] satisfies readonly (keyof ManifestBoundObserversForbiddenWorkflowConfigV1)[];
    expect(keys).not.toContain("submit");
    expect(keys).not.toContain("loadJournal");
    expect(keys).not.toContain("appendJournal");
    expect(keys).not.toContain("prepareEvidence");
    expect(keys).not.toContain("resolveStage");
  });

  it("journals local preflight and intent before network submission", () => {
    const events = observersForbiddenSubmissionPreludeV1({
      actionId: "action-1",
      actionInput: {
        schemaVersion: "midgard-production-cursor-family-action-v1",
        category: "observersForbiddenOnUntaggedNetwork",
        stage: "step_02",
        threadOutRef: `${"01".repeat(32)}#0`,
      },
      txHash: "02".repeat(32),
      referenceScripts: [],
    });
    expect(events.map((event) => event.kind)).toEqual([
      "preflight_passed",
      "submission_intent",
    ]);
    expect(events[1].txHash).toBe("02".repeat(32));
  });

  it("retains a matching self-loop intent and refuses cursor substitution", () => {
    const action = {
      stage: "step_02" as const,
      threadOutRef: `${"03".repeat(32)}#0`,
    };
    const actionId = observersForbiddenActionIdV1(action);
    expect(
      reconcileObserversForbiddenSubmissionIntentV1({
        intendedActionId: actionId,
        txHash: "04".repeat(32),
        transactionConfirmed: false,
        observedAction: action,
      }).kind,
    ).toBe("pending");
    expect(
      reconcileObserversForbiddenSubmissionIntentV1({
        intendedActionId: actionId,
        txHash: "04".repeat(32),
        transactionConfirmed: false,
        observedAction: {
          ...action,
          threadOutRef: `${"05".repeat(32)}#0`,
        },
      }).kind,
    ).toBe("conflict");
  });

  it("accepts an exactly confirmed transaction identity", () => {
    expect(
      reconcileObserversForbiddenSubmissionIntentV1({
        intendedActionId: "action-1",
        txHash: "06".repeat(32),
        transactionConfirmed: true,
        observedAction: "removed",
      }),
    ).toEqual({ kind: "confirmed", txHash: "06".repeat(32) });
  });
});
