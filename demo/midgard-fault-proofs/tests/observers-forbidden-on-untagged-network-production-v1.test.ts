import { describe, expect, it } from "vitest";

import type { ManifestBoundObserversForbiddenWorkflowConfig } from "../src/observers-forbidden-on-untagged-network/production-v1.js";
import {
  observersForbiddenActionId,
  observersForbiddenSubmissionPrelude,
  reconcileObserversForbiddenSubmissionIntent,
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
    ] satisfies readonly (keyof ManifestBoundObserversForbiddenWorkflowConfig)[];
    expect(keys).not.toContain("submit");
    expect(keys).not.toContain("loadJournal");
    expect(keys).not.toContain("appendJournal");
    expect(keys).not.toContain("prepareEvidence");
    expect(keys).not.toContain("resolveStage");
  });

  it("journals local preflight and intent before network submission", () => {
    const events = observersForbiddenSubmissionPrelude({
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
    const actionId = observersForbiddenActionId(action);
    expect(
      reconcileObserversForbiddenSubmissionIntent({
        intendedActionId: actionId,
        txHash: "04".repeat(32),
        transactionConfirmed: false,
        observedAction: action,
      }).kind,
    ).toBe("pending");
    expect(
      reconcileObserversForbiddenSubmissionIntent({
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
      reconcileObserversForbiddenSubmissionIntent({
        intendedActionId: "action-1",
        txHash: "06".repeat(32),
        transactionConfirmed: true,
        observedAction: "removed",
      }),
    ).toEqual({ kind: "confirmed", txHash: "06".repeat(32) });
  });
});
