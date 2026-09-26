import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";

import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
  computeFraudProofReleaseFinalityPolicyDigest,
  computeFraudProofWorkflowId,
  createWorkflowReconciliationPermitController,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalEvent,
  type HeaderFaultDecision,
  journalJsonDigest,
  MemoryFraudProofWorkflowJournalStore,
  normalizeJournalJson,
  resumeRecordedFraudProofWorkflow,
} from "@al-ft/midgard-fault-proofs";
import { describe, expect, it, vi } from "vitest";

import { unsafeOpenWatcherFaultDecisionJournalForTest } from "../../src/fault-proofs/fault-decision-journal.js";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "../../src/fault-proofs/fault-proof-application.js";
import { createWatcherFaultProofSupervisor } from "../../src/fault-proofs/fault-proof-supervisor.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";

const DEPLOYMENT = "dd".repeat(32),
  HEADER = "aa".repeat(28),
  TX = "bb".repeat(32);
const policy = {
  confirmationDepth: DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;
const releaseFinality = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT,
  blueprintHash: "ee".repeat(32),
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
  policy,
};
const fixture = (): {
  decision: HeaderFaultDecision;
  entries: FraudProofWorkflowJournalEntry[];
} => {
  const envelope = {
    schemaVersion: "midgard-production-header-decision-v1",
    classifierVersion: "midgard-production-header-classifier-v1",
    deploymentFingerprint: DEPLOYMENT,
    headerHash: HEADER,
    authenticatedObservationDigest: "11".repeat(32),
    payloadEnvelopeSha256: "12".repeat(32),
    payloadSha256: "13".repeat(32),
    replayVersion: "midgard-complete-canonical-replay-v1",
    replayDigest: "14".repeat(32),
    launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    launchScopeDigest: journalJsonDigest([
      ...WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    ]),
    classificationDigest: "16".repeat(32),
    decision: "fault_detected",
    category: "transitionTrace",
    violationId: "transitionTrace_v1",
    detectionId: "transitionTrace_v1:0",
    position: "0",
  } as const;
  const decision = {
    ...envelope,
    decisionDigest: journalJsonDigest(normalizeJournalJson(envelope)),
  };
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: DEPLOYMENT,
    category: "transitionTrace",
    target: { kind: "state_queue_header", headerHash: HEADER },
    decisionDigest: decision.decisionDigest,
  } as const;
  const artifact = {
    releaseFinality,
    evidenceBinding: {
      route: "canonical_block",
      headerHash: HEADER,
      payloadEnvelopeSha256: decision.payloadEnvelopeSha256,
      payloadSha256: decision.payloadSha256,
      l1BlockHash: "17".repeat(32),
      l1Slot: "42",
    },
    familyArtifact: { test: true },
  };
  const events: FraudProofWorkflowJournalEvent[] = [
    { kind: "started" },
    { kind: "prepared", artifact, artifactDigest: journalJsonDigest(artifact) },
    {
      kind: "preflight_passed",
      actionId: "remove",
      txHash: TX,
      localEvaluator: "lucid-local-uplc",
      referenceScripts: [],
    },
    {
      kind: "submission_intent",
      actionId: "remove",
      actionInput: { stage: "remove" },
      attempt: 1,
      txHash: TX,
    },
  ];
  const entries = events.map((event, sequence) => ({
    schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
    identity,
    workflowId: computeFraudProofWorkflowId(identity),
    recordedAt: "2026-09-10T00:00:00.000Z",
    sequence,
    event,
  }));
  return { decision, entries };
};

describe("existing signed workflow recovery authority", () => {
  it("schedules an exact target-absent journal without a new-start deadline and refuses every spend", async () => {
    const root = await mkdtemp("/var/tmp/midgard-reconciliation-test-");
    const { decision, entries } = fixture();
    const decisionJournal = await unsafeOpenWatcherFaultDecisionJournalForTest({
      directory: root,
      deploymentFingerprint: DEPLOYMENT,
      launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    });
    await decisionJournal.unsafeAppendDecisionEnvelopeForTest(decision);
    const journal = new DirectoryFraudProofWorkflowJournalStore(
      join(root, "fault-proofs", "transitionTrace", HEADER),
    );
    for (const entry of entries) await journal.append(entry, entry.sequence);
    const controller = createWorkflowReconciliationPermitController({
      decision,
      entries,
      deploymentFingerprint: DEPLOYMENT,
      rollbackGeneration: "0",
    });
    bindWorkflowActuationJournal({
      journal,
      permit: controller.permit,
      decisionDigest: decision.decisionDigest,
      deploymentFingerprint: DEPLOYMENT,
      category: "transitionTrace",
      headerHash: HEADER,
    });
    const check = (
      checkpoint: Parameters<
        typeof assertWorkflowJournalActuation
      >[0]["checkpoint"],
    ) =>
      assertWorkflowJournalActuation({
        journal,
        deploymentFingerprint: DEPLOYMENT,
        category: "transitionTrace",
        headerHash: HEADER,
        checkpoint,
      });
    let ran = 0;
    const supervisor = createWatcherFaultProofSupervisor({
      journalRoot: root,
      deploymentFingerprint: DEPLOYMENT,
      deadlineAlertHeadroomMs: 3600000,
      queueAuthenticationKey: new Uint8Array(32).fill(0xa5),
      execution: {
        verifyCompleted: async () => {
          throw new Error("unexpected completed execution");
        },
        execute: async ({ job }) => {
          expect(job.deadline).toBeNull();
          check("before_reconcile");
          check("before_terminal_verify");
          expect(() => check("before_preflight")).toThrow(
            "existing_signed_workflow_only",
          );
          expect(() => check("before_submit")).toThrow(
            "existing_signed_workflow_only",
          );
          ran++;
          return {
            kind: "pending",
            resume: "await_observation",
            reason: "retained signed attempt",
          };
        },
      },
    });
    try {
      await expect(
        supervisor.requestProgress({
          observation: progressObservation({
            deploymentFingerprint: DEPLOYMENT,
          }),
          rollbackGeneration: "0",
        }),
      ).resolves.toBeUndefined();
      await supervisor.close();
      expect(ran).toBe(1);
      controller.revoke("native_chain_rollback");
      expect(() => check("before_reconcile")).toThrow("native_chain_rollback");
    } finally {
      await supervisor.close();
      await rm(root, { recursive: true, force: true });
    }
  });
  it("uses the existing adapter and recorded evidence without replay, observation, preparation, or submission", async () => {
    const { decision, entries } = fixture();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    for (const entry of entries) await journal.append(entry, entry.sequence);
    const controller = createWorkflowReconciliationPermitController({
      decision,
      entries,
      deploymentFingerprint: DEPLOYMENT,
      rollbackGeneration: "0",
    });
    bindWorkflowActuationJournal({
      journal,
      permit: controller.permit,
      decisionDigest: decision.decisionDigest,
      deploymentFingerprint: DEPLOYMENT,
      category: "transitionTrace",
      headerHash: HEADER,
    });
    const forbidden = vi.fn(async (): Promise<never> => {
      throw new Error("new execution forbidden");
    });
    const reconcile = vi.fn(async () => ({
      kind: "pending" as const,
      txHash: TX,
    }));
    const result = await resumeRecordedFraudProofWorkflow({
      deploymentFingerprint: DEPLOYMENT,
      category: "transitionTrace",
      headerHash: HEADER,
      journal,
      adapter: {
        adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
        category: "transitionTrace",
        safety: FRAUD_PROOF_WORKFLOW_SAFETY,
        prepare: forbidden,
        observe: forbidden,
        preflight: forbidden,
        submit: forbidden,
        reconcile,
      },
      terminalVerifier: {
        verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
        verify: forbidden,
      },
      releaseFinalityAuthority: {
        authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
        verifyForWorkflow: async () => releaseFinality,
      },
    });
    expect(result.kind).toBe("pending");
    expect(reconcile).toHaveBeenCalledOnce();
    expect(forbidden).not.toHaveBeenCalled();
    if (result.kind !== "pending")
      throw new Error("expected pending recorded transaction");
    expect(result.entries.slice(0, entries.length)).toEqual(entries);
  });
  it("rejects missing signed intent, substituted evidence, and foreign deployment", () => {
    const { decision, entries } = fixture();
    const mint = (
      changes: Partial<
        Parameters<typeof createWorkflowReconciliationPermitController>[0]
      >,
    ) =>
      createWorkflowReconciliationPermitController({
        decision,
        entries,
        deploymentFingerprint: DEPLOYMENT,
        rollbackGeneration: "0",
        ...changes,
      });
    expect(() => mint({ entries: entries.slice(0, 3) })).toThrow(
      "exact existing signed workflow",
    );
    expect(() => mint({ deploymentFingerprint: "ff".repeat(32) })).toThrow(
      "exact existing signed workflow",
    );
    expect(() =>
      mint({ decision: { ...decision, payloadSha256: "fe".repeat(32) } }),
    ).toThrow("exact existing signed workflow");
  });
});
