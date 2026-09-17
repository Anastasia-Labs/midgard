import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  COMPLETE_CANONICAL_REPLAY,
  computeFraudProofWorkflowId,
  createWorkflowReconciliationPermitController,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
  HEADER_CLASSIFIER,
  HEADER_DECISION,
  type HeaderFaultDecision,
  journalJsonDigest,
  normalizeJournalJson,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it, vi } from "vitest";

import { unsafeOpenWatcherFaultDecisionJournalForTest } from "../../src/fault-proofs/fault-decision-journal.js";
import { authorizeWatcherProverFundingRecovery } from "../../src/funding/prover-funding-recovery.js";
import type { WatcherProverFundingReservationStore } from "../../src/funding/prover-funding-reservation.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";

const directories: string[] = [];
afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
});

const setup = async () => {
  const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
  const journalRoot = await mkdtemp(
    join(process.cwd(), ".mint-funding-recovery-"),
  );
  directories.push(journalRoot);
  const launchScope = ["mintItemNonCanonical"] as const;
  const txId = "22".repeat(32);
  const unsealed = {
    schemaVersion: HEADER_DECISION,
    classifierVersion: HEADER_CLASSIFIER,
    deploymentFingerprint: deploymentIdentity.manifestId,
    headerHash: "33".repeat(28),
    authenticatedObservationDigest: "44".repeat(32),
    payloadEnvelopeSha256: "55".repeat(32),
    payloadSha256: "66".repeat(32),
    replayVersion: COMPLETE_CANONICAL_REPLAY,
    replayDigest: "77".repeat(32),
    launchScope,
    launchScopeDigest: journalJsonDigest(normalizeJournalJson(launchScope)),
    classificationDigest: "88".repeat(32),
    decision: "fault_detected" as const,
    category: "mintItemNonCanonical" as const,
    violationId: "mint-item-non-canonical",
    detectionId: `mint-item-non-canonical:0:${txId}:0`,
    position: "0",
  };
  const decision: HeaderFaultDecision = {
    ...unsealed,
    decisionDigest: journalJsonDigest(normalizeJournalJson(unsealed)),
  };
  const decisions = await unsafeOpenWatcherFaultDecisionJournalForTest({
    directory: journalRoot,
    deploymentFingerprint: deploymentIdentity.manifestId,
    launchScope,
  });
  await decisions.unsafeAppendDecisionEnvelopeForTest(decision);
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: deploymentIdentity.manifestId,
    category: "mintItemNonCanonical" as const,
    target: {
      kind: "state_queue_header" as const,
      headerHash: decision.headerHash,
    },
    decisionDigest: decision.decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  const journalDirectory = join(
    journalRoot,
    "fault-proofs",
    identity.category,
    decision.headerHash,
  );
  const journal = new DirectoryFraudProofWorkflowJournalStore(journalDirectory);
  const familyIdentity = `${txId}:0:0:${"99".repeat(32)}:${"aa".repeat(32)}`;
  const artifact = { category: identity.category, familyIdentity };
  const entries: FraudProofWorkflowJournalEntry[] = [];
  for (const event of [
    { kind: "started" },
    { kind: "prepared", artifact, artifactDigest: journalJsonDigest(artifact) },
    {
      kind: "preflight_passed",
      actionId: "mintItemNonCanonical:submitInit",
      txHash: txId,
      localEvaluator: "lucid",
      referenceScripts: [],
    },
    {
      kind: "submission_intent",
      actionId: "mintItemNonCanonical:submitInit",
      txHash: txId,
      attempt: 1,
      actionInput: { familyIdentity },
      durableRecovery: {
        familyIdentity,
        sourceStage: "uninitialized",
        targetStage: "initialized",
      },
    },
  ] satisfies FraudProofWorkflowJournalEntry["event"][]) {
    const entry = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId,
      identity,
      sequence: entries.length,
      recordedAt: "2026-09-16T12:00:00.000Z",
      event,
    };
    await journal.append(entry, entries.length);
    entries.push(entry);
  }
  const controller = createWorkflowReconciliationPermitController({
    decision,
    deploymentFingerprint: deploymentIdentity.manifestId,
    rollbackGeneration: "0",
    entries,
  });
  const record = {
    reservationId: "bb".repeat(32),
    deploymentFingerprint: deploymentIdentity.manifestId,
    decisionDigest: decision.decisionDigest,
    policyDigest: "cc".repeat(32),
    reservationBasisDigest: "dd".repeat(32),
    revision: "0",
    state: "active",
    activeInputs: [],
    pendingTransition: null,
    lastConfirmedTransitionDigest: null,
    conflictCode: null,
  };
  const readAll = vi.fn(async () => [
    {
      ...record,
      recordDigest: journalJsonDigest(normalizeJournalJson(record)),
    },
  ]);
  const forbidden = vi.fn(async (): Promise<never> => {
    throw new Error("recovery must not mutate funding");
  });
  const store: WatcherProverFundingReservationStore = {
    readAll,
    readAbandonmentHandoff: async () => null,
    readPendingHandoff: async () => null,
    readPendingTransition: async () => null,
    readCompletionHandoff: async () => null,
    readConfirmedInput: async () => null,
    reserve: forbidden,
    prepareTransition: forbidden,
    confirmTransition: forbidden,
    abandonPendingTransition: forbidden,
    acknowledgeAbandonment: forbidden,
    markConflict: forbidden,
    release: forbidden,
  };
  return {
    entries,
    record,
    forbidden,
    journal,
    workflowId,
    authorize: () =>
      authorizeWatcherProverFundingRecovery({
        journalRoot,
        deploymentIdentity,
        actuationPermit: controller.permit,
        category: identity.category,
        rollbackGeneration: "0",
        store,
      }),
    replace: async (entry: FraudProofWorkflowJournalEntry) =>
      writeFile(
        join(
          journalDirectory,
          workflowId,
          `${entry.sequence.toString().padStart(8, "0")}.json`,
        ),
        JSON.stringify(entry),
      ),
  };
};

it("recovers mint's exact retained artifact under verified deployment finality without rewriting journals or funding", async () => {
  const test = await setup();
  await expect(test.authorize()).resolves.toBeUndefined();
  expect(await test.journal.load(test.workflowId)).toEqual(test.entries);
  expect(test.forbidden).not.toHaveBeenCalled();
});

it.each(["familyIdentity", "category", "releaseFinality"])(
  "rejects a self-consistent mint artifact with changed %s",
  async (field) => {
    const test = await setup();
    const prepared = test.entries[1]!;
    if (prepared.event.kind !== "prepared")
      throw new Error("missing prepared fixture");
    const artifact = { ...prepared.event.artifact, [field]: "ff".repeat(32) };
    await test.replace({
      ...prepared,
      event: {
        ...prepared.event,
        artifact,
        artifactDigest: journalJsonDigest(artifact),
      },
    });
    await expect(test.authorize()).rejects.toThrow();
    expect(test.forbidden).not.toHaveBeenCalled();
  },
);

it.each(["deploymentFingerprint", "decisionDigest"] as const)(
  "rejects a reservation bound to another %s",
  async (field) => {
    const test = await setup();
    test.record[field] = "ff".repeat(32);
    await expect(test.authorize()).rejects.toThrow(
      "original funding reservation",
    );
    expect(test.forbidden).not.toHaveBeenCalled();
  },
);

it("rejects signed attempt evidence inconsistent with the exact retained mint identity", async () => {
  const test = await setup();
  const intent = test.entries[3]!;
  if (intent.event.kind !== "submission_intent")
    throw new Error("missing intent fixture");
  await test.replace({
    ...intent,
    event: { ...intent.event, actionInput: { familyIdentity: "changed" } },
  });
  await expect(test.authorize()).rejects.toThrow();
  expect(test.forbidden).not.toHaveBeenCalled();
});
