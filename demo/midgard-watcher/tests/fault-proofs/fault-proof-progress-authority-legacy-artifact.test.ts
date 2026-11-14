import { createHash } from "node:crypto";

import {
  COMPLETE_CANONICAL_REPLAY,
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
  HEADER_CLASSIFIER,
  HEADER_DECISION,
  type HeaderFaultDecision,
  journalJsonDigest,
  type JournalJsonObject,
  normalizeJournalJson,
} from "@al-ft/midgard-fault-proofs";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { afterEach, expect, it } from "vitest";

import { unsafeOpenWatcherFaultDecisionJournalForTest } from "../../src/fault-proofs/fault-decision-journal.js";
import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.js";
import { createWatcherFaultProofProgressAuthority } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import {
  emptyWorkflowJournal,
  journalDirectory,
  recordObjectives,
  removeJournalDirectories,
  TEST_JOURNAL_KEY,
} from "../support/watcher-journal-fixture.js";

// A mint-item-non-canonical proof an earlier release started keeps that
// release's family-specific prepared artifact. This release does not import
// it: the objective is held by name, never run, and its rows stay as they
// are, while every other objective, of mint or any family, goes on.
afterEach(removeJournalDirectories);

const deploymentFingerprint =
  makeWatcherDeploymentAuthorityFixture().result.manifestId;
const launchScope = ["mintItemNonCanonical"] as const;
const category = "mintItemNonCanonical" as const;

const decisionOf = (headerHash: string, seed: number): HeaderFaultDecision => {
  const txId = seed.toString(16).padStart(2, "0").repeat(32);
  const unsealed = {
    schemaVersion: HEADER_DECISION,
    classifierVersion: HEADER_CLASSIFIER,
    deploymentFingerprint,
    headerHash,
    authenticatedObservationDigest: "44".repeat(32),
    payloadEnvelopeSha256: "55".repeat(32),
    payloadSha256: "66".repeat(32),
    replayVersion: COMPLETE_CANONICAL_REPLAY,
    replayDigest: "77".repeat(32),
    launchScope,
    launchScopeDigest: journalJsonDigest(normalizeJournalJson(launchScope)),
    classificationDigest: "88".repeat(32),
    decision: "fault_detected" as const,
    category,
    violationId: "mint-item-non-canonical",
    detectionId: `mint-item-non-canonical:0:${txId}:0`,
    position: "0",
  };
  return {
    ...unsealed,
    decisionDigest: journalJsonDigest(normalizeJournalJson(unsealed)),
  };
};

/** The workflow id the earlier release derived for its mint journals. */
const legacyWorkflowId = (decision: HeaderFaultDecision): string =>
  createHash("sha256")
    .update(
      [
        FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
        deploymentFingerprint,
        category,
        `header:${decision.headerHash}`,
        `decision:${decision.decisionDigest}`,
      ].join("\u0000"),
    )
    .digest("hex");

const journalOf = (
  decision: HeaderFaultDecision,
  artifact: JournalJsonObject,
): FraudProofWorkflowJournalEntry[] => {
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint,
    category,
    target: {
      kind: "state_queue_header" as const,
      headerHash: decision.headerHash,
    },
    decisionDigest: decision.decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  return [
    { kind: "started" as const },
    {
      kind: "prepared" as const,
      artifact,
      artifactDigest: journalJsonDigest(artifact),
    },
  ].map((event, sequence) => ({
    schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
    workflowId,
    identity,
    sequence,
    recordedAt: "2026-09-16T12:00:00.000Z",
    event,
  }));
};

const headerOf = (seed: number) =>
  buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({ spendInputs: [outRefCbor(seed, 0n)], fee: 1n }),
    ],
  });

it("holds a mint objective an earlier release prepared by its workflow id, never runs it, and leaves its rows and other objectives as they are", async () => {
  const journalRoot = await journalDirectory("mint-legacy-artifact");
  const legacyHeader = await headerOf(61);
  const currentHeader = await headerOf(62);
  const legacy = decisionOf(legacyHeader.headerHash, 0x22);
  const current = decisionOf(currentHeader.headerHash, 0x23);
  const decisions = await unsafeOpenWatcherFaultDecisionJournalForTest({
    directory: journalRoot,
    deploymentFingerprint,
    launchScope,
    authenticationKey: TEST_JOURNAL_KEY,
  });
  for (const decision of [legacy, current])
    await decisions.unsafeAppendDecisionEnvelopeForTest(decision);
  // The earlier release's journals already sit under the shared workflow id.
  expect(legacyWorkflowId(legacy)).toBe(
    computeFraudProofWorkflowId(journalOf(legacy, {})[0]!.identity),
  );
  const familyIdentity = `${"22".repeat(32)}:0:0:${"99".repeat(32)}:${"aa".repeat(32)}`;
  const legacyEntries = journalOf(legacy, { category, familyIdentity });
  const currentEntries = journalOf(current, {
    evidenceBinding: { headerHash: current.headerHash },
    releaseFinality: { policyDigest: "bb".repeat(32) },
    familyArtifact: { familyIdentity },
  });
  const workflowJournal = emptyWorkflowJournal();
  for (const [{ headerHash }, entries] of [
    [legacy, legacyEntries],
    [current, currentEntries],
  ] as const) {
    const journal = workflowJournal.forObjective({ category, headerHash });
    for (const entry of entries) await journal.append(entry, entry.sequence);
  }
  const objectives = [legacy, current].map(({ headerHash }) => ({
    category,
    headerHash,
  }));
  recordObjectives(journalRoot, objectives);
  const authority = createWatcherFaultProofProgressAuthority({
    journalRoot,
    workflowJournal,
    deploymentFingerprint,
    categories: installedLaunchScope(launchScope),
    authenticationKey: TEST_JOURNAL_KEY,
  });

  const admitted = await authority.admit({
    observation: progressObservation({
      deploymentFingerprint,
      headers: [legacyHeader, currentHeader],
    }),
    rollbackGeneration: "0",
  });

  expect(admitted).toEqual([]);
  expect(authority.decisionHolds()).toEqual([
    {
      kind: "objective",
      ...objectives[0],
      decisionDigest: null,
      detail: `${category}/${legacy.headerHash}: workflow ${legacyEntries[0]!.workflowId} prepared artifact is not in the generic artifact envelope`,
      readiness: "fault_proof_objective_unreadable",
    },
  ]);
  // The other objective is adopted, not held.
  expect(authority.unfinishedCount()).toBe(2);
  expect(workflowJournal.readExecution(objectives[0]!)).toEqual({
    workflowId: legacyEntries[0]!.workflowId,
    entries: legacyEntries,
  });
  expect(workflowJournal.readExecution(objectives[1]!)).toEqual({
    workflowId: currentEntries[0]!.workflowId,
    entries: currentEntries,
  });
});
