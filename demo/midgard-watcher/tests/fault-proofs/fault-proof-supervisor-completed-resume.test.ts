import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";

import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  computeFraudProofWorkflowId,
  createHeaderClassifier,
  createWorkflowActuationPermitController,
  DirectoryFraudProofWorkflowJournalStore,
  DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEvent,
  journalJsonDigest,
  normalizeJournalJson,
} from "@al-ft/midgard-fault-proofs";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { afterEach, describe, expect, it, vi } from "vitest";

import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.js";
import { watcherDeploymentReleaseFinalityAuthority } from "../../src/runtime/deployment-identity.js";
import { fundingTerminal } from "../funding/funding-handoff-fixture.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";

// The production supervisor admits exactly the installed launch scope. The
// double-spend classifier fixture launches one family, so the installed
// scope is narrowed to that family for this file only.
vi.mock("../../src/fault-proofs/fault-proof-application.js", async (load) => {
  const actual =
    await load<
      typeof import("../../src/fault-proofs/fault-proof-application.js")
    >();
  return {
    ...actual,
    WATCHER_INSTALLED_WORKFLOW_CATEGORIES: Object.freeze(["doubleSpend"]),
  };
});

const directories: string[] = [];
afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map(async (path) => rm(path, { recursive: true, force: true })),
  );
});

const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
const PROOF_TX = "aa".repeat(32);
const REMOVAL_TX = "bb".repeat(32);

const classifyDoubleSpend = async () => {
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({ spendInputs: [outRefCbor(91, 0n)], fee: 1n }),
      buildFixtureTransaction({ spendInputs: [outRefCbor(91, 0n)], fee: 2n }),
    ],
  });
  const observation = {
    ...authenticatedHeaderObservation(fixture),
    confirmationDepth: 30,
  };
  const classifier = await createHeaderClassifier({
    deploymentFingerprint: deploymentIdentity.manifestId,
    replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
    releaseFinalityAuthority:
      watcherDeploymentReleaseFinalityAuthority(deploymentIdentity),
  });
  const decision = await classifyHeader({
    classifier,
    observation,
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: 30,
      }),
    sources: [
      {
        sourceId: "libp2p-test",
        fetchPayloadByHeaderHash: async () => ({
          ok: true as const,
          provenance: {
            trustClass: "public_or_permissionless_da" as const,
            sourceId: "libp2p-test/peer-a",
            grade: "security" as const,
          },
          sourceId: "libp2p-test",
          sourcePeerId: "peer-a",
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          attempts: [],
        }),
      },
    ],
  });
  if (decision.decision !== "fault_detected")
    throw new Error("fixture must classify a double spend");
  return decision;
};

const submissionEvents = (
  actionId: string,
  txHash: string,
): FraudProofWorkflowJournalEvent[] => [
  {
    kind: "preflight_passed",
    actionId,
    txHash,
    localEvaluator: "lucid-local-uplc",
    referenceScripts: [],
  },
  {
    kind: "submission_intent",
    actionId,
    actionInput: { stage: actionId },
    attempt: 1,
    txHash,
  },
  { kind: "submitted", actionId, attempt: 1, txHash },
  { kind: "reconciled", actionId, outcome: "confirmed", txHash },
  { kind: "confirmed", actionId, txHash },
];

const writeExecution = async (
  root: string,
  decision: Awaited<ReturnType<typeof classifyDoubleSpend>>,
  completed: boolean,
  preparedOnly = false,
): Promise<void> => {
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: deploymentIdentity.manifestId,
    category: "doubleSpend",
    target: { kind: "state_queue_header", headerHash: decision.headerHash },
    decisionDigest: decision.decisionDigest,
  } as const;
  const artifact = { familyArtifact: { test: true } };
  const terminal = fundingTerminal(decision.headerHash, PROOF_TX, REMOVAL_TX);
  const events: FraudProofWorkflowJournalEvent[] = [
    { kind: "started" },
    { kind: "prepared", artifact, artifactDigest: journalJsonDigest(artifact) },
    ...(preparedOnly ? [] : submissionEvents("proof", PROOF_TX)),
    ...(preparedOnly ? [] : submissionEvents("remove", REMOVAL_TX)),
    ...(completed
      ? [
          {
            kind: "completed" as const,
            terminal,
            terminalDigest: journalJsonDigest(normalizeJournalJson(terminal)),
          },
        ]
      : []),
  ];
  const journal = new DirectoryFraudProofWorkflowJournalStore(
    join(root, "fault-proofs", "doubleSpend", decision.headerHash),
  );
  const workflowId = computeFraudProofWorkflowId(identity);
  for (const [sequence, event] of events.entries()) {
    await journal.append(
      {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        identity,
        workflowId,
        recordedAt: "2026-09-11T00:00:00.000Z",
        sequence,
        event,
      },
      sequence,
    );
  }
};

const recover = async (completed: boolean, rounds = 1) => {
  const root = await mkdtemp("/var/tmp/midgard-fault-supervisor-completed-");
  directories.push(root);
  const decision = await classifyDoubleSpend();
  await writeExecution(root, decision, completed);
  let ran = 0;
  const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
    journalRoot: root,
    deploymentFingerprint: deploymentIdentity.manifestId,
    run: async (job) => {
      ran += 1;
      return completed ? job.mode : { kind: "terminal_included" };
    },
  });
  // A live re-classification of the same header carries a fresh decision
  // digest: the authenticated observation digest differs at every point.
  const controller = createWorkflowActuationPermitController({
    decision,
    rollbackGeneration: "3",
  });
  let recovered = 0;
  for (let round = 0; round < rounds; round += 1) {
    recovered =
      round === 0
        ? await supervisor.recoverExisting(
            decision,
            controller.permit,
            Object.freeze({
              headerHash: decision.headerHash,
              headerEndTimeMs: "0",
              maturityAtMs: "604800000",
              latestSafeStartAtMs: "302400000",
            }),
            "3",
          )
        : (await supervisor.unsafeScheduleForTest({
            mode: "resume",
            category: "doubleSpend",
            headerHash: decision.headerHash,
            decisionDigest: decision.decisionDigest,
            rollbackGeneration: "3",
            observationRevision: String(round),
          }),
          1);
    await vi.waitFor(() => {
      expect(supervisor.status().activeJob).toBeNull();
      expect(supervisor.status().queuedJobCount).toBe(0);
    });
  }
  await supervisor.close();
  return { recovered, ran, status: supervisor.status() };
};

describe("fault-proof supervisor over a completed execution", () => {
  it("does not start a new execution for a header whose journal already completed", async () => {
    const outcome = await recover(true);
    expect(outcome.recovered).toBe(1);
    expect(outcome.ran).toBe(0);
    expect(outcome.status).toMatchObject({
      phase: "closed",
      queuedJobCount: 0,
    });
  }, 60_000);

  it("reconciles a provisionally finished execution again on later observations", async () => {
    const outcome = await recover(false, 2);
    expect(outcome.ran).toBe(2);
    expect(outcome.status.queuedJobCount).toBe(0);
  }, 60_000);

  it("reopens a yielded prepared objective only when a later observation schedules it", async () => {
    const root = await mkdtemp("/var/tmp/midgard-fault-supervisor-yielded-");
    directories.push(root);
    const decision = await classifyDoubleSpend();
    await writeExecution(root, decision, false, true);
    const run = vi.fn(async (_job: { mode: "run" | "resume" }) => ({
      kind: "pending",
      resumeOnObservation: true,
    }));
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      deploymentFingerprint: deploymentIdentity.manifestId,
      run,
    });
    const controller = createWorkflowActuationPermitController({
      decision,
      rollbackGeneration: "3",
    });
    const observe = async () =>
      await supervisor.recoverExisting(
        decision,
        controller.permit,
        {
          headerHash: decision.headerHash,
          headerEndTimeMs: "0",
          maturityAtMs: "604800000",
          latestSafeStartAtMs: "302400000",
        },
        "3",
      );
    try {
      await observe();
      await vi.waitFor(() => {
        expect(run).toHaveBeenCalledTimes(1);
        expect(supervisor.status().activeJob).toBeNull();
        expect(supervisor.status().queuedJobCount).toBe(0);
      });
      // Draining runnable work does not schedule another invocation by itself.
      await new Promise<void>((resolve) => setImmediate(resolve));
      expect(run).toHaveBeenCalledTimes(1);
      await observe();
      // Repeating the same canonical observation cannot create a busy loop.
      expect(run).toHaveBeenCalledTimes(1);
      await supervisor.unsafeScheduleForTest({
        mode: "resume",
        category: "doubleSpend",
        headerHash: decision.headerHash,
        decisionDigest: decision.decisionDigest,
        rollbackGeneration: "3",
        observationRevision: "next-canonical-observation",
      });
      await vi.waitFor(() => {
        expect(run).toHaveBeenCalledTimes(2);
        expect(supervisor.status().activeJob).toBeNull();
        expect(supervisor.status().queuedJobCount).toBe(0);
      });
      expect(supervisor.status().phase).toBe("accepting");
      expect(run.mock.calls.map(([job]) => job.mode)).toEqual([
        "resume",
        "resume",
      ]);
    } finally {
      await supervisor.close();
    }
  }, 60_000);

  it("still resumes an execution that has not completed", async () => {
    const outcome = await recover(false);
    expect(outcome.recovered).toBe(1);
    expect(outcome.ran).toBe(1);
  }, 60_000);
});
