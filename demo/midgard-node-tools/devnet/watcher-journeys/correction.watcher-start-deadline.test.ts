import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
} from "@al-ft/midgard-fault-proofs";
import { describe, expect, it, vi } from "vitest";

import { verifyJourneyCorrection } from "./correction.js";
import type { JourneyWatcherStartStatus } from "./correction.watcher-start-deadline.js";
import { workflowLoad } from "./correction-recovery.test-support.js";

const headerHash = "34".repeat(28);
const identity = {
  schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  deploymentFingerprint: "12".repeat(32),
  category: "transitionTrace",
  target: { kind: "state_queue_header", headerHash },
} as const;
const entry = (
  sequence: number,
  event: FraudProofWorkflowJournalEntry["event"],
): FraudProofWorkflowJournalEntry => ({
  schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  workflowId: computeFraudProofWorkflowId(identity),
  identity,
  sequence,
  recordedAt: "2026-11-10T05:00:00.000Z",
  event,
});
const started = [entry(0, { kind: "started" })];
const signed = [
  ...started,
  entry(1, {
    kind: "submission_intent",
    actionId: "init",
    actionInput: {},
    attempt: 1,
    txHash: "ab".repeat(32),
  }),
];

/** A stack watcher's queued job for the journey's header. */
const job = {
  mode: "run",
  category: "transitionTrace",
  headerHash,
  decisionDigest: "56".repeat(32),
  rollbackGeneration: "0",
  deadline: {
    headerHash,
    headerBlockTimeMs: "1794300000000",
    maturityAtMs: "1794300900000",
    latestSafeStartAtMs: "1794300450000",
  },
};
/** A stack watcher's `/v1/status` with these supervisor fields. */
const status = (
  supervisor: Partial<NonNullable<JourneyWatcherStartStatus["supervisor"]>>,
): JourneyWatcherStartStatus => ({
  liveness: "live",
  readiness: "not_ready",
  readinessReasons: ["deadline_unsafe"],
  supervisor: {
    phase: "accepting",
    recovered: true,
    queuedJobCount: 1,
    activeJob: null,
    earliestDeadlineJob: job,
    deadlineHealth: "unsafe",
    remainingSafeStartMs: "-1",
    journalDecisionMissing: [],
    ...supervisor,
  },
});
/** The watcher's own hold of the journey's objective past its safe start. */
const held = status({
  queuedJobCount: 0,
  earliestDeadlineJob: null,
  deadlineHealth: "safe",
  remainingSafeStartMs: null,
  journalDecisionMissing: [
    {
      kind: "objective",
      category: "transitionTrace",
      headerHash,
      decisionDigest: "56".repeat(32),
      detail: `transitionTrace/${headerHash}`,
      readiness: "fault_proof_start_deadline_passed",
    },
  ],
});

const hardTimeout = new Error("whole correction timeout");

/** The proof wait over these observations, each one poll; counts status reads. */
const proofWait = async (
  observations: readonly {
    records: readonly FraudProofWorkflowJournalEntry[];
    watcher: JourneyWatcherStartStatus;
  }[],
) => {
  const directory = await mkdtemp(join(tmpdir(), "journey-start-deadline-"));
  const load = workflowLoad();
  const now = vi.spyOn(Date, "now").mockReturnValue(0);
  let current = observations[0]!.watcher;
  let statusReads = 0;
  type Input = Parameters<typeof verifyJourneyCorrection>[0];
  try {
    await verifyJourneyCorrection({
      context: { deployment: {}, provider: {} } as Input["context"],
      transactions: {} as Input["transactions"],
      watcherStore: load.store,
      watcherStatus: async () => {
        statusReads += 1;
        return current;
      },
      proverAddress: "addr_test1unused",
      directory,
      category: "transitionTrace",
      headerHash,
      predecessorHeaderHash: "78".repeat(28),
      workflowBaseline: started,
      actionDepth: 1,
      correctionTimeoutMs: 2_850_000,
      progressAllowanceMs: 190_000,
      operatorVkey: "90".repeat(28),
      requireLive: () => undefined,
      stage: async (_name, action) => await action(),
      poll: async (_name, action) => {
        for (const observation of observations) {
          current = observation.watcher;
          load.serve(observation.records);
          expect(await action()).toBeUndefined();
        }
        throw hardTimeout;
      },
    });
  } finally {
    now.mockRestore();
    await rm(directory, { recursive: true, force: true });
  }
  return statusReads;
};

describe("the proof wait and the watcher's latest safe start", () => {
  it("fails at once, naming it, when the watcher holds the objective past its safe start", async () => {
    await expect(
      proofWait([{ records: started, watcher: held }]),
    ).rejects.toThrow(
      `fault_proof_start_deadline_passed for transitionTrace/${headerHash}`,
    );
  });

  it("fails at once when the safe start passed with the job queued and nothing signed", async () => {
    await expect(
      proofWait([{ records: started, watcher: status({}) }]),
    ).rejects.toThrow(/latest safe start.*remainingSafeStartMs -1/su);
  });

  it("keeps waiting on a job that is already running when its safe start passes", async () => {
    await expect(
      proofWait([
        { records: started, watcher: status({ activeJob: job }) },
        { records: started, watcher: status({ activeJob: job }) },
      ]),
    ).rejects.toBe(hardTimeout);
  });

  it("keeps waiting once the workflow has signed a step, which the watcher still reconciles", async () => {
    await expect(
      proofWait([
        { records: signed, watcher: held },
        { records: signed, watcher: status({}) },
      ]),
    ).rejects.toBe(hardTimeout);
  });

  it("keeps waiting while a restarted watcher answers its startup status", async () => {
    const startup = {
      liveness: "live",
      readiness: "not_ready",
      readinessReasons: ["startup_in_progress"],
      startup: { phase: "opening_journal" },
    } as unknown as JourneyWatcherStartStatus;
    await expect(
      proofWait([
        { records: started, watcher: startup },
        { records: started, watcher: startup },
      ]),
    ).rejects.toBe(hardTimeout);
  });

  it("ignores another header's hold and a safe start that has not passed", async () => {
    const other = "9a".repeat(28);
    await expect(
      proofWait([
        {
          records: started,
          watcher: status({
            earliestDeadlineJob: { ...job, headerHash: other },
            journalDecisionMissing: [
              {
                kind: "objective",
                category: "transitionTrace",
                headerHash: other,
                decisionDigest: "56".repeat(32),
                detail: `transitionTrace/${other}`,
                readiness: "fault_proof_start_deadline_passed",
              },
            ],
          }),
        },
        {
          records: started,
          watcher: status({
            deadlineHealth: "safe",
            remainingSafeStartMs: "1",
          }),
        },
      ]),
    ).rejects.toBe(hardTimeout);
  });
});
