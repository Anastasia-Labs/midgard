import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
} from "@al-ft/midgard-fault-proofs";
import { expect, vi } from "vitest";

import { verifyJourneyCorrection } from "./correction.js";

export const identity = {
  schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  deploymentFingerprint: "12".repeat(32),
  category: "validationTraceDispute",
  target: { kind: "state_queue_header", headerHash: "34".repeat(28) },
} as const;

export const entry = (
  sequence: number,
  event: FraudProofWorkflowJournalEntry["event"],
  recordedAt = "2026-11-10T05:00:00.000Z",
): FraudProofWorkflowJournalEntry => ({
  schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  workflowId: computeFraudProofWorkflowId(identity),
  identity,
  sequence,
  recordedAt,
  event,
});

type CorrectionInput = Parameters<typeof verifyJourneyCorrection>[0];

/**
 * Drive the journey's correction gate through `observations`, each the
 * watcher's journal as read at a wall-clock instant, starting (or resuming
 * on `retained`) at `startAt`; the run ends after the last observation. The
 * correction's own wait must outlast every observation: only the gate may
 * end a correction. The error names how many observations the gate read
 * before it failed.
 */
export const pollCorrectionProgress = async ({
  retained,
  observations,
  startAt,
  gate,
}: {
  retained: readonly FraudProofWorkflowJournalEntry[];
  observations: readonly {
    at: number;
    records: readonly FraudProofWorkflowJournalEntry[];
  }[];
  startAt: number;
  gate: Pick<
    CorrectionInput,
    "counterpartyWait" | "reconciliationAllowanceMs" | "resumeAllowanceMs"
  >;
}) => {
  const directory = await mkdtemp(join(tmpdir(), "journey-progress-clock-"));
  const now = vi.spyOn(Date, "now").mockReturnValue(startAt);
  let served: readonly FraudProofWorkflowJournalEntry[] = [];
  let read = 0;
  try {
    await verifyJourneyCorrection({
      context: { deployment: {}, provider: {} } as CorrectionInput["context"],
      transactions: {} as CorrectionInput["transactions"],
      watcherStore: {
        readWorkflowRows: async (_category, headerHash) =>
          new Map([[headerHash!, served]]),
      },
      proverAddress: "addr_test1unused",
      directory,
      category: identity.category,
      headerHash: identity.target.headerHash,
      predecessorHeaderHash: "78".repeat(28),
      workflowBaseline: retained,
      actionDepth: 1,
      operatorVkey: "90".repeat(28),
      requireLive: () => undefined,
      stage: async (_name, action) => await action(),
      ...gate,
      poll: async (_name, action, timeoutMs) => {
        expect(timeoutMs).toBeGreaterThan(observations.at(-1)!.at - startAt);
        for (const observation of observations) {
          now.mockReturnValue(observation.at);
          served = observation.records;
          read += 1;
          expect(await action()).toBeUndefined();
        }
        throw new Error("observations ended");
      },
    });
  } catch (error) {
    throw new Error(
      `at observation ${read.toString()} of ${observations.length.toString()}: ${(error as Error).message}`,
    );
  } finally {
    now.mockRestore();
    await rm(directory, { recursive: true, force: true });
  }
};
