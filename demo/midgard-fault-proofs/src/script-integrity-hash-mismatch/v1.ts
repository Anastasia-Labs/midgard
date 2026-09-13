import { mkdir } from "node:fs/promises";

import { createScriptIntegrityHashMismatchDirectoryJournal } from "./directory-journal.js";
import {
  type ScriptIntegrityHashMismatchEvidence,
  scriptIntegrityHashMismatchEvidenceIdentity,
} from "./family.js";
import {
  cancelScriptIntegrityHashMismatchWorkflow,
  runScriptIntegrityHashMismatchWorkflow,
  type ScriptIntegrityHashMismatchCursor,
  type ScriptIntegrityHashMismatchTransactionPort,
  type ScriptIntegrityHashMismatchWorkflowAction,
  type ScriptIntegrityHashMismatchWorkflowStage,
} from "./workflow.js";

export const SCRIPT_INTEGRITY_HASH_MISMATCH_RUNNER =
  "script-integrity-hash-mismatch-production-v1" as const;

export type ScriptIntegrityHashMismatchCapturedTransaction = Readonly<{
  txHash: string;
  target: ScriptIntegrityHashMismatchCursor;
  signedBytes: Uint8Array;
}>;
export interface ScriptIntegrityHashMismatchActuator {
  observe(identity: string): Promise<ScriptIntegrityHashMismatchCursor>;
  captureSignedTransaction(input: {
    action: ScriptIntegrityHashMismatchWorkflowAction;
    evidence: ScriptIntegrityHashMismatchEvidence;
    source: ScriptIntegrityHashMismatchCursor;
  }): Promise<ScriptIntegrityHashMismatchCapturedTransaction>;
  submitSignedTransaction(
    transaction: ScriptIntegrityHashMismatchCapturedTransaction,
  ): Promise<string>;
  transactionConfirmed(txHash: string): Promise<boolean>;
  acquireRemovalLease(identity: string): Promise<
    Readonly<{
      leaseId: string;
      commit(): Promise<void>;
      release(): Promise<void>;
    }>
  >;
}

const port = (
  actuator: ScriptIntegrityHashMismatchActuator,
): ScriptIntegrityHashMismatchTransactionPort =>
  Object.freeze({
    observe: actuator.observe.bind(actuator),
    transactionConfirmed: actuator.transactionConfirmed.bind(actuator),
    capture: async (
      input: Parameters<
        ScriptIntegrityHashMismatchTransactionPort["capture"]
      >[0],
    ) => {
      const captured = await actuator.captureSignedTransaction(input);
      if (captured.signedBytes.length === 0)
        throw new Error(
          "scriptIntegrityHashMismatch captured empty signed transaction",
        );
      return Object.freeze({
        txHash: captured.txHash,
        target: captured.target,
        submit: async () => await actuator.submitSignedTransaction(captured),
      });
    },
  });

/** Callback-free production driver with durable restart and leased removal. */
export const runScriptIntegrityHashMismatch = async ({
  evidence,
  journalDirectory,
  actuator,
}: {
  evidence: ScriptIntegrityHashMismatchEvidence;
  journalDirectory: string;
  actuator: ScriptIntegrityHashMismatchActuator;
}): Promise<"removed" | "cancelled"> => {
  await mkdir(journalDirectory, { recursive: true, mode: 0o700 });
  const journal =
    createScriptIntegrityHashMismatchDirectoryJournal(journalDirectory);
  const transactions = port(actuator);
  const identity = scriptIntegrityHashMismatchEvidenceIdentity(evidence);
  for (;;) {
    const observed = await actuator.observe(identity);
    let lease:
      | Awaited<
          ReturnType<ScriptIntegrityHashMismatchActuator["acquireRemovalLease"]>
        >
      | undefined;
    if (observed.stage === "proven")
      lease = await actuator.acquireRemovalLease(identity);
    try {
      const stage = await runScriptIntegrityHashMismatchWorkflow({
        evidence,
        journal,
        transactions,
      });
      if (stage === "removed") {
        await lease?.commit();
        return "removed";
      }
      if (stage === "cancelled") {
        await lease?.release();
        return "cancelled";
      }
      await lease?.release();
    } catch (error) {
      await lease?.release();
      throw error;
    }
  }
};

export const cancelScriptIntegrityHashMismatch = async ({
  evidence,
  journalDirectory,
  actuator,
}: {
  evidence: ScriptIntegrityHashMismatchEvidence;
  journalDirectory: string;
  actuator: ScriptIntegrityHashMismatchActuator;
}): Promise<"cancelled"> => {
  await mkdir(journalDirectory, { recursive: true, mode: 0o700 });
  return await cancelScriptIntegrityHashMismatchWorkflow({
    evidence,
    journal:
      createScriptIntegrityHashMismatchDirectoryJournal(journalDirectory),
    transactions: port(actuator),
  });
};

export const SCRIPT_INTEGRITY_HASH_MISMATCH_TERMINAL_STAGES = Object.freeze([
  "removed",
  "cancelled",
] satisfies readonly ScriptIntegrityHashMismatchWorkflowStage[]);

export {
  createManifestBoundScriptIntegrityHashMismatchWorkflow,
  createScriptIntegrityHashMismatchWorkflowRunnerSurface,
  type LoadedScriptIntegrityHashMismatchWorkflow,
  type LoadScriptIntegrityHashMismatchWorkflow,
  type ManifestBoundScriptIntegrityHashMismatchWorkflow,
  type ManifestBoundScriptIntegrityHashMismatchWorkflowConfig,
  SCRIPT_INTEGRITY_HASH_MISMATCH_CONFIG_KEYS,
} from "./manifest-workflow.js";
