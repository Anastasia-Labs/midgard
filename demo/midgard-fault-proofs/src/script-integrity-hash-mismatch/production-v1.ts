import { mkdir } from "node:fs/promises";

import { createScriptIntegrityHashMismatchDirectoryJournalV1 } from "./directory-journal-v1.js";
import {
  scriptIntegrityHashMismatchEvidenceIdentityV1,
  type ScriptIntegrityHashMismatchEvidenceV1,
} from "./family-v1.js";
import {
  cancelScriptIntegrityHashMismatchWorkflowV1,
  runScriptIntegrityHashMismatchWorkflowV1,
  type ScriptIntegrityHashMismatchCursorV1,
  type ScriptIntegrityHashMismatchTransactionPortV1,
  type ScriptIntegrityHashMismatchWorkflowActionV1,
  type ScriptIntegrityHashMismatchWorkflowStageV1,
} from "./workflow-v1.js";

export const SCRIPT_INTEGRITY_HASH_MISMATCH_PRODUCTION_RUNNER_V1 =
  "script-integrity-hash-mismatch-production-v1" as const;

export type ScriptIntegrityHashMismatchCapturedTransactionV1 = Readonly<{
  txHash: string;
  target: ScriptIntegrityHashMismatchCursorV1;
  signedBytes: Uint8Array;
}>;
export interface ScriptIntegrityHashMismatchProductionActuatorV1 {
  observe(identity: string): Promise<ScriptIntegrityHashMismatchCursorV1>;
  captureSignedTransaction(input: {
    action: ScriptIntegrityHashMismatchWorkflowActionV1;
    evidence: ScriptIntegrityHashMismatchEvidenceV1;
    source: ScriptIntegrityHashMismatchCursorV1;
  }): Promise<ScriptIntegrityHashMismatchCapturedTransactionV1>;
  submitSignedTransaction(
    transaction: ScriptIntegrityHashMismatchCapturedTransactionV1,
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

const productionPort = (
  actuator: ScriptIntegrityHashMismatchProductionActuatorV1,
): ScriptIntegrityHashMismatchTransactionPortV1 =>
  Object.freeze({
    observe: actuator.observe.bind(actuator),
    transactionConfirmed: actuator.transactionConfirmed.bind(actuator),
    capture: async (
      input: Parameters<
        ScriptIntegrityHashMismatchTransactionPortV1["capture"]
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
export const runScriptIntegrityHashMismatchProductionV1 = async ({
  evidence,
  journalDirectory,
  actuator,
}: {
  evidence: ScriptIntegrityHashMismatchEvidenceV1;
  journalDirectory: string;
  actuator: ScriptIntegrityHashMismatchProductionActuatorV1;
}): Promise<"removed" | "cancelled"> => {
  await mkdir(journalDirectory, { recursive: true, mode: 0o700 });
  const journal =
    createScriptIntegrityHashMismatchDirectoryJournalV1(journalDirectory);
  const transactions = productionPort(actuator);
  const identity = scriptIntegrityHashMismatchEvidenceIdentityV1(evidence);
  for (;;) {
    const observed = await actuator.observe(identity);
    let lease:
      | Awaited<
          ReturnType<
            ScriptIntegrityHashMismatchProductionActuatorV1["acquireRemovalLease"]
          >
        >
      | undefined;
    if (observed.stage === "proven")
      lease = await actuator.acquireRemovalLease(identity);
    try {
      const stage = await runScriptIntegrityHashMismatchWorkflowV1({
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

export const cancelScriptIntegrityHashMismatchProductionV1 = async ({
  evidence,
  journalDirectory,
  actuator,
}: {
  evidence: ScriptIntegrityHashMismatchEvidenceV1;
  journalDirectory: string;
  actuator: ScriptIntegrityHashMismatchProductionActuatorV1;
}): Promise<"cancelled"> => {
  await mkdir(journalDirectory, { recursive: true, mode: 0o700 });
  return await cancelScriptIntegrityHashMismatchWorkflowV1({
    evidence,
    journal:
      createScriptIntegrityHashMismatchDirectoryJournalV1(journalDirectory),
    transactions: productionPort(actuator),
  });
};

export const SCRIPT_INTEGRITY_HASH_MISMATCH_TERMINAL_STAGES_V1 = Object.freeze([
  "removed",
  "cancelled",
] satisfies readonly ScriptIntegrityHashMismatchWorkflowStageV1[]);

export {
  createManifestBoundScriptIntegrityHashMismatchWorkflowV1,
  createScriptIntegrityHashMismatchProductionWorkflowRunnerSurfaceV1,
  type LoadedScriptIntegrityHashMismatchProductionWorkflowV1,
  type LoadScriptIntegrityHashMismatchProductionWorkflowV1,
  type ManifestBoundScriptIntegrityHashMismatchWorkflowConfigV1,
  type ManifestBoundScriptIntegrityHashMismatchWorkflowV1,
  SCRIPT_INTEGRITY_HASH_MISMATCH_PRODUCTION_CONFIG_KEYS_V1,
} from "./manifest-workflow-v1.js";
