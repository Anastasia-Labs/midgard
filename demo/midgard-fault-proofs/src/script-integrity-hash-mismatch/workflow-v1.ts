import { createHash } from "node:crypto";

import type { ScriptIntegrityHashMismatchEvidence } from "./family-v1.js";
import { scriptIntegrityHashMismatchEvidenceIdentity } from "./family-v1.js";

export const SCRIPT_INTEGRITY_HASH_MISMATCH_WORKFLOW_STAGES = [
  "none",
  "step01",
  "step02",
  "step03",
  "step04:0",
  "step04:1",
  "step05",
  "proven",
  "removed",
  "cancelled",
] as const;
export type ScriptIntegrityHashMismatchWorkflowStage =
  (typeof SCRIPT_INTEGRITY_HASH_MISMATCH_WORKFLOW_STAGES)[number];
export type ScriptIntegrityHashMismatchWorkflowAction =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "submitStep04"
  | "submitStep05"
  | "removeDescendants"
  | "cancel";
export type ScriptIntegrityHashMismatchCursor = Readonly<{
  stage: ScriptIntegrityHashMismatchWorkflowStage;
  threadOutRef: string;
}>;
export type ScriptIntegrityHashMismatchJournalEntry = Readonly<{
  sequence: number;
  identity: string;
  action: ScriptIntegrityHashMismatchWorkflowAction;
  phase: "intent" | "submitted" | "confirmed";
  source: ScriptIntegrityHashMismatchCursor;
  target: ScriptIntegrityHashMismatchCursor;
  txHash: string;
}>;
export interface ScriptIntegrityHashMismatchJournal {
  load(
    identity: string,
  ): Promise<readonly ScriptIntegrityHashMismatchJournalEntry[]>;
  append(entry: ScriptIntegrityHashMismatchJournalEntry): Promise<void>;
}
export interface ScriptIntegrityHashMismatchTransactionPort {
  observe(identity: string): Promise<ScriptIntegrityHashMismatchCursor>;
  capture(input: {
    action: ScriptIntegrityHashMismatchWorkflowAction;
    evidence: ScriptIntegrityHashMismatchEvidence;
    source: ScriptIntegrityHashMismatchCursor;
  }): Promise<
    Readonly<{
      txHash: string;
      target: ScriptIntegrityHashMismatchCursor;
      submit: () => Promise<string>;
    }>
  >;
  transactionConfirmed(txHash: string): Promise<boolean>;
}

const actionFor = (
  stage: ScriptIntegrityHashMismatchWorkflowStage,
): ScriptIntegrityHashMismatchWorkflowAction | "done" =>
  ({
    none: "submitInit",
    step01: "submitStep01",
    step02: "submitStep02",
    step03: "submitStep03",
    "step04:0": "submitStep04",
    "step04:1": "submitStep04",
    step05: "submitStep05",
    proven: "removeDescendants",
    removed: "done",
    cancelled: "done",
  })[stage] as ScriptIntegrityHashMismatchWorkflowAction | "done";
const cursorDigest = (cursor: ScriptIntegrityHashMismatchCursor) =>
  createHash("sha256").update(JSON.stringify(cursor)).digest("hex");
const validate = (
  entries: readonly ScriptIntegrityHashMismatchJournalEntry[],
  identity: string,
) =>
  entries.forEach((entry, sequence) => {
    if (
      entry.identity !== identity ||
      entry.sequence !== sequence ||
      !/^[0-9a-f]{64}$/u.test(entry.txHash)
    )
      throw new Error(
        "scriptIntegrityHashMismatch journal identity/sequence/transaction changed",
      );
  });
const unresolved = (
  entries: readonly ScriptIntegrityHashMismatchJournalEntry[],
) =>
  [...entries]
    .reverse()
    .find(
      (entry) =>
        entry.phase === "intent" &&
        !entries.some(
          (later) =>
            later.sequence > entry.sequence &&
            later.txHash === entry.txHash &&
            later.phase === "confirmed",
        ),
    );

export const runScriptIntegrityHashMismatchWorkflow = async ({
  evidence,
  journal,
  transactions,
}: {
  evidence: ScriptIntegrityHashMismatchEvidence;
  journal: ScriptIntegrityHashMismatchJournal;
  transactions: ScriptIntegrityHashMismatchTransactionPort;
}): Promise<ScriptIntegrityHashMismatchWorkflowStage> => {
  const identity = scriptIntegrityHashMismatchEvidenceIdentity(evidence);
  const entries = await journal.load(identity);
  validate(entries, identity);
  const pending = unresolved(entries);
  if (pending !== undefined) {
    if (!(await transactions.transactionConfirmed(pending.txHash)))
      throw new Error(
        "scriptIntegrityHashMismatch exact intended transaction is unresolved",
      );
    const observed = await transactions.observe(identity);
    if (cursorDigest(observed) !== cursorDigest(pending.target))
      throw new Error(
        "scriptIntegrityHashMismatch restart cursor substitution",
      );
    await journal.append({
      ...pending,
      sequence: entries.length,
      phase: "confirmed",
    });
    return observed.stage;
  }
  const source = await transactions.observe(identity);
  const action = actionFor(source.stage);
  if (action === "done") return source.stage;
  const captured = await transactions.capture({ action, evidence, source });
  if (!/^[0-9a-f]{64}$/u.test(captured.txHash))
    throw new Error(
      "scriptIntegrityHashMismatch captured transaction is malformed",
    );
  const nextSequence = entries.length;
  const intent: ScriptIntegrityHashMismatchJournalEntry = {
    sequence: nextSequence,
    identity,
    action,
    phase: "intent",
    source,
    target: captured.target,
    txHash: captured.txHash,
  };
  await journal.append(intent);
  if ((await captured.submit()) !== captured.txHash)
    throw new Error(
      "scriptIntegrityHashMismatch provider substituted transaction identity",
    );
  await journal.append({
    ...intent,
    sequence: nextSequence + 1,
    phase: "submitted",
  });
  return source.stage;
};

export const cancelScriptIntegrityHashMismatchWorkflow = async ({
  evidence,
  journal,
  transactions,
}: {
  evidence: ScriptIntegrityHashMismatchEvidence;
  journal: ScriptIntegrityHashMismatchJournal;
  transactions: ScriptIntegrityHashMismatchTransactionPort;
}): Promise<"cancelled"> => {
  const identity = scriptIntegrityHashMismatchEvidenceIdentity(evidence);
  const entries = await journal.load(identity);
  validate(entries, identity);
  if (unresolved(entries) !== undefined)
    throw new Error(
      "scriptIntegrityHashMismatch must reconcile before cancellation",
    );
  const source = await transactions.observe(identity);
  if (["none", "proven", "removed", "cancelled"].includes(source.stage))
    throw new Error("scriptIntegrityHashMismatch stage cannot cancel");
  const captured = await transactions.capture({
    action: "cancel",
    evidence,
    source,
  });
  if (captured.target.stage !== "cancelled")
    throw new Error("scriptIntegrityHashMismatch cancellation target changed");
  const nextSequence = entries.length;
  const intent: ScriptIntegrityHashMismatchJournalEntry = {
    sequence: nextSequence,
    identity,
    action: "cancel",
    phase: "intent",
    source,
    target: captured.target,
    txHash: captured.txHash,
  };
  await journal.append(intent);
  if ((await captured.submit()) !== captured.txHash)
    throw new Error(
      "scriptIntegrityHashMismatch provider substituted cancellation",
    );
  await journal.append({
    ...intent,
    sequence: nextSequence + 1,
    phase: "submitted",
  });
  return "cancelled";
};
