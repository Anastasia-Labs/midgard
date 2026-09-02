import { createHash } from "node:crypto";

import type { ScriptIntegrityHashMismatchEvidenceV1 } from "./family-v1.js";
import { scriptIntegrityHashMismatchEvidenceIdentityV1 } from "./family-v1.js";

export const SCRIPT_INTEGRITY_HASH_MISMATCH_WORKFLOW_STAGES_V1 = [
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
export type ScriptIntegrityHashMismatchWorkflowStageV1 =
  (typeof SCRIPT_INTEGRITY_HASH_MISMATCH_WORKFLOW_STAGES_V1)[number];
export type ScriptIntegrityHashMismatchWorkflowActionV1 =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "submitStep04"
  | "submitStep05"
  | "removeDescendants"
  | "cancel";
export type ScriptIntegrityHashMismatchCursorV1 = Readonly<{
  stage: ScriptIntegrityHashMismatchWorkflowStageV1;
  threadOutRef: string;
}>;
export type ScriptIntegrityHashMismatchJournalEntryV1 = Readonly<{
  sequence: number;
  identity: string;
  action: ScriptIntegrityHashMismatchWorkflowActionV1;
  phase: "intent" | "submitted" | "confirmed";
  source: ScriptIntegrityHashMismatchCursorV1;
  target: ScriptIntegrityHashMismatchCursorV1;
  txHash: string;
}>;
export interface ScriptIntegrityHashMismatchJournalV1 {
  load(
    identity: string,
  ): Promise<readonly ScriptIntegrityHashMismatchJournalEntryV1[]>;
  append(entry: ScriptIntegrityHashMismatchJournalEntryV1): Promise<void>;
}
export interface ScriptIntegrityHashMismatchTransactionPortV1 {
  observe(identity: string): Promise<ScriptIntegrityHashMismatchCursorV1>;
  capture(input: {
    action: ScriptIntegrityHashMismatchWorkflowActionV1;
    evidence: ScriptIntegrityHashMismatchEvidenceV1;
    source: ScriptIntegrityHashMismatchCursorV1;
  }): Promise<
    Readonly<{
      txHash: string;
      target: ScriptIntegrityHashMismatchCursorV1;
      submit: () => Promise<string>;
    }>
  >;
  transactionConfirmed(txHash: string): Promise<boolean>;
}

const actionFor = (
  stage: ScriptIntegrityHashMismatchWorkflowStageV1,
): ScriptIntegrityHashMismatchWorkflowActionV1 | "done" =>
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
  })[stage] as ScriptIntegrityHashMismatchWorkflowActionV1 | "done";
const cursorDigest = (cursor: ScriptIntegrityHashMismatchCursorV1) =>
  createHash("sha256").update(JSON.stringify(cursor)).digest("hex");
const validate = (
  entries: readonly ScriptIntegrityHashMismatchJournalEntryV1[],
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
  entries: readonly ScriptIntegrityHashMismatchJournalEntryV1[],
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

export const runScriptIntegrityHashMismatchWorkflowV1 = async ({
  evidence,
  journal,
  transactions,
}: {
  evidence: ScriptIntegrityHashMismatchEvidenceV1;
  journal: ScriptIntegrityHashMismatchJournalV1;
  transactions: ScriptIntegrityHashMismatchTransactionPortV1;
}): Promise<ScriptIntegrityHashMismatchWorkflowStageV1> => {
  const identity = scriptIntegrityHashMismatchEvidenceIdentityV1(evidence);
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
  const intent: ScriptIntegrityHashMismatchJournalEntryV1 = {
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

export const cancelScriptIntegrityHashMismatchWorkflowV1 = async ({
  evidence,
  journal,
  transactions,
}: {
  evidence: ScriptIntegrityHashMismatchEvidenceV1;
  journal: ScriptIntegrityHashMismatchJournalV1;
  transactions: ScriptIntegrityHashMismatchTransactionPortV1;
}): Promise<"cancelled"> => {
  const identity = scriptIntegrityHashMismatchEvidenceIdentityV1(evidence);
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
  const intent: ScriptIntegrityHashMismatchJournalEntryV1 = {
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
