import { createHash } from "node:crypto";

import type { ReceivePurposeLanguageEvidence } from "./family-v1.js";
import { receivePurposeLanguageEvidenceIdentity } from "./family-v1.js";

export const RECEIVE_PURPOSE_LANGUAGE_WORKFLOW_STAGES = [
  "none",
  "step01",
  "step02",
  "step03",
  "proven",
  "removed",
  "cancelled",
] as const;
export type ReceivePurposeLanguageWorkflowStage =
  (typeof RECEIVE_PURPOSE_LANGUAGE_WORKFLOW_STAGES)[number];
export type ReceivePurposeLanguageWorkflowAction =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "removeDescendants"
  | "cancel";
export type ReceivePurposeLanguageCursor = Readonly<{
  stage: ReceivePurposeLanguageWorkflowStage;
  threadOutRef: string;
}>;
export type ReceivePurposeLanguageJournalEntry = Readonly<{
  sequence: number;
  identity: string;
  action: ReceivePurposeLanguageWorkflowAction;
  phase: "intent" | "submitted" | "confirmed";
  source: ReceivePurposeLanguageCursor;
  target: ReceivePurposeLanguageCursor;
  txHash: string;
}>;
export interface ReceivePurposeLanguageJournal {
  load(
    identity: string,
  ): Promise<readonly ReceivePurposeLanguageJournalEntry[]>;
  append(entry: ReceivePurposeLanguageJournalEntry): Promise<void>;
}
export interface ReceivePurposeLanguageTransactionPort {
  observe(identity: string): Promise<ReceivePurposeLanguageCursor>;
  capture(input: {
    action: ReceivePurposeLanguageWorkflowAction;
    evidence: ReceivePurposeLanguageEvidence;
    source: ReceivePurposeLanguageCursor;
  }): Promise<
    Readonly<{
      txHash: string;
      target: ReceivePurposeLanguageCursor;
      submit: () => Promise<string>;
    }>
  >;
  transactionConfirmed(txHash: string): Promise<boolean>;
}
const actionFor = (
  stage: ReceivePurposeLanguageWorkflowStage,
): ReceivePurposeLanguageWorkflowAction | "done" =>
  ({
    none: "submitInit",
    step01: "submitStep01",
    step02: "submitStep02",
    step03: "submitStep03",
    proven: "removeDescendants",
    removed: "done",
    cancelled: "done",
  })[stage] as ReceivePurposeLanguageWorkflowAction | "done";
const digest = (cursor: ReceivePurposeLanguageCursor) =>
  createHash("sha256").update(JSON.stringify(cursor)).digest("hex");
const validate = (
  entries: readonly ReceivePurposeLanguageJournalEntry[],
  identity: string,
) =>
  entries.forEach((entry, sequence) => {
    if (
      entry.identity !== identity ||
      entry.sequence !== sequence ||
      !/^[0-9a-f]{64}$/u.test(entry.txHash)
    )
      throw new Error(
        "receivePurposeLanguage journal identity/sequence/transaction changed",
      );
  });
const unresolved = (entries: readonly ReceivePurposeLanguageJournalEntry[]) =>
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
export const runReceivePurposeLanguageWorkflow = async ({
  evidence,
  journal,
  transactions,
}: {
  evidence: ReceivePurposeLanguageEvidence;
  journal: ReceivePurposeLanguageJournal;
  transactions: ReceivePurposeLanguageTransactionPort;
}): Promise<ReceivePurposeLanguageWorkflowStage> => {
  const identity = receivePurposeLanguageEvidenceIdentity(evidence);
  for (;;) {
    const entries = await journal.load(identity);
    validate(entries, identity);
    const pending = unresolved(entries);
    if (pending !== undefined) {
      if (!(await transactions.transactionConfirmed(pending.txHash)))
        throw new Error(
          "receivePurposeLanguage exact intended transaction is unresolved",
        );
      const observed = await transactions.observe(identity);
      if (digest(observed) !== digest(pending.target))
        throw new Error("receivePurposeLanguage restart cursor substitution");
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
        "receivePurposeLanguage captured transaction is malformed",
      );
    const nextSequence = entries.length;
    const intent: ReceivePurposeLanguageJournalEntry = {
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
        "receivePurposeLanguage provider substituted transaction identity",
      );
    await journal.append({
      ...intent,
      sequence: nextSequence + 1,
      phase: "submitted",
    });
    return source.stage;
  }
};
export const cancelReceivePurposeLanguageWorkflow = async ({
  evidence,
  journal,
  transactions,
}: {
  evidence: ReceivePurposeLanguageEvidence;
  journal: ReceivePurposeLanguageJournal;
  transactions: ReceivePurposeLanguageTransactionPort;
}): Promise<"cancelled"> => {
  const identity = receivePurposeLanguageEvidenceIdentity(evidence);
  const entries = await journal.load(identity);
  validate(entries, identity);
  if (unresolved(entries) !== undefined)
    throw new Error(
      "receivePurposeLanguage must reconcile before cancellation",
    );
  const source = await transactions.observe(identity);
  if (["none", "proven", "removed", "cancelled"].includes(source.stage))
    throw new Error("receivePurposeLanguage stage cannot cancel");
  const captured = await transactions.capture({
    action: "cancel",
    evidence,
    source,
  });
  if (captured.target.stage !== "cancelled")
    throw new Error("receivePurposeLanguage cancellation target changed");
  const nextSequence = entries.length;
  const intent: ReceivePurposeLanguageJournalEntry = {
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
    throw new Error("receivePurposeLanguage provider substituted cancellation");
  await journal.append({
    ...intent,
    sequence: nextSequence + 1,
    phase: "submitted",
  });
  if (!(await transactions.transactionConfirmed(captured.txHash)))
    throw new Error("receivePurposeLanguage cancellation is unconfirmed");
  const observed = await transactions.observe(identity);
  if (observed.stage !== "cancelled")
    throw new Error(
      "receivePurposeLanguage cancellation cursor was substituted",
    );
  await journal.append({
    ...intent,
    sequence: nextSequence + 2,
    phase: "confirmed",
  });
  return "cancelled";
};
