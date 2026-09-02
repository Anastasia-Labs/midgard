import { createHash } from "node:crypto";

import type { ReceivePurposeLanguageEvidenceV1 } from "./family-v1.js";
import { receivePurposeLanguageEvidenceIdentityV1 } from "./family-v1.js";

export const RECEIVE_PURPOSE_LANGUAGE_WORKFLOW_STAGES_V1 = [
  "none",
  "step01",
  "step02",
  "step03",
  "proven",
  "removed",
  "cancelled",
] as const;
export type ReceivePurposeLanguageWorkflowStageV1 =
  (typeof RECEIVE_PURPOSE_LANGUAGE_WORKFLOW_STAGES_V1)[number];
export type ReceivePurposeLanguageWorkflowActionV1 =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "removeDescendants"
  | "cancel";
export type ReceivePurposeLanguageCursorV1 = Readonly<{
  stage: ReceivePurposeLanguageWorkflowStageV1;
  threadOutRef: string;
}>;
export type ReceivePurposeLanguageJournalEntryV1 = Readonly<{
  sequence: number;
  identity: string;
  action: ReceivePurposeLanguageWorkflowActionV1;
  phase: "intent" | "submitted" | "confirmed";
  source: ReceivePurposeLanguageCursorV1;
  target: ReceivePurposeLanguageCursorV1;
  txHash: string;
}>;
export interface ReceivePurposeLanguageJournalV1 {
  load(
    identity: string,
  ): Promise<readonly ReceivePurposeLanguageJournalEntryV1[]>;
  append(entry: ReceivePurposeLanguageJournalEntryV1): Promise<void>;
}
export interface ReceivePurposeLanguageTransactionPortV1 {
  observe(identity: string): Promise<ReceivePurposeLanguageCursorV1>;
  capture(input: {
    action: ReceivePurposeLanguageWorkflowActionV1;
    evidence: ReceivePurposeLanguageEvidenceV1;
    source: ReceivePurposeLanguageCursorV1;
  }): Promise<
    Readonly<{
      txHash: string;
      target: ReceivePurposeLanguageCursorV1;
      submit: () => Promise<string>;
    }>
  >;
  transactionConfirmed(txHash: string): Promise<boolean>;
}
const actionFor = (
  stage: ReceivePurposeLanguageWorkflowStageV1,
): ReceivePurposeLanguageWorkflowActionV1 | "done" =>
  ({
    none: "submitInit",
    step01: "submitStep01",
    step02: "submitStep02",
    step03: "submitStep03",
    proven: "removeDescendants",
    removed: "done",
    cancelled: "done",
  })[stage] as ReceivePurposeLanguageWorkflowActionV1 | "done";
const digest = (cursor: ReceivePurposeLanguageCursorV1) =>
  createHash("sha256").update(JSON.stringify(cursor)).digest("hex");
const validate = (
  entries: readonly ReceivePurposeLanguageJournalEntryV1[],
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
const unresolved = (entries: readonly ReceivePurposeLanguageJournalEntryV1[]) =>
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
export const runReceivePurposeLanguageWorkflowV1 = async ({
  evidence,
  journal,
  transactions,
}: {
  evidence: ReceivePurposeLanguageEvidenceV1;
  journal: ReceivePurposeLanguageJournalV1;
  transactions: ReceivePurposeLanguageTransactionPortV1;
}): Promise<ReceivePurposeLanguageWorkflowStageV1> => {
  const identity = receivePurposeLanguageEvidenceIdentityV1(evidence);
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
    const intent: ReceivePurposeLanguageJournalEntryV1 = {
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
export const cancelReceivePurposeLanguageWorkflowV1 = async ({
  evidence,
  journal,
  transactions,
}: {
  evidence: ReceivePurposeLanguageEvidenceV1;
  journal: ReceivePurposeLanguageJournalV1;
  transactions: ReceivePurposeLanguageTransactionPortV1;
}): Promise<"cancelled"> => {
  const identity = receivePurposeLanguageEvidenceIdentityV1(evidence);
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
  const intent: ReceivePurposeLanguageJournalEntryV1 = {
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
