import { createHash } from "node:crypto";

import type { MissingScriptSourceEvidence } from "./family.js";
import { missingScriptSourceEvidenceIdentity } from "./family.js";

export const MISSING_SCRIPT_SOURCE_WORKFLOW_STAGES = [
  "none",
  "step01",
  "step02",
  "step03",
  "scan",
  "proven",
  "removed",
  "cancelled",
] as const;
export type MissingScriptSourceWorkflowStage =
  (typeof MISSING_SCRIPT_SOURCE_WORKFLOW_STAGES)[number];
export type MissingScriptSourceWorkflowAction =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "submitScanOrResume"
  | "submitStep05"
  | "removeDescendants"
  | "cancel";

export type MissingScriptSourceCursor = Readonly<{
  stage: MissingScriptSourceWorkflowStage;
  threadOutRef: string;
  checkpointHash: string;
  controlCbor: string;
}>;

export type MissingScriptSourceJournalEntry = Readonly<{
  sequence: number;
  identity: string;
  action: MissingScriptSourceWorkflowAction;
  phase: "intent" | "submitted" | "confirmed";
  source: MissingScriptSourceCursor;
  target: MissingScriptSourceCursor;
  txHash: string;
}>;

export interface MissingScriptSourceJournal {
  load(identity: string): Promise<readonly MissingScriptSourceJournalEntry[]>;
  append(entry: MissingScriptSourceJournalEntry): Promise<void>;
}

export interface MissingScriptSourceTransactionPort {
  observe(identity: string): Promise<MissingScriptSourceCursor>;
  capture(input: {
    action: MissingScriptSourceWorkflowAction;
    evidence: MissingScriptSourceEvidence;
    source: MissingScriptSourceCursor;
  }): Promise<
    Readonly<{
      txHash: string;
      target: MissingScriptSourceCursor;
      submit: () => Promise<string>;
    }>
  >;
  transactionConfirmed(txHash: string): Promise<boolean>;
}

const actionFor = (
  stage: MissingScriptSourceWorkflowStage,
): MissingScriptSourceWorkflowAction | "done" =>
  ({
    none: "submitInit",
    step01: "submitStep01",
    step02: "submitStep02",
    step03: "submitStep03",
    scan: "submitScanOrResume",
    proven: "removeDescendants",
    removed: "done",
    cancelled: "done",
  })[stage] as MissingScriptSourceWorkflowAction | "done";

const cursorDigest = (cursor: MissingScriptSourceCursor): string =>
  createHash("sha256").update(JSON.stringify(cursor)).digest("hex");

const validate = (
  entries: readonly MissingScriptSourceJournalEntry[],
  identity: string,
): void => {
  entries.forEach((entry, sequence) => {
    if (entry.identity !== identity || entry.sequence !== sequence)
      throw new Error("missingScriptSource journal identity/sequence changed");
    if (!/^[0-9a-f]{64}$/u.test(entry.txHash))
      throw new Error("missingScriptSource journal transaction is malformed");
  });
};

const unresolved = (entries: readonly MissingScriptSourceJournalEntry[]) =>
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

/** Durable family loop. The production factory owns both supplied ports. */
export const runMissingScriptSourceWorkflow = async ({
  evidence,
  journal,
  transactions,
}: {
  evidence: MissingScriptSourceEvidence;
  journal: MissingScriptSourceJournal;
  transactions: MissingScriptSourceTransactionPort;
}): Promise<MissingScriptSourceWorkflowStage> => {
  const identity = missingScriptSourceEvidenceIdentity(evidence);
  for (;;) {
    const entries = await journal.load(identity);
    validate(entries, identity);
    const pending = unresolved(entries);
    if (pending !== undefined) {
      if (!(await transactions.transactionConfirmed(pending.txHash)))
        throw new Error(
          "missingScriptSource exact intended transaction is unresolved",
        );
      const observed = await transactions.observe(identity);
      if (cursorDigest(observed) !== cursorDigest(pending.target))
        throw new Error(
          "missingScriptSource restart cursor/checkpoint substitution",
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
      throw new Error("missingScriptSource captured transaction is malformed");
    const nextSequence = entries.length;
    const intent: MissingScriptSourceJournalEntry = {
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
        "missingScriptSource provider substituted transaction identity",
      );
    await journal.append({
      ...intent,
      sequence: nextSequence + 1,
      phase: "submitted",
    });
    return source.stage;
  }
};

export const cancelMissingScriptSourceWorkflow = async ({
  evidence,
  journal,
  transactions,
}: {
  evidence: MissingScriptSourceEvidence;
  journal: MissingScriptSourceJournal;
  transactions: MissingScriptSourceTransactionPort;
}): Promise<"cancelled"> => {
  const identity = missingScriptSourceEvidenceIdentity(evidence);
  const entries = await journal.load(identity);
  validate(entries, identity);
  if (unresolved(entries) !== undefined)
    throw new Error("missingScriptSource must reconcile before cancellation");
  const source = await transactions.observe(identity);
  if (["none", "proven", "removed", "cancelled"].includes(source.stage))
    throw new Error("missingScriptSource stage cannot cancel");
  const captured = await transactions.capture({
    action: "cancel",
    evidence,
    source,
  });
  if (captured.target.stage !== "cancelled")
    throw new Error("missingScriptSource cancellation target changed");
  const nextSequence = entries.length;
  const intent: MissingScriptSourceJournalEntry = {
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
    throw new Error("missingScriptSource provider substituted cancellation");
  await journal.append({
    ...intent,
    sequence: nextSequence + 1,
    phase: "submitted",
  });
  if (!(await transactions.transactionConfirmed(captured.txHash)))
    throw new Error("missingScriptSource cancellation is unconfirmed");
  const observed = await transactions.observe(identity);
  if (observed.stage !== "cancelled")
    throw new Error("missingScriptSource cancellation cursor was substituted");
  await journal.append({
    ...intent,
    sequence: nextSequence + 2,
    phase: "confirmed",
  });
  return "cancelled";
};
