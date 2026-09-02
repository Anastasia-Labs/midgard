import { createHash } from "node:crypto";

import type { MissingScriptSourceEvidenceV1 } from "./family-v1.js";
import { missingScriptSourceEvidenceIdentityV1 } from "./family-v1.js";

export const MISSING_SCRIPT_SOURCE_WORKFLOW_STAGES_V1 = [
  "none",
  "step01",
  "step02",
  "step03",
  "scan",
  "proven",
  "removed",
  "cancelled",
] as const;
export type MissingScriptSourceWorkflowStageV1 =
  (typeof MISSING_SCRIPT_SOURCE_WORKFLOW_STAGES_V1)[number];
export type MissingScriptSourceWorkflowActionV1 =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "submitScanOrResume"
  | "submitStep05"
  | "removeDescendants"
  | "cancel";

export type MissingScriptSourceCursorV1 = Readonly<{
  stage: MissingScriptSourceWorkflowStageV1;
  threadOutRef: string;
  checkpointHash: string;
  controlCbor: string;
}>;

export type MissingScriptSourceJournalEntryV1 = Readonly<{
  sequence: number;
  identity: string;
  action: MissingScriptSourceWorkflowActionV1;
  phase: "intent" | "submitted" | "confirmed";
  source: MissingScriptSourceCursorV1;
  target: MissingScriptSourceCursorV1;
  txHash: string;
}>;

export interface MissingScriptSourceJournalV1 {
  load(identity: string): Promise<readonly MissingScriptSourceJournalEntryV1[]>;
  append(entry: MissingScriptSourceJournalEntryV1): Promise<void>;
}

export interface MissingScriptSourceTransactionPortV1 {
  observe(identity: string): Promise<MissingScriptSourceCursorV1>;
  capture(input: {
    action: MissingScriptSourceWorkflowActionV1;
    evidence: MissingScriptSourceEvidenceV1;
    source: MissingScriptSourceCursorV1;
  }): Promise<
    Readonly<{
      txHash: string;
      target: MissingScriptSourceCursorV1;
      submit: () => Promise<string>;
    }>
  >;
  transactionConfirmed(txHash: string): Promise<boolean>;
}

const actionFor = (
  stage: MissingScriptSourceWorkflowStageV1,
): MissingScriptSourceWorkflowActionV1 | "done" =>
  ({
    none: "submitInit",
    step01: "submitStep01",
    step02: "submitStep02",
    step03: "submitStep03",
    scan: "submitScanOrResume",
    proven: "removeDescendants",
    removed: "done",
    cancelled: "done",
  })[stage] as MissingScriptSourceWorkflowActionV1 | "done";

const cursorDigest = (cursor: MissingScriptSourceCursorV1): string =>
  createHash("sha256").update(JSON.stringify(cursor)).digest("hex");

const validate = (
  entries: readonly MissingScriptSourceJournalEntryV1[],
  identity: string,
): void => {
  entries.forEach((entry, sequence) => {
    if (entry.identity !== identity || entry.sequence !== sequence)
      throw new Error("missingScriptSource journal identity/sequence changed");
    if (!/^[0-9a-f]{64}$/u.test(entry.txHash))
      throw new Error("missingScriptSource journal transaction is malformed");
  });
};

const unresolved = (entries: readonly MissingScriptSourceJournalEntryV1[]) =>
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
export const runMissingScriptSourceWorkflowV1 = async ({
  evidence,
  journal,
  transactions,
}: {
  evidence: MissingScriptSourceEvidenceV1;
  journal: MissingScriptSourceJournalV1;
  transactions: MissingScriptSourceTransactionPortV1;
}): Promise<MissingScriptSourceWorkflowStageV1> => {
  const identity = missingScriptSourceEvidenceIdentityV1(evidence);
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
    const intent: MissingScriptSourceJournalEntryV1 = {
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

export const cancelMissingScriptSourceWorkflowV1 = async ({
  evidence,
  journal,
  transactions,
}: {
  evidence: MissingScriptSourceEvidenceV1;
  journal: MissingScriptSourceJournalV1;
  transactions: MissingScriptSourceTransactionPortV1;
}): Promise<"cancelled"> => {
  const identity = missingScriptSourceEvidenceIdentityV1(evidence);
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
  const intent: MissingScriptSourceJournalEntryV1 = {
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
