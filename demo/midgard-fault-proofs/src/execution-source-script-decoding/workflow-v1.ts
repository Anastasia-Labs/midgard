import { createHash } from "node:crypto";

import type { ExecutionSourceScriptDecodingEvidenceV1 } from "./family-v1.js";
import { executionSourceScriptDecodingEvidenceIdentityV1 } from "./family-v1.js";

export const EXECUTION_SOURCE_SCRIPT_DECODING_WORKFLOW_STAGES_V1 = [
  "none",
  "step01",
  "step02",
  "step03",
  "scan",
  "proven",
  "removed",
  "cancelled",
] as const;
export type ExecutionSourceScriptDecodingWorkflowStageV1 =
  (typeof EXECUTION_SOURCE_SCRIPT_DECODING_WORKFLOW_STAGES_V1)[number];
export type ExecutionSourceScriptDecodingWorkflowActionV1 =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "submitScanOrResume"
  | "submitStep05"
  | "removeDescendants"
  | "cancel";

export type ExecutionSourceScriptDecodingCursorV1 = Readonly<{
  stage: ExecutionSourceScriptDecodingWorkflowStageV1;
  threadOutRef: string;
  checkpointHash: string;
  controlCbor: string;
}>;

export type ExecutionSourceScriptDecodingJournalEntryV1 = Readonly<{
  sequence: number;
  identity: string;
  action: ExecutionSourceScriptDecodingWorkflowActionV1;
  phase: "intent" | "submitted" | "confirmed";
  source: ExecutionSourceScriptDecodingCursorV1;
  target: ExecutionSourceScriptDecodingCursorV1;
  txHash: string;
}>;

export interface ExecutionSourceScriptDecodingJournalV1 {
  load(
    identity: string,
  ): Promise<readonly ExecutionSourceScriptDecodingJournalEntryV1[]>;
  append(entry: ExecutionSourceScriptDecodingJournalEntryV1): Promise<void>;
}

export interface ExecutionSourceScriptDecodingTransactionPortV1 {
  observe(identity: string): Promise<ExecutionSourceScriptDecodingCursorV1>;
  capture(input: {
    action: ExecutionSourceScriptDecodingWorkflowActionV1;
    evidence: ExecutionSourceScriptDecodingEvidenceV1;
    source: ExecutionSourceScriptDecodingCursorV1;
  }): Promise<
    Readonly<{
      txHash: string;
      target: ExecutionSourceScriptDecodingCursorV1;
      submit: () => Promise<string>;
    }>
  >;
  transactionConfirmed(txHash: string): Promise<boolean>;
}

const actionFor = (
  stage: ExecutionSourceScriptDecodingWorkflowStageV1,
): ExecutionSourceScriptDecodingWorkflowActionV1 | "done" =>
  ({
    none: "submitInit",
    step01: "submitStep01",
    step02: "submitStep02",
    step03: "submitStep03",
    scan: "submitScanOrResume",
    proven: "removeDescendants",
    removed: "done",
    cancelled: "done",
  })[stage] as ExecutionSourceScriptDecodingWorkflowActionV1 | "done";

const cursorDigest = (cursor: ExecutionSourceScriptDecodingCursorV1): string =>
  createHash("sha256").update(JSON.stringify(cursor)).digest("hex");

const validate = (
  entries: readonly ExecutionSourceScriptDecodingJournalEntryV1[],
  identity: string,
): void => {
  entries.forEach((entry, sequence) => {
    if (entry.identity !== identity || entry.sequence !== sequence)
      throw new Error(
        "executionSourceScriptDecoding journal identity/sequence changed",
      );
    if (!/^[0-9a-f]{64}$/u.test(entry.txHash))
      throw new Error(
        "executionSourceScriptDecoding journal transaction is malformed",
      );
  });
};

const unresolved = (
  entries: readonly ExecutionSourceScriptDecodingJournalEntryV1[],
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

/** Durable family loop. The production factory owns both supplied ports. */
export const runExecutionSourceScriptDecodingWorkflowV1 = async ({
  evidence,
  journal,
  transactions,
}: {
  evidence: ExecutionSourceScriptDecodingEvidenceV1;
  journal: ExecutionSourceScriptDecodingJournalV1;
  transactions: ExecutionSourceScriptDecodingTransactionPortV1;
}): Promise<ExecutionSourceScriptDecodingWorkflowStageV1> => {
  const identity = executionSourceScriptDecodingEvidenceIdentityV1(evidence);
  for (;;) {
    const entries = await journal.load(identity);
    validate(entries, identity);
    const pending = unresolved(entries);
    if (pending !== undefined) {
      if (!(await transactions.transactionConfirmed(pending.txHash)))
        throw new Error(
          "executionSourceScriptDecoding exact intended transaction is unresolved",
        );
      const observed = await transactions.observe(identity);
      if (cursorDigest(observed) !== cursorDigest(pending.target))
        throw new Error(
          "executionSourceScriptDecoding restart cursor/checkpoint substitution",
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
        "executionSourceScriptDecoding captured transaction is malformed",
      );
    const nextSequence = entries.length;
    const intent: ExecutionSourceScriptDecodingJournalEntryV1 = {
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
        "executionSourceScriptDecoding provider substituted transaction identity",
      );
    await journal.append({
      ...intent,
      sequence: nextSequence + 1,
      phase: "submitted",
    });
    return source.stage;
  }
};

export const cancelExecutionSourceScriptDecodingWorkflowV1 = async ({
  evidence,
  journal,
  transactions,
}: {
  evidence: ExecutionSourceScriptDecodingEvidenceV1;
  journal: ExecutionSourceScriptDecodingJournalV1;
  transactions: ExecutionSourceScriptDecodingTransactionPortV1;
}): Promise<"cancelled"> => {
  const identity = executionSourceScriptDecodingEvidenceIdentityV1(evidence);
  const entries = await journal.load(identity);
  validate(entries, identity);
  if (unresolved(entries) !== undefined)
    throw new Error(
      "executionSourceScriptDecoding must reconcile before cancellation",
    );
  const source = await transactions.observe(identity);
  if (["none", "proven", "removed", "cancelled"].includes(source.stage))
    throw new Error("executionSourceScriptDecoding stage cannot cancel");
  const captured = await transactions.capture({
    action: "cancel",
    evidence,
    source,
  });
  if (captured.target.stage !== "cancelled")
    throw new Error(
      "executionSourceScriptDecoding cancellation target changed",
    );
  const nextSequence = entries.length;
  const intent: ExecutionSourceScriptDecodingJournalEntryV1 = {
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
      "executionSourceScriptDecoding provider substituted cancellation",
    );
  await journal.append({
    ...intent,
    sequence: nextSequence + 1,
    phase: "submitted",
  });
  if (!(await transactions.transactionConfirmed(captured.txHash)))
    throw new Error(
      "executionSourceScriptDecoding cancellation is unconfirmed",
    );
  const observed = await transactions.observe(identity);
  if (observed.stage !== "cancelled")
    throw new Error(
      "executionSourceScriptDecoding cancellation cursor was substituted",
    );
  await journal.append({
    ...intent,
    sequence: nextSequence + 2,
    phase: "confirmed",
  });
  return "cancelled";
};
