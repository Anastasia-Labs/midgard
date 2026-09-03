import { createHash } from "node:crypto";

import {
  type UnusedRedeemerEvidence,
  unusedRedeemerEvidenceIdentity,
} from "./family-v1.js";

export const UNUSED_REDEEMER_WORKFLOW_STAGES = [
  "none",
  "step01",
  "step02",
  "step02a",
  "step02b",
  "step02c",
  "step03",
  "step04",
  "step05",
  "step06",
  "proven",
  "removed",
  "cancelled",
] as const;
export type UnusedRedeemerWorkflowStage =
  (typeof UNUSED_REDEEMER_WORKFLOW_STAGES)[number];
export type UnusedRedeemerWorkflowAction =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep02a"
  | "submitStep02b"
  | "submitStep02c"
  | "submitStep03"
  | "submitStep04"
  | "submitStep05"
  | "submitStep06"
  | "removeDescendants"
  | "cancel";
export type UnusedRedeemerCursor = Readonly<{
  stage: UnusedRedeemerWorkflowStage;
  threadOutRef: string;
  checkpointDigest: string;
}>;
export type UnusedRedeemerJournalEntry = Readonly<{
  sequence: number;
  identity: string;
  action: UnusedRedeemerWorkflowAction;
  phase: "intent" | "submitted" | "confirmed";
  source: UnusedRedeemerCursor;
  target: UnusedRedeemerCursor;
  txHash: string;
}>;
export interface UnusedRedeemerJournal {
  load(identity: string): Promise<readonly UnusedRedeemerJournalEntry[]>;
  append(entry: UnusedRedeemerJournalEntry): Promise<void>;
}
export interface UnusedRedeemerActuator {
  observe(identity: string): Promise<UnusedRedeemerCursor>;
  capture(input: {
    action: UnusedRedeemerWorkflowAction;
    evidence: UnusedRedeemerEvidence;
    source: UnusedRedeemerCursor;
  }): Promise<
    Readonly<{
      txHash: string;
      target: UnusedRedeemerCursor;
      submit: () => Promise<string>;
    }>
  >;
  transactionConfirmed(txHash: string): Promise<boolean>;
}
const actions: Record<
  UnusedRedeemerWorkflowStage,
  UnusedRedeemerWorkflowAction | "done"
> = {
  none: "submitInit",
  step01: "submitStep01",
  step02: "submitStep02",
  step02a: "submitStep02a",
  step02b: "submitStep02b",
  step02c: "submitStep02c",
  step03: "submitStep03",
  step04: "submitStep04",
  step05: "submitStep05",
  step06: "submitStep06",
  proven: "removeDescendants",
  removed: "done",
  cancelled: "done",
};
const digest = (value: unknown) =>
  createHash("sha256").update(JSON.stringify(value)).digest("hex");
const validate = (
  entries: readonly UnusedRedeemerJournalEntry[],
  identity: string,
) =>
  entries.forEach((entry, sequence) => {
    if (
      entry.identity !== identity ||
      entry.sequence !== sequence ||
      !/^[0-9a-f]{64}$/u.test(entry.txHash)
    )
      throw new Error("unusedRedeemer journal identity changed");
  });
export const runUnusedRedeemerWorkflow = async ({
  evidence,
  journal,
  actuator,
}: {
  evidence: UnusedRedeemerEvidence;
  journal: UnusedRedeemerJournal;
  actuator: UnusedRedeemerActuator;
}): Promise<UnusedRedeemerWorkflowStage> => {
  const identity = unusedRedeemerEvidenceIdentity(evidence);
  const entries = await journal.load(identity);
  validate(entries, identity);
  const intent = [...entries]
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
  if (intent !== undefined) {
    if (!(await actuator.transactionConfirmed(intent.txHash)))
      throw new Error("unusedRedeemer exact intended transaction unresolved");
    const observed = await actuator.observe(identity);
    if (digest(observed) !== digest(intent.target))
      throw new Error("unusedRedeemer restart cursor substitution");
    await journal.append({
      ...intent,
      sequence: entries.length,
      phase: "confirmed",
    });
    return observed.stage;
  }
  const source = await actuator.observe(identity);
  const action = actions[source.stage];
  if (action === "done") return source.stage;
  const captured = await actuator.capture({ action, evidence, source });
  if (!/^[0-9a-f]{64}$/u.test(captured.txHash))
    throw new Error("unusedRedeemer captured transaction is malformed");
  const nextSequence = entries.length;
  const next: UnusedRedeemerJournalEntry = {
    sequence: nextSequence,
    identity,
    action,
    phase: "intent",
    source,
    target: captured.target,
    txHash: captured.txHash,
  };
  await journal.append(next);
  if ((await captured.submit()) !== captured.txHash)
    throw new Error("unusedRedeemer provider substituted transaction identity");
  await journal.append({
    ...next,
    sequence: nextSequence + 1,
    phase: "submitted",
  });
  return source.stage;
};

export const cancelUnusedRedeemerWorkflow = async ({
  evidence,
  journal,
  actuator,
}: {
  evidence: UnusedRedeemerEvidence;
  journal: UnusedRedeemerJournal;
  actuator: UnusedRedeemerActuator;
}): Promise<"cancelled"> => {
  const identity = unusedRedeemerEvidenceIdentity(evidence);
  const entries = await journal.load(identity);
  validate(entries, identity);
  const unresolved = [...entries]
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
  if (unresolved !== undefined)
    throw new Error("unusedRedeemer must reconcile before cancellation");
  const source = await actuator.observe(identity);
  if (
    source.stage === "none" ||
    source.stage === "proven" ||
    source.stage === "removed" ||
    source.stage === "cancelled"
  )
    throw new Error("unusedRedeemer stage cannot cancel");
  const captured = await actuator.capture({
    action: "cancel",
    evidence,
    source,
  });
  if (captured.target.stage !== "cancelled")
    throw new Error("unusedRedeemer cancellation target changed");
  const sequence = entries.length;
  const intent: UnusedRedeemerJournalEntry = {
    sequence,
    identity,
    action: "cancel",
    phase: "intent",
    source,
    target: captured.target,
    txHash: captured.txHash,
  };
  await journal.append(intent);
  if ((await captured.submit()) !== captured.txHash)
    throw new Error("unusedRedeemer provider substituted cancellation");
  await journal.append({
    ...intent,
    sequence: sequence + 1,
    phase: "submitted",
  });
  if (!(await actuator.transactionConfirmed(captured.txHash)))
    throw new Error("unusedRedeemer cancellation is unconfirmed");
  const observed = await actuator.observe(identity);
  if (observed.stage !== "cancelled")
    throw new Error("unusedRedeemer cancellation cursor was substituted");
  await journal.append({
    ...intent,
    sequence: sequence + 2,
    phase: "confirmed",
  });
  return "cancelled";
};
