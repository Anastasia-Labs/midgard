import { createHash } from "node:crypto";

import {
  unusedRedeemerEvidenceIdentityV1,
  type UnusedRedeemerEvidenceV1,
} from "./family-v1.js";

export const UNUSED_REDEEMER_WORKFLOW_STAGES_V1 = [
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
export type UnusedRedeemerWorkflowStageV1 =
  (typeof UNUSED_REDEEMER_WORKFLOW_STAGES_V1)[number];
export type UnusedRedeemerWorkflowActionV1 =
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
export type UnusedRedeemerCursorV1 = Readonly<{
  stage: UnusedRedeemerWorkflowStageV1;
  threadOutRef: string;
  checkpointDigest: string;
}>;
export type UnusedRedeemerJournalEntryV1 = Readonly<{
  sequence: number;
  identity: string;
  action: UnusedRedeemerWorkflowActionV1;
  phase: "intent" | "submitted" | "confirmed";
  source: UnusedRedeemerCursorV1;
  target: UnusedRedeemerCursorV1;
  txHash: string;
}>;
export interface UnusedRedeemerJournalV1 {
  load(identity: string): Promise<readonly UnusedRedeemerJournalEntryV1[]>;
  append(entry: UnusedRedeemerJournalEntryV1): Promise<void>;
}
export interface UnusedRedeemerActuatorV1 {
  observe(identity: string): Promise<UnusedRedeemerCursorV1>;
  capture(input: {
    action: UnusedRedeemerWorkflowActionV1;
    evidence: UnusedRedeemerEvidenceV1;
    source: UnusedRedeemerCursorV1;
  }): Promise<
    Readonly<{
      txHash: string;
      target: UnusedRedeemerCursorV1;
      submit: () => Promise<string>;
    }>
  >;
  transactionConfirmed(txHash: string): Promise<boolean>;
}
const actions: Record<
  UnusedRedeemerWorkflowStageV1,
  UnusedRedeemerWorkflowActionV1 | "done"
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
  entries: readonly UnusedRedeemerJournalEntryV1[],
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
export const runUnusedRedeemerWorkflowV1 = async ({
  evidence,
  journal,
  actuator,
}: {
  evidence: UnusedRedeemerEvidenceV1;
  journal: UnusedRedeemerJournalV1;
  actuator: UnusedRedeemerActuatorV1;
}): Promise<UnusedRedeemerWorkflowStageV1> => {
  const identity = unusedRedeemerEvidenceIdentityV1(evidence);
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
  const next: UnusedRedeemerJournalEntryV1 = {
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

export const cancelUnusedRedeemerWorkflowV1 = async ({
  evidence,
  journal,
  actuator,
}: {
  evidence: UnusedRedeemerEvidenceV1;
  journal: UnusedRedeemerJournalV1;
  actuator: UnusedRedeemerActuatorV1;
}): Promise<"cancelled"> => {
  const identity = unusedRedeemerEvidenceIdentityV1(evidence);
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
  const intent: UnusedRedeemerJournalEntryV1 = {
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
