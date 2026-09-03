import { createHash } from "node:crypto";

import {
  type UnusedScriptWitnessEvidence,
  unusedScriptWitnessEvidenceIdentity,
} from "./family-v1.js";

export const UNUSED_SCRIPT_WITNESS_WORKFLOW_STAGES = [
  "none",
  "step01",
  "step02",
  "step03",
  "step04",
  "step05",
  "step06",
  "proven",
  "removed",
  "cancelled",
] as const;
export type UnusedScriptWitnessWorkflowStage =
  (typeof UNUSED_SCRIPT_WITNESS_WORKFLOW_STAGES)[number];
export type UnusedScriptWitnessWorkflowAction =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "submitStep04"
  | "submitStep05"
  | "submitStep06"
  | "removeDescendants"
  | "cancel";
export type UnusedScriptWitnessCursor = Readonly<{
  stage: UnusedScriptWitnessWorkflowStage;
  threadOutRef: string;
  checkpointDigest: string;
}>;
export type UnusedScriptWitnessJournalEntry = Readonly<{
  sequence: number;
  identity: string;
  action: UnusedScriptWitnessWorkflowAction;
  phase: "intent" | "submitted" | "confirmed";
  source: UnusedScriptWitnessCursor;
  target: UnusedScriptWitnessCursor;
  txHash: string;
}>;
export interface UnusedScriptWitnessJournal {
  load(identity: string): Promise<readonly UnusedScriptWitnessJournalEntry[]>;
  append(entry: UnusedScriptWitnessJournalEntry): Promise<void>;
}
export interface UnusedScriptWitnessActuator {
  observe(identity: string): Promise<UnusedScriptWitnessCursor>;
  capture(input: {
    action: UnusedScriptWitnessWorkflowAction;
    evidence: UnusedScriptWitnessEvidence;
    source: UnusedScriptWitnessCursor;
  }): Promise<
    Readonly<{
      txHash: string;
      target: UnusedScriptWitnessCursor;
      submit: () => Promise<string>;
    }>
  >;
  transactionConfirmed(txHash: string): Promise<boolean>;
}
const actions: Record<
  UnusedScriptWitnessWorkflowStage,
  UnusedScriptWitnessWorkflowAction | "done"
> = {
  none: "submitInit",
  step01: "submitStep01",
  step02: "submitStep02",
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
  entries: readonly UnusedScriptWitnessJournalEntry[],
  identity: string,
) =>
  entries.forEach((entry, sequence) => {
    if (
      entry.identity !== identity ||
      entry.sequence !== sequence ||
      !/^[0-9a-f]{64}$/u.test(entry.txHash)
    )
      throw new Error("unusedScriptWitness journal identity changed");
  });
export const runUnusedScriptWitnessWorkflow = async ({
  evidence,
  journal,
  actuator,
}: {
  evidence: UnusedScriptWitnessEvidence;
  journal: UnusedScriptWitnessJournal;
  actuator: UnusedScriptWitnessActuator;
}): Promise<UnusedScriptWitnessWorkflowStage> => {
  const identity = unusedScriptWitnessEvidenceIdentity(evidence);
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
      throw new Error(
        "unusedScriptWitness exact intended transaction unresolved",
      );
    const observed = await actuator.observe(identity);
    if (digest(observed) !== digest(intent.target))
      throw new Error("unusedScriptWitness restart cursor substitution");
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
    throw new Error("unusedScriptWitness captured transaction is malformed");
  const nextSequence = entries.length;
  const next: UnusedScriptWitnessJournalEntry = {
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
    throw new Error(
      "unusedScriptWitness provider substituted transaction identity",
    );
  await journal.append({
    ...next,
    sequence: nextSequence + 1,
    phase: "submitted",
  });
  return source.stage;
};

export const cancelUnusedScriptWitnessWorkflow = async ({
  evidence,
  journal,
  actuator,
}: {
  evidence: UnusedScriptWitnessEvidence;
  journal: UnusedScriptWitnessJournal;
  actuator: UnusedScriptWitnessActuator;
}): Promise<"cancelled"> => {
  const identity = unusedScriptWitnessEvidenceIdentity(evidence);
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
    throw new Error("unusedScriptWitness must reconcile before cancellation");
  const source = await actuator.observe(identity);
  if (
    source.stage === "none" ||
    source.stage === "proven" ||
    source.stage === "removed" ||
    source.stage === "cancelled"
  )
    throw new Error("unusedScriptWitness stage cannot cancel");
  const captured = await actuator.capture({
    action: "cancel",
    evidence,
    source,
  });
  if (captured.target.stage !== "cancelled")
    throw new Error("unusedScriptWitness cancellation target changed");
  const sequence = entries.length;
  const intent: UnusedScriptWitnessJournalEntry = {
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
    throw new Error("unusedScriptWitness provider substituted cancellation");
  await journal.append({
    ...intent,
    sequence: sequence + 1,
    phase: "submitted",
  });
  if (!(await actuator.transactionConfirmed(captured.txHash)))
    throw new Error("unusedScriptWitness cancellation is unconfirmed");
  const observed = await actuator.observe(identity);
  if (observed.stage !== "cancelled")
    throw new Error("unusedScriptWitness cancellation cursor was substituted");
  await journal.append({
    ...intent,
    sequence: sequence + 2,
    phase: "confirmed",
  });
  return "cancelled";
};
