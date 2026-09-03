import { createHash } from "node:crypto";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { OutputReferenceScriptDecodingEvidence } from "./output-reference-script-decoding.js";

export const OUTPUT_REFERENCE_SCRIPT_DECODING_STAGES = [
  "none",
  "step01",
  "step02",
  "outputScan",
  "referenceBind",
  "scan",
  "step06",
  "proven",
  "removed",
  "cancelled",
] as const;
export type OutputReferenceScriptDecodingStage =
  (typeof OUTPUT_REFERENCE_SCRIPT_DECODING_STAGES)[number];
export type OutputReferenceScriptDecodingAction =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitOutputScan"
  | "submitReferenceBind"
  | "submitStructuralScan"
  | "submitStep06"
  | "cancel"
  | "removeDescendants"
  | "done";

type SubmitAction = Exclude<OutputReferenceScriptDecodingAction, "done">;
export type OutputReferenceScriptDecodingJournalEntry = Readonly<{
  sequence: number;
  identity: string;
  sourceStage: OutputReferenceScriptDecodingStage;
  targetStage: OutputReferenceScriptDecodingStage;
  action: SubmitAction;
  phase: "intent" | "submitted" | "confirmed";
  txHash: string;
}>;
export interface OutputReferenceScriptDecodingJournal {
  load(
    identity: string,
  ): Promise<readonly OutputReferenceScriptDecodingJournalEntry[]>;
  append(entry: OutputReferenceScriptDecodingJournalEntry): Promise<void>;
}
export interface OutputReferenceScriptDecodingActuator {
  observe(identity: string): Promise<OutputReferenceScriptDecodingStage>;
  build(input: {
    action: SubmitAction;
    evidence: OutputReferenceScriptDecodingEvidence;
    lease?: StateQueueMutationLeaseCoordinator;
  }): Promise<{
    txHash: string;
    targetStage: OutputReferenceScriptDecodingStage;
    submit(): Promise<string>;
  }>;
  transactionConfirmed(txHash: string): Promise<boolean>;
}

export const outputReferenceScriptDecodingEvidenceIdentity = (
  evidence: OutputReferenceScriptDecodingEvidence,
): string =>
  createHash("sha256")
    .update(
      JSON.stringify(
        {
          category: "outputReferenceScriptDecoding",
          subject: evidence.subject,
          outputIndex: evidence.outputIndex,
          transaction: evidence.canonicalTransactionCborHex,
          output: evidence.outputCborHex,
          script: evidence.referenceScriptItemHex,
        },
        (_key, value: unknown) =>
          typeof value === "bigint" ? value.toString() : value,
      ),
    )
    .digest("hex");

export const nextOutputReferenceScriptDecodingAction = (
  stage: OutputReferenceScriptDecodingStage,
): OutputReferenceScriptDecodingAction => {
  switch (stage) {
    case "none":
      return "submitInit";
    case "step01":
      return "submitStep01";
    case "step02":
      return "submitStep02";
    case "outputScan":
      return "submitOutputScan";
    case "referenceBind":
      return "submitReferenceBind";
    case "scan":
      return "submitStructuralScan";
    case "step06":
      return "submitStep06";
    case "proven":
      return "removeDescendants";
    case "removed":
    case "cancelled":
      return "done";
  }
};

const allowedTarget = (
  action: SubmitAction,
  target: OutputReferenceScriptDecodingStage,
): boolean => {
  const allowed: Record<
    SubmitAction,
    readonly OutputReferenceScriptDecodingStage[]
  > = {
    submitInit: ["step01"],
    submitStep01: ["step02"],
    submitStep02: ["outputScan"],
    submitOutputScan: ["outputScan", "referenceBind"],
    submitReferenceBind: ["scan"],
    submitStructuralScan: ["scan", "step06"],
    submitStep06: ["proven"],
    cancel: ["cancelled"],
    removeDescendants: ["removed"],
  };
  return allowed[action].includes(target);
};

const pending = (
  entries: readonly OutputReferenceScriptDecodingJournalEntry[],
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

export const runOutputReferenceScriptDecodingWorkflow = async ({
  evidence,
  journal,
  actuator,
  removalLease,
}: {
  evidence: OutputReferenceScriptDecodingEvidence;
  journal: OutputReferenceScriptDecodingJournal;
  actuator: OutputReferenceScriptDecodingActuator;
  removalLease?: StateQueueMutationLeaseCoordinator;
}): Promise<OutputReferenceScriptDecodingStage> => {
  const identity = outputReferenceScriptDecodingEvidenceIdentity(evidence);
  for (;;) {
    const entries = await journal.load(identity);
    if (
      entries.some(
        (entry, index) =>
          entry.identity !== identity || entry.sequence !== index,
      )
    )
      throw new Error(
        "outputReferenceScriptDecoding journal identity/sequence changed",
      );
    const pendingIntent = pending(entries);
    if (pendingIntent !== undefined) {
      if (!(await actuator.transactionConfirmed(pendingIntent.txHash)))
        throw new Error(
          "outputReferenceScriptDecoding exact intended transaction is unresolved",
        );
      if ((await actuator.observe(identity)) !== pendingIntent.targetStage)
        throw new Error(
          "outputReferenceScriptDecoding authenticated stage/transaction substitution",
        );
      await journal.append({
        ...pendingIntent,
        sequence: entries.length,
        phase: "confirmed",
      });
      continue;
    }
    const sourceStage = await actuator.observe(identity);
    const action = nextOutputReferenceScriptDecodingAction(sourceStage);
    if (action === "done") return sourceStage;
    if (action === "removeDescendants" && removalLease === undefined)
      throw new Error(
        "outputReferenceScriptDecoding removal requires state-queue lease",
      );
    const built = await actuator.build({
      action,
      evidence,
      ...(action === "removeDescendants" ? { lease: removalLease } : {}),
    });
    if (
      !/^[0-9a-f]{64}$/u.test(built.txHash) ||
      !allowedTarget(action, built.targetStage)
    )
      throw new Error(
        "outputReferenceScriptDecoding built transaction identity/target is invalid",
      );
    const intent: OutputReferenceScriptDecodingJournalEntry = {
      sequence: entries.length,
      identity,
      sourceStage,
      targetStage: built.targetStage,
      action,
      phase: "intent",
      txHash: built.txHash,
    };
    await journal.append(intent);
    if ((await built.submit()) !== built.txHash)
      throw new Error(
        "outputReferenceScriptDecoding submitter changed transaction identity",
      );
    await journal.append({
      ...intent,
      sequence: entries.length + 1,
      phase: "submitted",
    });
  }
};

export const cancelOutputReferenceScriptDecodingWorkflow = async ({
  evidence,
  journal,
  actuator,
}: {
  evidence: OutputReferenceScriptDecodingEvidence;
  journal: OutputReferenceScriptDecodingJournal;
  actuator: OutputReferenceScriptDecodingActuator;
}): Promise<"cancelled"> => {
  const identity = outputReferenceScriptDecodingEvidenceIdentity(evidence);
  const entries = await journal.load(identity);
  const sourceStage = await actuator.observe(identity);
  if (["none", "proven", "removed", "cancelled"].includes(sourceStage))
    throw new Error("outputReferenceScriptDecoding stage is not cancellable");
  const built = await actuator.build({ action: "cancel", evidence });
  if (
    !/^[0-9a-f]{64}$/u.test(built.txHash) ||
    built.targetStage !== "cancelled"
  )
    throw new Error(
      "outputReferenceScriptDecoding cancel identity/target is invalid",
    );
  const intent: OutputReferenceScriptDecodingJournalEntry = {
    sequence: entries.length,
    identity,
    sourceStage,
    targetStage: "cancelled",
    action: "cancel",
    phase: "intent",
    txHash: built.txHash,
  };
  await journal.append(intent);
  if ((await built.submit()) !== built.txHash)
    throw new Error(
      "outputReferenceScriptDecoding cancel transaction identity changed",
    );
  await journal.append({
    ...intent,
    sequence: entries.length + 1,
    phase: "submitted",
  });
  if (
    !(await actuator.transactionConfirmed(built.txHash)) ||
    (await actuator.observe(identity)) !== "cancelled"
  )
    throw new Error("outputReferenceScriptDecoding cancel did not reconcile");
  await journal.append({
    ...intent,
    sequence: entries.length + 2,
    phase: "confirmed",
  });
  return "cancelled";
};
