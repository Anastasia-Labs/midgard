import { createHash } from "node:crypto";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { OutputReferenceScriptDecodingEvidenceV1 } from "./output-reference-script-decoding-v1.js";

export const OUTPUT_REFERENCE_SCRIPT_DECODING_STAGES_V1 = [
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
export type OutputReferenceScriptDecodingStageV1 =
  (typeof OUTPUT_REFERENCE_SCRIPT_DECODING_STAGES_V1)[number];
export type OutputReferenceScriptDecodingActionV1 =
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

type SubmitAction = Exclude<OutputReferenceScriptDecodingActionV1, "done">;
export type OutputReferenceScriptDecodingJournalEntryV1 = Readonly<{
  sequence: number;
  identity: string;
  sourceStage: OutputReferenceScriptDecodingStageV1;
  targetStage: OutputReferenceScriptDecodingStageV1;
  action: SubmitAction;
  phase: "intent" | "submitted" | "confirmed";
  txHash: string;
}>;
export interface OutputReferenceScriptDecodingJournalV1 {
  load(
    identity: string,
  ): Promise<readonly OutputReferenceScriptDecodingJournalEntryV1[]>;
  append(entry: OutputReferenceScriptDecodingJournalEntryV1): Promise<void>;
}
export interface OutputReferenceScriptDecodingActuatorV1 {
  observe(identity: string): Promise<OutputReferenceScriptDecodingStageV1>;
  build(input: {
    action: SubmitAction;
    evidence: OutputReferenceScriptDecodingEvidenceV1;
    lease?: StateQueueMutationLeaseCoordinator;
  }): Promise<{
    txHash: string;
    targetStage: OutputReferenceScriptDecodingStageV1;
    submit(): Promise<string>;
  }>;
  transactionConfirmed(txHash: string): Promise<boolean>;
}

export const outputReferenceScriptDecodingEvidenceIdentityV1 = (
  evidence: OutputReferenceScriptDecodingEvidenceV1,
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

export const nextOutputReferenceScriptDecodingActionV1 = (
  stage: OutputReferenceScriptDecodingStageV1,
): OutputReferenceScriptDecodingActionV1 => {
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
  target: OutputReferenceScriptDecodingStageV1,
): boolean => {
  const allowed: Record<
    SubmitAction,
    readonly OutputReferenceScriptDecodingStageV1[]
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
  entries: readonly OutputReferenceScriptDecodingJournalEntryV1[],
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

export const runOutputReferenceScriptDecodingWorkflowV1 = async ({
  evidence,
  journal,
  actuator,
  removalLease,
}: {
  evidence: OutputReferenceScriptDecodingEvidenceV1;
  journal: OutputReferenceScriptDecodingJournalV1;
  actuator: OutputReferenceScriptDecodingActuatorV1;
  removalLease?: StateQueueMutationLeaseCoordinator;
}): Promise<OutputReferenceScriptDecodingStageV1> => {
  const identity = outputReferenceScriptDecodingEvidenceIdentityV1(evidence);
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
    const action = nextOutputReferenceScriptDecodingActionV1(sourceStage);
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
    const intent: OutputReferenceScriptDecodingJournalEntryV1 = {
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

export const cancelOutputReferenceScriptDecodingWorkflowV1 = async ({
  evidence,
  journal,
  actuator,
}: {
  evidence: OutputReferenceScriptDecodingEvidenceV1;
  journal: OutputReferenceScriptDecodingJournalV1;
  actuator: OutputReferenceScriptDecodingActuatorV1;
}): Promise<"cancelled"> => {
  const identity = outputReferenceScriptDecodingEvidenceIdentityV1(evidence);
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
  const intent: OutputReferenceScriptDecodingJournalEntryV1 = {
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
