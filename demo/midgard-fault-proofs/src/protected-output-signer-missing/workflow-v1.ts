import { createHash } from "node:crypto";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ProtectedOutputSignerMissingEvidenceV1 } from "./protected-output-signer-missing-v1.js";

export const PROTECTED_OUTPUT_SIGNER_STAGES_V1 = [
  "none",
  "step01",
  "step02",
  "step03",
  "scanning",
  "step05",
  "proven",
  "removed",
  "cancelled",
] as const;
export type ProtectedOutputSignerStageV1 =
  (typeof PROTECTED_OUTPUT_SIGNER_STAGES_V1)[number];
export type ProtectedOutputSignerActionV1 =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "submitScan"
  | "submitStep05"
  | "cancel"
  | "removeDescendants"
  | "done";

type SubmitAction = Exclude<ProtectedOutputSignerActionV1, "done">;
export type ProtectedOutputSignerJournalEntryV1 = Readonly<{
  sequence: number;
  identity: string;
  sourceStage: ProtectedOutputSignerStageV1;
  targetStage: ProtectedOutputSignerStageV1;
  action: SubmitAction;
  phase: "intent" | "submitted" | "confirmed";
  txHash: string;
}>;
export interface ProtectedOutputSignerJournalV1 {
  load(
    identity: string,
  ): Promise<readonly ProtectedOutputSignerJournalEntryV1[]>;
  append(entry: ProtectedOutputSignerJournalEntryV1): Promise<void>;
}
export interface ProtectedOutputSignerActuatorV1 {
  observe(identity: string): Promise<ProtectedOutputSignerStageV1>;
  build(input: {
    readonly action: SubmitAction;
    readonly evidence: ProtectedOutputSignerMissingEvidenceV1;
    readonly lease?: StateQueueMutationLeaseCoordinator;
  }): Promise<
    Readonly<{
      txHash: string;
      targetStage: ProtectedOutputSignerStageV1;
      submit(): Promise<string>;
    }>
  >;
  transactionConfirmed(txHash: string): Promise<boolean>;
}

export const protectedOutputSignerEvidenceIdentityV1 = (
  evidence: ProtectedOutputSignerMissingEvidenceV1,
): string =>
  createHash("sha256")
    .update(
      JSON.stringify(
        {
          category: "protectedOutputSignerMissing",
          subject: evidence.subject,
          outputIndex: evidence.outputIndex,
          tx: evidence.canonicalTransactionCborHex,
          output: evidence.outputCborHex,
          credential: evidence.paymentCredentialHex,
          witnesses: evidence.addressWitnessFieldPreimageHex,
        },
        (_key, value: unknown) =>
          typeof value === "bigint" ? value.toString() : value,
      ),
    )
    .digest("hex");

export const nextProtectedOutputSignerActionV1 = (
  stage: ProtectedOutputSignerStageV1,
): ProtectedOutputSignerActionV1 => {
  switch (stage) {
    case "none":
      return "submitInit";
    case "step01":
      return "submitStep01";
    case "step02":
      return "submitStep02";
    case "step03":
      return "submitStep03";
    case "scanning":
      return "submitScan";
    case "step05":
      return "submitStep05";
    case "proven":
      return "removeDescendants";
    case "removed":
    case "cancelled":
      return "done";
  }
};

const requiredTargetStage = (
  action: SubmitAction,
  claimed: ProtectedOutputSignerStageV1,
): ProtectedOutputSignerStageV1 => {
  switch (action) {
    case "submitInit":
      return "step01";
    case "submitStep01":
      return "step02";
    case "submitStep02":
      return "step03";
    case "submitStep03":
      return "scanning";
    case "submitScan":
      if (claimed !== "scanning" && claimed !== "step05")
        throw new Error("protectedOutputSignerMissing scan target is invalid");
      return claimed;
    case "submitStep05":
      return "proven";
    case "cancel":
      return "cancelled";
    case "removeDescendants":
      return "removed";
  }
};

const pendingIntent = (
  entries: readonly ProtectedOutputSignerJournalEntryV1[],
): ProtectedOutputSignerJournalEntryV1 | undefined =>
  [...entries]
    .reverse()
    .find(
      (candidate) =>
        candidate.phase === "intent" &&
        !entries.some(
          (later) =>
            later.sequence > candidate.sequence &&
            later.action === candidate.action &&
            later.txHash === candidate.txHash &&
            later.phase === "confirmed",
        ),
    );

/** Pre-submit-intent workflow with exact-hash restart reconciliation. */
export const runProtectedOutputSignerMissingWorkflowV1 = async ({
  evidence,
  journal,
  actuator,
  removalLease,
}: {
  readonly evidence: ProtectedOutputSignerMissingEvidenceV1;
  readonly journal: ProtectedOutputSignerJournalV1;
  readonly actuator: ProtectedOutputSignerActuatorV1;
  readonly removalLease?: StateQueueMutationLeaseCoordinator;
}): Promise<ProtectedOutputSignerStageV1> => {
  const identity = protectedOutputSignerEvidenceIdentityV1(evidence);
  for (;;) {
    const entries = await journal.load(identity);
    if (
      entries.some(
        (entry, index) =>
          entry.identity !== identity || entry.sequence !== index,
      )
    )
      throw new Error(
        "protectedOutputSignerMissing journal identity/sequence changed",
      );
    const pending = pendingIntent(entries);
    if (pending !== undefined) {
      if (!(await actuator.transactionConfirmed(pending.txHash)))
        throw new Error(
          "protectedOutputSignerMissing exact intended transaction is unresolved",
        );
      const observed = await actuator.observe(identity);
      if (observed !== pending.targetStage)
        throw new Error(
          "protectedOutputSignerMissing authenticated stage/transaction identity substitution",
        );
      await journal.append({
        ...pending,
        sequence: entries.length,
        phase: "confirmed",
      });
      continue;
    }
    const observed = await actuator.observe(identity);
    const action = nextProtectedOutputSignerActionV1(observed);
    if (action === "done") return observed;
    if (action === "removeDescendants" && removalLease === undefined)
      throw new Error(
        "protectedOutputSignerMissing removal requires the state-queue mutation lease",
      );
    const built = await actuator.build({
      action,
      evidence,
      ...(action === "removeDescendants" ? { lease: removalLease } : {}),
    });
    if (!/^[0-9a-f]{64}$/u.test(built.txHash))
      throw new Error(
        "protectedOutputSignerMissing signed transaction hash is invalid",
      );
    const nextSequence = entries.length;
    const intent: ProtectedOutputSignerJournalEntryV1 = {
      sequence: nextSequence,
      identity,
      sourceStage: observed,
      targetStage: requiredTargetStage(action, built.targetStage),
      action,
      phase: "intent",
      txHash: built.txHash,
    };
    await journal.append(intent);
    const submitted = await built.submit();
    if (submitted !== built.txHash)
      throw new Error(
        "protectedOutputSignerMissing submitter changed exact transaction identity",
      );
    await journal.append({
      ...intent,
      sequence: nextSequence + 1,
      phase: "submitted",
    });
  }
};
