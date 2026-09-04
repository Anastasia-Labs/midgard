import { createHash } from "node:crypto";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ProtectedOutputSignerMissingEvidence } from "./protected-output-signer-missing.js";

export const PROTECTED_OUTPUT_SIGNER_STAGES = [
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
export type ProtectedOutputSignerStage =
  (typeof PROTECTED_OUTPUT_SIGNER_STAGES)[number];
export type ProtectedOutputSignerAction =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "submitScan"
  | "submitStep05"
  | "cancel"
  | "removeDescendants"
  | "done";

type SubmitAction = Exclude<ProtectedOutputSignerAction, "done">;
export type ProtectedOutputSignerJournalEntry = Readonly<{
  sequence: number;
  identity: string;
  sourceStage: ProtectedOutputSignerStage;
  targetStage: ProtectedOutputSignerStage;
  action: SubmitAction;
  phase: "intent" | "submitted" | "confirmed";
  txHash: string;
}>;
export interface ProtectedOutputSignerJournal {
  load(identity: string): Promise<readonly ProtectedOutputSignerJournalEntry[]>;
  append(entry: ProtectedOutputSignerJournalEntry): Promise<void>;
}
export interface ProtectedOutputSignerActuator {
  observe(identity: string): Promise<ProtectedOutputSignerStage>;
  build(input: {
    readonly action: SubmitAction;
    readonly evidence: ProtectedOutputSignerMissingEvidence;
    readonly lease?: StateQueueMutationLeaseCoordinator;
  }): Promise<
    Readonly<{
      txHash: string;
      targetStage: ProtectedOutputSignerStage;
      submit(): Promise<string>;
    }>
  >;
  transactionConfirmed(txHash: string): Promise<boolean>;
}

export const protectedOutputSignerEvidenceIdentity = (
  evidence: ProtectedOutputSignerMissingEvidence,
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

export const nextProtectedOutputSignerAction = (
  stage: ProtectedOutputSignerStage,
): ProtectedOutputSignerAction => {
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
  claimed: ProtectedOutputSignerStage,
): ProtectedOutputSignerStage => {
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
  entries: readonly ProtectedOutputSignerJournalEntry[],
): ProtectedOutputSignerJournalEntry | undefined =>
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
export const runProtectedOutputSignerMissingWorkflow = async ({
  evidence,
  journal,
  actuator,
  removalLease,
}: {
  readonly evidence: ProtectedOutputSignerMissingEvidence;
  readonly journal: ProtectedOutputSignerJournal;
  readonly actuator: ProtectedOutputSignerActuator;
  readonly removalLease?: StateQueueMutationLeaseCoordinator;
}): Promise<ProtectedOutputSignerStage> => {
  const identity = protectedOutputSignerEvidenceIdentity(evidence);
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
    const action = nextProtectedOutputSignerAction(observed);
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
    const intent: ProtectedOutputSignerJournalEntry = {
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
