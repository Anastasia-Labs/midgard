import { createHash } from "node:crypto";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { SpendInputSignerMissingEvidence } from "./spend-input-signer-missing.js";

export const SPEND_INPUT_SIGNER_STAGES = [
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
export type SpendInputSignerStage = (typeof SPEND_INPUT_SIGNER_STAGES)[number];
export type SpendInputSignerAction =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "submitScan"
  | "submitStep05"
  | "cancel"
  | "removeDescendants"
  | "done";
type SubmitAction = Exclude<SpendInputSignerAction, "done">;

export type SpendInputSignerJournalEntry = Readonly<{
  sequence: number;
  identity: string;
  sourceStage: SpendInputSignerStage;
  targetStage: SpendInputSignerStage;
  action: SubmitAction;
  phase: "intent" | "submitted" | "confirmed";
  txHash: string;
}>;
export interface SpendInputSignerJournal {
  load(identity: string): Promise<readonly SpendInputSignerJournalEntry[]>;
  append(entry: SpendInputSignerJournalEntry): Promise<void>;
}
export interface SpendInputSignerActuator {
  observe(identity: string): Promise<SpendInputSignerStage>;
  build(input: {
    readonly action: SubmitAction;
    readonly evidence: SpendInputSignerMissingEvidence;
    readonly lease?: StateQueueMutationLeaseCoordinator;
  }): Promise<
    Readonly<{
      txHash: string;
      targetStage: SpendInputSignerStage;
      submit(): Promise<string>;
    }>
  >;
  transactionConfirmed(txHash: string): Promise<boolean>;
}

export const spendInputSignerWorkflowEvidenceIdentity = (
  evidence: SpendInputSignerMissingEvidence,
): string =>
  createHash("sha256")
    .update(
      JSON.stringify(
        {
          category: "spendInputSignerMissing",
          subject: evidence.subject,
          inputIndex: evidence.inputIndex,
          transaction: evidence.canonicalTransactionCborHex,
          outRef: [
            evidence.resolved.transactionId,
            evidence.resolved.outputIndex,
          ],
          descriptor: evidence.resolved.descriptorCborHex,
          credential: evidence.paymentCredentialHex,
          witnesses: evidence.addressWitnessFieldPreimageHex,
        },
        (_key, value: unknown) =>
          typeof value === "bigint" ? value.toString() : value,
      ),
    )
    .digest("hex");

export const nextSpendInputSignerAction = (
  stage: SpendInputSignerStage,
): SpendInputSignerAction => {
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

const targetFor = (
  action: SubmitAction,
  claimed: SpendInputSignerStage,
): SpendInputSignerStage => {
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
        throw new Error("spendInputSignerMissing scan target is invalid");
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
  entries: readonly SpendInputSignerJournalEntry[],
): SpendInputSignerJournalEntry | undefined =>
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

/** Exact-signed-hash durable transition loop. The concrete production actuator
 * is package-owned; this narrow port exists only to test crash boundaries. */
export const runSpendInputSignerMissingWorkflow = async ({
  evidence,
  journal,
  actuator,
  removalLease,
}: {
  readonly evidence: SpendInputSignerMissingEvidence;
  readonly journal: SpendInputSignerJournal;
  readonly actuator: SpendInputSignerActuator;
  readonly removalLease?: StateQueueMutationLeaseCoordinator;
}): Promise<SpendInputSignerStage> => {
  const identity = spendInputSignerWorkflowEvidenceIdentity(evidence);
  for (;;) {
    const entries = await journal.load(identity);
    if (
      entries.some(
        (entry, index) =>
          entry.identity !== identity || entry.sequence !== index,
      )
    )
      throw new Error(
        "spendInputSignerMissing journal identity/sequence changed",
      );
    const pending = pendingIntent(entries);
    if (pending !== undefined) {
      if (!(await actuator.transactionConfirmed(pending.txHash)))
        throw new Error(
          "spendInputSignerMissing exact intended transaction is unresolved",
        );
      const observed = await actuator.observe(identity);
      if (observed !== pending.targetStage)
        throw new Error(
          "spendInputSignerMissing authenticated stage/transaction identity substitution",
        );
      await journal.append({
        ...pending,
        sequence: entries.length,
        phase: "confirmed",
      });
      continue;
    }
    const observed = await actuator.observe(identity);
    const action = nextSpendInputSignerAction(observed);
    if (action === "done") return observed;
    if (action === "removeDescendants" && removalLease === undefined)
      throw new Error(
        "spendInputSignerMissing removal requires the state-queue mutation lease",
      );
    const built = await actuator.build({
      action,
      evidence,
      ...(action === "removeDescendants" ? { lease: removalLease } : {}),
    });
    if (!/^[0-9a-f]{64}$/u.test(built.txHash))
      throw new Error(
        "spendInputSignerMissing signed transaction hash is invalid",
      );
    const intent: SpendInputSignerJournalEntry = {
      sequence: entries.length,
      identity,
      sourceStage: observed,
      targetStage: targetFor(action, built.targetStage),
      action,
      phase: "intent",
      txHash: built.txHash,
    };
    await journal.append(intent);
    const submitted = await built.submit();
    if (submitted !== built.txHash)
      throw new Error(
        "spendInputSignerMissing submitter changed exact transaction identity",
      );
    await journal.append({
      ...intent,
      sequence: intent.sequence + 1,
      phase: "submitted",
    });
  }
};
