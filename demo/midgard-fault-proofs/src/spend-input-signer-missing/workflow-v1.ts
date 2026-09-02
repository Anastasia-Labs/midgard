import { createHash } from "node:crypto";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { SpendInputSignerMissingEvidenceV1 } from "./spend-input-signer-missing-v1.js";

export const SPEND_INPUT_SIGNER_STAGES_V1 = [
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
export type SpendInputSignerStageV1 =
  (typeof SPEND_INPUT_SIGNER_STAGES_V1)[number];
export type SpendInputSignerActionV1 =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "submitScan"
  | "submitStep05"
  | "cancel"
  | "removeDescendants"
  | "done";
type SubmitAction = Exclude<SpendInputSignerActionV1, "done">;

export type SpendInputSignerJournalEntryV1 = Readonly<{
  sequence: number;
  identity: string;
  sourceStage: SpendInputSignerStageV1;
  targetStage: SpendInputSignerStageV1;
  action: SubmitAction;
  phase: "intent" | "submitted" | "confirmed";
  txHash: string;
}>;
export interface SpendInputSignerJournalV1 {
  load(identity: string): Promise<readonly SpendInputSignerJournalEntryV1[]>;
  append(entry: SpendInputSignerJournalEntryV1): Promise<void>;
}
export interface SpendInputSignerActuatorV1 {
  observe(identity: string): Promise<SpendInputSignerStageV1>;
  build(input: {
    readonly action: SubmitAction;
    readonly evidence: SpendInputSignerMissingEvidenceV1;
    readonly lease?: StateQueueMutationLeaseCoordinator;
  }): Promise<
    Readonly<{
      txHash: string;
      targetStage: SpendInputSignerStageV1;
      submit(): Promise<string>;
    }>
  >;
  transactionConfirmed(txHash: string): Promise<boolean>;
}

export const spendInputSignerWorkflowEvidenceIdentityV1 = (
  evidence: SpendInputSignerMissingEvidenceV1,
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

export const nextSpendInputSignerActionV1 = (
  stage: SpendInputSignerStageV1,
): SpendInputSignerActionV1 => {
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
  claimed: SpendInputSignerStageV1,
): SpendInputSignerStageV1 => {
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
  entries: readonly SpendInputSignerJournalEntryV1[],
): SpendInputSignerJournalEntryV1 | undefined =>
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
export const runSpendInputSignerMissingWorkflowV1 = async ({
  evidence,
  journal,
  actuator,
  removalLease,
}: {
  readonly evidence: SpendInputSignerMissingEvidenceV1;
  readonly journal: SpendInputSignerJournalV1;
  readonly actuator: SpendInputSignerActuatorV1;
  readonly removalLease?: StateQueueMutationLeaseCoordinator;
}): Promise<SpendInputSignerStageV1> => {
  const identity = spendInputSignerWorkflowEvidenceIdentityV1(evidence);
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
    const action = nextSpendInputSignerActionV1(observed);
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
    const intent: SpendInputSignerJournalEntryV1 = {
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
