import { createHash } from "node:crypto";

import type { SpendInputSignerMissingEvidence } from "./spend-input-signer-missing.js";

export const SPEND_INPUT_SIGNER_STAGES = [
  "none",
  "step01",
  "step02",
  "scanning",
  "terminal",
  "proven",
  "removed",
  "cancelled",
] as const;
export type SpendInputSignerStage = (typeof SPEND_INPUT_SIGNER_STAGES)[number];
export type SpendInputSignerAction =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitScan"
  | "submitTerminal"
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
          route: evidence.route,
          transaction: evidence.canonicalTransactionCborHex,
          priorRoot: evidence.priorRoot,
          outRef:
            evidence.resolved === undefined
              ? null
              : [
                  evidence.resolved.transactionId,
                  evidence.resolved.outputIndex,
                ],
          descriptor: evidence.resolved?.descriptorCborHex ?? null,
          credential: evidence.paymentCredentialHex ?? null,
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
    case "scanning":
      return "submitScan";
    case "terminal":
      return "submitTerminal";
    case "proven":
      return "removeDescendants";
    case "removed":
    case "cancelled":
      return "done";
  }
};
