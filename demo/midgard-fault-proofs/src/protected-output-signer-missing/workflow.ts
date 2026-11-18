import { createHash } from "node:crypto";

import type { ProtectedOutputSignerMissingEvidence } from "./protected-output-signer-missing.js";

export const PROTECTED_OUTPUT_SIGNER_STAGES = [
  "none",
  "step01",
  "step02",
  "scanning",
  "terminal",
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
  | "submitScan"
  | "submitTerminal"
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
          route: evidence.route,
          tx: evidence.canonicalTransactionCborHex,
          output: evidence.outputCborHex ?? null,
          credential: evidence.paymentCredentialHex ?? null,
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
