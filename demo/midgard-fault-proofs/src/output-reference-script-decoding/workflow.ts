import { createHash } from "node:crypto";

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
