export const RESOLVED_OUTPUT_STAGES = [
  "none",
  "step01",
  "step02",
  "step03",
  "reconstructing",
  "step04Datum",
  "step05",
  "proven",
  "removed",
  "cancelled",
] as const;
export type ResolvedOutputStage = (typeof RESOLVED_OUTPUT_STAGES)[number];
export type ResolvedOutputAction =
  | "submitInit"
  | "submitStep01"
  | "submitStep02"
  | "submitStep03"
  | "submitReconstruction"
  | "submitStep04Datum"
  | "submitStep05"
  | "removeDescendants"
  | "done";

export type ResolvedOutputJournalEntry = Readonly<{
  sequence: number;
  identity: string;
  stage: ResolvedOutputStage;
  action?: Exclude<ResolvedOutputAction, "done">;
  phase?: "intent" | "submitted" | "confirmed";
  txHash: string;
  outputReference: string | null;
}>;
export type ResolvedOutputJournal = Readonly<{
  load(identity: string): Promise<readonly ResolvedOutputJournalEntry[]>;
  append(entry: ResolvedOutputJournalEntry): Promise<void>;
}>;

export const nextResolvedOutputAction = (
  stage: ResolvedOutputStage,
): ResolvedOutputAction => {
  switch (stage) {
    case "none":
      return "submitInit";
    case "step01":
      return "submitStep01";
    case "step02":
      return "submitStep02";
    case "step03":
      return "submitStep03";
    case "reconstructing":
      return "submitReconstruction";
    case "step04Datum":
      return "submitStep04Datum";
    case "step05":
      return "submitStep05";
    case "proven":
      return "removeDescendants";
    case "removed":
    case "cancelled":
      return "done";
  }
};
