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
