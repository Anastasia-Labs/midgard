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
