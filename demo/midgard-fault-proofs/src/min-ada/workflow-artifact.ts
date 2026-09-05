import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CanonicalBlockClassification } from "../workflow/classification.js";
import type { HistoricalNativeScriptCorpus } from "../workflow/historical-native-script-corpus.js";
import { admitMinAdaArtifact, prepareMinAdaArtifact } from "./artifact.js";
import {
  admitMinAdaForcedArtifact,
  MIN_ADA_FORCED_ARTIFACT,
  prepareMinAdaForcedArtifact,
} from "./forced-artifact.js";
export const prepareMinAdaWorkflowArtifact = async (args: {
  evidence: CanonicalBlockEvidence;
  classification: Extract<
    CanonicalBlockClassification,
    { decision: "fault_detected" }
  > & { category: "minAda" };
  historicalNativeScriptCorpus: HistoricalNativeScriptCorpus;
}) => {
  if (args.classification.selected.detectionId.startsWith("min-ada:forced:"))
    return prepareMinAdaForcedArtifact({
      block: args.evidence,
      detectionId: args.classification.selected.detectionId,
    });
  return prepareMinAdaArtifact(args);
};
export const admitMinAdaWorkflowArtifact = async (value: unknown) => {
  if (
    typeof value === "object" &&
    value !== null &&
    "schemaVersion" in value &&
    value.schemaVersion === MIN_ADA_FORCED_ARTIFACT
  ) {
    const plan = await admitMinAdaForcedArtifact(value);
    return {
      artifact: {
        kind: "min-ada-forced" as const,
        headerHash: plan.headerHash,
      },
      prepared: plan.evidence,
      forcedSource: plan.forcedSource,
    };
  }
  return admitMinAdaArtifact(value);
};
