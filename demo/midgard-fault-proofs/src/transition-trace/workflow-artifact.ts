import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  type HistoricalNativeScriptCorpus,
  requireHistoricalNativeScriptCorpus,
} from "../workflow/historical-native-script-corpus.js";
import {
  type JournalJsonObject,
  normalizeJournalJson,
} from "../workflow/journal.js";
import type { FraudProofRawL1Snapshot } from "../workflow/raw-l1-snapshot.js";

export const TRANSITION_TRACE_WORKFLOW_ARTIFACT =
  "midgard-transition-trace-workflow-artifact-v1";
export const createTransitionTraceWorkflowArtifact = ({
  evidence,
  corpus,
  proof,
  detectionId,
  l1Snapshot,
  eventOutRef,
}: {
  evidence: CanonicalBlockEvidence;
  corpus: HistoricalNativeScriptCorpus;
  proof: SDK.TransitionFaultProof;
  detectionId: string;
  l1Snapshot: FraudProofRawL1Snapshot;
  eventOutRef: string | null;
}): JournalJsonObject => {
  const history = requireHistoricalNativeScriptCorpus(corpus);
  if (history.currentEvidence !== evidence)
    throw new Error(
      "Transition artifact history is not the current admitted evidence",
    );
  return normalizeJournalJson({
    schemaVersion: TRANSITION_TRACE_WORKFLOW_ARTIFACT,
    headerHash: evidence.headerHash,
    detectionId,
    payloadEnvelopeCbor:
      evidence.reconstruction.payloadEnvelopeCbor.toString("hex"),
    predecessorEnvelopeCbor:
      history.reconstructions.at(-2)?.payloadEnvelopeCbor.toString("hex") ??
      null,
    proofCbor: Data.to(proof, SDK.TransitionFaultProof),
    eventOutRef,
    // Durable exact raw evidence is retained for recovery/audit. It does not
    // grant authority; every run reopens fresh history and raw L1 observations.
    l1Snapshot,
  }) as JournalJsonObject;
};

export const requireTransitionTraceWorkflowArtifact = (
  artifact: JournalJsonObject,
  freshlyDerived: JournalJsonObject,
): void => {
  if (
    Object.keys(artifact).sort().join(",") !==
    Object.keys(freshlyDerived).sort().join(",")
  )
    throw new Error("Transition artifact shape changed");
  for (const key of [
    "schemaVersion",
    "headerHash",
    "detectionId",
    "payloadEnvelopeCbor",
    "predecessorEnvelopeCbor",
    "proofCbor",
    "eventOutRef",
  ] as const)
    if (artifact[key] !== freshlyDerived[key])
      throw new Error(
        `Transition artifact ${key} differs from freshly admitted replay`,
      );
  if (
    artifact.l1Snapshot === null ||
    typeof artifact.l1Snapshot !== "object" ||
    Array.isArray(artifact.l1Snapshot)
  )
    throw new Error("Transition artifact omitted its retained raw L1 snapshot");
};
