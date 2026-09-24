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
import type { TransitionDepositOpening } from "./history-opening.js";
import {
  transitionProofCbor,
  type TransitionProofInput,
} from "./proof-material.js";

export const TRANSITION_TRACE_WORKFLOW_ARTIFACT =
  "midgard-transition-trace-workflow-artifact-v1";
export const createTransitionTraceWorkflowArtifact = ({
  evidence,
  corpus,
  proof,
  detectionId,
  l1Snapshot,
  eventOutRef,
  depositOpening,
}: {
  evidence: CanonicalBlockEvidence;
  corpus: HistoricalNativeScriptCorpus;
  proof: TransitionProofInput;
  detectionId: string;
  l1Snapshot: FraudProofRawL1Snapshot;
  eventOutRef: string | null;
  depositOpening: TransitionDepositOpening | null;
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
    proofCbor: transitionProofCbor(proof),
    eventOutRef,
    depositOpening,
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
  ] as const)
    if (artifact[key] !== freshlyDerived[key])
      throw new Error(
        `Transition artifact ${key} differs from freshly admitted replay`,
      );
  const expectedOpening = freshlyDerived.depositOpening;
  if (expectedOpening === null) {
    if (
      artifact.depositOpening !== null ||
      artifact.eventOutRef !== freshlyDerived.eventOutRef
    )
      throw new Error(
        "Transition artifact event reference differs from freshly admitted replay",
      );
  } else {
    const opening = artifact.depositOpening;
    if (
      typeof opening !== "object" ||
      opening === null ||
      Array.isArray(opening) ||
      typeof expectedOpening !== "object" ||
      Array.isArray(expectedOpening) ||
      Object.keys(opening).sort().join(",") !== "commitmentCbor,openingCbor" ||
      !("commitmentCbor" in opening) ||
      !("openingCbor" in opening) ||
      !("commitmentCbor" in expectedOpening) ||
      !("openingCbor" in expectedOpening) ||
      opening.commitmentCbor !== expectedOpening.commitmentCbor ||
      opening.openingCbor !== expectedOpening.openingCbor
    )
      throw new Error(
        "Transition artifact deposit opening differs from freshly admitted replay",
      );
    // Pointer churn changes the diagnostic outRef, not captured immutable facts.
    // The on-chain stage still opens against its own authenticated commitment.
  }
  if (
    artifact.l1Snapshot === null ||
    typeof artifact.l1Snapshot !== "object" ||
    Array.isArray(artifact.l1Snapshot)
  )
    throw new Error("Transition artifact omitted its retained raw L1 snapshot");
};
