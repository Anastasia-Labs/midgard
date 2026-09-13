import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { reconstructDaPayload } from "../transition-trace/reconstruct.js";
import type { CanonicalBlockClassification } from "../workflow/classification.js";
import {
  type CompleteCanonicalReplayContext,
  completeCanonicalReplayPredecessorEvidence,
} from "../workflow/complete-replay.js";
import {
  type JournalJsonObject,
  normalizeJournalJson,
} from "../workflow/journal.js";
import {
  detectWithdrawalMistagReplay,
  prepareWithdrawalMistagReplay,
  withdrawalMistagDetectionId,
} from "./replay.js";

export const WITHDRAWAL_MISTAG_ARTIFACT =
  "midgard-withdrawal-mistag-workflow-artifact-v1";
export const admitWithdrawalMistagWorkflowArtifact = async (
  value: JournalJsonObject,
) => {
  if (
    Object.keys(value).sort().join(",") !==
      "detectionId,headerHash,index,payloadEnvelopeCbor,predecessorEnvelopeCbor,schemaVersion" ||
    value.schemaVersion !== WITHDRAWAL_MISTAG_ARTIFACT
  )
    throw new Error("withdrawalMistag artifact shape changed");
  for (const key of ["headerHash", "payloadEnvelopeCbor"] as const)
    if (
      typeof value[key] !== "string" ||
      !/^(?:[0-9a-f]{2})+$/u.test(value[key])
    )
      throw new Error(`withdrawalMistag invalid ${key}`);
  const current = await reconstructDaPayload({
    payloadEnvelopeCbor: Buffer.from(
      value.payloadEnvelopeCbor as string,
      "hex",
    ),
    expectedHeaderHash: value.headerHash as string,
  });
  if (
    value.predecessorEnvelopeCbor !== null &&
    (typeof value.predecessorEnvelopeCbor !== "string" ||
      !/^(?:[0-9a-f]{2})+$/u.test(value.predecessorEnvelopeCbor))
  )
    throw new Error("withdrawalMistag invalid predecessor envelope");
  const predecessor =
    value.predecessorEnvelopeCbor === null
      ? undefined
      : await reconstructDaPayload({
          payloadEnvelopeCbor: Buffer.from(
            value.predecessorEnvelopeCbor as string,
            "hex",
          ),
          expectedHeaderHash: current.header.prevHeaderHash,
        });
  const index = value.index;
  if (typeof index !== "number" || !Number.isSafeInteger(index) || index < 0)
    throw new Error("withdrawalMistag invalid index");
  if (withdrawalMistagDetectionId(index) !== value.detectionId)
    throw new Error("withdrawalMistag detection identity changed");
  const prepared = await prepareWithdrawalMistagReplay({
    current,
    predecessor,
    index,
  });
  if (prepared === null)
    throw new Error("withdrawalMistag artifact does not prove a fault");
  return prepared;
};
export const prepareWithdrawalMistagWorkflowArtifact = async ({
  evidence,
  classification,
  replayContext,
}: {
  replayContext?: CompleteCanonicalReplayContext;
  evidence: CanonicalBlockEvidence;
  classification: Extract<
    CanonicalBlockClassification,
    { decision: "fault_detected" }
  >;
}) => {
  const predecessor = completeCanonicalReplayPredecessorEvidence({
    evidence,
    context: replayContext,
  });
  const findings = await detectWithdrawalMistagReplay({
    block: evidence,
    predecessor,
  });
  const finding = findings.find(
    (item) => item.detectionId === classification.selected.detectionId,
  );
  if (
    finding === undefined ||
    classification.category !== "withdrawalMistag" ||
    classification.headerHash !== evidence.headerHash
  )
    throw new Error("withdrawalMistag classification mismatch");
  const artifact = normalizeJournalJson({
    schemaVersion: WITHDRAWAL_MISTAG_ARTIFACT,
    headerHash: evidence.headerHash,
    detectionId: finding.detectionId,
    index: finding.index,
    payloadEnvelopeCbor:
      evidence.reconstruction.payloadEnvelopeCbor.toString("hex"),
    predecessorEnvelopeCbor:
      predecessor?.reconstruction.payloadEnvelopeCbor.toString("hex") ?? null,
  }) as JournalJsonObject;
  await admitWithdrawalMistagWorkflowArtifact(artifact);
  return artifact;
};
