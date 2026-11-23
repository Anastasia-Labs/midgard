import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { ledgerFromWitness, recordWitness } from "../ledger-store/witness.js";
import {
  ledgerWitnessJson,
  parseLedgerWitnessJson,
} from "../ledger-store/witness-json.js";
import { reconstructDaPayload } from "../transition-trace/reconstruct.reconstruct-da-payload.js";
import type { CanonicalBlockClassification } from "../workflow/classification.js";
import { completeCanonicalReplayPredecessorStoreLedger } from "../workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import type { CompleteCanonicalReplayContext } from "../workflow/complete-replay.replay-context-identity.js";
import {
  normalizeWorkflowJson,
  type WorkflowJsonObject,
} from "../workflow/identity.js";
import {
  detectWithdrawalMistagReplay,
  prepareWithdrawalMistagReplay,
  withdrawalMistagDetectionId,
} from "./replay.js";

export const WITHDRAWAL_MISTAG_ARTIFACT =
  "midgard-withdrawal-mistag-workflow-artifact-v3";
export const admitWithdrawalMistagWorkflowArtifact = async (
  value: WorkflowJsonObject,
) => {
  if (
    Object.keys(value).sort().join(",") !==
      "detectionId,headerHash,index,payloadEnvelopeCbor,predecessorWitness,schemaVersion" ||
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
  // The witness opens the root the authenticated header commits, never one
  // the artifact names.
  const predecessor =
    value.predecessorWitness === null
      ? undefined
      : ledgerFromWitness(
          current.header.prevUtxosRoot,
          parseLedgerWitnessJson(
            value.predecessorWitness,
            "withdrawalMistag predecessor witness",
          ),
        );
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
  const predecessor = await completeCanonicalReplayPredecessorStoreLedger({
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
  // Prepare the selected finding again over a recording ledger: the witness
  // holds what this preparation reads, which is what admission reads.
  const recording =
    predecessor === undefined ? undefined : recordWitness(predecessor);
  await prepareWithdrawalMistagReplay({
    current: evidence.reconstruction,
    predecessor: recording?.ledger,
    index: finding.index,
  });
  return normalizeWorkflowJson({
    schemaVersion: WITHDRAWAL_MISTAG_ARTIFACT,
    headerHash: evidence.headerHash,
    detectionId: finding.detectionId,
    index: finding.index,
    payloadEnvelopeCbor:
      evidence.reconstruction.payloadEnvelopeCbor.toString("hex"),
    predecessorWitness:
      recording === undefined ? null : ledgerWitnessJson(recording.witness()),
  }) as WorkflowJsonObject;
};
