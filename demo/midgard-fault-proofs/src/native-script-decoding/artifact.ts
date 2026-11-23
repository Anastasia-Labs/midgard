import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";

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
  detectNativeScriptDecodingReplay,
  type NativeScriptDecodingCoordinate,
  nativeScriptDecodingDetectionId,
  prepareNativeScriptDecodingReplay,
} from "./replay.js";

export const NATIVE_SCRIPT_DECODING_ARTIFACT =
  "midgard-native-script-decoding-workflow-artifact-v3";
export const admitNativeScriptDecodingWorkflowArtifact = async (
  value: WorkflowJsonObject,
) => {
  if (
    Object.keys(value).sort().join(",") !==
      "coordinate,detectionId,headerHash,payloadEnvelopeCbor,predecessorWitness,schemaVersion" ||
    value.schemaVersion !== NATIVE_SCRIPT_DECODING_ARTIFACT
  )
    throw new Error("nativeScriptDecoding artifact shape changed");
  for (const key of ["headerHash", "payloadEnvelopeCbor"] as const)
    if (
      typeof value[key] !== "string" ||
      !/^(?:[0-9a-f]{2})+$/u.test(value[key])
    )
      throw new Error(`nativeScriptDecoding invalid ${key}`);
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
            "nativeScriptDecoding predecessor witness",
          ),
        );
  const rawCoordinate = value.coordinate;
  const c = rawCoordinate as WorkflowJsonObject;
  if (
    typeof c !== "object" ||
    c === null ||
    Array.isArray(c) ||
    Object.keys(c).sort().join(",") !==
      "outpointCursor,outpointSourceKind,sourceIndex,sourceKind"
  )
    throw new Error("nativeScriptDecoding invalid coordinate");
  if (
    (c.sourceKind !== 0 && c.sourceKind !== 1) ||
    typeof c.sourceIndex !== "number" ||
    !Number.isSafeInteger(c.sourceIndex) ||
    c.sourceIndex < 0 ||
    typeof c.outpointSourceKind !== "string" ||
    !/^(?:0|-?[1-9][0-9]*)$/u.test(c.outpointSourceKind) ||
    typeof c.outpointCursor !== "string" ||
    !/^(?:0|-?[1-9][0-9]*)$/u.test(c.outpointCursor)
  )
    throw new Error("nativeScriptDecoding noncanonical coordinate");
  const coordinate = c as NativeScriptDecodingCoordinate;
  if (nativeScriptDecodingDetectionId(coordinate) !== value.detectionId)
    throw new Error("nativeScriptDecoding detection identity changed");
  const prepared = await prepareNativeScriptDecodingReplay({
    current,
    predecessor,
    coordinate,
  });
  if (prepared === null)
    throw new Error("nativeScriptDecoding artifact does not prove a fault");
  return prepared;
};
export const prepareNativeScriptDecodingWorkflowArtifact = async ({
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
  const findings = await detectNativeScriptDecodingReplay({
    block: evidence,
    predecessor,
  });
  const finding = findings.find(
    (item) => item.detectionId === classification.selected.detectionId,
  );
  if (
    finding === undefined ||
    classification.category !== "nativeScriptDecoding" ||
    classification.headerHash !== evidence.headerHash
  )
    throw new Error("nativeScriptDecoding classification mismatch");
  // Prepare the selected finding again over a recording ledger, and prove
  // the selected input's descriptor as the bind step does: the witness holds
  // what admission and capture read.
  const recording =
    predecessor === undefined ? undefined : recordWitness(predecessor);
  const prepared = await prepareNativeScriptDecodingReplay({
    current: evidence.reconstruction,
    predecessor: recording?.ledger,
    coordinate: finding.prepared.coordinate,
  });
  if (
    recording !== undefined &&
    prepared !== null &&
    prepared.ledgerTrie !== null
  )
    await prepared.ledgerTrie.prove(
      Buffer.from(
        SDKFraudProof.encodeMidgardTxInputCanonical(
          prepared.subjectFieldInputs[
            Number(prepared.coordinate.outpointCursor)
          ]!,
        ),
      ),
    );
  return normalizeWorkflowJson({
    schemaVersion: NATIVE_SCRIPT_DECODING_ARTIFACT,
    headerHash: evidence.headerHash,
    detectionId: finding.detectionId,
    coordinate: finding.prepared.coordinate,
    payloadEnvelopeCbor:
      evidence.reconstruction.payloadEnvelopeCbor.toString("hex"),
    predecessorWitness:
      recording === undefined ? null : ledgerWitnessJson(recording.witness()),
  }) as WorkflowJsonObject;
};
