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
  detectNativeScriptDecodingReplay,
  type NativeScriptDecodingCoordinate,
  nativeScriptDecodingDetectionId,
  prepareNativeScriptDecodingReplay,
} from "./replay.js";

export const NATIVE_SCRIPT_DECODING_ARTIFACT =
  "midgard-native-script-decoding-workflow-artifact-v1";
export const admitNativeScriptDecodingWorkflowArtifact = async (
  value: JournalJsonObject,
) => {
  if (
    Object.keys(value).sort().join(",") !==
      "coordinate,detectionId,headerHash,payloadEnvelopeCbor,predecessorEnvelopeCbor,schemaVersion" ||
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
  if (
    value.predecessorEnvelopeCbor !== null &&
    (typeof value.predecessorEnvelopeCbor !== "string" ||
      !/^(?:[0-9a-f]{2})+$/u.test(value.predecessorEnvelopeCbor))
  )
    throw new Error("nativeScriptDecoding invalid predecessor envelope");
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
  const rawCoordinate = value.coordinate;
  const c = rawCoordinate as JournalJsonObject;
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
  const predecessor = completeCanonicalReplayPredecessorEvidence({
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
  const artifact = normalizeJournalJson({
    schemaVersion: NATIVE_SCRIPT_DECODING_ARTIFACT,
    headerHash: evidence.headerHash,
    detectionId: finding.detectionId,
    coordinate: finding.prepared.coordinate,
    payloadEnvelopeCbor:
      evidence.reconstruction.payloadEnvelopeCbor.toString("hex"),
    predecessorEnvelopeCbor:
      predecessor?.reconstruction.payloadEnvelopeCbor.toString("hex") ?? null,
  }) as JournalJsonObject;
  await admitNativeScriptDecodingWorkflowArtifact(artifact);
  return artifact;
};
