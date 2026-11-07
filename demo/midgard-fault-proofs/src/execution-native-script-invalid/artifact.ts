/** The journaled `executionNativeScriptInvalid` workflow artifact: the retained payload,
 * the predecessor ledger and the coordinate, re-planned on every admission. */
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { checkLedgerJson, ledgerJson } from "../ledger-at.js";
import { reconstructDaPayload } from "../transition-trace/reconstruct.js";
import type { CanonicalBlockClassification } from "../workflow/classification.js";
import {
  type CompleteCanonicalReplayContext,
  completeCanonicalReplayPredecessorLedger,
} from "../workflow/complete-replay.js";
import {
  type JournalJsonObject,
  normalizeJournalJson,
} from "../workflow/journal.js";
import {
  detectExecutionNativeScriptInvalidReplay,
  type ExecutionNativeScriptInvalidCoordinate,
  prepareExecutionNativeScriptInvalidReplay,
} from "./replay.js";

export const EXECUTION_NATIVE_SCRIPT_INVALID_ARTIFACT =
  "midgard-execution-native-script-invalid-workflow-artifact-v1";

const HEX = /^(?:[0-9a-f]{2})+$/u;

const admitCoordinate = (
  value: unknown,
): ExecutionNativeScriptInvalidCoordinate => {
  const c = value as JournalJsonObject;
  if (
    typeof c !== "object" ||
    c === null ||
    Array.isArray(c) ||
    Object.keys(c).sort().join(",") !== "sourceIndex,sourceKind" ||
    (c.sourceKind !== 0 && c.sourceKind !== 1) ||
    typeof c.sourceIndex !== "number" ||
    !Number.isSafeInteger(c.sourceIndex) ||
    c.sourceIndex < 0
  )
    throw new Error("executionNativeScriptInvalid invalid coordinate");
  return { sourceKind: c.sourceKind, sourceIndex: c.sourceIndex };
};

export const admitExecutionNativeScriptInvalidWorkflowArtifact = async (
  value: JournalJsonObject,
) => {
  if (
    Object.keys(value).sort().join(",") !==
      "coordinate,detectionId,headerHash,payloadEnvelopeCbor,predecessorLedger,schemaVersion" ||
    value.schemaVersion !== EXECUTION_NATIVE_SCRIPT_INVALID_ARTIFACT
  )
    throw new Error("executionNativeScriptInvalid artifact shape changed");
  for (const key of ["headerHash", "payloadEnvelopeCbor"] as const)
    if (typeof value[key] !== "string" || !HEX.test(value[key]))
      throw new Error(`executionNativeScriptInvalid invalid ${key}`);
  const current = await reconstructDaPayload({
    payloadEnvelopeCbor: Buffer.from(
      value.payloadEnvelopeCbor as string,
      "hex",
    ),
    expectedHeaderHash: value.headerHash as string,
  });
  const predecessor =
    value.predecessorLedger === null
      ? undefined
      : await checkLedgerJson({
          value: value.predecessorLedger,
          utxosRoot: current.header.prevUtxosRoot,
          label: "executionNativeScriptInvalid predecessor ledger",
        });
  const prepared = await prepareExecutionNativeScriptInvalidReplay({
    current,
    coordinate: admitCoordinate(value.coordinate),
    ...(predecessor === undefined ? {} : { predecessor }),
  });
  if (prepared === null)
    throw new Error(
      "executionNativeScriptInvalid artifact does not prove a fault",
    );
  if (prepared.detectionId !== value.detectionId)
    throw new Error("executionNativeScriptInvalid detection identity changed");
  return { ...prepared, current };
};

export type AdmittedExecutionNativeScriptInvalidArtifact = Awaited<
  ReturnType<typeof admitExecutionNativeScriptInvalidWorkflowArtifact>
>;

export const prepareExecutionNativeScriptInvalidWorkflowArtifact = async ({
  evidence,
  classification,
  replayContext,
}: {
  readonly replayContext?: CompleteCanonicalReplayContext;
  readonly evidence: CanonicalBlockEvidence;
  readonly classification: Extract<
    CanonicalBlockClassification,
    { decision: "fault_detected" }
  >;
}): Promise<JournalJsonObject> => {
  const predecessor = completeCanonicalReplayPredecessorLedger({
    evidence,
    context: replayContext,
  });
  const findings = await detectExecutionNativeScriptInvalidReplay({
    block: evidence,
    ...(predecessor === undefined ? {} : { predecessor }),
  });
  const finding = findings.find(
    (item) => item.detectionId === classification.selected.detectionId,
  );
  if (
    finding === undefined ||
    classification.category !== "executionNativeScriptInvalid" ||
    classification.headerHash !== evidence.headerHash
  )
    throw new Error("executionNativeScriptInvalid classification mismatch");
  const artifact = normalizeJournalJson({
    schemaVersion: EXECUTION_NATIVE_SCRIPT_INVALID_ARTIFACT,
    headerHash: evidence.headerHash,
    detectionId: finding.detectionId,
    coordinate: finding.prepared.coordinate,
    payloadEnvelopeCbor:
      evidence.reconstruction.payloadEnvelopeCbor.toString("hex"),
    predecessorLedger:
      predecessor === undefined ? null : ledgerJson(predecessor),
  }) as JournalJsonObject;
  await admitExecutionNativeScriptInvalidWorkflowArtifact(artifact);
  return artifact;
};
