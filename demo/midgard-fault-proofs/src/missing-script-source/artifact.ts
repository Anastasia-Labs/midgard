/** The journaled `missingScriptSource` workflow artifact: the retained payload,
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
  detectMissingScriptSourceReplay,
  type MissingScriptSourceCoordinate,
  prepareMissingScriptSourceReplay,
} from "./replay.js";

export const MISSING_SCRIPT_SOURCE_ARTIFACT =
  "midgard-missing-script-source-workflow-artifact-v1";

const HEX = /^(?:[0-9a-f]{2})+$/u;

const admitCoordinate = (value: unknown): MissingScriptSourceCoordinate => {
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
    throw new Error("missingScriptSource invalid coordinate");
  return { sourceKind: c.sourceKind, sourceIndex: c.sourceIndex };
};

export const admitMissingScriptSourceWorkflowArtifact = async (
  value: JournalJsonObject,
) => {
  if (
    Object.keys(value).sort().join(",") !==
      "coordinate,detectionId,headerHash,payloadEnvelopeCbor,predecessorLedger,schemaVersion" ||
    value.schemaVersion !== MISSING_SCRIPT_SOURCE_ARTIFACT
  )
    throw new Error("missingScriptSource artifact shape changed");
  for (const key of ["headerHash", "payloadEnvelopeCbor"] as const)
    if (typeof value[key] !== "string" || !HEX.test(value[key]))
      throw new Error(`missingScriptSource invalid ${key}`);
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
          label: "missingScriptSource predecessor ledger",
        });
  const prepared = await prepareMissingScriptSourceReplay({
    current,
    coordinate: admitCoordinate(value.coordinate),
    ...(predecessor === undefined ? {} : { predecessor }),
  });
  if (prepared === null)
    throw new Error("missingScriptSource artifact does not prove a fault");
  if (prepared.detectionId !== value.detectionId)
    throw new Error("missingScriptSource detection identity changed");
  return { ...prepared, current };
};

export type AdmittedMissingScriptSourceArtifact = Awaited<
  ReturnType<typeof admitMissingScriptSourceWorkflowArtifact>
>;

export const prepareMissingScriptSourceWorkflowArtifact = async ({
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
  const findings = await detectMissingScriptSourceReplay({
    block: evidence,
    ...(predecessor === undefined ? {} : { predecessor }),
  });
  const finding = findings.find(
    (item) => item.detectionId === classification.selected.detectionId,
  );
  if (
    finding === undefined ||
    classification.category !== "missingScriptSource" ||
    classification.headerHash !== evidence.headerHash
  )
    throw new Error("missingScriptSource classification mismatch");
  const artifact = normalizeJournalJson({
    schemaVersion: MISSING_SCRIPT_SOURCE_ARTIFACT,
    headerHash: evidence.headerHash,
    detectionId: finding.detectionId,
    coordinate: finding.prepared.coordinate,
    payloadEnvelopeCbor:
      evidence.reconstruction.payloadEnvelopeCbor.toString("hex"),
    predecessorLedger:
      predecessor === undefined ? null : ledgerJson(predecessor),
  }) as JournalJsonObject;
  await admitMissingScriptSourceWorkflowArtifact(artifact);
  return artifact;
};
