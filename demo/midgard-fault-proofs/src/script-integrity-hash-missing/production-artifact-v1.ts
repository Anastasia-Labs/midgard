import { createHash } from "node:crypto";

import {
  decodeMidgardNativeTxCompactV1,
  decodeMidgardNativeTxWitnessSetCompactV1,
} from "@al-ft/midgard-core";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import {
  type FaultProofFieldOpeningPlanV1,
  planFaultProofFieldOpeningV1,
} from "../field-opening-v1.js";
import type { CanonicalBlockClassificationV1 } from "../workflow/classification-v1.js";
import {
  type JournalJsonObjectV1,
  normalizeJournalJsonV1,
} from "../workflow/journal-v1.js";
import {
  prepareScriptIntegrityHashMissingEvidenceV1,
  type ScriptIntegrityHashMissingEvidenceV1,
} from "./family-v1.js";
import {
  deriveScriptIntegrityHashMissingAuthenticatedSourceV1,
  detectScriptIntegrityHashMissingFromCanonicalEvidenceV1,
  reconstructScriptIntegrityHashMissingEvidenceV1,
  type ScriptIntegrityHashMissingAuthenticatedSourceV1,
} from "./replay-v1.js";
import { planScriptIntegrityHashMissingStagedWalkV1 } from "./staged-plan-v1.js";

export const PRODUCTION_SCRIPT_INTEGRITY_HASH_MISSING_ARTIFACT_V1 =
  "midgard-production-script-integrity-hash-missing-artifact-v1" as const;

type Portable =
  | null
  | boolean
  | number
  | string
  | Portable[]
  | { [key: string]: Portable };

const portable = (value: unknown): Portable => {
  if (typeof value === "bigint") return { $bigint: value.toString() };
  if (Buffer.isBuffer(value)) return { $bytes: value.toString("hex") };
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "number" ||
    typeof value === "string"
  )
    return value;
  if (Array.isArray(value)) return value.map(portable);
  if (typeof value === "object")
    return Object.fromEntries(
      Object.entries(value).map(([key, child]) => [key, portable(child)]),
    );
  throw new Error(
    "scriptIntegrityHashMissing artifact contains a non-portable value",
  );
};

const restore = (value: Portable): unknown => {
  if (Array.isArray(value)) return value.map(restore);
  if (value !== null && typeof value === "object") {
    const keys = Object.keys(value);
    if (keys.length === 1 && typeof value.$bigint === "string")
      return BigInt(value.$bigint);
    if (
      keys.length === 1 &&
      typeof value.$bytes === "string" &&
      /^(?:[0-9a-f]{2})*$/u.test(value.$bytes)
    )
      return Buffer.from(value.$bytes, "hex");
    return Object.fromEntries(
      Object.entries(value).map(([key, child]) => [key, restore(child)]),
    );
  }
  return value;
};

const encodePortable = (value: unknown): string =>
  JSON.stringify(portable(value));
const decodePortable = (value: string): unknown =>
  restore(JSON.parse(value) as Portable);
const sha256 = (value: string): string =>
  createHash("sha256").update(value).digest("hex");

/** Test harness seam only; production constructs artifacts from complete replay. */
export const testingOnlyScriptIntegrityHashMissingArtifactV1 = ({
  detectionId,
  evidence: rawEvidence,
  source,
}: {
  readonly detectionId: string;
  readonly evidence: ScriptIntegrityHashMissingEvidenceV1;
  readonly source: ScriptIntegrityHashMissingAuthenticatedSourceV1;
}): ProductionScriptIntegrityHashMissingArtifactV1 => {
  const evidence = prepareScriptIntegrityHashMissingEvidenceV1(rawEvidence);
  const evidenceJson = encodePortable(evidence);
  const sourceJson = encodePortable(source);
  const artifact = normalizeJournalJsonV1({
    schemaVersion: PRODUCTION_SCRIPT_INTEGRITY_HASH_MISSING_ARTIFACT_V1,
    headerHash: evidence.finding.headerHash,
    detectionId,
    evidenceJson,
    sourceJson,
    payloadSha256: sha256(`${evidenceJson}\u0000${sourceJson}`),
  }) as ProductionScriptIntegrityHashMissingArtifactV1;
  admitProductionScriptIntegrityHashMissingArtifactV1(artifact);
  return Object.freeze(artifact);
};

export type ProductionScriptIntegrityHashMissingArtifactV1 =
  JournalJsonObjectV1 &
    Readonly<{
      schemaVersion: typeof PRODUCTION_SCRIPT_INTEGRITY_HASH_MISSING_ARTIFACT_V1;
      headerHash: string;
      detectionId: string;
      evidenceJson: string;
      sourceJson: string;
      payloadSha256: string;
    }>;

export type AdmittedProductionScriptIntegrityHashMissingArtifactV1 = Readonly<{
  artifact: ProductionScriptIntegrityHashMissingArtifactV1;
  evidence: ScriptIntegrityHashMissingEvidenceV1;
  source: ScriptIntegrityHashMissingAuthenticatedSourceV1;
  scriptPlan: FaultProofFieldOpeningPlanV1;
  redeemerPlan: FaultProofFieldOpeningPlanV1;
  staged: ReturnType<typeof planScriptIntegrityHashMissingStagedWalkV1>;
}>;

export const scriptIntegrityHashMissingWitnessSetV1 = (compactCbor: string) => {
  const decoded = decodeMidgardNativeTxWitnessSetCompactV1(
    Buffer.from(compactCbor, "hex"),
  );
  return Object.freeze({
    addr_tx_wits_hash: Buffer.from(decoded.addrTxWitsHash).toString("hex"),
    script_tx_wits_hash: Buffer.from(decoded.scriptTxWitsHash).toString("hex"),
    redeemer_tx_wits_hash: Buffer.from(decoded.redeemerTxWitsHash).toString(
      "hex",
    ),
  });
};

export const admitProductionScriptIntegrityHashMissingArtifactV1 = (
  value: unknown,
  owner = "00".repeat(28),
): AdmittedProductionScriptIntegrityHashMissingArtifactV1 => {
  if (typeof value !== "object" || value === null || Array.isArray(value))
    throw new Error("scriptIntegrityHashMissing artifact must be an object");
  const record = value as Record<string, unknown>;
  const expected = [
    "detectionId",
    "evidenceJson",
    "headerHash",
    "payloadSha256",
    "schemaVersion",
    "sourceJson",
  ];
  if (
    Object.keys(record).sort().join(",") !== expected.join(",") ||
    record.schemaVersion !==
      PRODUCTION_SCRIPT_INTEGRITY_HASH_MISSING_ARTIFACT_V1 ||
    typeof record.headerHash !== "string" ||
    !/^[0-9a-f]{56}$/u.test(record.headerHash) ||
    typeof record.detectionId !== "string" ||
    typeof record.evidenceJson !== "string" ||
    typeof record.sourceJson !== "string" ||
    typeof record.payloadSha256 !== "string" ||
    record.payloadSha256 !==
      sha256(`${record.evidenceJson}\u0000${record.sourceJson}`)
  )
    throw new Error(
      "scriptIntegrityHashMissing artifact identity or digest changed",
    );
  const evidence = prepareScriptIntegrityHashMissingEvidenceV1(
    decodePortable(record.evidenceJson) as ScriptIntegrityHashMissingEvidenceV1,
  );
  const source = decodePortable(
    record.sourceJson,
  ) as ScriptIntegrityHashMissingAuthenticatedSourceV1;
  if (
    evidence.finding.headerHash !== record.headerHash ||
    !/^[0-9a-f]{56}$/u.test(owner)
  )
    throw new Error(
      "scriptIntegrityHashMissing artifact changed header or owner",
    );
  const staged = planScriptIntegrityHashMissingStagedWalkV1({
    transactionId: evidence.finding.transactionId,
    scriptWitnessesPreimageCbor: evidence.scriptWitnessesPreimageCbor,
    redeemersPreimageCbor: evidence.redeemersPreimageCbor,
  });
  const witnessSet = scriptIntegrityHashMissingWitnessSetV1(
    evidence.witnessSetCompactCbor,
  );
  const plan = (fieldIndex: 6 | 8, items: readonly Buffer[]) =>
    planFaultProofFieldOpeningV1({
      fieldIndex,
      anchorTxId: evidence.finding.transactionId,
      nativeTxCompactCbor: evidence.nativeTxCompactCbor,
      witnessSet,
      anchorWitnessSetHash: txWitnessSetHash(evidence.nativeTxCompactCbor),
      itemCbors: items,
      owner,
      publish: true,
      label: `scriptIntegrityHashMissing field ${fieldIndex.toString()}`,
    });
  return Object.freeze({
    artifact: Object.freeze(
      record,
    ) as ProductionScriptIntegrityHashMissingArtifactV1,
    evidence,
    source,
    staged,
    scriptPlan: plan(6, staged.scriptItems),
    redeemerPlan: plan(8, staged.redeemerItems),
  });
};

const txWitnessSetHash = (compactCbor: string): string =>
  Buffer.from(
    decodeMidgardNativeTxCompactV1(Buffer.from(compactCbor, "hex"))
      .transactionWitnessSetHash,
  ).toString("hex");

export const prepareProductionScriptIntegrityHashMissingArtifactV1 = async ({
  evidence,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >;
}): Promise<ProductionScriptIntegrityHashMissingArtifactV1> => {
  const selected = detectScriptIntegrityHashMissingFromCanonicalEvidenceV1(
    evidence,
  ).filter(
    ({ detectionId }) => detectionId === classification.selected.detectionId,
  );
  if (
    classification.category !== "scriptIntegrityHashMissing" ||
    classification.headerHash !== evidence.headerHash ||
    selected.length !== 1
  )
    throw new Error(
      "scriptIntegrityHashMissing selected replay detection changed",
    );
  const detection = selected[0]!;
  const reconstructed = await reconstructScriptIntegrityHashMissingEvidenceV1({
    evidence,
    transactionId: detection.transactionId,
    direction: detection.direction,
  });
  const source = await deriveScriptIntegrityHashMissingAuthenticatedSourceV1({
    block: evidence,
    evidence: reconstructed,
  });
  const evidenceJson = encodePortable(reconstructed);
  const sourceJson = encodePortable(source);
  const artifact = normalizeJournalJsonV1({
    schemaVersion: PRODUCTION_SCRIPT_INTEGRITY_HASH_MISSING_ARTIFACT_V1,
    headerHash: evidence.headerHash,
    detectionId: detection.detectionId,
    evidenceJson,
    sourceJson,
    payloadSha256: sha256(`${evidenceJson}\u0000${sourceJson}`),
  }) as ProductionScriptIntegrityHashMissingArtifactV1;
  admitProductionScriptIntegrityHashMissingArtifactV1(artifact);
  return Object.freeze(artifact);
};
