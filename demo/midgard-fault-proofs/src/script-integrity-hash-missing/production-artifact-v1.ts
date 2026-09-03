import { createHash } from "node:crypto";

import {
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
} from "../field-opening-v1.js";
import type { CanonicalBlockClassification } from "../workflow/classification-v1.js";
import {
  type JournalJsonObject,
  normalizeJournalJson,
} from "../workflow/journal-v1.js";
import {
  prepareScriptIntegrityHashMissingEvidence,
  type ScriptIntegrityHashMissingEvidence,
} from "./family-v1.js";
import {
  deriveScriptIntegrityHashMissingAuthenticatedSource,
  detectScriptIntegrityHashMissingFromCanonicalEvidence,
  reconstructScriptIntegrityHashMissingEvidence,
  type ScriptIntegrityHashMissingAuthenticatedSource,
} from "./replay-v1.js";
import { planScriptIntegrityHashMissingStagedWalk } from "./staged-plan-v1.js";

export const SCRIPT_INTEGRITY_HASH_MISSING_ARTIFACT =
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
export const testingOnlyScriptIntegrityHashMissingArtifact = ({
  detectionId,
  evidence: rawEvidence,
  source,
}: {
  readonly detectionId: string;
  readonly evidence: ScriptIntegrityHashMissingEvidence;
  readonly source: ScriptIntegrityHashMissingAuthenticatedSource;
}): ScriptIntegrityHashMissingArtifact => {
  const evidence = prepareScriptIntegrityHashMissingEvidence(rawEvidence);
  const evidenceJson = encodePortable(evidence);
  const sourceJson = encodePortable(source);
  const artifact = normalizeJournalJson({
    schemaVersion: SCRIPT_INTEGRITY_HASH_MISSING_ARTIFACT,
    headerHash: evidence.finding.headerHash,
    detectionId,
    evidenceJson,
    sourceJson,
    payloadSha256: sha256(`${evidenceJson}\u0000${sourceJson}`),
  }) as ScriptIntegrityHashMissingArtifact;
  admitScriptIntegrityHashMissingArtifact(artifact);
  return Object.freeze(artifact);
};

export type ScriptIntegrityHashMissingArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof SCRIPT_INTEGRITY_HASH_MISSING_ARTIFACT;
    headerHash: string;
    detectionId: string;
    evidenceJson: string;
    sourceJson: string;
    payloadSha256: string;
  }>;

export type AdmittedScriptIntegrityHashMissingArtifact = Readonly<{
  artifact: ScriptIntegrityHashMissingArtifact;
  evidence: ScriptIntegrityHashMissingEvidence;
  source: ScriptIntegrityHashMissingAuthenticatedSource;
  scriptPlan: FaultProofFieldOpeningPlan;
  redeemerPlan: FaultProofFieldOpeningPlan;
  staged: ReturnType<typeof planScriptIntegrityHashMissingStagedWalk>;
}>;

export const scriptIntegrityHashMissingWitnessSet = (compactCbor: string) => {
  const decoded = decodeMidgardNativeTxWitnessSetCompact(
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

export const admitScriptIntegrityHashMissingArtifact = (
  value: unknown,
  owner = "00".repeat(28),
): AdmittedScriptIntegrityHashMissingArtifact => {
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
    record.schemaVersion !== SCRIPT_INTEGRITY_HASH_MISSING_ARTIFACT ||
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
  const evidence = prepareScriptIntegrityHashMissingEvidence(
    decodePortable(record.evidenceJson) as ScriptIntegrityHashMissingEvidence,
  );
  const source = decodePortable(
    record.sourceJson,
  ) as ScriptIntegrityHashMissingAuthenticatedSource;
  if (
    evidence.finding.headerHash !== record.headerHash ||
    !/^[0-9a-f]{56}$/u.test(owner)
  )
    throw new Error(
      "scriptIntegrityHashMissing artifact changed header or owner",
    );
  const staged = planScriptIntegrityHashMissingStagedWalk({
    transactionId: evidence.finding.transactionId,
    scriptWitnessesPreimageCbor: evidence.scriptWitnessesPreimageCbor,
    redeemersPreimageCbor: evidence.redeemersPreimageCbor,
  });
  const witnessSet = scriptIntegrityHashMissingWitnessSet(
    evidence.witnessSetCompactCbor,
  );
  const plan = (fieldIndex: 6 | 8, items: readonly Buffer[]) =>
    planFaultProofFieldOpening({
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
    artifact: Object.freeze(record) as ScriptIntegrityHashMissingArtifact,
    evidence,
    source,
    staged,
    scriptPlan: plan(6, staged.scriptItems),
    redeemerPlan: plan(8, staged.redeemerItems),
  });
};

const txWitnessSetHash = (compactCbor: string): string =>
  Buffer.from(
    decodeMidgardNativeTxCompact(Buffer.from(compactCbor, "hex"))
      .transactionWitnessSetHash,
  ).toString("hex");

export const prepareScriptIntegrityHashMissingArtifact = async ({
  evidence,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >;
}): Promise<ScriptIntegrityHashMissingArtifact> => {
  const selected = detectScriptIntegrityHashMissingFromCanonicalEvidence(
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
  const reconstructed = await reconstructScriptIntegrityHashMissingEvidence({
    evidence,
    transactionId: detection.transactionId,
    direction: detection.direction,
  });
  const source = await deriveScriptIntegrityHashMissingAuthenticatedSource({
    block: evidence,
    evidence: reconstructed,
  });
  const evidenceJson = encodePortable(reconstructed);
  const sourceJson = encodePortable(source);
  const artifact = normalizeJournalJson({
    schemaVersion: SCRIPT_INTEGRITY_HASH_MISSING_ARTIFACT,
    headerHash: evidence.headerHash,
    detectionId: detection.detectionId,
    evidenceJson,
    sourceJson,
    payloadSha256: sha256(`${evidenceJson}\u0000${sourceJson}`),
  }) as ScriptIntegrityHashMissingArtifact;
  admitScriptIntegrityHashMissingArtifact(artifact);
  return Object.freeze(artifact);
};
