import { createHash } from "node:crypto";

import { midgardFieldCommitmentV1 } from "@al-ft/midgard-core";
import type { VerdictSubjectV1 } from "@al-ft/midgard-sdk";
import {
  ForcedInclusionTxV1Schema,
  HeaderV1Schema,
  OutputReferenceSchema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { JournalJsonObjectV1 } from "../workflow/journal-v1.js";
import {
  type ObserverOrderInvalidEvidenceV1,
  ObserverOrderInvalidVerdictSubjectV1Schema,
  prepareObserverOrderInvalidEvidenceV1,
} from "./family-v1.js";
import {
  type ObserverOrderInvalidStagedPlanV1,
  planObserverOrderInvalidStagedWalkV1,
} from "./staged-plan-v1.js";

export const PRODUCTION_OBSERVER_ORDER_INVALID_ARTIFACT_V1 =
  "midgard-production-observer-order-invalid-artifact-v1" as const;

export type ProductionObserverOrderInvalidArtifactV1 = JournalJsonObjectV1 &
  Readonly<{
    schemaVersion: typeof PRODUCTION_OBSERVER_ORDER_INVALID_ARTIFACT_V1;
    headerHash: string;
    detectionId: string;
    position: number;
    transactionId: string;
    sourceKind: "accepted" | "forced";
    observerIndex: number;
    subjectCbor: string;
    nativeTxCompactCbor: string;
    witnessSetCompactCbor: string;
    l2TransactionSourceCbor: string;
    transactionsPhasRoot: string;
    fieldPreimageCbor: string;
    fieldCommitment: string;
    transactionMembershipCbor: string;
    forcedSourceCbor: string;
  }>;

const hex = (value: unknown, bytes: number | null, label: string): string => {
  if (
    typeof value !== "string" ||
    !/^(?:[0-9a-f]{2})*$/u.test(value) ||
    (bytes !== null && value.length !== bytes * 2)
  )
    throw new Error(`observerOrderInvalid ${label} is not canonical hex`);
  return value;
};

const natural = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0)
    throw new Error(`observerOrderInvalid ${label} is not a natural`);
  return value as number;
};

export const buildProductionObserverOrderInvalidArtifactV1 = ({
  headerHash,
  detectionId,
  position,
  evidence,
  sourceKind = "accepted",
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  l2TransactionSourceCbor,
  transactionsPhasRoot,
  transactionMembershipCbor,
  forcedSourceCbor = "",
}: {
  readonly headerHash: string;
  readonly detectionId: string;
  readonly position: bigint;
  readonly evidence: ObserverOrderInvalidEvidenceV1;
  readonly sourceKind?: "accepted" | "forced";
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly l2TransactionSourceCbor: string;
  readonly transactionsPhasRoot: string;
  readonly transactionMembershipCbor: string;
  readonly forcedSourceCbor?: string;
}): ProductionObserverOrderInvalidArtifactV1 =>
  Object.freeze({
    schemaVersion: PRODUCTION_OBSERVER_ORDER_INVALID_ARTIFACT_V1,
    headerHash: hex(headerHash, 28, "header hash"),
    detectionId,
    position: natural(Number(position), "position"),
    transactionId: evidence.subject.transaction_id,
    sourceKind,
    observerIndex: evidence.observerIndex,
    subjectCbor: Data.to(
      evidence.subject as never,
      ObserverOrderInvalidVerdictSubjectV1Schema as never,
    ),
    nativeTxCompactCbor: hex(nativeTxCompactCbor, null, "compact source"),
    witnessSetCompactCbor: hex(
      witnessSetCompactCbor,
      null,
      "witness compact source",
    ),
    l2TransactionSourceCbor: hex(
      l2TransactionSourceCbor,
      null,
      "transaction source",
    ),
    transactionsPhasRoot: hex(
      transactionsPhasRoot,
      32,
      "transactions PHAS root",
    ),
    fieldPreimageCbor: evidence.fieldPreimageHex,
    fieldCommitment: evidence.fieldCommitmentHex,
    transactionMembershipCbor: hex(
      transactionMembershipCbor,
      null,
      "transaction membership",
    ),
    forcedSourceCbor: hex(forcedSourceCbor, null, "forced source"),
  });

export type AdmittedProductionObserverOrderInvalidArtifactV1 = Readonly<{
  artifact: ProductionObserverOrderInvalidArtifactV1;
  evidence: ObserverOrderInvalidEvidenceV1;
  staged: ObserverOrderInvalidStagedPlanV1;
}>;

export const admitProductionObserverOrderInvalidArtifactV1 = (
  value: unknown,
): AdmittedProductionObserverOrderInvalidArtifactV1 => {
  if (typeof value !== "object" || value === null || Array.isArray(value))
    throw new Error("observerOrderInvalid artifact must be an object");
  const raw = value as Record<string, unknown>;
  const expected = [
    "schemaVersion",
    "headerHash",
    "detectionId",
    "position",
    "transactionId",
    "sourceKind",
    "observerIndex",
    "subjectCbor",
    "nativeTxCompactCbor",
    "witnessSetCompactCbor",
    "l2TransactionSourceCbor",
    "transactionsPhasRoot",
    "fieldPreimageCbor",
    "fieldCommitment",
    "transactionMembershipCbor",
    "forcedSourceCbor",
  ].sort();
  if (
    Object.keys(raw).sort().join("\0") !== expected.join("\0") ||
    raw.schemaVersion !== PRODUCTION_OBSERVER_ORDER_INVALID_ARTIFACT_V1 ||
    typeof raw.detectionId !== "string" ||
    raw.detectionId.length === 0
  )
    throw new Error("observerOrderInvalid artifact shape/version changed");
  const artifact = Object.freeze({
    schemaVersion: PRODUCTION_OBSERVER_ORDER_INVALID_ARTIFACT_V1,
    headerHash: hex(raw.headerHash, 28, "header hash"),
    detectionId: raw.detectionId,
    position: natural(raw.position, "position"),
    transactionId: hex(raw.transactionId, 32, "transaction id"),
    sourceKind:
      raw.sourceKind === "accepted" || raw.sourceKind === "forced"
        ? raw.sourceKind
        : (() => {
            throw new Error(
              "observerOrderInvalid artifact source kind changed",
            );
          })(),
    observerIndex: natural(raw.observerIndex, "observer index"),
    subjectCbor: hex(raw.subjectCbor, null, "subject"),
    nativeTxCompactCbor: hex(raw.nativeTxCompactCbor, null, "compact source"),
    witnessSetCompactCbor: hex(
      raw.witnessSetCompactCbor,
      null,
      "witness compact source",
    ),
    l2TransactionSourceCbor: hex(
      raw.l2TransactionSourceCbor,
      null,
      "transaction source",
    ),
    transactionsPhasRoot: hex(
      raw.transactionsPhasRoot,
      32,
      "transactions PHAS root",
    ),
    fieldPreimageCbor: hex(raw.fieldPreimageCbor, null, "field preimage"),
    fieldCommitment: hex(raw.fieldCommitment, 32, "field commitment"),
    transactionMembershipCbor: hex(
      raw.transactionMembershipCbor,
      null,
      "transaction membership",
    ),
    forcedSourceCbor: hex(raw.forcedSourceCbor, null, "forced source"),
  }) satisfies ProductionObserverOrderInvalidArtifactV1;
  const subject = Data.from(
    artifact.subjectCbor,
    ObserverOrderInvalidVerdictSubjectV1Schema as never,
  ) as VerdictSubjectV1;
  if (subject.transaction_id !== artifact.transactionId)
    throw new Error(
      "observerOrderInvalid artifact subject changed transaction",
    );
  const field = Buffer.from(artifact.fieldPreimageCbor, "hex");
  if (
    midgardFieldCommitmentV1(field).toString("hex") !== artifact.fieldCommitment
  )
    throw new Error("observerOrderInvalid artifact field commitment changed");
  const evidence = prepareObserverOrderInvalidEvidenceV1({
    finding: { subject, observerIndex: artifact.observerIndex },
    fieldPreimage: field,
    committedFieldHashHex: artifact.fieldCommitment,
  });
  if (
    (artifact.sourceKind === "accepted") !==
    (artifact.forcedSourceCbor.length === 0)
  )
    throw new Error("observerOrderInvalid artifact source payload changed");
  return Object.freeze({
    artifact,
    evidence,
    staged: planObserverOrderInvalidStagedWalkV1({
      transactionId: artifact.transactionId,
      fieldPreimageCbor: artifact.fieldPreimageCbor,
      observerIndex: artifact.observerIndex,
    }),
  });
};

export const ObserverOrderInvalidForcedSourcePayloadV1Schema = Data.Object({
  header: HeaderV1Schema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxV1Schema,
  ),
  direction: Data.Integer(),
});

export const productionObserverOrderInvalidArtifactDigestV1 = (
  artifact: ProductionObserverOrderInvalidArtifactV1,
): string =>
  createHash("sha256")
    .update(
      JSON.stringify(
        Object.fromEntries(
          Object.entries(artifact).sort(([a], [b]) => a.localeCompare(b)),
        ),
      ),
    )
    .digest("hex");
