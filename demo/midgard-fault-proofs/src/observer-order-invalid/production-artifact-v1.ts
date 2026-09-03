import { createHash } from "node:crypto";

import { midgardFieldCommitment } from "@al-ft/midgard-core";
import type { VerdictSubject } from "@al-ft/midgard-sdk";
import {
  ForcedInclusionTxSchema,
  HeaderSchema,
  OutputReferenceSchema,
  rootMembershipProofSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { JournalJsonObject } from "../workflow/journal-v1.js";
import {
  type ObserverOrderInvalidEvidence,
  ObserverOrderInvalidVerdictSubjectSchema,
  prepareObserverOrderInvalidEvidence,
} from "./family-v1.js";
import {
  type ObserverOrderInvalidStagedPlan,
  planObserverOrderInvalidStagedWalk,
} from "./staged-plan-v1.js";

export const OBSERVER_ORDER_INVALID_ARTIFACT =
  "midgard-production-observer-order-invalid-artifact-v1" as const;

export type ObserverOrderInvalidArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof OBSERVER_ORDER_INVALID_ARTIFACT;
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

export const buildObserverOrderInvalidArtifact = ({
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
  readonly evidence: ObserverOrderInvalidEvidence;
  readonly sourceKind?: "accepted" | "forced";
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly l2TransactionSourceCbor: string;
  readonly transactionsPhasRoot: string;
  readonly transactionMembershipCbor: string;
  readonly forcedSourceCbor?: string;
}): ObserverOrderInvalidArtifact =>
  Object.freeze({
    schemaVersion: OBSERVER_ORDER_INVALID_ARTIFACT,
    headerHash: hex(headerHash, 28, "header hash"),
    detectionId,
    position: natural(Number(position), "position"),
    transactionId: evidence.subject.transaction_id,
    sourceKind,
    observerIndex: evidence.observerIndex,
    subjectCbor: Data.to(
      evidence.subject as never,
      ObserverOrderInvalidVerdictSubjectSchema as never,
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

export type AdmittedObserverOrderInvalidArtifact = Readonly<{
  artifact: ObserverOrderInvalidArtifact;
  evidence: ObserverOrderInvalidEvidence;
  staged: ObserverOrderInvalidStagedPlan;
}>;

export const admitObserverOrderInvalidArtifact = (
  value: unknown,
): AdmittedObserverOrderInvalidArtifact => {
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
    raw.schemaVersion !== OBSERVER_ORDER_INVALID_ARTIFACT ||
    typeof raw.detectionId !== "string" ||
    raw.detectionId.length === 0
  )
    throw new Error("observerOrderInvalid artifact shape/version changed");
  const artifact = Object.freeze({
    schemaVersion: OBSERVER_ORDER_INVALID_ARTIFACT,
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
  }) satisfies ObserverOrderInvalidArtifact;
  const subject = Data.from(
    artifact.subjectCbor,
    ObserverOrderInvalidVerdictSubjectSchema as never,
  ) as VerdictSubject;
  if (subject.transaction_id !== artifact.transactionId)
    throw new Error(
      "observerOrderInvalid artifact subject changed transaction",
    );
  const field = Buffer.from(artifact.fieldPreimageCbor, "hex");
  if (
    midgardFieldCommitment(field).toString("hex") !== artifact.fieldCommitment
  )
    throw new Error("observerOrderInvalid artifact field commitment changed");
  const evidence = prepareObserverOrderInvalidEvidence({
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
    staged: planObserverOrderInvalidStagedWalk({
      transactionId: artifact.transactionId,
      fieldPreimageCbor: artifact.fieldPreimageCbor,
      observerIndex: artifact.observerIndex,
    }),
  });
};

export const ObserverOrderInvalidForcedSourcePayloadSchema = Data.Object({
  header: HeaderSchema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxSchema,
  ),
  direction: Data.Integer(),
});

export const observerOrderInvalidArtifactDigest = (
  artifact: ObserverOrderInvalidArtifact,
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
