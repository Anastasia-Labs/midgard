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
  type ObserversForbiddenEvidence,
  observersForbiddenEvidenceCloses,
  ObserversForbiddenVerdictSubjectSchema,
  prepareObserversForbiddenEvidence,
} from "./family-v1.js";

export const OBSERVERS_FORBIDDEN_ARTIFACT =
  "midgard-production-observers-forbidden-on-untagged-network-artifact-v1" as const;

export type ObserversForbiddenArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof OBSERVERS_FORBIDDEN_ARTIFACT;
    headerHash: string;
    detectionId: string;
    position: number;
    transactionId: string;
    sourceKind: "accepted" | "forced";
    networkId: 0 | 1 | 255;
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

const canonicalHex = (value: unknown, bytes: number | null, label: string) => {
  if (
    typeof value !== "string" ||
    !/^(?:[0-9a-f]{2})*$/u.test(value) ||
    (bytes !== null && value.length !== bytes * 2)
  )
    throw new Error(`observersForbidden ${label} is not canonical hex`);
  return value;
};

const natural = (value: unknown, label: string) => {
  if (!Number.isSafeInteger(value) || (value as number) < 0)
    throw new Error(`observersForbidden ${label} is not a natural`);
  return value as number;
};

const network = (value: unknown): 0 | 1 | 255 => {
  if (value !== 0 && value !== 1 && value !== 255)
    throw new Error("observersForbidden network scalar changed");
  return value;
};

export const buildObserversForbiddenArtifact = ({
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
  readonly evidence: ObserversForbiddenEvidence;
  readonly sourceKind?: "accepted" | "forced";
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly l2TransactionSourceCbor: string;
  readonly transactionsPhasRoot: string;
  readonly transactionMembershipCbor: string;
  readonly forcedSourceCbor?: string;
}): ObserversForbiddenArtifact =>
  Object.freeze({
    schemaVersion: OBSERVERS_FORBIDDEN_ARTIFACT,
    headerHash: canonicalHex(headerHash, 28, "header hash"),
    detectionId,
    position: natural(Number(position), "position"),
    transactionId: evidence.subject.transaction_id,
    sourceKind,
    networkId: evidence.networkId,
    subjectCbor: Data.to(
      evidence.subject as never,
      ObserversForbiddenVerdictSubjectSchema as never,
    ),
    nativeTxCompactCbor: canonicalHex(
      nativeTxCompactCbor,
      null,
      "compact source",
    ),
    witnessSetCompactCbor: canonicalHex(
      witnessSetCompactCbor,
      null,
      "witness compact source",
    ),
    l2TransactionSourceCbor: canonicalHex(
      l2TransactionSourceCbor,
      null,
      "transaction source",
    ),
    transactionsPhasRoot: canonicalHex(
      transactionsPhasRoot,
      32,
      "transactions PHAS root",
    ),
    fieldPreimageCbor: evidence.observerFieldPreimageCbor,
    fieldCommitment: evidence.observerFieldCommitment,
    transactionMembershipCbor: canonicalHex(
      transactionMembershipCbor,
      null,
      "transaction membership",
    ),
    forcedSourceCbor: canonicalHex(forcedSourceCbor, null, "forced source"),
  });

export type AdmittedObserversForbiddenArtifact = Readonly<{
  artifact: ObserversForbiddenArtifact;
  evidence: ObserversForbiddenEvidence;
}>;

export const admitObserversForbiddenArtifact = (
  value: unknown,
): AdmittedObserversForbiddenArtifact => {
  if (typeof value !== "object" || value === null || Array.isArray(value))
    throw new Error("observersForbidden artifact must be an object");
  const raw = value as Record<string, unknown>;
  const keys = [
    "schemaVersion",
    "headerHash",
    "detectionId",
    "position",
    "transactionId",
    "sourceKind",
    "networkId",
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
    Object.keys(raw).sort().join("\0") !== keys.join("\0") ||
    raw.schemaVersion !== OBSERVERS_FORBIDDEN_ARTIFACT ||
    typeof raw.detectionId !== "string" ||
    raw.detectionId.length === 0
  )
    throw new Error("observersForbidden artifact shape/version changed");
  const artifact = Object.freeze({
    schemaVersion: OBSERVERS_FORBIDDEN_ARTIFACT,
    headerHash: canonicalHex(raw.headerHash, 28, "header hash"),
    detectionId: raw.detectionId,
    position: natural(raw.position, "position"),
    transactionId: canonicalHex(raw.transactionId, 32, "transaction id"),
    sourceKind:
      raw.sourceKind === "accepted" || raw.sourceKind === "forced"
        ? raw.sourceKind
        : (() => {
            throw new Error("observersForbidden source kind changed");
          })(),
    networkId: network(raw.networkId),
    subjectCbor: canonicalHex(raw.subjectCbor, null, "subject"),
    nativeTxCompactCbor: canonicalHex(
      raw.nativeTxCompactCbor,
      null,
      "compact source",
    ),
    witnessSetCompactCbor: canonicalHex(
      raw.witnessSetCompactCbor,
      null,
      "witness source",
    ),
    l2TransactionSourceCbor: canonicalHex(
      raw.l2TransactionSourceCbor,
      null,
      "transaction source",
    ),
    transactionsPhasRoot: canonicalHex(
      raw.transactionsPhasRoot,
      32,
      "transactions root",
    ),
    fieldPreimageCbor: canonicalHex(
      raw.fieldPreimageCbor,
      null,
      "field preimage",
    ),
    fieldCommitment: canonicalHex(raw.fieldCommitment, 32, "field commitment"),
    transactionMembershipCbor: canonicalHex(
      raw.transactionMembershipCbor,
      null,
      "membership",
    ),
    forcedSourceCbor: canonicalHex(raw.forcedSourceCbor, null, "forced source"),
  }) satisfies ObserversForbiddenArtifact;
  const subject = Data.from(
    artifact.subjectCbor,
    ObserversForbiddenVerdictSubjectSchema as never,
  ) as VerdictSubject;
  if (subject.transaction_id !== artifact.transactionId)
    throw new Error("observersForbidden artifact subject changed transaction");
  const field = Buffer.from(artifact.fieldPreimageCbor, "hex");
  if (
    midgardFieldCommitment(field).toString("hex") !== artifact.fieldCommitment
  )
    throw new Error("observersForbidden artifact field commitment changed");
  if (
    (artifact.sourceKind === "accepted") !==
    (artifact.forcedSourceCbor.length === 0)
  )
    throw new Error("observersForbidden artifact source payload changed");
  const evidence = prepareObserversForbiddenEvidence({
    finding: { subject, networkId: artifact.networkId },
    observerFieldPreimage: field,
    committedFieldHashHex: artifact.fieldCommitment,
  });
  if (!observersForbiddenEvidenceCloses(evidence))
    throw new Error("observersForbidden artifact does not close contradiction");
  return Object.freeze({ artifact, evidence });
};

export const ObserversForbiddenForcedSourcePayloadSchema = Data.Object({
  header: HeaderSchema,
  membership: rootMembershipProofSchema(
    OutputReferenceSchema,
    ForcedInclusionTxSchema,
  ),
  direction: Data.Integer(),
});

export const observersForbiddenArtifactDigest = (
  artifact: ObserversForbiddenArtifact,
) =>
  createHash("sha256")
    .update(
      JSON.stringify(
        Object.fromEntries(
          Object.entries(artifact).sort(([a], [b]) => a.localeCompare(b)),
        ),
      ),
    )
    .digest("hex");
