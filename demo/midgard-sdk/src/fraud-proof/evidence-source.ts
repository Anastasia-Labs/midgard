/**
 * Canonical evidence-source API (Goal task `Q03`).
 *
 * `GOAL_SPEC.md` §9.2 acceptance:
 *
 * > Builders consume verified `DaPayloadV1`/proof bundles and authenticated L1
 * > observations, not operator-private REST/DB/files except labelled
 * > diagnostics.
 *
 * This module owns the trust vocabulary and the fail-closed admission rules.
 * It deliberately holds no transport: transports live in
 * `@al-ft/midgard-fault-proofs` (`src/evidence/**`) and in the watcher, and
 * both must route every security input through the admission functions here.
 *
 * The admitted/prohibited class lists below are the authority. Anything not
 * enumerated is rejected; an unknown class is never treated as admitted.
 */
import { Effect } from "effect";

import { hashBlockHeader, type Header } from "../ledger-state.js";

export const CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION =
  "midgard-canonical-evidence-source-v1" as const;

/** Security inputs a fault-proof builder or watcher may rely on. */
export const ADMITTED_EVIDENCE_TRUST_CLASSES = [
  "authenticated_cardano_l1",
  "public_or_permissionless_da",
  "signed_deployment_identity",
  "deterministic_local_computation",
] as const;

export type AdmittedEvidenceTrustClass =
  (typeof ADMITTED_EVIDENCE_TRUST_CLASSES)[number];

/** Operator-private surfaces. Usable only as explicitly labelled diagnostics. */
export const PROHIBITED_EVIDENCE_TRUST_CLASSES = [
  "operator_private_database",
  "operator_admin_api",
  "operator_private_file",
  "operator_only_diagnostic_endpoint",
] as const;

export type ProhibitedEvidenceTrustClass =
  (typeof PROHIBITED_EVIDENCE_TRUST_CLASSES)[number];

export type EvidenceTrustClass =
  | AdmittedEvidenceTrustClass
  | ProhibitedEvidenceTrustClass;

export type EvidenceGrade = "security" | "diagnostic";

export const L1_SOURCE_MODES = ["local_node", "external_providers"] as const;
export type L1SourceMode = (typeof L1_SOURCE_MODES)[number];

export type EvidenceProvenance = {
  readonly trustClass: EvidenceTrustClass;
  /** Stable, non-secret identifier of the concrete source (peer id, mode id). */
  readonly sourceId: string;
  readonly grade: EvidenceGrade;
  /** Mandatory for `grade: "diagnostic"`, forbidden for `grade: "security"`. */
  readonly diagnosticLabel?: string;
};

export type CanonicalEvidenceRejectionCode =
  | "unknown_trust_class"
  | "prohibited_trust_class"
  | "diagnostic_grade_not_admitted"
  | "missing_diagnostic_label"
  | "diagnostic_label_on_security_evidence"
  | "empty_source_id"
  | "unknown_l1_source_mode"
  | "l1_observation_wrong_trust_class"
  | "insufficient_confirmation_depth"
  | "malformed_chain_point"
  | "malformed_header_hash"
  | "header_hash_mismatch"
  | "da_evidence_wrong_trust_class"
  | "payload_header_mismatch"
  | "transaction_source_inclusion_root_unauthenticated"
  | "evidence_grade_mismatch";

/**
 * Deterministic, value-free rejection. `detail` carries only codes, field
 * names, and public identifiers so evidence errors stay secret-safe.
 */
export class CanonicalEvidenceRejection extends Error {
  readonly code: CanonicalEvidenceRejectionCode;
  readonly detail: string;

  constructor(code: CanonicalEvidenceRejectionCode, detail: string) {
    super(`${code}: ${detail}`);
    this.name = "CanonicalEvidenceRejectionV1";
    this.code = code;
    this.detail = detail;
  }
}

const reject = (
  code: CanonicalEvidenceRejectionCode,
  detail: string,
): never => {
  throw new CanonicalEvidenceRejection(code, detail);
};

export const isAdmittedEvidenceTrustClass = (
  value: string,
): value is AdmittedEvidenceTrustClass =>
  (ADMITTED_EVIDENCE_TRUST_CLASSES as readonly string[]).includes(value);

export const isProhibitedEvidenceTrustClass = (
  value: string,
): value is ProhibitedEvidenceTrustClass =>
  (PROHIBITED_EVIDENCE_TRUST_CLASSES as readonly string[]).includes(value);

export type AdmitEvidenceProvenanceOptions = {
  readonly provenance: EvidenceProvenance;
  /**
   * Diagnostic evidence is admitted only when the caller opts in explicitly.
   * Builders that produce submittable proofs must leave this `false`.
   */
  readonly allowDiagnostic?: boolean;
};

/**
 * Fail-closed provenance admission.
 *
 * - unknown class -> rejected (never "assume admitted");
 * - prohibited class -> rejected unless the record is an explicitly labelled
 *   diagnostic *and* the caller opted into diagnostics;
 * - diagnostic grade -> rejected unless the caller opted into diagnostics;
 * - security grade with a diagnostic label -> rejected (no laundering).
 */
export const admitEvidenceProvenance = ({
  provenance,
  allowDiagnostic = false,
}: AdmitEvidenceProvenanceOptions): EvidenceProvenance => {
  const sourceId = provenance.sourceId.trim();
  if (sourceId.length === 0) {
    reject("empty_source_id", "provenance.sourceId");
  }
  const trustClass: string = provenance.trustClass;
  const admitted = isAdmittedEvidenceTrustClass(trustClass);
  const prohibited = isProhibitedEvidenceTrustClass(trustClass);
  if (!admitted && !prohibited) {
    reject("unknown_trust_class", `provenance.trustClass=${trustClass}`);
  }
  if (provenance.grade !== "security" && provenance.grade !== "diagnostic") {
    reject(
      "evidence_grade_mismatch",
      `provenance.grade=${String(provenance.grade)}`,
    );
  }
  if (provenance.grade === "security") {
    if (provenance.diagnosticLabel !== undefined) {
      reject(
        "diagnostic_label_on_security_evidence",
        `provenance.sourceId=${sourceId}`,
      );
    }
    if (prohibited) {
      reject(
        "prohibited_trust_class",
        `provenance.trustClass=${trustClass} cannot carry grade=security`,
      );
    }
    return Object.freeze({
      trustClass: provenance.trustClass,
      sourceId,
      grade: "security",
    });
  }
  if ((provenance.diagnosticLabel ?? "").trim().length === 0) {
    reject("missing_diagnostic_label", `provenance.sourceId=${sourceId}`);
  }
  if (!allowDiagnostic) {
    reject(
      prohibited ? "prohibited_trust_class" : "diagnostic_grade_not_admitted",
      `provenance.trustClass=${trustClass} provenance.sourceId=${sourceId}`,
    );
  }
  return Object.freeze({
    trustClass: provenance.trustClass,
    sourceId,
    grade: "diagnostic",
    diagnosticLabel: (provenance.diagnosticLabel ?? "").trim(),
  });
};

/** Admission for evidence that will back a submittable proof. */
export const assertSecurityGradeEvidence = (
  provenance: EvidenceProvenance,
): EvidenceProvenance =>
  admitEvidenceProvenance({ provenance, allowDiagnostic: false });

/** A bundle is security grade only when every contributing record is. */
export const combineEvidenceGrade = (
  provenances: readonly EvidenceProvenance[],
): EvidenceGrade =>
  provenances.some((provenance) => provenance.grade !== "security")
    ? "diagnostic"
    : "security";

export const requireEvidenceTrustClass = ({
  provenance,
  expected,
  code,
}: {
  readonly provenance: EvidenceProvenance;
  readonly expected: AdmittedEvidenceTrustClass;
  readonly code: CanonicalEvidenceRejectionCode;
}): EvidenceProvenance => {
  const admittedProvenance = assertSecurityGradeEvidence(provenance);
  if (admittedProvenance.trustClass !== expected) {
    reject(
      code,
      `expected=${expected} actual=${admittedProvenance.trustClass}`,
    );
  }
  return admittedProvenance;
};

export type AuthenticatedL1ChainPoint = {
  readonly slot: bigint;
  /** 32-byte lowercase hex Cardano block hash. */
  readonly blockHash: string;
};

export type AuthenticatedL1Observation = {
  readonly schemaVersion: typeof CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION;
  readonly sourceMode: L1SourceMode;
  readonly provenance: EvidenceProvenance;
  readonly chainPoint: AuthenticatedL1ChainPoint;
  /** Confirmation depth already observed for `chainPoint`. */
  readonly confirmationDepth: number;
};

/**
 * An authenticated L1 observation of one state-queue block header. This is the
 * only accepted origin of the committed header a DA payload is checked against.
 */
export type AuthenticatedStateQueueHeaderObservation =
  AuthenticatedL1Observation & {
    /** 28-byte lowercase hex header hash, as committed on L1. */
    readonly headerHash: string;
    readonly header: Header;
  };

const HEX = /^[0-9a-f]*$/u;

const requireHex = (
  value: string,
  byteLength: number,
  code: CanonicalEvidenceRejectionCode,
  fieldName: string,
): string => {
  const normalized = value.trim().toLowerCase();
  if (normalized.length !== byteLength * 2 || !HEX.test(normalized)) {
    reject(code, `${fieldName} must be ${byteLength.toString()}-byte hex`);
  }
  return normalized;
};

export type AdmitAuthenticatedL1ObservationOptions = {
  readonly observation: AuthenticatedL1Observation;
  /** Minimum confirmation depth the consumer requires. Default is 1. */
  readonly minimumConfirmationDepth?: number;
};

export const admitAuthenticatedL1Observation = ({
  observation,
  minimumConfirmationDepth = 1,
}: AdmitAuthenticatedL1ObservationOptions): AuthenticatedL1Observation => {
  if (
    !Number.isSafeInteger(minimumConfirmationDepth) ||
    minimumConfirmationDepth < 1
  ) {
    reject(
      "insufficient_confirmation_depth",
      `minimumConfirmationDepth=${String(minimumConfirmationDepth)}`,
    );
  }
  if (observation.schemaVersion !== CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION) {
    reject(
      "evidence_grade_mismatch",
      `observation.schemaVersion=${String(observation.schemaVersion)}`,
    );
  }
  if (
    !(L1_SOURCE_MODES as readonly string[]).includes(observation.sourceMode)
  ) {
    reject(
      "unknown_l1_source_mode",
      `observation.sourceMode=${String(observation.sourceMode)}`,
    );
  }
  const provenance = requireEvidenceTrustClass({
    provenance: observation.provenance,
    expected: "authenticated_cardano_l1",
    code: "l1_observation_wrong_trust_class",
  });
  const blockHash = requireHex(
    observation.chainPoint.blockHash,
    32,
    "malformed_chain_point",
    "observation.chainPoint.blockHash",
  );
  if (observation.chainPoint.slot < 0n) {
    reject("malformed_chain_point", "observation.chainPoint.slot");
  }
  if (
    !Number.isSafeInteger(observation.confirmationDepth) ||
    observation.confirmationDepth < minimumConfirmationDepth
  ) {
    reject(
      "insufficient_confirmation_depth",
      `required=${minimumConfirmationDepth.toString()} actual=${String(
        observation.confirmationDepth,
      )}`,
    );
  }
  return Object.freeze({
    schemaVersion: CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
    sourceMode: observation.sourceMode,
    provenance,
    chainPoint: Object.freeze({ slot: observation.chainPoint.slot, blockHash }),
    confirmationDepth: observation.confirmationDepth,
  });
};

/**
 * Admits a header observation and re-derives the header hash with the canonical
 * SDK hasher, so a caller cannot pair an arbitrary header with a real header
 * hash (or vice versa).
 */
export const admitAuthenticatedStateQueueHeaderObservation = async ({
  observation,
  minimumConfirmationDepth,
}: {
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly minimumConfirmationDepth?: number;
}): Promise<AuthenticatedStateQueueHeaderObservation> => {
  const base = admitAuthenticatedL1Observation({
    observation,
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  const headerHash = requireHex(
    observation.headerHash,
    28,
    "malformed_header_hash",
    "observation.headerHash",
  );
  const derived = await Effect.runPromise(hashBlockHeader(observation.header));
  if (derived !== headerHash) {
    reject(
      "header_hash_mismatch",
      `observation.headerHash=${headerHash} derived=${derived}`,
    );
  }
  return Object.freeze({
    ...base,
    headerHash,
    header: observation.header,
  });
};

/** Exact authentication of the normative transaction-source MPF root. */
export type TransactionsInclusionRootAuthentication = {
  readonly headerTransactionsRoot: string;
  readonly l2TransactionCount: bigint;
  /** MPF root over `(tx_id -> Data(L2TransactionSourceV1))`. */
  readonly sourceValuePhasRoot: string;
  readonly sourceValueCountedRoot: string;
  readonly sourceValueCount: bigint;
  readonly sourceInclusionAuthenticated: boolean;
};

/**
 * Fail-closed gate for the one normative source-leaf convention. Compact-only
 * leaves are not a compatibility route and cannot satisfy this API.
 */
export const assertTransactionSourceInclusionRootAuthenticated = (
  authentication: TransactionsInclusionRootAuthentication,
): TransactionsInclusionRootAuthentication => {
  if (
    !authentication.sourceInclusionAuthenticated ||
    authentication.sourceValueCount !== authentication.l2TransactionCount
  ) {
    reject(
      "transaction_source_inclusion_root_unauthenticated",
      [
        `header_transactions_root=${authentication.headerTransactionsRoot}`,
        `source_value_counted_root=${authentication.sourceValueCountedRoot}`,
        `source_value_count=${authentication.sourceValueCount.toString()}`,
        `l2_transaction_count=${authentication.l2TransactionCount.toString()}`,
      ].join(" "),
    );
  }
  return authentication;
};
