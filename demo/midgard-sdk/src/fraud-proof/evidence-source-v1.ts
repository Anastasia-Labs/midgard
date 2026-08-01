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
 * The admitted/prohibited class lists are the exact `trustPolicy` lists of
 * `docs/exec-plans/evidence/canonical-v1-watcher-dependency-map-v1.json`, whose
 * `unknownBehavior` is `fail_closed`. Anything not enumerated is rejected; an
 * unknown class is never treated as admitted.
 */
import { Effect } from "effect";

import { type HeaderV1, hashBlockHeaderV1 } from "@/ledger-state.js";

export const CANONICAL_EVIDENCE_SOURCE_V1_SCHEMA_VERSION =
  "midgard-canonical-evidence-source-v1" as const;

/** Security inputs a fault-proof builder or watcher may rely on. */
export const ADMITTED_EVIDENCE_TRUST_CLASSES_V1 = [
  "authenticated_cardano_l1",
  "public_or_permissionless_da",
  "signed_deployment_identity",
  "deterministic_local_computation",
] as const;

export type AdmittedEvidenceTrustClassV1 =
  (typeof ADMITTED_EVIDENCE_TRUST_CLASSES_V1)[number];

/** Operator-private surfaces. Usable only as explicitly labelled diagnostics. */
export const PROHIBITED_EVIDENCE_TRUST_CLASSES_V1 = [
  "operator_private_database",
  "operator_admin_api",
  "operator_private_file",
  "operator_only_diagnostic_endpoint",
] as const;

export type ProhibitedEvidenceTrustClassV1 =
  (typeof PROHIBITED_EVIDENCE_TRUST_CLASSES_V1)[number];

export type EvidenceTrustClassV1 =
  | AdmittedEvidenceTrustClassV1
  | ProhibitedEvidenceTrustClassV1;

export type EvidenceGradeV1 = "security" | "diagnostic";

export const L1_SOURCE_MODES_V1 = ["local_node", "external_providers"] as const;
export type L1SourceModeV1 = (typeof L1_SOURCE_MODES_V1)[number];

export type EvidenceProvenanceV1 = {
  readonly trustClass: EvidenceTrustClassV1;
  /** Stable, non-secret identifier of the concrete source (peer id, mode id). */
  readonly sourceId: string;
  readonly grade: EvidenceGradeV1;
  /** Mandatory for `grade: "diagnostic"`, forbidden for `grade: "security"`. */
  readonly diagnosticLabel?: string;
};

export type CanonicalEvidenceRejectionCodeV1 =
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
  | "native_inclusion_root_unauthenticated"
  | "evidence_grade_mismatch";

/**
 * Deterministic, value-free rejection. `detail` carries only codes, field
 * names, and public identifiers so evidence errors stay secret-safe.
 */
export class CanonicalEvidenceRejectionV1 extends Error {
  readonly code: CanonicalEvidenceRejectionCodeV1;
  readonly detail: string;

  constructor(code: CanonicalEvidenceRejectionCodeV1, detail: string) {
    super(`${code}: ${detail}`);
    this.name = "CanonicalEvidenceRejectionV1";
    this.code = code;
    this.detail = detail;
  }
}

const reject = (
  code: CanonicalEvidenceRejectionCodeV1,
  detail: string,
): never => {
  throw new CanonicalEvidenceRejectionV1(code, detail);
};

export const isAdmittedEvidenceTrustClassV1 = (
  value: string,
): value is AdmittedEvidenceTrustClassV1 =>
  (ADMITTED_EVIDENCE_TRUST_CLASSES_V1 as readonly string[]).includes(value);

export const isProhibitedEvidenceTrustClassV1 = (
  value: string,
): value is ProhibitedEvidenceTrustClassV1 =>
  (PROHIBITED_EVIDENCE_TRUST_CLASSES_V1 as readonly string[]).includes(value);

export type AdmitEvidenceProvenanceOptionsV1 = {
  readonly provenance: EvidenceProvenanceV1;
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
export const admitEvidenceProvenanceV1 = ({
  provenance,
  allowDiagnostic = false,
}: AdmitEvidenceProvenanceOptionsV1): EvidenceProvenanceV1 => {
  const sourceId = provenance.sourceId.trim();
  if (sourceId.length === 0) {
    reject("empty_source_id", "provenance.sourceId");
  }
  const trustClass: string = provenance.trustClass;
  const admitted = isAdmittedEvidenceTrustClassV1(trustClass);
  const prohibited = isProhibitedEvidenceTrustClassV1(trustClass);
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
    return Object.freeze({ trustClass: provenance.trustClass, sourceId, grade: "security" });
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
export const assertSecurityGradeEvidenceV1 = (
  provenance: EvidenceProvenanceV1,
): EvidenceProvenanceV1 =>
  admitEvidenceProvenanceV1({ provenance, allowDiagnostic: false });

/** A bundle is security grade only when every contributing record is. */
export const combineEvidenceGradeV1 = (
  provenances: readonly EvidenceProvenanceV1[],
): EvidenceGradeV1 =>
  provenances.some((provenance) => provenance.grade !== "security")
    ? "diagnostic"
    : "security";

export const requireEvidenceTrustClassV1 = ({
  provenance,
  expected,
  code,
}: {
  readonly provenance: EvidenceProvenanceV1;
  readonly expected: AdmittedEvidenceTrustClassV1;
  readonly code: CanonicalEvidenceRejectionCodeV1;
}): EvidenceProvenanceV1 => {
  const admittedProvenance = assertSecurityGradeEvidenceV1(provenance);
  if (admittedProvenance.trustClass !== expected) {
    reject(
      code,
      `expected=${expected} actual=${admittedProvenance.trustClass}`,
    );
  }
  return admittedProvenance;
};

export type AuthenticatedL1ChainPointV1 = {
  readonly slot: bigint;
  /** 32-byte lowercase hex Cardano block hash. */
  readonly blockHash: string;
};

export type AuthenticatedL1ObservationV1 = {
  readonly schemaVersion: typeof CANONICAL_EVIDENCE_SOURCE_V1_SCHEMA_VERSION;
  readonly sourceMode: L1SourceModeV1;
  readonly provenance: EvidenceProvenanceV1;
  readonly chainPoint: AuthenticatedL1ChainPointV1;
  /** Confirmation depth already observed for `chainPoint`. */
  readonly confirmationDepth: number;
};

/**
 * An authenticated L1 observation of one state-queue block header. This is the
 * only accepted origin of the committed header a DA payload is checked against.
 */
export type AuthenticatedStateQueueHeaderObservationV1 =
  AuthenticatedL1ObservationV1 & {
    /** 28-byte lowercase hex header hash, as committed on L1. */
    readonly headerHash: string;
    readonly header: HeaderV1;
  };

const HEX = /^[0-9a-f]*$/u;

const requireHex = (
  value: string,
  byteLength: number,
  code: CanonicalEvidenceRejectionCodeV1,
  fieldName: string,
): string => {
  const normalized = value.trim().toLowerCase();
  if (normalized.length !== byteLength * 2 || !HEX.test(normalized)) {
    reject(code, `${fieldName} must be ${byteLength.toString()}-byte hex`);
  }
  return normalized;
};

export type AdmitAuthenticatedL1ObservationOptionsV1 = {
  readonly observation: AuthenticatedL1ObservationV1;
  /** Minimum confirmation depth the consumer requires. Default is 1. */
  readonly minimumConfirmationDepth?: number;
};

export const admitAuthenticatedL1ObservationV1 = ({
  observation,
  minimumConfirmationDepth = 1,
}: AdmitAuthenticatedL1ObservationOptionsV1): AuthenticatedL1ObservationV1 => {
  if (
    !Number.isSafeInteger(minimumConfirmationDepth) ||
    minimumConfirmationDepth < 1
  ) {
    reject(
      "insufficient_confirmation_depth",
      `minimumConfirmationDepth=${String(minimumConfirmationDepth)}`,
    );
  }
  if (observation.schemaVersion !== CANONICAL_EVIDENCE_SOURCE_V1_SCHEMA_VERSION) {
    reject(
      "evidence_grade_mismatch",
      `observation.schemaVersion=${String(observation.schemaVersion)}`,
    );
  }
  if (!(L1_SOURCE_MODES_V1 as readonly string[]).includes(observation.sourceMode)) {
    reject(
      "unknown_l1_source_mode",
      `observation.sourceMode=${String(observation.sourceMode)}`,
    );
  }
  const provenance = requireEvidenceTrustClassV1({
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
    schemaVersion: CANONICAL_EVIDENCE_SOURCE_V1_SCHEMA_VERSION,
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
export const admitAuthenticatedStateQueueHeaderObservationV1 = async ({
  observation,
  minimumConfirmationDepth,
}: {
  readonly observation: AuthenticatedStateQueueHeaderObservationV1;
  readonly minimumConfirmationDepth?: number;
}): Promise<AuthenticatedStateQueueHeaderObservationV1> => {
  const base = admitAuthenticatedL1ObservationV1({
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
  const derived = await Effect.runPromise(hashBlockHeaderV1(observation.header));
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

/**
 * Result of authenticating a raw transactions MPF root against the header's
 * counted `transactions_root`.
 *
 * Two leaf-value conventions exist in the tree today:
 *
 * - `payload_source_value` — `(tx_id -> Data(L2TransactionSourceV1))`, produced
 *   by the node (`encodeTransactionRootValue`) and re-derived by
 *   `reconstructDaPayloadV1`; this is what a committed header commits to.
 * - `native_compact_value` — `(tx_id -> native_tx_compact_cbor)`, which the
 *   deployed Aiken `verify_native_tx_in_state_queue_node` requires the prover's
 *   membership proof to open.
 *
 * Only the convention whose counted commitment equals the header's
 * `transactions_root` can back a submittable proof, so this record is checked
 * before any inclusion argument is built.
 */
export type TransactionsInclusionRootAuthenticationV1 = {
  readonly headerTransactionsRoot: string;
  readonly l2TransactionCount: bigint;
  readonly payloadSourcePhasRoot: string;
  readonly payloadSourceCountedRoot: string;
  readonly nativeCompactPhasRoot: string;
  readonly nativeCompactCountedRoot: string;
  /** True when the native-compact convention opens the committed header root. */
  readonly nativeInclusionAuthenticated: boolean;
  /** True when the payload-source convention opens the committed header root. */
  readonly payloadSourceAuthenticated: boolean;
};

/**
 * Fail-closed gate for native inclusion arguments: a builder must never hand a
 * prover a membership proof whose root cannot be authenticated on-chain.
 */
export const assertNativeInclusionRootAuthenticatedV1 = (
  authentication: TransactionsInclusionRootAuthenticationV1,
): TransactionsInclusionRootAuthenticationV1 => {
  if (!authentication.nativeInclusionAuthenticated) {
    reject(
      "native_inclusion_root_unauthenticated",
      [
        `header_transactions_root=${authentication.headerTransactionsRoot}`,
        `native_compact_counted_root=${authentication.nativeCompactCountedRoot}`,
        `payload_source_counted_root=${authentication.payloadSourceCountedRoot}`,
        `l2_transaction_count=${authentication.l2TransactionCount.toString()}`,
      ].join(" "),
    );
  }
  return authentication;
};
