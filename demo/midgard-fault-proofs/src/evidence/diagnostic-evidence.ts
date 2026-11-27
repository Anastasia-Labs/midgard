/**
 * Labelled diagnostic evidence.
 *
 * Family output 7 (`docs/spec/protocol-decisions.md#family-outputs`) permits
 * operator-private REST/DB/file inputs only as
 * "labelled diagnostics". This module is the single place where such inputs may
 * enter the fault-proof tooling, and every record it produces carries
 * `grade: "diagnostic"` plus a mandatory label, so
 * `assertSecurityGradeEvidenceV1` rejects it and no canonical builder can
 * consume it by accident.
 *
 * Operator-private node and file surfaces, including `--midgard-node-url`
 * evidence preparation, remain diagnostic-only.
 */
import { type EvidenceProvenance } from "@al-ft/midgard-sdk/fraud-proof";

export const MIDGARD_NODE_URL_DIAGNOSTIC_LABEL =
  "operator node REST endpoint: diagnostic and import only, never a security input for a submitted proof" as const;

export const LOCAL_FILE_DIAGNOSTIC_LABEL =
  "operator-local file import: diagnostic and import only, never a security input for a submitted proof" as const;

export const SAMPLE_EVIDENCE_DIAGNOSTIC_LABEL =
  "generated sample evidence: diagnostic only, never an authenticated L1/public-DA security input for a submitted proof" as const;

/** Provenance every `--midgard-node-url` evidence import carries. */
export const MIDGARD_NODE_URL_DIAGNOSTIC_PROVENANCE: EvidenceProvenance = {
  trustClass: "operator_admin_api",
  sourceId: "midgard-node-url",
  grade: "diagnostic",
  diagnosticLabel: MIDGARD_NODE_URL_DIAGNOSTIC_LABEL,
};

/** Provenance every operator-local file evidence import carries. */
export const LOCAL_FILE_DIAGNOSTIC_PROVENANCE: EvidenceProvenance = {
  trustClass: "operator_private_file",
  sourceId: "transactions-file",
  grade: "diagnostic",
  diagnosticLabel: LOCAL_FILE_DIAGNOSTIC_LABEL,
};

export const SAMPLE_EVIDENCE_DIAGNOSTIC_PROVENANCE: EvidenceProvenance = {
  trustClass: "operator_only_diagnostic_endpoint",
  sourceId: "sample-double-spend",
  grade: "diagnostic",
  diagnosticLabel: SAMPLE_EVIDENCE_DIAGNOSTIC_LABEL,
};

/**
 * Human-readable, secret-free banner for CLI paths that consume diagnostic
 * evidence, so an operator can never mistake a diagnostic run for a
 * canonical-evidence run.
 */
export const diagnosticEvidenceBanner = (
  provenance: EvidenceProvenance,
): string =>
  `DIAGNOSTIC EVIDENCE (${provenance.trustClass}/${provenance.sourceId}): ${
    provenance.diagnosticLabel ?? ""
  }`;
