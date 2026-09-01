/**
 * Labelled diagnostic evidence (`Q03`).
 *
 * `GOAL_SPEC.md` §9.2 permits operator-private REST/DB/file inputs only as
 * "labelled diagnostics". This module is the single place where such inputs may
 * enter the fault-proof tooling, and every record it produces carries
 * `grade: "diagnostic"` plus a mandatory label, so
 * `assertSecurityGradeEvidenceV1` rejects it and no canonical builder can
 * consume it by accident.
 *
 * Operator-private node and file surfaces, including `--midgard-node-url`
 * evidence preparation, remain diagnostic-only.
 */
import {
  admitEvidenceProvenanceV1,
  type EvidenceProvenanceV1,
} from "@al-ft/midgard-sdk";

import {
  type FetchLike,
  fetchNodeBlockTransactions,
  type NodeTransactionPayload,
  readNodeTransactionPayloadsFile,
} from "../prepare-double-spend.js";

export const DIAGNOSTIC_EVIDENCE_V1_SCHEMA_VERSION =
  "midgard-diagnostic-evidence-v1" as const;

export const MIDGARD_NODE_URL_DIAGNOSTIC_LABEL_V1 =
  "operator node REST endpoint: diagnostic and import only, never a security input for a submitted proof" as const;

export const LOCAL_FILE_DIAGNOSTIC_LABEL_V1 =
  "operator-local file import: diagnostic and import only, never a security input for a submitted proof" as const;

export const SAMPLE_EVIDENCE_DIAGNOSTIC_LABEL_V1 =
  "generated sample evidence: diagnostic only, never an authenticated L1/public-DA security input for a submitted proof" as const;

/** Provenance every `--midgard-node-url` evidence import carries. */
export const MIDGARD_NODE_URL_DIAGNOSTIC_PROVENANCE_V1: EvidenceProvenanceV1 = {
  trustClass: "operator_admin_api",
  sourceId: "midgard-node-url",
  grade: "diagnostic",
  diagnosticLabel: MIDGARD_NODE_URL_DIAGNOSTIC_LABEL_V1,
};

/** Provenance every operator-local file evidence import carries. */
export const LOCAL_FILE_DIAGNOSTIC_PROVENANCE_V1: EvidenceProvenanceV1 = {
  trustClass: "operator_private_file",
  sourceId: "transactions-file",
  grade: "diagnostic",
  diagnosticLabel: LOCAL_FILE_DIAGNOSTIC_LABEL_V1,
};

export const SAMPLE_EVIDENCE_DIAGNOSTIC_PROVENANCE_V1: EvidenceProvenanceV1 = {
  trustClass: "operator_only_diagnostic_endpoint",
  sourceId: "sample-double-spend",
  grade: "diagnostic",
  diagnosticLabel: SAMPLE_EVIDENCE_DIAGNOSTIC_LABEL_V1,
};

export type DiagnosticBlockTransactionsV1 = {
  readonly schemaVersion: typeof DIAGNOSTIC_EVIDENCE_V1_SCHEMA_VERSION;
  readonly provenance: EvidenceProvenanceV1;
  readonly headerHash: string;
  readonly transactions: readonly NodeTransactionPayload[];
};

const diagnosticProvenance = ({
  trustClass,
  sourceId,
  diagnosticLabel,
}: {
  readonly trustClass:
    | "operator_admin_api"
    | "operator_private_file"
    | "operator_only_diagnostic_endpoint";
  readonly sourceId: string;
  readonly diagnosticLabel: string;
}): EvidenceProvenanceV1 =>
  admitEvidenceProvenanceV1({
    provenance: { trustClass, sourceId, grade: "diagnostic", diagnosticLabel },
    allowDiagnostic: true,
  });

/**
 * Diagnostic import of a block's transactions from the operator node's REST
 * API. The result is unusable as security evidence by construction.
 */
export const diagnosticBlockTransactionsFromMidgardNodeV1 = async ({
  midgardNodeUrl,
  headerHash,
  fetchImpl,
}: {
  readonly midgardNodeUrl: string;
  readonly headerHash: string;
  readonly fetchImpl?: FetchLike;
}): Promise<DiagnosticBlockTransactionsV1> => ({
  schemaVersion: DIAGNOSTIC_EVIDENCE_V1_SCHEMA_VERSION,
  provenance: diagnosticProvenance({
    trustClass: "operator_admin_api",
    sourceId: "midgard-node-url",
    diagnosticLabel: MIDGARD_NODE_URL_DIAGNOSTIC_LABEL_V1,
  }),
  headerHash,
  transactions: await fetchNodeBlockTransactions({
    midgardNodeUrl,
    headerHash,
    ...(fetchImpl === undefined ? {} : { fetchImpl }),
  }),
});

/** Diagnostic import of a block's transactions from an operator-local file. */
export const diagnosticBlockTransactionsFromFileV1 = async ({
  transactionsPath,
  headerHash,
}: {
  readonly transactionsPath: string;
  readonly headerHash: string;
}): Promise<DiagnosticBlockTransactionsV1> => ({
  schemaVersion: DIAGNOSTIC_EVIDENCE_V1_SCHEMA_VERSION,
  provenance: diagnosticProvenance({
    trustClass: "operator_private_file",
    sourceId: "transactions-file",
    diagnosticLabel: LOCAL_FILE_DIAGNOSTIC_LABEL_V1,
  }),
  headerHash,
  transactions: await readNodeTransactionPayloadsFile(transactionsPath),
});

/**
 * Human-readable, secret-free banner for CLI paths that consume diagnostic
 * evidence, so an operator can never mistake a diagnostic run for a
 * canonical-evidence run.
 */
export const diagnosticEvidenceBannerV1 = (
  provenance: EvidenceProvenanceV1,
): string =>
  `DIAGNOSTIC EVIDENCE (${provenance.trustClass}/${provenance.sourceId}): ${
    provenance.diagnosticLabel ?? ""
  }`;
