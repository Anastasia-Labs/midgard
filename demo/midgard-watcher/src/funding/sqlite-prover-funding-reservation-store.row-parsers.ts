import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  parseWorkflowFundingCompletionHandoff,
  parseWorkflowFundingPreparedTransition,
  parseWorkflowFundingSubmissionHandoff,
  type WorkflowFundingCompletionHandoff,
  type WorkflowFundingPreparedTransition,
  type WorkflowFundingSubmissionHandoff,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  parseWatcherProverFundingReservationRecord,
  type WatcherProverFundingReservationRecord,
} from "./prover-funding-reservation.js";
import { HEX_32 } from "./sqlite-prover-funding-reservation-store.derive-signed-transition.js";

/** A handoff's digest apart from where a workflow journal anchors it. */
export const unanchored = (handoff: object) =>
  computeDeploymentManifestJsonDigest({
    ...handoff,
    preparedArtifactDigest: null,
    expectedJournalSequence: null,
  });

/** Row parsers of the prover funding store: each verifies one row against
 * the digests it carries and depends on nothing else. */
export type HandoffRow = Readonly<{
  reservation_id: unknown;
  kind: unknown;
  identity_digest: unknown;
  record_digest: unknown;
  canonical_json: unknown;
}>;
export type SubmissionHandoff = Readonly<{
  kind: "submission";
  reservationId: string;
  identityDigest: string;
  transition: WorkflowFundingPreparedTransition;
  handoff: WorkflowFundingSubmissionHandoff;
}>;
export type CompletionHandoff = Readonly<{
  kind: "completion";
  reservationId: string;
  identityDigest: string;
  handoff: WorkflowFundingCompletionHandoff;
}>;
export type RecordRow = Readonly<{
  reservation_id: unknown;
  record_digest: unknown;
  canonical_json: unknown;
}>;
export type LeaseRow = Readonly<{
  out_ref: unknown;
  reservation_id: unknown;
  lease_phase: unknown;
  role: unknown;
}>;
export type LineageRow = Readonly<{
  reservation_id: unknown;
  action_kind: unknown;
  output_index: unknown;
  out_ref: unknown;
  resolved_output_cbor_hex: unknown;
  transition_digest: unknown;
  phase: unknown;
  lineage_digest: unknown;
}>;

export const parseHandoffRow = (
  row: HandoffRow,
): SubmissionHandoff | CompletionHandoff => {
  if (
    typeof row.reservation_id !== "string" ||
    !HEX_32.test(row.reservation_id) ||
    typeof row.identity_digest !== "string" ||
    !HEX_32.test(row.identity_digest) ||
    typeof row.record_digest !== "string" ||
    !HEX_32.test(row.record_digest) ||
    typeof row.canonical_json !== "string" ||
    (row.kind !== "submission" && row.kind !== "completion")
  )
    throw new Error("prover funding handoff row is malformed");
  const data: unknown = JSON.parse(row.canonical_json);
  if (
    watcherCanonicalJson(data) !== row.canonical_json ||
    computeDeploymentManifestJsonDigest(data) !== row.record_digest
  )
    throw new Error("prover funding handoff row digest mismatch");
  if (row.kind === "completion") {
    const handoff = parseWorkflowFundingCompletionHandoff(data);
    if (computeDeploymentManifestJsonDigest(handoff) !== row.identity_digest)
      throw new Error("prover funding completion identity mismatch");
    return {
      kind: "completion",
      reservationId: row.reservation_id,
      identityDigest: row.identity_digest,
      handoff,
    };
  }
  if (
    data === null ||
    typeof data !== "object" ||
    Array.isArray(data) ||
    Object.keys(data).sort().join(",") !== "handoff,transition" ||
    !("handoff" in data) ||
    !("transition" in data)
  )
    throw new Error("prover funding submission handoff is malformed");
  const transition = parseWorkflowFundingPreparedTransition(data.transition);
  const handoff = parseWorkflowFundingSubmissionHandoff(data.handoff);
  if (
    computeDeploymentManifestJsonDigest(transition) !== row.identity_digest ||
    handoff.preflight.txHash !== transition.transactionHash ||
    handoff.submissionIntent.txHash !== transition.transactionHash
  )
    throw new Error(
      "prover funding submission handoff differs from signed transaction",
    );
  return {
    kind: "submission",
    reservationId: row.reservation_id,
    identityDigest: row.identity_digest,
    transition,
    handoff,
  };
};
export const parseLineageRow = (row: LineageRow) => {
  if (
    typeof row.reservation_id !== "string" ||
    !HEX_32.test(row.reservation_id) ||
    typeof row.action_kind !== "string" ||
    !/^[a-z][a-zA-Z0-9_.:-]{0,127}$/u.test(row.action_kind) ||
    !Number.isSafeInteger(row.output_index) ||
    (row.output_index as number) < 0 ||
    typeof row.out_ref !== "string" ||
    !/^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(row.out_ref) ||
    typeof row.resolved_output_cbor_hex !== "string" ||
    !/^(?:[0-9a-f]{2})+$/u.test(row.resolved_output_cbor_hex) ||
    typeof row.transition_digest !== "string" ||
    !HEX_32.test(row.transition_digest) ||
    (row.phase !== "pending" && row.phase !== "confirmed") ||
    typeof row.lineage_digest !== "string" ||
    !HEX_32.test(row.lineage_digest)
  ) {
    throw new Error("prover funding lineage row is malformed");
  }
  const identity = Object.freeze({
    reservationId: row.reservation_id,
    sourceActionKind: row.action_kind,
    sourceOutputIndex: row.output_index as number,
    outRef: row.out_ref,
    resolvedOutputCborHex: row.resolved_output_cbor_hex,
    transitionDigest: row.transition_digest,
  });
  const output = CML.TransactionOutput.from_cbor_hex(
    identity.resolvedOutputCborHex,
  );
  if (
    output.to_canonical_cbor_hex() !== identity.resolvedOutputCborHex ||
    computeDeploymentManifestJsonDigest(identity) !== row.lineage_digest
  ) {
    throw new Error("prover funding lineage row digest mismatch");
  }
  return Object.freeze({ ...identity, phase: row.phase });
};
export const parseRecordRow = (
  row: RecordRow,
): WatcherProverFundingReservationRecord => {
  if (
    typeof row.reservation_id !== "string" ||
    !HEX_32.test(row.reservation_id) ||
    typeof row.record_digest !== "string" ||
    !HEX_32.test(row.record_digest) ||
    typeof row.canonical_json !== "string"
  ) {
    throw new Error("prover funding reservation row is malformed");
  }
  let value: unknown;
  try {
    value = JSON.parse(row.canonical_json);
  } catch {
    throw new Error("prover funding reservation row is malformed");
  }
  const record = parseWatcherProverFundingReservationRecord(value);
  if (
    watcherCanonicalJson(record) !== row.canonical_json ||
    record.reservationId !== row.reservation_id ||
    record.recordDigest !== row.record_digest
  ) {
    throw new Error("prover funding reservation row metadata mismatch");
  }
  return record;
};
