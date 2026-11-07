import { createHash } from "node:crypto";

import {
  admitAuthenticatedStateQueueHeaderObservation,
  type AuthenticatedL1ChainPoint,
  type AuthenticatedStateQueueHeaderObservation,
  CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
  type EvidenceProvenance,
  Header,
  type L1SourceMode,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { WatcherStateQueueHeader } from "../indexers/state-queue-snapshot.js";

export const WATCHER_HEADER_ROOT_RECONSTRUCTION_SCHEMA_VERSION =
  "midgard-watcher-header-root-reconstruction-v1" as const;

/**
 * The eight header-bound roots, in the exact order and under the exact names
 * `rootMismatches` (demo/midgard-fault-proofs/src/transition-trace/reconstruct.ts)
 * emits.
 */
export const WATCHER_HEADER_ROOT_FIELDS = [
  "utxos_root",
  "withdrawals_root",
  "forced_transactions_root",
  "transactions_root",
  "deposits_root",
  "transition_trace_root",
  "event_to_step_root",
  "validation_traces_root",
] as const;

/** The seven header-bound counts, in `countMismatches` order and naming. */
export const WATCHER_HEADER_COUNT_FIELDS = [
  "withdrawal_count",
  "forced_transaction_count",
  "l2_transaction_count",
  "deposit_count",
  "total_event_count",
  "transition_step_count",
  "validation_trace_count",
] as const;

export type WatcherHeaderRootField =
  (typeof WATCHER_HEADER_ROOT_FIELDS)[number];

export type WatcherHeaderCountField =
  (typeof WATCHER_HEADER_COUNT_FIELDS)[number];

export type WatcherHeaderRootSet = Readonly<
  Record<WatcherHeaderRootField, string>
>;

/** Counts as canonical decimal strings, so the record is plain JSON. */
export type WatcherHeaderCountSet = Readonly<
  Record<WatcherHeaderCountField, string>
>;

/**
 * Every reason this evaluation can report, in a fixed total order. The first
 * sixteen are the SDK's `CanonicalEvidenceRejectionCodeV1` values, reported
 * unchanged so an admission failure keeps its canonical name; the rest are the
 * reconstruction outcomes.
 */
export const WATCHER_HEADER_ROOT_RECONSTRUCTION_REASON_CODES = [
  "unknown_trust_class",
  "prohibited_trust_class",
  "diagnostic_grade_not_admitted",
  "missing_diagnostic_label",
  "diagnostic_label_on_security_evidence",
  "empty_source_id",
  "unknown_l1_source_mode",
  "l1_observation_wrong_trust_class",
  "insufficient_confirmation_depth",
  "malformed_chain_point",
  "malformed_header_hash",
  "header_hash_mismatch",
  "da_evidence_wrong_trust_class",
  "payload_header_mismatch",
  "native_inclusion_root_unauthenticated",
  "evidence_grade_mismatch",
  "malformed_payload",
  "non_canonical_payload",
  "wrong_payload_version",
  "invalid_payload_entries",
  "duplicate_source_event_key",
  "root_mismatch",
  "count_mismatch",
  "declared_counts_member_mismatch",
  "unenumerated_root_mismatch",
  "unenumerated_count_mismatch",
  "unexpected_reconstruction_failure",
] as const;

export type WatcherHeaderRootReconstructionReasonCode =
  (typeof WATCHER_HEADER_ROOT_RECONSTRUCTION_REASON_CODES)[number];

export type WatcherHeaderRootReconstructionResult = Readonly<{
  schemaVersion: typeof WATCHER_HEADER_ROOT_RECONSTRUCTION_SCHEMA_VERSION;
  action: "accept" | "reject";
  reasonCodes: readonly WatcherHeaderRootReconstructionReasonCode[];
  /** Diverging root field names, ordered by `WATCHER_HEADER_ROOT_FIELDS`. */
  rootMismatches: readonly WatcherHeaderRootField[];
  /** Diverging count field names, ordered by `WATCHER_HEADER_COUNT_FIELDS`. */
  countMismatches: readonly WatcherHeaderCountField[];
  /** Null when the canonical reconstruction did not reach a root set. */
  reconstructedRoots: WatcherHeaderRootSet | null;
  reconstructedCounts: WatcherHeaderCountSet | null;
  /** Always the L1-observed header's own values - never the payload's. */
  headerRoots: WatcherHeaderRootSet;
  headerCounts: WatcherHeaderCountSet;
  headerHash: string;
  headerPrevUtxosRoot: string;
  payloadEnvelopeSha256: string;
  /** Null when the envelope could not be unwrapped. */
  payloadSha256: string | null;
  resultDigest: string;
}>;

export type WatcherHeaderRootReconstructionErrorCode =
  | "invalid_header_record"
  | "header_cbor_mismatch"
  | "invalid_input_ids"
  | "result_not_accepted"
  | "unsupported_schema";

export class WatcherHeaderRootReconstructionError extends Error {
  readonly code: WatcherHeaderRootReconstructionErrorCode;
  readonly path: string;

  constructor(code: WatcherHeaderRootReconstructionErrorCode, path: string) {
    super(`${code}: ${path}`);
    this.name = "WatcherHeaderRootReconstructionError";
    this.code = code;
    this.path = path;
  }
}

export const fail = (
  code: WatcherHeaderRootReconstructionErrorCode,
  path: string,
): never => {
  throw new WatcherHeaderRootReconstructionError(code, path);
};

const HEX_28 = /^[0-9a-f]{56}$/u;

const HEX_32 = /^[0-9a-f]{64}$/u;

const HEX_BYTES = /^(?:[0-9a-f]{2})+$/u;

export const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const sha256Hex = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

const hex32 = (value: string, path: string): string => {
  if (!HEX_32.test(value)) {
    fail("invalid_header_record", path);
  }
  return value;
};

const hex28 = (value: string, path: string): string => {
  if (!HEX_28.test(value)) {
    fail("invalid_header_record", path);
  }
  return value;
};

const natural = (value: string, path: string): bigint => {
  if (!NATURAL.test(value)) {
    fail("invalid_header_record", path);
  }
  return BigInt(value);
};

// ---------------------------------------------------------------------------
// Non-circular header binding: the state-queue header record is the only header source
// ---------------------------------------------------------------------------

/**
 * Rebuilds the canonical `Header` struct from a state-queue header record
 * and admits it as an authenticated L1 observation.
 *
 * Provenance of every field: `header` is the decoded L1 state-queue node datum
 * (`state-queue-snapshot.ts` `WatcherStateQueueHeader`); `chainPoint`,
 * `confirmationDepth`, and `sourceMode` describe the L1 read that produced it;
 * `provenance` must be `authenticated_cardano_l1` (enforced by the SDK
 * admission). No argument of this function may originate from a DA payload.
 */
export const makeWatcherAuthenticatedHeaderObservation = async (input: {
  readonly header: WatcherStateQueueHeader;
  readonly chainPoint: AuthenticatedL1ChainPoint;
  readonly confirmationDepth: number;
  readonly sourceMode: L1SourceMode;
  readonly provenance: EvidenceProvenance;
  readonly minimumConfirmationDepth?: number;
}): Promise<AuthenticatedStateQueueHeaderObservation> => {
  const record = input.header;
  const header: Header = {
    prevUtxosRoot: hex32(record.prevUtxosRoot, "$.header.prevUtxosRoot"),
    utxosRoot: hex32(record.utxosRoot, "$.header.utxosRoot"),
    withdrawalsRoot: hex32(record.withdrawalsRoot, "$.header.withdrawalsRoot"),
    forcedTransactionsRoot: hex32(
      record.forcedTransactionsRoot,
      "$.header.forcedTransactionsRoot",
    ),
    transactionsRoot: hex32(
      record.transactionsRoot,
      "$.header.transactionsRoot",
    ),
    depositsRoot: hex32(record.depositsRoot, "$.header.depositsRoot"),
    transitionTraceRoot: hex32(
      record.transitionTraceRoot,
      "$.header.transitionTraceRoot",
    ),
    eventToStepRoot: hex32(record.eventToStepRoot, "$.header.eventToStepRoot"),
    validationTracesRoot: hex32(
      record.validationTracesRoot,
      "$.header.validationTracesRoot",
    ),
    withdrawalCount: natural(
      record.withdrawalCount,
      "$.header.withdrawalCount",
    ),
    forcedTransactionCount: natural(
      record.forcedTransactionCount,
      "$.header.forcedTransactionCount",
    ),
    l2TransactionCount: natural(
      record.l2TransactionCount,
      "$.header.l2TransactionCount",
    ),
    depositCount: natural(record.depositCount, "$.header.depositCount"),
    totalEventCount: natural(
      record.totalEventCount,
      "$.header.totalEventCount",
    ),
    transitionStepCount: natural(
      record.transitionStepCount,
      "$.header.transitionStepCount",
    ),
    validationTraceCount: natural(
      record.validationTraceCount,
      "$.header.validationTraceCount",
    ),
    startTime: natural(record.startTime, "$.header.startTime"),
    endTime: natural(record.endTime, "$.header.endTime"),
    blockSlot: natural(record.blockSlot, "$.header.blockSlot"),
    expectedNetworkId: natural(
      record.expectedNetworkId,
      "$.header.expectedNetworkId",
    ),
    minFeeA: natural(record.minFeeA, "$.header.minFeeA"),
    minFeeB: natural(record.minFeeB, "$.header.minFeeB"),
    prevHeaderHash: hex28(record.prevHeaderHash, "$.header.prevHeaderHash"),
    operatorVkey: hex28(record.operatorVkey, "$.header.operatorVkey"),
    protocolVersion: natural(
      record.protocolVersion,
      "$.header.protocolVersion",
    ),
  };
  if (!HEX_BYTES.test(record.headerCborHex)) {
    fail("invalid_header_record", "$.header.headerCborHex");
  }
  // The rebuilt struct must re-encode to the exact datum bytes read from the
  // L1 state-queue UTxO. Field-level drift between the record and the struct handed to
  // the canonical hasher is impossible past this point.
  if (Data.to(header, Header) !== record.headerCborHex) {
    fail("header_cbor_mismatch", "$.header.headerCborHex");
  }
  // `admit...` re-derives the header hash canonically and rejects a
  // caller-supplied `headerHash` that does not match, an unknown source mode, a
  // non-L1 trust class, a malformed chain point, and insufficient depth.
  return await admitAuthenticatedStateQueueHeaderObservation({
    observation: {
      schemaVersion: CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
      sourceMode: input.sourceMode,
      provenance: input.provenance,
      chainPoint: input.chainPoint,
      confirmationDepth: input.confirmationDepth,
      headerHash: record.headerHash,
      header,
    },
    ...(input.minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth: input.minimumConfirmationDepth }),
  });
};

// ---------------------------------------------------------------------------
// Deterministic mismatch reporting
// ---------------------------------------------------------------------------

export const rootSetFromHeader = (header: Header): WatcherHeaderRootSet => ({
  utxos_root: header.utxosRoot,
  withdrawals_root: header.withdrawalsRoot,
  forced_transactions_root: header.forcedTransactionsRoot,
  transactions_root: header.transactionsRoot,
  deposits_root: header.depositsRoot,
  transition_trace_root: header.transitionTraceRoot,
  event_to_step_root: header.eventToStepRoot,
  validation_traces_root: header.validationTracesRoot,
});

export const countSetFromHeader = (header: Header): WatcherHeaderCountSet => ({
  withdrawal_count: header.withdrawalCount.toString(),
  forced_transaction_count: header.forcedTransactionCount.toString(),
  l2_transaction_count: header.l2TransactionCount.toString(),
  deposit_count: header.depositCount.toString(),
  total_event_count: header.totalEventCount.toString(),
  transition_step_count: header.transitionStepCount.toString(),
  validation_trace_count: header.validationTraceCount.toString(),
});
