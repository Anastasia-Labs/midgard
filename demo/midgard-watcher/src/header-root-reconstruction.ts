/**
 * Header/root reconstruction (GOAL_SPEC 10.3 W22).
 *
 * The watcher must be able to say, for one state-queue block, whether the
 * public DA payload reconstructs *exactly* the root set and count set the
 * operator committed on L1 - and, when it does not, which fields diverge, with
 * no dependence on the operator's own claim about what the block contains.
 *
 * Two rules govern this module.
 *
 * 1. REUSE, NEVER RE-DERIVE. The reconstruction itself is the canonical one:
 *    `reconstructDaPayloadV1` from `@al-ft/midgard-fault-proofs`, reached
 *    through the Q03 evidence core
 *    `canonicalBlockEvidenceFromVerifiedPayloadV1`. The watcher owns no second
 *    root algorithm, so there is no watcher-vs-producer differential to keep in
 *    agreement: an agreement test here is an identity, not a comparison. The
 *    mismatch vocabulary is likewise the canonical one - the eight
 *    `rootMismatches` names and the seven `countMismatches` names - re-declared
 *    here only as a fixed total order (`WATCHER_HEADER_ROOT_FIELDS_V1`,
 *    `WATCHER_HEADER_COUNT_FIELDS_V1`) so the reported lists are stable
 *    regardless of the order the producer emits them in.
 *
 * 2. NON-CIRCULAR BINDING. The expected root/count set comes from ONE place:
 *    the W14 authenticated state-queue index record
 *    (`WatcherStateQueueHeaderV1`, read from the L1 state-queue UTxO datum).
 *    `makeWatcherAuthenticatedHeaderObservationV1` rebuilds the `HeaderV1`
 *    struct from that record's fields only, re-encodes it and requires the
 *    bytes to equal the datum's own `headerCborHex`, and then hands it to
 *    `admitAuthenticatedStateQueueHeaderObservationV1`, which re-derives the
 *    header hash with the canonical SDK hasher. A caller therefore cannot pair
 *    an arbitrary header with a real header hash, and cannot introduce a header
 *    field that L1 did not commit.
 *
 *    The payload's own embedded header is NEVER promoted to "expected". It is
 *    only ever a claim that must equal the L1-observed header byte for byte
 *    (`committedHeader` inside the Q03 core) and hash to the L1-observed header
 *    hash (`expectedHeaderHash`). A payload that is perfectly self-consistent
 *    but describes a different block is rejected before any root is compared.
 *    There is no code path in this module that accepts an operator-supplied
 *    root set, count set, or header.
 *
 * Bytes are arguments, not transports. The envelope bytes come from the W21
 * hash-addressed canonical block store (`canonical-block-store.ts`), which
 * persisted exactly what a public DA peer served; this module never fetches.
 * `daProvenance` must be `public_or_permissionless_da` or the evaluation fails
 * closed.
 *
 * Every evaluation returns a versioned, canonical-JSON digest-bound record
 * (the finality-engine pattern), so two runs over the same bytes produce the
 * same `resultDigest`, and a decision can be replayed from the record alone.
 */

import { createHash } from "node:crypto";

import {
  canonicalBlockEvidenceFromVerifiedPayloadV1,
  TransitionTraceChallengerError,
} from "@al-ft/midgard-fault-proofs";
import {
  admitAuthenticatedStateQueueHeaderObservationV1,
  type AuthenticatedL1ChainPointV1,
  type AuthenticatedStateQueueHeaderObservationV1,
  CANONICAL_EVIDENCE_SOURCE_V1_SCHEMA_VERSION,
  CanonicalEvidenceRejectionV1,
  type EvidenceProvenanceV1,
  HeaderV1,
  type L1SourceModeV1,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborUnsigned,
} from "../../midgard-core/src/codec/cbor.js";
import { unwrapDaPayloadV1 } from "../../midgard-core/src/da-payload-envelope.js";
import { DA_TRANSPORT_LIMITS_V1 } from "../../midgard-core/src/da-transport.js";
import {
  makeWatcherDurablePayloadV1,
  type WatcherReconstructedStateV1,
  watcherSha256CanonicalJsonV1,
} from "./durable-store.js";
import type { WatcherStateQueueHeaderV1 } from "./state-queue-indexer.js";

export const WATCHER_HEADER_ROOT_RECONSTRUCTION_V1_SCHEMA_VERSION =
  "midgard-watcher-header-root-reconstruction-v1" as const;

/**
 * The eight header-bound roots, in the exact order and under the exact names
 * `rootMismatches` (demo/midgard-fault-proofs/src/transition-trace/reconstruct.ts)
 * emits.
 */
export const WATCHER_HEADER_ROOT_FIELDS_V1 = [
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
export const WATCHER_HEADER_COUNT_FIELDS_V1 = [
  "withdrawal_count",
  "forced_transaction_count",
  "l2_transaction_count",
  "deposit_count",
  "total_event_count",
  "transition_step_count",
  "validation_trace_count",
] as const;

export type WatcherHeaderRootFieldV1 =
  (typeof WATCHER_HEADER_ROOT_FIELDS_V1)[number];
export type WatcherHeaderCountFieldV1 =
  (typeof WATCHER_HEADER_COUNT_FIELDS_V1)[number];

export type WatcherHeaderRootSetV1 = Readonly<
  Record<WatcherHeaderRootFieldV1, string>
>;
/** Counts as canonical decimal strings, so the record is plain JSON. */
export type WatcherHeaderCountSetV1 = Readonly<
  Record<WatcherHeaderCountFieldV1, string>
>;

/**
 * Every reason this evaluation can report, in a fixed total order. The first
 * sixteen are the SDK's `CanonicalEvidenceRejectionCodeV1` values, reported
 * unchanged so an admission failure keeps its canonical name; the rest are the
 * reconstruction outcomes.
 */
export const WATCHER_HEADER_ROOT_RECONSTRUCTION_REASON_CODES_V1 = [
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

export type WatcherHeaderRootReconstructionReasonCodeV1 =
  (typeof WATCHER_HEADER_ROOT_RECONSTRUCTION_REASON_CODES_V1)[number];

export type WatcherHeaderRootReconstructionResultV1 = Readonly<{
  schemaVersion: typeof WATCHER_HEADER_ROOT_RECONSTRUCTION_V1_SCHEMA_VERSION;
  action: "accept" | "reject";
  reasonCodes: readonly WatcherHeaderRootReconstructionReasonCodeV1[];
  /** Diverging root field names, ordered by `WATCHER_HEADER_ROOT_FIELDS_V1`. */
  rootMismatches: readonly WatcherHeaderRootFieldV1[];
  /** Diverging count field names, ordered by `WATCHER_HEADER_COUNT_FIELDS_V1`. */
  countMismatches: readonly WatcherHeaderCountFieldV1[];
  /** Null when the canonical reconstruction did not reach a root set. */
  reconstructedRoots: WatcherHeaderRootSetV1 | null;
  reconstructedCounts: WatcherHeaderCountSetV1 | null;
  /** Always the L1-observed header's own values - never the payload's. */
  headerRoots: WatcherHeaderRootSetV1;
  headerCounts: WatcherHeaderCountSetV1;
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

const fail = (
  code: WatcherHeaderRootReconstructionErrorCode,
  path: string,
): never => {
  throw new WatcherHeaderRootReconstructionError(code, path);
};

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const HEX_BYTES = /^(?:[0-9a-f]{2})+$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

const sha256Hex = (bytes: Uint8Array): string =>
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
// Non-circular header binding: the W14 record is the only header source
// ---------------------------------------------------------------------------

/**
 * Rebuilds the canonical `HeaderV1` struct from a W14 state-queue index record
 * and admits it as an authenticated L1 observation.
 *
 * Provenance of every field: `header` is the decoded L1 state-queue node datum
 * (`state-queue-indexer.ts` `WatcherStateQueueHeaderV1`); `chainPoint`,
 * `confirmationDepth`, and `sourceMode` describe the L1 read that produced it;
 * `provenance` must be `authenticated_cardano_l1` (enforced by the SDK
 * admission). No argument of this function may originate from a DA payload.
 */
export const makeWatcherAuthenticatedHeaderObservationV1 = async (input: {
  readonly header: WatcherStateQueueHeaderV1;
  readonly chainPoint: AuthenticatedL1ChainPointV1;
  readonly confirmationDepth: number;
  readonly sourceMode: L1SourceModeV1;
  readonly provenance: EvidenceProvenanceV1;
  readonly minimumConfirmationDepth?: number;
}): Promise<AuthenticatedStateQueueHeaderObservationV1> => {
  const record = input.header;
  const header: HeaderV1 = {
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
  // The rebuilt struct must re-encode to the exact datum bytes the W14 indexer
  // read from L1. Field-level drift between the record and the struct handed to
  // the canonical hasher is impossible past this point.
  if (Data.to(header, HeaderV1) !== record.headerCborHex) {
    fail("header_cbor_mismatch", "$.header.headerCborHex");
  }
  // `admit...` re-derives the header hash canonically and rejects a
  // caller-supplied `headerHash` that does not match, an unknown source mode, a
  // non-L1 trust class, a malformed chain point, and insufficient depth.
  return await admitAuthenticatedStateQueueHeaderObservationV1({
    observation: {
      schemaVersion: CANONICAL_EVIDENCE_SOURCE_V1_SCHEMA_VERSION,
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

const rootSetFromHeader = (header: HeaderV1): WatcherHeaderRootSetV1 => ({
  utxos_root: header.utxosRoot,
  withdrawals_root: header.withdrawalsRoot,
  forced_transactions_root: header.forcedTransactionsRoot,
  transactions_root: header.transactionsRoot,
  deposits_root: header.depositsRoot,
  transition_trace_root: header.transitionTraceRoot,
  event_to_step_root: header.eventToStepRoot,
  validation_traces_root: header.validationTracesRoot,
});

const countSetFromHeader = (header: HeaderV1): WatcherHeaderCountSetV1 => ({
  withdrawal_count: header.withdrawalCount.toString(),
  forced_transaction_count: header.forcedTransactionCount.toString(),
  l2_transaction_count: header.l2TransactionCount.toString(),
  deposit_count: header.depositCount.toString(),
  total_event_count: header.totalEventCount.toString(),
  transition_step_count: header.transitionStepCount.toString(),
  validation_trace_count: header.validationTraceCount.toString(),
});

/** Shape of the canonical producer's `PayloadRootSet` / `PayloadCountSet`. */
type CanonicalRootSet = {
  readonly utxosRoot: string;
  readonly withdrawalsRoot: string;
  readonly forcedTransactionsRoot: string;
  readonly transactionsRoot: string;
  readonly depositsRoot: string;
  readonly transitionTraceRoot: string;
  readonly eventToStepRoot: string;
  readonly validationTracesRoot: string;
};

type CanonicalCountSet = {
  readonly withdrawalCount: bigint;
  readonly forcedTransactionCount: bigint;
  readonly l2TransactionCount: bigint;
  readonly depositCount: bigint;
  readonly totalEventCount: bigint;
  readonly transitionStepCount: bigint;
  readonly validationTraceCount: bigint;
};

const rootSetFromCanonical = (
  roots: CanonicalRootSet,
): WatcherHeaderRootSetV1 => ({
  utxos_root: roots.utxosRoot,
  withdrawals_root: roots.withdrawalsRoot,
  forced_transactions_root: roots.forcedTransactionsRoot,
  transactions_root: roots.transactionsRoot,
  deposits_root: roots.depositsRoot,
  transition_trace_root: roots.transitionTraceRoot,
  event_to_step_root: roots.eventToStepRoot,
  validation_traces_root: roots.validationTracesRoot,
});

const countSetFromCanonical = (
  counts: CanonicalCountSet,
): WatcherHeaderCountSetV1 => ({
  withdrawal_count: counts.withdrawalCount.toString(),
  forced_transaction_count: counts.forcedTransactionCount.toString(),
  l2_transaction_count: counts.l2TransactionCount.toString(),
  deposit_count: counts.depositCount.toString(),
  total_event_count: counts.totalEventCount.toString(),
  transition_step_count: counts.transitionStepCount.toString(),
  validation_trace_count: counts.validationTraceCount.toString(),
});

/**
 * Extracts the canonical mismatch field list a `rootMismatch`/`countMismatch`
 * error carries.
 *
 * The producer formats the list as `<prefix><name>,<name>.`; the names are
 * exactly the ones `rootMismatches`/`countMismatches` emit. Parsing is
 * deliberately strict: an unrecognised prefix, an empty list, or any token
 * outside the declared enumeration yields `null`, which the caller turns into
 * an `unenumerated_*` reason rather than a partial mismatch list. Coupling to
 * this format is guarded by the per-field mutation suite, which asserts every
 * one of the eight root names and seven count names is produced.
 */
const parseCanonicalFieldList = <Field extends string>(
  message: string,
  prefixes: readonly string[],
  fields: readonly Field[],
): readonly Field[] | null => {
  for (const prefix of prefixes) {
    if (!message.startsWith(prefix)) {
      continue;
    }
    const body = message.slice(prefix.length).replace(/\.$/u, "");
    if (body.length === 0) {
      return null;
    }
    const tokens = body.split(",");
    const known = new Set<string>(fields);
    if (!tokens.every((token) => known.has(token))) {
      return null;
    }
    const emitted = new Set<string>(tokens);
    return fields.filter((field) => emitted.has(field));
  }
  return null;
};

const ROOT_MISMATCH_PREFIXES = [
  "Payload roots do not match committed header: ",
] as const;

const COUNT_MISMATCH_PREFIXES = [
  "Payload counts do not match committed header: ",
  "Payload declared counts do not match payload member arrays: ",
] as const;

const orderReasonCodes = (
  codes: readonly string[],
): readonly WatcherHeaderRootReconstructionReasonCodeV1[] => {
  const present = new Set(codes);
  return WATCHER_HEADER_ROOT_RECONSTRUCTION_REASON_CODES_V1.filter((code) =>
    present.has(code),
  );
};

type Classification = {
  readonly reasonCodes: readonly string[];
  readonly rootMismatches: readonly WatcherHeaderRootFieldV1[];
  readonly countMismatches: readonly WatcherHeaderCountFieldV1[];
};

const classifyFailure = (error: unknown): Classification => {
  if (error instanceof CanonicalEvidenceRejectionV1) {
    return {
      reasonCodes: [error.code],
      rootMismatches: [],
      countMismatches: [],
    };
  }
  if (error instanceof TransitionTraceChallengerError) {
    switch (error.code) {
      case "malformedPayload":
        return {
          reasonCodes: ["malformed_payload"],
          rootMismatches: [],
          countMismatches: [],
        };
      case "nonCanonicalPayload":
        return {
          reasonCodes: ["non_canonical_payload"],
          rootMismatches: [],
          countMismatches: [],
        };
      case "wrongPayloadVersion":
        return {
          reasonCodes: ["wrong_payload_version"],
          rootMismatches: [],
          countMismatches: [],
        };
      case "invalidPayloadEntries":
        return {
          reasonCodes: error.message.includes("duplicate source event key")
            ? ["invalid_payload_entries", "duplicate_source_event_key"]
            : ["invalid_payload_entries"],
          rootMismatches: [],
          countMismatches: [],
        };
      case "headerMismatch":
        return {
          reasonCodes: ["payload_header_mismatch"],
          rootMismatches: [],
          countMismatches: [],
        };
      case "rootMismatch": {
        const fields = parseCanonicalFieldList(
          error.message,
          ROOT_MISMATCH_PREFIXES,
          WATCHER_HEADER_ROOT_FIELDS_V1,
        );
        return fields === null
          ? {
              reasonCodes: ["root_mismatch", "unenumerated_root_mismatch"],
              rootMismatches: [],
              countMismatches: [],
            }
          : {
              reasonCodes: ["root_mismatch"],
              rootMismatches: fields,
              countMismatches: [],
            };
      }
      case "countMismatch": {
        const declared = error.message.startsWith(COUNT_MISMATCH_PREFIXES[1]);
        const base = declared
          ? ["count_mismatch", "declared_counts_member_mismatch"]
          : ["count_mismatch"];
        const fields = parseCanonicalFieldList(
          error.message,
          COUNT_MISMATCH_PREFIXES,
          WATCHER_HEADER_COUNT_FIELDS_V1,
        );
        return fields === null
          ? {
              reasonCodes: [...base, "unenumerated_count_mismatch"],
              rootMismatches: [],
              countMismatches: [],
            }
          : {
              reasonCodes: base,
              rootMismatches: [],
              countMismatches: fields,
            };
      }
      default:
        return {
          reasonCodes: ["unexpected_reconstruction_failure"],
          rootMismatches: [],
          countMismatches: [],
        };
    }
  }
  return {
    reasonCodes: ["unexpected_reconstruction_failure"],
    rootMismatches: [],
    countMismatches: [],
  };
};

const digestResult = (
  result: Omit<WatcherHeaderRootReconstructionResultV1, "resultDigest">,
): WatcherHeaderRootReconstructionResultV1 =>
  Object.freeze({
    ...result,
    resultDigest: watcherSha256CanonicalJsonV1(result),
  });

// ---------------------------------------------------------------------------
// Evaluation
// ---------------------------------------------------------------------------

export type EvaluateWatcherHeaderRootReconstructionInputV1 = {
  /**
   * An L1-authenticated header observation, ideally produced by
   * `makeWatcherAuthenticatedHeaderObservationV1`. It is re-admitted here, so a
   * hand-built observation whose header does not hash to its `headerHash` is
   * rejected rather than trusted.
   */
  readonly observation: AuthenticatedStateQueueHeaderObservationV1;
  /** Exact public `DaPayloadEnvelopeV1` bytes, from the W21 store. */
  readonly payloadEnvelopeCbor: Uint8Array;
  /** Provenance of those bytes; must be public/permissionless DA. */
  readonly daProvenance: EvidenceProvenanceV1;
  readonly minimumConfirmationDepth?: number;
};

export const evaluateWatcherHeaderRootReconstructionV1 = async (
  input: EvaluateWatcherHeaderRootReconstructionInputV1,
): Promise<WatcherHeaderRootReconstructionResultV1> => {
  const payloadEnvelopeSha256 = sha256Hex(input.payloadEnvelopeCbor);
  // Digest-only unwrap. The authoritative decode happens inside the canonical
  // reconstruction; this exists so a rejected block still records which inner
  // byte string was examined.
  let payloadSha256: string | null = null;
  try {
    const unwrapped = await unwrapDaPayloadV1(input.payloadEnvelopeCbor, {
      maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
    });
    payloadSha256 = sha256Hex(unwrapped.innerBytes);
  } catch {
    payloadSha256 = null;
  }

  const header = input.observation.header;
  const headerRoots = rootSetFromHeader(header);
  const headerCounts = countSetFromHeader(header);
  const common = {
    schemaVersion: WATCHER_HEADER_ROOT_RECONSTRUCTION_V1_SCHEMA_VERSION,
    headerRoots,
    headerCounts,
    headerHash: input.observation.headerHash,
    headerPrevUtxosRoot: header.prevUtxosRoot,
    payloadEnvelopeSha256,
    payloadSha256,
  } as const;

  try {
    const evidence = await canonicalBlockEvidenceFromVerifiedPayloadV1({
      observation: input.observation,
      payloadEnvelopeCbor: input.payloadEnvelopeCbor,
      daProvenance: input.daProvenance,
      ...(input.minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth: input.minimumConfirmationDepth }),
    });
    const reconstructedRoots = rootSetFromCanonical(
      evidence.reconstruction.roots,
    );
    const reconstructedCounts = countSetFromCanonical(
      evidence.reconstruction.counts,
    );
    return digestResult({
      ...common,
      action: "accept",
      reasonCodes: [],
      rootMismatches: [],
      countMismatches: [],
      reconstructedRoots,
      reconstructedCounts,
      payloadSha256: sha256Hex(evidence.reconstruction.payloadCbor),
    });
  } catch (error) {
    const classification = classifyFailure(error);
    return digestResult({
      ...common,
      action: "reject",
      reasonCodes: orderReasonCodes(classification.reasonCodes),
      rootMismatches: classification.rootMismatches,
      countMismatches: classification.countMismatches,
      reconstructedRoots: null,
      reconstructedCounts: null,
    });
  }
};

// ---------------------------------------------------------------------------
// Durable record
// ---------------------------------------------------------------------------

/**
 * Canonical CBOR commitment persisted as the reconstructed state:
 * `[ header_hash, [8 roots], [7 counts] ]`, all in the module's fixed field
 * order. It is derived only from values the reconstruction and the L1 header
 * agree on, so the record is reproducible from the same bytes.
 */
const reconstructedStateBytes = (
  result: WatcherHeaderRootReconstructionResultV1,
  roots: WatcherHeaderRootSetV1,
  counts: WatcherHeaderCountSetV1,
): Buffer =>
  encodeCborArrayRaw([
    encodeCborBytes(Buffer.from(result.headerHash, "hex")),
    encodeCborArrayRaw(
      WATCHER_HEADER_ROOT_FIELDS_V1.map((field) =>
        encodeCborBytes(Buffer.from(roots[field], "hex")),
      ),
    ),
    encodeCborArrayRaw(
      WATCHER_HEADER_COUNT_FIELDS_V1.map((field) =>
        encodeCborUnsigned(BigInt(counts[field])),
      ),
    ),
  ]);

/**
 * Builds the W03-reserved `WatcherReconstructedStateV1` record for an accepted
 * reconstruction. `inputIds` must be the exact W21 canonical-store input ids
 * whose bytes were reconstructed; nothing else is accepted as a source.
 */
export const makeWatcherHeaderRootReconstructedStateV1 = (input: {
  readonly result: WatcherHeaderRootReconstructionResultV1;
  readonly chainPointId: string;
  readonly inputIds: readonly string[];
}): WatcherReconstructedStateV1 => {
  const result = input.result;
  if (
    result.schemaVersion !==
    WATCHER_HEADER_ROOT_RECONSTRUCTION_V1_SCHEMA_VERSION
  ) {
    fail("unsupported_schema", "$.result.schemaVersion");
  }
  if (
    result.action !== "accept" ||
    result.reconstructedRoots === null ||
    result.reconstructedCounts === null
  ) {
    fail("result_not_accepted", "$.result.action");
  }
  const roots = result.reconstructedRoots as WatcherHeaderRootSetV1;
  const counts = result.reconstructedCounts as WatcherHeaderCountSetV1;
  if (typeof input.chainPointId !== "string" || input.chainPointId === "") {
    fail("invalid_input_ids", "$.chainPointId");
  }
  if (input.inputIds.length === 0) {
    fail("invalid_input_ids", "$.inputIds");
  }
  const seen = new Set<string>();
  for (const [index, inputId] of input.inputIds.entries()) {
    if (typeof inputId !== "string" || inputId === "" || seen.has(inputId)) {
      fail("invalid_input_ids", `$.inputIds[${index.toString()}]`);
    }
    seen.add(inputId);
  }
  return Object.freeze({
    blockHash: result.headerHash,
    chainPointId: input.chainPointId,
    priorStateRoot: result.headerPrevUtxosRoot,
    postStateRoot: roots.utxos_root,
    inputIds: Object.freeze([...input.inputIds]),
    state: makeWatcherDurablePayloadV1(
      reconstructedStateBytes(result, roots, counts).toString("hex"),
    ),
  });
};
