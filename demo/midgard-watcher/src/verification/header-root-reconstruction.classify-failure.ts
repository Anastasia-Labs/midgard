import { TransitionTraceChallengerError } from "@al-ft/midgard-fault-proofs";
import {
  type AuthenticatedStateQueueHeaderObservation,
  CanonicalEvidenceRejection,
  type EvidenceProvenance,
} from "@al-ft/midgard-sdk";

import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import {
  WATCHER_HEADER_COUNT_FIELDS,
  WATCHER_HEADER_ROOT_FIELDS,
  WATCHER_HEADER_ROOT_RECONSTRUCTION_REASON_CODES,
  type WatcherHeaderCountField,
  type WatcherHeaderCountSet,
  type WatcherHeaderRootField,
  type WatcherHeaderRootReconstructionReasonCode,
  type WatcherHeaderRootReconstructionResult,
  type WatcherHeaderRootSet,
} from "./header-root-reconstruction.make-watcher-authenticated-header-observation.js";

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

export const rootSetFromCanonical = (
  roots: CanonicalRootSet,
): WatcherHeaderRootSet => ({
  utxos_root: roots.utxosRoot,
  withdrawals_root: roots.withdrawalsRoot,
  forced_transactions_root: roots.forcedTransactionsRoot,
  transactions_root: roots.transactionsRoot,
  deposits_root: roots.depositsRoot,
  transition_trace_root: roots.transitionTraceRoot,
  event_to_step_root: roots.eventToStepRoot,
  validation_traces_root: roots.validationTracesRoot,
});

export const countSetFromCanonical = (
  counts: CanonicalCountSet,
): WatcherHeaderCountSet => ({
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

export const orderReasonCodes = (
  codes: readonly string[],
): readonly WatcherHeaderRootReconstructionReasonCode[] => {
  const present = new Set(codes);
  return WATCHER_HEADER_ROOT_RECONSTRUCTION_REASON_CODES.filter((code) =>
    present.has(code),
  );
};

type Classification = {
  readonly reasonCodes: readonly string[];
  readonly rootMismatches: readonly WatcherHeaderRootField[];
  readonly countMismatches: readonly WatcherHeaderCountField[];
};

export const classifyFailure = (error: unknown): Classification => {
  if (error instanceof CanonicalEvidenceRejection) {
    return {
      reasonCodes: [error.code],
      rootMismatches: [],
      countMismatches: [],
    };
  }
  if (error instanceof TransitionTraceChallengerError) {
    // Only evidence-shape errors receive specialized classifications here.
    // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
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
          WATCHER_HEADER_ROOT_FIELDS,
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
          WATCHER_HEADER_COUNT_FIELDS,
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

export const digestResult = (
  result: Omit<WatcherHeaderRootReconstructionResult, "resultDigest">,
): WatcherHeaderRootReconstructionResult =>
  Object.freeze({
    ...result,
    resultDigest: watcherSha256CanonicalJson(result),
  });

// ---------------------------------------------------------------------------
// Evaluation
// ---------------------------------------------------------------------------

export type EvaluateWatcherHeaderRootReconstructionInput = {
  /**
   * An L1-authenticated header observation, ideally produced by
   * `makeWatcherAuthenticatedHeaderObservation`. It is re-admitted here, so a
   * hand-built observation whose header does not hash to its `headerHash` is
   * rejected rather than trusted.
   */
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  /** Exact public `DaPayloadEnvelopeV1` bytes, from the W21 store. */
  readonly payloadEnvelopeCbor: Uint8Array;
  /** Provenance of those bytes; must be public/permissionless DA. */
  readonly daProvenance: EvidenceProvenance;
  readonly minimumConfirmationDepth?: number;
};
