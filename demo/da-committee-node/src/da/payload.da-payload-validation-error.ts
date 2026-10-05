import { type DaPayloadEnvelopeTimingStage } from "@al-ft/midgard-core/da-payload-envelope";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";

import type {
  PayloadCountSet,
  PayloadRootSet,
  ValidationSummary,
} from "../domain.js";
import { normalizeHex } from "../utils/hex.js";

export type DataSchema = Parameters<typeof LucidData.Nullable>[0];

export type PayloadVerificationOptions = {
  readonly payloadSchemaVersion: 1;
  readonly stateQueueOutRef: string;
  /**
   * The L2 UTxO set immediately before the block, already bound to
   * `header.prevUtxosRoot` by the caller (`resolvePreBlockUtxos`). Every
   * event's inputs resolve against the state immediately before it, replayed
   * from this set.
   */
  readonly preBlockUtxos: readonly (readonly [string, Uint8Array])[];
  readonly timing?: DaPayloadVerificationTimingOptions;
};

export type DaPayloadVerificationTimingStage =
  | DaPayloadEnvelopeTimingStage
  | "stored_hash"
  | "inner_decode"
  | "payload_structure_validation"
  | "semantic_validation";

export type DaPayloadVerificationTimingOptions = {
  readonly monotonicNow?: () => number;
  readonly onStageTiming?: (
    stage: DaPayloadVerificationTimingStage,
    durationMs: number,
  ) => void;
};

export type DaPayloadRootSet = PayloadRootSet & {
  readonly validationTracesRoot: string;
};

export type DaPayloadCountSet = PayloadCountSet & {
  readonly validationTraceCount: bigint;
};

export type VerifiedDaPayload = {
  readonly payload: SDK.DaPayload;
  readonly storedPayloadCbor: Buffer;
  readonly innerPayloadCbor: Buffer;
  readonly payloadSha256: string;
  readonly roots: DaPayloadRootSet;
  readonly counts: DaPayloadCountSet;
  readonly validation: Omit<
    ValidationSummary,
    "rootSummary" | "countSummary"
  > & {
    readonly rootSummary: DaPayloadRootSet;
    readonly countSummary: DaPayloadCountSet;
  };
};

export class DaPayloadValidationError extends Error {
  readonly code:
    | "malformed_da"
    | "non_canonical"
    | "wrong_version"
    | "duplicate_key"
    | "unsorted_key"
    | "header_hash_mismatch"
    | "header_mismatch"
    | "malformed_transaction"
    | "malformed_trace"
    | "unsupported_feature"
    | "consensus_bound"
    | "version_mismatch"
    | "root_mismatch"
    | "count_mismatch"
    | "coverage_mismatch";

  constructor(
    code: DaPayloadValidationError["code"],
    message: string,
    options?: ErrorOptions,
  ) {
    // The wrapped cause stays in the message: committee tick logs and the
    // stored validation_error carry only `message`.
    super(
      options?.cause === undefined
        ? message
        : `${message}: ${formatUnknownError(options.cause)}`,
      options,
    );
    this.name = "DaPayloadValidationError";
    this.code = code;
  }
}

export const readMonotonicNow = (
  timing: DaPayloadVerificationTimingOptions | undefined,
): number | undefined => {
  try {
    return (timing?.monotonicNow ?? (() => performance.now()))();
  } catch {
    return undefined;
  }
};

export const recordTiming = (
  timing: DaPayloadVerificationTimingOptions | undefined,
  stage: DaPayloadVerificationTimingStage,
  startedAt: number | undefined,
): void => {
  if (startedAt === undefined) return;
  const completedAt = readMonotonicNow(timing);
  if (completedAt === undefined) return;
  try {
    timing?.onStageTiming?.(stage, completedAt - startedAt);
  } catch {
    // Observability must not change committee validation semantics.
  }
};

export const validateEntries = (
  fieldName: string,
  entries: readonly SDK.DaPayloadEntry[],
): void => {
  let previousKey: string | undefined;
  for (const [index, [key, value]] of entries.entries()) {
    const normalizedKey = normalizeHex(key, {
      fieldName: `${fieldName}[${index.toString()}].key`,
    });
    normalizeHex(value, {
      fieldName: `${fieldName}[${index.toString()}].value`,
    });
    if (previousKey !== undefined) {
      if (normalizedKey === previousKey) {
        throw new DaPayloadValidationError(
          "duplicate_key",
          `${fieldName} contains duplicate key ${normalizedKey}`,
        );
      }
      if (normalizedKey < previousKey) {
        throw new DaPayloadValidationError(
          "unsorted_key",
          `${fieldName} keys must be sorted ascending`,
        );
      }
    }
    previousKey = normalizedKey;
  }
};

const validateCounts = (counts: SDK.DaPayloadCounts): void => {
  const fields = [
    ["withdrawal_count", counts.withdrawalCount],
    ["forced_transaction_count", counts.forcedTransactionCount],
    ["l2_transaction_count", counts.l2TransactionCount],
    ["deposit_count", counts.depositCount],
    ["total_event_count", counts.totalEventCount],
    ["transition_step_count", counts.transitionStepCount],
  ] as const;
  for (const [field, count] of fields) {
    if (count < 0n) {
      throw new DaPayloadValidationError(
        "count_mismatch",
        `${field} must be non-negative`,
      );
    }
  }
  const expectedTotal =
    counts.withdrawalCount +
    counts.forcedTransactionCount +
    counts.l2TransactionCount +
    counts.depositCount;
  if (counts.totalEventCount !== expectedTotal) {
    throw new DaPayloadValidationError(
      "count_mismatch",
      `total_event_count ${counts.totalEventCount.toString()} does not match source counts ${expectedTotal.toString()}`,
    );
  }
  if (counts.transitionStepCount !== counts.totalEventCount) {
    throw new DaPayloadValidationError(
      "count_mismatch",
      "transition_step_count must equal total_event_count",
    );
  }
};

export const validateDaPayloadCounts = (counts: SDK.DaPayloadCounts): void => {
  validateCounts(counts);
  if (counts.validationTraceCount < 0n) {
    throw new DaPayloadValidationError(
      "count_mismatch",
      "validation_trace_count must be non-negative",
    );
  }
  const expectedValidationTraces =
    counts.forcedTransactionCount + counts.l2TransactionCount;
  if (counts.validationTraceCount !== expectedValidationTraces) {
    throw new DaPayloadValidationError(
      "count_mismatch",
      `validation_trace_count ${counts.validationTraceCount.toString()} must equal forced_transaction_count + l2_transaction_count ${expectedValidationTraces.toString()}`,
    );
  }
};

export const decodeCanonicalData = <A>(
  hex: string,
  schema: DataSchema,
  fieldName: string,
): A => {
  const normalized = normalizeHex(hex, { fieldName });
  try {
    const value = LucidData.from(normalized, schema as never) as A;
    const recoded = LucidData.to(value as never, schema as never);
    if (recoded !== normalized) {
      throw new Error(`${fieldName} is not canonical for its schema`);
    }
    return value;
  } catch (cause) {
    throw new DaPayloadValidationError(
      "malformed_trace",
      `failed to decode ${fieldName}`,
      { cause },
    );
  }
};
