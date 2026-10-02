import {
  SUBMIT_SLOT_LENGTH_MS,
  SUBMIT_SLOT_VALIDITY_BUFFER,
} from "../local-ledger-slot.js";

/**
 * Shared transaction signing, submission, confirmation, and recovery helpers.
 *
 * These utilities centralize the messy provider-facing parts of transaction
 * handling so higher-level transaction builders can stay focused on protocol
 * logic.
 */
export const RETRY_ATTEMPTS = 1;

export const INIT_RETRY_AFTER_MILLIS = 2_000;

export const TX_OUTPUT_VISIBILITY_TIMEOUT_MS = 30_000;

export const TX_OUTPUT_VISIBILITY_POLL_INTERVAL_MS = 1_000;

export const TX_CONFIRMATION_TIMEOUT_MS = 90_000;

export const TX_CONFIRMATION_RETRIES = 1;

export const TX_CONFIRMATION_POLL_INTERVAL_MS = 5_000;

export const SUBMIT_RECOVERY_AWAIT_TIMEOUT_MS = 90_000;

export const SUBMIT_RECOVERY_POLL_INTERVAL_MS = 5_000;

export const EARLY_VALIDITY_RETRY_SLOT_BUFFER = SUBMIT_SLOT_VALIDITY_BUFFER;

export const SLOT_LENGTH_MS = SUBMIT_SLOT_LENGTH_MS;

export const DEFAULT_SIGNED_TX_INLINE_WAIT_MS = 60_000;

const DEFAULT_STALE_PROVIDER_VALIDITY_RETRY_MAX_ATTEMPTS = Math.ceil(
  DEFAULT_SIGNED_TX_INLINE_WAIT_MS / SLOT_LENGTH_MS,
);

/** Ogmios reports a spent or missing input as JSON-RPC error 3117 with its
 * outrefs under `data.unknownOutputReferences`. A provider wrapper can carry
 * that error only as formatted text (an `Error.cause` is not enumerable), so
 * the text forms are matched as well as the structured field. */
const UNKNOWN_OUTPUT_REFERENCE_TEXT_REGEX =
  /unknownOutputReferences|JSON-RPC error 3117\b|"code":\s*3117\b/;

export const isUnknownOutputReferenceSubmitError = (
  error: unknown,
): boolean => {
  const seen = new Set<unknown>();
  const hasStructuredUnknownInput = (value: unknown): boolean => {
    if (typeof value !== "object" || value === null || seen.has(value)) {
      return false;
    }
    seen.add(value);
    const record = value as Record<string, unknown>;
    if ("unknownOutputReferences" in record || "badInputs" in record) {
      return true;
    }
    return Object.values(record).some(hasStructuredUnknownInput);
  };
  if (hasStructuredUnknownInput(error)) {
    return true;
  }
  return errorTextSearchStrings(error).some(
    (message) =>
      message.includes("BadInputsUTxO") ||
      message.includes("UnknownInput") ||
      UNKNOWN_OUTPUT_REFERENCE_TEXT_REGEX.test(message),
  );
};

const OUTSIDE_VALIDITY_INTERVAL_REGEX =
  /OutsideValidityIntervalUTxO \(ValidityInterval \{invalidBefore = SJust \(SlotNo (\d+)\), invalidHereafter = SJust \(SlotNo (\d+)\)\}\) \(SlotNo (\d+)\)/;

export const EMULATOR_LOWER_BOUND_OUTSIDE_VALIDITY_REGEX =
  /Lower bound \((\d+)\) not in slot range \((\d+)\)/;

export type OutsideValidityIntervalDetails = {
  readonly invalidBeforeSlot: number;
  readonly invalidHereafterSlot?: number;
  readonly currentSlot: number;
};

export type SignedTxValidityInterval = {
  readonly invalidBeforeSlot?: number;
  readonly invalidHereafterSlot?: number;
};

const outsideValidityDetails = (
  invalidBeforeSlot: number,
  invalidHereafterSlot: number | undefined,
  currentSlot: number,
): OutsideValidityIntervalDetails | null =>
  [invalidBeforeSlot, currentSlot].every(Number.isFinite) &&
  (invalidHereafterSlot === undefined || Number.isFinite(invalidHereafterSlot))
    ? {
        invalidBeforeSlot,
        ...(invalidHereafterSlot === undefined ? {} : { invalidHereafterSlot }),
        currentSlot,
      }
    : null;

/**
 * Extracts slot-boundary details from an `OutsideValidityIntervalUTxO` error.
 */
export const parseOutsideValidityIntervalDetails = (
  error: unknown,
): OutsideValidityIntervalDetails | null => {
  const structured = parseStructuredOutsideValidityIntervalDetails(error);
  if (structured !== null) {
    return structured;
  }
  for (const message of errorTextSearchStrings(error)) {
    const normalizedMessage = message.replace(/\\"/g, '"');
    const match = OUTSIDE_VALIDITY_INTERVAL_REGEX.exec(normalizedMessage);
    if (match !== null) {
      return outsideValidityDetails(
        Number(match[1]),
        Number(match[2]),
        Number(match[3]),
      );
    }

    const emulatorLowerBoundMatch =
      EMULATOR_LOWER_BOUND_OUTSIDE_VALIDITY_REGEX.exec(normalizedMessage);
    if (emulatorLowerBoundMatch !== null) {
      return outsideValidityDetails(
        Number(emulatorLowerBoundMatch[1]),
        Number.MAX_SAFE_INTEGER,
        Number(emulatorLowerBoundMatch[2]),
      );
    }
  }
  return null;
};

type ProviderValidityRetryDecision =
  | {
      readonly status: "wait";
      readonly waitMs: number;
      readonly targetSlot: number;
    }
  | { readonly status: "already_valid" }
  | { readonly status: "expired" }
  | { readonly status: "window_too_narrow" }
  | { readonly status: "attempts_exhausted" };

export const resolveEarlyValidityRetry = (
  details: OutsideValidityIntervalDetails,
  attempt: number,
  maxAttempts = DEFAULT_STALE_PROVIDER_VALIDITY_RETRY_MAX_ATTEMPTS,
): ProviderValidityRetryDecision => {
  if (attempt >= maxAttempts) {
    return { status: "attempts_exhausted" };
  }
  if (
    details.invalidHereafterSlot !== undefined &&
    details.currentSlot >= details.invalidHereafterSlot
  ) {
    return { status: "expired" };
  }
  const targetSlot =
    details.invalidBeforeSlot + EARLY_VALIDITY_RETRY_SLOT_BUFFER;
  if (
    details.invalidHereafterSlot !== undefined &&
    targetSlot >= details.invalidHereafterSlot
  ) {
    return { status: "window_too_narrow" };
  }
  if (details.currentSlot >= targetSlot) {
    return { status: "already_valid" };
  }
  return {
    status: "wait",
    targetSlot,
    waitMs: (targetSlot - details.currentSlot) * SLOT_LENGTH_MS,
  };
};

export const resolveEarlyValidityRetryDelayMs = (
  details: OutsideValidityIntervalDetails,
  attempt: number,
): number | null => {
  const decision = resolveEarlyValidityRetry(details, attempt);
  return decision.status === "wait" ? decision.waitMs : null;
};

const parseStructuredOutsideValidityIntervalDetails = (
  value: unknown,
  seen = new Set<unknown>(),
): OutsideValidityIntervalDetails | null => {
  if (value === null || value === undefined) {
    return null;
  }
  if (typeof value !== "object") {
    return null;
  }
  if (seen.has(value)) {
    return null;
  }
  seen.add(value);
  if (value instanceof Error) {
    const fromCause = parseStructuredOutsideValidityIntervalDetails(
      value.cause,
      seen,
    );
    if (fromCause !== null) {
      return fromCause;
    }
  }
  const record = value as Record<string, unknown>;
  const direct = detailsFromStructuredRecord(record);
  if (direct !== null) {
    return direct;
  }
  for (const key of [
    "error",
    "data",
    "response",
    "body",
    "detail",
    "cause",
  ] as const) {
    const nested = parseStructuredOutsideValidityIntervalDetails(
      record[key],
      seen,
    );
    if (nested !== null) {
      return nested;
    }
  }
  return null;
};

const detailsFromStructuredRecord = (
  record: Record<string, unknown>,
): OutsideValidityIntervalDetails | null => {
  const validityInterval = record.validityInterval;
  if (typeof validityInterval !== "object" || validityInterval === null) {
    return null;
  }
  const interval = validityInterval as Record<string, unknown>;
  const invalidBeforeSlot = numberFromUnknown(interval.invalidBefore);
  const invalidHereafterSlot =
    numberFromUnknown(interval.invalidHereafter) ??
    numberFromUnknown(interval.invalidAfter);
  const currentSlot = numberFromUnknown(record.currentSlot);
  if (invalidBeforeSlot === null || currentSlot === null) {
    return null;
  }
  return outsideValidityDetails(
    invalidBeforeSlot,
    invalidHereafterSlot ?? undefined,
    currentSlot,
  );
};

const errorTextSearchStrings = (
  value: unknown,
  seen = new Set<unknown>(),
): readonly string[] => {
  if (typeof value === "string") {
    return [value];
  }
  if (value instanceof Error) {
    return [
      value.message,
      String(value),
      ...errorTextSearchStrings(value.cause, seen),
    ];
  }
  if (typeof value !== "object" || value === null || seen.has(value)) {
    return [String(value)];
  }
  seen.add(value);
  const record = value as Record<string, unknown>;
  const direct = ["message", "detail", "body"].flatMap((key) =>
    typeof record[key] === "string" ? [record[key] as string] : [],
  );
  const nested = ["cause", "error", "response", "data"].flatMap((key) =>
    errorTextSearchStrings(record[key], seen),
  );
  return [...direct, ...nested];
};

export const compactValidityInterval = (
  interval: SignedTxValidityInterval,
): SignedTxValidityInterval => ({
  ...(interval.invalidBeforeSlot === undefined
    ? {}
    : { invalidBeforeSlot: interval.invalidBeforeSlot }),
  ...(interval.invalidHereafterSlot === undefined
    ? {}
    : { invalidHereafterSlot: interval.invalidHereafterSlot }),
});

export const slotNumber = (value: unknown): number | undefined => {
  const parsed = numberFromUnknown(value);
  return parsed === null ? undefined : parsed;
};

const numberFromUnknown = (value: unknown): number | null => {
  if (typeof value === "number") {
    return Number.isSafeInteger(value) && value >= 0 ? value : null;
  }
  if (typeof value === "bigint") {
    return value >= 0n && value <= BigInt(Number.MAX_SAFE_INTEGER)
      ? Number(value)
      : null;
  }
  if (typeof value === "string" && /^\d+$/.test(value)) {
    const parsed = Number(value);
    return Number.isSafeInteger(parsed) ? parsed : null;
  }
  if (
    typeof value === "object" &&
    value !== null &&
    typeof (value as { to_str?: unknown }).to_str === "function"
  ) {
    return numberFromUnknown((value as { to_str: () => string }).to_str());
  }
  return null;
};
